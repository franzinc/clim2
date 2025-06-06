;; See the file LICENSE for the full license governing this code.
;;

(in-package :x11)

;;; Note -- All exports are now done in pkg.lisp, for space/performance
;;;         reasons.  jdi  (temporarily not true).

(defmacro def-exported-constant (name value)
  ;; define the constant and export it from :x11
  `(progn
     (eval-when (eval load compile)
       (export ',name))
     (defconstant ,name ,value)))

(eval-when (compile load eval)
  
  (defun ensure-list (x)
    (if (listp x)
	x
      (list x)))

  (defun transmogrify-ff-type (type)
      (if (consp type)
	(case (car type)
	  (:pointer
	   `((* ,@(transmogrify-ff-type (second type)))))
	  (:array
	   (break "not supported"))
	  (t (list type)))
      (list type))))

(defvar *type-hash* (make-hash-table))

(defun lookup-type-def (type) 
  (if (and (consp type) 
	   (eq (first type) :pointer))
      (list :foreign-address)
    (let ((type0 (gethash type *type-hash*)))	
      (if type0
	  (lookup-type-def type0)
	type))))

(defmacro def-exported-foreign-synonym-type (new-name old-name)
    `(progn
       (eval-when (eval load compile)
	 (export ',new-name))
       (setf (gethash ',new-name *type-hash*) 
	 (lookup-type-def ',old-name))
       (ff:def-foreign-type ,new-name ,@(transmogrify-ff-type old-name))))

(defmacro def-exported-foreign-struct (name-and-options &rest slots)
  (let (name array-name (options nil))
    (if (atom name-and-options)
	(setq name name-and-options)
      (setq name (car name-and-options)
	    options (cdr name-and-options)))
    (when (member :array options)
      (setq array-name (fintern "~A-~A" name 'array)
	    options (delete :array options)))
    `(progn
       ,(flet ((make-exports (name)
		 (list* name
			(fintern "~A-~A" 'make name)
			(mapcar #'(lambda (x)
				    (fintern "~A-~A" name (car x)))
				slots))))
	  `(eval-when (eval load compile)
	     (export '(,@(make-exports name)
		       ,@(make-exports array-name)))))
       ,(flet ((foo-slot (slot)
		 (destructuring-bind
		     (name &key type) slot
		   `(,name ,@(trans-slot-type type))))
	       (foo-slot-2 (slot)
		 (destructuring-bind
		     (name &key type) slot
		   `(,name ,(intern (format nil "~A-~A" name :union)) ,@(trans-slot-type type)))))
	  (if (notany #'(lambda (s) (member :overlays (cdr s))) slots)
	      (progn 
		`(ff::def-foreign-type ,name ;; (,name ,@options)
		     (:struct ,@(mapcar #'foo-slot slots))))
	    (destructuring-bind
		((first-slot-name . first-options) . other-slots) slots
	      (declare (ignore first-slot-name))
	      (if (and (null (member :overlays first-options))
		       (every #'(lambda (slot)
				  slot ;- Bug
				  #+ignore
				  (eq (getf (cdr slot) :overlays)
				      first-slot-name))
			      other-slots))
		  (progn 
		    `(ff::def-foreign-type ,name ;; (,name ,@options) 
				   (:union
				    ,@(mapcar #'(lambda (slot)
						 (setq slot (copy-list slot))
						 (remf (cdr slot) :overlays)
						 (list (intern (format nil "~A-~A" 
								       (first slot)
								       :union))						 
						       (list :struct (foo-slot slot))))
					     slots))))
		(error ":overlays used in a way we cannot handle")))))
       ,(when array-name
	  (progn 
	    `(ff::def-foreign-type ,array-name ;; (,array-name ,@options)
		 (:array ,name 1)))))))

(defun trans-slot-type (type)
  (if (atom type)
      (transmogrify-ff-type type)
    (ecase (car type)
      (:pointer `((* ,(second type))))
      (:array
       (destructuring-bind
	  (ignore type indicies) type
	(declare (ignore ignore))
	`((:array ,type ,@indicies)))))))

(defun trans-arg-type-1 (type)
  (cons (car type)
	(excl:if* (consp (cadr type)) 
	   then (ecase (caadr type)
		  ;;(:pointer '(:foreign-address t foreign-pointer-if-clos-instance))
		  (:pointer '(:foreign-address))
		  (:array '(:foreign-address)))
	   else (case (cadr type)
		  (void (error "void not allowed here"))
		  ((:signed-32bit :unsigned-32bit)
		   (error "32 bit?!"))
		  (fixnum-drawable '(:foreign-address t foreign-pointer-if-clos-instance))
		  (t
		   (ensure-list 
		    (lookup-type-def (second type))))))))


(defun trans-return-type (type)
  (excl:if* (consp type)
     then (ecase (car type)
	    (:pointer :foreign-address)
	    (:array :foreign-address))
     else (case type
	    (void :void)
	    ((integer int) :int)
	    (fixnum :fixnum)
	    (unsigned-fixnum :fixnum)
	    (t (let ((res (lookup-type-def type)))
		 (if (symbolp res)
		     (intern res :keyword)
		   res))))))
  
(defun foreign-pointer-if-clos-instance (action val ctype ltype)
  (declare (ignore ctype ltype))
  (case action
    (:convert 
     (if (typep val 'standard-object)
		  (ff::foreign-pointer-address val)
		val))
    (:convert-type 'fixnum)
    (:identify :arg)
    (:check t)))

(defmacro def-exported-foreign-function ((name &rest options) &rest args)
  `(progn
     (eval-when (eval load compile)
       (export ',name))
     (eval-when (compile eval load)
       ,(let ((c-name (second (assoc :name options)))
	      (return-type (or (second (assoc :return-type options))
			       'void)))	  
	  `(ff:def-foreign-call (,name ,c-name)
	       ,(or (mapcar #'trans-arg-type-1 args) '(:void))
	     :returning ,(ensure-list (trans-return-type return-type))
	     :call-direct t
	     :arg-checking nil)))))

(defmacro def-exported-foreign-macro ((name &rest options) &rest args)
  `(def-exported-foreign-function (,name  ,@options) ,@args))


;;; 
;;; 
;;;

(foreign-functions:def-foreign-type :pointer (* :char))
(def-exported-foreign-synonym-type void :int)
(def-exported-foreign-synonym-type caddr-t :pointer)
