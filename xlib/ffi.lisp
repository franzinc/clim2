;; See the file LICENSE for the full license governing this code.
;;

(in-package :x11)

;; Note -- All exports are now done in pkg.lisp, for space/performance
;;         reasons.  jdi  (temporarily not true).

(defmacro def-exported-constant (name value)
  ;; define the constant and export it from :x11
  `(progn
     (eval-when (eval load compile)
       (export ',name))
     (defconstant ,name ,value)))

(eval-when (compile load eval)
  (defun transmogrify-ff-type (type)
    (if (consp type)
	(case (car type)
	  (:pointer
	   `(* ,@(transmogrify-ff-type (second type))))
	  (:array
	   `(,@(third type) ,(second type)))
	  (t (list type)))
      (list type))))


(defmacro def-exported-foreign-synonym-type (new-name old-name)
  `(progn
     (eval-when (eval load compile)
       (export ',new-name))
     (ff::def-c-typedef ,new-name ,@(transmogrify-ff-type old-name))))

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
		   `(,name ,@(trans-slot-type type)))))
	  (if (notany #'(lambda (s) (member :overlays (cdr s))) slots)
	      `(ff::def-c-type (,name :no-defuns ,@options)
		 ,@(mapcar #'foo-slot slots))
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
		  `(ff::def-c-type (,name :no-defuns ,@options) :union
				   ,@(mapcar #'(lambda (slot)
						 (setq slot (copy-list slot))
						 (remf (cdr slot) :overlays)
						 (foo-slot slot))
					     slots))
		(error ":overlays used in a way we cannot handle")))))
       ,(when array-name
	  `(ff::def-c-type (,array-name :no-defuns ,@options)
	     1 ,name)))))



(defun trans-slot-type (type)
  (if (atom type)
      (transmogrify-ff-type type)
    (ecase (car type)
      (:pointer `(* ,(second type)))
      (:array
       (destructuring-bind
	  (ignore type indicies) type
	(declare (ignore ignore))
	`(,@indicies ,type))))))

;;; Delay version

#-(version>= 5 0)
(defun trans-arg-type (type)
  (excl:if* (consp type)
     then (ecase (car type)
	    (:pointer 'ff:foreign-address)
	    (:array 'ff:foreign-address))
     else (case type
	    (void (error "void not allowed here"))
	    ((int unsigned-int :unsigned-32bit :signed-32bit) 'integer)
	    ((fixnum-int fixnum-unsigned-int) 'fixnum)
	    (fixnum-drawable 'ff:foreign-address)
	    (t
	     (if (get type 'ff::cstruct)
		 'ff:foreign-address
	       't)))))

#+(and (version>= 5 0) (not (version>= 6 1)))
(defun trans-arg-type (type)
  (excl:if* (consp type)
     then (ecase (car type)
	    (:pointer :foreign-address)
	    (:array :foreign-address))
     else (case type
	    (void (error "void not allowed here"))
	    ((int unsigned-int :unsigned-32bit :signed-32bit) 'integer)
	    ((fixnum-int fixnum-unsigned-int) 'fixnum)
	    (fixnum-drawable :foreign-address)
	    (t
	     (if (get type 'ff::cstruct)
		 :foreign-address
	       't)))))

#-(version>= 6 1)
(defun trans-return-type (type)
  (excl:if* (consp type)
     then (ecase (car type)
	    (:pointer :unsigned-integer)
	    (:array :unsigned-integer))
     else (case type
	    (void :void)
	    ((integer int) :integer)
	    ((fixnum-int :fixnum) :fixnum)
	    (fixnum-int :fixnum)
	    (:unsigned-32bit :integer)
	    (:signed-32bit :integer)
	    (t :unsigned-integer))))


#-(version>= 6 1)
(defmacro def-exported-foreign-function ((name &rest options) &rest args)
  `(progn
     (eval-when (eval load compile)
       (export ',name))
     (eval-when (compile eval load)
       ,(let ((c-name (ff:convert-foreign-name (second (assoc :name options))))
	      (return-type (or (second (assoc :return-type options))
			       'void)))
	  `(delayed-defforeign
	    ',name
	    :arguments ',(mapcar #'trans-arg-type (mapcar #'second args))
	    :call-direct t
	    :callback t
	    :arg-checking nil
	    :return-type ,(trans-return-type return-type)
	    :entry-point ,c-name)))))


#+(version>= 6 1)
(defun trans-arg-type (type)
  (cons (car type)
	(excl:if* (consp (cadr type))
	   then (ecase (caadr type)
		  (:pointer '(:foreign-address))
		  (:array '(:foreign-address)))
	   else (case (cadr type)
		  (void (error "void not allowed here"))
		  ((int :signed-32bit) '(:int))
		  ((unsigned-int :unsigned-32bit) '(:unsigned-int))
		  ((fixnum-int fixnum-unsigned-int) '(:int fixnum))
		  (fixnum-drawable '(:foreign-address))
		  (t
		   (if (get (cadr type) 'ff::cstruct)
		       '(:foreign-address)
		     '(:lisp)))))))

#+(version>= 6 1)
(defun trans-return-type (type)
  (excl:if* (consp type)
     then (ecase (car type)
	    (:pointer :foreign-address)
	    (:array :foreign-address))
     else (case type
	    (void :void)
	    ((integer int) :int)
	    ((fixnum-int :fixnum) '(:int fixnum))
	    (:unsigned-32bit :unsigned-int)
	    (:signed-32bit :int)
	    (t :unsigned-int))))

#+(version>= 6 1)
(defmacro def-exported-foreign-function ((name &rest options) &rest args)
  `(progn
     (eval-when (eval load compile)
       (export ',name))
     (eval-when (compile eval load)
       ,(let ((c-name (second (assoc :name options)))
	      (return-type (or (second (assoc :return-type options))
			       'void)))
	  `(ff:def-foreign-call (,name ,c-name)
	       ,(or (mapcar #'trans-arg-type args) '(:void))
	     :returning ,(trans-return-type return-type)
	     :call-direct t
	     :arg-checking nil)))))


#-(version>= 6 1)
(defparameter *defforeigned-functions* nil
  "A list of name and defforeign arguments")

#-(version>= 6 1)
(defun delayed-defforeign (name &rest arguments)
  (setf *defforeigned-functions*
    (delete name *defforeigned-functions* :key #'car))
  (push (cons name arguments) *defforeigned-functions*))

#-(version>= 6 1)
(defun defforeign-functions-now ()
  (ff:defforeign-list *defforeigned-functions*))

;;; End of delay version

(defmacro def-exported-foreign-macro ((name &rest options) &rest args)
  `(def-exported-foreign-function (,name  ,@options) ,@args))

(foreign-functions:def-c-typedef :fixnum :int)
(foreign-functions:def-c-typedef :signed-32bit :int)
(foreign-functions:def-c-typedef :pointer * :char)
(foreign-functions:def-c-typedef :signed-8bit :char)

;; Create non-keyword versions.
(def-exported-foreign-synonym-type char :char)
(def-exported-foreign-synonym-type unsigned-char :unsigned-char)
(def-exported-foreign-synonym-type short  :short)
(def-exported-foreign-synonym-type unsigned-short :unsigned-short)
(def-exported-foreign-synonym-type int :int)
(def-exported-foreign-synonym-type unsigned-int :unsigned-int)
(def-exported-foreign-synonym-type long :long)
(def-exported-foreign-synonym-type unsigned-long :unsigned-long)
(def-exported-foreign-synonym-type float :single-float)
(def-exported-foreign-synonym-type double  :double-float)
(def-exported-foreign-synonym-type void :int)
(def-exported-foreign-synonym-type short-int short)
(def-exported-foreign-synonym-type long-int long)
(def-exported-foreign-synonym-type unsigned unsigned-int)
(def-exported-foreign-synonym-type long-float double)

(def-exported-foreign-synonym-type caddr-t :pointer)
(def-exported-foreign-synonym-type u-char unsigned-char)
(def-exported-foreign-synonym-type u-short unsigned-short)
(def-exported-foreign-synonym-type u-int unsigned-int)
(def-exported-foreign-synonym-type u-long unsigned-long)
(def-exported-foreign-synonym-type fd-mask long)
