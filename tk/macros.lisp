;; -*- mode: common-lisp; package: tk -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :tk)

(defmacro def-foreign-array-resource (name constructor)
  (let ((forms 
	 `(progn
	    (clim-sys:defresource ,name (n)
				  :constructor (cons n (,constructor :number n))
				  :matcher (not (< (car ,name) n)))
	    (defmacro ,(fintern "~A-~A" 'with name)
		((var n) &body body)
	      `(clim-sys:using-resource (,var ,',name ,n)
					(let ((,var (cdr ,var)))
					  ,@body))))))
    forms))

(defun make-cstruct-2 (size init)
  (excl::.primcall 'sys::make-svector
    #-64bit (+ 1 (truncate (+ size 3) 4))
    #+64bit (+ 1 (truncate (+ size 7) 8))
    init
    #.(sys::mdparam 'comp::md-cstruct-svector-type-code)
    init
    nil))

#+:ignore 
(define-compiler-macro make-cstruct-2 (&whole whole size init)
  (if (integerp size)
      `(excl::.primcall 'sys::make-svector
         #-64bit (+ 1 (truncate (+ ,size 3) 4))
         #+64bit (+ 1 (truncate (+ ,size 7) 8))
         ,init
         #.(sys::mdparam 'comp::md-cstruct-svector-type-code)
         ,init
         nil)
    whole))

(defun cstruct-constructor-generator-3 (size space init)
  #'(lambda (&key (number 1) (in-foreign-space space) (initialize init))
      (let ((-size- (* 8 number size)))
        (if in-foreign-space
            (malloc-initialized -size- init)
          ;;bug3004
          (make-cstruct-2 -size- (when initialize 0))))))

(defmacro define-ref-par-types (&rest types)
  (let ((forms nil)
	(type1 nil))
    
    (dolist (type types)
      (let ((type-array (fintern "~A-~A" type 'array))
	    (make-type-array (fintern "~A-~A-~A"
				      'make type 'array)))

	(if (eq type '*)
	    (setf type1 '(* char))
	  (setf type1 type))
	
	(setq forms
	  `(,@forms
	    (ff:def-foreign-type (,type-array (:pack 1)) (:array ,type1 1))
	    
	    (defun ,make-type-array (&key (number 1))
	      
	      (ff:allocate-fobject 
	       ',type-array :foreign-static-gc 
	       
	       (* number 
		  (ff::sizeof-fobject (ff:get-foreign-type ',type-array)))))
	    
	    (defun ,type-array (obj number)
	      (ff:fslot-value-typed (quote ,type-array) :foreign-static-gc obj number))
	    
	    (defsetf ,type-array (obj index) (val)  
	      `(setf (ff:fslot-value-typed (quote ,(quote ,type-array)) :foreign-static-gc ,obj ,index) ,val))
	    
	    (def-foreign-array-resource
		,type-array ,make-type-array)))))
    
    `(progn ,@forms)))

(define-ref-par-types
    :unsigned-int :int :unsigned-long :long *)

(defmacro with-ref-par (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
    (destructuring-bind
	((var value &optional type) &rest more-bindings)
	bindings
      (let ((&var (fintern "&~A" var))
	    (val '#:val)
	    (with-type-array (package-fintern (find-package :tk)
					      "~A-~A-~A" 'with type 'array))
	    (type-array (package-fintern (find-package :tk)
					 "~A-~A" type 'array)))
	`(let ((,val ,value))
	   (,with-type-array (,&var 1)
	     (symbol-macrolet ((,var (,type-array ,&var 0)))
	       (setf ,var ,val)
	       (multiple-value-prog1
		   (with-ref-par ,more-bindings ,@body)))))))))

(defmacro object-display (object)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (excl::slot-value-using-class-name 'display-object ,object
					'display)))

(defvar *malloced-objects*)

(defun note-malloced-object (obj &optional (free #'clim-utils::system-free))
  (push (cons free obj) *malloced-objects*)
  obj)

(defmacro with-malloced-objects (&body body)
  `(let ((*malloced-objects* nil))
     (unwind-protect
	 (progn ,@body)
       (dolist (entry *malloced-objects*)
	 (funcall (car entry) (cdr entry))))))

(defmacro lisp-string-to-string8 (string)
  (clim-utils:with-gensyms (length string8 i)
    (clim-utils:once-only (string)
      `(excl:ics-target-case
	(:+ics
	 (let* ((,length (length ,string))
		(,string8 (make-array (1+ ,length) :element-type '(unsigned-byte 8))))
	   (dotimes (,i ,length)
	     (setf (aref ,string8 ,i)
	       (logand (char-code (char ,string ,i)) #xff)))
	   (setf (aref ,string8 ,length) 0)
	   ,string8))
	(:-ics ,string)))))

(defmacro lisp-string-to-string16 (string)
  (clim-utils:with-gensyms (length string16 i j code)
    (clim-utils:once-only (string)
      `(excl:ics-target-case
	(:+ics
	 (let* ((,length (length ,string))
		(,string16 (make-array (* (1+ ,length) 2)
				       :element-type '(unsigned-byte 8)))
		(,j 0))
	   (dotimes (,i ,length)
	     (let ((,code (xchar-code (char ,string ,i))))
	       (setf (aref ,string16 ,j) (ldb (byte 8 8) ,code))
	       (incf ,j)
	       (setf (aref ,string16 ,j) (ldb (byte 8 0) ,code))
	       (incf ,j)))
	   (setf (aref ,string16 ,j) 0
		 (aref ,string16 (1+ ,j)) 0)
	   ,string16))
	(:-ics (error "~S called in non-ICS Lisp"
		      'lisp-string-to-string16))))))

(defmacro xchar-code (char)
  (clim-utils:with-gensyms (code)
    `(let ((,code (char-code
		   (excl:ics-target-case
		    (:+ics
		     #+(version>= 6 0 :pre-alpha 19) (excl::process-code ,char)
		     #-(version>= 6 0 :pre-alpha 19) ,char)
		    (:-ics ,char)))))
       (excl:ics-target-case
	(:+ics (logand ,code
		       (if (logbitp 15 ,code)
			   ;; jis-x208 and gaiji
			   #x7f7f
			 ;; ascii and jis-x201
			 #xff)))
	(:-ics ,code)))))
