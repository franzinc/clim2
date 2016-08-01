;; -*- mode: common-lisp; package: tk -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :tk)

(defmacro def-foreign-array-resource (name constructor)
  `(progn
     (clim-sys:defresource ,name (n)
       :constructor (cons n (,constructor :number n))
       :matcher (not (< (car ,name) n)))
     (defmacro ,(fintern "~A-~A" 'with name)
	 ((var n) &body body)
       `(clim-sys:using-resource (,var ,',name ,n)
	  (let ((,var (cdr ,var)))
	    ,@body)))))

(defmacro define-ref-par-types (&rest types)
  (let ((forms nil))
    (dolist (type types)
      (let ((type-array (fintern "~A-~A" type 'array))
	    (make-type-array (fintern "~A-~A-~A"
				      'make type 'array)))
	(setq forms
	  `(,@forms
	    (ff:def-c-type ,type-array 1 ,type)
	    (def-foreign-array-resource
		,type-array ,make-type-array)))))
    `(progn ,@forms)))

(define-ref-par-types
    :unsigned-int :int :unsigned-long :long
    *)

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
