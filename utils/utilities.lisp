;;; -*- Mode: Lisp; Package: CLIM-UTILS; Base: 10.; Syntax: Common-Lisp; Lowercase: Yes -*-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca.  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, Ca.  All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;

;; $fiHeader: utilities.lisp,v 1.1 91/08/30 13:57:51 cer Exp Locker: cer $

;;;
;;; Copyright (c) 1989, 1990 by Xerox Corporation.  All rights reserved. 
;;; Copyright (c) 1991, Franz Inc. All rights reserved
;;;

(in-package :clim-utils)

;;;
;;; Various UTILITIES
;;; 

;;;
;;; COMMONLISP Extensions
;;; 

(deftype boolean nil '(member nil t))

(defmacro compile-time-warn (warn-string)
  (warn warn-string)
  nil)

(eval-when (compile load eval)
  (defmacro with-collection (&body body)
    `(let (($with-collection-result$ nil)
	   $with-collection-tail$)
       (macrolet
	   ((collect (form)
	      ;;  The FORM is evaluated first so that COLLECT nests
	      ;; properly, i.e., The test to determine if this is
	      ;; the first value collected should be done after the
	      ;; value itself is generated in case it does
	      ;; collection as well.
	      `(let (($collectable$ ,form))
		 (if $with-collection-tail$
		     (rplacd $with-collection-tail$
			     (setq $with-collection-tail$
				   (list $collectable$)))
		     (setq $with-collection-result$
			   (setq $with-collection-tail$
				 (list $collectable$))))
		 $with-collection-tail$)))
	 ,@body $with-collection-result$))))

(defmacro with-gensyms ((&rest vars) &body body)
  `(let ,(mapcar #'(lambda (symbol)
		     `(,symbol (gensymbol ',symbol)))
		 vars)
     ,@body))

(defmacro integerize-single-float-coordinate (coord)
  ;; DCPL doesn't know the best way to declare things, so he goes all out
  (let ((coord+.5 `(+ (the single-float ,coord) .5f0)))
    `(the fixnum
	  (let ()
	    (declare (optimize (speed 3) (safety 0) (compilation-speed 0)))
	    #+(and excl foo-knows-what) (progn
		     #+(target-class r) (comp::ll :single-to-fixnum ,coord+.5)
		     #-(target-class r) (values (floor ,coord+.5)))
	    #-(and excl foo-knows-what) (values (floor ,coord+.5))))))

(defmacro integerize-double-float-coordinate (coord)
  `(the fixnum (values (floor (+ (the double-float ,coord) .5d0)))))

(defmacro integerize-float-coordinate (coord)
  `(the fixnum (values (floor (+ (the float ,coord) .5)))))

;; replaces my-round
(defun integerize-coordinate (coord)
  (etypecase coord
    (fixnum coord)
    (single-float
      (integerize-single-float-coordinate coord))
    (double-float
      (integerize-double-float-coordinate coord))
    #-Imach
    (float
      (integerize-float-coordinate coord))
    (ratio
      (values (floor (+ coord 1/2))))
    ;; disallow bignums and other types of numbers
    ))

(defmacro with-fast-vector-references ((&rest macros-and-arrays) &body body)
  (flet ((simple-part-accessor (array)
	   ;; Allegro only allows SVREF on simple T vectors.
	   #+excl `(let ((temp ,array))
		     (etypecase temp
		       (simple-vector temp)
		       ((vector t) (let ((temp2 (excl::ah_data temp)))
				     (setq temp2 (if (consp temp2)
						     (cdr temp2)
						   temp2))
				     (assert (and 
					       (zerop (the fixnum
							   (excl::ah_displacement temp)))
					       (typep temp2 'simple-vector))
					     ()
					     "Arrays passed to ~S must be non-displaced"
					     'with-fast-vector-references)
				     temp2))))
	   #+Genera array
	   #-(or excl Genera) array)
	 (internal-binding-declarations (variables)
	   #+excl `(declare (simple-vector ,@variables))
	   #+Genera `(declare (sys:array-register ,@variables))
	   #-(or excl Genera) `(declare)))
    (let* ((aref #+(or excl Genera) 'svref
		 #-(or excl Genera) 'aref)
	   (macro-names (mapcar #'first macros-and-arrays))
	   (internal-variables (mapcar #'gensymbol macro-names))
	   (arrays (mapcar #'second macros-and-arrays))
	   (bindings (mapcar #'(lambda (variable array)
				 `(,variable ,(simple-part-accessor array)))
			     internal-variables arrays))
	   (macros (mapcar #'(lambda (macro-name variable)
			       `(,macro-name (index) 
				  `(,',aref ,',variable (the fixnum ,index))))
			   macro-names internal-variables)))
  `(let ,bindings
     ,(internal-binding-declarations internal-variables)
     (progn ,@internal-variables nil)
     (macrolet ,macros
       ,@body)))))


#||
;;;
;;; Multiple value SETFs
;;;

#+PCL
(defun make-setf-function-name (accessor-name)
  (pcl::get-setf-function-name accessor-name))

#-PCL
(defun make-setf-function-name (accessor-name)
  (values `(setf ,accessor-name)
	  t))

#-PCL
(defun make-setf*-function-name (accessor-name)
  (declare (values setf-function-name defsetf-done-p))
  (let ((writer (get accessor-name 'setf-function-name))
	(old-p nil))
      (when writer
	(ignore-errors
	  (multiple-value-bind (vars vals store-vars store-form access-form)
	      (get-setf-method-multiple-value `(,accessor-name foo))
	    (declare (ignore vars vals store-vars access-form))
	    (when (or (equal (first store-form) writer)
		      (and (eql (first store-form) 'funcall)
			   (eql (first (second store-form)) 'function)
			   (equal (second (second store-form)) writer)))
	      (setf old-p t))))
	(return-from make-setf*-function-name (values writer old-p)))
      (values (setf (get accessor-name 'setf-function-name)
		    (intern (format nil "SETF* ~A:~S" 
                                    (package-name (symbol-package accessor-name))
                                    accessor-name)
                            (find-package 'clim-utils))
		    ;; --- There appears to be a Genera bug in that
		    ;; --- if you define a method named (setf foo) it goes
		    ;; --- ahead and redefines the SETF method for FOO.
		    #+Ignore
		    `(setf ,accessor-name))
	      nil)))

#+PCL
(defun make-setf*-function-name (accessor-name)
  (declare (values setf-function-name defsetf-done-p))
  (values (pcl::get-setf-function-name accessor-name)
	  (pcl::setfboundp accessor-name)))

(defmacro defgeneric* (function-spec lambda-list &body options)
  (assert (and (listp function-spec)
	       (eql (first function-spec) 'setf)
	       (null (cddr function-spec)))
	  ()
	  "Syntax error in ~S: This only works on ~S generic functions" 'defgeneric* 'setf)
  (let* ((accessor-name (second function-spec))
	 (accessor-arg (first (last lambda-list)))
	 (setf-function-name (make-setf*-function-name accessor-name)))
    `(define-group ,function-spec defgeneric*
       (defgeneric ,function-spec ,lambda-list ,@options)
       ,(expand-defsetf-for-defmethod* accessor-name accessor-arg
				       lambda-list setf-function-name))))

(defmacro defmethod* (name &body quals-lambda-list-and-body)
  (declare (arglist name [qualifiers]* lambda-list &body body))
  #+Genera (declare (zwei:indentation . #-PCL zwei:indent-for-clos-defmethod
					#+PCL pcl::indent-clos-defmethod))
  (assert (and (listp name) (eql (first name) 'setf) (null (cddr name))) ()
	  "Syntax error in ~S: This only works on ~S methods" 'defmethod* 'setf)
  (let (qualifiers real-arglist body accessor-arg
		   (accessor-name (second name)))
    (multiple-value-bind (setf-function-name old-p)
	(make-setf*-function-name accessor-name)
      (do ((qllab quals-lambda-list-and-body (cdr qllab)))
	  ((not (symbolp (first qllab)))
	   (setf qualifiers (nreverse qualifiers)
		 real-arglist (first qllab)
		 accessor-arg (let ((arg (first (last real-arglist))))
				(if (listp arg) (first arg) arg))
		 body (cdr qllab)))
	(push (first qllab) qualifiers))
      `(progn ,(unless old-p		;Don't write same SETF method again.
		 (expand-defsetf-for-defmethod* accessor-name accessor-arg
						real-arglist setf-function-name))
	      (defmethod ,setf-function-name ,@qualifiers ,real-arglist ,@body)))))

(defun expand-defsetf-for-defmethod*
       (accessor-name accessor-arg real-arglist setf-function-name)
  `(define-setf-method ,accessor-name (,accessor-arg)	;Only last one is real.
     (flet ((make-temp (name) (gensymbol name 'temp)))
       (let ((temps (list (make-temp ',accessor-arg)))
	     (store-temps (mapcar #'make-temp ',(butlast real-arglist))))
	 (values temps (list ,accessor-arg) store-temps
		 `(funcall #',',setf-function-name ,@store-temps ,@temps)
		 `(,',accessor-name ,@temps))))))

(defmacro setf* (place expr &rest more-pairs)
  (if more-pairs
      `(progn 
	 (setf* ,place ,expr)
	 (setf* ,@more-pairs))
      (multiple-value-bind (tvars vals svars store access)
	  (get-setf-method-multiple-value place)
	(declare (ignore access))
	`(let* (,@(mapcar #'list tvars vals))
	   (multiple-value-bind ,svars
	       ,expr
	     ,store)))))
||#


;;;
;;; Generates macros for accessing a slot used as a property list.
;;;

(defmacro def-property-slot-macros (name (class) slot-name)
  (declare (ignore class))
  `(progn
     (defmacro ,name (object key)
       `(getf (slot-value ,object ',',slot-name) ,key))
     (defsetf ,name (object key) (val)
       `(setf 
	 (getf (slot-value ,object ',',slot-name) ,key) 
	 ,val))))

(defmacro def-property-slot-accessors (name (class) slot-name)
  `(progn
     (defmethod ,name ((object ,class) key)
       (getf (slot-value object ',slot-name) key))
     (defmethod (setf ,name) (val (object ,class) key)
       (setf (getf (slot-value object ',slot-name) key) val))))

;;;
;;; Stub Sealers 
;;;

(defun unimplemented (&optional (string "So who you gun-na call..."))
  (error "Unimplemented: ~s." string))

(defmacro define-unimplemented-protocol-method (name protocol-name lambda-list)
  (let ((variables (with-collection
		       (dolist (var lambda-list)
			 (unless (member var lambda-list-keywords)
			   (collect (if (consp var) (car var) var))))))
	(protocol-var (first (find-if #'(lambda (lambda-var)
					  (and (consp lambda-var)
					       (eql (second lambda-var)
						    protocol-name)))
				      lambda-list))))
    `(defmethod ,name ,lambda-list
		#+Genera (declare 
			  (sys:function-parent ,name
					       define-unimplemented-protocol-method))
       (progn ,@variables)		;Ignore these variables...
       (error "The required operation ~S is not defined for the~
	       ~@[ ~S implementation of~] protocol ~S"
	      ',name
	      ,(when protocol-var `(class-name (class-of ,protocol-var)))
	      ',protocol-name))))

(defun warn-obsolete (fn)
  (warn "Obsoleted Call: ~a" fn))


;;;
;;; PROCESS & SYNCHRONIZATION
;;;

(defvar *multiprocessing-p* 
  #{
    (or excl Genera Lucid Lispworks) t
    otherwise nil
    }
    )
  
#+excl
(unless (excl::scheduler-running-p)
  (mp:start-scheduler))

(defmacro with-lockf ((place &optional state) &body forms)
  #+(or excl Xerox Genera ccl)
  (declare (ignore state #+ccl place))
  #{
    excl	`(mp:with-process-lock (,place) ,@forms)
    Lucid	`(lcl:with-process-lock (,place ,@(if state (cons state nil)))
		   ,@forms)
    lispworks	`(mp::with-lock (,place) ,@forms)
    Xerox	`(il:with.monitor ,place ,@forms)
    Cloe-Runtime `(progn ,@forms)
    Genera	`(process:with-lock (,place) ,@forms)
    Coral	`(progn ,@forms)
    }
  )

(defun initial-lock-value (&optional (lock-name "a Silica lock"))
  #-Genera (declare (ignore lock-name))
  #{
    excl	(mp::make-process-lock)
    lispworks	(mp::make-lock)
    Lucid	nil
    Coral	nil
    Xerox	(il:create.monitorlock)
    Cloe-Runtime nil
    Genera	(process:make-lock lock-name)
   }
  )

;;; A lock that CAN be relocked by the same process.
#-Genera
(defmacro with-simple-recursive-lock ((lock &optional (state "Unlock")) &body forms)
  `(flet ((foo () ,@forms))
     (declare (dynamic-extent #'foo))
     (invoke-with-simple-recursive-lock
       ,lock
       ,state
       #'foo)))

#-Genera
(defun invoke-with-simple-recursive-lock (place state continuation)
  (let ((store-value (current-process))
	(place-value (first place)))
    (if (and place-value (eql place-value store-value))
	(funcall continuation)
	(progn
	  (unless (null place-value)
	    (flet ((waiter ()
		     (null (first place))))
	      (declare (dynamic-extent #'waiter))
	      (process-wait state #'waiter)))
	  (unwind-protect
	      (progn (rplaca place store-value)
		     (funcall continuation))
	    (rplaca place nil))))))

(defmacro with-recursive-lockf ((place &optional state) &body forms)
  #+(or excl Xerox Genera ccl)
  (declare (ignore state #+ccl place))
  #{Genera `(process:with-lock (,place) ,@forms)
    Coral `(progn ,@forms)
    otherwise `(with-simple-recursive-lock (,place ,state) ,@forms)
    }
  )

(defun initial-recursive-lock-value (&optional (lock-name "a recursive Silica lock"))
  #-Genera (declare (ignore lock-name))
  #{coral nil
    Genera (process:make-lock lock-name :recursive t)
    otherwise (cons nil nil)
   }
  )

(defmacro without-scheduling (&body forms)
  "Evaluate the forms w/o letting any other process run."
  #{
    excl       `(mp:without-scheduling ,@forms)
    lispworks  `(sys::without-scheduling ,@forms)
    Lucid      `(lcl:with-scheduling-inhibited ,@forms)
    Xerox      `(progn ,@forms)
    Cloe-Runtime `(progn ,@forms)
    ;; should be process:with-no-other-processes if this is used as
    ;; a global locking mechanism
    Genera     `(scl:without-interrupts ,@forms)
    Coral      `(ccl:without-interrupts ,@forms) ; slh
   }
   )

(defun make-process (function &key name)
  #+(or ccl) (declare (ignore function name))
  (when *multiprocessing-p*
    #{
    lispworks  (mp:process-run-function name nil function)
    Lucid      (lcl:make-process :function function :name name)
    excl       (mp:process-run-function name function)
    Xerox      (il:add.process (funcall function) 'il:name name)
    Genera     (scl:process-run-function name function)
    otherwise  (warn "No implementation of MAKE-PROCESS for this system.")
    }))

(eval-when (compile load eval)
  (proclaim '(inline processp)))
(defun processp (object)
  #{
  ccl        (member object '(:user :event :interrupt))
  Lucid	     (lcl:processp object)
  excl	     (mp::process-p object)
  lispworks  (mp::process-p object)
  ;; In 7.3 and after it is `(process:process-p ,object)
  Genera     (scheduler-compatibility:process-p object)
  otherwise  (progn (warn "No implementation of PROCESSP for this system.")
		    nil)
  }
  )

(defun destroy-process (p)
  #+(or ccl) (declare (ignore p))
  #{
  Lucid      (lcl:kill-process p)
  excl	     (mp:process-kill p)
  lispworks  (mp:process-kill p)
  Xerox	     (il:del.process p)
  Genera     (scl:process-kill p)
  Coral	     nil
  otherwise  (warn "No implementation of DESTROY-PROCESS for this system.")
  }
  )

#+coral
(defvar *current-process* :user)

(eval-when (compile load eval)
  (proclaim '(inline current-process)))
(defun current-process ()
  #{
  Lucid      lcl:*current-process*
  excl	     mp:*current-process*
  lispworks  mp:*current-process*
  Xerox	     (il:this.process)
  Genera     scl:*current-process*
  coral	     *current-process*
  Cloe-Runtime nil
  }
  )

(eval-when (compile load eval)
  (proclaim '(inline all-processes)))
(defun all-processes ()
  #{
  Lucid      lcl:*all-processes*
  excl	     mp:*all-processes*
  lispworks  (mp::list-all-processes)
  Genera     sys:all-processes
  Coral	     (adjoin *current-process* '(:user))
  Cloe-Runtime nil
  }
  )

(defun show-processes ()
  #{
       Lucid	  (lcl::show-processes)
       Genera	  (si:com-show-processes)
       otherwise  (all-processes)
  }
  )
  
(eval-when (compile load eval)
  (proclaim '(inline process-yield)))

(defun process-yield ()
  #+Lucid      (lcl:process-allow-schedule)
  #+excl	     (mp:process-allow-schedule)
  #+lispworks  (mp::process-allow-scheduling)
  #+Xerox	     (il:block)
  #+Genera     (scl:process-allow-schedule)
  #+Coral	     (ccl:event-dispatch)
  #+Cloe-Runtime nil
  )

(defun process-wait (wait-reason predicate)
  "Cause the current process to go to sleep until the predicate returns TRUE."
  #{
  Lucid      (lcl:process-wait wait-reason predicate)
  excl	     (mp:process-wait wait-reason predicate)
  lispworks  (mp:process-wait wait-reason predicate)
  Xerox	     (let ((il:*who-line-state* wait-reason))
	       (loop
		 (il:block)
		 (when (and (funcall predicate))
		   (return))))
  Genera     (scl:process-wait wait-reason predicate)
  Coral	     (ccl::process-wait wait-reason predicate)
  Cloe-Runtime nil
  otherwise  (progn (compile-time-warn "Need an implementation for PROCESS-WAIT")
		    (error "~S doesn't have a definition.  Args are ~S ~S"
			   'process-wait wait-reason predicate))
  }
  )

(defun process-wait-with-timeout (wait-reason timeout predicate)
  "Cause the current process to go to sleep until the predicate returns TRUE or
   timeout seconds have gone by." 
  (when (null timeout)
    ;; ensure genera semantics, timeout = NIL means indefinite timeout
    (return-from process-wait-with-timeout
      (process-wait wait-reason predicate)))
  #{
  excl	     (mp:process-wait-with-timeout wait-reason timeout predicate)
  lispworks  (mp:process-wait-with-timeout wait-reason timeout predicate)
  Lucid	     (lcl:process-wait-with-timeout wait-reason timeout predicate)
  Genera     (sys:process-wait-with-timeout wait-reason (* timeout 60.) predicate)
  Coral	     (ccl::process-wait-with-timeout wait-reason timeout predicate)
  otherwise  (progn (compile-time-warn "Need an implementation for process-wait-with-timeout")
		    (error "~S doesn't have a definition.  Args are ~S ~S ~S"
			   'process-wait-with-timeout timeout wait-reason predicate))
  }
  )

(defun process-interrupt (process closure)
  (declare #+Coral (ignore process))
  #{
  lucid     (lcl:interrupt-process process closure)
  excl	    (mp:process-interrupt process closure)
  ;; ---    Is Lispworks' the same as Allegro?
  ;; ---    It is for everything else except ALL-PROCESSES.
  Genera    (scl:process-interrupt process closure)
  Coral     (let ((*current-process* :interrupt))
	      (funcall closure))
  otherwise (progn
	      (compile-time-warn "Need an implementation for process-interrupt")
	      (error "~S doesn't have a definition.  Args are ~S ~S"
		     'process-interrupt process closure))
  }
  )
