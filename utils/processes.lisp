;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: processes.lisp,v 1.9 92/09/08 10:34:30 cer Exp $

(in-package :clim-utils)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1988, 1989, 1990 International Lisp Associates.
 Portions copyright (c) 1992 Franz, Inc."

;;; Locks

(defvar *multiprocessing-p* 
  #{
    (or Allegro Genera Lucid Lispworks Minima) T
    otherwise nil
    }
    )
  
#+Allegro
(unless (excl::scheduler-running-p)
  (mp:start-scheduler))

(defmacro with-lock-held ((place &optional state) &body forms)
  #+(or Allegro Xerox Genera ccl Minima)
  (declare (ignore state #+ccl place))
  #{
    Allegro	`(mp:with-process-lock (,place) ,@forms)
    Lucid	`(lcl:with-process-lock (,place ,@(if state (cons state nil)))
		   ,@forms)
    lispworks	`(mp::with-lock (,place) ,@forms)
    Xerox	`(il:with.monitor ,place ,@forms)
    Cloe-Runtime `(progn ,@forms)
    Genera	`(process:with-lock (,place) ,@forms)
    Minima	`(minima:with-lock (,place) ,@forms)
    CCL-2	`(progn ,@forms)
    }
  )

(defun make-lock (&optional (lock-name "a CLIM lock"))
  #-(or Genera Minima Allegro) (declare (ignore lock-name))
  #{
    Allegro	(mp::make-process-lock :name lock-name)
    lispworks	(mp::make-lock)
    Lucid	nil
    CCL-2	nil
    Xerox	(il:create.monitorlock)
    Cloe-Runtime nil
    Genera	(process:make-lock lock-name)
    Minima	(minima:make-lock lock-name)
   }
  )

;;; A lock that CAN be relocked by the same process.
#-(or Genera Minima)
(defmacro with-simple-recursive-lock ((lock &optional (state "Unlock")) &body forms)
  `(flet ((foo () ,@forms))
     (declare (dynamic-extent #'foo))
     (invoke-with-simple-recursive-lock ,lock ,state #'foo)))

#-(or Genera Minima)
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

(defmacro with-recursive-lock-held ((place &optional state) &body forms)
  #+(or Allegro Xerox Genera ccl Minima)
  (declare (ignore state #+ccl place))
  #{Genera `(process:with-lock (,place) ,@forms)
    Minima `(minima:with-lock (,place) ,@forms)
    CCL-2 `(progn ,@forms)
    otherwise `(with-simple-recursive-lock (,place ,state) ,@forms)
    }
  )

(defun make-recursive-lock (&optional (lock-name "a recursive CLIM lock"))
  #-(or Genera Minima) (declare (ignore lock-name))
  #{CCL-2 nil
    Genera (process:make-lock lock-name :recursive T)
    Minima (minima:make-lock lock-name :recursive T)
    otherwise (cons nil nil)
   }
  )


;;; Atomic operations

(defmacro without-scheduling (&body forms)
  "Evaluate the forms w/o letting any other process run."
  #{
    Allegro    `(excl:without-interrupts ,@forms) 
    lispworks  `(sys::without-scheduling ,@forms)
    Lucid      `(lcl:with-scheduling-inhibited ,@forms)
    Xerox      `(progn ,@forms)
    Cloe-Runtime `(progn ,@forms)
    ;; should be process:with-no-other-processes if this is used as
    ;; a global locking mechanism
    Genera     `(scl:without-interrupts ,@forms)
    Minima     `(minima:with-no-other-processes ,@forms)
    CCL-2      `(ccl:without-interrupts ,@forms) ; slh
   }
   )

;; Atomically increments a fixnum value
#+Genera
(defmacro atomic-incf (reference &optional (delta 1))
  (let ((location '#:location)
	(old-value '#:old)
	(new-value '#:new))
    `(loop with ,location = (scl:locf ,reference)
	   for ,old-value = (scl:location-contents ,location)
	   for ,new-value = (sys:%32-bit-plus ,old-value ,delta)
	   do (when (scl:store-conditional ,location ,old-value ,new-value)
		(return ,new-value)))))

#-Genera
(defmacro atomic-incf (reference &optional (delta 1))
  (let ((value '#:value))
    (if (= delta 1)
	`(without-scheduling 
	   (let ((,value ,reference))
	     (if (eq ,value most-positive-fixnum)
		 (setf ,reference most-negative-fixnum)
		 (setf ,reference (the fixnum (1+ (the fixnum ,value)))))))
	(warn "Implement ~S for the case when delta is not 1" 'atomic-incf))))

;; Atomically decrements a fixnum value
#+Genera
(defmacro atomic-decf (reference &optional (delta 1))
  (let ((location '#:location)
	(old-value '#:old)
	(new-value '#:new))
    `(loop with ,location = (scl:locf ,reference)
	   for ,old-value = (scl:location-contents ,location)
	   for ,new-value = (sys:%32-bit-difference ,old-value ,delta)
	   do (when (scl:store-conditional ,location ,old-value ,new-value)
		(return ,new-value)))))

#-Genera
(defmacro atomic-decf (reference &optional (delta 1))
  (let ((value '#:value))
    (if (= delta 1)
	`(without-scheduling 
	   (let ((,value ,reference))
	     (if (eq ,value most-negative-fixnum)
		 (setf ,reference most-positive-fixnum)
		 (setf ,reference (the fixnum (1- (the fixnum ,value)))))))
	(warn "Implement ~S for the case when delta is not 1" 'atomic-decf))))


;;; Processes

(defun make-process (function &key name)
  #+(or ccl) (declare (ignore function name))
  (when *multiprocessing-p*
    #{
    lispworks  (mp:process-run-function name nil function)
    Lucid      (lcl:make-process :function function :name name)
    Allegro    (mp:process-run-function name function)
    Xerox      (il:add.process (funcall function) 'il:name name)
    Genera     (scl:process-run-function name function)
    Minima     (minima:make-process name :initial-function function)
    otherwise  (warn "No implementation of MAKE-PROCESS for this system.")
    }))

(eval-when (compile load eval) (proclaim '(inline processp)))
(defun processp (object)
  #{
  ccl        (member object '(:user :event :interrupt))
  Lucid	     (lcl:processp object)
  Allegro    (mp::process-p object)
  lispworks  (mp::process-p object)
  ;; In 7.3 and after it is `(process:process-p ,object)
  Genera     (process:process-p object)
  Minima     (typep object 'minima-internals::basic-process)
  otherwise  (progn (warn "No implementation of PROCESSP for this system.")
		    nil)
  }
  )

(defun destroy-process (p)
  #+(or ccl) (declare (ignore p))
  #{
  Lucid      (lcl:kill-process p)
  Allegro    (mp:process-kill p)
  lispworks  (mp:process-kill p)
  Xerox	     (il:del.process p)
  Genera     (scl:process-kill p)
  Minima     (minima:process-kill p)
  CCL-2	     nil
  otherwise  (warn "No implementation of DESTROY-PROCESS for this system.")
  }
  )

#+CCL-2
(defvar *current-process* :user)

(eval-when (compile load eval) (proclaim '(inline current-process)))
(defun current-process ()
  #{
  Lucid      lcl:*current-process*
  Allegro    mp:*current-process*
  lispworks  mp:*current-process*
  Xerox	     (il:this.process)
  Genera     scl:*current-process*
  Minima     (minima:current-process)
  CCL-2	     *current-process*
  Cloe-Runtime nil
  }
  )

(eval-when (compile load eval) (proclaim '(inline all-processes)))
(defun all-processes ()
  #{
  Lucid      lcl:*all-processes*
  Allegro    mp:*all-processes*
  lispworks  (mp::list-all-processes)
  Genera     sys:all-processes
  CCL-2	     (adjoin *current-process* '(:user))
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
  
(eval-when (compile load eval) (proclaim '(inline process-yield)))
(defun process-yield ()
  #{
  Lucid      (lcl:process-allow-schedule)
  Allegro    (mp:process-allow-schedule)
  lispworks  (mp::process-allow-scheduling)
  Xerox	     (il:block)
  Genera     (scl:process-allow-schedule)
  Minima     (sleep 1/10)
  CCL-2	     (ccl:event-dispatch)
  Cloe-Runtime nil
  }
  )

(defun process-wait (wait-reason predicate)
  #+(or Genera Minima) (declare (dynamic-extent predicate))
  "Cause the current process to go to sleep until the predicate returns TRUE."
  #{
  Lucid      (lcl:process-wait wait-reason predicate)
  Allegro    (mp:process-wait wait-reason predicate)
  lispworks  (mp:process-wait wait-reason predicate)
  Xerox	     (let ((il:*who-line-state* wait-reason))
	       (loop
		 (il:block)
		 (when (and (funcall predicate))
		   (return))))
  CCL-2	     (ccl::process-wait wait-reason predicate)
  Cloe-Runtime nil
  Genera     (scl:process-wait wait-reason predicate)
  Minima     (minima:process-wait wait-reason predicate)
  otherwise  (progn (compile-time-warn "Need an implementation for PROCESS-WAIT")
		    (error "~S doesn't have a definition.  Args are ~S ~S"
			   'process-wait wait-reason predicate))
  }
  )

(defun process-wait-with-timeout (wait-reason timeout predicate)
  #+(or Genera Minima) (declare (dynamic-extent predicate))
  "Cause the current process to go to sleep until the predicate returns TRUE or
   timeout seconds have gone by." 
  (when (null timeout)
    ;; ensure genera semantics, timeout = NIL means indefinite timeout
    (return-from process-wait-with-timeout
      (process-wait wait-reason predicate)))
  #{
  Allegro    (mp:process-wait-with-timeout wait-reason timeout predicate)
  lispworks  (mp:process-wait-with-timeout wait-reason timeout predicate)
  Lucid	     (lcl:process-wait-with-timeout wait-reason timeout predicate)
  Genera     (sys:process-wait-with-timeout wait-reason (* timeout 60.) predicate)
  CCL-2	     (ccl::process-wait-with-timeout wait-reason timeout predicate)
  otherwise  (progn (compile-time-warn "Need an implementation for process-wait-with-timeout")
		    (error "~S doesn't have a definition.  Args are ~S ~S ~S"
			   'process-wait-with-timeout timeout wait-reason predicate))
  }
  )

(defun process-interrupt (process closure)
  (declare #+CCL-2 (ignore process))
  #{
  Lucid     (lcl:interrupt-process process closure)
  Allegro   (mp:process-interrupt process closure)
  lispworks (mp:process-interrupt process closure)
  Genera    (scl:process-interrupt process closure)
  CCL-2     (let ((*current-process* :interrupt))
	      (funcall closure))
  Minima    (minima:process-interrupt process closure)
  otherwise (progn
	      (compile-time-warn "Need an implementation for process-interrupt")
	      (error "~S doesn't have a definition.  Args are ~S ~S"
		     'process-interrupt process closure))
  }
  )
