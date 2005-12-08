;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2004 Franz Inc, Oakland, CA - All rights reserved.
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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: lisp-utilities.lisp,v 2.6 2005/12/08 21:25:47 layer Exp $

(in-package :clim-utils)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

;;; Define useful tools that don't exist in vanilla CL.

(defvar *keyword-package* (find-package :keyword))

;;; FUNCTIONP doesn't do what we want and there isn't any other CL function
;;; that does.
(defun funcallable-p (thing)
  (etypecase thing
    (symbol (fboundp thing))
    (function t)))

;;; ONCE-ONLY does the same thing as it does in zetalisp.  I should have just
;;; lifted it from there but I am honest.  Not only that but this one is
;;; written in Common Lisp.  I feel a lot like bootstrapping, or maybe more
;;; like rebuilding Rome.
;; Variables can have &ENVIRONMENT ENV in it
(defmacro once-only (variables &body body)
  (let ((gensym-var (gensym))
        (run-time-vars (gensym))
        (run-time-vals (gensym))
        (expand-time-val-forms ()))
    (multiple-value-bind (variables #+(or Genera Minima) environment-var)
	(decode-once-only-arguments variables)
      (dolist (var variables)
	(push `(if (or (constantp ,var #+(or Genera Minima) ,environment-var)
		       (symbolp ,var))
		   ,var
		   (let ((,gensym-var (gensym)))
		     (push ,gensym-var ,run-time-vars)
		     (push ,var ,run-time-vals)
		     ,gensym-var))
	      expand-time-val-forms))
      `(let* (,run-time-vars
	      ,run-time-vals
	      (wrapped-body
		(let ,(mapcar #'list variables (reverse expand-time-val-forms))
		  ,@body)))
	 `(let ,(mapcar #'list (reverse ,run-time-vars)
			       (reverse ,run-time-vals))
	    ,wrapped-body)))))

(eval-when (compile load eval)
(defun decode-once-only-arguments (variables)
  (let ((vars nil)
	(env nil)
	(vl variables))
    (loop
      (when (null vl)
	(return-from decode-once-only-arguments
	  (values (nreverse vars) env)))
      (let ((var (pop vl)))
	(if (eq var '&environment)
	    (setq env (pop vl))
	    (push var vars))))))
)	;eval-when


(defmacro dorest ((var list &optional (by 'cdr)) &body body)
  `(do ((,var ,list (,by ,var)))
       ((null ,var) nil)
     ,@body))

;; Why is there DOLIST in CL but no DOVECTOR or DOSEQ{uence}


;; rfe5546 - evaluate the from-end argument at run-time
;;   This is a deviation from the old definition which evaluated from-end 
;;   (twice) at compile-time.
;;   All CLIM uses are compile-time constants. 
;;   Any non-contant usage would have generaed a compiler error or very
;;   incorrect code.
(defmacro dovector ((var vector &key (start 0) end from-end simple-p) 
		    &body body)
  (unless (constantp simple-p)
    (setq simple-p nil)
    (warn "SIMPLE-P should be a constant, ignoring it"))
  (when (and simple-p (null end))
    (warn "When SIMPLE-P is T, you must supply :END"))
  (let ((fvector '#:vector)
	(startd  '#:start)
	(endd    '#:end)
	(limit   '#:limit)
	(fromend '#:from-end)
	(variable (if (atom var) var (first var)))
	(index    (if (atom var) '#:index (second var)))
	(aref (if simple-p 'svref 'aref)))
    `(block nil
       (let* ((,fvector ,vector)
	      (,startd ,start)
	      (,endd ,(if simple-p 
			  `,end 
			`(or ,end (length ,fvector))))
	      (,fromend ,from-end)
	      (,index (if ,fromend (1- ,endd) ,startd))
	      (,limit (if ,fromend (1- ,startd) ,endd)))
	 (declare (type fixnum ,endd ,index ,limit)
		  ;; Turn on the afterburners...
		  (optimize (speed 3) (safety 0))
		  ,(if simple-p
		       `(type simple-vector ,fvector)
		     `(type vector ,fvector)))
	 (loop
	   (when (= ,index ,limit) (return))
	   (let ((,variable (,aref ,fvector ,index)))
	     ,@body)
	   (if ,fromend (decf ,index) (incf ,index)))))))

;;The old definition
#+ignore
(defmacro dovector ((var vector &key (start 0) end from-end simple-p) &body body)
  (unless (constantp simple-p)
    (setq simple-p nil)
    (warn "SIMPLE-P should be a constant, ignoring it"))
  (when (and simple-p (null end))
    (warn "When SIMPLE-P is T, you must supply :END"))
  (let ((fvector '#:vector)
	(startd  '#:start)
	(endd    '#:end)
	(limit   '#:limit)
	(variable (if (atom var) var (first var)))
	(index    (if (atom var) '#:index (second var)))
	(aref (if simple-p 'svref 'aref)))
    `(block nil
       (let* ((,fvector ,vector)
	      (,startd ,start)
	      (,endd ,(if simple-p `,end `(or ,end (length ,fvector))))
	      (,index (if ,from-end (1- ,endd) ,startd))
	      (,limit (if ,from-end (1- ,startd) ,endd)))
	 (declare (type fixnum ,endd ,index ,limit)
		  ;; Turn on the afterburners...
		  (optimize (speed 3) (safety 0))
		  ,(if simple-p
		       `(type simple-vector ,fvector)
		       `(type vector ,fvector)))
	 (loop
	   (when (= ,index ,limit) (return))
	   (let ((,variable (,aref ,fvector ,index)))
	     ,@body)
	   (,(if from-end 'decf 'incf) ,index))))))

(defmacro doseq ((var sequence) &body body)
  (let ((fcn (gensymbol 'doseq)))
    `(flet ((,fcn (,var) ,@body))
       (etypecase ,sequence
	 (list (dolist (thing ,sequence) (,fcn thing)))
	 (vector (dovector (thing ,sequence) (,fcn thing)))))))


;;; Stuff for dealing with CLIM coordinates

(deftype coordinate ()
  #+use-float-coordinates  'single-float
  #+use-fixnum-coordinates 'fixnum
  #-(or use-float-coordinates use-fixnum-coordinates) 't)

;; Convert a number of arbitrary type into a COORDINATE
(defmacro coordinate (x &optional (round-direction 'round))
  #-use-fixnum-coordinates (declare (ignore round-direction))
  #+use-float-coordinates `(the coordinate (float ,x 0f0))
  #+use-fixnum-coordinates (ecase round-direction
			     (round
			       #+(or allegro Lucid) `(the fixnum (round ,x))
			       #-(or allegro Lucid) `(the fixnum (values (floor (+ ,x .5f0)))))
			     (floor
			       `(the fixnum (values (floor ,x))))
			     (ceiling
			       `(the fixnum (values (ceiling ,x)))))
  #-(or use-float-coordinates use-fixnum-coordinates) `,x)

#-(or aclpc acl86win32)
(defconstant +largest-coordinate+
    #+use-float-coordinates (float (expt 10 (floor (log most-positive-fixnum 10))) 0f0)
    #+use-fixnum-coordinates most-positive-fixnum
    #-(or use-float-coordinates use-fixnum-coordinates) most-positive-fixnum)

#+(or aclpc acl86win32)
(defconstant +largest-coordinate+
  #+use-float-coordinates (float (* 3 (expt 10 (floor (log most-positive-fixnum 10)))) 0f0)
  #+use-fixnum-coordinates (* 3 (expt 10 (floor (log most-positive-fixnum 10))))
  #-(or use-float-coordinates use-fixnum-coordinates)
  (* 3 (expt 10 (floor (log most-positive-fixnum 10)))))

(defmacro integerize-single-float-coordinate (coord)
  `(the fixnum (values (floor (+ (the single-float ,coord) .5f0)))))

(defmacro integerize-double-float-coordinate (coord)
  `(the fixnum (values (floor (+ (the double-float ,coord) .5d0)))))

(defmacro integerize-float-coordinate (coord)
  `(the fixnum (values (floor (+ (the float ,coord) .5)))))

#+(or Genera Minima)
(defun-inline fix-coordinate (coord)
  (if (typep coord 'fixnum)
      coord
      (the fixnum (values (floor (+ coord .5f0))))))

#+allegro
(defmacro fix-coordinate (coord)
  `(the fixnum (if (excl:fixnump ,coord) ,coord (fixit ,coord))))

#+allegro
(defun fixit (coord)
  (declare (optimize (speed 3) (safety 0)))
  (typecase coord
    (fixnum
     coord)
    (single-float
     ;; note we use [x+0.5] rather than ROUND because
     ;; (1+ (round 1.5)) != (round 2.5) (cim 8/6/94)
     (values (the fixnum (floor (+ (the single-float coord) .5f0)))))
    (double-float
     (values (the fixnum (floor (+ (the double-float coord) .5f0)))))
    (t
     (values (floor (+ coord .5f0))))))

#-(or Genera Minima allegro)
(defun fix-coordinate (coord)
  (etypecase coord
    (fixnum coord)
    (single-float
      (integerize-single-float-coordinate coord))
    (double-float
      (integerize-double-float-coordinate coord))
    (float
      (integerize-float-coordinate coord))
    (ratio
      (values (floor (+ coord 1/2))))
    ;; disallow bignums and other types of numbers
    ))

(defmacro fix-coordinates (&body coords)
  `(progn
     ,@(mapcar #'(lambda (x) `(setq ,x (fix-coordinate ,x)))
	       coords)))

;; Assume that the value is a fixnum and that the result is a fixnum.
(defmacro fast-abs (int)
  (let ((value '#:value))
    `(let ((,value ,int))
       (declare (fixnum ,value))
       (the fixnum (if (< ,value 0) (the fixnum (- 0 ,value)) ,value)))))


;; COORDINATE-PAIRS is a list of pairs of coordinates.
;; The coordinates must be of type COORDINATE
(defmacro translate-coordinates (x-delta y-delta &body coordinate-pairs)
  (once-only (x-delta y-delta)
    `(progn
       ,@(let ((forms nil))
	   (dorest (pts coordinate-pairs cddr)
	     (push `(setf ,(first pts)
			  (the coordinate (+ ,(first pts)  ,x-delta))) forms)
	     (push `(setf ,(second pts)
			  (the coordinate (+ ,(second pts) ,y-delta))) forms))
	   (nreverse forms)))))

;; Warning: this macro evaluates its position arguments multiple times
(defmacro convert-to-device-coordinates (transform &body positions)
  (assert (evenp (length positions)) (positions)
	  "There must be an even number of elements in ~S" positions)
  (assert (every #'symbolp positions) (positions)
	  "Each of the positions must be a variable in ~S" positions)
  (let ((forms nil))
    (loop
      (when (null positions)
	(return `(progn ,@(nreverse forms))))
      (let* ((x (pop positions))
	     (y (pop positions)))
	(push `(progn
		 (multiple-value-setq (,x ,y)
		   (transform-position ,transform ,x ,y))
		 (fix-coordinates ,x ,y))
	      forms)))))

;; Warning: this macro evaluates its position arguments multiple times
(defmacro convert-to-device-distances (transform &body distances)
  (assert (evenp (length distances)) (distances)
	  "There must be an even number of elements in ~S" distances)
  (assert (every #'symbolp distances) (distances)
	  "Each of the distances must be a variable in ~S" distances)
  (let ((forms nil))
    (loop
      (when (null distances)
	(return `(progn ,@(nreverse forms))))
      (let* ((x (pop distances))
	     (y (pop distances)))
	(push `(progn
		 (multiple-value-setq (,x ,y)
		   (transform-distance ,transform ,x ,y))
		 (fix-coordinates ,x ,y))
	      forms)))))


;;; Characters that are ordinary text rather than potential input editor commands.
;;; Note that GRAPHIC-CHAR-P is true of #\Space
(defun ordinary-char-p (char)
  (and (eql char (code-char (char-code char))) ; false for #\control-c
       (or (graphic-char-p char)
	   ;; For characters, CHAR= and EQL are the same.  Not true of EQ!
	   (eql char #\Newline)
	   (eql char #\Return)
	   (eql char #\Tab))))

(defun whitespace-char-p (char)
  (and (characterp char)
       (or (char-equal char #\Space)
	   (eql char #\Tab))))

;;; Make sure we don't get screwed by environments like Coral's that
;;; have *print-case* set to :downcase by default.
#+(or (not ansi-90) aclpc)
(defvar *standard-io-environment-val-cache* nil)

#+(or (not ansi-90) aclpc)
(defun standard-io-environment-vars-and-vals ()
  (unless *standard-io-environment-val-cache*
    (setq *standard-io-environment-val-cache*
	  (list 10				;*read-base*
		(copy-readtable nil)		;*readtable*
		(find-package :user)		;*package*
		t				;*print-escape*
		nil				;*print-pretty*
		nil				;*print-radix*
		10				;*print-base*
		nil				;*print-circle*
		nil				;*print-level*
		nil				;*print-length*
		:upcase				;*print-case*
		t				;*print-gensym*
		t				;*print-array*
		nil)))				;*read-suppress*
  (values
    '(*read-base* *readtable* *package* *print-escape* *print-pretty*
      *print-radix* *print-base* *print-circle* *print-level* *print-length*
      *print-case* *print-gensym* *print-array* *read-suppress*)
    *standard-io-environment-val-cache*))

(defmacro with-standard-io-environment (&body body)
  #+(or (not ansi-90) aclpc)
  `(multiple-value-bind (vars vals)
       (standard-io-environment-vars-and-vals)
     (progv vars vals
       ,@body))
  #-(or (not ansi-90) aclpc)
  `(with-standard-io-syntax ,@body))

#+(and (or (not ansi-90) aclpc) (not Genera))
(defmacro with-standard-io-syntax (&body body)
  `(with-standard-io-environment ,@body))

;; Define this so we don't have to deal with stupid warnings about
;; the iteration variable being used, or not used, or what not
(defmacro repeat (n &body body)
  (let ((i '#:i))
    `(dotimes (,i ,n)
       #-(or Minima Genera allegro) (declare (ignore i))
       ,@body)))


;;; Have to provide CLIM-LISP:WITH-OPEN-STREAM, because it needs to use a different
;;; version of CLOSE.  We need this for printer streams, at least.

;;; This is a little more complicated than it absolutely needs to be.  The idea is
;;; that there should be as short a timing window during which the stream to be closed
;;; is open but we don't have our hands on it to pass to CLOSE.  We therefore don't
;;; want to bind the user's variable to the stream outside of the unwind-protect, but
;;; rather want to bind it inside.  The reason we need the temporary variable is
;;; because the user might declare the stream variable to be of type STREAM, which
;;; would not be true during the brief interval after the binding and before the
;;; setting of that variable.  I believe this implementation of WITH-OPEN-STREAM to
;;; have as small a timing window, and to be as semantically correct as possible.

#+(or (not clim-uses-lisp-stream-functions)	;Do this if we provide CLOSE function
      Genera					; Sigh.  CLOSE also shadowed for Genera.
      CCL-2)					; Sigh.  CLOSE also shadowed for CCL-2.
(defmacro with-open-stream ((stream-variable construction-form) &body body &environment env)
  (let ((aborted-variable (gensymbol 'aborted-p))
	(temporary-stream-variable (gensymbol 'stream)))
    (multiple-value-bind (documentation declarations actual-body)
	(extract-declarations body env)
      (declare (ignore documentation))
      `(let (,temporary-stream-variable
	     (,aborted-variable t))
       (unwind-protect
	   (multiple-value-prog1
	     (progn (setq ,temporary-stream-variable ,construction-form)
		    (let ((,stream-variable ,temporary-stream-variable))
		      ,@declarations
		      ,@actual-body))
	     (setf ,aborted-variable nil))
	 (when ,temporary-stream-variable
	   (close ,temporary-stream-variable :abort ,aborted-variable)))))))

(defun follow-synonym-stream (stream)
  #+Genera (si:follow-syn-stream stream)
  #+(and ansi-90 (not Genera)) (typecase stream
				 (synonym-stream
				   (symbol-value (synonym-stream-symbol stream)))
				 (t stream))
  #-(or Genera ansi-90) stream)

#-(or Genera ansi-90)
(eval-when (compile)
  (warn "You haven't defined ~S for this implementation.  A stub has been provided."
	'follow-synonym-stream))

;;; Interning. There are three places where the package matters here:
;;; the current package, the package that is current when FORMAT is
;;; called (matters for ~S &c), and the package into which the symbol
;;; is interned.  The aim is that the first two should always be the
;;; current packag, but you get to choose where the symbol is interned
;;; for PACKAGE-FINTERN.
;;;
;;; I am not sure if this stuff is really worth the cost, but...

;;; spr24505, carefully extract symbol-names, so
;;; case-sensitity works better.
(defun package-fintern (package format-string &rest format-args)
  ;; this argument order is unfortunate.
  (declare (dynamic-extent format-args))
  (intern (let ((pkg *package*))
	    (with-standard-io-environment
		(let ((*package* pkg))
		  (apply #'lisp:format () format-string
			 (mapcar #'(lambda (x)
				     (excl::if* (symbolp x)
					then (symbol-name x)
					else x))
				 format-args)))))
	  package))

(defun fintern (format-string &rest format-args)
  (declare (dynamic-extent format-args))
  (apply #'package-fintern *package* format-string format-args))


(defvar *gensymbol* 0)

(eval-when (compile load eval)
(defun gensymbol (&rest parts)
  (declare (dynamic-extent parts))
  (when (null parts) (setf parts '(gensymbol)))
  (make-symbol (lisp:format nil "~{~A-~}~D" parts (incf *gensymbol*))))
)	;eval-when

;;; For macro writers; you can have your GENSYMBOLs start at 1.  Use
;;; this in the macro, not in its expansion...
(defmacro with-related-gensymbols (&body body)
  `(let ((*gensymbol* 0))
     ,@body))

;; Used in generating internal function and method names.
;; (remove-word-from-string "com-" 'com-show-file) => "SHOW-FILE"
;; Always returns a new string that can be bashed to your heart's content
(defun remove-word-from-string (word string-or-symbol &optional only-from-beginning-p)
  (let ((string (etypecase string-or-symbol
		  (string string-or-symbol)
		  (symbol (string string-or-symbol)))))
    (let ((word-position (search word string :test #'char-equal)))
      (cond ((null word-position)
	     (concatenate 'string string))
	    ((zerop word-position)
	     (subseq string (length word)))
	    (only-from-beginning-p
	     (concatenate 'string string))
	    (t
	     (concatenate 'string
			  (subseq string 0 word-position)
			  (subseq string (+ word-position (length word)))))))))

;;; Why PUSHNEW doesn't do this is beyond me.
(defmacro push-unique (item reference &rest args &key test test-not key)
  (declare (ignore test test-not))
  `(let* ((evaled-item ,item)
	  (evaled-reference ,reference)
	  (element (find ,(if key `(funcall ,key evaled-item) 'evaled-item)
			 evaled-reference ,@args)))
     (setf ,reference
	   (if element
	       (substitute evaled-item element evaled-reference)
	       (cons evaled-item evaled-reference)))))

(defmacro catch-if (condition tag &body body)
  `(catch (if ,condition ,tag '#:tag-for-catch-if)
     ,@body))

;#+Genera
;(defmacro letf-globally (places-and-vals &body body)
;  `(sys:letf* ,places-and-vals ,@body))
;
;#-Genera
;;; can't hack return-from in macro for aclpc +++pr
(defmacro letf-globally (places-and-vals &body body)
  ;; I don't want to use LETF-globally, mind you, but I can't easily implement
  ;; LETF{-not-globally} without something like sys:%bind-location.
  ;; Of course, this one is really LETF*-GLOBALLY, but don't tell anyone.
  ;; A minor optimization: when you bind something to itself or don't
  ;;  say what to bind it to, it doesn't get SETF'd, since it isn't
  ;;  being changed.
 (if (null places-and-vals)
  `(progn ,@body)
  (let ((let-forms nil)
	(set-forms nil)
	(unwind-forms nil))
    ;; remember that we can't use SCL:LOOP
    (map nil #'(lambda (place-and-val)
		 (let* ((place (pop place-and-val))
			(val-p (not (null place-and-val)))
			(val (and val-p (pop place-and-val)))
			(temp-var (gensymbol 'letf-globally-temp)))
		   (when (and val-p (equal place val)) (setf val-p nil))   ;bind to itself?
		   (push `(,temp-var ,place) let-forms)
		   (when val-p (push place set-forms) (push val set-forms))
		   (push temp-var unwind-forms) (push place unwind-forms)))
	 places-and-vals)
    `(let ,(nreverse let-forms)
       (unwind-protect
	   (progn (setf ,@(nreverse set-forms)) ,@body)
	 (setf ,@unwind-forms))))))		;Undo backwards.

(defmacro letf-globally-if (condition places-and-vals &body body)
  #+Genera (declare (zwei:indentation 1 4 2 1))
  (when (null places-and-vals)
    (return-from letf-globally-if `(progn ,@body)))
  (let ((let-forms nil)
	(set-forms nil)
	(unwind-forms nil)
	(condition-value (gensymbol 'condition)))
    (map nil #'(lambda (place-and-val)
		 (let* ((place (pop place-and-val))
			(val-p (not (null place-and-val)))
			(val (and val-p (pop place-and-val)))
			(temp-var (gensymbol 'letf-globally-temp)))
		   (when (and val-p (equal place val)) (setf val-p nil))
		   (push `(,temp-var (and ,condition-value ,place)) let-forms)
		   (when val-p (push place set-forms) (push val set-forms))
		   (push temp-var unwind-forms) (push place unwind-forms)))
	 places-and-vals)
    `(let ((,condition-value ,condition))
       (let ,(nreverse let-forms)
	 (unwind-protect
	     (progn (when ,condition-value (setf ,@(nreverse set-forms)))
		    ,@body)
	   (when ,condition-value (setf ,@unwind-forms)))))))

#-(and ansi-90 (not allegro) (not Symbolics))
(eval-when (compile load eval)
  (proclaim '(declaration non-dynamic-extent)))

#+aclpc
(eval-when (compile load eval)
  (proclaim '(declaration non-dynamic-extent ignorable)))

#+(and ansi-90 (not allegro) (not aclpc) (not Symbolics))
(define-declaration non-dynamic-extent (spec env)
  (let ((vars (rest spec))
        (result nil))
    (dolist (v vars)
      (block process-var
        (multiple-value-bind (type local info)
                             (variable-information v env)
          (declare (ignore local))
          (case type
            (:lexical
             (when (cdr (assoc 'dynamic-extent info))
               (warn "The variable ~S has been declared ~S,~%it cannot be now declared ~S"
                     v 'dynamic-extent 'non-dynamic-extent)
               (return-from process-var))
             (when (cdr (assoc 'ignore info))
               (warn "The variable ~S has been declared ~S,~%it cannot be now declared ~S"
                     v 'ignore 'non-dynamic-extent)
               (return-from process-var))
             (push `(,v dynamic-extent nil) result))
            (otherwise
             (warn "~S is not a lexical variable, it cannot be declared ~S."
                   v 'non-dynamic-extent))))))
    (values :variable (nreverse result))))


(defun time-elapsed-p (delta-seconds start-time)
  (let ((internal-units (* delta-seconds internal-time-units-per-second))
	(internal-time (get-internal-real-time)))
    ;; don't worry about the clock wrapping around
    (> (- internal-time start-time) internal-units)))

(defmacro with-timeout-predicate (((predicate-fun old-predicate)
                                   (timeout-var timeout-val)) &body body)
  (let ((start-time (gensymbol "START-TIME"))
        (delta-time (gensymbol "DELTA-TIME")))
    `(let* ((,delta-time ,timeout-val) ;; Eval it only once...
            (,start-time (when  ,delta-time
                           (get-internal-time-units-per-second)))
            (,timeout-var nil))
       (flet ((,predicate-fun ()
                 (cond ((,old-predicate)
                        t)
                       ((and ,delta-time
                             (time-elapsed-p ,delta-time ,start-time))
                        (setq ,timeout-var t)
                        t)
                       (t nil))))
         ,@body))))


;;; Bindings on the stack.  A work in progress.
;;; Which Lisps support this?

;; I suppose this could be done with IMPORT
#+Genera
(progn
(defmacro with-stack-list ((var &rest elements) &body body)
  `(scl:with-stack-list (,var ,@elements) ,@body))

(defmacro with-stack-list* ((var &rest elements) &body body)
  `(scl:with-stack-list* (,var ,@elements) ,@body))

(defun-inline evacuate-list (list)
  (if (and (sys:%pointerp list)
	   (not (or (sys:%pointer-lessp list sys:%control-stack-low)
		    (sys:%pointer-lessp (progn #+3600  sys:%control-stack-limit
					       #+imach (sys:%read-internal-register
							 sys:%register-control-stack-limit))
					list))))
      (copy-list list)
      list))
)	;#+Genera

#+Cloe-Runtime
(progn
(defmacro with-stack-list ((var &rest elements) &body body)
  `(sys::with-stack-list (,var ,@elements) ,@body))

(defmacro with-stack-list* ((var &rest elements) &body body)
  `(sys::with-stack-list* (,var ,@elements) ,@body))

#+Cloe-Runtime
(defun-inline evacuate-list (list)
  (if (logtest (the fixnum (sys::gcltype list)) sys::lo$k-_astack)
      (copy-list list)
      list))
)	;#+Cloe-Runtime

#+allegro
(progn
(defmacro with-stack-list ((var &rest elements) &body body)
  `(let ((,var (list ,@elements)))
     (declare (dynamic-extent ,var))
     ,@body))

(defmacro with-stack-list* ((var &rest elements) &body body)
  `(let ((,var (list* ,@elements)))
     (declare (dynamic-extent ,var))
     ,@body))

(defun-inline evacuate-list (list)
  (if (and (consp list)
	   (excl::stack-allocated-p list))
      (copy-list list)
    list))

)	;#+Allegro

#+CCL-2
(progn
(defmacro with-stack-list ((var &rest elements) &body body)
  `(let ((,var (list ,@elements)))
     (declare (dynamic-extent ,var))
     ,@body))

(defmacro with-stack-list* ((var &rest elements) &body body)
  `(let ((,var (list* ,@elements)))
     (declare (dynamic-extent ,var))
     ,@body))

(defun evacuate-list (list)
  ;;--- Dunno if this is the right function to be calling
  ;;--- but it seems to give the right answers.
  (cond ((and (ccl::stack-area-endptr list)
              (listp list))
         (copy-list list))
        (t list)))
)	;#+CCL-2

#-(or Genera Cloe-Runtime allegro CCL-2)
(progn
(defmacro with-stack-list ((var &rest elements) &body body)
  `(let ((,var (list ,@elements)))
     ,@body))

(defmacro with-stack-list* ((var &rest elements) &body body)
  `(let ((,var (list* ,@elements)))
     ,@body))

;; Since with-stack-list does nothing, this doesn't either.
;; When stack-consing works for non-Genera/Cloe, make this do something.
(defmacro evacuate-list (list)
  `,list)
)	;#-(or Genera Cloe-Runtime allegro)

#+Genera
(defmacro with-stack-array ((name size &rest options) &body body)
  `(sys:with-stack-array (,name ,size ,@options) ,@body))

#-Genera
(defmacro with-stack-array ((name size &rest options) &body body)
  `(let ((,name (make-stack-array ,size ,@options)))
     ,@body))

#-Genera	;in case anybody wants to implement this...
(defun-inline make-stack-array (size &rest options)
  (declare (dynamic-extent options))
  (apply #'make-array size options))

#+(or Genera Cloe-Runtime)
(defmacro with-keywords-removed ((new-list list keywords-to-remove) &body body)
  (declare (zwei:indentation 0 3 1 1))
  `(si::with-rem-keywords (,new-list ,list ,keywords-to-remove)
     ,@body))

#+(or Genera Cloe-Runtime)
(defun remove-keywords (list keywords-to-remove)
  (si::rem-keywords list keywords-to-remove))

#-(or Genera Cloe-Runtime)
(progn
(defmacro with-keywords-removed ((new-list list keywords-to-remove) &body body)
  `(let ((,new-list (remove-keywords ,list ,keywords-to-remove)))
     ,@body))

(defun remove-keywords (list keywords)
  (macrolet ((remove-keywords-1 (name-var predicate-form)
	       `(let ((head nil)
		      (tail nil))
		  (do ()
		      ((null list))
		    (let ((,name-var (pop list))
			  (value (pop list)))
		      (unless ,predicate-form
			(setq tail (setq head (list ,name-var value)))
			(return))))
		  (do ()
		      ((null list) head)
		    (let ((,name-var (pop list))
			  (value (pop list)))
		      (unless ,predicate-form
			(setq tail (setf (cddr tail) (list ,name-var value)))))))))
    (cond ((null list) nil)
	  ((null keywords) list)
	  ;; Special case: use EQ instead of MEMBER when only one keyword is supplied.
	  ((null (cdr keywords))
	   (let ((keyword (car keywords)))
	     (remove-keywords-1 name (eq name keyword))))
	  (t
	   (remove-keywords-1 name (member name keywords))))))

)	;#-(or Genera Cloe-Runtime)


;;; Generate a list of keyword information for a given type of keyword.
;;; This is a list of lists of the form:
;;;      (name init-value keyword supplied-p-var &optional accessor-name)
;;; We also put values for the TYPE property of the keywords according
;;; to the incrementor so we can detect these keywords quickly at
;;; runtime.  The value on the property is either some fixed value, an
;;; integer which increments once per keyword, or a member of a set.  We
;;; currently use this facility for defining drawing-state keywords and
;;; transform keywords.  See the files GRAPHICS-TRANSFORM-MIXIN and
;;; DRAWING-STATE-MIXIN for how it's used.

(defmacro define-keywords (name type (incrementor start) accessor-prefix
			   &body names-and-values &aux (offset 0))
  (flet ((counter-incrementor () (incf offset))
	 (member-incrementor () (pop start))
	 (value-incrementor  () start))
    (let ((keyword-stuff (mapcar #'(lambda (n&v)
				     `(,(first n&v) ,(second n&v)
				       ,(intern (string (first n&v)) *keyword-package*)
				       ,(gensymbol (first n&v) 'p)
				       ,@(when accessor-prefix
					   `(,(fintern "~A-~A" accessor-prefix (first n&v))))))
				 names-and-values))
	  (incrementor (ecase incrementor
			 (counter #'counter-incrementor)
			 (member #'member-incrementor)
			 (value #'value-incrementor))))
      `(progn (defparameter ,name ',keyword-stuff)
	      ,@(when type
		  (mapcar #'(lambda (k)
			      `(setf (get ',(third k) ',type) ,(funcall incrementor)))
			  keyword-stuff))))))


(defmacro writing-clauses (&body body)
  (let ((clauses-var (gensymbol 'clauses)))
    `(let ((,clauses-var nil))
       (macrolet ((clause (clause)
		    `(push ,clause ,',clauses-var)))
	 ,@body)
       (nreverse ,clauses-var))))


;;; Arglist tools

;; (flatten-arglist '(foo bar &optional baz (quux)
;; 		          &rest mumble
;; 		          &key frotz (trouble) ((:izzy the-cat))))
;; (FOO BAR &OPTIONAL BAZ QUUX &REST MUMBLE &KEY FROTZ TROUBLE IZZY)
;; make-pass-on-arglist is the only caller
(defun flatten-arglist (arglist)
  (let ((new-arglist nil)
	(mode :positional))
    (dolist (arg-spec arglist)
      (cond ((listp arg-spec)
	     (case mode
	       (&optional
		 (push (first arg-spec) new-arglist))
	       (&key
		 ;; deal with "(... &key ((:keyword var) default))" syntax
		 (let ((thing (first arg-spec)))
		   (push (if (listp thing)
			     (let ((name (first thing)))
			       (if (eq (symbol-package name) *keyword-package*)
				   (intern (symbol-name name))
				   name))
			     thing)
			 new-arglist)))))
	    ((member arg-spec '(&key &rest &optional))
	     (setq mode arg-spec)
	     (push arg-spec new-arglist))
	    (t (push arg-spec new-arglist))))
    (nreverse new-arglist)))

;; (make-pass-on-arglist '(foo bar &optional baz (quux)
;; 		          &rest mumble
;; 		          &key frotz (trouble) ((:izzy the-cat))))
;; (FOO BAR BAZ QUUX MUMBLE :FROTZ FROTZ :TROUBLE TROUBLE ':IZZY THE-CAT)
;; --- It looks like &REST and &KEY don't get along well here.  A big
;; question in such a circumstane is who should be doing the defaulting?
;; I.e., should this do the defaulting, by making the above arglist be
;; 	:FROTZ FROTZ :TROUBLE TROUBLE ':IZZY THE-CAT MUMBLE
;; for apply, or should it let the eventual caller do the defaulting by
;; punting the defaulting here and just doing
;;	MUMBLE
;; for apply?  There are also interactions with &allow-other-keys that
;; aren't done here.  If this is for pass-on, then &allow-other-keys
;; implies an &rest beforehand, otherwise it is impossible to get all
;; the keys that are being passed on.  There's also the question of
;; whether &allow-other-keys here should put :allow-other-keys in the
;; passed on call list.
(defun make-pass-on-arglist (arglist)
  (let ((new-arglist nil)
	(fa (flatten-arglist arglist))
	(mode :positional)
	(apply-p nil))
    (do ((args fa (cdr args))
	 (original-args arglist (cdr original-args)))
	((null args) nil)
      (let ((arg (first args))
	    (arg-spec (first original-args)))
	(cond ((member arg '(&key &optional &rest))
	       (setq mode arg))
	      ((eq arg '&allow-other-keys)
	       (unless (eq mode '&key)
		 (error "~&&ALLOW-OTHER-KEYS must follow &KEY")))
	      (t (case mode
		   (&key
		     (let ((arg-name arg) (arg-var arg))
		       (cond ((and (listp arg-spec)
				   (listp (first arg-spec)))
			      (setq arg-name `',(first (first arg-spec)))
			      (setq arg-var (second (first arg-spec))))
			     (t (setq arg-name (intern (symbol-name arg) *keyword-package*))))
		       (push arg-name new-arglist)
		       (push arg-var new-arglist)))
		   (&rest (setq apply-p t)
			  (push arg new-arglist))
		   (t (push arg new-arglist)))))))
    (values
      (nreverse new-arglist)
      apply-p)))

(defun ignore-arglist (arglist)
  (flet ((lambda-list-element-compare (element and-option)
	   (and (atom element)
		(string= element and-option))))
    `(progn ,@(let ((args nil))
		(dolist (arg arglist)
		  ;; These various &keys may be in some other package.
		  (cond ((member arg '(&rest &downward-rest &key &allow-other-keys &optional)
				 :test #'lambda-list-element-compare)
			 nil)
			(t (push (cond ((atom arg) arg)
				       ((atom (car arg)) (car arg))
				       (t (cadar arg)))
				 args)
			   (when (and (consp arg) (consp (cdr arg)) (consp (cddr arg)))
			     (push (third arg) args)))))
		(nreverse args))
	    nil)))

(defun canonicalize-and-match-lambda-lists (canonical-order user-specified
					    &optional allow-user-keys)
  (declare (values lambda-list ignores))
  (check-type canonical-order list)
  (check-type user-specified list)
  (let* ((new-lambda-list nil)
	 (ignores nil)
	 (rest-pos (or (position '&rest user-specified) (length user-specified)))
	 (key-pos (or (position '&key user-specified) rest-pos))
	 (rest-and-key (nthcdr (min rest-pos key-pos) user-specified)))
    (when allow-user-keys
      (setq user-specified (subseq user-specified 0 (min rest-pos key-pos))))
    (flet ((user-var-symbol (entry)
	     ;; FOO | (FOO NIL) | ((:FOO BAR) NIL) | (FOO NIL FOO-P) | ((:FOO BAR) FOO-P)
	     ;;--- We don't support the FOO-P syntax yet.
	     (cond ((atom entry)
		    entry)
		   ((atom (setq entry (first entry)))
		    entry)
		   (t (second entry))))
	   (user-var-name (entry)
	     ;; FOO | (FOO NIL) | ((:FOO BAR) NIL) | (FOO NIL FOO-P) | ((:FOO BAR) FOO-P)
	     ;;--- We don't support the FOO-P syntax yet.
	     (cond ((atom entry)
		    entry)
		   ((atom (setq entry (first entry)))
		    entry)
		   (t (first entry)))))
      (declare (dynamic-extent #'user-var-symbol #'user-var-name))
      (dolist (canonical-var canonical-order)
	(let ((user-entry (first (member canonical-var user-specified
					 :test #'string-equal
					 :key #'user-var-name))))
	(cond (user-entry
	       (push (user-var-symbol user-entry) new-lambda-list)
	       (setq user-specified (remove user-entry user-specified)))
	      (t (let ((canonical-gensym (get canonical-var 'canonical-gensym)))
		   (unless canonical-gensym
		     (setq canonical-gensym (make-symbol (symbol-name canonical-var)))
		     (setf (get canonical-var 'canonical-gensym) canonical-gensym))
		   (push canonical-gensym new-lambda-list)
		   (push canonical-gensym ignores))))))
      (when (set-difference user-specified '(&key &allow-other-keys))
	(error "The arguments ~S aren't valid for this lambda list."
	       user-specified))
      (values (if allow-user-keys
		  (append (nreverse new-lambda-list) rest-and-key)
		  (nreverse new-lambda-list))
	      (nreverse ignores)))))


#+Genera
(defmacro defun-property ((symbol indicator) lambda-list &body body)
  `(zl:::scl:defun (:property ,symbol ,indicator) ,lambda-list ,@body))

#-Genera
(defmacro defun-property ((symbol indicator) lambda-list &body body)
  (let ((function-name
	  (make-symbol (lisp:format nil "~A-~A-~A" symbol indicator 'property))))
    `(progn (defun ,function-name ,lambda-list ,@body)
	    (eval-when (load eval) (setf (get ',symbol ',indicator) #',function-name)))))

(defmacro do-delimited-substrings (((string &key (start 0) end)
				    (start-index-var end-index-var &optional char-var))
				   substring-form
				   &body char-clauses)
  (let ((special-characters ())
	(next-var (gensymbol 'next))
	(string-var (if (symbolp string) string (gensymbol 'string)))
	(end-var (gensymbol 'end))
	(char-var (or char-var (gensymbol 'the-char))))
    (dolist (char-clause char-clauses)
      (let ((chars (first char-clause)))
	(if (atom chars)
	    (push chars special-characters)
	    (setf special-characters (nconc special-characters chars)))))
    `(,(if (eq string string-var) 'let 'let*)
	   ((,start-index-var ,start)
	   ,@(unless (eq string string-var)
	       `((,string-var ,string)))
	   (,end-var (or ,end (length ,string-var)))
	   (,end-index-var 0)
	   (,next-var nil)
	   ,@(when (cdr special-characters)
	       `((,char-var ,(car special-characters)))))
       (declare (type fixnum ,start-index-var ,end-var ,end-index-var)
		(type (or fixnum null) ,next-var)
		(character ,char-var))
       (loop
	 (setf ,next-var ,(if (null (cdr special-characters))
			      `(position ,(first special-characters) ,string-var
					 :start ,start-index-var :end ,end-var)
			      `(flet ((char-in-set-p (set char) (find char set)))
				 (declare (dynamic-extent #'char-in-set-p))
				 (position ,(coerce special-characters 'string) ,string-var
					   :start ,start-index-var :end ,end-var
					   :test #'char-in-set-p))))
	 (setf ,end-index-var (or ,next-var ,end-var))
	 ,substring-form
	 (when (null ,next-var) (return (values)))
	 ,@(when (cdr special-characters)
	     `((setf ,char-var (aref ,string-var ,next-var))))	;NOT svref
	 ,@(if (null (cdr char-clauses))
	       (cdr (first char-clauses))
	       `((case ,char-var
		   ,@char-clauses)))
	 (setf ,start-index-var (1+ ,end-index-var))))))


;;; COMPILER-LET replacement from Dave Moon.

;;; Put a compile-time property into the lexical environment
;;; This macro is to be invoked in macro expansions
(defmacro with-compile-time-local-property (&environment env (indicator value) &body body)
  (let ((database (compile-time-local-property-database env)))
    (cond ((eq (getf database indicator body) body)
	   (setf database (list* indicator value database)))
	  (t
	   (setf database (copy-list database))
	   (setf (getf database indicator) value)))
    ;; "Loosemore's Device"
    `(macrolet ((get-compile-time-local-property-1 () ',database))
       ,@body)))

;;; Retrieve a compile time property from the lexical environment
;;; This function is to be called from macro expanders
(defun get-compile-time-local-property (indicator env &optional default)
  (getf (compile-time-local-property-database env) indicator default))

(defun compile-time-local-property-database (env)
  (macroexpand-1 `(get-compile-time-local-property-1) env))

;;; This macro gets shadowed by MACROLET.  Its expansion is the database.
(defmacro get-compile-time-local-property-1 ()
  `nil)

#-(or Genera Cloe-Runtime)
(defvar *compile-time-property-table* (make-hash-table))

;;; Retrieve information from a database that only lasts through COMPILE-FILE
;;; Symbol doesn't have to be a symbol, it can be a class object
(defun compile-time-property (symbol indicator &optional default)
  #+Genera
  (multiple-value-bind (value flag)
      (compiler:file-declaration symbol indicator)
    (if flag value default))
  #+Cloe-Runtime
  (when system::*file-declaration-environment*
    (multiple-value-bind (value flag)
	(clos-internals::file-declaration symbol indicator)
      (if flag value default)))
  #+Minima-Developer
  (multiple-value-bind (value flag)
      (zl:::compiler:file-declaration symbol indicator)
    (if flag value default))
  #-(or Genera Cloe-Runtime Minima-Developer)
  ;; For anything else, do it the dumb way that doesn't reset after compilation
  (let ((table (gethash indicator *compile-time-property-table*)))
    (unless table
      (setf (gethash indicator *compile-time-property-table*)
	    (setq table (make-hash-table))))
    (values (gethash symbol table default))))

(defsetf compile-time-property #+Genera compiler:file-declare
			       #-Genera set-compile-time-property)

#-Genera
(defun set-compile-time-property (symbol indicator value)
  #+Cloe-Runtime
  (when system::*file-declaration-environment*
    (setf (clos-internals::file-declaration symbol indicator) value))
  #+Minima-Developer
  (zl:::compiler:file-declare symbol indicator value)
  #-(or Cloe-Runtime Minima-Developer)
  (let ((table (gethash indicator *compile-time-property-table*)))
    (unless table
      (setf (gethash indicator *compile-time-property-table*)
	    (setq table (make-hash-table))))
    (setf (gethash symbol table) value))
  value)

#+allegro
(defmacro define-dynamic-extent-args (name lambda-list &rest de-args)
  (flet ((lambda-vars (lambda-list)
	   (let ((vars nil))
	     (dolist (var lambda-list)
	       (when (consp var)
		 (setq var (car var)))
	       (when (and (symbolp var)
			  (not (member var lambda-list-keywords
				       :test #'eq)))
		 (push var vars)))
	     vars)))
    (let ((arglist '#:arglist)
	  (vars '#:vars)
	  (vals '#:vals))
      `(define-compiler-macro ,name (&rest ,arglist)
	 (destructuring-bind ,lambda-list ,arglist
	   ,@(lambda-vars lambda-list)
	   (let ((,vars nil)
		 (,vals nil))
	     (mapc #'(lambda (x y)
		       (when y
			 (push x ,vars)
			 (push y ,vals)))
		   ',(mapcar #'copy-symbol de-args)
		   (list ,@de-args))
	     `(let ,(mapcar #'list ,vars ,vals)
		(declare (dynamic-extent ,@,vars))
		(funcall ',',name  ,@(sublis (mapcar #'cons ,vals ,vars)
					     ,arglist)))))))))


#-(or Genera (and ansi-90 (not (and allegro (not (version>= 4 1))))))
(defmacro define-compiler-macro (name lambda-list &body body &environment env)
  env
  #+allegro `(excl::defcmacro ,name ,lambda-list ,@body)
  #-(or Genera allegro) (progn name lambda-list body env nil))	;Suppress compiler warnings.

#+aclpc
(defmacro define-compiler-macro (name lambda-list &body body &environment env)
  env
  (progn name lambda-list body env nil))

#+Genera
;;; Support (proclaim '(function ...)) and (proclaim '(ftype ...)).
;;; This is part of deleting spurious multiple-definition warnings about constructors.
;;; Of course, who knows if this will work in other lisps.
(zl:::scl:defun (:property ftype zl:::scl:proclaim) (decl compile-time)
  (declare (ignore compile-time))		;Do it at load time as well.
  (mapc #'compiler:function-defined (cdr decl)))


#-(or Genera ansi-90)
(defmacro print-unreadable-object ((object stream &key type identity) &body body)
  `(flet ((print-unreadable-object-body () ,@body))
     (declare (dynamic-extent #'print-unreadable-object-body))
     (print-unreadable-object-1 ,object ,stream ,type ,identity
				#'print-unreadable-object-body
				',(not (null body)))))

#-(or Genera ansi-90)
;;; EXTRA-SPACE-REQUIRED is optional because old compiled code didn't always supply it.
(defun print-unreadable-object-1 (object stream type identity continuation
					 &optional (extra-space-required t))
  (write-string "#<" stream)
  ;; wish TYPE-OF worked in PCL
  (when type (lisp:format stream "~S " (class-name (class-of object))))
  (funcall continuation)
  (when identity
    (when extra-space-required (write-char #\space stream))
    (#+PCL pcl::printing-random-thing-internal	;; I assume PCL gets this right. -- rsl
     #-PCL print-unreadable-object-identity
     object stream))
  (write-string ">" stream))

#-(or PCL Genera ansi-90)
(defun print-unreadable-object-identity (object stream)
  #+Genera (format stream "~O" (sys:%pointer object))
  #+allegro (format stream "@~X" (excl::pointer-to-address object))
  ;; Lucid prints its addresses out in Hex.
  #+Lucid (format stream "~X" (sys:%pointer object))
  ;; Probably aren't any #+(and (not Genera) (not allegro) (not PCL) (not ansi-90))
  ;; implementations (actually, this is false: Lispworks).
  #-(or Genera allegro Lucid) (declare (ignore object))
  #-(or Genera allegro Lucid) (format stream "???"))

#-(or Genera ansi-90 Lucid)
(defvar *print-readably* nil)

#-(or Genera Lucid ansi-90)
(deftype real (&optional (min '*) (max '*))
  (labels ((convert (limit floatp)
	     (typecase limit
	       (number (if floatp (float limit 0f0) (rational limit)))
	       (list (map 'list #'convert limit))
	       (otherwise limit))))
    `(or (float ,(convert min t) ,(convert max t))
	 (rational ,(convert min nil) ,(convert max nil)))))

#+Genera-Release-8-1
(defun realp (x)
  (typep x 'real))

(defconstant *end-of-file-marker* :eof)
(deftype end-of-file-marker ()
  '(member :eof))

#+Cloe-Runtime
(defmacro load-time-value (form &optional read-only-p)
  (declare (ignore read-only-p))
  form)


;;; Use a lambda-list to extract arguments from a list and bind variables.
;;; This "should" signal an error if the list doesn't match the lambda-list.
;;; In implementations that have DESTRUCTURING-BIND, the easiest thing is to use it.
;;; We're actually not using the destructuring part, just the ability to match
;;; a lambda-list to a list of arguments without making a closure and using APPLY.
;;; ANSI CL has DESTRUCTURING-BIND.  Cloe and Lucid have it.
;;; Genera has it in both SYMBOLICS-COMMON-LISP and FUTURE-COMMON-LISP,
;;; but the CLIM-LISP package doesn't use either of those.
;;; Last time I checked Franz did not have it
(defmacro bind-to-list (lambda-list list &body body)
  (cond ((not (constantp list))
	 #+Genera `(scl:destructuring-bind ,lambda-list ,list
		     ,(ignore-arglist lambda-list)
		     ,@body)
	 #+Cloe-Runtime `(cloe:destructuring-bind ,lambda-list ,list
			   ,(ignore-arglist lambda-list)
			   ,@body)
	 #+Minima `(destructuring-bind ,lambda-list ,list
		     ,(ignore-arglist lambda-list)
		     ,@body)
	 #+Lucid `(lucid-common-lisp:destructuring-bind ,lambda-list ,list
		    ,(ignore-arglist lambda-list)
		    ,@body)
 	 #+allegro `(destructuring-bind ,lambda-list ,list
 		      ,(ignore-arglist lambda-list)
 		      ,@body)
	 ;; For the other systems, I guess we'll just give up and do it the slow way
	 #-(or Genera Cloe-Runtime Minima Lucid allegro)
	 `(flet ((bind-to-list-body ,lambda-list
		   ,@(when (member '&rest lambda-list)
		       `((declare (dynamic-extent ,(second (member '&rest lambda-list))))))
		   ,(ignore-arglist lambda-list)
		   ,@body))
	    (declare (dynamic-extent #'bind-to-list-body))
	    (apply #'bind-to-list-body ,list)))
	(t
	 ;; This special case supposedly comes up a lot, but I think it never comes up
	 ;; This optimization plays fast and loose with order of evaluation issues
	 ;; for the default value forms in the lambda-list
	 (setq list (eval list))
	 `(symbol-macrolet
	    ,(do ((item)
		  (result nil)
		  (mode nil))
		 ((null lambda-list) (nreverse result))
	       (setq item (pop lambda-list))
	       (cond ((member item '(&optional &rest &key &aux))
		      (setq mode item))
		     ((member item lambda-list-keywords))
		     ((eq mode '&rest)
		      (push `(,item ',list) result))
		     ((eq mode '&key)
		      (multiple-value-bind (variable default supplied-p)
			  (if (atom item) (values item nil nil)
			    (values (if (atom (car item)) (car item) (cadar item))
				    (second item) (third item)))
			(do ((l list (cddr l))
			     (k (parameter-specifier-keyword item)))
			    ((null l)
			     (push `(,variable ,default) result)
			     (when supplied-p
			       (push `(,supplied-p 'nil) result)))
			  (when (eq (first l) k)
			    (push `(,variable ',(second l)) result)
			    (when supplied-p
			      (push `(,supplied-p 't) result))
			    (return)))))
		     (t
		      (multiple-value-bind (variable default supplied-p)
			  (if (atom item) (values item nil nil)
			    (values (first item) (second item) (third item)))
			(cond ((null list)
			       (push `(,variable ,default) result)
			       (when supplied-p
				 (push `(,supplied-p 'nil) result)))
			      (t
			       (push `(,variable ',(pop list)) result)
			       (when supplied-p
				 (push `(,supplied-p 't) result))))))))
	    ,@body))))

;;; Optimization to not bother with destructuring bind if none of the variables
;;; will be used
(defun lambda-list-variables-used-in-body (lambda-list body)
  ;; First collect the variables bound by lambda-list
  (let ((variables nil))
    (do* ((lambda-list lambda-list (cdr lambda-list))
	  (item (first lambda-list) (first lambda-list)))
	 ((null lambda-list))
      (cond ((member item lambda-list-keywords))
	    ((atom item)
	     (push item variables))
	    (t
	     (push (if (atom (car item)) (car item) (cadar item)) variables)
	     (when (cddr item) (push (caddr item) variables)))))
    (when variables
      #+Genera
      (lt:mapforms #'(lambda (subform kind usage state)
		       (declare (ignore usage state))
		       (when (and (member subform variables)
				  (not (member subform lt:*mapforms-bound-variables*))
				  (member kind 'lt:(set symeval)))
			 (return-from lambda-list-variables-used-in-body t)))
		   `(progn ,@body)
		   :bound-variables nil)
      #+Cloe-Runtime
      (labels ((mapper (subform context)
		 ;; It's not worth worrying about being fooled by shadowing bindings
		 ;; with the same name
		 (when (and (member subform variables)
			    (member context '(:access :assign)))
		   (return-from lambda-list-variables-used-in-body t))
		 (clos-internals::map-forms-recurse #'mapper subform context)))
	(clos-internals::map-forms-toplevel #'mapper `(progn ,@body)))
      #+(or Genera Cloe-Runtime) nil
      #-(or Genera Cloe-Runtime)
      ;; We don't know how to do this correctly in other Lisps, since there isn't any
      ;; standardized code-walker, but as a first approximation we can assume that
      ;; if the symbols appear textually they are used as variables, and if they don't,
      ;; they aren't.  Tricky use of macros could defeat this, but it should work well enough.
      ;; Of course this can be fooled by a quoted constant with the same name as a variable
      ;; into producing an unnecessary binding, but that's only an efficiency issue.
      (labels ((analyze (tree)
		 (if (atom tree)
		     (member tree variables)
		     (some #'analyze tree))))
	(analyze body)))))

;;; Get the keyword argument name from an &KEY parameter specifier
(defun parameter-specifier-keyword (spec)
  (cond ((atom spec)
	 (intern (symbol-name spec) *keyword-package*))
	((atom (car spec))
	 (intern (symbol-name (car spec)) *keyword-package*))
	(t (caar spec))))

;;; This is needed because FIND-CLASS in the compile-file environment doesn't look
;;; also in the run-time environment, at least in Symbolics CLOS, which is pretty
;;; embarrassing when we can't find the class T.
;;; In Lucid 4.0 this produces spurious wrong number of arguments warnings for the calls
;;; to FIND-CLASS.  There is no run-time error, it really does accept three arguments.
(defun find-class-that-works (name &optional (errorp t) environment)
  #+Genera (declare (inline compile-file-environment-p))
  #+ccl-2 (when (eq environment 'compile-file)
            (setq environment ccl::*fcomp-compilation-environment*))
  #+allegro (let ((environment (compile-file-environment-p environment)))
	      (if environment
	          (or (find-class name nil environment)
		      (find-class name errorp nil))
	          (find-class name errorp)))
  #+aclpc (find-class name errorp environment)
  #-(or allegro aclpc)
          (if (compile-file-environment-p environment)
	        (or (find-class name nil environment)
		    (find-class name errorp nil))
	        (find-class name errorp environment)))

#+(and allegro never-never-and-never)
(eval-when (compile)
  (warn "~S hacked for lack of environment support in 4.1" 'find-class-that-works))


;;; F-ers

(define-modify-macro minf (&rest other-values) min)
(define-modify-macro maxf (&rest other-values) max)

(defmacro minf-or (place &rest things)
  `(if (null ,place) (setf ,place (min ,@things)) (minf ,place ,@things)))

(defmacro maxf-or (place &rest things)
  `(if (null ,place) (setf ,place (max ,@things)) (maxf ,place ,@things)))

(define-modify-macro roundf (&optional (divisor 1)) round)


;;; Simple vector support

(defun-inline copy-vector-portion (from-vector from-start to-vector to-start length)
  (declare (type fixnum from-start to-start length)
	   (type simple-vector from-vector to-vector))
  (declare (optimize (speed 3) (safety 0)))
  (cond (#+Genera (< length 8) #-Genera t
	 (let (#+(or Genera Minima) (from-vector from-vector)
	       #+(or Genera Minima) (to-vector to-vector))
	   #+(or Genera Minima) (declare (type simple-vector from-vector to-vector))
	   (repeat length
	     (setf (svref to-vector to-start) (svref from-vector from-start))
	     (incf from-start)
	     (incf to-start))))
	#+Genera
	(t
	 (si:copy-array-portion from-vector from-start (+ from-start length)
				to-vector to-start (+ to-start length)))))

;; VECTOR must be a simple vector, FILL-POINTER serves as its fill pointer.
;; The returned values are a (possibly new) vector and the new fill pointer.
;; The idiom for using this is
;; (MULTIPLE-VALUE-SETQ (VECTOR FP) (SIMPLE-VECTOR-PUSH-EXTEND ELEMENT VECTOR FP)).
(defun simple-vector-push-extend (element vector fill-pointer &optional extension)
  (declare (values vector fill-pointer))
  (declare (type fixnum fill-pointer)
	   (type simple-vector vector))
  (let ((length (array-dimension vector 0)))
    (declare (type fixnum length))
    (when (= fill-pointer length)
      ;; Grow the vector
      (let ((new-vector (make-array (+ length (max (ash length -1) (or extension 20)))
				    :element-type (array-element-type vector))))
	(copy-vector-portion vector 0 new-vector 0 length)
	(setq vector new-vector)))
    ;; Insert the new element and return the right values
    (setf (svref vector fill-pointer) element)
    (incf fill-pointer)
    (values vector fill-pointer)))

(defun simple-vector-insert-element (element index vector fill-pointer &optional extension)
  (declare (values vector fill-pointer))
  (declare (type fixnum index fill-pointer)
	   (type simple-vector vector))
  (let ((length (array-dimension vector 0)))
    (declare (type fixnum length))
    (cond ((= fill-pointer length)
	   ;; Grow the vector, leaving a hole for the new element
	   (let ((new-vector (make-array (+ length (max (ash length -1) (or extension 20)))
					 :element-type (array-element-type vector))))
	     (copy-vector-portion vector 0 new-vector 0 index)
	     (copy-vector-portion vector index new-vector (1+ index) (- length index))
	     (setq vector new-vector)))
	  (t
	   ;; Leave a hole for the new element
	   (let (#+(or Genera Minima) (vector vector))
	     #+(or Genera Minima) (declare (type simple-vector vector))
	     (do ((i fill-pointer (1- i)))
		 ((= i index))
	       (declare (type fixnum i)
			(optimize (speed 3) (safety 0)))
	       (setf (svref vector i) (svref vector (1- i)))))))
    ;; Plug in the new element and return the right values
    (setf (svref vector index) element)
    (incf fill-pointer)
    (values vector fill-pointer)))


;;; Debugging support
(defmacro compiler-warn (format-string &rest format-args)
  `(macrolet ((warn-it ()
		(warn ,format-string ,@format-args)))
     (warn-it)))


;;; Condition support
;;; The idea here is to provide a macro that will arrange for the abort choice
;;; to be on the "Abort" debugger choice, if possible.
;;; I would just use WITH-SIMPLE-RESTART but in Genera that doesn't end up
;;; on the <Abort> key.  Note that in Allegro, this choice ends up as an ordinary
;;; proceed option, but in Lucid it ends up on ":A".
(defmacro with-simple-abort-restart ((format-string &rest format-args) &body body)
  #{
    Genera `(scl:catch-error-restart ((sys:abort) ,format-string ,@format-args)
	      ,@body)
    otherwise `(with-simple-restart (abort ,format-string ,@format-args)
		 ,@body)
   }
  )

(defmacro with-simple-abort-restart-if (condition (format-string &rest format-args) &body body)
  (let ((name (gensymbol 'abort-restart-form)))
    `(flet ((,name () ,@body))
       (declare (dynamic-extent #',name))
       (if ,condition
	   (with-simple-abort-restart (,format-string ,@format-args) (,name))
	   ;; Still establish it as a restart, just don't handle ABORT.
	   (with-simple-restart (nil ,format-string ,@format-args)
	     (,name))))))


#||

;;; The DEFINE-CLASS-MIXTURE macro is designed to allow some flexibility
;;; in the way the programmer defines class hierarchies.  It works
;;; pretty much the same as the Flavors :MIXTURE option to DEFFLAVOR,
;;; except without one of the more mystifying features, namely the
;;; ability to condition the keywords' effects on the value of other
;;; keywords.  Here is an example of the kind of this this version
;;; handles:
;;; (define-class-mixture drawing-path
;;;   (:stretch-p stretching-drawing-path-mixin)
;;;   (:orientation
;;;     (nil horizontal-drawing-path-mixin)
;;;     (:diagonal "Diagonal drawing paths not yet implemented")
;;;     (:horizontal horizontal-drawing-path-mixin)
;;;     (:vertical vertical-drawing-path-mixin)))
(defmacro define-class-mixture (name &body specs)
  `(define-group ,name define-class-mixture
     ,@(define-class-mixture-compile-time name specs)))
#+Genera (scl:defprop define-class-mixture "Class Mixture" si:definition-type-name)

;;; Use this to define a class-mixture and a resource at the same time.
;;; This defines a mixture, and a resource of them which has a matcher
;;; and a constructor which will do what you want, namely give you
;;; objects which have the right properties flavors mixed in.
(defmacro define-class-mixture-and-resource (name (&key initializer initial-copies)
					     &body specs)
  (let ((matcher-function-name
	  (make-symbol (lisp:format nil "~A-~A" name 'matcher)))
	(constructor-function-name
	  (make-symbol (lisp:format nil "~A-~A" name 'constructor))))
    `(define-group ,name define-class-mixture-and-resource
       (define-class-mixture ,name ,@specs)
       (defun ,matcher-function-name (object os &rest args)
	 (declare (dynamic-extent args))
	 (declare (ignore os))			;Object-storage object from resource
	 (apply #'mixture-typep object ',name args))
       (defun ,constructor-function-name (rd &rest args)
	 (declare (dynamic-extent args))
	 (declare (ignore rd))			;resource-descriptor for resource
	 (apply #'make-mixture-instance ',name args))
       (defresource ,name (&rest args)
	 :constructor ,constructor-function-name
	 :matcher ,matcher-function-name
	 :initializer ,initializer
	 :initial-copies ,initial-copies))))

;;; Create an instance of a class which has a mixture option.
(defun make-mixture-instance (mixture-name &rest args)
  (declare (dynamic-extent args))
  (apply #'make-instance
	 (apply (get mixture-name 'mixture-class-name) args)
	 args))

(defun mixture-typep (instance mixture-name &rest args)
  (declare (dynamic-extent args))
  (typep instance (apply (get mixture-name 'mixture-class-name) args)))

;;; The guts of DEFINE-CLASS-MIXTURE.  This creates a class-name
;;; function for a given mixture definition, plus all the classes you
;;; need to make it work.  The class names are heuristically chosen to
;;; be the ones the programmer would likely have chosen for the mixed
;;; flavors.  There is no attempt to fix problems this heuristic might
;;; cause, unlike Flavors.

(defun define-class-mixture-compile-time (name specs)
  (let ((keywords nil)
	(forms nil)
	(classes nil)
	(trimmed-names nil))
    (labels ((compile (form) (push form forms))
	     (define-class (class-name component-list)
	       (compile `(defclass ,class-name
				   (,@component-list ,name)
			   ())))
	     (substring (string start &optional end)
	       (setf string (string string))
	       (subseq string start (or end (length string))))
	     (trim-off-name-parts (the-class-name)
	       (let ((pair (assoc the-class-name trimmed-names)))
		 (when pair (return-from trim-off-name-parts (cdr pair))))
	       (let* ((class-name (string the-class-name))
		      (sname (string name))
		      (nl (length sname)))
		 (when (and (> (length class-name) 6)
			    (string-equal class-name "BASIC-" :end1 6))
		   (setf class-name (substring class-name 6)))
		 (when (and (> (length class-name) 6)
			    (string-equal class-name "-MIXIN"
					  :start1 (- (length class-name) 6)))
		   (setf class-name (substring class-name 0 (- (length class-name) 6))))
		 (when (and (> (length class-name) nl)
			    (string-equal class-name sname :start1 (- (length class-name) nl)))
		   (setf class-name (substring class-name 0 (- (length class-name) nl))))
		 (push (cons the-class-name class-name) trimmed-names)
		 class-name))
	     (invent-class-name (subclasses)
	       (when (stringp (first subclasses))
		 (return-from invent-class-name `(error "Can't decode mixture ~S: ~A"
							',name ,(first subclasses))))
	       (intern
		 (with-output-to-string (out)
		   (dolist (class subclasses)
		     (princ (trim-off-name-parts class) out))
		   (princ name out))))
	     (process-specs (specs classes-so-far)
	       (when (null specs)
		 (let ((class-name (invent-class-name classes-so-far)))
		   (when (symbolp class-name)
		     (unless (member class-name classes)
		       (push class-name classes)
		       (define-class class-name classes-so-far))
		     (return-from process-specs `',class-name))
		   (return-from process-specs class-name)))
	       (let* ((first-spec (first specs))
		      (keyword (first first-spec))
		      (variable-name (intern (symbol-name keyword)))
		      (rest (rest first-spec)))
		 (pushnew variable-name keywords)
		 (assert (consp rest) ()
			 "Ill-formatted mixture specification: ~S" specs)
		 (when (and (first rest) (symbolp (first rest)))
		   (assert (null (cdr rest)) ()
			   "A component-class mixture spec must stand alone. ~S is incorrect."
			   first-spec)
		   (return-from process-specs
		     `(if ,variable-name
			  ,(process-specs (cdr specs) (cons (first rest) classes-so-far))
			  ,(process-specs (cdr specs) classes-so-far))))
		 (assert (and rest (listp rest)) ()
			 "A specification keyword must be followed by one or more values.  ~
			  ~S is incorrect."
			 first-spec)
		 `(case ,variable-name
		    ,@(mapcar #'(lambda (subspec)
				  `((,(first subspec))
				    ,(if (second subspec)
					 (process-specs (cdr specs)
							(cons (second subspec) classes-so-far))
					 (process-specs (cdr specs) classes-so-far))))
			      rest)
		    ,@(unless (assoc 'otherwise rest)
			`((otherwise (error "~S is illegal as the ~S option to mixture ~S.~%~
					     The legal values are ~{~S~^, ~}."
					    ,variable-name ,keyword ',name
					    ',(mapcar #'first rest)))))))))
      (let ((result (process-specs specs nil)))
	(compile `(defun-property (,name mixture-class-name)
				  (&key ,@keywords &allow-other-keys)
		    ,result))))
    (nreverse forms)))

||#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Foreign Structure Constructors
;;;

(eval-when (compile)
  (ff:def-foreign-call (_malloc "malloc")
      ((data :int))
    :call-direct t
    :arg-checking nil
    :returning :foreign-address)
  (ff:def-foreign-call (_free "free")
      ((data (* :char) simple-string))
    :call-direct t
    :strings-convert nil		; cac 25feb00
    :arg-checking nil
    :returning :void))

;;; This is here to limit need for FF package to compile time.
(defun system-free (x) (_free x))

;;; ALLOCATE-CSTRUCT was adapted from ff:make-cstruct.
;;; We aren't using ff:make-cstruct because it uses excl:aclmalloc.
#-mswindows
(defun allocate-cstruct (name &key
			      (number 1)
			      (initialize
			       (ff::cstruct-property-initialize
				(ff::cstruct-prop name)))
			      )
  (declare (optimize (speed 3)))
  (let* ((prop (ff::cstruct-prop name))
	 (size (* number (ff::cstruct-property-length prop))))
    (when initialize (setq initialize 0))
    (allocate-memory size initialize)))

(defun allocate-memory (size init)
  ;; Used only by ALLOCATE-CSTRUCT.
  (let ((pointer (_malloc size)))
    (when init
      (do ((i 0 (+ i #-64bit 4 #+64bit 8)))
	  ((>= i size))
	(declare (fixnum i))
	(setf (sys:memref-int pointer i 0 :unsigned-natural) 0)))
    pointer))

;; Adapted from FF:STRING-TO-CHAR*.
;;; We aren't using FF:STRING-TO-CHAR* because it uses excl:aclmalloc.
(defun string-to-foreign (string &optional address)
  "Convert a Lisp string to a C string, by copying."
  (declare (optimize (speed 3))
	   (type string string)
	   (type integer address))
  (unless (stringp string)
    (excl::.type-error string 'string))
  (let* ((length (length string)))
    (declare (optimize (safety 0))
	     (type fixnum length))
    (unless address
      (setq address (_malloc (1+ length))))
    (dotimes (i length)
      (declare (fixnum i))
      (setf (sys:memref-int address 0 i :unsigned-byte)
	(char-code (char string i))))
    (setf (sys:memref-int address 0 length :unsigned-byte) 0)
    address))
