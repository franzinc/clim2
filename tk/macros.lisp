;; -*- mode: common-lisp; package: tk -*-
;;
;;				-[]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, CA  All rights reserved.
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
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: macros.cl,v 1.2 92/01/02 15:08:51 cer Exp Locker: cer $

(in-package :tk)

(defvar *temp-with-ref-par* nil)

(defmacro with-ref-par (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
    (destructuring-bind
	((var value) &rest more-bindings) bindings
      `(let ((,var (or (pop *temp-with-ref-par*)
		       (make-array 1 :element-type '(unsigned-byte
						     32)))))
	 (declare (type (simple-array (unsigned-byte 32) (1)) ,var))
	 (setf (aref ,var 0) ,value)
	 (multiple-value-prog1
	     (with-ref-par ,more-bindings ,@body)
	   (push ,var *temp-with-ref-par*))))))

