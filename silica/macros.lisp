;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

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
;; $fiHeader: macros.lisp,v 1.3 92/01/31 14:55:43 cer Exp $

(in-package :silica)


(defmacro with-sheet-medium ((medium sheet) &body body)
  `(flet ((with-sheet-medium-body (,medium) ,@body))
     (declare (dynamic-extent #'with-sheet-medium-body))
     (invoke-with-sheet-medium ,sheet #'with-sheet-medium-body)))

(defmacro with-sheet-medium-bound ((sheet medium) &body body)
  `(flet ((with-sheet-medium-bound-body () ,@body))
     (declare (dynamic-extent #'with-sheet-medium-bound-body))
     (invoke-with-sheet-medium-bound ,sheet ,medium 
				     #'with-sheet-medium-bound-body)))

(defmacro with-temporary-medium ((medium sheet) &body body)
  (let ((sheet-var '#:sheet))
    `(let* ((,sheet-var ,sheet)
	    (,medium (allocate-medium (sheet-port ,sheet-var) ,sheet-var)))
       (unwind-protect
	   (progn ,@body)
	 (deallocate-medium (sheet-port ,sheet-var) ,medium)))))


(defmacro with-port-locked ((port) &body body)
  `(with-lock-held  ((port-lock (sheet-port ,port)))
     ,@body))

(defmacro with-graft-locked ((graft) &body body)
  `(with-lock-held ((graft-lock (sheet-graft ,graft)))
     ,@body))


(defmacro with-look-and-feel-realization ((frame-manager frame) &body forms)
  `(macrolet ((realize-pane (&rest args)
		`(realize-pane-1 ,',frame-manager ,',frame ,@args)))
     ,@forms))
