;; -*- mode: common-lisp; package: silica -*-
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
;; $fiHeader: macros.cl,v 1.2 92/01/02 15:32:54 cer Exp $

(in-package :silica)


(defmacro with-sheet-medium ((medium sheet) &body body)
  `(with-sheet-medium-1 
    ,sheet 
    #'(lambda (,medium) ,@body)))

(defmacro with-sheet-medium-bound ((sheet medium) &body body)
  `(with-sheet-medium-bound-1
    ,sheet
    ,medium
    #'(lambda () ,@body)))

(defmacro with-temporary-medium ((medium sheet) &body body)
  (let ((s (gensym)))
    `(let* ((,s ,sheet)
	    (,medium (allocate-medium (sheet-port ,s) ,s)))
       (unwind-protect
	   (progn ,@body)
	 (deallocate-medium (sheet-port ,s) ,medium)))))


(defmacro with-port-locked ((port) &body body)
  `(with-lock-held  ((port-lock (sheet-port ,port)))
     ,@body))

(defmacro with-graft-locked ((graft) &body body)
  `(with-lock-held ((graft-lock (sheet-graft graft)))
     ,@body))


(defmacro with-look-and-feel-realization ((realizer frame) &rest forms)
  `(macrolet ((realize-pane (&rest foo)
		`(realize-pane-1 ,',realizer ,',frame ,@foo)))
     ,@forms))
