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
;; $fiHeader$

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
	    (,medium (allocate-medium (port ,s) ,s)))
       (unwind-protect
	   (progn ,@body)
	 (deallocate-medium (port ,s) ,medium)))))


(defmacro with-drawing-options ((medium &rest drawing-options) &body
				body)
  (if drawing-options
      `(invoke-with-drawing-options 
	,(if (eq medium t) '*standard-output* medium)
	#'(lambda () ,@body)
	,@drawing-options)
    `(progn ,@body)))


(defmacro with-port-locked ((port) &body body)
  `(mp::with-lock ((port-lock (port ,port)))
		  ,@body))


(defmacro with-graft-locked ((graft) &body body)
  `(mp::with-lock ((graft-lock (graft graft)))
		  ,@body))

