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
;; $fiHeader: std-sheet.lisp,v 1.3 92/01/31 14:55:49 cer Exp $

(in-package :silica)


(defclass standard-sheet 
	  (sheet
	   permanent-medium-sheet-output-mixin
	   mirrored-sheet-mixin
	   sheet-multiple-child-mixin
	   sheet-transformation-mixin
	   standard-repainting-medium
	   standard-sheet-input-mixin
	   mute-repainting-mixin)
    ())


(defclass simple-sheet
	  (sheet 
	   sheet-multiple-child-mixin 
	   sheet-transformation-mixin
	   standard-repainting-medium
	   standard-sheet-input-mixin
	   temporary-medium-sheet-output-mixin
	   mute-repainting-mixin)
  ())


(defmethod handle-event (sheet event)
  (declare (ignore sheet event))
  #+++ignore (warn "ignoring event ~S,~S" sheet event)
  nil)
