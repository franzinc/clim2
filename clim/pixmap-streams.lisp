;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

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
;; $fiHeader: pixmap-streams.lisp,v 1.6 92/05/22 19:28:18 cer Exp $

(in-package :clim-internals)

(defclass pixmap-stream (input-protocol-mixin
			 output-protocol-mixin
			 mirrored-sheet-mixin
			 sheet-permanently-enabled-mixin
			 permanent-medium-sheet-output-mixin
			 sheet-transformation-mixin
			 sheet)
	  ())

(defmethod realize-mirror ((port basic-port) (stream pixmap-stream))
  nil)

(defmethod update-mirror-transformation ((port basic-port) (sheet pixmap-stream))
  nil)

(defmethod update-mirror-region ((port basic-port) (sheet pixmap-stream))
  nil)

(defmethod initialize-instance :after ((stream pixmap-stream) 
				       &key port sheet pixmap
					    width height)
  (setf (sheet-direct-mirror stream) pixmap
	(port stream) port
	(sheet-transformation stream) +identity-transformation+
	(sheet-region stream) (make-bounding-rectangle 0 0 width height)
	;;--- What about text style?
	(medium-foreground stream) (or (medium-foreground sheet) +black+)
	(medium-background stream) (or (medium-background sheet) +white+)))

;; Interface to this stuff

(defmacro with-output-to-pixmap ((stream sheet &rest options) &body body)
  (default-output-stream stream with-output-to-pixmap)
  `(flet ((with-output-to-pixmap-body (,stream) ,@body))
     (declare (dynamic-extent #'with-output-to-pixmap-body))
     (invoke-with-output-to-pixmap 
       ,sheet #'with-output-to-pixmap-body ,@options)))

(defmethod invoke-with-output-to-pixmap (sheet continuation &key width height)
  (let* ((pixmap (allocate-pixmap sheet width height))
	 (stream (allocate-pixmap-stream sheet pixmap width height)))
    (funcall continuation stream)
    pixmap))


(defmethod allocate-pixmap (sheet width height)
  (port-allocate-pixmap (port sheet) sheet width height))

(defmethod allocate-pixmap-stream (sheet pixmap width height)
  (make-instance 'pixmap-stream 
    :default-text-margin width
    :pixmap pixmap 
    :width width
    :height height
    :port (port sheet)
    :sheet sheet))

(defun copy-from-pixmap (pixmap pixmap-x pixmap-y width height
			 medium medium-x medium-y)
  (medium-copy-from-pixmap pixmap pixmap-x pixmap-y width height
			   medium medium-x medium-y))

(defun copy-to-pixmap (medium medium-x medium-y width height
		       &optional pixmap (pixmap-x 0) (pixmap-y 0))
  (unless pixmap
    (setf pixmap (allocate-pixmap medium width height)))
  (medium-copy-to-pixmap medium medium-x medium-y width height
			 pixmap pixmap-x pixmap-y)
  pixmap)
