;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-USER; Base: 10; Lowercase: Yes -*-
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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $fiHeader: color-ed.lisp,v 1.6 92/07/27 11:03:12 cer Exp $

(in-package :clim-user)


(define-application-frame color-editor ()
    ((red :initform 0)
     (green :initform 0)
     (blue :initform 0)
     (ihs :initform :rgb))
  (:command-table (color-editor :inherit-from (accept-values-pane)))
  (:pane
    (scrolling ()
      (make-pane 'application-pane
		 :width :compute :height :compute
		 :display-function 'color-editor-display-function))))

(defun color-editor-display-function (frame stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (accept-values-pane-displayer
    frame stream
    :resynchronize-every-pass t
    :displayer #'color-editor-display-function-1))

(defun color-editor-display-function-1 (frame stream)
  (with-slots (red green blue ihs) frame
    (with-room-for-graphics (stream)
      (draw-rectangle* stream 0 0 100 100
		       :ink (funcall
			      (if (eq ihs :ihs) #'make-ihs-color #'make-rgb-color)
			      (float (/ red 100))
			      (float (/ green 100))
			      (float (/ blue 100)))))
    (terpri stream)
    (setq ihs (accept '(member :ihs :rgb)
		      :stream stream
		      :default ihs
		      :prompt "Ihs/rgb"))
    (terpri stream)
    (if (eq ihs :ihs)
	(progn
	  (setq red (accept 'integer
			    :stream stream
			    :default red
			    :prompt "Intensity"))
	  (terpri stream)
	  (setq green (accept 'integer
			      :stream stream
			      :default green
			      :prompt "Hue"))
	  (terpri stream)
	  (setq blue (accept 'integer
			     :stream stream
			     :default blue
			     :prompt "Saturation"))
	  (terpri stream))
	(progn
	  (setq red (accept 'integer
			    :stream stream
			    :default red
			    :prompt "Red"))
	  (terpri stream)
	  (setq green (accept 'integer
			      :stream stream
			      :default green
			      :prompt "Green"))
	  (terpri stream)
	  (setq blue (accept 'integer
			     :stream stream
			     :default blue
			     :prompt "Blue"))
	  (terpri stream)))))

(define-color-editor-command (com-do-nothing :name t :menu t) ()
  ())

(defun do-color-editor ()
  (run-frame-top-level (make-application-frame 'color-editor)))
