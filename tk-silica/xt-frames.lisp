;; -*- mode: common-lisp; package: xm-silica -*-
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
;; $fiHeader: xt-frames.lisp,v 1.7 92/04/10 14:27:56 cer Exp Locker: cer $


(in-package :xm-silica)

;; Basic intrinsics frame-manager

(defclass xt-frame-manager (standard-frame-manager) 
    ())

(defmethod frame-wrapper ((framem xt-frame-manager) 
			  (frame standard-application-frame) pane)
  (declare (ignore pane))
  (with-look-and-feel-realization (framem frame)
    (let* ((menu-bar (slot-value frame 'menu-bar))
	   (menu-bar-pane
	     (and menu-bar
		  (make-pane 'menu-bar
			     :command-table (if (eq menu-bar t)
						(frame-command-table frame)
						(find-command-table menu-bar)))))
	   (pointer-doc-pane
	     ;;--- Don't like these forward references
	     (and (clim-internals::frame-pointer-documentation-p frame)
		  (setf (slot-value frame 'clim-internals::pointer-documentation-pane)
			(make-pane
			  'clim-internals::pointer-documentation-pane
			  :max-width +fill+
			  ;;--- This should be one line height in some text style
			  :height 15))))
	   (application-panes (call-next-method)))
      (cond ((and menu-bar-pane pointer-doc-pane)
	     (vertically () menu-bar-pane application-panes pointer-doc-pane))
	    (menu-bar-pane
	     (vertically () menu-bar-pane application-panes))
	    (pointer-doc-pane
	     (vertically () application-panes pointer-doc-pane))
	    (t
	     application-panes)))))


;;;

(defclass presentation-event (event)
    ((value :initarg :value :reader presentation-event-value)
     (sheet :initarg :sheet :reader event-sheet)))

(defmethod handle-event (sheet (event presentation-event))
  (throw-highlighted-presentation
    (make-instance 'standard-presentation
		   :object (presentation-event-value event)
		   :type 'command)
    *input-context*
    (make-instance 'pointer-button-press-event
		   :sheet sheet
		   :x 0
		   :y 0
		   :modifiers 0
		   :button 256)))

(defun command-button-callback (button dunno frame item)
  (distribute-event
    (port frame)
    (make-instance 'presentation-event
		   :sheet (frame-top-level-sheet frame)
		   :value (second item))))

(defmethod clim-internals::port-notify-user ((port xt-port) 
					     frame format-string &rest format-arguments)
  (format excl::*initial-terminal-io* 
	  "Notify ~a~%" 
	  (list frame format-string format-arguments)))
