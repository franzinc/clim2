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
;; $fiHeader: xt-frames.lisp,v 1.4 92/02/24 13:06:24 cer Exp $


(in-package :xm-silica)

;; Basic intrinsics frame-manager

(defclass xt-frame-manager (standard-frame-manager) 
    ())

(defmethod frame-wrapper ((framem xt-frame-manager) 
			  (frame standard-application-frame) pane)
  (declare (ignore pane))
  (let ((menu-bar (slot-value frame 'menu-bar)))
    (if menu-bar
	(with-look-and-feel-realization (framem frame)
	  (vertically ()
	    (realize-pane 'menu-bar
			  :command-table (if (eq menu-bar t)
					     (frame-command-table frame)
					     (find-command-table menu-bar)))
	    (call-next-method)))
	(call-next-method))))

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

