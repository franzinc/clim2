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
;; $fiHeader: xt-frames.lisp,v 1.2 92/01/31 14:56:37 cer Exp Locker: cer $


(in-package :xm-silica)

;; Basic intrinsics frame-manager

(defclass xt-frame-manager (frame-manager) 
	  ())

(defmethod note-frame-enabled :after ((framem xt-frame-manager) frame)
  ;; Perhaps we want to resize the top leve sheet if there is one
  (when (frame-top-level-sheet frame)
    (setf (sheet-enabled-p (frame-top-level-sheet frame)) t)))

(defmethod note-frame-disabled :after ((framem xt-frame-manager) frame)
  (setf (sheet-enabled-p (frame-top-level-sheet frame)) nil))

(defmethod adopt-frame ((framem xt-frame-manager) (frame standard-application-frame))
  (call-next-method)
  (when (frame-panes frame)
    (let* ((top-pane (frame-panes frame))
	   (sheet (with-look-and-feel-realization (framem frame)
		    (realize-pane 'silica::top-level-sheet 
				  :region (make-bounding-rectangle
					   0 0 (sheet-width top-pane) 
					   (sheet-height top-pane))
				  :parent (find-graft)))))
      (setf (frame-top-level-sheet frame) sheet
	    (frame-shell frame) (sheet-shell sheet))
      (sheet-adopt-child sheet (frame-panes frame)))))


(defmethod clim-internals::frame-wrapper ((frame t) (framem xt-frame-manager) pane)
  (declare (ignore pane))
  (let ((mb (slot-value frame 'clim-internals::menu-bar)))
    (if mb
	(with-look-and-feel-realization (framem frame)
	  (vertically ()
		      (realize-pane 'menu-bar
				    :command-table (if (eq mb t)
						       (frame-command-table frame)
						     (find-command-table mb)))
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
   (sheet-port frame)
   (make-instance 'presentation-event
		  :sheet (frame-top-level-sheet frame)
		  :value (second item))))
