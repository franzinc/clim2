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
;; $fiHeader: xt-gadgets.lisp,v 1.16 92/07/20 16:01:57 cer Exp Locker: cer $

(in-package :xm-silica)

(defclass ask-widget-for-size-mixin () ())

(defmethod compose-space ((pane ask-widget-for-size-mixin) &key width height)
  (multiple-value-bind
      (x y width height borderwidth
       care-x care-y care-width care-height care-borderwidth)
      (tk::widget-best-geometry (sheet-direct-mirror pane) :width
				width :height height)
    (declare (ignore x y borderwidth care-x care-y care-width
		     care-height care-borderwidth)) 
    (make-instance 'space-requirement :width width :height height)))


;;; Pane

(defmethod find-widget-class-and-initargs-for-sheet :around
	   ((port xt-port) (parent t)
	    (pane pane))
  ;; Include the name of the pane
  (multiple-value-bind
      (class initargs)
      (call-next-method)
    (let ((name (pane-name pane)))
      (values class
	      (progn
		(when (and name (not (getf initargs :name)))
		  (setf (getf initargs :name) (string name)))
		initargs)))))

;; Background/foreground/text-style mixin

(defmethod find-widget-class-and-initargs-for-sheet :around
    ((port xt-port) (parent t)
     (sheet foreground-background-and-text-style-mixin))
  (multiple-value-bind
      (class initargs)
      (call-next-method)
    (with-slots
	(silica::foreground silica::background text-style)
	sheet
      
      ;; Background can either be a pixel of a bitmap
      ;; Foreground has to be a pixel
      ;; decode-gadget-background
      ;; decode-gadget-foreground
      
      (when silica::background
	(with-sheet-medium (medium sheet)
	  (setf initargs
	    (append (decode-gadget-background medium sheet silica::background) 
		    initargs))))
      
      (when silica::foreground
	(with-sheet-medium (medium sheet)
	  (setf initargs
	    (append (decode-gadget-foreground medium sheet silica::foreground)
		    initargs))))

      (when text-style
	(setf (getf initargs :font-list)
	  (text-style-mapping port text-style)))

      (values class initargs))))

(defmethod decode-gadget-background (medium sheet ink)
  (declare (ignore sheet))
  (let ((gc (decode-ink ink medium)))
    (if (tk::gcontext-tile gc)
	(list :background-pixmap (tk::gcontext-tile gc))
      (list :background (tk::gcontext-foreground gc)))))

(defmethod decode-gadget-foreground (medium sheet ink)
  (declare (ignore sheet))
  (let ((pixel (decode-color medium ink)))
    (list :foreground pixel)))

(defclass xt-pane (pane) ())

(defmethod note-gadget-activated :after ((client t) (gadget xt-pane))
  (let (m)
    (when (setq m (sheet-direct-mirror gadget))
      (xt::set-sensitive m t))))

(defmethod note-gadget-deactivated :after ((client t) (gadget xt-pane))
  (let (m)
    (when (setq m (sheet-direct-mirror gadget))
      (xt::set-sensitive m nil))))

;(defclass xt-composite-pane () ())

(defclass xt-leaf-pane (sheet-permanently-enabled-mixin
			client-overridability-mixin
			mirrored-sheet-mixin 
			ask-widget-for-size-mixin
			xt-pane)
	  ())

;;--- This isn't really right.  Genera and CLX ports have kludges
;;--- for the time being, too.
(defmethod sheet-shell (sheet)
  (do ((w (sheet-mirror sheet) (tk::widget-parent w)))
      ((typep w '(or tk::shell null))
       w)))


(defun compute-new-scroll-bar-values (scroll-bar mmin mmax value slider-size)
  (multiple-value-bind
      (smin smax) (gadget-range* scroll-bar)
    (values 
     (fix-coordinate
      (compute-symmetric-value
       smin smax (* value (- 1 slider-size)) mmin mmax))
     (max 1
	  (fix-coordinate
	   (compute-symmetric-value
	    smin smax slider-size mmin mmax))))))

(defun wait-for-callback-invocation (port predicate &optional (whostate "Waiting for callback"))
  (if (eq mp:*current-process* (port-process port))
      (progn
	(loop 
	  (when (funcall predicate) (return nil))
	  (process-next-event port)))
    (mp:process-wait whostate predicate)))


(defun set-button-accelerator-from-keystroke (button keystroke)
  ;;-- Somehow we have to convert the keystroke into some text and
  ;;-- some character so that pressing it will have an effect
  (when keystroke 
    (tk::set-values button 
		    :accelerator-text (princ-to-string keystroke))))
