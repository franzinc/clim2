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
;; $fiHeader: xt-gadgets.lisp,v 1.25 92/11/13 14:47:19 cer Exp $

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
    (make-space-requirement :width width :height height)))


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
  (let ((pixel (decode-color ink medium)))
    (list :foreground pixel)))

(defclass xt-pane (basic-pane) 
	  ;;--- Is this useful a hack enabling things to be passed through
	  ;;--- to the mirror
	  ((silica::mirror-initargs  :initarg :mirror-initargs))
  (:default-initargs :mirror-initargs nil))

(defmethod find-widget-class-and-initargs-for-sheet :around ((port xt-port)
							     (parent t)
							     (sheet xt-pane))
  (multiple-value-bind
      (class initargs)
      (call-next-method)
    (values class (append (slot-value sheet 'silica::mirror-initargs) initargs))))
	    
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

(defclass xt-top-level-sheet (top-level-sheet) 
	  ((accelerator-gestures :initform nil :reader top-level-sheet-accelerator-gestures)))

(defmethod top-level-sheet-accelerator-gestures ((sheet top-level-sheet)) nil)

(defmethod sheet-disown-child :after ((sheet xt-top-level-sheet) (child basic-sheet))
  (setf (slot-value sheet 'accelerator-gestures) nil))

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

(defun set-button-mnemonic (menubar button mnem)
  (when mnem 
    (record-accelerator menubar (list mnem :meta))
    (tk::set-values button :mnemonic mnem)))


(defun record-accelerator (menubar gesture)
  (let ((sheet (frame-top-level-sheet (pane-frame menubar))))
    (push gesture (slot-value sheet 'accelerator-gestures))))

(defmethod distribute-event ((port xt-port) (event keyboard-event))
  (unless (discard-accelerator-event-p port event)
    (call-next-method)))

(defmethod discard-accelerator-event-p ((port xt-port) event)
  (let ((frame (pane-frame (event-sheet event))))
    (and frame
	 (some #'(lambda (gesture)
		   (clim-internals::keyboard-event-matches-gesture-name-p event gesture port))
	       (top-level-sheet-accelerator-gestures (frame-top-level-sheet frame))))))



;; Xt-orriented-gadget

(defclass xt-oriented-gadget () ())

(defmethod find-widget-class-and-initargs-for-sheet :around ((port xt-port)
                                                             (parent t)
                                                             (sheet xt-oriented-gadget))
  (multiple-value-bind
      (class initargs)
      (call-next-method)
    (with-accessors ((orientation gadget-orientation)) sheet
      (unless (getf initargs :orientation)
        (setf (getf initargs :orientation) orientation)))
    (values class initargs)))

(defmethod (setf gadget-orientation) :after (nv (gadget xt-oriented-gadget))
  (when (sheet-direct-mirror gadget)
    (set-widget-orientation gadget nv)))

(defmethod set-widget-orientation ((gadget xt-oriented-gadget) nv)
  (tk::set-values (sheet-direct-mirror gadget) :orientation nv))
