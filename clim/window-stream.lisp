;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: window-stream.lisp,v 1.3 92/01/31 14:59:02 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

(defclass console
	  ()
     ((key-table :accessor console-key-table)
      (pointer-list :initform nil :accessor console-pointer-list)))

#-Silica (progn
;;; Table to associate host windows with their CLIM window "owners".
(defvar *host-window-to-clim-window-mapping* (make-hash-table))

(defun associate-clim-window-with-host-window (host-window clim-window)
  (setf (gethash host-window *host-window-to-clim-window-mapping*)
	clim-window))

(defun clim-window-for-host-window (host-window &key (error-if-no-match t))
  (let ((clim-window (gethash host-window *host-window-to-clim-window-mapping*)))
    (if (null clim-window)
	(if error-if-no-match
	    (error "Could not find CLIM window associated with ~S" host-window)
	    nil)
	clim-window)))
)	;#-Silica

#+Silica
(define-stream-protocol-class window ())

;;; Anything to be gained by the CLX drawable/window distinction?
(defclass window-stream
	  ;; The ordering of these two groups of classes matters if the
	  ;; method combinations are going to come out right.  However
	  ;; the ordering of the classes withing the two levels should not
	  ;; matter as each class defines its own stand-alone protocol.
	  (graphics-output-recording
	   #-Silica window-output-recording
	   #-Silica input-and-window-protocol-intermediary
	   #-Silica output-and-window-protocol-intermediary
	   #-Silica output-and-recording-protocol-intermediary
	   #-Silica window-mixin
	   ;; This part still stands, but we need better layering so that you can
	   ;; have a window stream with no output recording mixed in.
	   output-recording-mixin
	   #-Silica graphics-mixin
	   input-protocol-mixin
	   output-protocol-mixin
	   #+Silica window)
     ())

#-Silica	;no such slots in Silica
(defmethod print-object ((window window-stream) stream)
  (print-unreadable-object (window stream :type t :identity t)
    (let ((left (safe-slot-value window 'left))
	  (top (safe-slot-value window 'top))
	  (right (safe-slot-value window 'right))
	  (bottom (safe-slot-value window 'bottom)))
      (format stream "/x ~D:~D y ~D:~D/" left right top bottom))))

(defmethod window-stream-class-name ((window-stream window-stream))
  (class-name (class-of window-stream)))

#+Silica
(defmethod window-modifier-state ((window window-stream))
  (let ((pointer (stream-primary-pointer window)))
    (pointer-button-state pointer)))


;;; Creation functions

#-Silica
(defun open-root-window (window-type &rest creation-args)
  (declare (dynamic-extent creation-args))
  (assert (member window-type *implementations*) (window-type)
	  "The implementation type supplied, ~S, is not one of~{ ~S~}"
	  window-type *implementations*)
  (apply (window-type-creation-function window-type) creation-args))

#-Silica
(defun open-window-stream (&rest args
			   &key parent left top right bottom width height window-class
			   &allow-other-keys)
  (declare (dynamic-extent args))
  (declare (arglist &key parent left top right bottom width height
		    ;; Initialization arguments to window-stream
		    ;; that are not supplied below
		    borders console default-text-margin default-text-style depth
		    display-device-type draw-p end-of-line-action end-of-page-action
		    initial-cursor-visibility input-buffer label name output-record
		    ;((clim-internals::primary-pointer))	;I think we don't want to mention this
		    record-p save-under scroll-bars window-class
		    stream-background stream-foreground
		    text-cursor text-margin viewport vertical-spacing))
  (assert (not (null parent)) (parent)
	  "You must supply the ~S option to ~S" ':parent 'open-window-stream)
  (assert (typep parent 'window-stream) (parent)
	  "The value of the ~S option to ~S must be a window-stream"
	  ':parent 'open-window-stream)
  (let ((p-left 0) (p-top 0) (p-right nil) (p-bottom nil))
    (multiple-value-setq (p-right p-bottom)
      (window-inside-size parent))
    (macrolet ((rationalize-dimensions (low high delta direction)
		 (flet ((parent-name (name) (fintern "~A-~A" 'p name)))
		   (let ((p-low (parent-name low))
			 (p-high (parent-name high)))
		     `(progn 
			(assert (not (and ,low ,high ,delta
					  (/= ,delta (- ,high ,low))))
				(,low ,high ,delta)
				,(format
				   nil
				   "The ~A dimensions of this window are overconstrained."
				   direction))
			(when ,low (setf ,p-low ,low))
			(when ,high (setf ,p-high ,high))
			(when ,delta (if ,high
					 (setf ,p-low (- ,p-high ,delta))
					 (setf ,p-high (+ ,p-low ,delta))))
			(setf ,low ,p-low ,high ,p-high ,delta (- ,p-high ,p-low)))))))
      (rationalize-dimensions left right width "horizontal")
      (rationalize-dimensions top bottom height "vertical")))
  (with-keywords-removed (window-args args
			  '(:parent :left :top :right :bottom :width :height :window-class))
    (let ((class-name (or window-class (window-stream-class-name parent))))
      (apply #'make-instance class-name
	     :parent parent :left left :top top :right right :bottom bottom
	     :pointers (and parent (stream-pointers parent))
	     window-args))))

;;; These aren't really tested since they aren't the primary way to create sheets.
#+Silica
(defun open-root-window (port-type &rest creation-args)
  (assert (member port-type *port-types* :key #'car) (port-type)
	  "The implementation type supplied, ~S, is not one of~{ ~S~}"
	  port-type *port-types*)
  (let ((port-type (second (assoc port-type *port-types*))))
    (let ((port (apply #'find-port :port-type port-type creation-args)))
      (values (find-graft :port port :origin :nw) port))))

#+Silica
(defun open-window-stream (&rest args &key parent left top right bottom width height
			   &allow-other-keys)
  (declare (dynamic-extent args))
  ;; --- incorporate size-hacking stuff from old definition below
  (assert (not (null parent)) (parent)
	  "You must supply the ~S option to ~S" ':parent 'open-window-stream)
  (when width
    (when right
      (error "Can't supply both :RIGHT and :WIDTH."))
    (setq right (+ left width)))
  (when height
    (when bottom
      (error "Can't supply both :BOTTOM and :HEIGHT."))
    (setq bottom (+ top height)))
  (make-instance 'window-stream :parent parent :min-x left :min-y top :max-x right :max-y bottom))


#-Silica (progn

;;; For hooking up with host window decorations.
(defmethod window-set-viewport-position* :after ((window window-stream) new-x new-y)
  (declare (ignore new-x new-y))
  (redisplay-decorations window))

(defmethod bounding-rectangle-set-edges :after ((window window-stream) left top right bottom)
  (declare (ignore left top right bottom))
  (redisplay-decorations window))

(defmethod bounding-rectangle-set-position* :after ((window window-stream) left top)
  (declare (ignore left top))
  (redisplay-decorations window))

(defmethod bounding-rectangle-set-size :after ((window window-stream) width height)
  (declare (ignore width height))
  (redisplay-decorations window))

(defmethod window-clear :after ((window window-stream))
  (redisplay-decorations window))

(defmethod window-refresh :after ((window window-stream))
  (redisplay-decorations window))

(defmethod redisplay-decorations ((window window-stream))
  )

)	;#-Silica
