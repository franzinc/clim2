;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: window-protocol.lisp,v 1.6 92/05/07 13:13:10 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 International Lisp Associates."

;;; So we don't process operations twice, once when *we* process it,
;;; again when the implementation layer says "Hey, guess what?"
(defvar *synchronous-window-operation-being-processed* nil)

(define-stream-protocol-class window ())

(defclass window-mixin
	  (standard-bounding-rectangle window)
     ((parent :accessor window-parent :initarg :parent)
      (children :accessor window-children :initform nil)
      ;; --- better term than console?
      (console :accessor window-console :initarg :console)
      (name :accessor window-name :initarg :name)
      (label :accessor window-label :initarg :label)
      (depth :accessor window-depth :initarg :depth)	;is this bits-per-pixel?
      (viewport :accessor window-viewport :initarg :viewport)
      (update-region :accessor window-update-region :initform nil)
      (visibility :accessor window-visibility :initform nil)
      #+Ignore
      (input-process :accessor window-input-process)
      (inside-width-cache :initform nil))
  (:default-initargs :parent nil :console nil :name nil :label nil))

;; Prevent using the instance-ref instructions on these, since they
;; usually have :BEFORE/:AFTER qualifiers on the SETF methods that will
;; cause the instance-ref instructions to take a slow trap.
#+(or Genera Minima)
(progn
  (declaim (notinline window-label      (setf window-label)))
  (declaim (notinline window-visibility (setf window-visibility))))

;;; Runs :AROUND so that edges will be set up by primary method and :AFTER method
;;; for the specific implementation.
(defmethod initialize-instance :around ((window window-mixin)
					&key parent scroll-bars borders)
  ;; All ports accept these standard window decoration options
  ;; Accept them here for the benefit of warn-if-pane-descriptions-invalid
  scroll-bars borders  ;ignored, declare ignore doesn't work here
  (call-next-method)
  (when parent
    (push window (window-children parent)))
  (with-slots (viewport left top right bottom) window
    ;; Make initial viewport the same size as the window rooted at the origin.
    ;; (window edges are in parent's coordinate system, while viewport edges are in window's)

    ;;--- Is this right?  At least it works...
    (multiple-value-bind (width height) (window-inside-size window)
      (setf viewport (make-bounding-rectangle 0 0 width height))
      ;;--- Cross protocol violation
      (when (extended-output-stream-p window)
	(setf (stream-default-text-margin window)
	      width)))))

;;; WINDOW-ERASE-VIEWPORT req'd
;;; WINDOW-STACK-ON-TOP req'd
;;; WINDOW-STACK-ON-BOTTOM req'd
;;; COPY-AREA req'd

(defmethod window-refresh :around ((stream window-mixin))
  (when (window-drawing-possible stream)
    (call-next-method)))

(defmethod window-stream-stack-on-top :around ((window window-mixin))
  (let ((*synchronous-window-operation-being-processed* t))
    (call-next-method)))

(defmethod bounding-rectangle-set-edges :around ((window window-mixin) left top right bottom)
  (declare (ignore left top right bottom))
  (let ((*synchronous-window-operation-being-processed* T))
    (call-next-method)))

(defmethod bounding-rectangle-set-position :around ((window window-mixin) nx ny)
  (declare (ignore nx ny))
  (let ((*synchronous-window-operation-being-processed* t))
    (call-next-method)))

(defmethod bounding-rectangle-set-size :around ((window window-mixin) width height)
  (declare (ignore width height))
  (let ((*synchronous-window-operation-being-processed* t))
    (call-next-method)))

;;; These methods are needed for hooks on other mixins to work.
(defmethod window-viewport-position ((window window-mixin))
  (with-slots (viewport) window
    (bounding-rectangle-position viewport)))

(defmethod window-set-viewport-position ((window window-mixin) x y)
  (with-slots (viewport) window
    (bounding-rectangle-set-position viewport x y)))

#+CLIM-1-compatibility
(define-compatibility-function (window-viewport-position*
				window-viewport-position)
			       (window)
  (window-viewport-position window))

#+CLIM-1-compatibility
(define-compatibility-function (window-set-viewport-position*
				window-set-viewport-position)
			       (window x y)
  (window-set-viewport-position window x y))

(defmethod window-with-zero-viewport-position
	   ((window window-mixin) continuation)
  ;;--- should be on window-and-graphics-intermediary??
  ;; Margin components should not respect the stream transform
  (letf-globally (((medium-transformation window) +identity-transformation+))
    (let ((viewport (window-viewport window)))
      (multiple-value-bind (x y) (bounding-rectangle-position viewport)
	(unwind-protect
	    (progn (bounding-rectangle-set-position viewport 0 0)
		   (funcall continuation))
	  (bounding-rectangle-set-position viewport x y))))))

(defmethod window-clear ((window window-mixin))
  (window-erase-viewport window)
  (when (extended-output-stream-p window)	;can we assume this?
    (stream-set-cursor-position window 0 0)
    (setf (stream-baseline window) 0
	  (stream-current-line-height window) 0))
  (bounding-rectangle-set-position (window-viewport window) 0 0))

;;; Basically a hook for other mixins.
(defmethod window-refresh ((window window-mixin))
  (window-erase-viewport window))

(defmethod window-expose ((window window-mixin))
  (setf (window-visibility window) t)
  (window-stack-on-top window))


;;; --- This needs to be on window-output...
;(defmethod window-fresh-page ((stream window-mixin))
;  (multiple-value-call #'window-set-viewport-position
;    window (window-stream-cursor-position stream)))
;

(defun window-size-viewport-to-fit (window)
  (multiple-value-bind (w h) (window-inside-size window)
    (bounding-rectangle-set-size (window-viewport window) w h)))


;;; Coordinate translations

;;; The true interfaces: 
;;; WINDOW-TO-SCREEN-COORDINATES Window-X Window-Y -> Screen-X Screen-Y
(defmethod window-to-screen-coordinates ((window window-mixin) x y)
  (declare (type coordinate x y))
  (multiple-value-bind (dx dy) (window-to-screen-coordinate-translations window)
    (declare (type coordinate dx dy))
    (values (the coordinate (+ x dx)) (the coordinate (+ y dy)))))

(defmethod screen-to-window-coordinates ((window window-mixin) x y)
  (declare (type coordinate x y))
  (multiple-value-bind (dx dy) (window-to-screen-coordinate-translations window)
    (declare (type coordinate dx dy))
    (values (the coordinate (- x dx)) (the coordinate (- y dy)))))
	   
;;; An internal routine
(defmethod window-to-screen-coordinate-translations ((window window-mixin))
  (with-slots (left top viewport parent) window
    (declare (type coordinate left top))
    (multiple-value-bind (left-translation top-translation) (window-margins window)
      (declare (type coordinate left-translation top-translation))
      (setq left-translation (the coordinate (+ left-translation left)))
      (setq top-translation (the coordinate (+ top-translation top)))
      (when parent
	(multiple-value-bind (parent-left parent-top)
	    (window-to-screen-coordinate-translations parent)
	  (declare (type coordinate parent-left parent-top))
	  (setq left-translation (the coordinate (+ left-translation parent-left)))
	  (setq top-translation (the coordinate (+ top-translation parent-top)))))
      (when viewport
	(multiple-value-bind (viewport-left viewport-top)
	    (bounding-rectangle-position viewport)
	  (declare (type coordinate viewport-left viewport-top))
	  (setq left-translation (the coordinate (- left-translation viewport-left)))
	  (setq top-translation (the coordinate (- top-translation viewport-top)))))
      (values left-translation top-translation))))

(defmethod drawing-surface-to-viewport-coordinates ((window window-mixin) dsx dsy)
  (declare (type coordinate dsx dsy))
  (multiple-value-bind (vx vy) (window-viewport-position window)
    (declare (type coordinate vx vy))
    (multiple-value-bind (ml mt) (window-margins window)
      (declare (type coordinate ml mt))
      (values (the coordinate (+ (the coordinate (- dsx vx)) ml))
	      (the coordinate (+ (the coordinate (- dsy vy)) mt))))))

(defmethod viewport-to-drawing-surface-coordinates ((window window-mixin) x y)
  (declare (type coordinate x y))
  (multiple-value-bind (vx vy) (window-viewport-position window)
    (declare (type coordinate vx vy))
    (multiple-value-bind (ml mt) (window-margins window)
      (declare (type coordinate ml mt))
      (values (the coordinate (- (the coordinate (+ x vx)) ml))
	      (the coordinate (- (the coordinate (+ y vy)) mt))))))

;;; Hmm.
(defmethod bounding-rectangle-set-edges :after ((window window-mixin) left top right bottom)
  (declare (ignore left top right bottom))
  (window-size-viewport-to-fit window)
  (setf (slot-value window 'inside-width-cache) nil))

(defmethod bounding-rectangle-set-size :after ((window window-mixin) width height)
  (declare (ignore width height))
  (window-size-viewport-to-fit window)
  (setf (slot-value window 'inside-width-cache) nil))

(defmethod window-inside-size ((window window-mixin))
  (multiple-value-bind (left top right bottom) (window-inside-edges window)
    (declare (type coordinate left top right bottom))
    (values (the coordinate (- right left)) (the coordinate (- bottom top)))))

(defmethod window-inside-edges ((stream window-mixin))
  (multiple-value-bind (lom tom rom bom) (host-window-margins stream)
    (declare (type coordinate lom tom rom bom))
    (multiple-value-bind (lim tim rim bim) (window-margins stream)
      (declare (type coordinate lim tim rim bim))
      (with-bounding-rectangle* (left top right bottom) stream
	(values (the coordinate (+ left lom lim))  (the coordinate (+ top tom tim))
		(the coordinate (- right rom rim)) (the coordinate (- bottom bom bim)))))))

(defmethod window-set-inside-edges ((stream window-mixin) 
				    new-left new-top new-right new-bottom)
  (multiple-value-bind (lom tom rom bom) (host-window-margins stream)
    (declare (type coordinate lom tom rom bom))
    (multiple-value-bind (lim tim rim bim) (window-margins stream)
      (declare (type coordinate lim tim rim bim))
      (bounding-rectangle-set-edges
       stream
       (the coordinate (- (coordinate new-left)   lom lim))
       (the coordinate (- (coordinate new-top)    tom tim))
       (the coordinate (+ (coordinate new-right)  rom rim))
       (the coordinate (+ (coordinate new-bottom) bom bim))))))

;; the port needs to define host-window-margins

(defmethod window-set-inside-size ((window window-mixin) new-width new-height)
  (multiple-value-bind (left top) (window-inside-edges window)
    (window-set-inside-edges window left top (+ left new-width) (+ top new-height))))

(defmacro define-window-inside-methods (useless &aux code)
  (labels ((emit-methods (vars primitive)
	     (do* ((i 0 (1+ i))
		   (names vars (cdr names)))
		  ((null names))
	       (emit-method (method-name (car names) 'window-inside) i vars primitive)))
	   (method-name (name prefix) (fintern "~A-~A" prefix name))
	   (emit-method (name nth-value vars called-method)
	     (push `(defmethod ,name ((window window-mixin))
		      ,(if (= nth-value 0) `(values (,called-method window))
			   `(multiple-value-bind ,(subseq vars 0 (1+ nth-value))
				(,called-method window)
			      (declare (ignore ,@(subseq vars 0 nth-value)))
			      ,(nth nth-value vars))))
		   code)))
    (emit-methods '(left top right bottom) 'window-inside-edges)
    (emit-methods '(width height) 'window-inside-size))
  `(define-group ,useless define-window-inside-methods ,@(nreverse code)))

(define-window-inside-methods methods)

(defmethod window-inside-width :around ((window window-mixin))
   (with-slots (inside-width-cache) window
     (or inside-width-cache
	 (setf inside-width-cache (call-next-method)))))

#+Genera
(defgeneric stream-compatible-inside-size (window)
  (:selector :inside-size))

#+Genera
(defmethod stream-compatible-inside-size ((window window-mixin))
  (bounding-rectangle-size (window-viewport window)))

#+Genera
(defgeneric stream-compatible-visible-cursorpos-limits (window &optional unit)
  (:selector :visible-cursorpos-limits))

#+Genera
(defmethod stream-compatible-visible-cursorpos-limits 
	   ((window window-mixin) &optional (unit ':pixel))
  (with-bounding-rectangle* (left top right bottom) (window-viewport window)
    (ecase unit
      (:pixel (values left top right bottom))
      (:character (let ((char-width (stream-character-width window #\M))
			(line-height (stream-line-height window)))
		    (values (floor left char-width) (floor top line-height)
			    (floor right char-width) (floor bottom line-height)))))))

#+Genera
(defgeneric stream-compatible-size-in-characters (window)
  (:selector :size-in-characters))

#+Genera
(defmethod stream-compatible-size-in-characters ((window window-mixin))
  (with-bounding-rectangle* (left top right bottom) (window-viewport window)
    (let ((char-width (stream-character-width window #\M))
	  (line-height (stream-line-height window)))
      (values (floor (- right left) char-width)
	      (floor (- bottom top) line-height)))))

(defmethod window-label-size ((window window-mixin) &optional (label (window-label window)))
  (declare (ignore label))
  ;; default method returns nothing.
  (values (coordinate 0) (coordinate 0)))

(defmethod window-note-size-or-position-change ((window window-mixin)
						new-left new-top new-right new-bottom)
  ;;
  (with-slots (left top right bottom) window
    (setf left new-left)
    (setf top new-top)
    (setf right new-right)
    (setf bottom new-bottom))
  ;; for the time being, make the viewport fill the window
  (window-size-viewport-to-fit window))

;;; Offset from root
(defun window-offset (stream)
  (let ((x-offset (coordinate 0))
	(y-offset (coordinate 0)))
    (declare (type coordinate x-offset y-offset))
    (do ((s stream (window-parent s)))
	((null s) nil)
      (multiple-value-bind (x y) (bounding-rectangle-position s)
	(declare (type coordinate x y))
	(multiple-value-bind (ml mt) (host-window-margins s)
	  (declare (type coordinate ml mt))
	  (setq x-offset (the coordinate (+ x-offset x ml)))
	  (setq y-offset (the coordinate (+ y-offset y mt))))))
    (values x-offset y-offset)))

;; This is called by OUTPUT-RECORDING-MIXIN's whopper on SET-VIEWPORT-POSITION.
;; It shifts a region of the "host screen" that's visible to some other visible
;; location.  It does NOT do any cleaning up after itself.  It does not side-effect
;; the output history of the window.  It calls COPY-AREA whose contract is to 
;; do the above, the whole above, and nothing but the above.
(defmethod window-shift-visible-region ((window window-mixin)
					old-left old-top old-right old-bottom
					new-left new-top new-right new-bottom)
  (declare (type coordinate new-left new-top new-right new-bottom))
  (declare (ignore old-right old-bottom new-right new-bottom))
  (let ((delta-x (- old-left new-left))
	(delta-y (- old-top new-top)))
    (multiple-value-bind (stream-width stream-height)
	(window-inside-size window)
      (declare (type coordinate stream-width stream-height))
      (let (from-x from-y)
	(cond ((and (>= delta-x 0)
		    (>= delta-y 0))
	       ;; shifting down and to the right
	       (setq from-x 0
		     from-y 0))
	      ((and (>= delta-x 0)
		    (<= delta-y 0))
	       ;; shifting up and to the right
	       (setq from-x 0
		     from-y (abs delta-y)))
	      ((>= delta-y 0)
	       ;; shifting down and to the left
	       (setq from-x (abs delta-x)
		     from-y 0))
	      (t
	       ;; shifting up and to the left
	       (setq from-x (abs delta-x)
		     from-y (abs delta-y))))
	(let ((width (- stream-width (abs delta-x)))
	      (height (- stream-height (abs delta-y))))
	  (multiple-value-bind (ml mt) 
	      (window-margins window)
	    (declare (type coordinate ml mt))
	    (translate-coordinates ml mt from-x from-y)
	    (copy-area window
		       from-x from-y
		       (+ from-x width) (+ from-y height)
		       (+ from-x delta-x) (+ from-y delta-y))))))))

(defun window-root (window)
  (do ((win window (window-parent win)))
      ((null (window-parent win))
       win)))

(defun window-top-level-window (window)
  (do* ((win window parent)
	(parent (window-parent win) parent-parent)
	(parent-parent (if parent (window-parent parent) T) (window-parent parent)))
       ((null parent-parent) win)
    (when (eq parent-parent t) (return nil))))

;;; Make sure cursor is always visible.
(defmethod stream-advance-cursor-line :after ((stream output-and-window-protocol-intermediary))
  (stream-ensure-cursor-visible stream))

(defmethod stream-set-cursor-position :after ((stream output-and-window-protocol-intermediary)
					      new-x new-y)
  (stream-ensure-cursor-visible stream new-x new-y))

(defmethod stream-set-cursor-position-internal :after
	   ((stream output-and-window-protocol-intermediary) new-x new-y)
  (stream-ensure-cursor-visible stream new-x new-y))

(defmethod stream-ensure-cursor-visible ((stream output-and-window-protocol-intermediary)
					 &optional cx cy)
  (when (or (not (output-recording-stream-p stream))
	    (stream-drawing-p stream))
    (unless cy (multiple-value-setq (cx cy) (stream-cursor-position stream)))
    (with-bounding-rectangle* (vleft vtop vright vbottom) (window-viewport stream)
      (let ((new-x nil)
	    (new-y nil))
	;; Vertical case
	(unless (eq (stream-end-of-page-action stream) ':allow)
	  (incf cy (min (- vbottom vtop)
			(* 2 (stream-line-height stream))))
	  (unless (<= vtop cy vbottom)
	    (setf new-y (max 0 (- cy (- vbottom vtop))))))
	;; Horizontal case
	(unless (eq (stream-end-of-line-action stream) ':allow)
	  (unless (<= vleft cx vright)
	    (setf new-x (max 0 (- cx (- vright vleft 
					(* 4 (stream-character-width stream #\W))))))))
	(when (or new-x new-y)
	  (window-set-viewport-position stream (or new-x vleft) (or new-y vtop)))))))

;;; Rudimentary audio

(defmethod window-beep ((stream t))
  #+Genera (scl:beep)
  #+CCL-2 (ccl::ed-beep)
  nil)

(defun beep (&optional (stream *standard-output*))
  (window-beep stream))

(defun whistle (&key (direction ':down))
  #-Genera (declare (ignore direction))
  #+Genera (compiler:inhibit-style-warnings (dbg:whistle :direction direction))
  #-Genera (repeat 3 (beep)))
