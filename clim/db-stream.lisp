;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

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
;;;
;;; Copyright (c) 1990 by Xerox Corporations.  All rights reserved.
;;;
;; $fiHeader: db-stream.lisp,v 1.8 92/02/24 13:07:16 cer Exp $

(in-package :clim-internals)

"Improvements Copyright (c) 1990 by International Lisp Associates.  All rights reserved."


;;; CLIM stream sheets and panes

;;--- How to keep PANE-BACKGROUND/FOREGROUND in sync with the medium?
(defclass clim-stream-sheet 
	  (window-stream			;includes output recording
	   pane
	   sheet-permanently-enabled-mixin
	   sheet-mute-input-mixin
	   sheet-multiple-child-mixin
	   mute-repainting-mixin
	   space-requirement-mixin)
    ()
  (:default-initargs 
    :medium t 
    ;;:max-width +fill+ :min-width 0
    ;;:max-height +fill+ :min-height 0
    ))

;;--- Do we still need this?
(defmethod pane-stream ((pane clim-stream-sheet))
  pane)

(defmethod note-sheet-region-changed :after ((pane clim-stream-sheet) &key &allow-other-keys)
  (let ((viewport (pane-viewport pane)))
    (setf (stream-default-text-margin pane)
	  (if viewport
	      (bounding-rectangle-width (sheet-region viewport))
	      (bounding-rectangle-width pane)))))

(defmethod viewport-region-changed ((pane t) viewport)
  (declare (ignore viewport)))

(defmethod viewport-region-changed ((pane clim-stream-sheet) viewport)
  (setf (sheet-region pane)
	(make-bounding-rectangle
	  0 0 
	  (max (bounding-rectangle-width pane)
	       (bounding-rectangle-width viewport))
	  (max (bounding-rectangle-height pane)
	       (bounding-rectangle-height viewport))))
  (setf (stream-default-text-margin pane)
	(bounding-rectangle-width (sheet-region viewport))))

(defmethod invoke-with-drawing-options ((sheet clim-stream-sheet) continuation
					&rest options
					&key ink &allow-other-keys)
  (declare (dynamic-extent options))
  (with-sheet-medium (medium sheet)
    ;; Close the current output record if the drawing ink is changing
    (unless (eq (medium-ink medium) ink)
      (stream-close-text-output-record sheet))
    (apply #'invoke-with-drawing-options medium continuation options)))

(defmethod default-space-requirements ((pane clim-stream-sheet)
				       &key (width 0 widthp)
					    (min-width width)
					    (max-width (if widthp width +fill+))
					    (height 0 heightp)
					    (min-height height)
					    (max-height (if heightp height +fill+)))
  (values width min-width max-width height min-height max-height))

(defclass clim-stream-pane (clim-stream-sheet)
    ((incremental-redisplay-p
       :initarg :incremental-redisplay :initform nil)
     (display-function 
       :reader pane-display-function
       :initarg :display-function :initform nil)
     (display-time
       :reader pane-display-time
       :initarg :display-time :initform :command-loop		
       :type (member nil :command-loop t))))
	   
(defmethod initialize-instance :after 
	   ((pane clim-stream-pane) &key &allow-other-keys)
  (setf (sheet-transformation pane) +identity-transformation+))

(defmethod compose-space ((pane clim-stream-pane) &key width height)
  (let ((sr (call-next-method)))
    (let ((sr-components
	    (multiple-value-list (space-requirement-components sr))))
      (when (member :compute sr-components)
	(multiple-value-bind (width height)
	    (let ((record
		    (with-output-to-output-record (pane)
		      (invoke-pane-redisplay-function 
			(pane-frame pane) pane
			;;--- Are all pane display functions prepared to
			;;--- ignore these arguments?  I think not...
			:max-width width
			:max-height height))))
	      (bounding-rectangle-size record))
	  ;;--- Yabba dabba doo!
	  (setq sr-components (nsubstitute width :compute sr-components :end 3))
	  (setq sr-components (nsubstitute height :compute sr-components :start 3)))
	(setq sr (make-space-requirement 
		   :width      (pop sr-components)
		   :min-width  (pop sr-components)
		   :max-width  (pop sr-components)
		   :height     (pop sr-components)
		   :min-height (pop sr-components)
		   :max-height (pop sr-components)))))
    sr))

#+ignore
(defmethod note-sheet-grafted :after ((pane clim-stream-pane))
  (let ((xform (sheet-transformation pane)))
    (setq xform (make-scaling-transformation 1 -1))
    (setf (sheet-transformation pane) xform)))

;;; This is a soon-to-be-obsolete method, but we need it for now when the
;;; CLIM-STREAM-PANE is a child of the old-style viewport.  It shouldn't
;;; get called under the new viewport scheme.

#+ignore
(defmethod allocate-space :after ((pane clim-stream-pane) width height)
  (declare (ignore width height))
  (ecase (graft-origin (graft pane))
    (:nw)
    (:sw
     (let ((xform (sheet-transformation pane)))
       (setq xform (make-scaling-transformation 1 -1))
       ;; --- stream-panes ALWAYS have to have a parent to manage the
       ;; viewport clipping, etc.
       (setq xform (compose-transformations
		    xform
		    (make-translation-transformation
		     0 (1- (bounding-rectangle-height
			    (sheet-parent pane))))))
       (setf (sheet-transformation pane) xform)))))

(defmethod pane-stream ((pane clim-stream-pane))
  (unless (port pane) 
    (error "Can't call ~S on ~S until it's been grafted!"
	   'pane-stream pane))
  pane)

#+ignore
(defmethod update-region ((pane clim-stream-pane) width height
			  &key no-repaint &allow-other-keys)
  (when (pane-scroller pane)
    (update-extent (pane-viewport pane) width height
		   :no-repaint no-repaint)))

;;; ---This assumes that the stream-pane is always inside a viewport, which
;;; actually defines its visible size.  The stream pane's size is supposed
;;; to represent the size of the contents, but may be stretched to fill the
;;; available viewport space.

(defmethod change-space-requirement :around
	   ((pane clim-stream-pane) &rest keys &key width height)
  (declare (dynamic-extent keys))
  ;; Assume always called with width height
  (multiple-value-bind (history-width history-height)
      (if (stream-current-output-record pane)
	  (bounding-rectangle-size (stream-current-output-record pane))
	  (values width height))
    ;; Don't ever shrink down smaller than our contents.
    (apply #'call-next-method pane :width (max width history-width)
				   :height (max height history-height) keys)))


(defclass interactor-pane (clim-stream-pane) ())
(defclass application-pane (clim-stream-pane) ())
(defclass command-menu-pane (clim-stream-pane) ())

(defmethod compose-space :before ((pane command-menu-pane) &key width height)
  (declare (ignore width height))
  (window-clear pane))

;; This is a macro because it counts on being expanded inside of a call
;; to WITH-LOOK-AND-FEEL-REALIZATION
(defmacro make-clim-stream-pane (&rest options
				 &key (type ''clim-stream-pane) 
				      label (scroll-bars ':vertical)
				 &allow-other-keys)
  (with-keywords-removed (options options '(:type :label :scroll-bars))
    (macrolet ((setf-unless (slot-keyword value)
		 `(when (eq (getf options ',slot-keyword #1='#:default) #1#)
		    (setf (getf options ',slot-keyword) ,value))))
      (setf-unless :width 100)
      (setf-unless :min-width 0)
      (setf-unless :max-width +fill+)
      (setf-unless :height 100)
      (setf-unless :min-height 0)
      (setf-unless :max-height +fill+))
    (let ((pane `(realize-pane ,type ,@options))
	  (hscroll (not (null (member scroll-bars '(t :both :horizontal)))))
	  (vscroll (not (null (member scroll-bars '(t :both :vertical))))))
      (when scroll-bars
	(setq pane `(scrolling (:horizontally ,hscroll
				:vertically ,vscroll)
		      ,pane)))
      (when label
	(setq pane `(vertically ()
		      ,pane
		      (realize-pane 'label-pane 
				    :text ,label
				    :max-width +fill+))))
      `(outlining (:thickness 1)
	 ,pane))))

(defmacro make-clim-interactor-pane (&rest options)
  `(make-clim-stream-pane :type 'interactor-pane ,@options))

(defmacro make-clim-application-pane (&rest options)
  `(make-clim-stream-pane :type 'application-pane ,@options))


;;; "Window protocol"

(defmethod window-clear ((stream clim-stream-sheet))
  (with-sheet-medium (medium stream)
    (letf-globally (((medium-transformation medium) +identity-transformation+))
      (clear-output-history stream)
      (window-erase-viewport stream)
      (when (extended-output-stream-p stream)	;can we assume this?
	(stream-set-cursor-position* stream 0 0)
	(setf (stream-baseline stream) 0
	      (stream-current-line-height stream) 0))
      ;; Flush the old mouse position relative to this window
      ;; so that we don't get bogus highlighted presentations
      ;; when menus first pop up.
      (let ((pointer (stream-primary-pointer stream)))
	(when pointer
	  (setf (pointer-window pointer) nil)))
      ;; Doesn't really need to do force-output.
      (force-output stream))))

;;; Basically a hook for other mixins.
(defmethod window-refresh ((stream clim-stream-sheet))
  (window-erase-viewport stream))

(defmethod window-refresh :after ((stream clim-stream-sheet))
  (frame-replay *application-frame* stream)
  (let ((text-record (stream-text-output-record stream)))
    (when text-record (replay text-record stream))))

(defmethod window-erase-viewport ((stream window-stream))
  (with-output-recording-options (stream :record nil)
    (multiple-value-call #'draw-rectangle*
      (sheet-medium stream)
      (bounding-rectangle* (or (pane-viewport stream)
			       stream))
      :ink +background-ink+)))

(defmethod window-expose ((stream clim-stream-sheet))
  (setf (window-visibility stream) t))

(defmethod (setf window-label) (label (stream clim-stream-sheet))
  nil)

(defmethod (setf window-visibility) (nv (stream clim-stream-sheet))
  (if nv 
      (enable-frame (pane-frame stream))
      (disable-frame (pane-frame stream))))

;;--- This is wrong
(defmethod window-visibility ((stream clim-stream-sheet))
  t)

(defmethod window-viewport ((stream clim-stream-sheet))
  (pane-viewport-region stream))

(defmethod window-viewport-position* ((stream clim-stream-sheet))
  (bounding-rectangle-position* (pane-viewport-region stream)))

(defmethod window-set-viewport-position* ((stream clim-stream-sheet) x y)
  (scroll-extent stream :x x :y y))

(defmethod window-inside-size ((stream clim-stream-sheet))
  (bounding-rectangle-size (pane-viewport-region stream)))

(defmethod window-set-inside-size ((stream clim-stream-sheet) width height)
  (change-space-requirement stream :width width :height height)
  (clear-space-requirement-caching-in-ancestors stream)
  (layout-frame (pane-frame stream)))

(defun-inline window-parent (window)
  (sheet-parent window))

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

(defun beep (&optional (stream *standard-output*))
  (port-beep (port stream) stream))

;; This is called by OUTPUT-RECORDING-MIXIN's whopper on set-viewport-position*.
;; It shifts a region of the "host screen" that's visible to some other visible
;; location.  It does NOT do any cleaning up after itself.  It does not side-effect
;; the output history of the window.  It calls COPY-AREA whose contract is to 
;; do the above, the whole above, and nothing but the above.
(defmethod window-shift-visible-region ((window clim-stream-sheet)
					old-left old-top old-right old-bottom
					new-left new-top new-right new-bottom)
  (declare (type coordinate new-left new-top new-right new-bottom))
  (declare (ignore old-right old-bottom new-right new-bottom))
  (let ((delta-x (- old-left new-left))
	(delta-y (- old-top new-top)))
    (multiple-value-bind (stream-width stream-height)
	(bounding-rectangle-size (pane-viewport-region window))
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
	      (values 0 0)
	    (declare (type coordinate ml mt))
	    (translate-fixnum-positions ml mt from-x from-y)
	    (let ((tf (sheet-transformation window)))
 	      (multiple-value-call #'copy-area 
		window
		(untransform-point* tf from-x from-y)
		(untransform-point* tf (+ from-x width) (+ from-y height))
		(untransform-point* tf (+ from-x delta-x) (+ from-y delta-y))))))))))

#+Genera
(defgeneric stream-compatible-inside-size (window)
  (:selector :inside-size))

#+Genera
(defmethod stream-compatible-inside-size ((window clim-stream-sheet))
  (bounding-rectangle-size (pane-viewport-region window)))

#+Genera
(defgeneric stream-compatible-visible-cursorpos-limits (window &optional unit)
  (:selector :visible-cursorpos-limits))

#+Genera
(defmethod stream-compatible-visible-cursorpos-limits 
	   ((window clim-stream-sheet) &optional (unit ':pixel))
  (with-bounding-rectangle* (left top right bottom) (pane-viewport-region window)
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
(defmethod stream-compatible-size-in-characters ((window clim-stream-sheet))
  (with-bounding-rectangle* (left top right bottom) (pane-viewport-region window)
    (let ((char-width (stream-character-width window #\M))
	  (line-height (stream-line-height window)))
      (values (floor (- right left) char-width)
	      (floor (- bottom top) line-height)))))
