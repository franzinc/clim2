;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: db-stream.lisp,v 1.24 92/07/20 16:00:10 cer Exp Locker: cer $

(in-package :clim-internals)

"Copyright (c) 1990 by International Lisp Associates.  All rights reserved.
 Portions copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 Symbolics, Inc.  All rights reserved."


;;; CLIM stream sheets and panes

;;--- How to keep PANE-BACKGROUND/FOREGROUND in sync with the medium?
;;--- I'm not convinced that including WINDOW-STREAM here is right...
;;--- Note the this allows you to specify default text style, but it
;;--- doesn't pass it along to the medium, so it has no effect.
(defclass clim-stream-sheet 
	  (window-stream			;includes output recording
	   sheet-permanently-enabled-mixin
	   sheet-mute-input-mixin
	   sheet-multiple-child-mixin
	   space-requirement-mixin
	   ;; We depend on PERMANENT-MEDIUM-SHEET-OUTPUT-MIXIN, which
	   ;; we get from PANE...
	   pane)
    ()
  (:default-initargs 
    :medium t 
    ;; Make sure that CLIM streams have fresh translation transformation
    ;; so that we can modify it during scrolling
    :transformation (clim-utils::make-translation-transformation-1 0.0f0 0.0f0)))

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
  (let ((region (sheet-region pane)))
    ;; It should be safe to modify the sheet's region
    (setf (slot-value region 'left) 0
	  (slot-value region 'top)  0
	  (slot-value region 'right)  (max (bounding-rectangle-width pane)
					   (bounding-rectangle-width viewport))
	  (slot-value region 'bottom) (max (bounding-rectangle-height pane)
					   (bounding-rectangle-height viewport)))
    (note-sheet-region-changed pane))
  (setf (stream-default-text-margin pane)
	(bounding-rectangle-width (sheet-region viewport))))

(defmethod invoke-with-drawing-options ((sheet clim-stream-sheet) continuation
					&rest options
					&key ink &allow-other-keys)
  (declare (dynamic-extent options))
  (let ((medium (sheet-medium sheet)))
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
  ;; Make sure that CLIM streams have fresh translation transformation
  ;; so that we can modify it during scrolling
  (setf (sheet-transformation pane)
	(clim-utils::make-translation-transformation-1 0.0f0 0.0f0)))

(defmethod pane-needs-redisplay ((pane clim-stream-pane))
  (with-slots (display-time) pane
    (ecase display-time
      ((t) (setq display-time nil) t)
      ((nil) nil)
      (:command-loop t))))

(defmethod pane-needs-redisplay ((pane pane)) nil)

;;--- Although the unit options are mostly applicable here I guess
;;--- other classes might want to use it also.
;;--- Perhaps we add a COMPOSE-SPACE method on pane which does this.
;;--- Perhaps the class hierarchy needs a big sort out.
(defmacro with-space-requirement ((sr &rest vars) &body body)
  ;; A handy macro that makes it slightly easy to manipulate space requirements
  (unless vars
    (setq vars '(sr-width sr-min-width sr-max-width sr-height sr-min-height sr-max-height)))
  `(multiple-value-bind ,vars
       (space-requirement-components ,sr) 
     (macrolet ((do-with-space-req-components (operator var vars &body body)
		  `(,operator
		    ,@(mapcar #'(lambda (a-var)
				  `(symbol-macrolet ((,var ,a-var))
				     ,@body))
			      vars)))
		(make-sr ()
		  `,'(make-space-requirement
		       ,@(mapcan #'list '(:width :min-width :max-width
					  :height :min-height :max-height)
				 vars))))
       ,@body)))

(defmethod compose-space ((pane clim-stream-pane) &key width height)
  (let ((sr (call-next-method)))
    (labels ((process-compute-space-requirements ()
	       (with-space-requirement (sr)
		 (when (do-with-space-req-components or
			   sr-component 
			   (sr-width sr-min-width sr-max-width
			    sr-height sr-min-height sr-max-height) 
			 (eq sr-component :compute))
		   (multiple-value-bind (width height)
		       (let ((record
			       (let ((*sizing-application-frame* t))
				 (with-output-to-output-record (pane)
				   (invoke-pane-display-function 
				     (pane-frame pane) pane
				     ;;--- Are all pane display functions prepared to
				     ;;--- ignore these arguments?  I think not...
				     :max-width width
				     :max-height height)))))
			 (bounding-rectangle-size record))
		     (do-with-space-req-components progn
		         sr-component (sr-width sr-min-width sr-max-width)
		       (when (eq sr-component :compute)
			 (setq sr-component width)))
		     (do-with-space-req-components progn
		         sr-component (sr-height sr-min-height sr-max-height)
		       (when (eq sr-component :compute)
			 (setq sr-component height))))
		   (setq sr (make-sr)))))
	     (process-unit-space-requirements ()
	       (with-space-requirement (sr)
		 (let ((changed nil))
		   (do-with-space-req-components progn
		       sr-component 
		       (sr-width sr-min-width sr-max-width
			sr-height sr-min-height sr-max-height) 
		     (when (unit-space-requirement-p sr-component)
		       (setq sr-component (process-unit-space-requirement pane sr-component)
			     changed t)))
		   (when changed
		     (setq sr (make-sr))))))
	     (process-relative-space-requirements ()
	       (with-space-requirement (sr)
		 (unless (and (numberp sr-width)
			      (numberp sr-height)
			      (do-with-space-req-components and
				  sr-component 
				  (sr-min-width sr-max-width
				   sr-min-height sr-max-height) 
				(or (numberp sr-component)
				    (relative-space-requirement-p sr-component))))
		   (error "Illegal space requirement ~S" sr))
		 (let ((changed nil))
		   (when (relative-space-requirement-p sr-min-width)
		     (setq sr-min-width (- sr-width (process-unit-space-requirement
						      pane (car sr-min-width)))
			   changed t))
		   (when (relative-space-requirement-p sr-max-width)
		     (setq sr-max-width (+ sr-width (process-unit-space-requirement
						      pane (car sr-max-width)))
			   changed t))
		   (when (relative-space-requirement-p sr-min-height)
		     (setq sr-min-height (- sr-height (process-unit-space-requirement
							pane (car sr-min-height)))
			   changed t))
		   (when (relative-space-requirement-p sr-max-height)
		     (setq sr-max-height (+ sr-height (process-unit-space-requirement
							pane (car sr-max-height)))
			   changed t))
		   (when changed
		     (setq sr (make-sr)))))))
      (declare (dynamic-extent #'process-compute-space-requirements
			       #'process-unit-space-requirements
			       #'process-relative-space-requirements))
      (process-compute-space-requirements)
      (process-unit-space-requirements)
      (process-relative-space-requirements)
      sr)))

(defun relative-space-requirement-p (sr)
  (and (consp sr)
       (= (length sr) 2)
       (or (numberp (second sr))
	   (unit-space-requirement-p (second sr)))))

(defun unit-space-requirement-p (sr)
  (and (consp sr)
       (= (length sr) 2)
       (member (second sr) '(:line :character :mm :point :pixel))))

(defun process-unit-space-requirement (pane sr)
  (destructuring-bind (number unit) sr
    (let ((graft (or (graft pane)
		     (find-graft))))		;--- is this right?
      (ecase unit
	(:pixel number)
	(:mm (* number (/ (graft-pixel-width graft)
			  (graft-mm-width graft))))
	(:point (* number (graft-pixels-per-point graft)))
	(:character (* number (stream-string-width pane "M")))
	(:line (+ (* number (stream-line-height pane))
		  (* (1- number) (stream-vertical-spacing pane))))))))

#+++ignore	;obsolete now that the default coordinate origin is :NW
(defmethod note-sheet-grafted :after ((pane clim-stream-pane))
  (let ((xform (sheet-transformation pane)))
    (setq xform (make-scaling-transformation 1 -1))
    (setf (sheet-transformation pane) xform)))

;; This is a soon-to-be-obsolete method, but we need it for now when the
;; CLIM-STREAM-PANE is a child of the old-style viewport.  It shouldn't
;; get called under the new viewport scheme.
#+++ignore	;obsolete now that the default coordinate origin is :NW
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

;; This assumes that the stream-pane is always inside a viewport, which
;; actually defines its visible size.  The stream pane's size is supposed
;; to represent the size of the contents, but may be stretched to fill the
;; available viewport space.
(defmethod change-space-requirements :around
	   ((pane clim-stream-pane) &rest keys &key width height)
  (declare (dynamic-extent keys))
  ;; Assume always called with width height
  (multiple-value-bind (history-width history-height)
      (if (stream-current-output-record pane)
	  (bounding-rectangle-size (stream-current-output-record pane))
	  (values width height))
    ;; Don't ever shrink down smaller than our contents.
    (if (and (numberp width)
	     (numberp height))
	(apply #'call-next-method pane :width (max width history-width)
				       :height (max height history-height) keys)
	(call-next-method))))


(defclass interactor-pane (clim-stream-pane) ())
(defclass application-pane (clim-stream-pane) ())
(defclass command-menu-pane (clim-stream-pane) ())
(defclass accept-values-pane (clim-stream-pane) ())
(defclass pointer-documentation-pane (clim-stream-pane) ())

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
    (let ((pane `(make-pane ,type ,@options)))
      (when scroll-bars
	(setq pane `(scrolling (:scroll-bars ,scroll-bars)
		      ,pane)))
      (when label
	(setq pane `(vertically ()
		      ,pane
		      (make-pane 'label-pane 
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
  (let ((medium (sheet-medium stream)))
    (letf-globally (((medium-transformation medium) +identity-transformation+))
      (clear-output-history stream)
      (window-erase-viewport stream)
      (when (extended-output-stream-p stream)	;can we assume this?
	;; This is important since if the viewport position is at some
	;; negative position then things get really confused since the
	;; cursor might be visible at (0,0) and the extent is big
	;; enough but...
	;;--- This does a lot of uncessary expensive bitblting, but
	;;--- how do we avoid, it since a lot of what it does we need
	;;--- to do to reset the viewport
	(scroll-extent stream :x 0 :y 0)
	(stream-set-cursor-position stream 0 0)
	(setf (stream-baseline stream) (coordinate 0)
	      (stream-current-line-height stream) (coordinate 0)))
      ;; Flush the old mouse position relative to this window
      ;; so that we don't get bogus highlighted presentations
      ;; when menus first pop up.
      #+++ignore				;--- what is this trying to do?
      (let ((pointer (stream-primary-pointer stream)))
	(when pointer
	  (setf (pointer-sheet pointer) nil)))
      ;; We need to do a FORCE-OUTPUT in case it is a long time before
      ;; anything gets drawn on the same stream.
      (force-output stream))))

;;; Basically a hook for other mixins.
(defmethod window-refresh ((stream clim-stream-sheet))
  (window-erase-viewport stream))

(defmethod window-refresh :after ((stream clim-stream-sheet))
  (frame-replay *application-frame* stream)
  (let ((text-record (stream-text-output-record stream)))
    (when text-record (replay text-record stream))))

(defmethod window-erase-viewport ((stream window-stream))
  (let ((medium (sheet-medium stream)))
    (multiple-value-call #'draw-rectangle*
      medium (bounding-rectangle* (or (pane-viewport stream) stream))
      :ink +background-ink+)))

(defmethod window-expose ((stream clim-stream-sheet))
  (setf (window-visibility stream) t))

;;--- Is there any way to do this?
(defmethod (setf window-label) (label (stream clim-stream-sheet))
  nil)

(defmethod (setf window-visibility) (visibility (stream clim-stream-sheet))
  (if visibility 
      (enable-frame (pane-frame stream))
      (disable-frame (pane-frame stream))))

;;--- This is wrong
(defmethod window-visibility ((stream clim-stream-sheet))
  t)

(defmethod window-viewport ((stream clim-stream-sheet))
  (pane-viewport-region stream))

(defmethod window-viewport-position ((stream clim-stream-sheet))
  (bounding-rectangle-position (pane-viewport-region stream)))

(defmethod window-set-viewport-position ((stream clim-stream-sheet) x y)
  (scroll-extent stream :x x :y y))

(defgeneric* (setf window-viewport-position) (x y stream))
(defmethod* (setf window-viewport-position) (x y (stream clim-stream-sheet))
  (window-set-viewport-position stream x y))

(defmethod window-inside-size ((stream clim-stream-sheet))
  (bounding-rectangle-size (pane-viewport-region stream)))

(defmethod window-set-inside-size ((stream clim-stream-sheet) width height)
  (change-space-requirements stream :width width :height height :resize-frame t))

(defun-inline window-parent (window)
  (sheet-parent window))

(defun window-root (window)
  (graft window))

(defun window-top-level-window (window)
  (do* ((win window parent)
	(parent (window-parent win) parent-parent)
	(parent-parent (if parent (window-parent parent) t) (window-parent parent)))
       ((null parent-parent) win)
    (when (eq parent-parent t)
      (return nil))))

(defun beep (&optional (stream *standard-output*))
  (when (typep stream '(or standard-encapsulating-stream sheet))
    (medium-beep (sheet-medium stream))))

;; This is called by SCROLL-EXTENT.  It shifts a region of the "host screen"
;; that's visible to some other visible location.  It does NOT do any cleaning
;; up after itself.  It does not side-effect the output history of the window.
;; It calls COPY-AREA, whose contract is to do the above, the whole above, and
;; nothing but the above.
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
		     from-y (- delta-y)))
	      ((>= delta-y 0)
	       ;; shifting down and to the left
	       (setq from-x (- delta-x)
		     from-y 0))
	      (t
	       ;; shifting up and to the left
	       (setq from-x (- delta-x)
		     from-y (- delta-y))))
	(let ((width (- stream-width (abs delta-x)))
	      (height (- stream-height (abs delta-y)))
	      (transform (sheet-transformation window)))
	  (multiple-value-call #'copy-area 
	    window
	    (untransform-position transform from-x from-y)
	    (untransform-distance transform width height)
	    (untransform-position transform (+ from-x delta-x) (+ from-y delta-y))))))))

;;;--- Why do we need this?
(defmethod window-shift-visible-region ((window t)
					old-left old-top old-right old-bottom
					new-left new-top new-right new-bottom)
  (declare (ignore old-left old-top old-right old-bottom
		   new-left new-top new-right new-bottom))
  nil)

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
