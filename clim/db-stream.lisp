;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: db-stream.lisp,v 1.51 93/05/13 16:23:01 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990 by International Lisp Associates.  All rights reserved.
 Portions copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 Symbolics, Inc.  All rights reserved."


;;; CLIM stream sheets and panes

;;--- How to keep PANE-BACKGROUND/FOREGROUND in sync with the medium?
;;--- I'm not convinced that including WINDOW-STREAM here is right...
(defclass clim-stream-sheet 
	  (window-stream			;includes output recording
	   sheet-permanently-enabled-mixin
	   sheet-mute-input-mixin
	   sheet-multiple-child-mixin
	   space-requirement-mixin
	   space-requirement-cache-mixin
	   permanent-medium-sheet-output-mixin
	   basic-pane)
    ((input-editor-stream :initform nil :accessor stream-input-editor-stream))
  (:default-initargs 
    :medium t 
    :transformation +identity-transformation+))

(defmethod stream-input-editor-stream ((stream sheet)) nil)
(defmethod (setf stream-input-editor-stream) (value (stream sheet))
  value)

(defmethod stream-input-editor-stream ((stream standard-encapsulating-stream)) 
  (stream-input-editor-stream (encapsulating-stream-stream stream)))

(defmethod (setf stream-input-editor-stream) (value (stream standard-encapsulating-stream)) 
  (setf (stream-input-editor-stream (encapsulating-stream-stream stream)) value))

(defun maybe-redraw-input-editor-stream (stream region)
  (let ((input-editor-stream (stream-input-editor-stream stream)))
    (when input-editor-stream
      (multiple-value-bind (x-pos y-pos)
	(input-buffer-input-position->cursor-position input-editor-stream 0)
	(when (region-contains-position-p (or region +everywhere+) x-pos y-pos)
	  (with-end-of-page-action (input-editor-stream :allow)
	    (redraw-input-buffer input-editor-stream)))))))

(defmethod handle-repaint :after ((sheet clim-stream-sheet) region)
  (maybe-redraw-input-editor-stream sheet region))

;;--- Do we still need this?
(defmethod pane-stream ((pane clim-stream-sheet))
  pane)

(defmethod note-sheet-region-changed :after ((pane clim-stream-sheet) &key &allow-other-keys)
  (setf (stream-default-text-margin pane)
	(bounding-rectangle-width (window-viewport pane))))

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


(defmethod update-region ((sheet clim-stream-sheet) nleft ntop nright nbottom &key no-repaint)
  (declare (ignore no-repaint))
  (with-bounding-rectangle* (left top right bottom) sheet
    (when (or (< nleft left)
	      (< ntop  top)
	      (> nright  right)
	      (> nbottom bottom))
      ;; It should be safe to modify the sheet's region
      (let ((region (sheet-region sheet)))
	(setf (slot-value region 'left) (min nleft left)
	      (slot-value region 'top)  (min ntop  top)
	      (slot-value region 'right)  (max nright  right)
	      (slot-value region 'bottom) (max nbottom bottom))
	(note-sheet-region-changed sheet)))))


(defmethod invoke-with-drawing-options ((sheet clim-stream-sheet) continuation
					&rest options
					&key ink &allow-other-keys)
  (declare (dynamic-extent options))
  (let* ((medium (sheet-medium sheet))
	 (ink-changing (and ink (not (eq (medium-ink medium) ink)))))
    (when ink-changing
      ;; Close the current output record if the drawing ink is changing
      (stream-close-text-output-record sheet))
    (multiple-value-prog1
      (apply #'invoke-with-drawing-options medium continuation options)
      (when ink-changing
	;; If it changed on the way in, it's changing back on the way out
	;; This might create more text output records that it should, but
	;; better to be safe than sorry
	(stream-close-text-output-record sheet)))))

(defmethod default-space-requirements ((pane clim-stream-sheet)
				       &key (min-width 0)
					    (width 100)
					    (max-width +fill+)
					    (min-height 0)
					    (height 100)
					    (max-height +fill+))
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
       :type (member nil :command-loop :no-clear t))))

;;--- Need a way to set PANE-NEEDS-REDISPLAY, right?
(defmethod pane-needs-redisplay ((pane clim-stream-pane))
  (declare (values needs-redisplay clear))
  (with-slots (display-time) pane
    (ecase display-time
      ((t)
       (setq display-time nil) 
       (values t t))
      ((nil)
       (values nil nil))
      (:command-loop
       (values t t))
      (:no-clear 
       (values t nil)))))

(defmethod pane-needs-redisplay ((pane basic-pane)) 
  (values nil nil))

;;--- Although the unit options are mostly applicable here I guess
;;--- other classes might want to use it also.
;;--- Perhaps we add a COMPOSE-SPACE method on pane which does this.
;;--- Perhaps the class hierarchy needs a big sort out.
#+Genera (zwei:defindentation (do-with-space-req-components 0 3 1 3 2 3 3 1))
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
    (labels 
      ((process-compute-space-requirements ()
	 (with-space-requirement (sr)
	   (when (do-with-space-req-components or
		     sr-component 
		     (sr-width sr-min-width sr-max-width
		      sr-height sr-min-height sr-max-height) 
		   (eq sr-component :compute))
	     (multiple-value-bind (width height)
		 (let ((record
			 (let ((history (stream-output-history pane)))
			   (if (and history
				    (> (output-record-count history :fastp t) 0))
			       history
			       (let ((*sizing-application-frame* t))
				 (with-output-to-output-record (pane)
				   (funcall 
				     (if (slot-value pane 'incremental-redisplay-p)
					 #'invoke-pane-redisplay-function 
					 #'invoke-pane-display-function)
				     (pane-frame pane) pane
				     ;;--- Are all pane display functions prepared to
				     ;;--- ignore these arguments?  I think not...
				     :max-width width
				     :max-height height)))))))
		   (with-bounding-rectangle* (left top right bottom) record
		     (values (- right (min 0 left))
			     (- bottom (min 0 top)))))
	       (when (zerop width) (setq width 100))
	       (when (zerop height) (setq height 100))
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
	;; Stream panes always have to have a parent to manage the
	;; viewport clipping, etc.
	(setq xform (compose-transformations
		      xform
		      (make-translation-transformation
			0 (1- (bounding-rectangle-height (sheet-parent pane))))))
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
	   ((pane clim-stream-pane) &rest keys &key width height &allow-other-keys)
  (declare (dynamic-extent keys))
  ;; Assume always called with width and height
  (multiple-value-bind (history-width history-height)
      (if (stream-output-history pane)
	  (bounding-rectangle-size (stream-output-history pane))
	  (values width height))
    ;; Don't ever shrink down smaller than our contents.
    (if (and (numberp width)
	     (numberp height))
	(apply #'call-next-method pane :width (max width history-width)
				       :height (max height history-height) keys)
	(call-next-method))))


(defclass interactor-pane (clim-stream-pane) ())
(defclass application-pane (clim-stream-pane) ())

(defclass accept-values-pane (clim-stream-pane) ())

(defclass pointer-documentation-pane (clim-stream-pane) ())

(defclass title-pane (clim-stream-pane) 
    ((display-string :initform nil :initarg :display-string)))

(defclass command-menu-pane (clim-stream-pane) ())

(defmethod compose-space :before ((pane command-menu-pane) &key width height)
  (declare (ignore width height))
  ;;------ !!!!!!!!!!!!!!!!!!!!!!
  ;;
  #+ignore
  (window-clear pane))


;; This is a macro because it counts on being expanded inside of a call
;; to WITH-LOOK-AND-FEEL-REALIZATION
(defmacro make-clim-stream-pane (&rest options
				 &key (type ''clim-stream-pane) 
				      label 
				      (label-alignment #+Genera :bottom #-Genera :top)
				      (scroll-bars ':vertical)
				      (borders t)
				      (display-after-commands nil dac-p)
				      (background nil background-p)
				      name
				 &allow-other-keys)
  (setq options (remove-keywords options '(:type :scroll-bars :borders
					   :label :background
					   :label-alignment :display-after-commands)))
  (let* ((stream '#:clim-stream)
	 (display-time
	   (and dac-p
		`(:display-time ,(cond ((eq display-after-commands t) :command-loop)
				       ((eq display-after-commands :no-clear) :no-clear)
				       (t nil)))))
	 (background-var (and background (gensym)))
	 (pane
	   `(setq ,stream (make-pane ,type 
				     ,@display-time
				     ,@(and background-p `(:background ,background-var))
				     ,@options))))
    (when scroll-bars
      (setq pane `(scrolling
		      (:scroll-bars ,scroll-bars
		       :name ,name
		       ,@(and background-p `(:background ,background-var)))
		    ,pane)))
    (when label
      (let ((label (if (stringp label)
		       `(make-pane 'label-pane 
			  :label ,label
			  :max-width +fill+
			   ,@(and background-p `(:background ,background-var)))
		       `(make-pane 'label-pane 
			  :label ,(first label)
			  :max-width +fill+ ,@(rest label)
			   ,@(and background-p `(:background ,background-var))))))
	(ecase label-alignment
	  (:bottom
	   (setq pane `(vertically 
			   (,@(and background-p `(:background ,background-var)))
			 ,pane
			 ,label)))
	  (:top
	   (setq pane `(vertically 
			   (,@(and background-p `(:background ,background-var)))
			 ,label
			 ,pane))))))
    (when borders 
      (setq pane `(outlining 
		      (:thickness 1 
		       :name ,name
		       ,@(and background-p `(:background ,background-var)))
		    (spacing (:thickness 1
			      ,@(and background-p `(:background ,background-var)))
		      ,pane))))
    `(let ((,stream)
	   ,@(and background-p `((,background-var ,background))))
       (values ,pane ,stream))))

(defmacro make-clim-interactor-pane (&rest options)
  `(make-clim-stream-pane :type 'interactor-pane ,@options))

(defmacro make-clim-application-pane (&rest options)
  `(make-clim-stream-pane :type 'application-pane ,@options))


;;; "Window protocol"

(defun-inline window-stream-p (x)
  (typep x 'clim-stream-sheet))

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
	;;--- how do we avoid it, since a lot of what it does we need
	;;--- to do to reset the viewport
	(scroll-extent stream 0 0)
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

(defmethod window-refresh ((stream clim-stream-sheet))
  (window-erase-viewport stream))

(defmethod window-refresh :after ((stream clim-stream-sheet))
  (frame-replay *application-frame* stream)
  (let ((text-record (stream-text-output-record stream)))
    (when text-record (replay text-record stream)))
  (let ((presentation (highlighted-presentation stream nil)))
    (when presentation
      (highlight-presentation 
	presentation (presentation-type presentation) stream :highlight))))

(defmethod window-refresh :around ((stream clim-stream-sheet))
  (with-viewport-position-saved (stream)
    (call-next-method)))

(defmethod window-erase-viewport ((stream clim-stream-sheet))
  (let ((medium (sheet-medium stream)))
    (multiple-value-call #'medium-clear-area
      medium (bounding-rectangle* (window-viewport stream)))))

(defmethod window-expose ((stream clim-stream-sheet))
  (setf (window-visibility stream) t))

;;--- Is there any way to do this?
(defmethod (setf window-label) (label (stream clim-stream-sheet))
  nil)

(defmethod (setf window-visibility) (visibility (stream clim-stream-sheet))
  (let ((frame (pane-frame stream)))
    (if frame
	(if visibility (enable-frame frame) (disable-frame frame))
	(setf (sheet-enabled-p stream) visibility))))

(defmethod window-visibility ((stream clim-stream-sheet))
  (mirror-visible-p (port stream) stream))

(defmethod window-viewport ((stream clim-stream-sheet))
  (or (pane-viewport-region stream)
      ;; Not a scrolling pane, so the sheet's region is the viewport
      (sheet-region stream)))

(defmethod window-viewport-position ((stream clim-stream-sheet))
  (bounding-rectangle-position (window-viewport stream)))

(defmethod window-set-viewport-position ((stream clim-stream-sheet) x y)
  (when (pane-viewport stream)
    (scroll-extent stream x y)))

(defgeneric* (setf window-viewport-position) (x y stream))
(defmethod* (setf window-viewport-position) (x y (stream clim-stream-sheet))
  (window-set-viewport-position stream x y))

(defmethod window-inside-edges ((stream clim-stream-sheet))
  (bounding-rectangle* (sheet-region (or (pane-viewport stream) stream))))

(defun window-inside-left (stream)
  (multiple-value-bind (left top right bottom)
      (window-inside-edges stream)
    (declare (ignore top right bottom))
    left))

(defun window-inside-top (stream)
  (multiple-value-bind (left top right bottom)
      (window-inside-edges stream)
    (declare (ignore left right bottom))
    top))

(defun window-inside-right (stream)
  (multiple-value-bind (left top right bottom)
      (window-inside-edges stream)
    (declare (ignore left top bottom))
    right))

(defun window-inside-bottom (stream)
  (multiple-value-bind (left top right bottom)
      (window-inside-edges stream)
    (declare (ignore left top right))
    bottom))

(defmethod window-inside-size ((stream clim-stream-sheet))
  (bounding-rectangle-size (window-viewport stream)))

(defmethod window-set-inside-size ((stream clim-stream-sheet) width height)
  (change-space-requirements stream :width width :height height :resize-frame t))

(defmethod window-inside-width ((stream clim-stream-sheet))
  (bounding-rectangle-width (window-viewport stream)))

(defmethod window-inside-height ((stream clim-stream-sheet))
  (bounding-rectangle-height (window-viewport stream)))

(defmethod window-margins ((stream clim-stream-sheet))
  (values (coordinate 0) (coordinate 0)
	  (coordinate 0) (coordinate 0)))

(defun-inline window-parent (window)
  (sheet-parent window))

(defun-inline window-children (window)
  (sheet-children window))

(defun window-root (window)
  (graft window))

(defun-inline window-top-level-window (window)
  (sheet-top-level-sheet window))

(defmethod window-stack-on-bottom ((stream clim-stream-sheet))
  (bury-sheet (window-top-level-window stream)))

(defmethod window-stack-on-top ((stream clim-stream-sheet))
  (raise-sheet (window-top-level-window stream)))

(defun beep (&optional (stream *standard-output*))
  (typecase stream
    (sheet
      (medium-beep (sheet-medium stream)))
    (encapsulating-stream
      (beep (encapsulating-stream-stream stream)))))

;; If you close window in a frame, just exit from the frame.
;; Otherwise, destroy directly mirrored sheets, or just disable the sheet.
;;--- This functionality is dubious
(defmethod close ((sheet clim-stream-sheet) &key abort)
  (declare (ignore abort))
  (let ((frame (pane-frame sheet)))
    (if frame
        (frame-exit frame)
	(if (sheet-direct-mirror sheet)
	    (destroy-mirror (port sheet) sheet)
	    (setf (sheet-enabled-p sheet) nil)))))

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
  (bounding-rectangle-size (window-viewport window)))

#+Genera
(defgeneric stream-compatible-visible-cursorpos-limits (window &optional unit)
  (:selector :visible-cursorpos-limits))

#+Genera
(defmethod stream-compatible-visible-cursorpos-limits 
	   ((window clim-stream-sheet) &optional (unit ':pixel))
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
(defmethod stream-compatible-size-in-characters ((window clim-stream-sheet))
  (with-bounding-rectangle* (left top right bottom) (window-viewport window)
    (let ((char-width (stream-character-width window #\M))
	  (line-height (stream-line-height window)))
      (values (floor (- right left) char-width)
	      (floor (- bottom top) line-height)))))


;;; This is too useful to simply omit
(defun open-window-stream (&key left top right bottom width height
				(foreground +black+) (background +white+)
				text-style default-text-style
				(vertical-spacing 2)
				(end-of-line-action :allow)
				(end-of-page-action :allow)
				output-record (draw t) (record t)
				(initial-cursor-visibility :off)
				text-margin default-text-margin
				save-under input-buffer
				(scroll-bars :vertical) borders label)
  (declare (ignore borders input-buffer))
  (when (or width height)
    (assert (and (null right) (null bottom))))
  (when (null left) (setq left 0))
  (when (null top)  (setq top 0))
  (when (or (null width) (null height))
    (if (or (null right) (null bottom))
	(setq width 100
	      height 100)
	(setq width (- right left)
	      height (- bottom top))))
  (let* ((stream
	  ;;-- This should call frame-manager-get-menu to get
	  ;;-- the right kind of menu-frame
	  (frame-manager-get-menu (find-frame-manager)
					       ;;-- what about :save-under
					       :label label
					       :scroll-bars scroll-bars))
	 (port (port stream)))
    (setf (medium-foreground stream) foreground
	  (medium-background stream) background
	  (medium-default-text-style stream) (or default-text-style *default-text-style*)
	  (medium-text-style stream) (or text-style *default-text-style*)
	  (stream-vertical-spacing stream) vertical-spacing
	  (stream-end-of-line-action stream) end-of-line-action
	  (stream-end-of-page-action stream) end-of-page-action
	  (stream-recording-p stream) record
	  (stream-drawing-p stream) draw
	  (cursor-visibility (stream-text-cursor stream)) initial-cursor-visibility
	  (stream-text-margin stream) text-margin
	  (stream-default-text-margin stream) default-text-margin)
    (when output-record
      (stream-output-history stream) (make-instance output-record))
    (window-set-inside-size stream width height)
    (window-set-viewport-position stream 0 0)
    (position-sheet-carefully
      (frame-top-level-sheet (pane-frame stream)) width height)
    stream))
