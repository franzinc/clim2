;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: GENERA-CLIM; Base: 10; Lowercase: Yes -*-

;;--- We really need to handle this for all the ports
(defmethod window-visibility ((stream sheet-implementation-mixin))
  (with-slots (window) stream
    (scl:send window :exposed-p)))

;;--- Whatever became of this??
(defmethod with-clipping-region-1
	   ((stream sheet-implementation-mixin) (region bounding-rectangle) continuation)
  (let ((window (slot-value stream 'window)))
    (multiple-value-bind (vx vy) (window-viewport-position* stream)
      (declare (fixnum vx vy))
      (multiple-value-bind (ml mt) (window-margins stream)
	(declare (fixnum ml mt))
	(with-bounding-rectangle* (left top right bottom) region
	  (with-clim-sheet-clipping-region window
					   (+ (- left vx) ml) (+ (- top vy) mt)
					   (+ (- right vx) ml) (+ (- bottom vy) mt)
					   continuation stream))))))

(scl:defmethod (with-clim-sheet-clipping-region clim-sheet)
	       (left top right bottom continuation stream)
  (tv:with-sheet-clipping-region ((+ left tv:left-margin-size) (+ top tv:top-margin-size)
				  (+ right tv:left-margin-size) (+ bottom tv:top-margin-size))
    (declare (sys:downward-function))		;yecch
    (funcall continuation stream)))

;;--- Genera port should really use Genera cursors
(defclass sheet-window-stream
	  (sheet-implementation-mixin window-stream)
    ()
  (:default-initargs :display-device-type nil
		     :text-cursor (make-instance 'sheet-text-cursor)))

(defmethod stream-clear-input :after ((stream sheet-window-stream))
  (with-slots (window) stream
    (scl:send window :clear-input)))


;;--- Naha's hack for handling Solstices properly

;;; Elements are ((character-set . display-device-class) . hash-table)
(defvar *character-set-text-style-map-alist* nil)

(defun text-style-mapping-character-style (device character-set text-style)
  (let ((tbl (assoc-if #'(lambda (charset.device)
			   (and (eq (car charset.device) character-set)
				(eq (cdr charset.device) (class-of device))))
		       *character-set-text-style-map-alist*))
	(style (standardize-text-style device character-set (parse-text-style text-style)))
	result)
    (when tbl
      (setq result (gethash style (cdr tbl) nil)))
    (unless result
      (error "No character style for ~a for ~a ~a" style device character-set))
    result))

(defmethod add-text-style-mapping :after ((display-device sheet-device)
					  character-set text-style font)
  (let ((tbl (assoc-if #'(lambda (charset.device)
			   (and (eq (car charset.device) character-set)
				(eq (cdr charset.device) (class-of display-device))))
		       *character-set-text-style-map-alist*)))
    (unless tbl
      (setq tbl (cons (cons character-set (class-of display-device))
		      (make-hash-table)))
      (push tbl *character-set-text-style-map-alist*))
    (setq tbl (cdr tbl))
    (let ((character-style (si:backtranslate-font font si:*b&w-screen*)))
      (if character-style
	  (setf (gethash text-style tbl) character-style)
	  (warn "No mapping for ~a in ~a ~a ~a" font text-style
		display-device character-set)))))


;;--- We really do want to use Genera cursors...

(defclass sheet-text-cursor
	  (cursor)
    ((x :initarg :x :initform 0)		;needed if cursor is scrolled off viewport
     (y :initarg :y :initform 0)
     (stream :initarg :stream)
     (active :initform nil)			;false if visibility is :inactive
     (blinker :initform nil)))			;Genera window system blinker to be used

(defmethod cursor-active-p ((cursor sheet-text-cursor))
  (not (null (slot-value cursor 'active))))

(defmethod (setf cursor-stream) (new-value (cursor sheet-text-cursor))
  (setf (slot-value cursor 'stream) new-value)
  (let ((blinker (slot-value cursor 'blinker)))
    (when blinker
      (tv:blinker-set-sheet blinker (slot-value new-value 'window))))
  new-value)

#-Silica
(defmethod cursor-position* ((cursor sheet-text-cursor))
  (values (slot-value cursor 'x) (slot-value cursor 'y)))

#-Silica
(defmethod cursor-set-position* ((cursor sheet-text-cursor) x y)
  (setf (slot-value cursor 'x) x
	(slot-value cursor 'y) y)
  (let ((blinker (slot-value cursor 'blinker)))
    (when blinker
      (multiple-value-bind (x y)
	  (drawing-surface-to-viewport-coordinates (slot-value cursor 'stream) x y)
	(tv:blinker-set-cursorpos blinker x y)))))

(defmethod cursor-visibility ((cursor sheet-text-cursor))
  (let ((active (slot-value cursor 'active))
	(blinker (slot-value cursor 'blinker)))
    (cond ((not active) :inactive)
	  ((not blinker) :off)
	  ((member (tv:blinker-visibility blinker) '(:blink :on t)) :on)
	  (t :off))))

(defmethod (setf cursor-visibility) (new-visibility (cursor sheet-text-cursor))
  (setf (slot-value cursor 'active) (not (eq new-visibility :inactive)))
  (let ((stream (slot-value cursor 'stream))
	(blinker (slot-value cursor 'blinker)))
    (when (and (null blinker) (eq new-visibility :on))
      ;; It isn't safe to create the blinker any earlier than this, because of the
      ;; order of initializations while creating a sheet-window-stream
      (setq blinker 
	    (setf (slot-value cursor 'blinker)
		  (flavor:make-instance 'tv:rectangular-blinker
					:sheet (slot-value stream 'window)
					:visibility nil
					:deselected-visibility nil
					:follow-p nil)))
      (multiple-value-bind (x y)
	  (drawing-surface-to-viewport-coordinates stream (slot-value cursor 'x)
							  (slot-value cursor 'y))
	(tv:blinker-set-cursorpos blinker x y)))
    (when blinker
      (tv:blinker-set-visibility blinker (if (eq new-visibility :on)
					     (if (tv:sheet-selected-p
						   (slot-value stream 'window))
						 :blink
						 :on)
					     nil))
      (scl:send blinker :set-deselected-visibility (if (eq new-visibility :on) :on nil))))
  new-visibility)

;; The window system draws it, so we don't have to
(defmethod draw-cursor ((cursor sheet-text-cursor) on-p)
  on-p)
