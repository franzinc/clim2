;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

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
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: layout.lisp,v 1.20 92/07/20 15:59:21 cer Exp Locker: cer $

(in-package :silica)


(defclass space-requirement ()
    ((width :initarg :width :accessor space-requirement-width)
     (max-width :accessor space-requirement-max-width)
     (min-width :accessor space-requirement-min-width)
     (height :initarg :height :accessor space-requirement-height)
     (max-height :accessor space-requirement-max-height)
     (min-height :accessor space-requirement-min-height)))

(defmethod space-requirement-components ((sr space-requirement))
  (with-slots (width min-width max-width
	       height min-height max-height) sr
    (values width min-width max-width
	    height min-height max-height)))

(defmethod print-object ((object space-requirement) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (min-width width max-width min-height height max-height) object
      (format stream "~D,~D,~D ~D,~D,~D"
	min-width width  max-width 
	min-height height max-height))))

(defmethod copy-space-requirement ((sr space-requirement))
  (with-slots (width height max-width max-height min-width min-height) sr
    (make-instance 'space-requirement
      :width width
      :height height
      :max-width max-width
      :max-height max-height
      :min-width min-width
      :min-height min-height)))

(defmethod initialize-instance :after ((sr space-requirement)
				       &key
				       (width (error "width not specified"))
				       (min-width width)
				       (max-width width)
				       (height (error "height not specified"))
				       (max-height height)
				       (min-height height))
  (setf (slot-value sr 'min-height) min-height
	(slot-value sr 'max-height) max-height
	(slot-value sr 'min-width) min-width
	(slot-value sr 'max-width) max-width))

(defun-inline make-space-requirement (&rest args)
  (declare (dynamic-extent args))
  (apply #'make-instance 'space-requirement args))

(defconstant +fill+ (floor (/ (expt 10 (floor (log most-positive-fixnum 10))) 100)))


;;; Layout protocol

;(defgeneric compose-space (sheet)
;  )
;
;(defmethod compose-space (sheet)
;  (multiple-value-bind (width height)
;      (bounding-rectangle-size sheet)
;    (make-instance 'space-requirement
;		   :width width
;		   :height height)))
;
;(defgeneric allocate-space (sheet width height))
;
;(defmethod allocate-space (sheet width height)
;  (declare (ignore sheet width height)))

(defun parse-box-contents (contents)
  (mapcar #'(lambda (x)
	      ;; Handle top-down layout syntax
	      (if (and (consp x)
		       (typep (first x) '(or (member :fill) number)))
		  `(list ,(first x) ,(second x))
		  x))
	  contents))

(defmacro vertically ((&rest options &key spacing &allow-other-keys)
		      &body contents)
  (declare (ignore spacing))
  `(make-pane 'vbox-pane
     :contents (list ,@(parse-box-contents contents))
     ,@options))

(defmacro horizontally ((&rest options &key spacing &allow-other-keys)
			&body contents)
  (declare (ignore spacing))
  `(make-pane 'hbox-pane
     :contents (list ,@(parse-box-contents contents))
     ,@options))



(defmethod resize-sheet ((sheet sheet) width height)
  (unless (and (> width 0) (> height 0))
    (error "Trying to resize sheet ~S to be too small (~D x ~D)"
	   sheet width height))
  (when (or width height)
    (with-bounding-rectangle* (left top right bottom) sheet
      (let ((owidth (- right left))
	    (oheight (- bottom top)))
	(if (or (and width (/= owidth width))
		(and height (/= oheight height)))
	    ;; It should be safe to modify the sheet's region, since
	    ;; each sheet gets a fresh region when it is created
	    (let ((region (sheet-region sheet)))
	      (setf (slot-value region 'left) left
		    (slot-value region 'top)  top
		    (slot-value region 'right)  (if width (+ left width) right)
		    (slot-value region 'bottom) (if height (+ top height) bottom))
	      (note-sheet-region-changed sheet))
	    ;;--- Do this so that we relayout the rest of tree.
	    ;;--- I guess we do not want to do this always but ...
	    (allocate-space sheet owidth oheight))))))

(defmethod move-sheet ((sheet sheet) x y)
  (let ((transform (sheet-transformation sheet)))
    (multiple-value-bind (old-x old-y)
	(transform-position transform 0 0)
      (when (or (and x (/= old-x x))
		(and y (/= old-y y)))
	;; We would like to use volatile transformations here, but it's
	;; not really safe, since the current implementation of transformations
	;; is likely to cause them to be shared
	(setf (sheet-transformation sheet)
	      (compose-translation-with-transformation
		transform
		(if x (- x old-x) 0) (if y (- y old-y) 0)))))))

(defmethod move-and-resize-sheet ((sheet sheet) x y width height)
  (resize-sheet sheet width height)
  (move-sheet sheet x y))


;;; Various

;;--- What about PANE-FOREGROUND/BACKGROUND vs. MEDIUM-FOREGROUND/BACKGROUND?
;;--- Make a PANE protocol class, and call this BASIC-PANE
(defclass pane 
	  (sheet-transformation-mixin
	   standard-sheet-input-mixin
	   standard-repainting-mixin
	   permanent-medium-sheet-output-mixin
	   sheet)
    ((frame :reader pane-frame :initarg :frame)
     (framem :reader frame-manager :initarg :frame-manager)
     (name :accessor pane-name :initform nil :initarg :name)))

;;;--- This should be elsewhere
(defmethod frame-manager ((stream standard-encapsulating-stream))
  (frame-manager (slot-value stream 'stream)))

(defmethod frame-manager ((stream t))
  (cond (*application-frame* (frame-manager *application-frame*))
	(t (find-frame-manager))))

(defmethod print-object ((p pane) stream)
  (print-unreadable-object (p stream :type t :identity t)
    (when (slot-boundp p 'name) (format stream "~S" (slot-value p 'name)))))

(defmethod panep ((x pane)) t)
(defmethod panep ((x t)) nil)

(defmethod repaint-sheet ((pane pane) region)
  (declare (ignore region))
  nil)

;;--- This is suspicious - it should either be on a composite-pane or
;;--- on a top-level sheet
#+++ignore
(defmethod note-sheet-region-changed :after ((sheet pane) &key port)
  (declare (ignore port))
  (multiple-value-bind (width height) (bounding-rectangle-size sheet)
    (allocate-space sheet width height)))

(defmethod pane-frame ((x sheet)) nil)

(defclass pane-background-mixin () 
    ((background :initform +white+ :accessor pane-background)))

;;--- This is conceptually what we need, but it hasn't been tested.
;;--- Look carefully at who uses PANE-BACKGROUND-MIXIN first...
#+++ignore
(defmethod repaint-sheet ((pane pane-background-mixin) region)
  (with-sheet-medium (medium pane)
    (multiple-value-call #'draw-rectangle*
      medium (bounding-rectangle* (sheet-region pane))
      :ink (pane-background pane))))

;(defclass list-contents-mixin ()
;    ((contents :initform nil)
;     (nslots :initform nil :initarg :nslots)
;     ;; Reverse-p is provided because it makes sense to let users specify
;     ;; vertical lists from top to bottom, but internally it is easier to deal
;     ;; with bottom to top, because of the lower left coordinate system.
;     ;; For example, a user would like to list the vbox contents from top to
;     ;; bottom, while y coords increases from bottom to top. Whereas with
;     ;; a hbox, user specifys left to right just as x coord increases left
;     ;; to right.
;     ;; 
;     ;; A problem with reverse-p solution is that when new panes are inserted, 
;     ;; all panes before them in the user's view of contents changes coord,
;     ;; but the user may expect the one's later in the list to change coords.
;     ;; For example in a vertical list, adding a pane to the end will
;     ;; reposition all of the list items.
;     
;     ;; A possible solution is to user a upper left coordinate
;     ;; system so that added items have absolute y values that are increasing.
;     ;; Once upon a time Doug told me he prefered this, but it'll have to wait
;     ;; for now.   A deeper problem with this solution is that different panes
;     ;; would have different coordinate systems which maybe confusing if the
;     ;; coordinate system is exposed to the client in anyway.
;     (reverse-p :initform nil :initarg :reverse-p)))
;
;;;;
;;;; Insertion/Deletion Protocols
;;;;
;
;(defmethod contents ((lcm list-contents-mixin))
;  (with-slots (reverse-p contents) lcm
;    (if reverse-p (reverse contents)
;	contents)))
;    
;(defmethod (setf contents) (new-contents (lcm list-contents-mixin))
;  (with-slots (reverse-p contents children) lcm
;    (dolist (pane contents)
;      (sheet-disown-child lcm pane))
;    (dolist (pane new-contents)
;      (sheet-adopt-child lcm pane))
;    (setf contents (if reverse-p (reverse new-contents)
;		       new-contents)
;	  ;; So that things repaint from top to bottom
;	  ;; ??? However, it only work is you go through this method.
;	  children (if reverse-p (nreverse children)
;			  children))
;    
;    (note-space-requirements-changed (sheet-parent lcm) lcm)))
;
;
;
;(defun check-position (position reverse-p contents)
;  (let ((len (length contents)))
;    (when reverse-p 
;      (setq position (if position (- len position) 0)))
;    (unless position (setq position len))
;    (check-type position number)
;    (assert (and (<= 0 position) (<= position len))
;	    (position)
;	    "Position argument out of bounds")
;    position))
;
;(defmethod insert-panes ((lcm list-contents-mixin) panes
;			 &key position &allow-other-keys)
;  (with-slots (reverse-p contents) lcm
;    (setq position (check-position position reverse-p contents))
;    (dolist (pane panes)
;      (insert-pane lcm pane :position position :batch-p t)
;      (unless reverse-p (incf position)))
;    (note-space-requirements-changed (sheet-parent lcm) lcm)))
;
;(defmethod insert-pane ((lcm list-contents-mixin) (pane pane) 
;			&key position batch-p &allow-other-keys)
;  (with-slots (contents reverse-p) lcm
;    (unless batch-p
;      (setq position (check-position position reverse-p contents)))
;    (if (zerop position)
;	(push pane contents)
;	(let ((tail (nthcdr (1- position) contents)))
;	  (setf (cdr tail) 
;		(cons pane (cdr tail)))))
;    (sheet-adopt-child lcm pane)
;    (unless batch-p
;      (note-space-requirements-changed (sheet-parent lcm) lcm))))
;			      
;(defmethod remove-panes ((lcm list-contents-mixin) panes)
;  (dolist (pane panes)
;    (with-slots (contents) lcm
;      (setf contents (delete pane contents))
;      (sheet-disown-child lcm pane)))
;  (note-space-requirements-changed (sheet-parent lcm) lcm))
;
;(defmethod remove-pane ((lcm list-contents-mixin) (pane pane)
;			&key batch-p &allow-other-keys)
;  (with-slots (contents) lcm
;    (setf contents (delete pane contents :test #'eq))
;    (sheet-disown-child lcm pane)
;    (unless batch-p (note-space-requirements-changed (sheet-parent lcm) lcm))))
;
;(defmethod note-space-requirements-changed :before (parent (lcm list-contents-mixin))
;  #-PCL ;; PCL uses this variable for method-table cache misses, unfortunately.
;  (declare (ignore parent))
;  (with-slots (nslots contents) lcm
;    (setf nslots (length contents))))



;;--- CLIM 0.9 has some other methods on top-level sheets -- do we want them?
(defclass top-level-sheet 
	  (;;--- Temporary kludge until we get the protocols correct
	   ;;--- so that ACCEPT works correctly on a raw sheet
	   clim-internals::window-stream
	   wrapping-space-mixin
	   sheet-multiple-child-mixin
	   mirrored-sheet-mixin
	   pane)
    ()
    ;;--- More of same...
    (:default-initargs :text-cursor nil :text-margin 10))

;;--- Needed methods include:
;;---   INVOKE-WITH-RECORDING-OPTIONS
;;---   DEFAULT-TEXT-MARGIN
;;---   STREAM-READ-GESTURE

(defmethod note-layout-mixin-region-changed ((pane top-level-sheet) &key port)
  (if port
      ;; We do this because if the WM has resized us then we want to
      ;; do the complete LAYOUT-FRAME thing including clearing caches
      ;; etc etc.
      (multiple-value-call #'layout-frame
	(pane-frame pane) 
	(bounding-rectangle-size pane))
      ;; Otherwise just call ALLOCATE-SPACE etc
      (call-next-method)))

#+++ignore
(defmethod allocate-space ((sheet top-level-sheet) width height)
  (resize-sheet (first (sheet-children sheet)) width height))

#+++ignore
(defmethod compose-space ((sheet top-level-sheet) &key width height)
  (compose-space (first (sheet-children sheet)) :width width :height height))


(defclass composite-pane
	  (sheet-multiple-child-mixin 
	   pane)
    ())

(defclass leaf-pane 
	  (sheet-permanently-enabled-mixin
	   client-overridability-mixin
	   pane)
    ((cursor :initarg :cursor :initform nil
	     :accessor sheet-cursor)))

