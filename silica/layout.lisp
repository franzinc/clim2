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
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: layout.lisp,v 1.9 92/03/24 19:36:44 cer Exp Locker: cer $

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

(defconstant +fill+ (/ (expt 10 (floor (log most-positive-fixnum 10))) 100))


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

(defmacro vertically (options &body contents)
  `(realize-pane 'vbox-pane
		 :contents (list ,@contents)
		 ,@options))


(defmacro horizontally (options &body contents)
  `(realize-pane 'hbox-pane
		 :contents (list ,@contents)
		 ,@options))


(defmethod resize-sheet* ((sheet sheet) width height &key force)
  (when (or width height)
    (with-bounding-rectangle* 
	(minx miny maxx maxy) sheet
	(when (or force
		  (and width (/= (- maxx minx) width))
		  (and height (/= (- maxy miny) height)))
	  (setf (sheet-region sheet)
	    (make-bounding-rectangle
	     minx miny
	     (if width (+ width minx) maxx)
	     (if height (+ height miny) maxy)))))))

(defmethod move-and-resize-sheet* ((sheet sheet) minx miny width height)
  (resize-sheet* sheet width height)
  (let ((trans (sheet-transformation sheet)))
    (multiple-value-bind (x y)
	(transform-point* trans 0 0)
      (when (or (and minx (/= x minx))
		(and miny (/= y miny)))
	(setf (sheet-transformation sheet)
	      (compose-translation-with-transformation
		trans
		(if minx (- minx x) 0)
		(if miny (- miny y) 0)))))))

(defmethod move-sheet* ((sheet sheet) minx miny)
  (let ((trans (sheet-transformation sheet)))
    (multiple-value-bind (x y)
	(transform-point* trans 0 0)
      (when (or (/= x minx)
		(/= y miny))
	(setf (sheet-transformation sheet)
	      (compose-translation-with-transformation
		trans
		(- minx x)
		(- miny y)))))))


;; Various

;;--- What about PANE-FOREGROUND/BACKGROUND vs. MEDIUM-FOREGROUND/BACKGROUND?
(defclass pane 
    (sheet 
     sheet-transformation-mixin
     standard-sheet-input-mixin
     standard-repainting-medium
     permanent-medium-sheet-output-mixin
     mute-repainting-mixin)
    ((frame :reader pane-frame :initarg :frame)
     (framem :reader pane-frame-manager :initarg :frame-manager)
     (name :initform nil :reader pane-name)))

(defmethod panep ((x pane)) t)
(defmethod panep ((x t)) nil)

;;--- This is suspicious - it should either be on a composite-pane or
;;--- on a top-level sheet
#+++ignore
(defmethod note-sheet-region-changed :after ((sheet pane) &key port)
  (declare (ignore port))
  (multiple-value-bind (width height) (bounding-rectangle-size sheet)
    (allocate-space sheet width height)))

(defmethod pane-frame ((x sheet)) nil)

(defclass leaf-mixin (sheet-leaf-mixin) ())
(defclass composite-pane (pane sheet-multiple-child-mixin) ())
(defclass mute-input-mixin (sheet-mute-input-mixin) ())

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
;    (note-space-requirement-changed (sheet-parent lcm) lcm)))
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
;    (note-space-requirement-changed (sheet-parent lcm) lcm)))
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
;      (note-space-requirement-changed (sheet-parent lcm) lcm))))
;			      
;(defmethod remove-panes ((lcm list-contents-mixin) panes)
;  (dolist (pane panes)
;    (with-slots (contents) lcm
;      (setf contents (delete pane contents))
;      (sheet-disown-child lcm pane)))
;  (note-space-requirement-changed (sheet-parent lcm) lcm))
;
;(defmethod remove-pane ((lcm list-contents-mixin) (pane pane)
;			&key batch-p &allow-other-keys)
;  (with-slots (contents) lcm
;    (setf contents (delete pane contents :test #'eq))
;    (sheet-disown-child lcm pane)
;    (unless batch-p (note-space-requirement-changed (sheet-parent lcm) lcm))))
;
;(defmethod note-space-requirement-changed :before (parent (lcm list-contents-mixin))
;  #-PCL ;; PCL uses this variable for method-table cache misses, unfortunately.
;  (declare (ignore parent))
;  (with-slots (nslots contents) lcm
;    (setf nslots (length contents))))



;;--- CLIM 0.9 has some other methods on top-level sheets -- do we want them?

(defclass top-level-sheet 
    (
     ;; Have these so that we can use 
     clim-internals::window-stream
     ;;
     pane
     wrapping-space-mixin
     mirrored-sheet-mixin
     sheet-multiple-child-mixin)
    ()
  (:default-initargs :text-cursor nil :text-margin 10))


;; invoke-with-recording-options
;; default-text-margin
;; stream-read-gesture


(defmethod note-layout-mixin-region-changed ((pane top-level-sheet) &key port)
  (if port
      (multiple-value-call #'layout-frame
	(pane-frame pane) 
	(bounding-rectangle-size pane))
    (call-next-method)))

;(defmethod allocate-space ((sheet top-level-sheet) width height)
;  ;;--- To make openlook windows fill the vertical dimension we need
;  ;;--- to do this why???. Actually doing this seems to make a lot of sense
;  ;;--- Perhaps what should happen is that if the sheet-region is
;  ;;--- changed by the port we should layout the entire frame instead
;  (clear-space-requirement-caches-in-tree (first (sheet-children sheet)))
;  ;;
;  (resize-sheet* (first (sheet-children sheet)) width height))
;
;(defmethod compose-space ((sheet top-level-sheet) &key width height)
;  (compose-space (first (sheet-children sheet)) :width width :height height))


(defclass leaf-pane 
	  (sheet-permanently-enabled-mixin
	   client-overridability
	   pane)
    ((cursor :initarg :cursor :initform nil
	     :accessor sheet-cursor)))

