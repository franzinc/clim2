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
;;;
;;; Copyright (c) 1989, 1990 by Xerox Corporation.  All rights reserved. 
;;;
;; $fiHeader: db-layout.lisp,v 1.11 92/03/30 17:52:01 cer Exp $

(in-package :silica)


;;;
;;; Layout Protocol
;;;

(defgeneric compose-space (pane &key width height)
  (:documentation "<Pane> should calculate how much space it wants."))

(defgeneric allocate-space (pane width height)
  (:documentation "<Pane> should allocate given space amongst itself and children"))

(defgeneric note-space-requirement-changed (composite child)
  (:documentation "Tells the composite that the child's shape has changed"))

;;;
;;; Default Methods for Layout Protocol
;;;

(defmethod allocate-space ((pane leaf-mixin) width height)
  (declare (ignore width height)))

(warn "boug")

(defmethod allocate-space (pane width height)
  (declare (ignore width height)))

(defmethod note-space-requirement-changed ((composite composite-pane) child)
  (declare (ignore child))
  (note-space-requirement-changed (sheet-parent composite) composite))

(defmethod note-space-requirement-changed ((composite null) child)
  ;; ??? Why is this method necessary -- RR
  ;; I'm sure it is, I just don't like the looks of it, which probably means
  ;; that there's a deeper problem somewhere.
  (declare (ignore child)))


;;;
;;; Layout Mixin
;;;

(defclass layout-mixin () ())

(defmethod change-space-requirement ((pane layout-mixin) &key &allow-other-keys)
  nil)

(defmethod change-space-requirement :after
	   ((pane layout-mixin) &key resize-frame &allow-other-keys)
  (if resize-frame
      (layout-frame (pane-frame pane))
      (note-space-requirement-changed (sheet-parent pane) pane)))

(defmethod note-space-requirement-changed (parent child)
  nil)

(defmethod note-sheet-region-changed :after ((pane layout-mixin) &key port-did-it)
  (note-layout-mixin-region-changed pane :port port-did-it))

(defmethod note-layout-mixin-region-changed ((pane layout-mixin) &key port)  
  (declare (ignore port))
  (multiple-value-bind (width height) (bounding-rectangle-size pane)
    (allocate-space pane width height)))


#||

(defmacro changing-space-requirements ((pane &key resize-frame) &body body)
 `(progn
    ,@body {translate "change-space-req" to %set-space-req}
    (if resize-frame
        (layout-frame frame)
        (note-space-requirement-changed (sheet-parent pane) pane))))

||#

(defun map-space-requirement (fn req)
  (setf (space-requirement-width req) (funcall fn (space-requirement-width req))
	(space-requirement-height req) (funcall fn (space-requirement-height req))
	(space-requirement-max-width req) (funcall fn (space-requirement-max-width req))
	(space-requirement-min-width req) (funcall fn (space-requirement-min-width req))
	(space-requirement-max-height req) (funcall fn (space-requirement-max-height req))
	(space-requirement-min-height req) (funcall fn (space-requirement-min-height req)))
  req)

(defun space-requirement+* (req &key (width 0) 
				     (max-width width)
				     (min-width width)
				     (height 0)
				     (max-height height)
				     (min-height height))
  (make-space-requirement :width (+ (space-requirement-width req) width)
			  :height (+ (space-requirement-height req) height)
			  :max-width (+ (space-requirement-max-width req) max-width)
			  :min-width (+ (space-requirement-min-width req) min-width)
			  :max-height (+ (space-requirement-max-height req) max-height)
			  :min-height (+ (space-requirement-min-height req) min-height)))

(defun space-requirement-combine (fn sr1 sr2)
  (make-space-requirement :width (funcall fn (space-requirement-width sr1)
					     (space-requirement-width sr2))
			  :height (funcall fn (space-requirement-height sr1)
					      (space-requirement-height sr2))
			  :max-width (funcall fn (space-requirement-max-width sr1)
						 (space-requirement-max-width sr2))
			  :min-width (funcall fn (space-requirement-min-width sr1)
						 (space-requirement-min-width sr2))
			  :max-height (funcall fn (space-requirement-max-height sr1)
						  (space-requirement-max-height sr2))
			  :min-height (funcall fn (space-requirement-min-height sr1)
						  (space-requirement-min-height sr2))))

(defun space-requirement+ (sr1 sr2)
  (space-requirement-combine #'+ sr1 sr2))

(defun %set-space-requirement (sr &key width height max-width 
				       max-height min-width min-height &allow-other-keys)
  (when width (setf (space-requirement-width sr) width))
  (when max-width (setf (space-requirement-max-width sr) max-width))
  (when min-width (setf (space-requirement-min-width sr) min-width))
  (when height (setf (space-requirement-height sr) height))
  (when max-height (setf (space-requirement-max-height sr) max-height))
  (when min-height (setf (space-requirement-min-height sr) min-height))
  sr)


;;;
;;;
;;;


;;;
;;; Space Req Mixin
;;; 

(defclass space-requirement-mixin (layout-mixin)
    ((initial-space-requirement :reader pane-initial-space-requirement)
     (space-requirement :reader pane-space-requirement)))

(defmethod initialize-instance :after ((pane space-requirement-mixin) 
				       &rest args
				       &key width max-width min-width
					    height max-height min-height)
  (declare (dynamic-extent args))
  (declare (ignore width min-width max-width
		   height min-height max-height))
  (multiple-value-bind (width min-width max-width
			height min-height max-height)
      (apply #'default-space-requirements pane :allow-other-keys t args)
    (setf (slot-value pane 'space-requirement)
	  (make-space-requirement :width width 
				  :height height 
				  :max-width max-width 
				  :min-width min-width 
				  :max-height max-height
				  :min-height min-height)
	  (slot-value pane 'initial-space-requirement)
	  (make-space-requirement :width width 
				  :height height 
				  :max-width max-width 
				  :min-width min-width 
				  :max-height max-height
				  :min-height min-height))))

(defmethod default-space-requirements ((pane space-requirement-mixin) 
				       &key (width 0)
					    (min-width width)
					    (max-width width)
					    (height 0)
					    (min-height height)
					    (max-height height))
  (values width
	  min-width
	  max-width
	  height
	  min-height
	  max-height))
	  
	  

(defmethod compose-space ((pane space-requirement-mixin) &key width height)
  (or (slot-value pane 'space-requirement)
      (slot-value pane 'initial-space-requirement)))

(defmethod change-space-requirement ((pane space-requirement-mixin) &rest keys)
  (declare (dynamic-extent keys))
  (with-slots (space-requirement) pane
    (apply #'%set-space-requirement space-requirement keys)))



;;;
;;;  Client Space-req Mixin (read Client Space-req)
;;;

(defclass basic-client-space-requirement-mixin (layout-mixin)
    ((client-space-requirement :initform nil))
  (:documentation "User can specify a value for the client-space-requirement
slot that defaults to NIL"))

;;; BUG: what do we do if the value is NIL!

(defmethod change-space-requirement ((pane basic-client-space-requirement-mixin) &rest keys)
  (declare (dynamic-extent keys))
  (with-slots (client-space-requirement) pane
    (apply #'%set-space-requirement client-space-requirement keys)))

(defmethod initialize-instance :after ((pane basic-client-space-requirement-mixin) 
				       &rest args
				       &key width max-width min-width 
					    height max-height min-height)
  (declare (dynamic-extent args))
  (when (or width max-width min-width height max-height min-height)
    (multiple-value-bind (width min-width max-width
			  height min-height max-height)
	(apply #'default-space-requirements pane :allow-other-keys t args)
      (setf (slot-value pane 'client-space-requirement)
	    (make-space-requirement :width width :height height 
				    :max-width max-width
				    :min-width min-width
				    :max-height max-height
				    :min-height min-height)))))

(warn "duplicate?")

(defmethod default-space-requirements ((pane basic-client-space-requirement-mixin) 
				       &key (width 0)
					    (min-width width)
					    (max-width width)
					    (height 0)
					    (min-height height)
					    (max-height height))
  (values width min-width max-width
	  height min-height max-height))

;;;;

(defclass client-space-requirement-mixin (basic-client-space-requirement-mixin)
    ()
  (:documentation "If the user specifies some space requirements then
use it otherwise use other value"))

(defmethod compose-space :around ((pane client-space-requirement-mixin) &key width height)
  (or (slot-value pane 'client-space-requirement)
      (call-next-method)))



;;;
;;; Client space space-requirement  (read Client Space Space-requirement)
;;; Just like the above but used by panes that are using the client-specified
;;; space-requirement to determine how much free space is requested (spacer, filler).
;;;

;(defclass client-space-space-requirement-mixin (client-space-requirement-mixin)
;    ())
;
;(defmethod initialize-instance :around
;	   ((pane client-space-space-requirement-mixin) &rest rest 
;	    &key space)
;  (if space
;      (apply #'call-next-method pane :width space :height space rest)
;      (call-next-method)))
;;;
;;;  Client Stretchability
;;;

;(defclass client-stretchability (layout-mixin)
;    ((max-width :initform nil :initarg :max-width)
;     (max-height :initform nil :initarg :max-height)))
;
;(defmethod compose-space :around ((pane client-stretchability) &key width height)
;  (with-slots (max-width max-height) pane
;    (let ((value (call-next-method)))
;      (when max-width (setf (space-requirement-max-width value) max-width))
;      (when max-height (setf (space-requirement-max-height value) max-height))
;      value)))

;;;
;;; Client Overridability
;;;

(defclass client-overridability (layout-mixin)
    ((width :initform nil :initarg :width)
     (height :initform nil :initarg :height)
     (max-width :initform nil :initarg :max-width)
     (max-height :initform nil :initarg :max-height)
     (min-width :initform nil :initarg :min-width)
     (min-height :initform nil :initarg :min-height))
  (:documentation "Client can specify values that override those
provided elsewhere"))

(defmethod compose-space :around ((pane client-overridability) &key width height)
  (with-slots (width height max-width max-height min-width min-height) pane
    (let ((value (call-next-method)))
      (when (or width height max-width max-height min-width
		min-height)
	(setq value (copy-space-requirement value)))
      (when width  (setf (space-requirement-width value) width))
      (when height  (setf (space-requirement-height value) height))
      (when max-width (setf (space-requirement-max-width value) max-width))
      (when max-height (setf (space-requirement-max-height value) max-height))
      (when min-width (setf (space-requirement-min-width value) min-width))
      (when min-height (setf (space-requirement-min-height value) min-height))
      value)))


;;;
;;; Client Demandability
;;;
;;; Used by db-scroll
;(defclass client-demandability (client-space-requirement-mixin
;				space-requirement-cache-mixin)
;    ((space-demand-p :initform t :initarg :space-demand-p)))
;  
;(defmethod compose-space ((pane client-demandability) &key width height)
;  (with-slots (client-space-requirement space-demand-p) pane
;    (if space-demand-p 
;	(or client-space-requirement (compose-space (sheet-child pane)))
;      (let ((child-req (compose-space (sheet-child pane))))
;	(if client-space-requirement 
;	    (make-space-requirement :width (min (space-requirement-width client-space-req)
;					        (space-requirement-width child-req))
;			            :max-width (max (space-requirement-max-width client-space-req)
;					            (space-requirement-max-width child-req))
;			            :height  (min (space-requirement-height client-space-req)
;					          (space-requirement-height child-req))
;			            :max-height (max (space-requirement-max-height client-space-req)
;					             (space-requirement-max-height child-req)))
;	  child-req)))))

;;;
;;; Space Req Cache
;;;

(defclass space-requirement-cache-mixin (layout-mixin)
    ((space-requirement :initform nil)))

(defmethod compose-space :around ((pane space-requirement-cache-mixin) &key width height)
  (with-slots (space-requirement) pane
    (or space-requirement (setf space-requirement (call-next-method)))))

(defmethod clear-space-requirement-cache ((pane layout-mixin))
  nil)

(defmethod clear-space-requirement-cache ((pane t))
  nil)

(defmethod clear-space-requirement-cache ((pane space-requirement-cache-mixin))
  (with-slots (space-requirement) pane
    (setf space-requirement nil)))

(defmethod note-space-requirement-changed :before
	   (composite (pane space-requirement-cache-mixin))
  (declare (ignore composite))
  (clear-space-requirement-cache pane))

(defun clear-space-requirement-caches-in-tree (sheet)
  (map-over-sheets #'(lambda (sheet) 
		       (clear-space-requirement-cache sheet))
		   sheet))

(defun clear-space-requirement-caching-in-ancestors (menu)
  (do ((parent (sheet-parent menu) (sheet-parent parent)))
      ((null parent))
    (clear-space-requirement-cache parent)))

;;;
;;; Wrapping Space Mixin
;;;

(defclass wrapping-space-mixin (layout-mixin)
    ()
  (:documentation "Supports echoing allocations down and compositions up."))
	 
(defmethod allocate-space ((pane wrapping-space-mixin) width height)
  (resize-sheet* (sheet-child pane) width height))

(defmethod compose-space ((pane wrapping-space-mixin) &key width height)
  (let ((child (sheet-child pane)))
    (compose-space child :width width :height height)))


;;;
;;; Transforming Wrapping Space Mixin
;;;

;(defclass simple-transforming-wrapping-space-mixin (space-requirement-cache-mixin)
;    ()
;  (:documentation "Supports echoing allocations down and compositions up.~%~
;                   Transforming the echos for translations and reflection."))
;	 
;(defmethod allocate-space ((pane simple-transforming-wrapping-space-mixin)
;			   width height)
;  (let ((child (sheet-child pane)))
;    (multiple-value-bind (width height) 
;	(untransform-dimensions (sheet-transformation child) width height)
;      (resize-sheet* child width height))))
;  
;(defmethod compose-space ((pane simple-transforming-wrapping-space-mixin) &key width height)
;  (let* ((child (sheet-child pane))
;	 (xform (sheet-transformation child))
;	 (req   (compose-space child))
;	 (width (space-requirement-width req))
;	 (height (space-requirement-height req))
;	 (max-width (space-requirement-max-width req))
;	 (max-height (space-requirement-max-height req))
;	 (min-width (space-requirement-min-width req))
;	 (min-height (space-requirement-min-height req)))
;    
;    (multiple-value-setq (width height) (untransform-dimensions xform width height))
;    (multiple-value-setq (max-width max-height) (untransform-dimensions xform max-width max-height))
;    (multiple-value-setq (min-width min-height) (untransform-dimensions xform min-width min-height))
;    
;    (make-space-requirement :width width :max-width max-width :min-width min-width
;		             :height height :max-height max-height :min-height min-height)))


;;;
;;; Restrainer-pane
;;;
;;; Use this wrapping composite to not allow note-space-requirement-changeds to propagate up.

(defclass restrainer-pane (composite-pane 
			   simple-wrapper-mixin
			   wrapping-space-mixin
			   mute-input-mixin)
    ())

(defmethod note-space-requirement-changed ((pane restrainer-pane) inner)
  (declare (ignore inner))
  (allocate-space pane 
		  (bounding-rectangle-width pane) (bounding-rectangle-height pane)))



;;; Generally useful layout function
;;; Used all over to satisfy constraints

(defun allocate-space-to-items (given wanted items min-size
				desired-size max-size item-size)
  (flet ((desired-size (x) (funcall desired-size x))
	 (min-size (x) (funcall min-size x))
	 (max-size (x) (funcall max-size x))
	 (item-size (x) (funcall item-size x)))
    (let ((stretch-p (>= given (desired-size wanted)))
	  extra give used sizes)
      (if stretch-p
	  (setq give (- (max-size wanted) (desired-size wanted))
		extra (min (- given (desired-size wanted)) give))
	  (setq give (- (desired-size wanted) (min-size wanted))
		extra (min (- (desired-size wanted) given) give)))
      (dolist (item items)
	(let* ((item-size (item-size item))
	       (alloc (desired-size item-size)))
	  (when (> give 0)
	    (if stretch-p
		(progn
		  (setq used (/ (* (- (max-size item-size)
				      (desired-size item-size))
				   extra)
				give))
		  (incf alloc used)
		  (decf give (- (max-size item-size)
				(desired-size item-size))))
		(progn
		  (setq used (/ (* (- (desired-size item-size)
				      (min-size item-size))
				   extra)
				give))
		  (decf alloc used)
		  (decf give (- (desired-size item-size)
				(min-size item-size)))))
	    (decf extra used))
	  (push alloc sizes)))
      (nreverse sizes))))
	
;;; Most of the layout panes should inherit from this

(defclass layout-pane (mute-input-mixin
		       pane-background-mixin
		       composite-pane 
		       space-requirement-cache-mixin
		       sheet-permanently-enabled-mixin)
    ())


;;--- Does this need to be more complicated?
;;--- What is the correct behavior.  Should it take a space requirement?
;;--- and just call ALLOCATE-SPACE?
(defclass bboard-pane (space-requirement-mixin layout-pane) ())

(defmethod allocate-space ((pane bboard-pane) width height)
  (declare (ignore width height))
  (dolist (child (sheet-children pane))
    (let ((space-req (compose-space child)))
      (resize-sheet* child 
		     (space-requirement-width space-req)
		     (space-requirement-height space-req)))))
