;;; -*- Mode: Lisp; Package: silica; Base: 10.; Syntax: Common-Lisp -*-
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

(in-package :silica)

;;;
;;; Layout Protocol
;;;

(defgeneric compose-space (pane)
  (:documentation
    "<Pane> should calculate how much space it wants."))

(defgeneric allocate-space (pane width height)
  (:documentation
    "<Pane> should allocate given space amongst itself and children"))

(defgeneric space-req-changed (composite child)
  (:documentation
    "Tells the composite that the child's shape has changed"))

;;;
;;; Default Methods for Layout Protocol
;;;

(defmethod allocate-space ((pane leaf-mixin) width height)
  (declare (ignore width height)))

(warn "boug")

(defmethod allocate-space (pane width height)
  (declare (ignore width height)))

(defmethod space-req-changed ((composite composite-pane) child)
  (declare (ignore child))
  (space-req-changed (sheet-parent composite) composite))

(defmethod space-req-changed ((composite null) child)
  ;; ??? Why is this method necessary -- RR
  ;; I'm sure it is, I just don't like the looks of it, which probably means
  ;; that there's a deeper problem somewhere.
  (declare (ignore child)))


;;;
;;; Layout Mixin
;;;

(defclass layout-mixin ()
    ())



(defmethod change-space-req ((pane layout-mixin) &key &allow-other-keys)
  nil)

(defmethod change-space-req :after
	   ((pane layout-mixin) &key resize-frame &allow-other-keys)
  (if resize-frame
      (relayout-frame (pane-frame pane))
    (space-req-changed (sheet-parent pane) pane)))

(defmethod space-req-changed (parent child)
  nil)

#||

(defmacro changing-space-reqs ((pane &key resize-frame) &body body)
 `(progn
    ,@body {translate "change-space-req" to %set-space-req}
    (if resize-frame
        (layout-frame frame)
        (space-req-changed (sheet-parent pane) pane))))

||#

(defun map-space-req (fn req)
  (setf (space-req-width req) (funcall fn (space-req-width req))
	(space-req-height req) (funcall fn (space-req-height req))
	(space-req-max-width req) (funcall fn (space-req-max-width req))
	(space-req-min-width req) (funcall fn (space-req-min-width req))
	(space-req-max-height req) (funcall fn (space-req-max-height req))
	(space-req-min-height req) (funcall fn (space-req-min-height req)))
  req)

(defun space-req+* (req &key (width 0) 
			     (max-width width)
			     (min-width width)
			     (height 0)
			     (max-height height)
			     (min-height height))
  (make-space-req :width (+ (space-req-width req) width)
		  :height (+ (space-req-height req) height)
		  :max-width (+ (space-req-max-width req) max-width)
		  :min-width (+ (space-req-min-width req) min-width)
		  :max-height (+ (space-req-max-height req) max-height)
		  :min-height (+ (space-req-min-height req) min-height)))

(defun space-req+ (sr1 sr2)
  (make-space-req :width (+ (space-req-width sr1) (space-req-width sr2))
		  :height (+ (space-req-height sr1) (space-req-height sr2))
		  :max-width (+ (space-req-max-width sr1) (space-req-max-width sr2))
		  :min-width (+ (space-req-min-width sr1) (space-req-min-width sr2))
		  :max-height (+ (space-req-max-height sr1) (space-req-max-height sr2))
		  :min-height (+ (space-req-min-height sr1) (space-req-min-height sr2))))


(defun %set-space-req (sr &key width height max-width 
			       max-height min-width min-height &allow-other-keys)
  (when width (setf (space-req-width sr) width))
  (when max-width (setf (space-req-max-width sr) max-width))
  (when min-width (setf (space-req-min-width sr) min-width))
  (when height (setf (space-req-height sr) height))
  (when max-height (setf (space-req-max-height sr) max-height))
  (when min-height (setf (space-req-min-height sr) min-height))
  sr)


;;;
;;;
;;;


;;;
;;; Space Req Mixin
;;; 

(defclass space-req-mixin (layout-mixin)
    ((space-req :reader pane-space-req)))

(defmethod initialize-instance :after 
	   ((pane space-req-mixin) 
	    &key  
	    (width 0) (max-width width) (min-width width)
	    (height 0) (max-height height) (min-height height))
  (setf (slot-value pane 'space-req)
    (make-space-req :width width 
		    :height height 
		    :max-width max-width 
		    :min-width min-width 
		    :max-height max-height
		    :min-height min-height)))

(defmethod compose-space ((pane space-req-mixin))
  (slot-value pane 'space-req))

(defmethod change-space-req ((pane space-req-mixin) &rest keys)
  (declare (dynamic-extent keys))
  (with-slots (space-req) pane
    (apply #'%set-space-req space-req keys)))



;;;
;;;  Client Space-req Mixin (read Client Space-req)
;;;

(defclass client-space-req-mixin (layout-mixin)
    ((client-space-req :initform nil)))

(defmethod initialize-instance :after 
	   ((pane client-space-req-mixin) 
	    &key width max-width min-width height max-height min-height &allow-other-keys)
  (when (or width max-width min-width height max-height min-height)
    (setf (slot-value pane 'client-space-req)
      (make-space-req :width (or width 0) :height (or height 0) 
		      :max-width (or max-width width +fill+)
		      :min-width (or min-width width 0)
		      :max-height (or max-height height +fill+)
		      :min-height (or min-height height 0)))))

(defmethod compose-space ((pane client-space-req-mixin))
  (or (slot-value pane 'client-space-req)
      (call-next-method)))

(defmethod change-space-req ((pane client-space-req-mixin) &rest keys)
  (declare (dynamic-extent keys))
  (with-slots (client-space-req) pane
    (apply #'%set-space-req client-space-req keys)))

;;;
;;; Client space space-req  (read Client Space Space-req)
;;; Just like the above but used by panes that are using the client-specified
;;; space-req to determine how much free space is requested (spacer, filler).
;;;

(defclass client-space-space-req-mixin (client-space-req-mixin)
    ())

(defmethod initialize-instance :around
	   ((pane client-space-space-req-mixin) &rest rest 
	    &key space)
  (if space
      (apply #'call-next-method pane :width space :height space rest)
      (call-next-method)))


;;;
;;;  Client Stretchability
;;;

(defclass client-stretchability (layout-mixin)
    ((max-width :initform nil :initarg :max-width)
     (max-height :initform nil :initarg :max-height)))

(defmethod compose-space :around ((pane client-stretchability))
  (with-slots (max-width max-height) pane
    (let ((value (call-next-method)))
      (when max-width (setf (space-req-max-width value) max-width))
      (when max-height (setf (space-req-max-height value) max-height))
      value)))

;;;
;;; Client Overridability
;;;

(defclass client-overridability (layout-mixin)
    ((width :initform nil :initarg :width)
     (height :initform nil :initarg :height)
     (max-width :initform nil :initarg :max-width)
     (max-height :initform nil :initarg :max-height)
     (min-width :initform nil :initarg :min-width)
     (min-height :initform nil :initarg :min-height)))

(defmethod compose-space :around ((pane client-overridability))
  (with-slots (width height max-width max-height min-width min-height) pane
    (let ((value (call-next-method)))
      (when width  (setf (space-req-width value) width))
      (when height  (setf (space-req-height value) height))
      (when max-width (setf (space-req-max-width value) max-width))
      (when max-height (setf (space-req-max-height value) max-height))
      (when min-width (setf (space-req-min-width value) min-width))
      (when min-height (setf (space-req-min-height value) min-height))
      value)))


;;;
;;; Client Demandability
;;;
;;; Used by db-scroll

(defclass client-demandability (client-space-req-mixin
				space-req-cache-mixin)
    ((space-demand-p :initform t :initarg :space-demand-p)))
  
(defmethod compose-space ((pane client-demandability))
  (with-slots (client-space-req space-demand-p) pane
    (if space-demand-p 
	(or client-space-req (compose-space (sheet-child pane)))
      (let ((child-req (compose-space (sheet-child pane))))
	(if client-space-req 
	    (make-space-req :width  (min (space-req-width client-space-req)
					 (space-req-width child-req))
			    :max-width (max (space-req-max-width client-space-req)
					    (space-req-max-width child-req))
			    :height  (min (space-req-height client-space-req)
					  (space-req-height child-req))
			    :max-height (max (space-req-max-height client-space-req)
					     (space-req-max-height child-req)))
	  child-req)))))

;;;
;;; Space Req Cache
;;;

(defclass space-req-cache-mixin (layout-mixin)
    ((space-req :initform nil)))

(defmethod compose-space :around ((pane space-req-cache-mixin))
  (with-slots (space-req) pane
    (or space-req (setf space-req (call-next-method)))))

(defmethod clear-space-req-cache ((pane layout-mixin))
  nil)

(defmethod clear-space-req-cache ((pane t))
  nil)

(defmethod clear-space-req-cache ((pane space-req-cache-mixin))
  (with-slots (space-req) pane
    (setf space-req nil)))

(defmethod space-req-changed :before (composite (pane space-req-cache-mixin))
  (declare (ignore composite))
  (clear-space-req-cache pane))

(defun clear-space-req-caches-in-tree (sheet)
  (walk-tree #'(lambda (sheet depth nth) 
		 (declare (ignore depth nth))
		 (clear-space-req-cache sheet))
	     sheet))

(defun clear-space-req-caching-in-ancestors (menu)
  (do ((parent (sheet-parent menu) (sheet-parent parent)))
      ((null parent))
    (clear-space-req-cache parent)))

;;;
;;; Wrapping Space Mixin
;;;

(defclass wrapping-space-mixin ()
    ()
  (:documentation "Supports echoing allocations down and compositions up."))
	 
(defmethod allocate-space ((pane wrapping-space-mixin) width height)
  (resize-sheet* (sheet-child pane) width height))

(defmethod compose-space ((pane wrapping-space-mixin))
  (let ((child (sheet-child pane)))
    (compose-space child)))


;;;
;;; Transforming Wrapping Space Mixin
;;;

(defclass simple-transforming-wrapping-space-mixin (space-req-cache-mixin)
    ()
  (:documentation "Supports echoing allocations down and compositions up.~%~
                   Transforming the echos for translations and reflection."))
	 
(defmethod allocate-space ((pane simple-transforming-wrapping-space-mixin)
			   width height)
  (let ((child (sheet-child pane)))
    (multiple-value-bind (width height) 
	(untransform-dimensions (sheet-transformation child) width height)
      (resize-sheet* child width height))))
  
(defmethod compose-space ((pane simple-transforming-wrapping-space-mixin))
  (let* ((child (sheet-child pane))
	 (xform (sheet-transformation child))
	 (req   (compose-space child))
	 (width (space-req-width req))
	 (height (space-req-height req))
	 (max-width (space-req-max-width req))
	 (max-height (space-req-max-height req))
	 (min-width (space-req-min-width req))
	 (min-height (space-req-min-height req)))
    
    (multiple-value-setq (width height) (untransform-dimensions xform width height))
    (multiple-value-setq (max-width max-height) (untransform-dimensions xform max-width max-height))
    (multiple-value-setq (min-width min-height) (untransform-dimensions xform min-width min-height))
    
    (make-space-req :width width :max-width max-width :min-width min-width
		    :height height :max-height max-height :min-height min-height)))


;;;
;;; Restrainer-pane
;;;
;;; Use this wrapping composite to not allow space-req-changeds to propagate up.

(defclass restrainer-pane (composite-pane 
				    simple-wrapper-mixin
				    wrapping-space-mixin
				    mute-input-mixin)
  ())

(defmethod space-req-changed ((pane restrainer-pane) inner)
  (declare (ignore inner))
  (allocate-space pane (bounding-rectangle-width pane) 
		  (bounding-rectangle-height pane)))



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
