;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: db-layout.lisp,v 1.24 92/09/08 10:34:18 cer Exp $

(in-package :silica)

"Copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
 Portions copyright (c) 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 Xerox Corp.  All rights reserved."


;;;
;;; Layout Protocol
;;;

(defgeneric compose-space (pane &key width height)
  (:documentation "<Pane> should calculate how much space it wants."))

(defgeneric allocate-space (pane width height)
  (:documentation "<Pane> should allocate given space amongst itself and children"))

(defgeneric note-space-requirements-changed (composite child)
  (:documentation "Tells the composite that the child's shape has changed"))

;;;
;;; Default Methods for Layout Protocol
;;;

;;--- This seems dubious...
(defmethod allocate-space ((pane basic-sheet) width height)
  (declare (ignore width height)))

(defmethod note-space-requirements-changed ((composite composite-pane) child)
  (declare (ignore child))
  (note-space-requirements-changed (sheet-parent composite) composite))

(defmethod note-space-requirements-changed ((composite null) child)
  ;; ??? Why is this method necessary -- RR
  ;; I'm sure it is, I just don't like the looks of it, which probably means
  ;; that there's a deeper problem somewhere.
  (declare (ignore child)))


;;;
;;; Layout Mixin
;;;

(defclass layout-mixin () ())

(defmethod change-space-requirements ((pane layout-mixin) &key &allow-other-keys)
  nil)

(defmethod note-sheet-region-changed :after ((pane layout-mixin) &key port-did-it)
  (note-layout-mixin-region-changed pane :port port-did-it))

(defmethod note-layout-mixin-region-changed ((pane layout-mixin) &key port)  
  (declare (ignore port))
  (multiple-value-bind (width height) (bounding-rectangle-size pane)
    (allocate-space pane width height)))


(defmacro changing-space-requirements ((&rest options &key resize-frame layout) 
				       &body body)
  (declare (ignore resize-frame layout))
  `(flet ((changing-space-requirements-body () ,@body))
     (declare (dynamic-extent #'changing-space-requirements-body))
     (invoke-with-changing-space-requirements 
       #'changing-space-requirements-body ,@options)))

(defmethod change-space-requirements-to ((pane layout-mixin) space-requirement)
  (multiple-value-bind (width min-width max-width
			height min-height max-height)
      (space-requirement-components space-requirement)
    (change-space-requirements pane
      :width width :min-width min-width :max-width max-width 
      :height height :min-height min-height :max-height max-height)))


;;--- Do we need this?
(defmethod change-space-requirements-to-default ((pane basic-pane))
  nil)

(defmethod change-space-requirements-to-default ((pane layout-mixin))
  nil)

(defvar *inside-changing-space-requirements* nil)

(defun invoke-with-changing-space-requirements (continuation &key resize-frame layout)
  (let ((old-inside-changing-space-requirements 
	  *inside-changing-space-requirements*)
	(*inside-changing-space-requirements* 
	  (cons nil *inside-changing-space-requirements*)))
    (multiple-value-prog1
      (funcall continuation)
      (unless (or (not layout) old-inside-changing-space-requirements)
	;; Layout that frames that need to be laid out and
	;; re-layout the minimal subtrees that need to be laid out
	(let* ((frames (remove-duplicates
			 (remove-if-not 
			   #'(lambda (x) (application-frame-p x))
			   *inside-changing-space-requirements*)))
	       (panes (remove-if #'(lambda (x)
				     (or (not (panep x))
					 (member (pane-frame x) frames)))
				 *inside-changing-space-requirements*)))
	  (mapc #'layout-frame frames)
	  (dolist (pane panes)
	    (unless (some #'(lambda (p) 
			      (and (not (eq p pane))
				   (sheet-ancestor-p pane p)))
			  panes)
	      ;; do this because the change has occurred somewhere in
	      ;; the tree
	      (clear-space-requirement-caches-in-tree pane) 
	      (multiple-value-call #'allocate-space 
	        pane (bounding-rectangle-size pane)))))))))

(defmethod note-space-requirement-changed (parent child)
  (multiple-value-call #'allocate-space 
    parent (bounding-rectangle-size parent)))

(defmethod change-space-requirements :around ((pane layout-mixin) 
					      &key resize-frame &allow-other-keys)
  (call-next-method)
  (if *inside-changing-space-requirements*
      (push (if resize-frame (pane-frame pane) (sheet-parent pane))
	    (cdr *inside-changing-space-requirements*))
      (if resize-frame
	  (layout-frame (pane-frame pane))
	  (note-space-requirement-changed (sheet-parent pane) pane))))


(defun space-requirement-combine (function sr1 sr2)
  (multiple-value-bind (width1 min-width1 max-width1 height1 min-height1 max-height1)
      (space-requirement-components sr1)
    (multiple-value-bind (width2 min-width2 max-width2 height2 min-height2 max-height2)
	(space-requirement-components sr2)
      (make-space-requirement
	:width (funcall function width1 width2)
	:min-width (funcall function min-width1 min-width2)
	:max-width (funcall function max-width1 max-width2)
	:height (funcall function height1 height2)
	:min-height (funcall function min-height1 min-height2)
	:max-height (funcall function max-height1 max-height2)))))

;; Add two space requirements
(defun-inline space-requirement+ (sr1 sr2)
  (space-requirement-combine #'+ sr1 sr2))

;; The "spread" version of the above...
(defun space-requirement+* (sr &key (width 0) (max-width width) (min-width width)
				    (height 0) (max-height height) (min-height height))
  (multiple-value-bind (srwidth  srmin-width  srmax-width 
			srheight srmin-height srmax-height)
      (space-requirement-components sr)
    (make-space-requirement
      :width  (+ srwidth width)
      :min-width (+ srmin-width min-width)
      :max-width (+ srmax-width max-width)
      :height (+ srheight height)
      :min-height (+ srmin-height min-height)
      :max-height (+ srmax-height max-height))))

(define-compiler-macro space-requirement+ (&whole form sr1 sr2)
  (cond ((and (listp sr1)
	      (eq (first sr1) 'make-space-requirement))
	 (destructuring-bind (f &key (width 0) (max-width width) (min-width width)
				     (height 0) (max-height height) (min-height height))
	     sr1
	   (declare (ignore f))
	   `(space-requirement+* ,sr2
	      :width ,width :max-width ,max-width :min-width ,min-width
	      :height ,height :max-height ,max-height :min-height ,min-height)))
	((and (listp sr2)
	      (eq (first sr2) 'make-space-requirement))
	 (destructuring-bind (f &key (width 0) (max-width width) (min-width width)
				     (height 0) (max-height height) (min-height height))
	     sr2
	   (declare (ignore f))
	   `(space-requirement+* ,sr1
	      :width ,width :max-width ,max-width :min-width ,min-width
	      :height ,height :max-height ,max-height :min-height ,min-height)))
	(t form)))


;;;
;;; Space Req Mixin
;;; 

(defclass space-requirement-mixin (layout-mixin)
    ((initial-space-requirement :reader pane-initial-space-requirements)
     (space-requirement :reader pane-space-requirements)))

(defmethod initialize-instance :after ((pane space-requirement-mixin) 
				       &rest args
				       &key width max-width min-width
					    height max-height min-height)
  (declare (dynamic-extent args))
  (declare (ignore width min-width max-width
		   height min-height max-height))
  (with-slots (space-requirement initial-space-requirement) pane
    (multiple-value-bind (width min-width max-width
			  height min-height max-height)
	(apply #'default-space-requirements pane :allow-other-keys t args)
      (let ((sr (make-space-requirement
		  :width width :min-width min-width :max-width max-width 
		  :height height :min-height min-height :max-height max-height)))
	;; Space requirements are immutable...
	(setf space-requirement sr
	      initial-space-requirement sr)))))

(defmethod change-space-requirements-to-default ((pane space-requirement-mixin))
  (when (pane-initial-space-requirements pane)
    (change-space-requirements-to pane (pane-initial-space-requirements pane))))

(defmethod default-space-requirements ((pane space-requirement-mixin) 
				       &key (width 0)
					    (min-width width)
					    (max-width width)
					    (height 0)
					    (min-height height)
					    (max-height height))
  (values width  min-width  max-width
	  height min-height max-height))

(defmethod compose-space ((pane space-requirement-mixin) &key width height)
  (declare (ignore width height))
  (or (slot-value pane 'space-requirement)
      (slot-value pane 'initial-space-requirement)))

(defmethod change-space-requirements ((pane space-requirement-mixin) 
				      &key width min-width max-width
					   height min-height max-height
					   resize-frame)
  (declare (ignore resize-frame))
  (with-slots (space-requirement) pane
    (multiple-value-bind (srwidth  srmin-width  srmax-width 
			  srheight srmin-height srmax-height)
	(space-requirement-components space-requirement)
      (setq space-requirement (make-space-requirement 
				:width (or width srwidth)
				:min-width (or min-width srmin-width)
				:max-width (or max-width srmax-width)
				:height (or height srheight)
				:min-height (or min-height srmin-height)
				:max-height (or max-height srmax-height))))))


;;;
;;;  Client Space-req Mixin (read Client Space-req)
;;;

(defclass basic-client-space-requirement-mixin (layout-mixin)
    ((client-space-requirement :initform nil))
  (:documentation "User can specify a value for the client-space-requirement slot that defaults to NIL"))

(defmethod change-space-requirements ((pane basic-client-space-requirement-mixin) 
				      &key width min-width max-width 
					   height min-height max-height
					   resize-frame)
  (declare (ignore resize-frame))
  (with-slots (client-space-requirement) pane
    ;;--- This doesn't look very convincing!
    (let ((srwidth 100)  (srmin-width 0)  (srmax-width +fill+) 
	  (srheight 100) (srmin-height 0) (srmax-height +fill+))
      (when client-space-requirement
	(multiple-value-setq (srwidth  srmin-width  srmax-width 
			      srheight srmin-height srmax-height)
	  (space-requirement-components client-space-requirement)))
      (setq client-space-requirement (make-space-requirement 
				       :width (or width srwidth)
				       :min-width (or min-width srmin-width)
				       :max-width (or max-width srmax-width)
				       :height (or height srheight)
				       :min-height (or min-height srmin-height)
				       :max-height (or max-height srmax-height))))))

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
	    (make-space-requirement 
	      :width width :min-width min-width :max-width max-width
	      :height height :min-height min-height :max-height max-height)))))

;;--- This duplicates the method on SPACE-REQUIREMENT-MIXIN.  Can we share?
(defmethod default-space-requirements ((pane basic-client-space-requirement-mixin) 
				       &key (width 0)
					    (min-width width)
					    (max-width width)
					    (height 0)
					    (min-height height)
					    (max-height height))
  (values width  min-width  max-width
	  height min-height max-height))

;;;;

;;--- The only subclass of BASIC-CLIENT-SPACE-REQUIREMENT-MIXIN.  Merge them?
(defclass client-space-requirement-mixin (basic-client-space-requirement-mixin)
    ()
  (:documentation "If the user specifies some space requirements then use it otherwise use other value"))

(defmethod compose-space :around ((pane client-space-requirement-mixin) &key width height)
  (declare (ignore width height))
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

;(defclass client-stretchability-mixin (layout-mixin)
;    ((max-width :initform nil :initarg :max-width)
;     (max-height :initform nil :initarg :max-height)))
;

;(defmethod compose-space :around ((pane client-stretchability-mixin) &key width height)
;  (with-slots (max-width max-height) pane
;    (let ((sr (call-next-method)))
;      (when (or max-width max-height)
;	(multiple-value-bind (srwidth  srmin-width  srmax-width 
;			      srheight srmin-height srmax-height)
;	    (space-requirement-components sr)
;	  (setq sr (make-space-requirement :width srwidth 
;					   :min-width srmin-width
;					   :max-width (or max-width srmax-width)
;					   :height srheight 
;					   :min-height srmin-height
;					   :max-height (or max-height srmax-height)))))
;      sr)))

;;;
;;; Client Overridability
;;;

(defclass client-overridability-mixin (layout-mixin)
    ((width :initform nil :initarg :width)
     (min-width :initform nil :initarg :min-width)
     (max-width :initform nil :initarg :max-width)
     (height :initform nil :initarg :height)
     (min-height :initform nil :initarg :min-height)
     (max-height :initform nil :initarg :max-height))
  (:documentation "Client can specify values that override those provided elsewhere"))

(defmethod compose-space :around ((pane client-overridability-mixin) &key width height)
  (declare (ignore width height))
  (with-slots (width height max-width max-height min-width min-height) pane
    (let ((sr (call-next-method)))
      (if (or width min-width max-width height min-height max-height)
	  (multiple-value-bind (srwidth srmin-width srmax-width
				srheight srmin-height srmax-height)
	      (space-requirement-components sr)
	    (make-space-requirement
	      :width (or width srwidth)
	      :min-width (or min-width srmin-width)
	      :max-width (or max-width srmax-width)
	      :height (or height srheight)
	      :min-height (or min-height srmin-height)
	      :max-height (or max-height srmax-height)))
	  sr))))


;;;
;;; Client Demandability
;;;
;;; Used by db-scroll
;(defclass client-demandability-mixin (client-space-requirement-mixin
;				       space-requirement-cache-mixin)
;    ((space-demand-p :initform t :initarg :space-demand-p)))
;  
;(defmethod compose-space ((pane client-demandability-mixin) &key width height)
;  (with-slots (client-space-requirement space-demand-p) pane
;    (if space-demand-p 
;	(or client-space-requirement (compose-space (sheet-child pane)))
;	(let ((child-req (compose-space (sheet-child pane))))
;	  (if client-space-requirement 
;	      (make-space-requirement
;		:width (min (space-requirement-width client-space-req)
;			    (space-requirement-width child-req))
;		:max-width (max (space-requirement-max-width client-space-req)
;				(space-requirement-max-width child-req))
;		:height (min (space-requirement-height client-space-req)
;			     (space-requirement-height child-req))
;		:max-height (max (space-requirement-max-height client-space-req)
;				 (space-requirement-max-height child-req)))
;	      child-req)))))

;;;
;;; Space Req Cache
;;;

(defclass space-requirement-cache-mixin (layout-mixin)
    ((space-requirement-cache :initform nil)))

(defmethod compose-space :around ((pane space-requirement-cache-mixin) &key width height)
  (declare (ignore width height))
  (with-slots (space-requirement-cache) pane
    (or space-requirement-cache
	(setf space-requirement-cache (call-next-method)))))

(defmethod clear-space-requirement-cache ((pane layout-mixin))
  nil)

(defmethod clear-space-requirement-cache ((pane t))
  nil)

(defmethod clear-space-requirement-cache ((pane space-requirement-cache-mixin))
  (with-slots (space-requirement-cache) pane
    (setf space-requirement-cache nil)))

(defmethod change-space-requirements :before ((pane space-requirement-cache-mixin) &key)
  (clear-space-requirement-cache pane))

(defmethod note-space-requirements-changed :before
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
  (resize-sheet (sheet-child pane) width height))

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
;      (resize-sheet child width height))))
;  
;(defmethod compose-space ((pane simple-transforming-wrapping-space-mixin) &key width height)
;  (declare (ignore width height))
;  (let* ((child (sheet-child pane))
;	 (xform (sheet-transformation child))
;	 (req   (compose-space child)))
;    (multiple-value-bind (width  min-width  max-width 
;			  height min-height max-height)
;	(space-requirement-components req)
;      (multiple-value-setq (width height) 
;	(untransform-dimensions xform width height))
;      (multiple-value-setq (max-width max-height)
;	(untransform-dimensions xform max-width max-height))
;      (multiple-value-setq (min-width min-height)
;	(untransform-dimensions xform min-width min-height))
;      (make-space-requirement :width width :min-width min-width :max-width max-width
;			      :height height :min-height min-height :max-height max-height))))


;;;
;;; Restrainer-pane
;;;
;;; Use this wrapping composite to not allow note-space-requirements-changed to propagate up.

(defclass restrainer-pane (simple-wrapper-mixin
			   wrapping-space-mixin
			   sheet-mute-input-mixin
			   composite-pane)
    ())

(defmethod note-space-requirements-changed ((pane restrainer-pane) inner)
  (declare (ignore inner))
  (allocate-space 
    pane 
    (bounding-rectangle-width pane) (bounding-rectangle-height pane)))



;;; Generally useful layout function
;;; Used all over to satisfy constraints

;;--- I think that since coordinates are ultimately integers we loose
;;--- along the way, calling FIX-COORDINATE on alloc helps a little but we
;;--- still end up with a gap along the bottom-right edges.
#+++ignore
(defun allocate-space-to-items (given wanted items min-size
				desired-size max-size item-size)
  (macrolet ((desired-size (x) `(funcall desired-size ,x))
	     (min-size (x) `(funcall min-size ,x))
	     (max-size (x) `(funcall max-size ,x))
	     (item-size (x) `(funcall item-size ,x)))
    (let ((stretch-p (>= given (desired-size wanted)))
	  extra give used sizes)
      (if stretch-p
	  (setq give (- (max-size wanted) (desired-size wanted))
		extra (min (- given (desired-size wanted)) give))
	  (setq give (- (desired-size wanted) (min-size wanted))
		extra (min (- (desired-size wanted) given) give)))
      (dolist (item items)
	(let (alloc)
	  (typecase item
	    ((member :fill)
	     (if stretch-p
		 (progn
		   (setq alloc (/ (* +fill+ extra) give))
		   (setq alloc (ceiling alloc))
		   (fix-coordinates alloc)
		   (decf give +fill+))
		 (setq alloc 0)))
	    (t
	      (let* ((item-size (item-size item)))
		(if (eq item-size :fill)
		    (if stretch-p
			(progn
			  (setq alloc (/ (* +fill+ extra) give))
			  (setq alloc (ceiling alloc))
			  (fix-coordinates alloc)
			  (decf give +fill+))
			(setq alloc 0))
		    (progn
		      (setq alloc (desired-size item-size))
		      (when (> give 0)
			(if stretch-p
			    (progn
			      (setq used (/ (* (- (max-size item-size)
						  (desired-size item-size))
					       extra)
					    give))
			      (incf alloc used)
			      (fix-coordinates alloc)
			      (decf give (- (max-size item-size)
					    (desired-size item-size))))
			    (progn
			      (setq used (/ (* (- (desired-size item-size)
						  (min-size item-size))
					       extra)
					    give))
			      (decf alloc used)
			      (fix-coordinates alloc)
			      (decf give (- (desired-size item-size)
					    (min-size item-size)))))
			(decf extra used)))))))
	  (push alloc sizes)))
      (nreverse sizes))))
	
;; This new supa dupa version saves up all the fills to the end and
;; then divides up any leftover space equally amongst them and it
;; seems to work!
#---ignore
(defun allocate-space-to-items (given wanted items min-size
				desired-size max-size item-size)
  (macrolet ((desired-size (x) `(funcall desired-size ,x))
	     (min-size (x) `(funcall min-size ,x))
	     (max-size (x) `(funcall max-size ,x))
	     (item-size (x) `(funcall item-size ,x)))
    (let ((stretch-p (>= given (desired-size wanted)))
	  (allocated 0)
	  extra give used sizes)
      (if stretch-p
	  (setq give (- (max-size wanted) (desired-size wanted))
		extra (min (- given (desired-size wanted)) give))
	  (setq give (- (desired-size wanted) (min-size wanted))
		extra (min (- (desired-size wanted) given) give)))
      (dolist (item items)
	(let (alloc)
	  (typecase item
	    ((member :fill)
	     (setq alloc :fill))
	    (t
	      (let ((item-size (item-size item)))
		(if (eq item-size :fill)
		    (setq alloc :fill)
		    (progn
		      (setq alloc (desired-size item-size))
		      (when (> give 0)
			(if stretch-p
			    (progn
			      (setq used (/ (* (- (max-size item-size)
						  (desired-size item-size))
					       extra)
					    give))
			      (incf alloc used)
			      (fix-coordinates alloc)
			      (decf give (- (max-size item-size)
					    (desired-size item-size))))
			    (progn
			      (setq used (/ (* (- (desired-size item-size)
						  (min-size item-size))
					       extra)
					    give))
			      (decf alloc used)
			      (fix-coordinates alloc)
			      (decf give (- (desired-size item-size)
					    (min-size item-size)))))
			(decf extra used)))))))
	  (when (numberp alloc)
	    (incf allocated alloc))
	  (push alloc sizes)))
      (let ((fills (count :fill sizes))
	    (leftover (- given allocated)))
	(when (> fills 0)
	  (let ((x (if (> leftover 0) 
		       (fix-coordinate (/ leftover fills)) 
		       0)))
	    (setf sizes (nsubstitute x :fill sizes)))))
      (nreverse sizes))))


;;; Most of the layout panes should inherit from this

(defclass layout-pane (sheet-mute-input-mixin
		       pane-background-mixin
		       space-requirement-mixin
		       space-requirement-cache-mixin
		       sheet-permanently-enabled-mixin
		       composite-pane)
    ())


;;--- Does this need to be more complicated?
;;--- What is the correct behavior.  Should it take a space requirement?
;;--- and just call ALLOCATE-SPACE?
(defclass bboard-pane (space-requirement-mixin layout-pane) ())

(defmethod handle-event :after ((pane bboard-pane) (event pointer-motion-event))
  (deallocate-event event))

(defmethod allocate-space ((pane bboard-pane) width height)
  (declare (ignore width height))
  (dolist (child (sheet-children pane))
    (let ((space-req (compose-space child)))
      (resize-sheet child 
		    (space-requirement-width space-req)
		    (space-requirement-height space-req)))))
