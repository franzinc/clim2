;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: sheet.lisp,v 1.34 92/12/16 16:49:48 cer Exp $

(in-package :silica)

"Copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
 Portions copyright (c) 1992 Symbolics, Inc.  All rights reserved."


(defgeneric sheet-parent (sheet))
(defgeneric sheet-children (sheet))
(defgeneric sheet-child (sheet))
(defgeneric sheet-enabled-children (sheet))
(defgeneric sheet-siblings (sheet))
(defgeneric sheet-ancestor-p (sheet putative-ancestor))

(defgeneric sheet-adopt-child (sheet child))
(defgeneric sheet-disown-child (sheet child))

(defgeneric raise-sheet (sheet))
(defgeneric bury-sheet (sheet))
(defgeneric reorder-sheets (sheet new-ordering))

(defgeneric sheet-enabled-p (sheet))
(defgeneric (setf sheet-enabled-p) (enabled-p sheet))
(defgeneric sheet-viewable-p (sheet))
(defgeneric sheet-occluding-sheets (sheet child))

(defgeneric sheet-shell (sheet))

(defmethod bounding-rectangle* ((sheet basic-sheet))
  (bounding-rectangle* (sheet-region sheet)))

(defmethod sheet-child ((sheet basic-sheet))
  (let ((children (sheet-children sheet)))
    (when (cdr children)
      (error "The sheet ~S has more than one child" sheet))
    (car children)))


;;; Genealogy

(defclass sheet-leaf-mixin () ())

(defclass sheet-parent-mixin () ())

(defclass sheet-single-child-mixin (sheet-parent-mixin)
    ((children :initform nil :accessor sheet-children)))

(defun map-over-sheets (function sheet)
  (declare (dynamic-extent function))
  (funcall function sheet)
  (when (typep sheet 'sheet-parent-mixin)
    (dolist (child (sheet-children sheet))
      (map-over-sheets function child))))

(defmethod sheet-adopt-child :before ((sheet basic-sheet) (child basic-sheet))
  (when (sheet-parent child)
    (error "Sheet ~S trying to adopt child ~S that already has a parent" sheet child)))

(defmethod sheet-adopt-child ((sheet sheet-single-child-mixin) child)
  (when (sheet-children sheet)
    (error "Single-child sheet ~S already has a child" sheet))
  (setf (sheet-children sheet) (list child)
	(sheet-parent child) sheet))

(defmethod sheet-child ((sheet sheet-single-child-mixin))
  (car (sheet-children sheet)))

(defmethod sheet-adopt-child :after ((sheet basic-sheet) child)
  (note-sheet-adopted child)
  (when (port sheet)
    (setf (port child :graft (graft sheet)) (port sheet))))



(defclass sheet-multiple-child-mixin (sheet-parent-mixin)
    ((children :initform nil :accessor sheet-children)))

(defmethod sheet-adopt-child ((sheet sheet-multiple-child-mixin) child)
  (setf (sheet-children sheet) 
	;; Preserve the order in which the sheets were adopted
	;;--- This may have unwanted effects if the children overlap...
	(nconc (sheet-children sheet) (list child)))
  (setf (sheet-parent child) sheet))

(defmethod (setf port) ((port null) sheet &key graft)
  (declare (ignore graft))
  (note-sheet-degrafted sheet)
  (setf (slot-value sheet 'port) port)
  (when (typep sheet 'sheet-parent-mixin)
    (dolist (child (sheet-children sheet))
      (setf (port child) port))))

(defmethod (setf port) ((port basic-port) sheet &key graft)
  (setf (slot-value sheet 'port)  port
	(slot-value sheet 'graft) graft)
  (note-sheet-grafted sheet)
  (when (typep sheet 'sheet-parent-mixin)
    (dolist (child (sheet-children sheet))
      (setf (port child :graft graft) port)))
  (note-sheet-tree-grafted port sheet))

(defmethod note-sheet-tree-grafted ((port port) (sheet basic-sheet))
  ;; This method is invoked when the sheet and its descendents have
  ;; been mirrored
  nil)

(defmethod sheet-disown-child ((parent sheet-multiple-child-mixin) child)
  (unless (eq (sheet-parent child) parent)
    (error "~S is not child of ~S" child parent))
  (note-sheet-disowned child)
  (when (port parent)
    (setf (port child) nil))
  (setf (sheet-parent child) nil)
  (setf (sheet-children parent)
	(delete child (sheet-children parent))))

(defmethod sheet-disown-child ((parent sheet-single-child-mixin) child)
  (unless (eq (sheet-parent child) parent)
    (error "~S is not child of ~S" child parent))
  (note-sheet-disowned child)
  (when (port parent)
    (setf (port child) nil))
  (setf (sheet-parent child) nil
	(sheet-children parent) nil))


(defun sheet-top-level-sheet (sheet)
  (do* ((s sheet parent)
	(parent (sheet-parent s) parent-parent)
	(parent-parent (if parent (sheet-parent parent) t) (sheet-parent parent)))
       ((null parent-parent) s)
    (when (eq parent-parent t)
      (return nil))))


;;; Raising and lowering

(defmethod raise-sheet ((sheet basic-sheet))
  (let ((parent (sheet-parent sheet)))
    (when parent
      (setf (sheet-children parent)
	    (cons sheet (delete sheet (sheet-children parent))))))
  (when (sheet-direct-mirror sheet)
    (raise-mirror (port sheet) sheet)))

(defmethod bury-sheet ((sheet basic-sheet))
  (let ((parent (sheet-parent sheet)))
    (when parent
      (setf (sheet-children parent)
	    (nconc (delete sheet (sheet-children parent)) (list sheet)))))
  (when (sheet-direct-mirror sheet)
    (bury-mirror (port sheet) sheet)))

(defmethod reorder-sheets ((parent basic-sheet) new-ordering)
  ;; Error check new ordering.
  ;; This could be optimized if we can guarantee that the ordering of
  ;; SHEET-CHILDREN is always kept up to date wrt the actual stacking
  ;; order.  Unfortunately this is quite hard under X11 for top level windows.
  (assert (null (set-exclusive-or new-ordering (sheet-children parent)))
  	  (new-ordering)
	  "Specified ordering ~S does not contain children of sheet ~S"
	  new-ordering parent)
  (setf (sheet-children parent) new-ordering)
  (let ((port (port parent)))
    (dolist (child (reverse new-ordering))
      (when (sheet-direct-mirror child)
	(raise-mirror port child)))))


;;; Geometry

(defgeneric map-sheet-position-to-parent (sheet x y))
(defmethod map-sheet-position-to-parent ((sheet basic-sheet) x y)
  (transform-position (sheet-transformation sheet) x y))

(defgeneric map-sheet-position-to-child (sheet x y))
(defmethod map-sheet-position-to-child ((sheet basic-sheet) x y)
  (untransform-position (sheet-transformation sheet) x y))

(defgeneric map-sheet-rectangle*-to-parent (sheet left top right bottom))
(defmethod map-sheet-rectangle*-to-parent ((sheet basic-sheet) left top right bottom)
  (transform-rectangle*
    (sheet-transformation sheet)
    left top right bottom))

(defgeneric map-sheet-rectangle*-to-child (sheet left top right bottom))
(defmethod map-sheet-rectangle*-to-child ((sheet basic-sheet) left top right bottom)
  (untransform-rectangle*
    (sheet-transformation sheet)
    left top right bottom))

(defgeneric child-containing-position (sheet x y))
(defmethod child-containing-position ((sheet basic-sheet) x y)
  (find-if #'(lambda (child)
	       (and (sheet-enabled-p child)
		    (multiple-value-bind (x y)
			(untransform-position (sheet-transformation child) x y)
		      (region-contains-position-p (sheet-region child) x y))))
	   (sheet-children sheet)))

(defgeneric children-overlapping-region (sheet region))
(defmethod children-overlapping-region ((sheet basic-sheet) region)
  (if (or (null region)				;--- kludge
	  (eq region +everywhere+))
      (remove-if-not #'sheet-enabled-p (sheet-children sheet))
      (with-bounding-rectangle* (left top right bottom) region
	(remove-if-not
	  #'(lambda (child)
	      (and (sheet-enabled-p child)
		   (multiple-value-call #'ltrb-overlaps-ltrb-p
		     (bounding-rectangle* child)
		     (untransform-rectangle* 
		       (sheet-transformation child) left top right bottom))))
	  (sheet-children sheet)))))

(defgeneric map-over-sheets-containing-position (function sheet x y)
  (declare (dynamic-extent function)))
(defmethod map-over-sheets-containing-position (function (sheet basic-sheet) x y)
  (declare (dynamic-extent function))
  (dolist (child (sheet-children sheet))
    (when (and (sheet-enabled-p child)
	       (multiple-value-bind (x y)
		   (untransform-position (sheet-transformation child) x y)
		 (region-contains-position-p (sheet-region child) x y)))
      (funcall function child))))

(defgeneric map-over-sheets-overlapping-region (function sheet region)
  (declare (dynamic-extent function)))
(defmethod map-over-sheets-overlapping-region (function (sheet basic-sheet) region)
  (declare (dynamic-extent function))
  (if (or (null region)				;--- kludge
	  (eq region +everywhere+))
      (dolist (child (sheet-children sheet))
	(when (sheet-enabled-p child)
	  (funcall function child)))
      (with-bounding-rectangle* (left top right bottom) region
	(dolist (child (sheet-children sheet))
	  (when (and (sheet-enabled-p child)
		     (multiple-value-call #'ltrb-overlaps-ltrb-p
		       (bounding-rectangle* child)
		       (untransform-rectangle* 
			 (sheet-transformation child) left top right bottom)))
	    (funcall function child))))))

;;;; 

(defclass sheet-identity-transformation-mixin () ())

(defmethod sheet-transformation ((sheet sheet-identity-transformation-mixin))
  +identity-transformation+)

(defclass sheet-transformation-mixin ()
    ((transformation 
       :initarg :transformation :initform +identity-transformation+
       :accessor sheet-transformation)
     (cached-device-transformation :initform nil
				   :accessor sheet-cached-device-transformation)
     ;; This next is here for lack of a better place, and because the accessor
     ;; sheet-device-region can only work on a sheet that is of this class.
     (cached-device-region :initform nil
			   :accessor sheet-cached-device-region)))

(defclass sheet-translation-mixin (sheet-transformation-mixin) ())

(defclass sheet-y-inverting-transformation-mixin (sheet-transformation-mixin) ())


;;; Notification

(defgeneric note-sheet-adopted (sheet))
(defmethod note-sheet-adopted ((sheet basic-sheet)) nil)

(defgeneric note-sheet-disowned (sheet))
(defmethod note-sheet-disowned ((sheet basic-sheet)) nil)

(defgeneric note-sheet-grafted (sheet))
(defmethod note-sheet-grafted ((sheet basic-sheet)) nil)

(defgeneric note-sheet-degrafted (sheet))
(defmethod note-sheet-degrafted ((sheet basic-sheet)) nil)

(defgeneric note-sheet-enabled (sheet))
(defmethod note-sheet-enabled ((sheet basic-sheet)) nil)

(defgeneric note-sheet-disabled (sheet))
(defmethod note-sheet-disabled ((sheet basic-sheet)) nil)

(defgeneric sheet-engrafted-p (sheet))
(defmethod sheet-engrafted-p ((sheet basic-sheet))
  (let ((parent (sheet-parent sheet)))
    (or (graftp parent)
	(sheet-engrafted-p parent))))

(defmethod (setf sheet-region) :after (region (sheet basic-sheet))
  (declare (ignore region))
  (note-sheet-region-changed sheet))

(defgeneric note-sheet-region-changed (sheet &key port-did-it))
(defmethod note-sheet-region-changed ((sheet basic-sheet) &key port-did-it)
  (declare (ignore port-did-it))
  nil)

(defmethod (setf sheet-transformation) :after (transformation (sheet basic-sheet))
  (declare (ignore transformation))
  (note-sheet-transformation-changed sheet))

(defgeneric note-sheet-transformation-changed (sheet &key port-did-it))
(defmethod note-sheet-transformation-changed ((sheet basic-sheet) &key port-did-it)
  (declare (ignore port-did-it)) 
  nil)

(defgeneric invalidate-cached-regions (sheet))

(defmethod invalidate-cached-regions ((sheet basic-sheet)) nil)

(defmethod invalidate-cached-regions ((sheet sheet-transformation-mixin)) 
  (let ((region (sheet-cached-device-region sheet)))
    (when region
      (if (eq region +nowhere+)			;it can happen...
	  (setf (sheet-cached-device-region sheet) nil)
	  (setf (slot-value (sheet-cached-device-region sheet) 'left) nil)))))

(defmethod invalidate-cached-regions :after ((sheet sheet-parent-mixin))
  ;;--- In theory if this sheet has a mirror we don't need to do any more
  (unless (sheet-direct-mirror sheet)
    (mapc #'invalidate-cached-regions (sheet-children sheet))))

(defmethod note-sheet-region-changed :before ((sheet basic-sheet) &key port-did-it)
  (declare (ignore port-did-it))
  (invalidate-cached-regions sheet))

(defgeneric invalidate-cached-transformations (sheet))

(defmethod invalidate-cached-transformations ((sheet basic-sheet)) nil)

(defmethod invalidate-cached-transformations ((sheet sheet-transformation-mixin)) 
  (let ((region (sheet-cached-device-region sheet)))
    (when region
      (if (eq region +nowhere+)			;it can happen...
	  (setf (sheet-cached-device-region sheet) nil)
	  (setf (slot-value (sheet-cached-device-region sheet) 'left) nil))))
  (setf (sheet-cached-device-transformation sheet) nil))

(defmethod invalidate-cached-transformations :after ((sheet sheet-parent-mixin))
  ;;--- In theory if this sheet has a mirror we don't need to do any more
  (unless (sheet-direct-mirror sheet)
    (mapc #'invalidate-cached-transformations (sheet-children sheet))))
 
;;--- Check to see if the call to INVALIDATE-CACHED-REGIONS is really necessary.
;;--- CER thinks we do because regions depend on transformations.
(defmethod note-sheet-transformation-changed :before ((sheet basic-sheet) &key port-did-it)
  (declare (ignore port-did-it))
  (invalidate-cached-transformations sheet)
  (invalidate-cached-regions sheet))


(defmethod (setf sheet-enabled-p) :after (enabled (sheet basic-sheet))
  (if enabled
      (note-sheet-enabled sheet)
      (note-sheet-disabled sheet)))

(defmethod sheet-enabled-children ((sheet sheet-parent-mixin))
  (remove-if-not #'sheet-enabled-p (sheet-children sheet)))

(defmethod sheet-siblings ((sheet basic-sheet))
  (let ((parent (sheet-parent sheet)))
    (assert parent () "Sheet must have parent")
    (remove sheet (sheet-children parent))))

(defmethod sheet-ancestor-p ((sheet basic-sheet) (ancestor basic-sheet))
  (do ((sheet sheet (sheet-parent sheet)))
      ((or (null sheet)
	   (eq sheet ancestor))
       (and sheet t))))

(defmethod sheet-viewable-p ((sheet basic-sheet))
  (do ((sheet sheet (sheet-parent sheet)))
      ((graftp sheet) t)
    (unless (sheet-enabled-p sheet) (return nil))))

(defmethod sheet-occluding-sheets ((sheet basic-sheet) (child basic-sheet))
  (let ((sheets nil)
	(r (transform-region (sheet-transformation child) (sheet-region child))))
    (dolist (child1 (sheet-children sheet) (error "Sheet ~S is not the parent of ~S" sheet child))
      (cond ((eq child1 child) (return (nreverse sheets)))
	    ((region-intersects-region-p r (transform-region (sheet-transformation child1) (sheet-region child1)))
	     (push child1 r))))))

;;; Making sheets

(defmethod initialize-instance :after ((sheet basic-sheet) &key parent children)
  (when parent
    (sheet-adopt-child parent sheet))
  (dolist (child children)
    (sheet-adopt-child sheet child)))


;;; Output

(defclass standard-sheet-output-mixin () ())

(defclass primitive-sheet-output-mixin () ())

(defclass sheet-mute-output-mixin () ())

(defclass sheet-with-medium-mixin (standard-sheet-output-mixin)
    ((medium :initform nil :accessor sheet-medium)
     (medium-type :initarg :medium :initform t :accessor sheet-medium-type)))

(defmethod invalidate-cached-regions :before ((sheet sheet-with-medium-mixin))
  (let ((medium (sheet-medium sheet)))
    (when medium 
      (invalidate-cached-regions medium))))

(defmethod invalidate-cached-transformations :before ((sheet sheet-with-medium-mixin))
  (let ((medium (sheet-medium sheet)))
    (when medium
      (invalidate-cached-transformations medium))))


(defclass permanent-medium-sheet-output-mixin (sheet-with-medium-mixin) ())

(defmethod note-sheet-grafted :around ((sheet permanent-medium-sheet-output-mixin))
  ;; By making this an :AROUND method we make sure that the mirror has
  ;; been realized at this point, if it's a mirrored sheet.  This is pretty
  ;; horrible but it makes sure that things happen in the right order.
  (call-next-method)
  (let ((medium-type (sheet-medium-type sheet)))
    (when medium-type
      (setf (sheet-medium sheet)
	    (if (mediump medium-type)
		medium-type
		(make-medium (port sheet) sheet)))
      (when (port sheet)
	(engraft-medium (sheet-medium sheet) (port sheet) sheet)))))

(defmethod note-sheet-degrafted ((sheet permanent-medium-sheet-output-mixin))
  (when (sheet-medium sheet)
    (degraft-medium (sheet-medium sheet) (port sheet) sheet)
    (setf (sheet-medium sheet) nil)))


(defclass temporary-medium-sheet-output-mixin (sheet-with-medium-mixin) ())

;; One medium is shared among several closely related sheets (such as
;; the CLIM scroll bars).  The foreground/background/text-style must
;; be the same for all the sheets.  Ditto the transformation.  The sheets
;; must even share the same drawable.
(defclass shared-medium-sheet-output-mixin (sheet-with-medium-mixin)
    ((shared-medium-sheet :initform nil :initarg :shared-medium-sheet)))

(defmethod note-sheet-grafted :around ((sheet shared-medium-sheet-output-mixin))
  (call-next-method)
  (with-slots (shared-medium-sheet) sheet
    (when shared-medium-sheet
      (setf (sheet-medium sheet) (sheet-medium shared-medium-sheet)))))

;; This is badly named since it merely specifies the default
(defclass sheet-permanently-enabled-mixin () ()
  (:default-initargs :enabled t))

(defmethod initialize-instance :after ((sheet sheet-permanently-enabled-mixin) 
				       &key enabled)
  (setf (sheet-enabled-p sheet) enabled))


;;; Sheet pointer cursors and pointer grabbing

(defmethod (setf sheet-pointer-cursor) (cursor (sheet basic-sheet))
  (port-set-sheet-pointer-cursor (port sheet) sheet cursor)
  (setf (slot-value sheet 'pointer-cursor) cursor))

(defmethod port-set-sheet-pointer-cursor ((port basic-port) sheet cursor)
  (declare (ignore sheet cursor))
  nil)

(defmacro with-pointer-grabbed ((sheet &rest options) &body body)
  `(flet ((with-pointer-grabbed-body () ,@body))
     (declare (dynamic-extent #'with-pointer-grabbed-body))
     (invoke-with-pointer-grabbed ,sheet #'with-pointer-grabbed-body ,@options)))

(defun invoke-with-pointer-grabbed (sheet continuation &rest options)
  (declare (dynamic-extent options))
  (apply #'port-invoke-with-pointer-grabbed (port sheet) sheet continuation options))

(defmethod port-invoke-with-pointer-grabbed :around
	   ((port basic-port) (sheet basic-sheet) continuation &key)
  (declare (ignore continuation))
  (letf-globally (((port-grabbing-sheet port) sheet))
    (call-next-method)))

(defmethod port-invoke-with-pointer-grabbed 
	   ((port basic-port) (sheet basic-sheet) continuation &key)
  (funcall continuation))

(defmethod (setf sheet-grabbed-pointer-cursor) (cursor (sheet basic-sheet))
  (port-set-sheet-grabbed-pointer-cursor (port sheet) sheet cursor))

(defmethod port-set-sheet-grabbed-pointer-cursor
	   ((port basic-port) (sheet basic-sheet) cursor)
  (declare (ignore cursor))
  nil)
