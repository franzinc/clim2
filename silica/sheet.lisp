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
;; $fiHeader: sheet.lisp,v 1.8 92/03/24 19:36:50 cer Exp Locker: cer $

(in-package :silica)


(defgeneric sheet-parent (sheet))
(defgeneric sheet-children (sheet))
(defgeneric sheet-child (sheet))
(defgeneric sheet-enabled-children (sheet))
(defgeneric sheet-siblings (sheet))
(defgeneric sheet-ancestor-p (sheet))

(defgeneric sheet-adopt-child (sheet child))
(defgeneric sheet-disown-child (sheet child))

(defgeneric raise-sheet (sheet))
(defgeneric bury-sheet (sheet))
(defgeneric reorder-sheets (sheet new-ordering))

(defgeneric sheet-enabled-p (sheet))
(defgeneric (setf sheet-enabled-p) (enabled-p sheet))
(defgeneric sheet-viewable-p (sheet))
(defgeneric occluding-sheets (sheet child))

(defmethod bounding-rectangle* ((x sheet))
  (bounding-rectangle* (sheet-region x)))

(defmethod sheet-child (sheet)
  (let ((x (sheet-children sheet)))
    (unless (and x (null (cdr x)))
      (error "Not one child:~S" sheet))
    (car x)))

(defun map-over-sheets (function sheet)
  (declare (dynamic-extent function))
  (funcall function sheet)
  (when (typep sheet 'sheet-parent-mixin)
    (dolist (child (sheet-children sheet))
      (map-over-sheets function child))))

;;; Genealogy

(defclass sheet-leaf-mixin () ())

(defclass sheet-parent-mixin () ())

(defclass sheet-single-child-mixin (sheet-parent-mixin)
    ((child :initform nil :accessor sheet-child)))

(defmethod sheet-adopt-child :before ((sheet sheet) (child sheet))
  (when (sheet-parent child)
    (error "Trying to adopt a child that has a parent: ~S" child)))

(defmethod sheet-adopt-child ((sheet sheet-single-child-mixin) child)
  (when (sheet-child sheet)
    (error "Sheet Already has a child: ~S" sheet))
  (setf (sheet-child sheet) child
	(sheet-parent child) sheet))

(defmethod sheet-adopt-child :after (sheet child)
  (note-sheet-adopted child)
  (when (port sheet)
    (setf (port child :graft (graft sheet)) (port sheet))))

(defmethod sheet-children ((sheet sheet-single-child-mixin))
  (let ((x (sheet-child sheet)))
    (and x (list x))))

(defclass sheet-multiple-child-mixin (sheet-parent-mixin)
    ((children :initform nil :accessor sheet-children)))


(defmethod sheet-adopt-child ((sheet sheet-multiple-child-mixin) child)
  (push child (sheet-children sheet))
  (setf (sheet-parent child) sheet))

(defmethod (setf port) ((port null) sheet &key graft)
  (declare (ignore graft))
  (note-sheet-degrafted sheet)
  (setf (slot-value sheet 'port) port)
  (when (typep sheet 'sheet-parent-mixin)
    (dolist (child (sheet-children sheet))
      (setf (port child) port))))

(defmethod (setf port) ((port port) sheet &key graft)
  (setf (slot-value sheet 'port)  port
	(slot-value sheet 'graft) graft)
  (note-sheet-grafted sheet)
  (when (typep sheet 'sheet-parent-mixin)
    (dolist (child (sheet-children sheet))
      (setf (port child :graft graft) port))))

(defmethod sheet-disown-child ((parent sheet-multiple-child-mixin) child)
  (unless (eq (sheet-parent child) parent)
    (error "~S is not child of ~S" child parent))
  (setf (sheet-parent child) nil)
  (setf  (sheet-children parent)
	 (delete child (sheet-children parent)))
  (note-sheet-disowned child)
  (when (port parent)
    (setf (port child) nil)))


(defmethod sheet-disown-child ((parent sheet-single-child-mixin) child)
  (unless (eq (sheet-parent child) parent)
    (error "~S is not child of ~S" child parent))
  (setf (sheet-parent child) nil
	(sheet-child parent) nil)
  (note-sheet-disowned child)
  (when (port parent)
    (setf (port child) nil)))


;;;;

(defgeneric map-sheet-point*-to-parent (sheet x y)
  (:method (sheet x y)
   (transform-point* (sheet-transformation sheet) x y)))


(defgeneric map-sheet-point*-to-child (sheet x y)
  (:method (sheet x y)
   (untransform-point* (sheet-transformation sheet) x y)))

(defgeneric map-child-bounding-rectangle*-to-parent (sheet min-x min-y max-x max-y)
  (:method (sheet min-x min-y max-x max-y)
   (transform-rectangle*
     (sheet-transformation sheet)
     min-x min-y max-x max-y)))

(defgeneric map-parent-bounding-rectangle*-to-child (sheet min-x min-y max-x max-y)
  (:method (sheet min-x min-y max-x max-y)
   (untransform-rectangle*
     (sheet-transformation sheet)
     min-x min-y max-x max-y)))

(defgeneric child-containing-point (sheet point))

(defgeneric child-containing-point* (sheet x y))

(defmethod child-containing-point* (sheet x y)
  (find-if #'(lambda (child)
	       (and (sheet-enabled-p child)
		    (multiple-value-bind (x y)
			(untransform-point* (sheet-transformation child) x y)
		      (region-contains-point*-p (sheet-region child) x y))))
	   (sheet-children sheet)))


(defgeneric children-overlapping-region (sheet region))

(defmethod children-overlapping-region (sheet region)
  (remove-if-not #'(lambda (child)
		     (and (sheet-enabled-p child)
			  (multiple-value-call #'ltrb-overlaps-ltrb-p
			    (bounding-rectangle* child)
			    (bounding-rectangle*
			      (untransform-region 
				(sheet-transformation child) region)))))
		 (sheet-children sheet)))

(defgeneric children-overlapping-rectangle* (sheet min-x min-y max-x max-y))

(defgeneric delta-transformation (sheet ancestor)
  (:method (sheet ancestor)
   (let ((parent (sheet-parent sheet)))
     (cond
       ((eq parent ancestor)
	(sheet-transformation sheet))
       ((null parent) 
	(error "in delta transformation: ~S,~S"
	       sheet parent))
       (t
	(compose-transformations 
	  (sheet-transformation sheet)
	  (delta-transformation parent ancestor)))))))

(defgeneric allocated-region (sheet child))

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

(defgeneric note-sheet-adopted (sheet)
  (:method (sheet) nil))

(defgeneric note-sheet-disowned (sheet)
  (:method (sheet) nil))

(defgeneric note-sheet-grafted (sheet)
  (:method (sheet) nil))

(defgeneric note-sheet-degrafted (sheet)
  (:method (sheet) nil))

(defgeneric note-sheet-enabled (sheet)
  (:method (sheet) nil))

(defgeneric note-sheet-disabled (sheet)
  (:method (sheet) nil))

(defgeneric sheet-engrafted-p (sheet)
  (:method ((sheet sheet))
   (let ((parent (sheet-parent sheet)))
     (or (graftp parent)
	 (sheet-engrafted-p parent)))))

;;---

(defmethod (setf sheet-region) :after (nv sheet)
  (note-sheet-region-changed sheet))

(defgeneric note-sheet-region-changed (sheet &key port-did-it)
  (:method (sheet &key port-did-it) (declare (ignore port-did-it)) nil))

(defmethod (setf sheet-transformation) :after (nv sheet)
  (note-sheet-transformation-changed sheet))

(defgeneric note-sheet-transformation-changed (sheet &key port-did-it)
  (:method (sheet &key port-did-it) (declare (ignore port-did-it)) nil))


(defgeneric invalidate-cached-transformations (sheet)
  (:method ((sheet t)) nil)
  (:method :after ((sheet sheet-parent-mixin))
	   (mapc #'invalidate-cached-transformations (sheet-children sheet))))
	     
(defgeneric invalidate-cached-regions (sheet)
    (:method ((sheet t)) nil)
  (:method :after ((sheet sheet-parent-mixin))
	   (mapc #'invalidate-cached-regions (sheet-children sheet))))

(defmethod note-sheet-region-changed :before (sheet &key port-did-it)
  (declare (ignore port-did-it))
  (invalidate-cached-regions sheet))

;;-- Check to see if the call to invalidate-cached-regions is really necessary.
(defmethod note-sheet-transformation-changed :before (sheet &key port-did-it)
  (declare (ignore port-did-it))
  (invalidate-cached-transformations sheet)
  (invalidate-cached-regions sheet))


(defgeneric update-native-transformation (port sheet))

(defmethod (setf sheet-enabled-p) :after  (nv (sheet sheet))
  (if nv
      (note-sheet-enabled sheet)
      (note-sheet-disabled sheet)))

;;; Making sheets

(defmethod initialize-instance :after ((sheet sheet) &key parent children)
  (when parent
    (sheet-adopt-child parent sheet))
  (dolist (child children)
    (sheet-adopt-child sheet child)))


;;; Output

(defclass standard-sheet-output-mixin () ())

(defclass primitive-sheet-output-mixin () ())

(defclass mute-sheet-output-mixin () ())

(defclass sheet-with-medium (standard-sheet-output-mixin)
    ((medium :initform nil :accessor sheet-medium)
     (medium-type :initarg :medium :initform  t :accessor sheet-medium-type)))

(defclass permanent-medium-sheet-output-mixin (sheet-with-medium) ())

(defmethod note-sheet-grafted :after ((sheet permanent-medium-sheet-output-mixin))
  (when (sheet-medium-type sheet)
    (setf (sheet-medium sheet)
	  (make-medium (port sheet) sheet))
    (when (port sheet)
      (engraft-medium (sheet-medium sheet) (port sheet) sheet))))

(defmethod note-sheet-degrafted ((sheet permanent-medium-sheet-output-mixin))
  (when (sheet-medium sheet)
    (degraft-medium (sheet-medium sheet) (port sheet) sheet)
    (setf (sheet-medium sheet) nil)))


(defclass temporary-medium-sheet-output-mixin (sheet-with-medium) ())

;; This is badly named since it merely specifies the default

(defclass sheet-permanently-enabled-mixin () ()
  (:default-initargs :enabled t))

(defmethod initialize-instance :after ((sheet sheet-permanently-enabled-mixin) 
				       &key enabled)
  (setf (sheet-enabled-p sheet) enabled))
