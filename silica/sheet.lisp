(in-package :silica)
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

(defmethod bounding-rectangle* ((x sheet))
  (bounding-rectangle* (sheet-region x)))

(defgeneric sheet-parent (sheet) 
  )

(defgeneric sheet-children (sheet)
  )

(defgeneric sheet-child (sheet)
  )

(defgeneric adopt-child (sheet child)
  )

(defgeneric disown-child (sheet child)
  )

(defgeneric sheet-siblings (sheet)
  )

(defgeneric sheet-enabled-children (sheet)
  )

(defgeneric sheet-ancestor-p (sheet)
  )

(defgeneric raise-sheet (sheet)
  )

(defgeneric bury-sheet (sheet)
  )

(defgeneric reorder-sheets (sheet new-ordering)
  )

(defgeneric sheet-enabled-p (sheet)
  )


(defgeneric (setf sheet-enabled-p) (enabled-p sheet)
  )

(defgeneric sheet-viewable-p (sheet)
  )

(defgeneric occluding-sheets (sheet child)
  )


;;; Genealogy

(defclass sheet-leaf-mixin ()
  ())

(defclass sheet-parent-mixin ()
	  ())

(defclass sheet-single-child-mixin (sheet-parent-mixin)
  ((child :initform nil :accessor sheet-child))
  )

(defmethod adopt-child :before ((sheet sheet) (child sheet))
  (when (sheet-parent child)
    (error "Trying to adopt a child that has a parent: ~S" child)))

(defmethod adopt-child ((sheet sheet-single-child-mixin) child)
  (when (sheet-child sheet)
    (error "Sheet Already has a child: ~S" sheet))
  (setf (sheet-child sheet) child
	(sheet-parent child) sheet))

(defmethod adopt-child :after (sheet child)
  (note-sheet-adopted child)
  (when (port sheet)
    (setf (port child :graft (graft sheet)) (port sheet))))

(defmethod sheet-children ((sheet sheet-single-child-mixin))
  (let ((x (sheet-child sheet)))
    (and x (list x))))

(defclass sheet-multiple-child-mixin (sheet-parent-mixin)
  ((children :initform nil :accessor sheet-children))
  )


(defmethod adopt-child ((sheet sheet-multiple-child-mixin) child)
  (push child (sheet-children sheet))
  (setf (sheet-parent child) sheet))

(defmethod (setf port) ((port null) sheet &key graft)
  (declare (ignore graft))
  (note-sheet-degrafted sheet)
  (setf (slot-value sheet 'port) port)
  (dolist (child (sheet-children sheet))
    (setf (port child) port)))

(defmethod (setf port) ((port port) sheet &key graft)
  (setf (slot-value sheet 'port) port
	(slot-value sheet 'graft) graft)
  (note-sheet-grafted sheet)
  (when (typep sheet 'sheet-parent-mixin)
    (dolist (child (sheet-children sheet))
      (setf (port child :graft graft) port))))

;;;;

(defgeneric map-sheet-point*-to-parent (sheet x y)
  (:method (sheet x y)
	   (transform-point*
	    (sheet-transformation sheet)
	    x y)))


(defgeneric map-sheet-point*-to-child (sheet x y)
  (:method (sheet x y)
	   (untransform-point*
	    (sheet-transformation sheet)
	    x y)))

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

(defgeneric child-containing-point (sheet point)
  )

(defgeneric child-containing-point* (sheet x y)
  
  )

(defmethod child-containing-point* (sheet x y)
  (find-if #'(lambda (child)
	       (and (sheet-enabled-p child)
		    (multiple-value-bind
			(x y)
			(untransform-point* (sheet-transformation child) x y)
		      (region-contains-point*-p (sheet-region child) 
						x y))))
	   (sheet-children sheet)))


(defgeneric children-overlapping-region (sheet region)
  )

(defmethod children-overlapping-region (sheet region)
  (remove-if-not #'(lambda (child)
		     (and (sheet-enabled-p child)
			  (multiple-value-call
			      #'ltrb-overlaps-ltrb-p
			    (bounding-rectangle* child)
			    (bounding-rectangle*
			     (untransform-region 
			      (sheet-transformation child)
			      region)))))
		 (sheet-children sheet)))

(defgeneric children-overlapping-rectangle* (sheet min-x min-y max-x max-y)
  )

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

(defgeneric allocated-region (sheet child)
  )

;;;; 

(defclass sheet-identity-transformation-mixin ()
	  ())

(defmethod sheet-transformation ((sheet sheet-identity-transformation-mixin))
  +identity-transformation+)

(defclass sheet-transformation-mixin ()
	  ((transformation 
	    :initarg :transformation
	    :initform +identity-transformation+
	    :accessor sheet-transformation)))

(defclass sheet-translation-mixin (sheet-transformation-mixin)
	  ())

(defclass sheet-y-inverting-transformation-mixin (sheet-transformation-mixin)
	  ())



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


(defmethod (setf sheet-region) :after (nv sheet)
  (note-sheet-region-changed sheet))

(defgeneric note-sheet-region-changed (sheet &key port)
  (:method (sheet &key port) nil))

(defmethod (setf sheet-transformation) :after (nv sheet)
  (note-sheet-transformation-changed sheet))

(defgeneric note-sheet-transformation-changed (sheet &key port)
  (:method (sheet &key port)
	   (declare (ignore port))
	   nil))


(defgeneric sheet-engrafted-p (sheet)
  (:method ((sheet sheet))
	   (let ((parent (sheet-parent sheet)))
	     (or (graftp parent)
		 (sheet-engrafted-p parent)))))


(defmethod note-sheet-region-changed :after (sheet &key port)
  (declare (ignore port))
  (invalidate-cached-regions sheet))

(defmethod note-sheet-transformation-changed :after (sheet &key port)
  (declare (ignore port))
  (invalidate-cached-transformations sheet))



(defgeneric update-native-transformation (port sheet)
  )

(defmethod (setf sheet-enabled-p) :after  (nv (sheet sheet))
  (if nv
      (note-sheet-enabled sheet)
    (note-sheet-disabled sheet)))

;;; Making sheets

(defmethod initialize-instance :after ((sheet sheet) &key parent children)
  (when parent
    (adopt-child parent sheet))
  (dolist (child children)
    (adopt-child sheet child)))


;;; Output

(defclass standard-sheet-output-mixin ()
	  ())

(defclass primitive-sheet-output-mixin ()
	  ())

(defclass mute-sheet-output-mixin ()
	  ())

(defclass sheet-with-medium (standard-sheet-output-mixin)
  ((medium :initform nil :accessor sheet-medium)
   (medium-type :initarg :medium :initform  t :accessor sheet-medium-type)))

(defclass permanent-medium-sheet-output-mixin (sheet-with-medium)
  ())

(defmethod note-sheet-grafted :after ((sheet permanent-medium-sheet-output-mixin))
  (when (sheet-medium-type sheet)
    (setf (sheet-medium sheet)
	  (make-medium (port sheet) sheet))
    (when (port sheet)
      (engraft-medium (sheet-medium sheet) (port sheet) sheet))))

(defmethod note-sheet-degrafted ((sheet permanent-medium-sheet-output-mixin))
  (when (sheet-medium sheet)
    (error "do something")))


(defclass temporary-medium-sheet-output-mixin (sheet-with-medium)
  ())

(defclass sheet-permanently-enabled-mixin () ())

(defmethod initialize-instance :after ((sheet sheet-permanently-enabled-mixin) &key)
  (setf (sheet-enabled-p sheet) t))
