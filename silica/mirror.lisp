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
;; $fiHeader: mirror.lisp,v 1.7 92/02/08 14:51:36 cer Exp $

(in-package :silica)


;;;; sheet-device-transformation - transformation from sheets
;;;; coordinate space to its mirror

;;;; sheet-device-region - clipping region in mirror coordinate space

;;; Mirrors

(defgeneric sheet-mirror (sheet))
(defgeneric sheet-direct-mirror (sheet))

(defgeneric sheet-native-transformation (sheet))
(defgeneric sheet-native-region (sheet))
(defgeneric sheet-native-region* (sheet))
(defgeneric sheet-device-region (sheet))

(defgeneric realize-mirror (port sheet))
(defgeneric destroy-mirror (port sheet))
(defgeneric enable-mirror (port sheet))
(defgeneric disable-mirror (port sheet))

(defmethod sheet-direct-mirror ((sheet t)) nil)


(defclass mirrored-sheet-mixin ()
    ((mirror :initform nil :accessor sheet-direct-mirror)
     (native-transformation :initform +identity-transformation+
			    :accessor sheet-native-transformation)))

;;; Native transformation

(defmethod sheet-native-transformation (sheet)
  (compose-transformations 
    (sheet-transformation sheet)
    (sheet-native-transformation (sheet-parent sheet))))

;;; Device-transformation
;;; transformation from a sheets coordinate space to the coordinate
;;; space of the mirror

(defgeneric sheet-device-transformation (sheet))

(defmethod sheet-device-transformation ((sheet sheet))
  (compose-transformations
    (sheet-transformation sheet)
    (sheet-device-transformation (sheet-parent sheet))))

(defmethod sheet-device-transformation :around ((sheet mirrored-sheet-mixin))
  (if (sheet-direct-mirror sheet)
      (sheet-native-transformation sheet)
      (call-next-method)))

;;; Native region

(defmethod sheet-native-region ((sheet mirrored-sheet-mixin))
  (transform-region
    (sheet-native-transformation sheet)
    (sheet-region sheet)))

(defmethod sheet-native-region* ((sheet mirrored-sheet-mixin))
  (with-bounding-rectangle* (minx miny maxx maxy) (sheet-region sheet)
    (transform-rectangle*
      (sheet-native-transformation sheet)
      minx miny maxx maxy)))

;;; Device region
  

(defmethod sheet-device-region ((sheet mirrored-sheet-mixin))
  (sheet-native-region sheet))

;;--- This assumes that sheet siblings do not overlap...
(defmethod sheet-device-region (sheet)
  (region-intersection
    (transform-region 
      (sheet-device-transformation sheet)
      (sheet-region sheet))
    (sheet-device-region (sheet-parent sheet))))

;;;; Mirror region stuff


(defmethod sheet-actual-native-edges* (sheet)
  ;; Returns the sheet-region in the parents native coordinate space
  (let* ((region (sheet-region sheet))
	 (sheet-to-parent (sheet-transformation sheet))
	 (parent (sheet-parent sheet))
	 (parent-to-native (sheet-device-transformation parent)))
    (with-bounding-rectangle* (left top right bottom)
	(transform-region 
	  parent-to-native
	  (transform-region sheet-to-parent region))
      (values left top right bottom))))

;; Returns the regions mirror in the mirror coordinate space
(defgeneric mirror-region (port sheet))

;; Returns the mirror's region in its parents coordinate space
(defgeneric mirror-edges* (port sheet))

;; Sets the mirror's region in its parents coordinate space
(defgeneric set-mirror-edges* (port sheet minx miny maxx maxy))

(defmethod mirror-origin (port sheet)
  (declare (ignore port sheet))
  :nw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmethod sheet-mirror ((sheet sheet))
  (sheet-direct-mirror
    (sheet-mirrored-ancestor sheet)))

(defgeneric sheet-mirrored-ancestor (sheet)
  (:method ((sheet sheet))
   (cond ((sheet-direct-mirror sheet)
	  sheet)
	 ((null (sheet-parent sheet))
	  (error "Error in sheet mirrored ancestor: ~S"
		 sheet))
	 (t
	  (sheet-mirrored-ancestor
	    (sheet-parent sheet))))))



(defmethod realize-mirror :around ((port port) (sheet mirrored-sheet-mixin))
  (let ((mirror
	  (or (sheet-direct-mirror sheet)
	      (setf (sheet-direct-mirror sheet)
		    (call-next-method)))))
    (setf (gethash mirror (port-mirror->sheet-table port)) sheet)
    (update-mirror-transformation port sheet)
    mirror))

(defmethod destroy-mirror :around ((port port) (sheet mirrored-sheet-mixin))
  (call-next-method)
  (setf (sheet-direct-mirror sheet) nil))

(defmethod note-sheet-grafted :after ((sheet mirrored-sheet-mixin))
  (realize-mirror (sheet-port sheet) sheet))

(defmethod note-sheet-degrafted :after ((sheet mirrored-sheet-mixin))
  (when (sheet-direct-mirror sheet)
    (destroy-mirror (sheet-port sheet) sheet)))

(defmethod note-sheet-enabled ((sheet mirrored-sheet-mixin))
  (call-next-method)
  (when (sheet-direct-mirror sheet)
    (enable-mirror (sheet-port sheet) sheet)))

(defmethod note-sheet-disabled ((sheet mirrored-sheet-mixin))
  (call-next-method)
  (when (sheet-direct-mirror sheet)
    (disable-mirror (sheet-port sheet) sheet)))


(defgeneric invalidate-cached-transformations (sheet)
  (:method (sheet) nil)
  (:method :after ((sheet sheet-parent-mixin))
	   (mapc #'invalidate-cached-transformations (sheet-children sheet))))
	     

(defgeneric invalidate-cached-regions (sheet)
  (:method (sheet) nil)
  (:method :after ((sheet sheet-parent-mixin))
	   (mapc #'invalidate-cached-regions (sheet-children sheet))))


;;--- What sucks big?
(warn "This sucks big")

(defmethod invalidate-cached-transformations :after ((sheet mirrored-sheet-mixin))
  (when (sheet-direct-mirror sheet)
    (update-mirror-region (sheet-port sheet) sheet)))

(defmethod note-sheet-region-changed :after ((sheet mirrored-sheet-mixin) &key port)
  (when (sheet-direct-mirror sheet)
    (unless port
      (update-mirror-region (sheet-port sheet) sheet))))


;; I do not think we need to do this because
;; invalidate-cached-transformations does it anyway.

#+ignore
(defmethod note-sheet-transformation-changed :after ((sheet mirrored-sheet-mixin) &key port)
  (when (sheet-direct-mirror sheet)
    (unless port
      (update-mirror-region (sheet-port sheet) sheet))))

(defun sheet-top-level-mirror (sheet)
  (let ((mirror nil))
    (loop
      (when (graftp sheet)
	(return mirror))
      (when (sheet-direct-mirror sheet) 
	(setq mirror sheet))
      (setq sheet (sheet-parent sheet)))))

;;; There is some confusion here about mirror-inside-edges*
;;; This actual returns coordinates in the mirrors parents space
;;; rather than

(defmethod update-mirror-region ((port port) (sheet mirrored-sheet-mixin))
  ;; If the sheet-region is changed this updates the mirrors region  accordingly
  (update-mirror-region-1 port sheet (sheet-parent sheet)))
  
(defmethod update-mirror-region-1 ((port port) sheet parent)
  (declare (ignore parent))
  ;; Coordinates in parent space
  (multiple-value-bind (target-left target-top target-right target-bottom)
      (sheet-actual-native-edges* sheet)
    (multiple-value-bind (actual-left actual-top actual-right actual-bottom)
	(mirror-native-edges* (sheet-port sheet) sheet)
      (unless nil				;---WTF ???
	(set-sheet-mirror-edges*
	  (sheet-port sheet) sheet
	  target-left target-top target-right target-bottom))))
  (update-mirror-transformation port sheet))

(defmethod update-mirror-transformation ((port port) (sheet mirrored-sheet-mixin))
  ;; Compute transformation from sheet-region to mirror cordinates.
  (update-mirror-transformation-1 port sheet (sheet-parent sheet)))

(defmethod update-mirror-transformation-1 ((port port) sheet parent)
  (declare (ignore parent))
  ;; would imagine that a lot of the time this would be identity
  (multiple-value-bind (mirror-left mirror-top mirror-right mirror-bottom)
      (mirror-inside-edges* port sheet)
    (with-bounding-rectangle* (left top right bottom) (sheet-region sheet)
      (let ((tr-x (- mirror-left left))
	    (tr-y (- mirror-top top))
	    (sc-x (/ (- mirror-right mirror-left)
		     (- right left)))
	    (sc-y (/ (- mirror-bottom mirror-top)
		     (- bottom top))))
	(when (or (/= sc-x 1.0)
		  (/= sc-y 1.0))
	  (warn "Mirror scaling ~S, ~S" (list tr-x tr-y sc-x sc-y) sheet))
	(setf (sheet-native-transformation sheet)
	      ;;--- Inline the composition to save some consing
	      (compose-transformations
		(make-translation-transformation tr-x tr-y)
		(make-scaling-transformation sc-x sc-y)))))))

;;; Mirror region protocol

;; Returns the coordinates of sheet's mirror in the coordinates of the
;; parent of the mirror
(defgeneric mirror-region* (port sheet)
  (declare (values left top right bottom)))

;; Returns the coordinates of sheet's mirror in the coordinates of the
;; mirror itself.  That is, it will return 0,0,WIDTH,HEIGHT for most
;; known window systems
(defgeneric mirror-inside-region* (port sheet)
  (declare (values left top right bottom)))

;; There also seems to be a need for a mirrors region in the
;; (sheet-mirror (sheet-parent sheet)) coordinate system
;; ie. the mirors parent is something not known to CLIM

(defgeneric set-mirror-region* (port sheet minx miny maxx maxy))

(defmethod mirror-region-updated ((port port) (sheet mirrored-sheet-mixin))
  (let* ((native-region (mirror-region port sheet))
	 (region (sheet-region sheet))
	 (parent (sheet-parent sheet))
	 (sheet-to-parent (sheet-transformation sheet))
	 (parent-to-native (sheet-native-transformation parent))
	 (region-changed-p nil)
	 (transformation-changed-p nil))
    (let* (;; Sheet in parent space
	   (oparent-region
	     (transform-region sheet-to-parent region))
	   ;; Mirror in parents coord space
	   (nparent-region
	     (untransform-region parent-to-native native-region)))
      (with-bounding-rectangle* (ominx ominy omaxx omaxy) oparent-region
	(with-bounding-rectangle* (nminx nminy nmaxx nmaxy) nparent-region
	  (let ((dx (- nminx ominx))
		(dy (- nminy ominy)))
	    (unless (and (zerop dx)
			 (zerop dy))
	      (setq sheet-to-parent (compose-translation-with-transformation
				      sheet-to-parent dx dy)
		    transformation-changed-p t)))
	  ;; If the width/height have changed then the region has changed
	  (let* ((owidth (- omaxx ominx))
		 (oheight (- omaxy ominy))
		 (nwidth (- nmaxx nminx))
		 (nheight (- nmaxy nminy))
		 (dw (- nwidth owidth))
		 (dh (- nheight oheight)))
	    (unless (and (zerop dw)
			 (zerop dh))
	      (multiple-value-bind (nw nh)
		  (untransform-distance sheet-to-parent nwidth nheight)
		(setf region 
		      (with-bounding-rectangle* (left top right bottom) region
			(declare (ignore right bottom))
			(make-bounding-rectangle left top 
						 (+ left nw) (+ top nh)))
		      region-changed-p t))))
	  (when transformation-changed-p
	    (setf (slot-value sheet 'transformation) sheet-to-parent))
	  (when region-changed-p
	    (setf (slot-value sheet 'region) region))
	  (when (or transformation-changed-p region-changed-p)
	    (update-mirror-transformation port sheet))
	  (when  region-changed-p
	    (note-sheet-region-changed sheet :port t))
	  (when transformation-changed-p
	    (note-sheet-transformation-changed sheet :port t)))))))

(defmethod handle-event ((sheet mirrored-sheet-mixin)
			 (event window-configuration-event))
  (mirror-region-updated (sheet-port sheet) sheet))
