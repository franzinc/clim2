;; -*- mode: common-lisp; package: silica -*-
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
;; $fiHeader$

(in-package :silica)


;;;; sheet-device-transformation - transformation from sheets
;;;; coordinate space to its mirror

;;;; sheet-device-region - clipping region in mirror coordinate space

;;; Mirrors

(defgeneric sheet-mirror (sheet))

(defgeneric sheet-native-transformation (sheet)
  )


(defgeneric sheet-native-region (sheet)
  )

(defmethod sheet-direct-mirror ((sheet t))
  nil)

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

(defgeneric sheet-device-transformation (sheet)
  )

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



;;; Device region
  

(defmethod sheet-device-region ((sheet mirrored-sheet-mixin))
  (sheet-native-region sheet))

(defmethod sheet-device-region (sheet)
  (region-intersection
   (transform-region 
    (sheet-transformation sheet)
    (sheet-region sheet))
   (sheet-device-region (sheet-parent sheet))))

;;;; Mirror region stuff


(defmethod sheet-actual-native-edges (sheet)
  ;; Returns the sheet-region in the parents native coordinate space
  (let* ((port (port sheet))
	 (region (sheet-region sheet))
	 (sheet-to-parent (sheet-transformation sheet))
	 (parent (sheet-parent sheet))
	 (parent-to-native (sheet-device-transformation parent)))
    (with-bounding-rectangle*
     (a b c d)
     (transform-region 
      parent-to-native
      (transform-region sheet-to-parent region))
     (values a b c d))))

(defgeneric mirror-region (port sheet)
  ;; Returns the regions mirror in the mirror coordinate space
  )

(defgeneric mirror-edges* (port sheet)
  ;; returns the mirror's region in its parents coordinate space
  )

(defgeneric set-mirror-edges* (port sheet minx miny maxx maxy)
  ;; sets the mirror's region in its parents coordinate space
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defgeneric realize-mirror (port sheet)
  )
  
(defmethod sheet-mirror ((sheet sheet))
  (sheet-direct-mirror
   (sheet-mirrored-ancestor sheet)))

(defgeneric sheet-mirrored-ancestor (sheet)
  (:method ((sheet sheet))
	   (cond ((sheet-direct-mirror sheet)
		  sheet)
		 ((null (sheet-parent sheet))
		  (error
		   "error in sheet mirrored ancestor: ~S"
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

(defmethod note-sheet-grafted :after ((sheet mirrored-sheet-mixin))
  (realize-mirror (port sheet) sheet))

(defmethod note-sheet-enabled ((sheet mirrored-sheet-mixin))
  (call-next-method)
  (when (sheet-direct-mirror sheet)
    (enable-mirror (port sheet) sheet)))

(defmethod note-sheet-disabled ((sheet mirrored-sheet-mixin))
  (call-next-method)
  (when (sheet-direct-mirror sheet)
    (disable-mirror (port sheet) sheet)))


(defgeneric invalidate-cached-transformations (sheet)
  (:method (sheet) nil)
  (:method :after ((sheet sheet-parent-mixin))
	   (mapc #'invalidate-cached-transformations (sheet-children sheet))))
	     

(defgeneric invalidate-cached-regions (sheet)
  (:method (sheet) nil)
  (:method :after ((sheet sheet-parent-mixin))
	   (mapc #'invalidate-cached-regions (sheet-children sheet))))


(warn "This sucks big")

(defmethod invalidate-cached-transformations :after ((sheet mirrored-sheet-mixin))
  (when (sheet-direct-mirror sheet)
    (update-mirror-region (port sheet) sheet)))

(defmethod note-sheet-region-changed :after ((sheet mirrored-sheet-mixin) &key port)
  (when (sheet-direct-mirror sheet)
    (unless port
      (update-mirror-region (port sheet) sheet))))


(defmethod note-sheet-transformation-changed :after ((sheet mirrored-sheet-mixin) &key port)
  (when (sheet-direct-mirror sheet)
    (unless port
      (update-mirror-region (port sheet) sheet))))



;;; There is some confusion here about mirror-inside-edges*
;;; This actual returns coordinates in the mirrors parents space
;;; rather than

(defmethod update-mirror-region ((port port) (sheet mirrored-sheet-mixin))
  ;; If the sheet-region is changed this updates the mirrors region accordingly
  (multiple-value-bind
      ;; Coordinates in parent space
      (target-left target-top target-right target-bottom)
      (sheet-actual-native-edges sheet)
    (multiple-value-bind
	(actual-left actual-top actual-right actual-bottom)
	(mirror-native-edges* (port sheet) sheet)
      (unless nil
	(set-sheet-mirror-edges*
	 (port sheet) sheet
	 target-left target-top target-right target-bottom))))
  (update-mirror-transformation port sheet))

(defmethod update-mirror-transformation ((port port) (sheet mirrored-sheet-mixin))
  ;; Compute transformation from sheet-region to mirror cordinates. I
  ;; would imagine that a lot of the time this would be identity
  (multiple-value-bind
      (mirror-left mirror-top mirror-right mirror-bottom)
      (mirror-inside-edges* port sheet)
    (with-bounding-rectangle* 
     (left top right bottom) (sheet-region sheet)
     (let ((tr-x (- mirror-left left))
	   (tr-y (- mirror-top top))
	   (sc-x (/ (- mirror-right mirror-left)
		    (- right left)))
	   (sc-y (/ (- mirror-bottom mirror-top)
		    (- bottom top))))
       (when (or (/= sc-x 1.0)
		 (/= sc-y 1.0))
	 (warn "mirror scalling ~S, ~S" (list tr-x tr-y sc-x sc-y) sheet))
       (setf (sheet-native-transformation sheet)
	     (compose-transformations
	      (make-translation-transformation tr-x tr-y)
	      (make-scaling-transformation sc-x sc-y)))))))

(defgeneric sheet-device-region (sheet)
  )

;;; Mirror region protocol

(defgeneric mirror-region* (port sheet)
  ;;; In parents coordinates
  )

(defgeneric mirror-inside-region* (port sheet)
  ;;; In mirrors own coordinates
  )

;; There also seems to be a need for a mirrors region in the
;; (sheet-mirror (sheet-parent sheet)) coordinate system
;; ie. the mirors parent is something not known to CLIM

(defgeneric set-mirror-region* (port sheet minx miny maxx maxy)
  )

(defmethod mirror-region-updated ((port port) (sheet mirrored-sheet-mixin))
  (let* ((native-region (mirror-region port sheet))
	 (region (sheet-region sheet))
	 (parent (sheet-parent sheet))
	 (sheet-to-parent (sheet-transformation sheet))
	 (parent-to-native (sheet-native-transformation parent))
	 (region-changed-p nil)
	 (transformation-changed-p nil))
	 
    (let* (
	   
	   ;;  Sheet in parent space
	   
	   (oparent-region
	    (transform-region sheet-to-parent region))
	   
	   ;;  Mirror in parents coord space
	   
	   (nparent-region
	    (untransform-region parent-to-native native-region))
	   )
      #+ignore
      (format t "oparent ~S, nparent ~S~%"
	      oparent-region nparent-region)
      
      (with-bounding-rectangle*
       (ominx ominy omaxx omaxy) oparent-region
       (with-bounding-rectangle* 
	(nminx nminy nmaxx nmaxy) nparent-region
	
	(let ((dx (- nminx ominx))
	      (dy (- nminy ominy)))

	  #+ignore
	  (format t "dx ~d, dy ~d~%"
		  dx dy)
	  
	  (unless (and (zerop dx)
		       (zerop dy))
	    (setq sheet-to-parent (compose-translation-transformation
				   sheet-to-parent dx dy)
		  transformation-changed-p t)))
	
	      
	;; If the width/height have changed then the region has changed
	
	(let* ((owidth (- omaxx ominx))
	       (oheight (- omaxy ominy))
	       (nwidth (- nmaxx nminx))
	       (nheight (- nmaxy nminy))
	       (dw (- nwidth owidth))
	       (dh (- nheight oheight)))
	  
	  #+ignore
	  (format t "dw ~d, dh ~d~%"
		  dw dh)
	  
	  (unless (and (zerop dw)
		       (zerop dh))
	    (multiple-value-bind
		(nw nh)
		(untransform-distance sheet-to-parent nwidth nheight)
	      (setf region 
		    (with-bounding-rectangle* 
		     (a b c d)
		     region
		     (declare (ignore c d))
		     (make-bounding-rectangle a 
					      b 
					      (+ a nw)
					      (+ b nh)))
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

(defmethod handle-event ((sheet mirrored-sheet-mixin) (event window-configuration-event))
  (mirror-region-updated (port sheet) sheet))




