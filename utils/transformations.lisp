;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2007 Franz Inc, Oakland, CA - All rights reserved.
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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: transformations.lisp,v 2.7 2007/04/17 21:45:54 layer Exp $

(in-package :clim-utils)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;;; Generic Functions

(defgeneric transformation-equal (transform1 transform2))

(defgeneric identity-transformation-p (transform))
(defgeneric translation-transformation-p (transform))
(defgeneric invertible-transformation-p (transform))
(defgeneric reflection-transformation-p (transform))
(defgeneric rigid-transformation-p (transform))
(defgeneric even-scaling-transformation-p (transform))
(defgeneric scaling-transformation-p (transform))
(defgeneric rectilinear-transformation-p (transform))

(defgeneric compose-transformations (transform1 transform2))
(defgeneric invert-transformation (transform))
(defgeneric compose-translation-with-transformation (transform dx dy))
(defgeneric compose-scaling-with-transformation (transform mx my &optional origin))
(defgeneric compose-rotation-with-transformation (transform angle &optional origin))

(defgeneric transform-position (transform x y)
  #-aclpc (declare (values x y)))
(defgeneric untransform-position (transform x y)
  #-aclpc (declare (values x y)))

(defgeneric transform-distance (transform dx dy)
  #-aclpc (declare (values dx dy)))
(defgeneric untransform-distance (transform dx dy)
  #-aclpc (declare (values dx dy)))

(defgeneric transform-rectangle* (transform x1 y1 x2 y2)
  #-aclpc (declare (values x1 y1 x2 y2)))
(defgeneric untransform-rectangle* (transform x1 y1 x2 y2)
  #-aclpc (declare (values x1 y1 x2 y2)))


;;; Transformations

;;--- Watch out, we define methods on this protocol class!
(define-protocol-class transformation ())

;;; This class is not part of the advertised interface.
;;; It exists because EQL specializers are slow in PCL.
(defclass identity-transformation (transformation) ())

(defmethod make-load-form ((transform identity-transformation) #-aclpc &optional #-aclpc environment)
 #-aclpc (declare (ignore environment))
  '+identity-transformation+)

(defvar +identity-transformation+ (make-instance 'identity-transformation))


;;; Translation transformations

#+(or aclpc acl86win32) ; aclpc seems not to like using a class in its defclass
(defclass translation-transformation (transformation)
    ((inverse :reader invert-transformation)
     (tx :type single-float :initarg :tx)
     (ty :type single-float :initarg :ty)))
 
#-(or aclpc acl86win32) ; aclpc seems not to like using a class in its defclass
(defclass translation-transformation (transformation)
    ((inverse :type translation-transformation :reader invert-transformation)
     (tx :type single-float :initarg :tx)
     (ty :type single-float :initarg :ty)))

(define-constructor make-translation-transformation-1 translation-transformation
                    (tx ty)
  :tx tx :ty ty)

(defmethod print-object ((transform translation-transformation) stream)
  (print-unreadable-object (transform stream :type t :identity t)
    (with-slots (tx ty) transform
      (declare (type single-float tx ty))
      (format stream "(~D,~D)" tx ty))))

(defmethod make-load-form ((transform translation-transformation)
                             #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
  (with-slots (tx ty) transform
    (declare (type single-float tx ty))
    `(make-translation-transformation-1 ,tx ,ty)))


;;; General transformations

(defclass standard-transformation (transformation)
    ((inverse #-aclpc :type #-aclpc standard-transformation :reader invert-transformation)
     (mxx :type single-float :initarg :mxx)
     (mxy :type single-float :initarg :mxy)
     (myx :type single-float :initarg :myx)
     (myy :type single-float :initarg :myy)
     (tx :type single-float :initarg :tx)
     (ty :type single-float :initarg :ty)))

(define-constructor make-standard-transformation-1 standard-transformation
                    (mxx mxy myx myy tx ty)
  :mxx mxx :mxy mxy :myx myx :myy myy :tx tx :ty ty)

(defmethod make-load-form ((transform standard-transformation)
                             #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
  (with-slots (mxx mxy myx myy tx ty) transform
    (declare (type single-float mxx mxy myx myy tx ty))
    `(make-standard-transformation-1 ,mxx ,mxy ,myx ,myy ,tx ,ty)))

(defmethod print-object ((transform standard-transformation) stream)
  (print-unreadable-object (transform stream :type t :identity t)
    (with-slots (mxx mxy myx myy tx ty) transform
      (declare (single-float mxx mxy myx myy tx ty))
      (if (and (zerop mxy) (zerop myx))
          (format stream "scale (~D,~D) translate (~D,~D)" mxx myy tx ty)
          (format stream "[~D ~D ~D ~D] ~D ~D)" mxx mxy myx myy tx ty)))))


;;; Conditions

(define-condition transformation-error (error) ())

(define-condition transformation-underspecified (transformation-error)
  ((points :reader transformation-underspecified-points :initarg :points))
  (:report
    (lambda (condition stream)
      (format stream "You can't make a transformation from the three collinear points ~@
                     (~D,~D), (~D,~D), and (~D,~D)"
        (nth 0 (transformation-underspecified-points condition))
        (nth 1 (transformation-underspecified-points condition))
        (nth 2 (transformation-underspecified-points condition))
        (nth 3 (transformation-underspecified-points condition))
        (nth 4 (transformation-underspecified-points condition))
        (nth 5 (transformation-underspecified-points condition))))))

(define-condition reflection-underspecified (transformation-underspecified) ()
  (:report
    (lambda (condition stream)
      (format stream "You can't make a reflection from the two coincident points ~@
                     (~D,~D) and (~D,~D)"
        (nth 0 (transformation-underspecified-points condition))
        (nth 1 (transformation-underspecified-points condition))
        (nth 2 (transformation-underspecified-points condition))
        (nth 3 (transformation-underspecified-points condition))))))

(define-condition singular-transformation (transformation-error)
  ((transformation :reader singular-transformation-transformation :initarg :transformation))
  (:report
    (lambda (condition stream)
      (format stream "The transformation ~S is singular"
	(singular-transformation-transformation condition)))))


;;; Constructors

(defun-inline make-transformation-1 (mxx mxy myx myy tx ty)
  (declare (type single-float mxx mxy myx myy tx ty))
  (cond ((not (and (= mxx 1f0) (= mxy 0f0) (= myx 0f0) (= myy 1f0)))
	 (make-standard-transformation-1 mxx mxy myx myy tx ty))
	((not (and (= tx 0f0) (= ty 0f0)))
	 (make-translation-transformation-1 tx ty))
	(t +identity-transformation+)))

(defun make-transformation (mxx mxy myx myy tx ty)
  (declare (type real mxx mxy myx myy tx ty))
  #+Genera (declare lt:(side-effects simple reducible))
  (make-transformation-1 (float mxx 0f0) (float mxy 0f0)
			 (float myx 0f0) (float myy 0f0)
			 (float tx 0f0) (float ty 0f0)))

(defun make-3-point-transformation* (x1 y1 x2 y2 x3 y3
				     x1-image y1-image x2-image y2-image x3-image y3-image)
  (declare (type real x1 y1 x2 y2 x3 y3 
		      x1-image y1-image x2-image y2-image x3-image y3-image))
  #+Genera (declare lt:(side-effects simple reducible))
  (let* ((x1y2 (* x1 y2)) (x2y1 (* x2 y1))
	 (x2y3 (* x2 y3)) (x3y2 (* x3 y2))
	 (x3y1 (* x3 y1)) (x1y3 (* x1 y3))
	 (1/det (+ x1y2 (- x2y1) x2y3 (- x3y2) x3y1 (- x1y3))))
    (when (zerop 1/det)
      (error 'transformation-underspecified :points (list x1 y1 x2 y2 x3 y3)))
    (setq 1/det (/ 1/det))
    (let ((x2-x1 (- x2 x1)) (y1-y2 (- y1 y2))
	  (x3-x2 (- x3 x2)) (y2-y3 (- y2 y3))
	  (x1-x3 (- x1 x3)) (y3-y1 (- y3 y1))
	  (x1y2-x2y1 (- x1y2 x2y1))
	  (x2y3-x3y2 (- x2y3 x3y2))
	  (x3y1-x1y3 (- x3y1 x1y3)))
      (make-transformation
	(* (+ (* x1-image y2-y3) (* x2-image y3-y1) (* x3-image y1-y2)) 1/det)
	(* (+ (* x1-image x3-x2) (* x2-image x1-x3) (* x3-image x2-x1)) 1/det)
	(* (+ (* y1-image y2-y3) (* y2-image y3-y1) (* y3-image y1-y2)) 1/det)
	(* (+ (* y1-image x3-x2) (* y2-image x1-x3) (* y3-image x2-x1)) 1/det)
	(* (+ (* x1-image x2y3-x3y2) (* x2-image x3y1-x1y3) (* x3-image x1y2-x2y1)) 1/det)
	(* (+ (* y1-image x2y3-x3y2) (* y2-image x3y1-x1y3) (* y3-image x1y2-x2y1)) 1/det)))))

(defun make-3-point-transformation (point-1 point-2 point-3
				    point-1-image point-2-image point-3-image)
  #+Genera (declare lt:(side-effects simple reducible))
  (make-3-point-transformation* (point-x point-1) (point-y point-1)
				(point-x point-2) (point-y point-2)
				(point-x point-3) (point-y point-3)
				(point-x point-1-image) (point-y point-1-image)
				(point-x point-2-image) (point-y point-2-image)
				(point-x point-3-image) (point-y point-3-image)))

(defun make-translation-transformation (delta-x delta-y)
  (declare (type real delta-x delta-y))
  #+Genera (declare lt:(side-effects simple reducible))
  (let ((delta-x (float delta-x 0f0))
	(delta-y (float delta-y 0f0)))
    (declare (type single-float delta-x delta-y))
    (if (and (= delta-x 0f0) (= delta-y 0f0))
	+identity-transformation+
	(make-translation-transformation-1 delta-x delta-y))))

(defun make-rotation-transformation* (angle origin-x origin-y)
  (declare (type real angle origin-x origin-y))
  #+Genera (declare lt:(side-effects simple reducible))
  (let ((angle (mod (float angle 0f0) (float (* 2 pi) 0f0))))
    (declare (type single-float angle))
    (if (= angle 0f0)
	+identity-transformation+
	(let* ((c (cos angle))
	       (s (sin angle))
	       (1-cc (- 1f0 c))
	       (origin-x (float origin-x 0f0))
	       (origin-y (float origin-y 0f0)))
	  (declare (type single-float c s 1-cc origin-x origin-y))
	  (make-standard-transformation-1 c (- s) s c
					  (+ (* 1-cc origin-x) (* s origin-y))
					  (- (* 1-cc origin-y) (* s origin-x)))))))

(defun-inline make-rotation-transformation (angle &optional (origin nil origin-p))
  #+Genera (declare lt:(side-effects simple reducible))
  (if origin-p
      (make-rotation-transformation* angle (point-x origin) (point-y origin))
      (make-rotation-transformation* angle 0 0)))

(defun make-scaling-transformation* (mx my origin-x origin-y)
  (declare (type real mx my origin-x origin-y))
  #+Genera (declare lt:(side-effects simple reducible))
  (let ((mx (float mx 0f0))
	(my (float my 0f0)))
    (declare (type single-float mx my))
    (if (and (= mx 1f0) (= my 1f0))
	+identity-transformation+
	(make-standard-transformation-1 mx 0f0 0f0 my
					(* (- 1f0 mx) (float origin-x 0f0))
					(* (- 1f0 my) (float origin-y 0f0))))))

(defun-inline make-scaling-transformation (mx my &optional (origin nil origin-p))
  #+Genera (declare lt:(side-effects simple reducible))
  (if origin-p
      (make-scaling-transformation* mx my (point-x origin) (point-y origin))
      (make-scaling-transformation* mx my 0 0)))

(defun make-reflection-transformation* (x1 y1 x2 y2)
  (declare (type real x1 y1 x2 y2))
  #+Genera (declare lt:(side-effects simple reducible))
  (when (and (= x1 x2) (= y1 y2))
    (error 'reflection-underspecified :points (list x1 y1 x2 y2)))
  (let* ((x1 (float x1 0f0))
	 (y1 (float y1 0f0))
	 (x2 (float x2 0f0))
	 (y2 (float y2 0f0))
	 (nx (- y1 y2))
	 (ny (- x2 x1))
	 (nxx (* nx nx))
	 (nxy (- (* nx ny)))
	 (nyy (* ny ny)))
    (declare (type single-float x1 y1 x2 y2 nx ny nxx nxy nyy))
    (let ((norm (/ 2f0 (+ nxx nyy))))
      (declare (type single-float norm))
      (setq nxx (* nxx norm) nxy (* nxy norm) nyy (* nyy norm)))
    (make-standard-transformation-1 (- 1f0 nxx) nxy nxy (- 1f0 nyy)
				    (- (* nxx x1) (* nxy y1))
				    (- (* nyy y1) (* nxy x1)))))

(defun-inline make-reflection-transformation (point-1 point-2)
  #+Genera (declare lt:(side-effects simple reducible))
  (make-reflection-transformation* (point-x point-1) (point-y point-1)
				   (point-x point-2) (point-y point-2)))


;;; Predicates

(defmethod transformation-equal
	   ((transform1 identity-transformation) (transform2 identity-transformation))
  t)

(defmethod transformation-equal
	   ((transform1 translation-transformation) (transform2 translation-transformation))
  (with-slots ((tx1 tx) (ty1 ty)) transform1
    (declare (type single-float tx1 ty1))
    (with-slots ((tx2 tx) (ty2 ty)) transform2
      (declare (type single-float tx2 ty2))
      (and (= tx1 tx2) (= ty1 ty2)))))

(defmethod transformation-equal
	   ((transform1 standard-transformation) (transform2 standard-transformation))
  (with-slots ((mxx1 mxx) (mxy1 mxy) (myx1 myx) (myy1 myy) (tx1 tx) (ty1 ty)) transform1
    (declare (type single-float mxx1 mxy1 myx1 myy1 tx1 ty1))
    (with-slots ((mxx2 mxx) (mxy2 mxy) (myx2 myx) (myy2 myy) (tx2 tx) (ty2 ty)) transform2
      (declare (type single-float mxx2 mxy2 myx2 myy2 tx2 ty2))
      (and (= mxx1 mxx2)
	   (= mxy1 mxy2)
	   (= myx1 myx2)
	   (= myy1 myy2)
	   (= tx1 tx2)
	   (= ty1 ty2)))))

(defmethod transformation-equal
	   ((transform1 identity-transformation) (transform2 translation-transformation))
  nil)

(defmethod transformation-equal
	   ((transform1 identity-transformation) (transform2 standard-transformation))
  nil)

(defmethod transformation-equal
	   ((transform1 translation-transformation) (transform2 identity-transformation))
  nil)

(defmethod transformation-equal
	   ((transform1 translation-transformation) (transform2 standard-transformation))
  nil)

(defmethod transformation-equal
	   ((transform1 standard-transformation) (transform2 identity-transformation))
  nil)

(defmethod transformation-equal
	   ((transform1 standard-transformation) (transform2 translation-transformation))
  nil)


;;; Identity transformation?
(defmethod identity-transformation-p ((transform identity-transformation))
  t)

(defmethod identity-transformation-p ((transform standard-transformation))
  nil)

(defmethod identity-transformation-p ((transform translation-transformation))
  nil)


;;; Translation transformation?
(defmethod translation-transformation-p ((transform identity-transformation))
  t)

(defmethod translation-transformation-p ((transform translation-transformation))
  t)

(defmethod translation-transformation-p ((transform standard-transformation))
  nil)


;;; Invertible transformation?
(defmethod invertible-transformation-p ((transform identity-transformation))
  t)

(defmethod invertible-transformation-p ((transform translation-transformation))
  t)

(defmethod invertible-transformation-p ((transform standard-transformation))
  (with-slots (mxx mxy myx myy) transform
    (declare (type single-float mxx mxy myx myy))
    (not (zerop (- (* mxx myy) (* mxy myx))))))


;;; Reflection transformation?
(defmethod reflection-transformation-p ((transform identity-transformation))
  nil)

(defmethod reflection-transformation-p ((transform translation-transformation))
  nil)

(defmethod reflection-transformation-p ((transform standard-transformation))
  (with-slots (mxx mxy myx myy) transform
    (declare (type single-float mxx mxy myx myy))
    (minusp (- (* mxx myy) (* mxy myx)))))


;;; Rigid transformation?
(defmethod rigid-transformation-p ((transform identity-transformation))
  t)

(defmethod rigid-transformation-p ((transform translation-transformation))
  t)

(defmethod rigid-transformation-p ((transform standard-transformation))
  (with-slots (mxx mxy myx myy) transform
    (declare (type single-float mxx mxy myx myy))
    (and (= (- (* mxx myy) (* mxy myx)) 1f0)
	 (= (+ (* mxx mxy) (* myx myy)) 1f0)
	 (= (+ (expt mxx 2) (expt myx 2)) 1f0))))


;;; Even scaling transformation?
(defmethod even-scaling-transformation-p ((transform identity-transformation))
  t)

(defmethod even-scaling-transformation-p ((transform translation-transformation))
  t)

(defmethod even-scaling-transformation-p ((transform standard-transformation))
  (with-slots (mxx mxy myx myy) transform
    (declare (type single-float mxx mxy myx myy))
    (and (= mxy 0f0) (= myx 0f0)
	 (= mxx myy))))


;;; Scaling transformation?
(defmethod scaling-transformation-p ((transform identity-transformation))
  t)

(defmethod scaling-transformation-p ((transform translation-transformation))
  t)

(defmethod scaling-transformation-p ((transform standard-transformation))
  (with-slots (mxy myx) transform
    (declare (type single-float mxy myx))
    (and (= mxy 0f0) (= myx 0f0))))


;;; Rectilinear transformation?
(defmethod rectilinear-transformation-p ((transform identity-transformation))
  t)

(defmethod rectilinear-transformation-p ((transform translation-transformation))
  t)

(defmethod rectilinear-transformation-p ((transform standard-transformation))
  (with-slots (mxx mxy myx myy) transform
    (declare (type single-float mxx mxy myx myy))
    (or (and (= mxy 0f0) (= myx 0f0))
	(and (= mxx 0f0) (= myy 0f0)))))


;;; Complex constructors

(defmethod invert-transformation ((transform identity-transformation))
  transform)

(defmethod slot-unbound (class (transform translation-transformation) (slot (eql 'inverse)))
  (declare (ignore class))
  (with-slots (tx ty inverse) transform
    (declare (type single-float tx ty))
    (let ((i (make-translation-transformation-1 (- tx) (- ty))))
      (setf (slot-value i 'inverse) transform)
      (setf inverse i))))

(defmethod slot-unbound (class (transform standard-transformation) (slot (eql 'inverse)))
  (declare (ignore class))
  (with-slots (mxx mxy myx myy tx ty inverse) transform
    (declare (type single-float mxx mxy myx myy tx ty))
    (let ((1/det (- (* mxx myy) (* mxy myx))))
      (when (zerop 1/det)
	(error 'singular-transformation :transformation transform))
      (setq 1/det (/ 1/det))
      (let ((i (make-standard-transformation-1 (* myy 1/det)
					       (* (- mxy) 1/det)
					       (* (- myx) 1/det)
					       (* mxx 1/det)
					       (* (- (* mxy ty) (* myy tx)) 1/det)
					       (* (- (* myx tx) (* mxx ty)) 1/det))))
	;; Link the transformation to its inverse
	(setf (slot-value i 'inverse) transform)
	(setf inverse i)))))


;;; Composition operators

(defmethod compose-transformations
	   ((transform1 identity-transformation) (transform2 transformation))
  transform2)

(defmethod compose-transformations
	   ((transform1 transformation) (transform2 identity-transformation))
  transform1)

(defmethod compose-transformations
	   ((transform1 translation-transformation) (transform2 translation-transformation))
  (with-slots ((tx1 tx) (ty1 ty)) transform1
    (declare (type single-float tx1 ty1))
    (with-slots ((tx2 tx) (ty2 ty)) transform2
      (declare (type single-float tx2 ty2))
      (let ((tx (+ tx1 tx2))
	    (ty (+ ty1 ty2)))
	(declare (type single-float tx ty))
	(if (and (= tx 0f0) (= ty 0f0))
	    +identity-transformation+
	    (make-translation-transformation-1 tx ty))))))

(defmethod compose-transformations
	   ((transform1 standard-transformation) (transform2 standard-transformation))
  (with-slots ((mxx1 mxx) (mxy1 mxy) (myx1 myx) (myy1 myy) (tx1 tx) (ty1 ty)) transform1
    (declare (type single-float mxx1 mxy1 myx1 myy1 tx1 ty1))
    (with-slots ((mxx2 mxx) (mxy2 mxy) (myx2 myx) (myy2 myy) (tx2 tx) (ty2 ty)) transform2
      (declare (type single-float mxx2 mxy2 myx2 myy2 tx2 ty2))
      (make-transformation-1 (+ (* mxx1 mxx2) (* mxy1 myx2))
			     (+ (* mxx1 mxy2) (* mxy1 myy2))
			     (+ (* myx1 mxx2) (* myy1 myx2))
			     (+ (* myx1 mxy2) (* myy1 myy2))
			     (+ tx1 (* mxx1 tx2) (* mxy1 ty2))
			     (+ ty1 (* myx1 tx2) (* myy1 ty2))))))

(defmethod compose-transformations
	   ((transform1 standard-transformation) (transform2 translation-transformation))
  (with-slots ((mxx1 mxx) (mxy1 mxy) (myx1 myx) (myy1 myy) (tx1 tx) (ty1 ty)) transform1
    (declare (type single-float mxx1 mxy1 myx1 myy1 tx1 ty1))
    (with-slots ((tx2 tx) (ty2 ty)) transform2
      (make-standard-transformation-1 mxx1 mxy1 myx1 myy1
				      (+ tx1 (* mxx1 tx2) (* mxy1 ty2))
				      (+ ty1 (* myx1 tx2) (* myy1 ty2))))))

(defmethod compose-transformations
	   ((transform1 translation-transformation) (transform2 standard-transformation))
  (with-slots ((tx1 tx) (ty1 ty)) transform1
    (declare (type single-float tx1 ty1))
    (with-slots ((mxx2 mxx) (mxy2 mxy) (myx2 myx) (myy2 myy) (tx2 tx) (ty2 ty)) transform2
      (declare (type single-float mxx2 mxy2 myx2 myy2 tx2 ty2))
      (make-standard-transformation-1 mxx2 mxy2 myx2 myy2 (+ tx1 tx2) (+ ty1 ty2)))))


;;; Translation composition operators

(defmethod compose-translation-with-transformation
	   ((transform identity-transformation) dx dy)
  (declare (type real dx dy))
  (let ((dx (float dx 0f0))
	(dy (float dy 0f0)))
    (declare (type single-float dx dy))
    (if (and (= dx 0f0) (= dy 0f0))
	transform
	(make-translation-transformation-1 dx dy))))

(defmethod compose-translation-with-transformation
	   ((transform translation-transformation) dx dy)
  (declare (type real dx dy))
  (let ((dx (float dx 0f0))
	(dy (float dy 0f0)))
    (declare (type single-float dx dy))
    (if (and (= dx 0f0) (= dy 0f0))
	transform
	(with-slots (tx ty) transform
	  (declare (type single-float tx ty))
	  (let ((tx (+ dx tx))
		(ty (+ dy ty)))
	    (declare (type single-float tx ty))
	    (if (and (= tx 0f0) (= ty 0f0))
		+identity-transformation+
		(make-translation-transformation-1 tx ty)))))))

(defmethod compose-translation-with-transformation
	   ((transform standard-transformation) dx dy)
  (declare (type real dx dy))
  (let ((dx (float dx 0f0))
	(dy (float dy 0f0)))
    (declare (type single-float dx dy))
    (if (and (= dx 0f0) (= dy 0f0))
	transform
	(with-slots (mxx mxy myx myy tx ty) transform
	  (declare (type single-float mxx mxy myx myy tx ty))
	  (make-standard-transformation-1 mxx mxy myx myy
					  (+ tx (* mxx dx) (* mxy dy))
					  (+ ty (* myx dx) (* myy dy)))))))



;;; Scaling composition operators

(defmethod compose-scaling-with-transformation
	   ((transform identity-transformation) mx my &optional (origin nil origin-p))
  (declare (type real mx my))
  (if origin-p
      (make-scaling-transformation mx my origin)
      (make-scaling-transformation mx my)))

(defmethod compose-scaling-with-transformation
	   ((transform translation-transformation) mx my &optional (origin nil origin-p))
  (declare (type real mx my))
  (let ((mx (float mx 0f0))
	(my (float my 0f0)))
    (declare (type single-float mx my))
    (if (and (= mx 1f0) (= my 1f0))
	transform
	(with-slots (tx ty) transform
	  (declare (type single-float tx ty))
	  (if origin-p
	      (make-transformation-1 mx 0f0 0f0 my
				     (+ tx (* (- 1f0 mx) (float (point-x origin) 0f0)))
				     (+ ty (* (- 1f0 my) (float (point-y origin) 0f0))))
	      (make-transformation-1 mx 0f0 0f0 my tx ty))))))

(defmethod compose-scaling-with-transformation
	   ((transform standard-transformation) mx my &optional (origin nil origin-p))
  (declare (type real mx my))
  (let ((mx (float mx 0f0))
	(my (float my 0f0)))
    (declare (type single-float mx my))
    (if (and (= mx 1f0) (= my 1f0))
	transform
	(with-slots ((mxx1 mxx) (mxy1 mxy) (myx1 myx) (myy1 myy) (tx1 tx) (ty1 ty)) transform
	  (declare (type single-float mxx1 mxy1 myx1 myy1 tx1 ty1))
	  (if origin-p
	      (let ((tx2 (* (- 1f0 mx) (float (point-x origin) 0f0)))
		    (ty2 (* (- 1f0 my) (float (point-y origin) 0f0))))
		(declare (type single-float tx2 ty2))
		(make-transformation-1 (* mxx1 mx) (* mxy1 my) (* myx1 mx) (* myy1 my)
				       (+ tx1 (* mxx1 tx2) (* mxy1 ty2))
				       (+ ty1 (* myx1 tx2) (* myy1 ty2))))
	      (make-transformation-1 (* mxx1 mx) (* mxy1 my) (* myx1 mx) (* myy1 my)
				     tx1 ty1))))))


;;; Rotation composition operators

(defmethod compose-rotation-with-transformation
	   ((transform identity-transformation) angle &optional (origin nil origin-p))
  (declare (type real angle))
  (if origin-p
      (make-rotation-transformation angle origin)
      (make-rotation-transformation angle)))

(defmethod compose-rotation-with-transformation
	   ((transform translation-transformation) angle &optional (origin nil origin-p))
  (let ((angle (mod (float angle 0f0) (float (* 2 pi) 0f0))))
    (declare (type single-float angle))
    (if (= angle 0f0)
	transform
	(with-slots ((tx1 tx) (ty1 ty)) transform
	  (declare (type single-float tx1 ty1))
	  (let* ((c (cos angle))
		 (s (sin angle))
		 (1-cc (- 1f0 c)))
	    (declare (type single-float c s 1-cc))
	    (if origin-p
		(let ((origin-x (point-x origin))
		      (origin-y (point-y origin)))
		  (declare (type single-float origin-x origin-y))
		  (make-standard-transformation-1
		    c (- s) s c
		    (+ tx1 (* 1-cc origin-x) (* s origin-y))
		    (+ ty1 (- (* 1-cc origin-y) (* s origin-x)))))
		(make-standard-transformation-1 c (- s) s c tx1 ty1)))))))

(defmethod compose-rotation-with-transformation
	   ((transform standard-transformation) angle &optional (origin nil origin-p))
  (let ((angle (mod (float angle 0f0) (float (* 2 pi) 0f0))))
    (declare (type single-float angle))
    (if (= angle 0f0)
	transform
        (with-slots ((mxx1 mxx) (mxy1 mxy) (myx1 myx) (myy1 myy) (tx1 tx) (ty1 ty))
	            transform
	  (declare (type single-float mxx1 mxy1 myx1 myy1 tx1 ty1))
	  (let* ((c (cos angle))
		 (s (sin angle))
		 (1-cc (- 1f0 c))
		 (mxx (+ (* mxx1 c) (* mxy1 s)))
		 (mxy (- (* mxy1 c) (* mxx1 s)))
		 (myx (+ (* myx1 c) (* myy1 s)))
		 (myy (- (* myy1 c) (* myx1 s))))
	    (declare (type single-float c s 1-cc mxx mxy myx myy))
	    (if origin-p
		(let ((origin-x (point-x origin))
		      (origin-y (point-y origin)))
		  (declare (type single-float origin-x origin-y))
		  (let ((tx2 (+ (* 1-cc origin-x) (* s origin-y)))
			(ty2 (- (* 1-cc origin-y) (* s origin-x))))
		    (declare (type single-float tx2 ty2))
		    (make-transformation-1 mxx mxy myx myy
					   (+ tx1 (* mxx1 tx2) (* mxy1 ty2))
					   (+ ty1 (* myx1 tx2) (* myy1 ty2)))))
	        (make-transformation-1 mxx mxy myx myy tx1 ty1)))))))


;;; Transforming and untransforming of points

(defmethod transform-position ((transform identity-transformation) x y)
  (declare (type real x y))
  (values (coordinate x) (coordinate y)))

;; This macro is specially designed for the transformation functions
;; and shouldn't be applied elsewhere without studying the context
;; of how it is currently used.  It gets rid of a few million bytes
;; of garbage without disturbing the status quo too much.  JPM 6/98.
(defmacro quickadd (a b)
  `(if (typep ,a 'fixnum)
       (the fixnum (+ (the fixnum ,a) (the fixnum (round ,b))))
     (the short-float (+ (the short-float (coerce ,a 'short-float))
			 (the short-float ,b)))))

(defmethod transform-position ((transform translation-transformation) x y)
  #+original
  (declare (type real x y))
  #+original
  (let ((x (coordinate x))
	(y (coordinate y)))
    (declare (type coordinate x y))
    (with-slots (tx ty) transform
      (declare (type single-float tx ty))
      (values (coordinate (+ x tx))
	      (coordinate (+ y ty)))))
  (declare (type real x y)
	   (optimize (speed 3) (safety 0)))
  (with-slots (tx ty) transform
    (declare (type short-float tx ty))
    (values (quickadd x tx)
	    (quickadd y ty))))

;; coordinates *would* be single floats if we compiled with feature use-float-coordinates
;; ... why *don't* we?
(defmethod transform-position ((transform standard-transformation) x y)
;;  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type real x y))
  (let ((x #-ignore (float x 0f0) #+ignore (coordinate x))
	(y #-ignore (float y 0f0) #+ignore (coordinate y)))
    (declare (type #-ignore single-float #+ignore coordinate x y))
    (with-slots (mxx mxy myx myy tx ty) transform
      (declare (type single-float mxx mxy myx myy tx ty))
      (values (the #-ignore single-float #+ignore coordinate (+ (* x mxx) (* y mxy) tx))
	      (the #-ignore single-float #+ignore coordinate (+ (* x myx) (* y myy) ty))))))

(defmethod untransform-position ((transform identity-transformation) x y)
  (declare (type real x y))
  (values (coordinate x) (coordinate y)))

(defmethod untransform-position ((transform translation-transformation) x y)
  (declare (type real x y))
  (let ((x (coordinate x))
	(y (coordinate y)))
    (declare (type coordinate x y))
    (with-slots (tx ty) transform
      (declare (type single-float tx ty))
      (values (coordinate (- x tx)) (coordinate (- y ty))))))

(defmethod untransform-position ((transform standard-transformation) x y)
  (declare (type real x y))
  (transform-position (slot-value transform 'inverse) x y))


;;; Transforming and untransforming of distances

(defmethod transform-distance ((transform identity-transformation) dx dy)
  (declare (type real dx dy))
  (values (coordinate dx) (coordinate dy)))

(defmethod transform-distance ((transform translation-transformation) dx dy)
  (declare (type real dx dy))
  (values (coordinate dx) (coordinate dy)))

(defmethod transform-distance ((transform standard-transformation) dx dy)
  (declare (type real dx dy))
  (let ((dx (coordinate dx))
	(dy (coordinate dy)))
    (declare (type coordinate dx dy))
    (with-slots (mxx mxy myx myy tx ty) transform
      (declare (type single-float mxx mxy myx myy tx ty))
      (values (coordinate (+ (* dx mxx) (* dy mxy)))
	      (coordinate (+ (* dx myx) (* dy myy)))))))


(defmethod untransform-distance ((transform identity-transformation) dx dy)
  (declare (type real dx dy))
  (values (coordinate dx) (coordinate dy)))

(defmethod untransform-distance ((transform translation-transformation) dx dy)
  (declare (type real dx dy))
  (values (coordinate dx) (coordinate dy)))

(defmethod untransform-distance ((transform standard-transformation) dx dy)
  (declare (type real dx dy))
  (transform-distance (slot-value transform 'inverse) dx dy))


;;; Transforming and untransforming of points

(defmethod transform-rectangle* ((transform identity-transformation) x1 y1 x2 y2)
  (declare (type real x1 y1 x2 y2))
  (values (coordinate x1) (coordinate y1)
	  (coordinate x2) (coordinate y2)))

(defmethod transform-rectangle* ((transform translation-transformation) x1 y1 x2 y2)
  (declare (type real x1 y1 x2 y2)
	   (optimize (speed 3) (safety 0)))
  (let ((x1 (coordinate x1))
	(y1 (coordinate y1))
	(x2 (coordinate x2))
	(y2 (coordinate y2)))
    (declare (type coordinate x1 y1 x2 y2))
    (with-slots (tx ty) transform
      (declare (type single-float tx ty))
      #+original
      (values (coordinate (+ x1 tx)) (coordinate (+ y1 ty))
	      (coordinate (+ x2 tx)) (coordinate (+ y2 ty)))
      (values (quickadd x1 tx)
	      (quickadd y1 ty)
	      (quickadd x2 tx)
	      (quickadd y2 ty)))))

(defmethod transform-rectangle* ((transform standard-transformation) x1 y1 x2 y2)
  (declare (type real x1 y1 x2 y2))
  (assert (rectilinear-transformation-p transform) (transform)
	  "Bounding rectangles can only be transformed by a rectilinear transformation")
  (let ((x1 (coordinate x1))
	(y1 (coordinate y1))
	(x2 (coordinate x2))
	(y2 (coordinate y2)))
    (declare (type coordinate x1 y1 x2 y2))
    (with-slots (mxx mxy myx myy tx ty) transform
      (declare (type single-float mxx mxy myx myy tx ty))
      (values (coordinate (+ (* x1 mxx) (* y1 mxy) tx))
	      (coordinate (+ (* x1 myx) (* y1 myy) ty))
	      (coordinate (+ (* x2 mxx) (* y2 mxy) tx))
	      (coordinate (+ (* x2 myx) (* y2 myy) ty))))))


(defmethod untransform-rectangle* ((transform identity-transformation) x1 y1 x2 y2)
  (declare (type real x1 y1 x2 y2))
  (values (coordinate x1) (coordinate y1)
	  (coordinate x2) (coordinate y2)))

(defmethod untransform-rectangle* ((transform translation-transformation) x1 y1 x2 y2)
  (declare (type real x1 y1 x2 y2))
  (let ((x1 (coordinate x1))
	(y1 (coordinate y1))
	(x2 (coordinate x2))
	(y2 (coordinate y2)))
    (declare (type coordinate x1 y1 x2 y2))
    (with-slots (tx ty) transform
      (declare (type single-float tx ty))
      (values (coordinate (- x1 tx)) (coordinate (- y1 ty))
	      (coordinate (- x2 tx)) (coordinate (- y2 ty))))))

(defmethod untransform-rectangle* ((transform standard-transformation) x1 y1 x2 y2)
  (declare (type real x1 y1 x2 y2))
  (transform-rectangle* (slot-value transform 'inverse) x1 y1 x2 y2))
