;; -*- mode: common-lisp; package: tk -*-
;;
;;				-[]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, CA  All rights reserved.
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
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: meta-tk.lisp,v 1.4 92/01/31 14:54:59 cer Exp $


(in-package :tk)


(defclass xt-class (handle-class standard-class)
  (
   (handle :initform nil)
   (entry-point :initarg :entry-point :reader class-entry-point)
   (resources :initform nil :initarg :resources :reader class-resources)
   (constraint-resources :initform nil
			 :initarg :constraints
			 :reader class-constraint-resources)
   
   (direct-resources :initform nil :initarg :direct-resources :reader class-direct-resources)
   (constraint-resources :initform nil
			 :initarg :direct-constraints
			 :reader class-direct-constraint-resources)))


(defmethod class-handle ((class xt-class))
  (unless (clos::class-finalized-p class)
    (clos::finalize-inheritance class))
  (dolist (c (clos::class-precedence-list class) 
	     (error "Cannot get handle for class" class))
    (when (and (typep c 'xt-class)
	       (object-handle c))
      (return (object-handle c)))))

(defmethod clos::finalize-inheritance :after ((class xt-class))
  (let ((super (car (clos::class-direct-superclasses class))))
    (when (typep super 'xt-class)
      (unless (slot-value class 'resources)
	(setf (slot-value class 'resources)
	      (slot-value super 'resources)))
      (unless (slot-value class 'constraint-resources)
	(setf (slot-value class 'constraint-resources)
	  (slot-value super 'constraint-resources))))))
