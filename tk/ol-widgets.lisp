;; -*- mode: common-lisp; package: xt -*-
;;
;;				-[]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1992 Franz Inc, Berkeley, CA  All rights reserved.
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
;; $fiHeader: ol-widgets.lisp,v 1.3 92/07/27 19:29:14 cer Exp $

(in-package :xt)


(defmethod make-widget ((w menu-shell) &rest args &key parent (name "") &allow-other-keys)
  (remf args :parent)
  (remf args :name)
  (apply #'create-popup-shell name (class-of w) parent args))


(defmethod make-widget ((w transient-shell) &rest args &key parent (name "") &allow-other-keys)
  (remf args :parent)
  (remf args :name)
  (apply #'create-popup-shell name (class-of w) parent args))


(tk::add-resource-to-class (find-class 'menu-shell)
			   (make-instance 'resource
					  :name :menu-pane
					  :type 'tk::widget
					  :original-name 
					  (string-to-char*
					   "menuPane")))


(tk::add-resource-to-class (find-class 'text-edit)
			   (make-instance 'resource
					  :name :source
					  :type 'string
					  :original-name 
					  (string-to-char* "source")))


(tk::add-resource-to-class (find-class 'ol-list)
			   (make-instance 'resource
					  :name :appl-add-item
					  :type 'function
					  :original-name 
					  (string-to-char*
					   "applAddItem")))

(tk::add-resource-to-class (find-class 'ol-list)
			   (make-instance 'resource
					  :name :appl-touch-item
					  :type 'function
					  :original-name 
					  (string-to-char* "applTouchItem")))

(tk::add-resource-to-class (find-class 'ol-list)
			   (make-instance 'resource
					  :name :appl-delete-item
					  :type 'function
					  :original-name 
					  (string-to-char*
					   "applDeleteItem")))

(tk::add-resource-to-class (find-class 'ol-list)
			   (make-instance 'resource
					  :name :view-height
					  :type 'dimension
					  :original-name 
					  (string-to-char*
					   "viewHeight")))

(tk::add-resource-to-class (find-class 'ol-list)
			   (make-instance 'resource
					  :name :recompute-width
					  :type 'boolean
					  :original-name 
					  (string-to-char* "recomputeWidth")))
