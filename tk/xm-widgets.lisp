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
;; $fiHeader: xm-widgets.lisp,v 1.7 92/07/27 19:29:20 cer Exp $

(in-package :tk)

(defmethod make-widget ((w xm-gadget) 
			&rest args &key parent (managed t) (name "") &allow-other-keys)
  (remf args :managed)
  (remf args :name)
  (remf args :parent)
  (let ((class (class-of w)))
    (if managed
	(apply #'create-managed-widget name class parent args)
      (apply #'create-widget name class parent args))))

(defmethod make-widget ((w xm-dialog-shell) &rest args &key parent (name "") &allow-other-keys)
  (remf args :parent)
  (remf args :name)
  (apply #'create-popup-shell name (class-of w) parent args))

(defmethod make-widget ((w xm-menu-shell) &rest args &key parent (name "") &allow-other-keys)
  (remf args :parent)
  (remf args :name)
  (apply #'create-popup-shell name (class-of w) parent args))

(tk::add-resource-to-class (find-class 'vendor-shell)
			   (make-instance 'resource
					  :name :delete-response
					  :type 'tk::delete-response
					  :original-name 
					  (string-to-char*
					   "deleteResponse")))


(tk::add-resource-to-class (find-class 'vendor-shell)
			   (make-instance 'resource
					  :name :keyboard-focus-policy
					  :type 'tk::keyboard-focus-policy
					  :original-name 
					  (string-to-char*
					   "keyboardFocusPolicy")))


;; Moved here as to be after loading xm-funs.

;;-- This is a problem cos we dont know the number of items

(defconstant xm_string_default_char_set "")

(defmethod convert-resource-in (class (type (eql 'xm-string)) value)
  (and (not (zerop value))
       (with-ref-par ((string 0))
	 ;;--- I think we need to read the book about
	 ;;--- xm_string_get_l_to_r and make sure it works with multiple
	 ;;-- segment strings
	 (xm_string_get_l_to_r value xm_string_default_char_set string)
	 (char*-to-string (aref string 0)))))

(defmethod convert-resource-in ((parent t) (type (eql 'xm-string-table)) value)
  value)

(defun convert-xm-string-table-in (parent table n)
  (let ((r nil))
    (dotimes (i n (nreverse r))
      (push (convert-resource-in parent 'xm-string (x-arglist table i))
	    r))))

(defmethod convert-resource-out ((parent t) (type (eql 'xm-string)) value)
  (xm_string_create_l_to_r (string-to-char* value) (string-to-char* "")))

(defmethod convert-resource-out ((parent t) (type (eql 'xm-background-pixmap)) value)
  (etypecase value
    (pixmap
     (encode-pixmap nil value))))
