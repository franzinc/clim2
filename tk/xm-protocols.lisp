;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-1998 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; $Id: xm-protocols.lisp,v 1.15.22.2 1998/07/06 23:10:15 layer Exp $

(in-package :tk)

(defmethod spread-callback-data (widget call-data (type (eql :protocol-callback)))
  (declare (ignore widget call-data))
 (values))

(defun add-protocol-callback (shell property protocol function &rest args)
  (let ((type :protocol-callback))
    (xm_add_protocol_callback
     shell
     (if (integerp property) property (xm-intern-atom shell property))
     (if (integerp protocol) protocol (xm-intern-atom shell protocol))
     (or *callback-handler-address*
	 (setq *callback-handler-address* (register-function 'callback-handler)))
     (caar (push
	    (list (new-callback-id) (cons function args) type)
	    (widget-callback-data shell))))))

(defun add-wm-protocol-callback (shell protocol fn &rest args)
  (apply
   #'add-protocol-callback
   shell
   "WM_PROTOCOLS"
   (case protocol
     (:wm-delete-window "WM_DELETE_WINDOW")
     (:wm-save-your-self "WM_SAVE_YOUR_SELF")
     (t protocol))
   fn
   args))

(defun xm-intern-atom (shell name &optional only-if-exists)
  (xm_intern_atom
   (object-display shell)
   (lisp-string-to-string8 name)
   (if only-if-exists 1 0)))
