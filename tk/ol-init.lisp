;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2004 Franz Inc, Oakland, CA - All rights reserved.
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
;; $Id: ol-init.lisp,v 2.6 2005/12/08 21:25:46 layer Exp $

(in-package :tk)

(defun ol-initialize ()
  (ol_toolkit_initialize))

(defun-c-callable ol-error-handler ((message :unsigned-natural))
  (let ((*error-output* excl:*initial-terminal-io*))
    (error "OLit: ~A" (excl:native-to-string message))))


(defun-c-callable ol-warning-handler ((message :unsigned-natural))
  (let ((*error-output* excl:*initial-terminal-io*))
    (warn "OLit: ~A" (excl:native-to-string message))))

(defun-c-callable ol-error-va-handler ((message :unsigned-natural))
  (let ((*error-output* excl:*initial-terminal-io*))
    (error "OLit: ~A" (excl:native-to-string message))))


(defun-c-callable ol-warning-va-handler ((message :unsigned-natural))
  (let ((*error-output* excl:*initial-terminal-io*))
    (warn "OLit: ~A" (excl:native-to-string message))))


(defun install-ol-error-handlers ()
  (ol_set_warning_handler (register-foreign-callable 'ol-warning-handler))
  (ol_set_va_display_warning_msg_handler (register-foreign-callable 'ol-warning-va-handler))
  (ol_set_error_handler (register-foreign-callable 'ol-error-handler))
  (ol_set_va_display_error_msg_handler (register-foreign-callable 'ol-error-va-handler)))

(install-ol-error-handlers)

(defvar *ol-done* nil)

(unless *ol-done*
  (ol-initialize)
  (xt-initialize)
  (define-toolkit-classes
       *intrinsic-classes*
       *openlook-classes*)
  (setq *ol-done* t))

(defmethod make-widget ((w event) name parent &rest args &key (managed t)
			&allow-other-keys)
  (remf args :managed)
  (let ((class (class-of w)))
    (if managed
	(apply #'create-managed-widget name class parent args)
      (apply #'create-widget name class parent args))))

(add-resource-to-class (find-class 'menu-button)
			   (make-instance 'resource
					  :name :menu-pane
					  :type 'widget
					  :original-name
					  (clim-utils:string-to-foreign
					   "menuPane")))

(add-resource-to-class (find-class 'abbrev-menu-button)
		       (make-instance 'resource
				      :name :menu-pane
				      :type 'widget
				      :original-name
				      (clim-utils:string-to-foreign
				       "menuPane")))

#+dlfcn
(when sys::*toolkit-shared*
  (defun reinitialize-toolkit ()
    (ol-initialize)
    (xt_toolkit_initialize)
    (setup-error-handlers)
    (install-ol-error-handlers)
    (fixup-class-entry-points))
  (push 'reinitialize-toolkit excl::*restart-actions*))
