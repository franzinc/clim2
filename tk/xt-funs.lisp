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
;; $fiHeader: xt-funs.lisp,v 1.7 92/07/01 15:44:40 cer Exp $

;;
;; This file contains compile time only code -- put in clim-debug.fasl.
;;

(in-package :xt)

(defforeign 'xt_get_resource_list :entry-point "_XtGetResourceList")
(defforeign 'xt_get_constraint_resource_list :entry-point "_XtGetConstraintResourceList")

(defforeign 'xt_initialize_widget_class :entry-point "_XtInitializeWidgetClass")

(defforeign 'xt_free
    :arguments '(ff:foreign-address)
    :call-direct t
    :callback nil
    :arg-checking nil
    :return-type :void
    :entry-point "_XtFree")

(defforeign 'xt_toolkit_initialize
    :entry-point "_XtToolkitInitialize")

(defforeign 'xt_create_application_context
    :entry-point "_XtCreateApplicationContext")


(defforeign 'xt_app_set_error_handler
    :entry-point "_XtAppSetErrorHandler")

(defforeign 'xt_app_set_warning_handler
    :entry-point "_XtAppSetWarningHandler")

(defforeign 'xt_open_display
    :entry-point "_XtOpenDisplay")

(defforeign 'xt_app_create_shell 
    :entry-point "_XtAppCreateShell")

;;;; 

(defforeign 'xt_create_widget :entry-point "_XtCreateWidget")
(defforeign 'xt_create_managed_widget
    :arguments '(simple-string ff:foreign-address ff:foreign-address
		 ff:foreign-address fixnum)
    :call-direct t
    :callback t
    :arg-checking nil
    :return-type :unsigned-integer
    :entry-point "_XtCreateManagedWidget")

(defforeign 'xt_realize_widget :entry-point "_XtRealizeWidget")
(defforeign 'xt_manage_child :entry-point "_XtManageChild")
(defforeign 'xt_is_managed :entry-point "_XtIsManaged")
(defforeign 'xt_unmanage_child :entry-point "_XtUnmanageChild")
(defforeign 'xt_manage_children :entry-point "_XtManageChildren")
(defforeign 'xt_destroy_widget :entry-point "_XtDestroyWidget")
(defforeign 'xt_create_popup_shell :entry-point "_XtCreatePopupShell")
(defforeign 'xt_popup :entry-point "_XtPopup")
(defforeign 'xt_popdown :entry-point "_XtPopdown")
(defforeign 'xt_window :entry-point "_XtWindow")
(defforeign 'xt_parent :entry-point "_XtParent")
(defforeign 'xt_query_geometry :entry-point "_XtQueryGeometry")
(defforeign 'xt_configure_widget :entry-point "_XtConfigureWidget")

(defforeign 'xt_set_values
    :arguments '(ff:foreign-address ff:foreign-address fixnum)
    :call-direct t
    :callback t
    :arg-checking nil
    :return-type :void
    :entry-point "_XtSetValues")
(defforeign 'xt_get_values
    :arguments '(ff:foreign-address ff:foreign-address fixnum)
    :call-direct t
    :callback t
    :arg-checking nil
    :return-type :void
    :entry-point "_XtGetValues")


(defforeign 'xt_app_pending
    :arguments '(ff:foreign-address)
    :call-direct t
    ;; Maybe callback can be safely set to nil...
    :callback t
    :arg-checking nil
    :return-type :fixnum
    :entry-point "_XtAppPending")
(defforeign 'xt_app_process_event
    :arguments '(ff:foreign-address fixnum)
    :call-direct t
    :callback t
    :arg-checking nil
    :return-type :void
    :entry-point "_XtAppProcessEvent")
(defforeign 'xt_add_event_handler :entry-point "_XtAddEventHandler")
(defforeign 'xt_build_event_mask :entry-point "_XtBuildEventMask")

(defforeign 'xt_add_callback :entry-point "_XtAddCallback")
(defforeign 'xt_has_callbacks :entry-point "_XtHasCallbacks")
(defforeign 'xt_remove_all_callbacks :entry-point "_XtRemoveAllCallbacks")

(defforeign 'xtsetsensitive :entry-point "_XtSetSensitive")
