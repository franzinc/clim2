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
;; $fiHeader$


(in-package :xt)

(defforeign 'quark_to_string
    :entry-point "_XrmQuarkToString")

(defforeign 'get-resource-list-1 :entry-point "_XtGetResourceList")
(defforeign 'get-constraint-resource-list-1 :entry-point "_XtGetConstraintResourceList")

(defforeign 'initialize-widget-class :entry-point "_XtInitializeWidgetClass")

(defforeign 'xt_free :entry-point "_XtFree")

(defforeign 'toolkit-initialize
    :entry-point "_XtToolkitInitialize")

(defforeign 'create_application_context
    :entry-point "_XtCreateApplicationContext")


(defforeign 'app_set_error_handler
    :entry-point "_XtAppSetErrorHandler")

(defforeign 'app_set_warning_handler
    :entry-point "_XtAppSetWarningHandler")

(defforeign 'open_display
    :entry-point "_XtOpenDisplay")

(defforeign 'app_create_shell 
    :entry-point "_XtAppCreateShell")

;;;; 

(defforeign 'create_widget :entry-point "_XtCreateWidget")
(defforeign 'create_managed_widget :entry-point "_XtCreateManagedWidget")
(defforeign 'realize_widget :entry-point "_XtRealizeWidget")
(defforeign 'manage_child :entry-point "_XtManageChild")
(defforeign 'xt_is_managed :entry-point "_XtIsManaged")
(defforeign 'unmanage_child :entry-point "_XtUnmanageChild")
(defforeign 'manage_children :entry-point "_XtManageChildren")
(defforeign 'destroy_widget :entry-point "_XtDestroyWidget")
(defforeign 'create_popup_shell :entry-point "_XtCreatePopupShell")
(defforeign '_popup :entry-point "_XtPopup")
(defforeign '_popdown :entry-point "_XtPopdown")
(defforeign 'xt_window :entry-point "_XtWindow")
(defforeign 'xt_parent :entry-point "_XtParent")
(defforeign 'xt_query_geometry :entry-point "_XtQueryGeometry")
(defforeign 'xt_configure_widget :entry-point "_XtConfigureWidget")

(defforeign 'set_values :entry-point "_XtSetValues")
(defforeign 'get_values :entry-point "_XtGetValues")

(defforeign 'app_pending :return-type :fixnum :entry-point "_XtAppPending")
(defforeign 'app_process_event :entry-point "_XtAppProcessEvent")
(defforeign 'add_event_handler :entry-point "_XtAddEventHandler")
(defforeign 'xt_build_event_mask :entry-point "_XtBuildEventMask")

(defforeign 'add_callback :entry-point "_XtAddCallback")
(defforeign 'xt_has_callbacks :entry-point "_XtHasCallbacks")
