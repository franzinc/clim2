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
;; $fiHeader: xt-funs.lisp,v 1.11 92/09/08 15:16:27 cer Exp Locker: cer $

;;
;; This file contains compile time only code -- put in clim-debug.fasl.
;;

(in-package :xt)

(defforeign 'xt_get_resource_list
    :entry-point "_XtGetResourceList"
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address)
    :arg-checking nil
    :return-type :void)
    
(defforeign 'xt_get_constraint_resource_list
    :entry-point "_XtGetConstraintResourceList"
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_initialize_widget_class
    :entry-point "_XtInitializeWidgetClass"
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_free
    :entry-point "_XtFree"
    :arguments '(foreign-address)
    :call-direct t
    :callback nil
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_toolkit_initialize
    :entry-point "_XtToolkitInitialize"
    :call-direct t
    :arguments nil
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_create_application_context
    :entry-point "_XtCreateApplicationContext"
    :call-direct t
    :arguments nil
    :arg-checking nil
    :return-type :unsigned-integer)


(defforeign 'xt_app_set_error_handler
    :entry-point "_XtAppSetErrorHandler"
    :call-direct t
    :arguments '(foreign-address integer)
    :arg-checking nil
    :return-type :integer)

(defforeign 'xt_app_set_warning_handler
    :entry-point "_XtAppSetWarningHandler"
    :call-direct t
    :arguments '(foreign-address integer)
    :arg-checking nil
    :return-type :integer)

(defforeign 'xt_open_display
    :entry-point "_XtOpenDisplay"
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address foreign-address
		 foreign-address fixnum foreign-address foreign-address)
    :arg-checking nil
    :return-type :unsigned-integer)

(defforeign 'xt_app_create_shell 
    :entry-point "_XtAppCreateShell"
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address foreign-address
		 foreign-address fixnum)
    :arg-checking nil
    :return-type :unsigned-integer)

;;;; 

(defforeign 'xt_create_widget
    :entry-point "_XtCreateWidget"
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address
		 foreign-address fixnum)
    :arg-checking nil
    :return-type :unsigned-integer)
    
    
(defforeign 'xt_create_managed_widget
    :entry-point "_XtCreateManagedWidget"
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address
		 foreign-address fixnum)
    :arg-checking nil
    :return-type :unsigned-integer)

(defforeign 'xt_realize_widget
    :entry-point "_XtRealizeWidget"
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :void)
    
(defforeign 'xt_destroy_widget
    :entry-point "_XtDestroyWidget"
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_manage_child
    :entry-point "_XtManageChild"
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_is_managed
    :entry-point "_XtIsManaged"
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :fixnum)
    
(defforeign 'xt_unmanage_child
    :entry-point "_XtUnmanageChild"
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_manage_children
    :entry-point "_XtManageChildren"
    :call-direct t
    :arguments '(foreign-address fixnum)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_create_popup_shell
    :entry-point "_XtCreatePopupShell"
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address foreign-address fixnum)
    :arg-checking nil
    :return-type :unsigned-integer)
    
(defforeign 'xt_popup
    :entry-point "_XtPopup"
    :call-direct t
    :arguments '(foreign-address integer)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_popdown
    :entry-point "_XtPopdown"
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :void)
    
(defforeign 'xt_window
    :entry-point "_XtWindow"
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :unsigned-integer)
    
(defforeign 'xt_parent
    :entry-point "_XtParent"
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :unsigned-integer)
	    
(defforeign 'xt_query_geometry
    :entry-point "_XtQueryGeometry"
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address)
    :arg-checking nil
    :return-type :unsigned-integer)

(defforeign 'xt_configure_widget
    :entry-point "_XtConfigureWidget"
    :call-direct t
    :arguments '(foreign-address fixnum fixnum fixnum fixnum fixnum)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_set_values
    :arguments '(foreign-address foreign-address fixnum)
    :call-direct t
    :arg-checking nil
    :return-type :void
    :entry-point "_XtSetValues")
(defforeign 'xt_get_values
    :arguments '(foreign-address foreign-address fixnum)
    :call-direct t
    :arg-checking nil
    :return-type :void
    :entry-point "_XtGetValues")


(defforeign 'xt_app_pending
    :arguments '(foreign-address)
    :call-direct t
    :arg-checking nil
    :return-type :fixnum
    :entry-point "_XtAppPending")
(defforeign 'xt_app_process_event
    :arguments '(foreign-address fixnum)
    :call-direct t
    :arg-checking nil
    :return-type :void
    :entry-point "_XtAppProcessEvent")


(defforeign 'xt_app_interval_next_timer
    :arguments '(ff:foreign-address)
    :call-direct t
    ;; Maybe callback can be safely set to nil...
    :callback t
    :arg-checking nil
    :return-type :unsigned-integer
    :entry-point "_XtAppIntervalNextTimer")

(defforeign 'xt_add_event_handler
    :entry-point "_XtAddEventHandler"
    :call-direct t
    :arguments '(foreign-address integer fixnum foreign-address foreign-address)
    :arg-checking nil
    :return-type :void)
    
(defforeign 'xt_build_event_mask
    :entry-point "_XtBuildEventMask"
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :unsigned-integer)

(defforeign 'xt_add_callback
    :entry-point "_XtAddCallback"
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address foreign-address)
    :arg-checking nil
    :return-type :void)
    
(defforeign 'xt_has_callbacks
    :entry-point "_XtHasCallbacks"
    :call-direct t
    :arguments '(foreign-address foreign-address)
    :arg-checking nil
    :return-type :unsigned-integer)
    
(defforeign 'xt_remove_all_callbacks
    :entry-point "_XtRemoveAllCallbacks"
    :call-direct t
    :arguments '(foreign-address foreign-address)
    :arg-checking nil
    :return-type :void)
    
(defforeign 'xt_set_sensitive
    :entry-point "_XtSetSensitive"
    :call-direct t
    :arguments '(foreign-address fixnum)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_grab_pointer
    :entry-point "_XtGrabPointer"
    :call-direct t
    :arguments '(foreign-address	; display
		 foreign-address	; widget
		 fixnum			; owner
		 fixnum			; pgrabmode
		 fixnum			; kgrabmode
		 foreign-address	; confine to
		 foreign-address	; cursor
		 fixnum			; time
		 )
    :arg-checking nil
    :return-type :fixnum)

(defforeign 'xt_ungrab_pointer
    :entry-point "_XtUngrabPointer"
    :call-direct t
    :arguments '(foreign-address	; display
		 fixnum			; time
		 )
    :arg-checking nil
    :return-type :fixnum)

