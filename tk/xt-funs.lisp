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
;; $Id: xt-funs.lisp,v 1.32 2000/03/04 05:13:49 duane Exp $

;;
;; This file contains compile time only code -- put in clim-debug.fasl.
;;

(in-package :xt)

(defforeign 'xt_get_resource_list
    :entry-point (ff:convert-to-lang "XtGetResourceList")
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_get_constraint_resource_list
    :entry-point (ff:convert-to-lang "XtGetConstraintResourceList")
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_initialize_widget_class
    :entry-point (ff:convert-to-lang "XtInitializeWidgetClass")
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_free
    :entry-point (ff:convert-to-lang "XtFree")
    :arguments '(foreign-address)
    :call-direct t
    :callback nil
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_toolkit_initialize
    :entry-point (ff:convert-to-lang
		  #-rs6000 "XtToolkitInitialize"
		  #+rs6000 "_XtToolkitInitialize")
    :call-direct t
    :arguments nil
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_create_application_context
    :entry-point (ff:convert-to-lang "XtCreateApplicationContext")
    :call-direct t
    :arguments nil
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xt_destroy_application_context
    :entry-point (ff:convert-to-lang "XtDestroyApplicationContext")
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xt_app_set_error_handler
    :entry-point (ff:convert-to-lang "XtAppSetErrorHandler")
    :call-direct t
    :arguments '(foreign-address integer)
    :arg-checking nil
    :return-type :integer)

(defforeign 'xt_app_set_warning_handler
    :entry-point (ff:convert-to-lang "XtAppSetWarningHandler")
    :call-direct t
    :arguments '(foreign-address integer)
    :arg-checking nil
    :return-type :integer)

(defforeign 'xt_open_display
    :entry-point (ff:convert-to-lang "XtOpenDisplay")
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address foreign-address
		 foreign-address fixnum foreign-address foreign-address)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xt_close_display
    :entry-point (ff:convert-to-lang "XtCloseDisplay")
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xt_database
    :entry-point (ff:convert-to-lang "XtDatabase")
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xt_get_application_name_and_class
    :entry-point (ff:convert-to-lang "XtGetApplicationNameAndClass")
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address)
    :arg-checking nil
    :return-type :foreign-address)


(defforeign 'xt_convert_and_store
    :entry-point (ff:convert-to-lang "XtConvertAndStore")
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address
		 foreign-address foreign-address)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xt_app_create_shell
    :entry-point (ff:convert-to-lang "XtAppCreateShell")
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address foreign-address
		 foreign-address fixnum)
    :arg-checking nil
    :return-type :foreign-address)

;;;;

(defforeign 'xt_create_widget
    :entry-point (ff:convert-to-lang "XtCreateWidget")
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address
		 foreign-address fixnum)
    :arg-checking nil
    :return-type :foreign-address)


(defforeign 'xt_create_managed_widget
    :entry-point (ff:convert-to-lang "XtCreateManagedWidget")
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address
		 foreign-address fixnum)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xt_realize_widget
    :entry-point (ff:convert-to-lang "XtRealizeWidget")
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_is_realized
    :entry-point (ff:convert-to-lang "XtIsRealized")
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :fixnum)

(defforeign 'xt_destroy_widget
    :entry-point (ff:convert-to-lang "XtDestroyWidget")
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_manage_child
    :entry-point (ff:convert-to-lang "XtManageChild")
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_is_managed
    :entry-point (ff:convert-to-lang "XtIsManaged")
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :fixnum)

(defforeign 'xt_unmanage_child
    :entry-point (ff:convert-to-lang "XtUnmanageChild")
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_manage_children
    :entry-point (ff:convert-to-lang "XtManageChildren")
    :call-direct t
    :arguments '(foreign-address fixnum)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_unmanage_children
    :entry-point (ff:convert-to-lang "XtUnmanageChildren")
    :call-direct t
    :arguments '(foreign-address fixnum)
    :arg-checking nil
    :return-type :void)


(defforeign 'xt_create_popup_shell
    :entry-point (ff:convert-to-lang "XtCreatePopupShell")
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address foreign-address fixnum)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xt_popup
    :entry-point (ff:convert-to-lang "XtPopup")
    :call-direct t
    :arguments '(foreign-address integer)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_popdown
    :entry-point (ff:convert-to-lang "XtPopdown")
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_window
    :entry-point (ff:convert-to-lang "XtWindow")
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xt_parent
    :entry-point (ff:convert-to-lang "XtParent")
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xt_name
    :entry-point (ff:convert-to-lang "XtName")
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xt_class
    :entry-point (ff:convert-to-lang "XtClass")
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xt_query_geometry
    :entry-point (ff:convert-to-lang "XtQueryGeometry")
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xt_configure_widget
    :entry-point (ff:convert-to-lang "XtConfigureWidget")
    :call-direct t
    :arguments '(foreign-address fixnum fixnum fixnum fixnum fixnum)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_set_values
    :arguments '(foreign-address foreign-address fixnum)
    :call-direct t
    :arg-checking nil
    :return-type :void
    :entry-point (ff:convert-to-lang "XtSetValues"))
(defforeign 'xt_get_values
    :arguments '(foreign-address foreign-address fixnum)
    :call-direct t
    :arg-checking nil
    :return-type :void
    :entry-point (ff:convert-to-lang "XtGetValues"))


(defforeign 'xt_app_pending
    :arguments '(foreign-address)
    :call-direct t
    :arg-checking nil
    :return-type :fixnum
    :entry-point (ff:convert-to-lang "XtAppPending"))


(defforeign 'xt_app_peek_event
    :arguments '(foreign-address foreign-address)
    :call-direct t
    :arg-checking nil
    :return-type :fixnum
    :entry-point (ff:convert-to-lang "XtAppPeekEvent"))

(defforeign 'xt_app_process_event
    :arguments '(foreign-address fixnum)
    :call-direct t
    :arg-checking nil
    :return-type :void
    :entry-point (ff:convert-to-lang "XtAppProcessEvent"))


(defforeign 'xt_app_interval_next_timer
    :arguments '(ff:foreign-address)
    :call-direct t
    ;; Maybe callback can be safely set to nil...
    :callback t
    :arg-checking nil
    :return-type :fixnum
    :entry-point (ff:convert-to-lang "XtAppIntervalNextTimer"))

(defforeign 'xt_add_event_handler
    :entry-point (ff:convert-to-lang "XtAddEventHandler")
    :call-direct t
    :arguments '(foreign-address integer fixnum foreign-address foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_build_event_mask
    :entry-point (ff:convert-to-lang "XtBuildEventMask")
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xt_add_callback
    :entry-point (ff:convert-to-lang "XtAddCallback")
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_has_callbacks
    :entry-point (ff:convert-to-lang "XtHasCallbacks")
    :call-direct t
    :arguments '(foreign-address foreign-address)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xt_remove_all_callbacks
    :entry-point (ff:convert-to-lang "XtRemoveAllCallbacks")
    :call-direct t
    :arguments '(foreign-address foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_set_sensitive
    :entry-point (ff:convert-to-lang "XtSetSensitive")
    :call-direct t
    :arguments '(foreign-address fixnum)
    :arg-checking nil
    :return-type :void)

(defforeign 'xt_grab_pointer
    :entry-point (ff:convert-to-lang "XtGrabPointer")
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
    :entry-point (ff:convert-to-lang "XtUngrabPointer")
    :call-direct t
    :arguments '(foreign-address	; display
		 fixnum			; time
		 )
    :arg-checking nil
    :return-type :fixnum)

(defforeign 'xt_ungrab_button
    :entry-point (ff:convert-to-lang "XtUngrabButton")
    :call-direct t
    :arguments '(foreign-address	; widget
		 fixnum			; button
		 fixnum			; modifiers
		 )
    :arg-checking nil
    :return-type :void)

(defforeign 'xt-last-timestamp-processed
    :entry-point (ff:convert-to-lang "XtLastTimestampProcessed")
    :call-direct t
    :arguments '(foreign-address	; display
		 )
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xt_set_keyboard_focus
    :entry-point (ff:convert-to-lang "XtSetKeyboardFocus")
    :call-direct t
    :arguments '(foreign-address foreign-address)
    :arg-checking nil
    :return-type :foreign-address)

(ff:defforeign 'init_clim_gc_cursor_stuff
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :entry-point (ff:convert-to-lang "init_clim_gc_cursor_stuff")
    :return-type :foreign-address)

(ff:defforeign 'set_clim_gc_cursor_widget
    :call-direct t
    :arguments '(foreign-address integer)
    :arg-checking nil
    :entry-point (ff:convert-to-lang "set_clim_gc_cursor_widget")
    :return-type :foreign-address)

;(ff:defforeign 'xt_get_multi_click_time
;    :call-direct t
;    :arguments '(foreign-address)
;    :arg-checking nil
;    :entry-point (ff:convert-to-lang "XtGetMultiClickTime")
;    :return-type :foreign-address)

(ff:defforeign 'xt_parse_translation_table
    :call-direct t
    :arguments '(simple-string)
    :arg-checking nil
    :entry-point (ff:convert-to-lang "XtParseTranslationTable")
    :return-type :foreign-address)

(ff:defforeign 'xt_parse_accelerator_table
    :call-direct t
    :arguments '(simple-string)
    :arg-checking nil
    :entry-point (ff:convert-to-lang "XtParseAcceleratorTable")
    :return-type :foreign-address)

(ff:defforeign 'xt_app_set_fallback_resources
    :call-direct t
    :arguments '(foreign-address foreign-address)
    :arg-checking nil
    :entry-point (ff:convert-to-lang "XtAppSetFallbackResources")
    :return-type :void)

(ff:defforeign 'xt_widget_num_popups
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :entry-point (ff:convert-to-lang "xt_widget_num_popups")
    :return-type :fixnum)

(ff:defforeign 'xt_set_language_proc
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address)
    :arg-checking nil
    :entry-point (ff:convert-to-lang "XtSetLanguageProc")
    :return-type :void)

;; this isn't part of Xt but is useful for debugging. The locale
;; handling is all done through XtSetLanguageProc above

(ff:defforeign 'setlocale-1
    :call-direct t
    :arg-checking nil
    :entry-point (ff:convert-to-lang "setlocale")
    :arguments '(integer integer)
    :return-type :foreign-address)

#+debug
(progn

(ff:defforeign 'xlc-current-lc
    :call-direct t
    :arguments nil
    :arg-checking nil
    :entry-point (ff:convert-to-lang "_XlcCurrentLC")
    :return-type :foreign-address)

(ff:defforeign 'init-font-set
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :entry-point (ff:convert-to-lang "initFontSet")
    :return-type :foreign-address)


(ff:defforeign 'islower
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :entry-point (ff:convert-to-lang "islower")
    :return-type :foreign-address)


(ff:defforeign 'toupper
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :entry-point (ff:convert-to-lang "toupper")
    :return-type :foreign-address)

) ;;progn
