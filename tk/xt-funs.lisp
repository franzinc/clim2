;; See the file LICENSE for the full license governing this code.
;;

;;
;; This file contains compile time only code -- put in clim-debug.fasl.
;;

(in-package :xt)

(def-foreign-call (xt_get_resource_list "XtGetResourceList")
    ((x :foreign-address) (y :foreign-address) (z :foreign-address))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_get_constraint_resource_list "XtGetConstraintResourceList")
    ((x :foreign-address) (y :foreign-address) (z :foreign-address))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_initialize_widget_class "XtInitializeWidgetClass")
    ((x :foreign-address))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_free "XtFree")
    ((x :foreign-address))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_toolkit_initialize #-rs6000 "XtToolkitInitialize"
					 #+rs6000 "_XtToolkitInitialize")

    (:void)
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_create_application_context "XtCreateApplicationContext")
    (:void)
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_destroy_application_context "XtDestroyApplicationContext")
    ((x :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_app_set_error_handler "XtAppSetErrorHandler")
    ((x :foreign-address) y)
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_app_set_warning_handler "XtAppSetWarningHandler")
    ((x :foreign-address) y)
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_open_display "XtOpenDisplay")
    ((a :foreign-address)(b :foreign-address)(c :foreign-address)(d :foreign-address)
			 (e :foreign-address)(f :int fixnum)(g :foreign-address)(h :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_close_display "XtCloseDisplay")
    ((x :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_database "XtDatabase")
    ((x :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_get_application_name_and_class "XtGetApplicationNameAndClass")
    ((x :foreign-address) (y :foreign-address) (z :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_convert_and_store "XtConvertAndStore")
    ((a :foreign-address) (b :foreign-address) (c :foreign-address)
			  (d :foreign-address) (e :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_app_create_shell "XtAppCreateShell")
    ((a :foreign-address) (b :foreign-address) (c :foreign-address)
     (d :foreign-address) (e :foreign-address) (f :int fixnum))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

;;;;
(def-foreign-call (xt_create_widget "XtCreateWidget")
    ((a :foreign-address) (b :foreign-address) (c :foreign-address)
			  (d :foreign-address) (e :int fixnum))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_create_managed_widget "XtCreateManagedWidget")
    ((a :foreign-address) (b :foreign-address) (c :foreign-address)
			  (d :foreign-address) (e :int fixnum))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_realize_widget "XtRealizeWidget")
    ((x :foreign-address))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_is_realized "XtIsRealized")
    ((x :foreign-address))
  :returning (:int fixnum)
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_destroy_widget "XtDestroyWidget")
    ((x :foreign-address))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_manage_child "XtManageChild")
    ((x :foreign-address))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_is_managed "XtIsManaged")
    ((x :foreign-address))
  :returning (:int fixnum)
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_unmanage_child "XtUnmanageChild")
    ((x :foreign-address))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_unmap_widget "XtUnmapWidget")
    ((x :foreign-address))
  :returning (:int fixnum)
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_manage_children "XtManageChildren")
    ((x :foreign-address) (y :int fixnum))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_unmanage_children "XtUnmanageChildren")
    ((x :foreign-address) (y :int fixnum))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_create_popup_shell "XtCreatePopupShell")
    ((a :foreign-address) (b :foreign-address) (c :foreign-address)
			  (d :foreign-address) (e :int fixnum))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_popup "XtPopup")
    ((x :foreign-address) y)
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_popdown "XtPopdown")
    ((x :foreign-address))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_window "XtWindow")
    ((x :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_parent "XtParent")
    ((x :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_name "XtName")
    ((x :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_class "XtClass")
    ((x :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_query_geometry "XtQueryGeometry")
    ((x :foreign-address) (y :foreign-address) (z :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_configure_widget "XtConfigureWidget")
    ((a :foreign-address) (b :int fixnum) (c :int fixnum)
     (d :int fixnum) (e :int fixnum) (f :int fixnum))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_set_values "XtSetValues")
    ((x :foreign-address) (y :foreign-address) (z :int fixnum))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_get_values "XtGetValues")
    ((x :foreign-address) (y :foreign-address) (z :int fixnum))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_app_pending "XtAppPending")
    ((x :foreign-address))
  :returning (:int fixnum)
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_app_peek_event "XtAppPeekEvent")
    ((x :foreign-address) (y :foreign-address))
  :returning (:int fixnum)
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_app_process_event "XtAppProcessEvent")
    ((x :foreign-address) (y :int fixnum))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_app_interval_next_timer "XtAppIntervalNextTimer")
    ((x :foreign-address))
  :returning (:int fixnum)
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_add_event_handler "XtAddEventHandler")
    ((a :foreign-address) b (c :int fixnum)
			  (d :foreign-address) (e :foreign-address))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_build_event_mask "XtBuildEventMask")
    ((x :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_add_callback "XtAddCallback")
    ((w :foreign-address)(x :foreign-address)(y :foreign-address)(z :foreign-address))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_has_callbacks "XtHasCallbacks")
    ((x :foreign-address) (y :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_remove_all_callbacks "XtRemoveAllCallbacks")
    ((x :foreign-address) (y :foreign-address))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_set_sensitive "XtSetSensitive")
    ((x :foreign-address) (y :int fixnum))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_grab_pointer "XtGrabPointer")
    ((display :foreign-address) (widget :foreign-address) (owner :int fixnum)
     (pgrabmode :int fixnum) (kgrabmode :int fixnum) (confine-to :foreign-address)
     (cursor :foreign-address) (time :int fixnum))
  :returning (:int fixnum)
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_ungrab_pointer "XtUngrabPointer")
    ((display :foreign-address) (time :int fixnum))
  :returning (:int fixnum)
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_ungrab_button "XtUngrabButton")
    ((widget :foreign-address) (button :int fixnum) (modifiers :int fixnum))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt-last-timestamp-processed "XtLastTimestampProcessed")
    ((display :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_set_keyboard_focus "XtSetKeyboardFocus")
    ((x :foreign-address) (y :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (init_clim_gc_cursor_stuff "init_clim_gc_cursor_stuff")
    ((x :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (set_clim_gc_cursor_widget "set_clim_gc_cursor_widget")
    ((x :foreign-address) y)
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_parse_translation_table "XtParseTranslationTable")
    ((x (* :char) simple-string))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil
  :strings-convert nil)

(def-foreign-call (xt_parse_accelerator_table "XtParseAcceleratorTable")
    ((x (* :char) simple-string))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil
  :strings-convert nil)

(def-foreign-call (xt_app_set_fallback_resources "XtAppSetFallbackResources")
    ((x :foreign-address) (y :foreign-address))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_widget_num_popups "xt_widget_num_popups")
    ((x :foreign-address))
  :returning (:int fixnum)
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xt_set_language_proc "XtSetLanguageProc")
    ((x :foreign-address) (y :foreign-address) (z :foreign-address))
  :returning :void
  :call-direct t
  :arg-checking nil)

;; this isn't part of Xt but is useful for debugging. The locale
;; handling is all done through XtSetLanguageProc above

(def-foreign-call (setlocale-1 "setlocale")
    (x y)
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (x-supports-locale "XSupportsLocale")
    ()
  :returning (:int fixnum)
  :strings-convert nil
  :arg-checking nil)

(def-foreign-call (x-set-locale-modifiers "XSetLocaleModifiers")
    ((x :foreign-address))
  :call-direct t
  :returning :foreign-address
  :arg-checking nil)

#+debug
(progn

(def-foreign-call (xlc-current-lc "_XlcCurrentLC")
    (:void)
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (init-font-set "initFontSet")
    ((x :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (islower "islower")
    ((x :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (toupper "toupper")
    ((x :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

) ;;progn
