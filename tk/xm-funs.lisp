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
;; $Id: xm-funs.lisp,v 1.22.34.2 2001/05/23 19:49:13 duane Exp $

;;
;; This file contains compile time only code -- put in clim-debug.fasl.
;;

(in-package :tk)

(defforeign 'xm_string_create_l_to_r
    :entry-point (ff:convert-foreign-name "XmStringCreateLtoR")
    :call-direct t
    :callback nil
    :arguments '(foreign-address foreign-address)
    :return-type :foreign-address)

(defforeign 'xm_string_concat
    :entry-point (ff:convert-foreign-name "XmStringConcat")
    :call-direct t
    :callback nil
    :arguments '(foreign-address foreign-address)
    :return-type :foreign-address)

(defforeign 'xm_string_copy
    :entry-point (ff:convert-foreign-name "XmStringCopy")
    :call-direct t
    :callback nil
    :arguments '(foreign-address)
    :return-type :foreign-address)

(defforeign 'xm_string_get_l_to_r
    :entry-point (ff:convert-foreign-name "XmStringGetLtoR")
    :call-direct t
    :callback nil
    :arguments '(foreign-address foreign-address foreign-address)
    :arg-checking nil
    :return-type :fixnum)

;;; New method to support Motif2.1
(defforeign 'xm_string_free
    :entry-point (ff:convert-foreign-name "XmStringFree")
    :call-direct t
    :callback nil
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xm_get_pixmap
    :entry-point (ff:convert-foreign-name "XmGetPixmap")
    :call-direct t
    :arguments '(foreign-address foreign-address integer integer)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xm_font_list_init_font_context
    :entry-point (ff:convert-foreign-name "XmFontListInitFontContext")
    :call-direct t
    :callback nil
    :arguments '(foreign-address foreign-address)
    :arg-checking nil
    :return-type :fixnum)

(defforeign 'xm_font_list_free_font_context
    :entry-point (ff:convert-foreign-name "XmFontListFreeFontContext")
    :call-direct t
    :callback nil
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xm_font_list_get_next_font
    :entry-point (ff:convert-foreign-name "XmFontListGetNextFont")
    :call-direct t
    :callback nil
    :arguments '(foreign-address foreign-address foreign-address)
    :arg-checking nil
    :return-type :fixnum)

(defforeign 'xm_font_list_create
    :entry-point (ff:convert-foreign-name "XmFontListCreate")
    :call-direct t
    :callback nil
    :arguments '(foreign-address foreign-address)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xm_font_list_free
    :entry-point (ff:convert-foreign-name "XmFontListFree")
    :call-direct t
    :callback nil
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xm_font_list_entry_free
    :entry-point (ff:convert-foreign-name "XmFontListEntryFree")
    :call-direct t
    :callback nil
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xm_add_protocol_callback
    :entry-point (ff:convert-foreign-name "XmAddProtocolCallback")
    :call-direct t
    :arguments '(foreign-address integer integer foreign-address foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xm_intern_atom
    :entry-point (ff:convert-foreign-name "XmInternAtom")
    :call-direct t
    :callback nil
    :arguments '(foreign-address foreign-address fixnum)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xm_main_window_set_area
    :entry-point (ff:convert-foreign-name "XmMainWindowSetAreas")
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address
		 foreign-address foreign-address foreign-address)
    :arg-checking nil
    :return-type :void)

(ff:defforeign 'xm_process_traversal
    :entry-point (ff:convert-foreign-name "XmProcessTraversal")
    :call-direct t
    :arguments '(foreign-address integer)
    :arg-checking nil
    :return-type :fixnum)

(defforeign 'xm-message-box-get-child
    :entry-point (ff:convert-foreign-name "XmMessageBoxGetChild")
    :call-direct t
    :arguments '(foreign-address fixnum)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xm-selection-box-get-child
    :entry-point (ff:convert-foreign-name "XmSelectionBoxGetChild")
    :call-direct t
    :arguments '(foreign-address fixnum)
    :arg-checking nil
    :return-type :foreign-address)


(defforeign 'xm_file_selection_do_search
    :entry-point (ff:convert-foreign-name "XmFileSelectionDoSearch")
    :call-direct t
    :arguments '(foreign-address foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xm_option_label_gadget
    :entry-point (ff:convert-foreign-name "XmOptionLabelGadget")
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xm_option_button_gadget
    :entry-point (ff:convert-foreign-name "XmOptionButtonGadget")
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'initializemydrawingareaquerygeometry
    :entry-point (ff:convert-foreign-name "InitializeMyDrawingAreaQueryGeometry")
    :call-direct t
    :callback nil
    :arguments '(integer)
    :arg-checking nil
    :return-type :void)

(defforeign 'xm_get_focus_widget
    :entry-point (ff:convert-foreign-name "XmGetFocusWidget")
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xm_is_traversable
    :entry-point (ff:convert-foreign-name "XmIsTraversable")
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xm_font_list_append_entry
    :entry-point (ff:convert-foreign-name "XmFontListAppendEntry")
    :call-direct t
    :callback nil
    :arguments '(foreign-address foreign-address)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xm_font_list_entry_create
    :entry-point (ff:convert-foreign-name "XmFontListEntryCreate")
    :call-direct t
    :callback nil
    :arguments '(foreign-address foreign-address foreign-address)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xm_font_list_entry_get_font
    :entry-point (ff:convert-foreign-name "XmFontListEntryGetFont")
    :call-direct t
    :callback nil
    :arguments '(foreign-address foreign-address)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xm_font_list_next_entry
    :entry-point (ff:convert-foreign-name "XmFontListNextEntry")
    :call-direct t
    :callback nil
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :foreign-address)

(defforeign 'xm_toggle_button_set_state
    :entry-point (ff:convert-foreign-name "XmToggleButtonSetState")
    :call-direct t
    :callback t
    :arguments '(foreign-address fixnum fixnum)
    :arg-checking nil
    :return-type :void)

(defforeign 'xm_toggle_button_get_state
    :entry-point (ff:convert-foreign-name "XmToggleButtonGetState")
    :call-direct t
    :callback t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :fixnum)

(ff:defforeign 'xm_text_field_get_selection
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :entry-point (ff:convert-foreign-name "XmTextFieldGetSelection")
    :return-type :integer)

(ff:defforeign 'xm_text_get_selection
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :entry-point (ff:convert-foreign-name "XmTextGetSelection")
    :return-type :integer)

(ff:defforeign 'xm_scale_set_value
    :call-direct t
    :arguments '(foreign-address fixnum)
    :arg-checking nil
    :entry-point (ff:convert-foreign-name "XmScaleSetValue")
    :return-type :void)

(ff:defforeign 'xm_get_display
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :entry-point (ff:convert-foreign-name "XmGetXmDisplay")
    :return-type :integer)

(ff:defforeign 'xm_change_color
    :call-direct t
    :arguments '(foreign-address fixnum)
    :arg-checking nil
    :entry-point (ff:convert-foreign-name "XmChangeColor")
    :return-type :void)

(provide :clim-debugxm)
