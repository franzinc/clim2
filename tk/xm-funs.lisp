;; -*- mode: common-lisp; package: tk -*-
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
;; $fiHeader: xm-funs.lisp,v 1.8 93/04/27 14:36:00 cer Exp $

;;
;; This file contains compile time only code -- put in clim-debug.fasl.
;;

(in-package :tk)

(defforeign 'xm_string_create_l_to_r
    :entry-point (ff:convert-to-lang "XmStringCreateLtoR")
    :call-direct t
    :callback nil
    :arguments '(foreign-address foreign-address)
    :return-type :unsigned-integer)

(defforeign 'xm_string_get_l_to_r
    :entry-point (ff:convert-to-lang "XmStringGetLtoR")
    :call-direct t
    :callback nil
    :arguments '(foreign-address foreign-address foreign-address)
    :arg-checking nil
    :return-type :fixnum)

(defforeign 'xm_get_pixmap
    :entry-point (ff:convert-to-lang "XmGetPixmap")
    :call-direct t
    :arguments '(foreign-address foreign-address integer integer)
    :arg-checking nil
    :return-type :unsigned-integer)

(defforeign 'xm_font_list_init_font_context
    :entry-point (ff:convert-to-lang "XmFontListInitFontContext")
    :call-direct t
    :callback nil
    :arguments '(foreign-address foreign-address)
    :arg-checking nil
    :return-type :fixnum)

(defforeign 'xm_font_list_free_font_context
    :entry-point (ff:convert-to-lang "XmFontListFreeFontContext")
    :call-direct t
    :callback nil
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xm_font_list_get_next_font
    :entry-point (ff:convert-to-lang "XmFontListGetNextFont")
    :call-direct t
    :callback nil
    :arguments '(foreign-address foreign-address foreign-address)
    :arg-checking nil
    :return-type :fixnum)

(defforeign 'xm_font_list_create
    :entry-point (ff:convert-to-lang "XmFontListCreate")
    :call-direct t
    :callback nil
    :arguments '(foreign-address foreign-address)
    :arg-checking nil
    :return-type :unsigned-integer)

(defforeign 'xm_add_protocol_callback
    :entry-point (ff:convert-to-lang "XmAddProtocolCallback")
    :call-direct t
    :arguments '(foreign-address integer integer foreign-address foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xm_intern_atom 
    :entry-point (ff:convert-to-lang "XmInternAtom")
    :call-direct t
    :callback nil
    :arguments '(foreign-address foreign-address fixnum)
    :arg-checking nil
    :return-type :unsigned-integer)

(defforeign 'xm_main_window_set_area
    :entry-point (ff:convert-to-lang "XmMainWindowSetAreas")
    :call-direct t
    :arguments '(foreign-address foreign-address foreign-address
		 foreign-address foreign-address foreign-address)
    :arg-checking nil
    :return-type :void)

(ff:defforeign 'xm_process_traversal
    :entry-point (ff:convert-to-lang "XmProcessTraversal")
    :call-direct t
    :arguments '(foreign-address integer)
    :arg-checking nil
    :return-type :fixnum)

(defforeign 'xm-message-box-get-child
    :entry-point (ff:convert-to-lang "XmMessageBoxGetChild")
    :call-direct t
    :arguments '(foreign-address fixnum)
    :arg-checking nil
    :return-type :unsigned-integer)

(defforeign 'xm-selection-box-get-child
    :entry-point (ff:convert-to-lang "XmSelectionBoxGetChild")
    :call-direct t
    :arguments '(foreign-address fixnum)
    :arg-checking nil
    :return-type :unsigned-integer)


(defforeign 'xm_file_selection_do_search
    :entry-point (ff:convert-to-lang "XmFileSelectionDoSearch")
    :call-direct t
    :arguments '(foreign-address foreign-address)
    :arg-checking nil
    :return-type :void)

(defforeign 'xm_option_label_gadget
    :entry-point (ff:convert-to-lang "XmOptionLabelGadget")
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :unsigned-integer)

(defforeign 'xm_option_button_gadget
    :entry-point (ff:convert-to-lang "XmOptionButtonGadget")
    :call-direct t
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :unsigned-integer)

(defforeign 'initializemydrawingareaquerygeometry
    :entry-point (ff:convert-to-lang "InitializeMyDrawingAreaQueryGeometry")
    :call-direct t
    :callback nil
    :arguments '(integer)
    :arg-checking nil
    :return-type :void)




(defforeign 'xm_font_list_append_entry
    :entry-point (ff:convert-to-lang "XmFontListAppendEntry")
    :call-direct t
    :callback nil
    :arguments '(foreign-address foreign-address)
    :arg-checking nil
    :return-type :unsigned-integer)

(defforeign 'xm_font_list_entry_create
    :entry-point (ff:convert-to-lang "XmFontListEntryCreate")
    :call-direct t
    :callback nil
    :arguments '(foreign-address foreign-address foreign-address)
    :arg-checking nil
    :return-type :unsigned-integer)



(defforeign 'xm_font_list_entry_get_font
    :entry-point (ff:convert-to-lang "XmFontListEntryGetFont")
    :call-direct t
    :callback nil
    :arguments '(foreign-address foreign-address)
    :arg-checking nil
    :return-type :unsigned-integer)

(defforeign 'xm_font_list_next_entry
    :entry-point (ff:convert-to-lang "XmFontListNextEntry")
    :call-direct t
    :callback nil
    :arguments '(foreign-address)
    :arg-checking nil
    :return-type :unsigned-integer)
