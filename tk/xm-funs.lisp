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
;; $fiHeader: xm-funs.lisp,v 1.4 92/04/28 09:25:09 cer Exp Locker: cer $

;;
;; This file contains compile time only code -- put in clim-debug.fasl.
;;

(in-package :tk)

(defforeign 'xm_string_create_l_to_r
    :entry-point "_XmStringCreateLtoR"
    :return-type :integer)

(defforeign 'xm_string_get_l_to_r
    :entry-point "_XmStringGetLtoR"
    :return-type :integer)

(defforeign 'xm_get_pixmap
    :entry-point "_XmGetPixmap")

(defforeign 'xm_font_list_init_font_context
    :entry-point "_XmFontListInitFontContext")

(defforeign 'xm_font_list_free_font_context
    :entry-point "_XmFontListFreeFontContext")

(defforeign 'xm_font_list_get_next_font
    :entry-point "_XmFontListGetNextFont")

(defforeign 'xm_font_list_create
    :entry-point "_XmFontListCreate")

(defforeign 'xm_add_protocol_callback
    :entry-point "_XmAddProtocolCallback")

(defforeign 'xm_intern_atom 
    :entry-point "_XmInternAtom")

(defforeign 'xm_main_window_set_area :entry-point
  "_XmMainWindowSetAreas")

(ff:defforeign 'xm_process_traversal
    :entry-point "_XmProcessTraversal")

(defforeign 'xm-message-box-get-child
    :entry-point "_XmMessageBoxGetChild")


(defforeign 'xm_file_selection_do_search
    :entry-point "_XmFileSelectionDoSearch")

(defforeign 'xm_option_label_gadget
    :entry-point "_XmOptionLabelGadget")

(defforeign 'xm_option_button_gadget
    :entry-point "_XmOptionButtonGadget")
