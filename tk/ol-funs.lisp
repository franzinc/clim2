;; -*- mode: common-lisp; package: tk -*-
;;
;;				-[]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, CA  All rights reserved.
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
;; $fiHeader: ol-funs.lisp,v 1.4 92/06/23 08:19:14 cer Exp Locker: cer $

;;
;; This file contains compile time only code -- put in clim-debug.fasl.
;;

(in-package :tk)

(defforeign 'ol_toolkit_initialize :entry-point "_OlToolkitInitialize")

(defforeign 'ol_set_warning_handler 
    :entry-point "_OlSetWarningHandler")

(defforeign 'ol_set_error_handler 
  :entry-point "_OlSetErrorHandler")

(defforeign 'ol_set_va_display_error_msg_handler
    :entry-point "_OlSetVaDisplayErrorMsgHandler")

(defforeign 'ol_set_va_display_warning_msg_handler
    :entry-point "_OlSetVaDisplayWarningMsgHandler")

(defforeign 'ol_add_callback :entry-point "_OlAddCallback")

(defforeign 'ol_set_input_focus :entry-point "_OlSetInputFocus")

(defforeign 'ol_menu_post :entry-point "_OlMenuPost")
(defforeign 'ol_menu_popdown :entry-point "_OlMenuPopdown")
(defforeign 'ol_appl_add_item :entry-point "_ol_appl_add_item")
(defforeign 'ol_list_item_pointer :entry-point "_ol_list_item_pointer")

(defforeign 'ol_appl_touch_item :entry-point "_ol_appl_touch_item")

(defforeign 'ol_text_edit_clear_buffer :entry-point "_OlTextEditClearBuffer")
(defforeign 'ol_text_edit_read_substring :entry-point "_OlTextEditReadSubString")
(defforeign 'ol_text_edit_insert :entry-point "_OlTextEditInsert")
(defforeign 'ol_text_edit_get_last_position :entry-point "_OlTextEditGetLastPosition")
(defforeign 'ol_register_help :entry-point "_OlRegisterHelp")
