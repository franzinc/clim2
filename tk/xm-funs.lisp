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
;; $fiHeader: xm-funs.lisp,v 1.1 92/03/30 17:58:42 cer Exp Locker: cer $

(in-package :tk)

(defforeign 'string_create_l_to_r
    :entry-point "_XmStringCreateLtoR"
    :return-type :integer)

(defforeign 'string_get_l_to_r
    :entry-point "_XmStringGetLtoR"
    :return-type :integer)

(defforeign 'get_pixmap
    :entry-point "_XmGetPixmap")

(defforeign 'xm-message-box-get-child
    :entry-point "_XmMessageBoxGetChild")
