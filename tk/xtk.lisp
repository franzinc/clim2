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
;; $fiHeader$

(in-package :tk)

(def-c-typedef :cardinal :unsigned-int)

(def-c-type (xtk-class :in-foreign-space) :struct
  (superclass :long)
  (name * :char)
  (widget-size :cardinal)
  (class-part-initialize * :unsigned-long)
  (inited :unsigned-long))

(def-c-type (x-resource :in-foreign-space) :struct
  (name * :char)
  (class * :char)
  (type * :char)
  (size :cardinal)
  (offset :cardinal)
  (default-type * :char)
  (default-addr * :char)
  )

(def-c-type (xtk-widget :in-foreign-space) :struct
  (self :unsigned-long)
  (widget-class :unsigned-long)
  )

(def-c-type (x-resource-list :in-foreign-space) 1 x-resource)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defforeign 'get-resource-list-1 :entry-point "_XtGetResourceList")
(defforeign 'get-constraint-resource-list-1 :entry-point "_XtGetConstraintResourceList")

(defforeign 'initialize-widget-class :entry-point "_XtInitializeWidgetClass")

(defforeign 'x-free :entry-point "_XtFree")

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
