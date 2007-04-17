;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLX-CLIM; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2007 Franz Inc, Oakland, CA - All rights reserved.
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
;; $Id: clx-frames.lisp,v 2.7 2007/04/17 21:45:51 layer Exp $

(in-package :clx-clim)

;;;"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."


(defclass clx-frame-manager (standard-frame-manager)
    ()
  (:default-initargs :dialog-view +textual-dialog-view+))

(defmethod make-frame-manager ((port clx-port) &key palette &allow-other-keys)
  (make-instance 'clx-frame-manager 
    :port port :palette palette))

(defmethod frame-wrapper ((framem clx-frame-manager) 
			  (frame standard-application-frame) pane)
  (let ((menu-bar (slot-value frame 'menu-bar)))
    (with-look-and-feel-realization (framem frame)
      (outlining ()
	(if menu-bar
	    (vertically ()
	      (compute-menu-bar-pane frame menu-bar)
	      pane)
	    pane)))))
