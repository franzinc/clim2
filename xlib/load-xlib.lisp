;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2004 Franz Inc, Oakland, CA - All rights reserved.
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
;; $Id: load-xlib.lisp,v 2.5 2004/01/16 19:15:45 layer Exp $

(in-package :x11)

#+(and (not (version>= 5 0)) (not dlfcn))
(progn

  (defvar sys::*libx11-pathname* "X11")

  (unless (ff:get-entry-point (ff:convert-foreign-name "XAllocColor"))
    (load "clim2:;stub-x.o"
	  :system-libraries (list sys::*libx11-pathname*)
	  :print t))

  (unless (ff:get-entry-point (ff:convert-foreign-name "lisp_XDrawString"))
    (load "clim2:;xlibsupport.o"
	  :system-libraries (list sys::*libx11-pathname*)
	  :print t)))
