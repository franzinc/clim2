;;; -*- Package: win; mode: Common-Lisp -*-
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
;; $Id: winwidgh.lisp,v 1.3 1998/08/06 23:15:46 layer Exp $

(in-package :acl-clim)

;; These seem to be missing from winapi-dev
(defconstant LBS_DISABLENOSCROLL #x1000)
(defconstant HLN_SELCHANGE #x1)
(defconstant CB_SETTOPINDEX #x015c)

(ff:def-foreign-type browseinfo
    (:struct (hwndOwner win:hwnd)
	     (pidlRoot win:long #+ig win:lpcitemidlist)
	     (pszDisplayName win:lpstr)
	     (lpszTitle win:lpcstr)
	     (ulflags win:uint)
	     (lpfn (* :void) #+ig win:bffcallback)
	     (lparam win:lparam)
	     (iImage :int)))

(ff:def-foreign-call (SHBrowseForFolder "SHBrowseForFolder")
    ((info browseinfo))
  :returning :int
  :release-heap :when-ok)

(ff:def-foreign-call (FormatMessage "FormatMessageA")
    ((flags :int) (source :int) (messageid :int)
		  (languageid :int) (buffer :int)
		  (size :int) (arguments :int))
  :arg-checking nil
  :returning :int)

;; This should be equivalent to win:createpen but not cons.
(ff:def-foreign-call (CreatePen "CreatePen")
    ((flags :int) (source :int) (color :int))
  :arg-checking nil
  :call-direct t
  :returning :int)

;; This should be equivalent to win:createrectrgn but not cons.
(ff:def-foreign-call (CreateRectRgn "CreateRectRgn")
    ((left :int) (top :int) (right :int) (bottom :int))
  :arg-checking nil
  :call-direct t
  :returning :int)

;; This should be equivalent to win:getdc but not cons.
(ff:def-foreign-call (GetDC "GetDC")
    ((window :int))
  :arg-checking nil
  :call-direct t
  :returning :int)

;; This should be equivalent to win:selectobject but not cons.
(ff:def-foreign-call (SelectObject "SelectObject")
    ((a :int) (b :int))
  :arg-checking nil
  :call-direct t
  :returning :int)
