;;; -*- Package: acl-clim; mode: Common-Lisp -*-
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
;; $Id: winwidgh.lisp,v 1.7.38.1 2000/08/15 15:19:12 layer Exp $

(in-package :acl-clim)

;; These seem to be missing from winapi-dev
(defconstant LBS_DISABLENOSCROLL #x1000)
(defconstant CB_SETTOPINDEX #x015c)
(defconstant TTN_FIRST -520)
(defconstant TTN_NEEDTEXTA TTN_FIRST)	; ascii
(defconstant TTN_NEEDTEXTW (- TTN_FIRST 10)); unicode
(defconstant TTN_NEEDTEXT TTN_NEEDTEXTA)

(defconstant EM_SETMARGINS #xD3)
(defconstant EC_LEFTMARGIN 1)
(defconstant EC_RIGHTMARGIN 2)
(defconstant EC_USEFONTINFO #xffff)

(defvar SRCOR #xee0086)

(ff:def-foreign-type browseinfo
    (:struct (hwndOwner win:hwnd)
	     (pidlRoot win:long #+ig win:lpcitemidlist)
	     (pszDisplayName win:lpstr)
	     (lpszTitle win:lpcstr)
	     (ulflags win:uint)
	     (lpfn (* :void) #+ig win:bffcallback)
	     (lparam win:lparam)
	     (iImage :int)))

(ff:def-foreign-type toolinfo
    (:struct (cbsize win:uint)
	     (uflags win:uint)
	     (hwnd win:hwnd)
	     (uid win:uint)
	     (rect win:rect)
	     (hinst win:hinstance)
	     (lpsztext win:lpstr)
	     (lparam win:lparam)))

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

;; This should be equivalent to win:getdc but not cons.
(ff:def-foreign-call (ReleaseDC "ReleaseDC")
    ((window :int) (dc :int))
  :arg-checking nil
  :call-direct t
  :returning :int)

(ff:def-foreign-call (SetBkMode "SetBkMode")
    ((dc :int) (mode :int))
  :arg-checking nil
  :call-direct t
  :returning :int)

(ff:def-foreign-call (SetBkColor "SetBkColor")
    ((dc :int) (color :int))
  :arg-checking nil
  :call-direct t
  :returning :int)

(ff:def-foreign-call (SetTextColor "SetTextColor")
    ((dc :int) (color :int))
  :arg-checking nil
  :call-direct t
  :returning :int)

(ff:def-foreign-call (SetROP2 "SetROP2")
    ((dc :int) (rop2 :int))
  :arg-checking nil
  :call-direct t
  :returning :int)

;; This should be equivalent to win:selectobject but not cons.
(ff:def-foreign-call (SelectObject "SelectObject")
    ((a :int) (b :int))
  :arg-checking nil
  :call-direct t
  :returning :int)

(ff:def-foreign-call (SetWindowsHookEx "SetWindowsHookExA")
    ((a :int) (b :int) (c :int) (d :int))
  :returning :int)

(ff:def-foreign-call (CallNextHookEx "CallNextHookExA")
    ((a :int) (b :int) (c :int) (d :int))
  :returning :int)

;;; These are used only in the CreateDIBitmap code
;;;

(ff:def-foreign-call memcpy
    ((to (* :void)) (from (* :void)) (nbytes :int))
  ;; really returns (* :void) but can't hack that (why?)
  :returning :int)

(ff:def-foreign-call (system-malloc "malloc")
    ((bytes :int))
  ;; really (* :void)
  :returning :int)

(ff:def-foreign-call (system-free "free")
    ((address (* :void)))
  :returning :void)

(defmacro with-malloced-space ((address-var bytes) &body body)
  ;; Ensure the space is freed
  `(let ((,address-var (system-malloc ,bytes)))
     (when (zerop ,address-var)
       (error "Failed to malloc ~A bytes" ,bytes))
     (unwind-protect
	 (progn ,@body)
       (system-free ,address-var))))
