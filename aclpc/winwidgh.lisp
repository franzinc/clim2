;;; -*- Package: acl-clim; mode: Common-Lisp -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; $Id: winwidgh.lisp,v 2.7 2006/04/06 23:23:05 layer Exp $

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
	     (pidlRoot win:lpcitemidlist)
	     (pszDisplayName win:lpstr)
	     (lpszTitle win:lpcstr)
	     (ulflags win:uint)
	     (lpfn (* :void)) ; BFFCALLBACK
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
    ((info (* browseinfo)))
  :returning win:pvoid  ; LPITEMIDLIST
  :release-heap :when-ok)

(ff:def-foreign-call (FormatMessage "FormatMessageA")
    ((flags :int)
     (source (* :long))
     (messageid :int)
     (languageid :int)
     (buffer (* :long))
     (size :int)
     (arguments (* :long)))
  :arg-checking #.cl-user::*ffi-arg-checking*
  :returning :int)

;; This should be equivalent to win:createpen but not cons.
(ff:def-foreign-call (CreatePen "CreatePen")
    ((flags :int) (source :int) (color :int))
  :arg-checking #.cl-user::*ffi-arg-checking*
  :call-direct #.cl-user::*ffi-call-direct*
  :returning :int)

;; This should be equivalent to win:createrectrgn but not cons.
(ff:def-foreign-call (CreateRectRgn "CreateRectRgn")
    ((left :int) (top :int) (right :int) (bottom :int))
  :arg-checking #.cl-user::*ffi-arg-checking*
  :call-direct #.cl-user::*ffi-call-direct*
  :returning :int)

;; This should be equivalent to win:getdc but not cons.
(ff:def-foreign-call (GetDC "GetDC")
    ((window win:hwnd))
  :arg-checking #.cl-user::*ffi-arg-checking*
  :call-direct #.cl-user::*ffi-call-direct*
  :returning win:hdc)

;; This should be equivalent to win:getdc but not cons.
(ff:def-foreign-call (ReleaseDC "ReleaseDC")
    ((window win:hwnd) (dc win:hdc))
  :arg-checking #.cl-user::*ffi-arg-checking*
  :call-direct #.cl-user::*ffi-call-direct*
  :returning :int)

(ff:def-foreign-call (SetBkMode "SetBkMode")
    ((dc win:hdc) (mode :int))
  :arg-checking #.cl-user::*ffi-arg-checking*
  :call-direct #.cl-user::*ffi-call-direct*
  :returning :int)

(ff:def-foreign-call (SetBkColor "SetBkColor")
    ((dc win:hdc) (color :int))
  :arg-checking #.cl-user::*ffi-arg-checking*
  :call-direct #.cl-user::*ffi-call-direct*
  :returning :int)

(ff:def-foreign-call (SetTextColor "SetTextColor")
    ((dc win:hdc) (color :int))
  :arg-checking #.cl-user::*ffi-arg-checking*
  :call-direct #.cl-user::*ffi-call-direct*
  :returning :int)

(ff:def-foreign-call (SetROP2 "SetROP2")
    ((dc win:hdc) (rop2 :int))
  :arg-checking #.cl-user::*ffi-arg-checking*
  :call-direct #.cl-user::*ffi-call-direct*
  :returning :int)

;; This should be equivalent to win:selectobject but not cons.
(ff:def-foreign-call (SelectObject "SelectObject")
    ((a win:hdc) (b win:hpen))
  :arg-checking #.cl-user::*ffi-arg-checking*
  :call-direct #.cl-user::*ffi-call-direct*
  :returning win:hpen)

(ff:def-foreign-call (SetWindowsHookEx "SetWindowsHookExA")
    ((a :int) (b win:hookproc) (c win:hinstance) (d win:dword))
  :returning win:pvoid)

(ff:def-foreign-call (CallNextHookEx "CallNextHookExA")
    ((a (* :nat))
     (b :int)
     (c win:wparam)
     (d win:lparam))
  :returning win:lresult)

;;; These are used only in the CreateDIBitmap code
;;;

(ff:def-foreign-call memcpy
    ((to (* :void)) (from (* :void)) (nbytes :int))
  ;; really returns (* :void) but can't hack that (why?)
  :returning :int)

(ff:def-foreign-call (system-malloc "malloc")
    ((bytes :int))
  ;; really (* :void)
  :returning win:pvoid)

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
