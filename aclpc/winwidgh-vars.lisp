;;; -*- Package: acl-clim; mode: Common-Lisp -*-
;; See the file LICENSE for the full license governing this code.
;;

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

