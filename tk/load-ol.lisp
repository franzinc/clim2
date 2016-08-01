;; -*- mode: common-lisp; package: user -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :user)

(require :climg)

#+dlfcn
(progn
  (defvar sys::*toolkit-shared* nil)

  (unless (ff:get-entry-point (ff:convert-foreign-name "ol_appl_add_item")
			      #-(version>= 5 0) :note-shared-library-references
			      #-(version>= 5 0) nil)
    (load "clim2:;climol.so")
    (setq sys::*toolkit-shared* t)))

#-dlfcn
(progn
  (defvar sys::*libtk-pathname* "Ol")
  (defvar sys::*libxt-pathname* "Xt")

  (unless (ff:get-entry-point (ff:convert-foreign-name "XtToolkitInitialize"))
    (load "stub-olit.o"
	  :system-libraries (list sys::*libtk-pathname*
				  sys::*libxt-pathname*
				  sys::*libx11-pathname*)
	  :print t)
    (load "stub-xt.o"
	  :system-libraries (list sys::*libxt-pathname*
				  sys::*libx11-pathname*)
	  :print t))

  (unless (ff:get-entry-point (ff:convert-foreign-name "ol_appl_add_item"))
    (load "olsupport.o"
	  :system-libraries (list sys::*libtk-pathname*
				  sys::*libxt-pathname*
				  sys::*libx11-pathname*)
	  :print t))

  (unless (ff:get-entry-point (ff:convert-foreign-name "XtAppIntervalNextTimer"))
    (load "xtsupport.o"
	  :system-libraries (list sys::*libxt-pathname*
				  sys::*libx11-pathname*)
	  :print t)))

(pushnew :clim-openlook *features*)
