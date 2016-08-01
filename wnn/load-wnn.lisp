;; -*- mode: common-lisp; package: user -*-
;;
;; See the file LICENSE for the full license governing this code.
;;

(in-package :user)

;;; compare with tk/load-xm for update to new makefile style

(excl:ics-target-case
(:+ics

#+(version>= 5 0)
(unless (ff:get-entry-point (ff:convert-foreign-name "jl_open_lang"))
  (load (merge-pathnames (make-pathname
			  :type (or (car excl::*load-foreign-types*)
				    (error "Don't know foreign extension.")))
			 "clim2:;wnn")))

#+(and (not (version>= 5 0)) dlfcn)
(unless (ff:get-entry-point (ff:convert-foreign-name "jl_open_lang")
			    :note-shared-library-references
			    nil)
  (load "clim2:;wnn.so"))

#+(and (not (version>= 5 0)) (not dlfcn))
(progn
  (defvar sys::*libwnn-pathname* "wnn")

  (unless (ff:get-entry-point (ff:convert-foreign-name "jl_open_lang"))
    (load "stub-wnn.o"
	  :system-libraries (list sys::*libwnn-pathname*)
	  :print t)))

(provide :wnn)
(pushnew :wnn *features*)

)) ;; ics-target-case


