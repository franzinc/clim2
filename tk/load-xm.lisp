;; -*- mode: common-lisp; package: user -*-
;; See the file LICENSE for the full license governing this code.
;;

(cl:in-package #:user)

(eval-when (compile eval load)
  (require :climg))

(defvar sys::*clim-library-search-path* '("/usr/X11/lib/" "/usr/X11R6/lib/" "/usr/local/lib/"
                                          "/opt/local/lib/" "/sw/lib/"))

;;; On Mac OS X, the linker hard-codes the paths to libraries into
;;; climxm.dylib, so we instruct it to link weakly against Motif (so
;;; it doesn't complain if the file isn't found), and pre-load its
;;; dependency libraries' basenames from known places in the file
;;; system here.
#+macosx
(unless (ff:get-entry-point (ff:convert-foreign-name "XtInitialize"))
  (excl.osi:with-command-output (sys::library (format nil "otool -L ~a"
                                                      (probe-file
                                                       (format nil "clim2:;climxm.~a"
                                                               (car excl::*load-foreign-types*)))))
    (when (eql (elt sys::library 0) #\Tab)
      (let ((sys::library-name (nth-value 2 (match-re ".*/(lib[^ ]+) " sys::library)))
            (values nil))
        (unless (dolist (directory sys::*clim-library-search-path*)
                  (let ((pathname (merge-pathnames sys::library-name directory)))
                    (when (probe-file pathname)
                      (handler-case (progn (load pathname)
                                           (return t))
                        (error (e) (push (list pathname e) values))))))
          (error "Can't find ~a in ~{~a~^, ~}~@[~%Paths that had load errors:~%~:{~a:~t~a~%~}~]"
                 sys::library-name sys::*clim-library-search-path*
                 values))))))

#+(version>= 5 0)
(unless (ff:get-entry-point (ff:convert-foreign-name "XmCreateMyDrawingArea"))
  (let (
;;;; See spr12165 for the *dlopen-mode* hack explanation.
	#+(and dlfcn sun4 (not sunos4))
	(excl::*dlopen-mode* (excl:ics-target-case
			      (:+ics #x102)
			      (:-ics excl::*dlopen-mode*))))
    (load (merge-pathnames (make-pathname
			    :type (or (car excl::*load-foreign-types*)
				      (error "Don't know foreign extension.")))
			   "clim2:;climxm"))))

#+(and (not (version>= 5 0)) dlfcn)
(unless (ff:get-entry-point (ff:convert-to-lang "XmCreateMyDrawingArea")
			    :note-shared-library-references
			    nil)
  (let ((ff::*dlopen-mode* (excl:ics-target-case
			    (:+ics #x102)
			    (:-ics ff::*dlopen-mode*))))
    (load "clim2:;climxm.so")))

#+(and (not (version>= 5 0)) (not dlfcn))
(progn

(defvar sys::*libtk-pathname* "Xm")
(defvar sys::*libxt-pathname* "Xt")

(unless (ff:get-entry-point (ff:convert-foreign-name "XtToolkitInitialize"))
  (load "clim2:;stub-motif.o"
	:system-libraries (list sys::*libtk-pathname*
				sys::*libxt-pathname*
				sys::*libx11-pathname*)
	:print t)
  (load "clim2:;stub-xt.o"
	:system-libraries (list sys::*libxt-pathname*
				sys::*libx11-pathname*)
	:print t))

(unless (ff:get-entry-point (ff:convert-foreign-name "XmCreateMyDrawingArea"))
  (load "clim2:;xmsupport.o"
	:system-libraries (list sys::*libtk-pathname*
				sys::*libxt-pathname*
				sys::*libx11-pathname*)
	:print t))

(unless (ff:get-entry-point (ff:convert-foreign-name "XtAppIntervalNextTimer"))
  (load "clim2:;xtsupport.o"
	:system-libraries (list sys::*libxt-pathname*
				sys::*libx11-pathname*)
	:print t))
)

(pushnew :clim-motif *features*)
