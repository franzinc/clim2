;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-


(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

(defun run-cloe-application (name)
  (win::start-windows)
  (unwind-protect
      (progn
	(win::show-window (win::get-term-window) :type :minimize)
	(run-frame-top-level (make-application-frame name :parent (open-root-window :cloe))))
    (win::stop-windows)))

(defun cloe-debugger-hook (condition hook)
  (declare (ignore hook))
  (let ((term (win::get-term-window)))
    (win::show-window term :type :show-normal)
    (unwind-protect
        (invoke-debugger condition)
      (when (frame-top-level-sheet *application-frame*)
        (win::show-window term :type :minimize)))))
