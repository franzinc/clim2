;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLOE-CLIM; Base: 10; Lowercase: Yes -*-

;; $Header: /repo/cvs.copy/clim2/cloe/cloe-activities.lisp,v 1.4 1997/02/05 01:45:49 tomj Exp $

(in-package :cloe-clim)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."


(defun run-cloe-application (name &key (port (find-port)))
  (win::show-window (win::get-term-window) :type :minimize)
  (run-frame-top-level 
    (make-application-frame name :frame-manager (find-frame-manager :port port))))

(defun cloe-debugger-hook (condition hook)
  (declare (ignore hook))
  (let ((term (win::get-term-window)))
    (win::show-window term :type :show-normal)
    (unwind-protect
        (invoke-debugger condition)
      (when (frame-top-level-sheet *application-frame*)
        (win::show-window term :type :minimize)))))
