;;; -*- Mode: LISP; Syntax: Common-lisp; Package: hpgl-clim; Base: 10; Lowercase: Yes -*-

;; $Header: /repo/cvs.copy/clim2/test/hpgl-tests.lisp,v 1.3 1997/02/05 01:52:03 tomj Exp $

(in-package :clim-user)

(defmethod invoke-with-output-to-printer-stream ((type (eql :hpgl)) stream continuation)
  (hpgl-clim:with-output-to-hpgl-stream (*standard-output* stream)
    (funcall continuation)))

(defmethod invoke-with-pipe-to-printer ((type (eql :hpgl)) continuation)
  (with-open-stream 
      (printer-stream
       (excl:run-shell-command (format nil "/usr/tech/cer/3rd/hpgl2ps-v2/hpgl2ps | lpr -P~A" *allegro-printer*)
			       :input :stream :wait
			       nil))
    (funcall continuation printer-stream)))

(defun run-hpgl-tests (&key (output :view))
  (run-printer-tests output :hpgl))
