;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LUCID; Base: 10 -*-
;;;
;;;; Lucid-before-patches, Module CLIM
;;;
;;; ***************************************************************************
;;;
;;;        Copyright (C) 1991 by Lucid, Inc.  All Rights Reserved
;;;
;;; ***************************************************************************
;;;
;;; Lucid specific hacks which need to be used by CLIM.
;;;
;;;
;;; Edit-History:
;;;
;;; Created: PW  2-Nov-91
;;;
;;;
;;; End-of-Edit-History


(in-package :lucid)

;;; $fiHeader$
;;; kludge to check for the presence of NEWCHED
#+ignore
(when (boundp '*minimum-os-time-quantum*)
  (defun %sleep (time)
    (when (> time 0)
      (let ((ms-time (round time (/ internal-time-units-per-second 1000))))
	(if (< ms-time *minimum-os-time-quantum*) ; in msec's
	    (process-allow-schedule)
	    (process-wait "Sleep"
	      #'(lambda (wakeup-time)
		  (time> (get-ms-time) wakeup-time))
	      (time-increment (get-ms-time) ms-time)))))))


;;; load-time-value isn't until 4.1.  We have can't define this in :LCL because
;;; CLOS gets it from :LUCID.

(defmacro load-time-value (form &optional read-only-p)
  (interpretive-load-time-value-expander form read-only-p))

(defun interpretive-load-time-value-expander (form read-only-p)
  (unless (or (eq read-only-p nil)
	      (eq read-only-p t))
    (check-type read-only-p (member nil t)))
  (let ((cached-value-var (gentemp "LoadTimeValueCache" *clos-package*)))
    (proclaim `(special ,cached-value-var))
    `(if (boundp ',cached-value-var)
	 ,cached-value-var
	 (setq ,cached-value-var ,form))))

(export '(load-time-value) :lucid)
