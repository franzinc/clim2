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
;; $Id: gc-cursor.lisp,v 2.6.6.1 2006/01/28 14:12:50 moore Exp $

(in-package :xm-silica)

(defvar *use-clim-gc-cursor* nil)
(defvar *gc-before* nil)
(defvar *gc-after*  nil)

(deftype c-pointer ()
  '(unsigned-byte #+64bit 64 #-64bit 32))

(defun init-gc-cursor (frame &optional force)
  (when *use-clim-gc-cursor*
    (when (or force (null *gc-before*))
      (setq *gc-before* (ff:get-entry-point "clim_starting_gc")
	    *gc-after*  (ff:get-entry-point "clim_stopping_gc"))
      (pushnew (make-array 1 :element-type 'c-pointer
			   :initial-element *gc-before*)
	       (excl:gc-before-c-hooks))
      (pushnew (make-array 1 :element-type 'c-pointer
			   :initial-element *gc-after*)
	       (excl:gc-after-c-hooks)))
    (let* ((sheet (frame-top-level-sheet frame))
	   (mirror (and sheet (sheet-direct-mirror sheet))))
      (if mirror
	  (tk::set_clim_gc_cursor_widget
	   mirror
	   (realize-cursor (port sheet)
			   sheet
			   (sheet-pointer-cursor sheet)))
	(tk::set_clim_gc_cursor_widget 0 0)))))

(defun reinitialize-gc-cursor ()
  ;; Force init-gc-cursor to go through its initialization
  ;; code the next time it is called.
  (setq *gc-before* nil)
  (setq *gc-after* nil))

(pushnew 'reinitialize-gc-cursor excl::*restart-actions*)

(defmethod (setf sheet-pointer-cursor) :after (cursor (sheet xt-top-level-sheet))
  (declare (ignore cursor))
  (init-gc-cursor (pane-frame sheet)))

(defmethod clim-internals::receive-gesture :after ((stream xt-top-level-sheet)
						   (gesture pointer-enter-event))
  ;; If the top level window has a cursor we need to pass that in somehow
  ;; so that it gets restored appropriately.
  (unless (eq (pointer-boundary-event-kind gesture) :inferior)
    (init-gc-cursor (pane-frame stream))))


