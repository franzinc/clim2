;; -*- mode: common-lisp; package: xm-silica -*-
;;
;;				-[Fri Nov 11 15:11:52 1994 by smh]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1992 Franz Inc, Berkeley, CA  All rights reserved.
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
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: gc-cursor.lisp,v 1.1 93/05/13 16:24:48 cer Exp $


(in-package :xm-silica)

(defvar *use-clim-gc-cursor* nil)
(defvar *gc-before* nil)
(defvar *gc-after*  nil)

(defun init-gc-cursor (frame)
  (when *use-clim-gc-cursor*
    (unless *gc-before*			; Do just once.
      (let ((vec (vector nil nil)))
	(format excl:*initial-terminal-io* "~&Vec before ~s~%" vec)
	(tk::init_clim_gc_cursor_stuff vec)
	(format excl:*initial-terminal-io* "~&Vec after  ~s~%" vec)
	(force-output excl:*initial-terminal-io*)
	(setq *gc-before* (svref vec 0)
	      *gc-after*  (svref vec 1))
	(pushnew *gc-before* (excl::gc-before-hooks))
	(pushnew *gc-after*  (excl::gc-after-hooks))
	))
    (let* ((sheet (frame-top-level-sheet frame))
	   (mirror (and sheet (sheet-direct-mirror sheet))))
      (if mirror
	  (tk::set_clim_gc_cursor_widget 
	   mirror
	   (let ((cursor (sheet-pointer-cursor sheet)))
	     (if cursor
		 (realize-cursor (port sheet) sheet cursor)
	       0)))
	(tk::set_clim_gc_cursor_widget 0 0)))))

(defmethod (setf sheet-pointer-cursor) :after (cursor (sheet xt-top-level-sheet))
  (declare (ignore cursor))
  (init-gc-cursor (pane-frame sheet)))

(defmethod clim-internals::receive-gesture :after ((stream xt-top-level-sheet) 
						   (gesture pointer-enter-event))
  ;; If the top level window has a cursor we need to pass that in somehow
  ;; so that it gets restored appropriately.
  (unless (eq (pointer-boundary-event-kind gesture) :inferior)
    (init-gc-cursor (pane-frame stream))))


