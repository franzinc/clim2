;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLOE-CLIM; Base: 10; Lowercase: Yes -*-

;; $fiHeader: cloe-dc.lisp,v 1.1 92/10/01 10:03:56 cer Exp $

(in-package :cloe-clim)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."


(eval-when (compile load eval)
;; Define the new key chars for Cloe CLIM.  Regular Cloe defines 0-127, we define
;; 128-139 as the F-keys (F1 thru F12), 140 for c-sh-A, and 141 as c-sh-V
(sys::define-character-name "F1" 128)
(sys::define-character-name "F2" 129)
(sys::define-character-name "F3" 130)
(sys::define-character-name "F4" 131)
(sys::define-character-name "F5" 132)
(sys::define-character-name "F6" 133)
(sys::define-character-name "F7" 134)
(sys::define-character-name "F8" 135)
(sys::define-character-name "F9" 136)
;; Note windows traps F10 as alt-space. Why?
(sys::define-character-name "F10" 137)
(sys::define-character-name "F11" 138)
(sys::define-character-name "F12" 139)
(sys::define-character-name "Arglist" 140)
(sys::define-character-name "ShowValue" 141)
)	;eval-when

;;;

(defvar *dc*)

(defvar *current-pen*)
(defvar *current-brush*)
(defvar *current-rop2*)
(defvar *current-background-color*)
(defvar *current-text-color*)
(defvar *current-font*)

;;; Stock objects
(defvar *null-pen*)
(defvar *black-pen*)
(defvar *white-pen*)

(defvar *null-brush*)
(defvar *black-brush*)
(defvar *white-brush*)

;;; Original objects
(defvar *color-to-image* (make-hash-table))
(defvar *black-image*)
(defvar *white-image*)

(defvar *ink-to-image* (make-hash-table))


(defstruct (dc-image (:predicate nil))
  (bitmap nil)
  solid-1-pen
  (pen-table (make-hash-table))
  brush
  (rop2 win::r2_copypen)
  text-color
  background-color)

(defun initialize-dc ()
  (unless win::*windows-channel*
    (win::connect-to-winfe))
  ;; Dummy window to represent class.
  (setf *dc* (win::create-window "Vanilla" "CLIM" win::ws_popup 0 0 0 0 0 0 0 "arg"))

  ;; Stock objects
  (setf *null-pen* (win::get-stock-object win::null_pen))
  (setf *black-pen* (win::get-stock-object win::black_pen))
  (setf *white-pen* (win::get-stock-object win::white_pen))
  (setf *current-pen* *black-pen*)

  (setf *null-brush* (win::get-stock-object win::null_brush))
  (setf *black-brush* (win::get-stock-object win::black_brush))
  (setf *white-brush* (win::get-stock-object win::white_brush))
  (setf *current-brush* *white-brush*)

  (setf *current-rop2* win::r2_copypen)
  (setf *current-background-color* #xffffff)
  (setf *current-text-color* #x000000)
  (setf *current-font* nil)

  (setf *black-image*
	(make-dc-image :solid-1-pen *black-pen* :brush *black-brush*
		       :text-color #x000000 :background-color nil))
  (setf (gethash #x000000 *color-to-image*) *black-image*)
  (setf *white-image*
	(make-dc-image :solid-1-pen *white-pen* :brush *white-brush*
		       :text-color #xffffff :background-color nil))
  (setf (gethash #xffffff *color-to-image*) *white-image*)
  )

;;;

(defun select-pen (pen)
  (unless (eql *current-pen* pen)
    (win::select-pen *dc* pen)
    (setf *current-pen* pen))
  pen)

(defun select-brush (brush)
  (unless (eql *current-brush* brush)
    (win::select-brush *dc* brush)
    (setf *current-brush* brush))
  brush)

(defun select-rop2 (rop2)
  (unless (eql *current-rop2* rop2)
    (win::set-rop2 *dc* rop2)
    (setf *current-rop2* rop2))
  rop2)

(defun select-background-color (color)
  (unless (eql *current-background-color* color)
    (cond ((null color)
	   #||(win::set-background-mode *dc* win::transparent)||#)
	  (t
	   #||(when (null *current-background-color*)
		(win::set-background-mode *dc* win::opaque))||#
	   (win::set-background-color *dc* color)
    (setf *current-background-color* color))))
  color)

(defun select-text-color (color)
  (unless (eql *current-text-color* color)
    (win::set-text-color *dc* color)
    (setf *current-text-color* color))
  color)

(defun select-font (font)
  (unless (eql *current-font* font)
    (win::select-font *dc* font)
    (setf *current-font* font))
  font)

;;;

(defun set-dc-for-drawing (image line-style)
  (let ((dashes (line-style-dashes line-style)))
    (select-pen
      (let* ((thickness (max 1 (round (line-style-thickness line-style))))
	     (code (if dashes (- thickness) thickness)))
	(declare (fixnum thickness code))
	(if (= code 1)
	    (dc-image-solid-1-pen image)
	    (or (gethash code (dc-image-pen-table image))
		(setf (gethash code (dc-image-pen-table image))
		      (win::create-pen (if dashes win::ps_dash win::ps_solid)
				       thickness
				       (dc-image-text-color image)))))))
    (when dashes
      (select-background-color nil)))
  (select-brush *null-brush*)
  (select-rop2 (dc-image-rop2 image))
  image)

(defun set-dc-for-filling (image)
  (select-pen *null-pen*)
  (select-brush (dc-image-brush image))
  (let ((background-color (dc-image-background-color image)))
    (when background-color
      (select-background-color background-color)))
  (select-rop2 (dc-image-rop2 image)))

(defun set-dc-for-ink (medium ink line-style)
  (let ((image (dc-image-for-ink medium ink)))
    (if line-style
	(set-dc-for-drawing image line-style)
	(set-dc-for-filling image))))

(defun set-dc-for-text (medium ink font)
  (select-text-color (dc-image-text-color (dc-image-for-ink medium ink)))
  (select-background-color nil)
  (select-font font))

(defgeneric dc-image-for-ink (medium ink))
