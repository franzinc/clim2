;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: ACL-CLIM; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-1998 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; $Id: acl-dc.lisp,v 1.9 1999/05/04 01:21:00 layer Exp $

#|****************************************************************************
*                                                                            *
*                                                                            *
*  This file implements the lowest levels of Windows drawing activity, the   *
*  management of Device Contexts, Pens, Brushes and other resources.         *
*                                                                            *
*                                                                            *
****************************************************************************|#


(in-package :acl-clim)

(defvar *current-window*)
(defvar *dc-initialized* nil)

;;; Stock objects
(defvar *null-pen*)
(defvar *black-pen*)
(defvar *white-pen*)
(defvar *ltgray-pen*)

(defvar *null-brush*)
(defvar *black-brush*)
(defvar *white-brush*)
(defvar *ltgray-brush*)

;;; Original objects
(defvar *black-image*)
(defvar *white-image*)
(defvar *blank-image*)
(defvar *ltgray-image*)

;; This is not just any 75% gray.  There are
;; methods to treat this object specially and
;; use COLOR_BTNFACE as the realization of this ink.
(defconstant +ltgray+ (make-gray-color .75))

(defmethod make-load-form ((design (eql (symbol-value '+ltgray+))) &optional environment)
  (declare (ignore environment))
  '+ltgray+)

(defmethod print-object ((object (eql (symbol-value '+ltgray+))) stream)
  (format stream "#<CLIM LtGray>"))

(defvar *original-font* nil)

;;; Created Objects
(defvar *created-pen* nil)
(defvar *created-brush* nil)
(defvar *created-tile* nil)
(defvar *created-bitmap* nil)
(defvar *created-font* nil)
(defvar *created-region* nil)

;; Device context information
(defstruct (dc-image (:predicate nil))
  (bitmapinfo nil)			; colors of unmasked bitmap
  (bitmap nil)				; bits of unmasked bitmap
  solid-1-pen				; style, width, and color of lines
  brush					; color/stipple used to fill polygons
  (rop2 win:R2_COPYPEN)			; set the foreground mix mode
  text-color				; foreground color
  background-color			; background color
  and-bitmap				; AND part of masked bitmap (monochrome)
  and-brush                             ; AND part of masked brush (monochrome)
  )

(defun initialize-dc ()
  (unless (win:IsWindow *current-window*)
    (error "No Window: ~S" *current-window*))
  ;; Stock objects
  (setf *null-pen* (win:GetStockObject win:NULL_PEN))
  (setf *black-pen* (win:GetStockObject win:BLACK_PEN))
  (setf *white-pen* (win:GetStockObject win:WHITE_PEN))
  (setf *ltgray-pen* 
    (createPen win:PS_SOLID 1 (win:GetSysColor win:COLOR_BTNFACE)))
  ;;
  (setf *null-brush* (win:GetStockObject win:NULL_BRUSH))
  (setf *black-brush* (win:GetStockObject win:BLACK_BRUSH))
  (setf *white-brush* (win:GetStockObject win:WHITE_BRUSH))
  (setf *ltgray-brush* 
    (win:CreateSolidBrush (win:GetSysColor win:COLOR_BTNFACE)))
  ;;
  #+obsolete
  (setf *black-image*
    (make-dc-image :solid-1-pen *black-pen*
		   :brush *black-brush*
		   :text-color #x000000
		   :background-color nil
		   :rop2 win:R2_COPYPEN))
  #+obsolete
  (setf *white-image*
    (make-dc-image :solid-1-pen *white-pen*
		   :brush *white-brush*
		   :text-color #xffffff
		   :background-color nil
		   :rop2 win:R2_COPYPEN))
  (setf *blank-image*
    #+possibly
    (make-dc-image :solid-1-pen *black-pen* 
		   :brush *black-brush*
		   :text-color #xffffffff ; see CLR_NONE
                   :background-color nil
		   :rop2 win:R2_MERGEPEN )
    (make-dc-image :solid-1-pen *black-pen*
		   :brush *black-brush*
		   :text-color #x000000 
		   :background-color nil
		   :rop2 win:R2_NOP ))
  ;;
  (setf *ltgray-image*
    (make-dc-image :solid-1-pen *ltgray-pen* 
                   :brush *ltgray-brush*
                   :text-color (win:GetSysColor win:COLOR_BTNFACE)
                   :background-color nil
		   :rop2 win:R2_COPYPEN))
  ;;
  (setq *dc-initialized* t)
  )

;;;

(defvar *original-bitmap* nil)
(defvar *extra-objects* nil)

(defun release-objects (window dc)
  (when *created-pen*
    (selectobject dc *black-pen*)
    (win:DeleteObject *created-pen*)
    (setq *created-pen* nil))
  (when *created-brush*
    (selectobject dc *white-brush*)
    (win:DeleteObject *created-brush*)
    (setq *created-brush* nil))
  (when *created-tile*
    (win:DeleteObject *created-tile*)
    (setq *created-tile* nil))
  (when *created-region*
    (selectobject dc (ct::null-handle win:HRGN))
    (win:DeleteObject *created-region*)
    (setq *created-region* nil))
  (when (and *created-font* *original-font*)
    (selectobject dc *original-font*)
    (win:DeleteObject *created-font*)
    (setq *created-font* nil))     
  (when (and *created-bitmap* *original-bitmap*)
    (selectobject dc *original-bitmap*)
    (win:DeleteObject *created-bitmap*)
    (setq *created-bitmap* nil))
  (dolist (xtra *extra-objects*)
	(win:DeleteObject xtra))
  (setq *extra-objects* nil)
  (win:ReleaseDC window dc))

(defclass acl-pixmap (pixmap)
  ((bitmap :initarg :bitmap)
   (for-medium :initarg :for-medium)
   (cdc :initarg :cdc :reader pixmap-cdc)
   (width :initarg :width :reader pixmap-width)
   (height :initarg :height :reader pixmap-height)
   (original-bitmap :initarg :original-bitmap)))

;; The call to GetDC will cons a bignum (16 bytes).
;; We should probably try to optimize this some how
;; since it is called constantly during simple
;; things like repaint.
(defmacro with-dc ((window dc) &rest body)
  `(if (typep ,window 'acl-pixmap-medium)
       (let ((,dc (pixmap-cdc (medium-drawable ,window))))
	 ,@body)
     ;; It was my intention to rewrite this to
     ;; cache the DC, in combination with setting
     ;; CS_OWNDC.
     (let ((,dc 0))
       (unwind-protect
	   (progn
	     (setq ,dc (getDc ,window))
	     ,@body)
	 (release-objects ,window ,dc)))))

(defmacro with-medium-dc ((medium dc) &rest body)
  `(if (typep ,medium 'acl-pixmap-medium)
       (let ((,dc (pixmap-cdc (medium-drawable ,medium))))
	 ,@body)
     ;; It was my intention to rewrite this to
     ;; cache the DC, in combination with setting
     ;; CS_OWNDC.
     (let ((,dc 0))
       (unwind-protect
	   (progn
	     (setq ,dc (getDc (medium-drawable ,medium)))
	     ,@body)
	 (release-objects (medium-drawable ,medium) ,dc)))))

(defmacro with-compatible-dc ((dc cdc) &rest body)
  `(let ((,cdc nil))
     (unwind-protect
	 (progn
	   (setf ,cdc (win:CreateCompatibleDC ,dc))
	   ,@body)
       (selectobject ,cdc *original-bitmap*)
       (when (and *created-bitmap*
		  (not (ct::null-handle-p win:hbitmap *created-bitmap*)))
	 (win:DeleteObject *created-bitmap*))
       (setf *created-bitmap* nil)
       (win:DeleteDC ,cdc))))

(defun set-dc-for-drawing (dc image line-style)
  ;; Note: DASHES may be a list, i.e. (5 2).  CreatePen
  ;; only supports four dash types, and here we only use
  ;; one of them.  Complex dash patterns cannot be supported
  ;; using CreatePen.
  (let* ((dashes (line-style-dashes line-style))
	 (thickness (max 1 (round (line-style-thickness line-style))))
	 (code (if dashes (- thickness) thickness))
	 (brush *null-brush*)
	 (pen (when (= code 1) (dc-image-solid-1-pen image)))
	 (rop2 (dc-image-rop2 image))
	 (text-color (dc-image-text-color image)))
    (declare (fixnum thickness code))
    (unless pen
      (when *created-pen*
	(push *created-pen* *extra-objects*))
      (when (and dashes (> thickness 1))
	;; CreatePen does not support thick dashed lines.
	;; So render dashes with thickness=1.
	;; (Use ExtCreatePen!)
	(setq thickness 1))
      (when (= rop2 win:R2_XORPEN)
	(setq text-color #xffffff))		; black
      (setq pen
	(setq *created-pen*
	  (createPen (if dashes win:PS_DASH win:PS_SOLID)
		     thickness
		     text-color))))
    (selectobject dc pen)
    (if dashes
	(win:SetBkMode dc win:TRANSPARENT)
      (win:SetBkMode dc win:OPAQUE))
    (when brush (selectobject dc brush))
    (when rop2 (win:SetRop2 dc rop2))
    t))

(defun set-dc-for-filling (dc image &optional xorg yorg)
  (let ((background-color (dc-image-background-color image))
        (text-color (dc-image-text-color image))
	(brush (dc-image-brush image))
	(pen *null-pen*)
	(rop2 (dc-image-rop2 image)))
    (selectobject dc pen)
    (when background-color
      (cond ((minusp background-color)
	     ;; This affects brushes created with CreateHatchBrush.
	     (win:SetBkMode dc win:TRANSPARENT))
	    (t
	     (win:SetBkMode dc win:OPAQUE)
	     (win:SetBkColor dc background-color))))
    (when text-color (win:SetTextColor dc text-color))
    (when brush 
      (when (and xorg yorg)
	;; Is this working?  JPM.
	(win:SetBrushOrgEx dc xorg yorg 0))
      (selectobject dc brush))
    (when rop2  (win:SetRop2 dc rop2))
    t))

(defun set-dc-for-ink (dc medium ink line-style &optional xorg yorg)
  (let ((image (dc-image-for-ink medium ink)))
    (if line-style
      (set-dc-for-drawing dc image line-style)
      (set-dc-for-filling dc image xorg yorg))))

(defun set-cdc-for-pattern (dc medium ink line-style)
  (declare (ignore line-style))
  (let ((image (dc-image-for-ink medium ink))
	#+ign
	size)
    (when (typep ink 'pattern)
      #+ign
      (multiple-value-bind (array designs) (decode-pattern ink)
	(declare (ignore array))
	(setf size (length designs)))
      (setf *created-bitmap* (dc-image-bitmap image))
      (cond ((or (not *created-bitmap*)
		 (ct::null-handle-p win:hbitmap *created-bitmap*))
	     (format *terminal-io* "No bitmap (~s) ~%" *created-bitmap*))
	    ((ct::null-handle-p win:hdc dc)
	     (format *terminal-io* "No DC (~s) ~%" dc))
	    (t
	     (setf *original-bitmap* (selectobject dc *created-bitmap*))))
      #+ign
      (unless (> size 2)
	(let ((background-color (dc-image-background-color image))
	      (text-color (dc-image-text-color image)))
	  (cond ((not background-color))
		((minusp background-color)
		 (win:SetBkMode dc win:TRANSPARENT))
		(t
		 (win:SetBkMode dc win:OPAQUE)
		 (win:SetBkColor dc background-color)))
	  (when text-color
	    (win:SetTextColor dc text-color)))
	(win:SetRop2 dc (dc-image-rop2 image))))
    win:SRCCOPY))

(defun set-dc-for-text (dc medium ink font)
  (let* ((image (dc-image-for-ink medium ink))
	 (text-color (dc-image-text-color image))
	 (background-color (dc-image-background-color image))
	 (brush (dc-image-brush image))
	 (pen (dc-image-solid-1-pen image)))
    (win:SetMapMode dc win:MM_TEXT)
    (when font (selectobject dc font))
    (cond ((not background-color)
	   (win:SetBkMode dc win:TRANSPARENT))
	  ((minusp background-color)
	   (win:SetBkMode dc win:TRANSPARENT))
	  (t
	   (win:SetBkMode dc win:OPAQUE)
	   (win:SetBkColor dc background-color)))
    (when brush (win:SelectObject dc brush))
    (when pen (win:SelectObject dc pen))
    (when text-color (win:SetTextColor dc text-color))
    ;; SetRop2 has no effect on text.  If you want to use
    ;; one, you have to draw into a bitmap and biblt that.
    t))

(defgeneric dc-image-for-ink (medium ink))


