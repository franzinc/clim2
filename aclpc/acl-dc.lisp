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
;; $Id: acl-dc.lisp,v 1.4.8.13 1999/06/09 21:29:47 layer Exp $

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

(defun release-objects (window dc)
  (win:ReleaseDC window dc))

(defclass acl-pixmap (pixmap)
  ((bitmap :initarg :bitmap)
   (for-medium :initarg :for-medium)
   (cdc :initarg :cdc :reader pixmap-cdc)
   (width :initarg :width :reader pixmap-width)
   (height :initarg :height :reader pixmap-height)
   (original-bitmap :initarg :original-bitmap)))

(defmethod isa-pixmap ((object t)) nil)
(defmethod isa-pixmap ((object acl-pixmap)) t)

(defmethod isa-pixmap-medium ((object t)) nil)
(defmethod isa-pixmap-medium ((object basic-pixmap-medium)) t)

;; The call to GetDC will cons a bignum (16 bytes).
;; We should probably try to optimize this some how
;; since it is called constantly during simple
;; things like repaint.
(defmacro with-dc ((window dc) &rest body)
  ;; It was my intention to rewrite this to
  ;; cache the DC, in combination with setting
  ;; CS_OWNDC.
  `(let ((,dc 0))
     (unwind-protect
	 (progn
	   ;; There are occasions where GetDC returns 0
	   ;; despite the fact that you did everything right.
	   ;; "Since only five common device contexts are
	   ;; available at any given time, failure to release
	   ;; a device context can prevent other applications
	   ;; from accessing a device context."  Microsoft document.
	   (setq ,dc (getDc ,window))
	   ,@body)
       (unless (zerop ,dc)
	 (ReleaseDC ,window ,dc)))))

;; JPM: this macro should be rewritten to expand the body only once.
(defmacro with-medium-dc ((medium dc) &rest body)
  `(cond ((isa-pixmap-medium ,medium)
	  (let ((,dc (pixmap-cdc (medium-drawable ,medium)))) ,@body))
	 ((isa-pixmap ,medium)
	  (let ((,dc (pixmap-cdc ,medium))) ,@body))
	 (t
	  ;; It was my intention to rewrite this to
	  ;; cache the DC, in combination with setting
	  ;; CS_OWNDC.
	  (with-dc ((medium-drawable ,medium) ,dc) ,@body))))

(defmacro with-compatible-dc ((dc cdc) &rest body)
  `(let ((,cdc nil))
     (unwind-protect
	 (progn
	   (setf ,cdc (win:CreateCompatibleDC ,dc))
	   ,@body)
       (when (valid-handle *original-bitmap*) 
	 (selectobject ,cdc *original-bitmap*))
       (when (valid-handle *created-bitmap*) 
	 (or (win:DeleteObject *created-bitmap*)
	     (error "DeleteObject")))
       (setf *created-bitmap* nil)
       (win:DeleteDC ,cdc))))

(defun valid-handle (handle &optional stuff)
  (declare (ignore stuff)
	   (fixnum handle)
	   (optimize (speed 3) (safety 0)))
  (when (and handle (not (zerop handle)))
    (if (< -100 handle 100) nil t)))
  
(defun check-handle (handle)
  (when (or (not (valid-handle handle)))
    (error "invalid handle")))

(defun set-dc-for-drawing-1 (dc image line-style)
  ;; Note: DASHES may be a list, i.e. (5 2).  CreatePen
  ;; only supports four dash types, and here we only use
  ;; one of them.  Complex dash patterns cannot be supported
  ;; using CreatePen.
  (assert (valid-handle dc))
  (let* ((dashes (line-style-dashes line-style))
	 (thickness (max 1 (round (line-style-thickness line-style))))
	 (code (if dashes (- thickness) thickness))
	 (brush *null-brush*)
	 (pen (when (= code 1) (dc-image-solid-1-pen image)))
	 (rop2 (dc-image-rop2 image))
	 (text-color (dc-image-text-color image))
	 (created-pen nil))
    (declare (fixnum thickness code))
    (unless pen
      (when (and dashes (> thickness 1))
	;; CreatePen does not support thick dashed lines.
	;; So render dashes with thickness=1.
	;; (Use ExtCreatePen!)
	(setq thickness 1))
      (when (= rop2 win:R2_XORPEN)
	(setq text-color #xffffff))		; black
      (setq pen
	(setq created-pen
	  (createPen (if dashes win:PS_DASH win:PS_SOLID)
		     thickness
		     text-color))))
    (when (valid-handle pen) (selectobject dc pen))
    (if dashes
	(SetBkMode dc win:TRANSPARENT)
      (SetBkMode dc win:OPAQUE))
    (when (valid-handle brush) (selectobject dc brush))
    (when rop2 (SetRop2 dc rop2))
    created-pen))

(defun set-dc-for-filling (dc image &optional xorg yorg)
  (assert (valid-handle dc))
  (let ((background-color (dc-image-background-color image))
        (text-color (dc-image-text-color image))
	(brush (dc-image-brush image))
	(pen *null-pen*)
	(rop2 (dc-image-rop2 image)))
    (when (valid-handle pen) (selectobject dc pen))
    (when background-color
      (cond ((minusp background-color)
	     ;; This affects brushes created with CreateHatchBrush.
	     (SetBkMode dc win:TRANSPARENT))
	    (t
	     (SetBkMode dc win:OPAQUE)
	     (SetBkColor dc background-color))))
    (when text-color (SetTextColor dc text-color))
    (when (valid-handle brush)
      (when (and xorg yorg)
	;; Is this working?  JPM.
	(win:SetBrushOrgEx dc xorg yorg 0))
      (selectobject dc brush))
    (when rop2  (SetRop2 dc rop2))
    t))

(defun set-dc-for-ink-1 (dc image line-style &optional xorg yorg)
  (let ((retval nil))
    ;; For transparent patterns, we actually have two images.
    ;; Callers should prevent us from getting here in that case,
    ;; but in case they fail, do something responsible here:
    (when (consp image) (setq image (second image)))
    (if line-style
	(setq retval (set-dc-for-drawing-1 dc image line-style))
      (set-dc-for-filling dc image xorg yorg))

    retval))

(defmacro with-set-dc-for-ink ((dc medium ink line-style &optional xorg yorg) 
			       &body body)
  `(let ((..winpen.. nil))
     (unwind-protect 
	 (with-dc-image-for-ink (..image..) (,medium ,ink)
           (setq ..winpen.. (set-dc-for-ink-1 ,dc ..image.. ,line-style
					      ,xorg ,yorg))
	   ,@body)
       (when (valid-handle ..winpen..)
	 (when (valid-handle *black-pen*) (selectobject ,dc *black-pen*))
	 (or (win:DeleteObject ..winpen..) 
	     (error "with-set-dc-for-ink: DeleteObject"))))))

(defmethod isa-pattern ((object t)) nil)
(defmethod isa-pattern ((object pattern)) t)

(defun set-dc-for-text-1 (dc font image)
  (when (consp image) (setq image (second image)))
  (let* ((text-color (dc-image-text-color image))
	 #+ignore
	 (background-color (dc-image-background-color image))
	 (brush (dc-image-brush image))
	 (pen (dc-image-solid-1-pen image)))
    (win:SetMapMode dc win:MM_TEXT)
    (when (valid-handle font) (selectobject dc font))
    ;; Seems like we never want opaque background. JPM.
    #+ignore				
    (cond ((not background-color)
	   (win:SetBkMode dc win:TRANSPARENT))
	  ((minusp background-color)
	   (win:SetBkMode dc win:TRANSPARENT))
	  (t
	   (win:SetBkMode dc win:OPAQUE)
	   (win:SetBkColor dc background-color)))
    (SetBkMode dc win:TRANSPARENT)
    (when (valid-handle brush) (SelectObject dc brush))
    (when (valid-handle pen) (SelectObject dc pen))
    (when text-color (SetTextColor dc text-color))
    ;; SetRop2 has no effect on text.  If you want to use
    ;; one, you have to draw into a bitmap and biblt that.
    t))

(defmacro with-set-dc-for-text ((dc medium ink font) &body body)
  `(with-dc-image-for-ink (..image..) (,medium ,ink)
     (set-dc-for-text-1 ,dc ,font ..image..)
     ,@body))

(defgeneric dc-image-for-ink (medium ink))


