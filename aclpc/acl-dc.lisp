;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: ACL-CLIM; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

#|****************************************************************************
*                                                                            *
*                                                                            *
*  This file implements the lowest levels of Windows drawing activity, the   *
*  management of Device Contexts, Pens, Brushes and other resources.         *
*                                                                            *
*                                                                            *
****************************************************************************|#


(in-package :acl-clim)

;; This is not just any 75% gray.  There are
;; methods to treat this object specially and
;; use COLOR_BTNFACE as the realization of this ink.
(defconstant +ltgray+ (make-gray-color .75))

(defmethod make-load-form ((design (eql (symbol-value '+ltgray+))) &optional environment)
  (declare (ignore environment))
  '+ltgray+)

(defmethod print-object ((object (eql (symbol-value '+ltgray+))) stream)
  (format stream "#<CLIM LtGray>"))

;; Device context information
(defstruct (dc-image (:predicate nil))
  (bitmapinfo nil)			; colors of unmasked bitmap
  (bitmap nil)				; bits of unmasked bitmap
  solid-1-pen				; style, width, and color of lines
  brush					; color/stipple used to fill polygons
  (rop2 win:R2_COPYPEN)			; set the foreground mix mode
  text-color				; foreground color
  background-color			; background color
  ;; These two are obsolete:
  and-bitmap				; AND part of masked bitmap (monochrome)
  and-brush                             ; AND part of masked brush (monochrome)
  )

;; Bitmap has to handled carefully when using transparent inks
;; on mswindows.  See, for example dc-image-for-transparent-pattern.
(defun destroy-dc-image (dc &key (destroy-bitmap t))
  (let (;; (bitmapinfo (dc-image-bitmapinfo dc)) ; instance of win:bitmapinfo
	(bitmap (dc-image-bitmap dc))
	(solid-1-pen (dc-image-solid-1-pen dc))
	(brush (dc-image-brush dc))
	;; (rop2 (dc-image-rop2 dc))
	;; (text-color (dc-image-text-color dc))
	;; (background-color (dc-image-background-color dc))
	;; (and-bitmap (dc-image-and-bitmap dc))
	;; (and-brush (dc-image-and-brush dc))
	)
    (when solid-1-pen 
      (win:DeleteObject solid-1-pen)
      (setf (dc-image-solid-1-pen dc) nil))
    (when brush
      (win:DeleteObject brush)
      (setf (dc-image-brush dc) nil))
    (when (and bitmap
	       destroy-bitmap)
      (win:DeleteObject bitmap)
      (setf (dc-image-bitmap dc) nil))
    dc))

(defun initialize-dc ()
  (unless (dc-initialized *acl-port*)
    (unless (win:IsWindow (current-window *acl-port*))
      (error "No Window: ~S" (current-window *acl-port*)))
    ;; Stock objects
    (setf (null-pen *acl-port*)(win:GetStockObject win:NULL_PEN))
    (setf (black-pen *acl-port*)(win:GetStockObject win:BLACK_PEN))
    (setf (ltgray-pen *acl-port*)
      (CreatePen win:PS_SOLID 1 (win:GetSysColor win:COLOR_BTNFACE)))
    ;;
    (setf (null-brush *acl-port*)(win:GetStockObject win:NULL_BRUSH))
    (setf (black-brush *acl-port*)(win:GetStockObject win:BLACK_BRUSH))
    (setf (ltgray-brush *acl-port*)
      (win:CreateSolidBrush (win:GetSysColor win:COLOR_BTNFACE)))

    (setf (blank-image *acl-port*)
	  #+possibly
      (make-dc-image :solid-1-pen (black-pen *acl-port*) 
		     :brush (black-brush *acl-port*)
		     :text-color #xffffffff ; see CLR_NONE
		     :background-color nil
		     :rop2 win:R2_MERGEPEN )
      (make-dc-image :solid-1-pen (black-pen *acl-port*)
		     :brush (black-brush *acl-port*)
		     :text-color #x000000 
		     :background-color nil
		     :rop2 win:R2_NOP ))
    ;;
    (setf (ltgray-image *acl-port*)
	  (make-dc-image :solid-1-pen (ltgray-pen *acl-port*) 
			 :brush (ltgray-brush *acl-port*)
			 :text-color (win:GetSysColor win:COLOR_BTNFACE)
			 :background-color nil
			 :rop2 win:R2_COPYPEN))
    ;;
    (setf (dc-initialized *acl-port*) t)
    ))

(defun destroy-dc ()
  (when (dc-initialized *acl-port*)
    (setf (null-pen *acl-port*) nil
	  (null-brush *acl-port*) nil)
  
    (destroy-dc-image (blank-image *acl-port*))
    (setf (black-pen *acl-port*) nil
	  (black-brush *acl-port*) nil
	  (blank-image *acl-port*) nil)
  
    (destroy-dc-image (ltgray-image *acl-port*))
    (setf (ltgray-pen *acl-port*) nil
	  (ltgray-image *acl-port*) nil
	  (ltgray-brush *acl-port*) nil)
    
    (setf (dc-initialized *acl-port*) nil)))

;;;

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
	   (setq ,dc (GetDC ,window))
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
       (when (valid-handle *created-bitmap*) 
	 (or (win:DeleteObject *created-bitmap*)
	     (error "DeleteObject")))
       (setf *created-bitmap* nil)
       (win:DeleteDC ,cdc))))

(defmacro valid-handle (handle)
  `(let ((h ,handle))
     (declare (fixnum h)
	      (optimize (speed 3) (safety 0)))
     (and h (not (zerop h)) (not (< -100 h 100)))))
  
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
	 (brush (null-brush *acl-port*))
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
	  (CreatePen (if dashes win:PS_DASH win:PS_SOLID)
		     thickness
		     text-color))))
    (when (valid-handle pen) (SelectObject dc pen))
    (if dashes
	(SetBkMode dc win:TRANSPARENT)
      (SetBkMode dc win:OPAQUE))
    (when (valid-handle brush) (SelectObject dc brush))
    (when rop2 (SetROP2 dc rop2))
    created-pen))

(defun set-dc-for-filling (dc image &optional xorg yorg)
  (assert (valid-handle dc))
  (let ((background-color (dc-image-background-color image))
        (text-color (dc-image-text-color image))
	(brush (dc-image-brush image))
	(pen (null-pen *acl-port*))
	(rop2 (dc-image-rop2 image)))
    (when (valid-handle pen) (SelectObject dc pen))
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
      (SelectObject dc brush))
    (when rop2  (SetROP2 dc rop2))
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
	 (when (valid-handle (black-pen *acl-port*))
	   (SelectObject ,dc (black-pen *acl-port*)))
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
    (when (valid-handle font) (SelectObject dc font))
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
    ;; SetROP2 has no effect on text.  If you want to use
    ;; one, you have to draw into a bitmap and biblt that.
    t))

(defmacro with-set-dc-for-text ((dc medium ink font) &body body)
  `(with-dc-image-for-ink (..image..) (,medium ,ink)
     (set-dc-for-text-1 ,dc ,font ..image..)
     ,@body))

(defgeneric dc-image-for-ink (medium ink))


