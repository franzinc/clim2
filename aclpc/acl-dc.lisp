;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: ACL-CLIM; Base: 10; Lowercase: Yes -*-


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

(defstruct (dc-image (:predicate nil))
  (bitmap nil)
  solid-1-pen
  ;; Hash table doesn't appear to be needed anymore. JPM Jan 98.
  (pen-table nil #+obsolete (make-hash-table))
  brush
  (rop2 win:r2_copypen)
  text-color
  background-color)

(defun initialize-dc ()
  (unless (win:iswindow *current-window*)
    (error "No Window: ~S" *current-window*))
  ;; Stock objects
  (setf *null-pen* (win:getStockObject win:null_pen))
  (setf *black-pen* (win:getStockObject win:black_pen))
  (setf *white-pen* (win:getStockObject win:white_pen))
  (setf *ltgray-pen* 
    (win:createPen win:ps_solid 1 (win:getSysColor win:COLOR_BTNFACE)))
  ;;
  (setf *null-brush* (win:getStockObject win:null_brush))
  (setf *black-brush* (win:getStockObject win:black_brush))
  (setf *white-brush* (win:getStockObject win:white_brush))
  (setf *ltgray-brush* 
    (win:createSolidBrush (win:getSysColor win:COLOR_BTNFACE)))
  ;;
  #+obsolete
  (setf *black-image*
    (make-dc-image :solid-1-pen *black-pen*
		   :brush *black-brush*
		   :text-color #x000000
		   :background-color nil
		   :rop2 win:r2_copypen))
  #+obsolete
  (setf *white-image*
    (make-dc-image :solid-1-pen *white-pen*
		   :brush *white-brush*
		   :text-color #xffffff
		   :background-color nil
		   :rop2 win:r2_copypen))
  (setf *blank-image*
    #+possibly
    (make-dc-image :solid-1-pen *black-pen* 
		   :brush *black-brush*
		   :text-color #xffffffff ; see CLR_NONE
                   :background-color nil
		   :rop2 win:r2_mergepen )
    (make-dc-image :solid-1-pen *black-pen*
		   :brush *black-brush*
		   :text-color #x000000 
		   :background-color nil
		   :rop2 win:r2_nop ))
  ;;
  (setf *ltgray-image*
    (make-dc-image :solid-1-pen *ltgray-pen* 
                   :brush *ltgray-brush*
                   :text-color (win:getSysColor win:COLOR_BTNFACE)
                   :background-color nil
		   :rop2 win:r2_copypen))
  ;;
  (setq *dc-initialized* t)
  )

;;;

(defvar *original-bitmap* nil)
(defvar *extra-objects* nil)

(defun release-objects (window dc)
  (when *created-pen*
    (win:selectObject dc *black-pen*)
    (win:deleteObject *created-pen*)
    (setq *created-pen* nil))
  (when *created-brush*
    (win:selectObject dc *white-brush*)
    (win:deleteObject *created-brush*)
    (setq *created-brush* nil))
  (when *created-tile*
    (win:deleteObject *created-tile*)
    (setq *created-tile* nil))
  (when *created-region*
    (win:selectObject dc (ct::null-handle win:hrgn))
    (win:deleteObject *created-region*)
    (setq *created-region* nil))
  (when (and *created-font* *original-font*)
    (win:selectObject dc *original-font*)
    (win:deleteObject *created-font*)
    (setq *created-font* nil))     
  (when (and *created-bitmap* *original-bitmap*)
    (win:selectObject dc *original-bitmap*)
	(note-destroyed *created-bitmap*)
    (win:deleteObject *created-bitmap*)
    (setq *created-bitmap* nil))
  (dolist (xtra *extra-objects*)
	(note-destroyed xtra)
	(win:deleteObject xtra))
  (setq *extra-objects* nil)
  (win:releaseDc window dc))

(defvar *note-created* ())

(defun note-created (kind obj)
  (declare (ignore kind))
  #+ignore (push (cons obj kind)  *note-created*)
  obj)

(defun note-destroyed (obj)
  #+ignore (setf *note-created* (delete obj *note-created* :key #'car))
  obj)


(defclass acl-pixmap (pixmap)
  ((bitmap :initarg :bitmap)
   (for-medium :initarg :for-medium)
   (cdc :initarg :cdc :reader pixmap-cdc)
   (width :initarg :width :reader pixmap-width)
   (height :initarg :height :reader pixmap-height)
   (original-bitmap :initarg :original-bitmap)))

#+ignore ; old version, new below supports with output to pixmap
(defmacro with-dc ((window dc) &rest body)
  `(let ((,dc nil))
    (unwind-protect
     (progn
       (setf ,dc (win:getDc ,window))
       ,@body)
     (release-objects ,window ,dc))))

#+++ignore
(defmacro with-dc ((window dc) &rest body)
  `(let ((,dc nil))
     (if (typep ,window 'acl-pixmap)
       (with-slots (cdc for-medium) ,window
	 (let ((,dc cdc)
	       (medium for-medium))
	   (progn
	     ,@body)
	 ))
       (unwind-protect
	 (progn
	   (setf ,dc (win:getDc ,window))
	   ,@body)
	 (release-objects ,window ,dc))       
       )))

(defmacro with-dc ((window dc) &rest body)
  `(let ((,dc nil))
     (if (typep ,window 'acl-pixmap)
	 (with-slots (cdc for-medium) ,window
	   (let ((,dc cdc)
		 (medium for-medium))
	     medium
	     ,@body))
       (unwind-protect
	   (progn
	     (setf ,dc (win:getDc ,window))
	     ,@body)
	 (release-objects ,window ,dc))       
       )))


(defmacro with-compatible-dc ((dc cdc) &rest body)
  `(let ((,cdc nil))
     (unwind-protect
	 (progn
	   (setf ,cdc (win:createCompatibleDC ,dc))
	   ,@body)
       (win:selectObject ,cdc *original-bitmap*)
       (when (and *created-bitmap*
		  (not (ct::null-handle-p win:hbitmap *created-bitmap*)))
	 (win:deleteObject *created-bitmap*))
       (setf *created-bitmap* nil)
       (win:deleteDc ,cdc))))

(defun set-dc-for-drawing (dc image line-style)
  (let* ((dashes (line-style-dashes line-style))
	 (thickness (max 1 (round (line-style-thickness line-style))))
	 (code (if dashes (- thickness) thickness))
	 (brush *null-brush*)
	 (pen (when (= code 1) (dc-image-solid-1-pen image)))
	 (rop2 (dc-image-rop2 image)))
    (declare (fixnum thickness code))
    (unless pen
      (when *created-pen*
	(push *created-pen* *extra-objects*))
      (setq pen
	(setq *created-pen*
	  (note-created 'pen 
			(win:createPen (if dashes win:ps_dash win:ps_solid)
				       thickness
				       (dc-image-text-color image))))))
    (win:selectObject dc pen)
    (if dashes
	(win:setBkMode dc win:TRANSPARENT)
      (win:setBkMode dc win:OPAQUE))
    (win:selectObject dc brush)
    (when rop2 (win:setRop2 dc rop2))
    t))

(defun set-dc-for-filling (dc image)
  (let ((background-color (dc-image-background-color image))
        (text-color (dc-image-text-color image))
	(brush (dc-image-brush image))
	(pen *null-pen*)
	(rop2 (dc-image-rop2 image)))
    (win:selectObject dc pen)
    (when background-color
      (cond ((minusp background-color)
	     (win:setBkMode dc win:TRANSPARENT))
	    (t
	     (win:setBkMode dc win:OPAQUE)
	     (win:setBkColor dc background-color))))
    (when text-color (win:setTextColor dc text-color))
    (when brush (win:selectObject dc brush))
    (when rop2  (win:setRop2 dc rop2))
    t))

(defun set-dc-for-ink (dc medium ink line-style)
  (let ((image (dc-image-for-ink medium ink)))
    (if line-style
      (set-dc-for-drawing dc image line-style)
      (set-dc-for-filling dc image))))

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
	     (setf *original-bitmap* (win:selectObject dc *created-bitmap*))))
      #+ign
      (unless (> size 2)
	(let ((background-color (dc-image-background-color image))
	      (text-color (dc-image-text-color image)))
	  (cond ((not background-color))
		((minusp background-color)
		 (win:setbkmode dc win:TRANSPARENT))
		(t
		 (win:setbkmode dc win:OPAQUE)
		 (win:setBkColor dc background-color)))
	  (when text-color
	    (win:setTextColor dc text-color)))
	(win:setRop2 dc (dc-image-rop2 image))))
    win:srccopy))

(defun set-dc-for-text (dc medium ink font)
  (let* ((image (dc-image-for-ink medium ink))
	 (text-color (dc-image-text-color image))
	 (background-color (dc-image-background-color image))
	 (oldfont nil))
    (setq oldfont (win:selectObject dc font))
    (cond ((not background-color)
	   (win:setBkMode dc win:transparent))
	  ((minusp background-color)
	   (win:setBkMode dc win:transparent))
	  (t
	   (win:setbkmode dc win:OPAQUE)
	   (win:setBkColor dc background-color)))
    (win:setTextColor dc text-color)
    (unless *original-font* (setf *original-font* oldfont))
    (win:setRop2 dc (dc-image-rop2 image))))

(defgeneric dc-image-for-ink (medium ink))


