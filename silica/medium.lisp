;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: medium.lisp,v 1.18 92/07/08 16:29:16 cer Exp $

(in-package :silica)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1991, 1992 Franz, Inc.  All rights reserved."

(defmethod engraft-medium ((medium basic-medium) port sheet)
  (declare (ignore port))
  (setf (medium-sheet medium) sheet)
  nil)

(defmethod degraft-medium ((medium basic-medium) port sheet)
  (declare (ignore sheet port))
  nil)

(defmethod invoke-with-sheet-medium ((sheet sheet) continuation)
  (declare (dynamic-extent continuation))
  (let ((medium (sheet-medium sheet)))
    (if medium
	(funcall continuation medium)
	(with-temporary-medium (medium sheet)
	  (with-sheet-medium-bound (sheet medium)
	    (funcall continuation medium))))))

;; Special-case the one we know is going to work all the time
(defmethod invoke-with-sheet-medium ((sheet permanent-medium-sheet-output-mixin) continuation)
  (declare (dynamic-extent continuation))
  (let ((medium (slot-value sheet 'medium)))
    (if medium 
	(funcall continuation medium)
	;; Some gadgets won't have a medium while they are being created.
	;; Go get one now so that foreground/background can be decoded, etc.
	(call-next-method))))

;;--- Use DEFOPERATION
(defmethod invoke-with-sheet-medium ((x standard-encapsulating-stream) continuation)
  (declare (dynamic-extent continuation))
  (invoke-with-sheet-medium (slot-value x 'stream) continuation))

(defun invoke-with-sheet-medium-bound (sheet medium continuation)
  (declare (dynamic-extent continuation))
  (cond ((sheet-medium sheet)
	 (funcall continuation))
	(medium
	 (letf-globally (((sheet-medium sheet) medium))
	   (engraft-medium medium (port sheet) sheet)
	   (funcall continuation)))
	(t
	 (flet ((call-continuation (medium)
		  (declare (ignore medium))
		  (funcall continuation)))
	   (declare (dynamic-extent #'call-continuation))
	   (invoke-with-sheet-medium
	     sheet #'call-continuation)))))

(defgeneric make-medium (port sheet))
			  

;;; Line styles

(define-protocol-class line-style ())

(defclass standard-line-style (line-style)
    ((unit :type (member :normal :point)
	   :initform :normal :initarg :unit
	   :reader line-style-unit)
     (thickness :type real
		:initform 1 :initarg :thickness
		:reader line-style-thickness)
     (joint-shape :type (member :miter :bevel :round :none)
		  :initform :miter :initarg :joint-shape
		  :reader line-style-joint-shape)
     (cap-shape :type (member :butt :square :round :no-end-point)
		:initform :butt :initarg :cap-shape
		:reader line-style-cap-shape)
     (dashes :initform nil :initarg :dashes
	     :reader line-style-dashes)
     #+++ignore (initial-dash-phase :initform 0 :initarg :initial-dash-phase
				    :reader line-style-initial-dash-phase)))

(defmethod print-object ((line-style standard-line-style) stream)
  (print-unreadable-object (line-style stream :type t :identity t)
    (with-slots (unit thickness joint-shape cap-shape dashes) line-style
      (format stream "Units ~(~A~), thickness ~D, joint ~(~A~), cap ~(~A~), dashes ~S"
	      unit thickness joint-shape cap-shape dashes))))

(defvar +default-line-style+ (make-instance 'standard-line-style))
(defvar +dashed-line-styles+
	(make-array 5 :initial-contents
		        (list (make-instance 'standard-line-style :thickness 0 :dashes t)
			      (make-instance 'standard-line-style :thickness 1 :dashes t)
			      (make-instance 'standard-line-style :thickness 2 :dashes t)
			      (make-instance 'standard-line-style :thickness 3 :dashes t)
			      (make-instance 'standard-line-style :thickness 4 :dashes t))))
(defvar +undashed-line-styles+
	(make-array 5 :initial-contents
		        (list (make-instance 'standard-line-style :thickness 0 :dashes nil)
			      (make-instance 'standard-line-style :thickness 1 :dashes nil)
			      (make-instance 'standard-line-style :thickness 2 :dashes nil)
			      (make-instance 'standard-line-style :thickness 3 :dashes nil)
			      (make-instance 'standard-line-style :thickness 4 :dashes nil))))

(defun-inline make-line-style-1 (unit thickness dashes joint-shape cap-shape)
  #+Genera (declare lt:(side-effects simple reducible))
  (if (and (eq unit :normal)
	   (eq joint-shape :miter)
	   (eq cap-shape :butt)
	   (integerp thickness) (<= 0 thickness 4)
	   (or (eq dashes t) (eq dashes nil)))
      ;; Cache the common case when only :DASHES and :THICKNESS are provided
      (svref (if dashes +dashed-line-styles+ +undashed-line-styles+) thickness)
      (make-instance 'standard-line-style
	:unit unit :thickness thickness :dashes dashes
	:joint-shape joint-shape :cap-shape cap-shape)))

(defun make-line-style (&key (unit :normal) (thickness 1) (dashes nil)
			     (joint-shape :miter) (cap-shape :butt))
  #+Genera (declare lt:(side-effects simple reducible))
  (make-line-style-1 unit thickness dashes joint-shape cap-shape))

(defvar +highlighting-line-style+ (make-line-style :thickness 1))

(defmethod make-load-form ((line-style standard-line-style))
  (with-slots (unit thickness joint-shape cap-shape dashes) line-style
    `(make-line-style ,@(unless (eq unit :points) `(:unit ,unit))
		      ,@(unless (= thickness 1) `(:thickness ,thickness))
		      ,@(unless (eq joint-shape :miter) `(:joint-shape ,joint-shape))
		      ,@(unless (eq cap-shape :butt) `(:cap-shape ,cap-shape))
		      ,@(unless (eq dashes nil) `(:dashes ,dashes)))))


(defmethod invoke-with-drawing-options ((sheet sheet) continuation &rest options)
  (declare (dynamic-extent options))
  (with-sheet-medium (medium sheet)
    (apply #'invoke-with-drawing-options medium continuation options)))

;; For string streams, sigh
(defmethod invoke-with-drawing-options ((stream t) continuation &rest options)
  (declare (ignore options))
  (funcall continuation))

;;--- CLIM 1.0 had this stuff that frobbed the clipping region.  Is it right?
#+++ignore
(defmethod medium-clipping-region ((medium basic-medium))
  (with-slots (transformation transformed-clipping-region) medium
    (untransform-region transformation transformed-clipping-region)))

#+++ignore
(defmethod (setf medium-clipping-region) (clipping-region (medium basic-medium))
  (with-slots (transformation transformed-clipping-region) medium
    (setf transformed-clipping-region (transform-region transformation clipping-region)))
  clipping-region)

(defmacro with-medium-clipping-region ((medium region) &body body)
  `(flet ((with-medium-clipping-region-body (,medium) ,@body))
     (declare (dynamic-extent #'with-medium-clipping-region-body))
     (invoke-with-medium-clipping-region 
       ,medium #'with-medium-clipping-region-body ,region)))

(defmethod invoke-with-medium-clipping-region 
	   ((medium basic-medium) continuation region)
  (let ((saved-region (medium-clipping-region medium)))
    (unwind-protect
	(progn
	  (setf (medium-clipping-region medium) region)
	  (funcall continuation medium))
      (setf (medium-clipping-region medium) saved-region))))

(defmethod invalidate-cached-regions ((medium basic-medium)) nil)
(defmethod invalidate-cached-transformations ((medium basic-medium)) nil)

;; NOTE: if you change the keyword arguments accepted by this method, you
;; also have to change the list of keywords in *ALL-DRAWING-OPTIONS*
(defmethod invoke-with-drawing-options
	   ((medium basic-medium) continuation
	    &key ink clipping-region transformation
		 line-style line-unit line-thickness (line-dashes nil dashes-p)
		 line-joint-shape line-cap-shape
		 (text-style nil text-style-p) (text-family nil text-family-p)
		 (text-face nil text-face-p) (text-size nil text-size-p))
  (with-slots ((medium-ink ink)
	       (medium-transformation transformation)
	       (transformed-clipping-region region)
	       (medium-line-style line-style)) medium
    (let* ((saved-ink medium-ink)
	   (saved-transformation medium-transformation)
	   (saved-clipping-region transformed-clipping-region)
	   (saved-line-style medium-line-style))
      (unwind-protect
	  (progn
	    (when ink
	      (setf medium-ink ink))
	    (when transformation
	      (setf medium-transformation
		    (compose-transformations saved-transformation transformation)))
	    (when clipping-region
	      (setf transformed-clipping-region
		    (region-intersection saved-clipping-region
					 (transform-region medium-transformation
							   clipping-region))))
	    (cond ((or line-unit line-thickness line-joint-shape line-cap-shape dashes-p)
		   (when (null line-style)
		     (setf line-style saved-line-style))
		   (setf medium-line-style
			 (make-line-style-1
			   (or line-unit (line-style-unit line-style))
			   (or line-thickness (line-style-thickness line-style))
			   (if dashes-p line-dashes (line-style-dashes line-style))
			   (or line-joint-shape (line-style-joint-shape line-style))
			   (or line-cap-shape (line-style-cap-shape line-style)))))
		  (line-style
		   (setf medium-line-style line-style)))
	    (when (or text-family-p text-face-p text-size-p)
	      (if text-style-p
		  (setq text-style (with-stack-list (style text-family text-face text-size)
				     (merge-text-styles style text-style)))
		  (setq text-style (make-text-style text-family text-face text-size)
			text-style-p t)))
	    (if text-style-p
		(flet ((call-continuation (stream)
			 (declare (ignore stream))
			 (funcall continuation)))
		  (declare (dynamic-extent #'call-continuation))
		  (invoke-with-text-style medium #'call-continuation text-style medium))
	        (funcall continuation)))
	(setf medium-line-style saved-line-style)
	(setf transformed-clipping-region saved-clipping-region)
	(setf medium-transformation saved-transformation)
	(setf medium-ink saved-ink)))))


(defmethod allocate-medium (port sheet)
  (or (pop (port-medium-cache port))
      (make-medium port sheet)))

(defmethod deallocate-medium (port medium)
  (push medium (port-medium-cache port)))



;; Make sheets do the medium protocol

(defprotocol medium-protocol ()
  (:roles medium))

(defrole medium ()
  ((foreground :accessor medium-foreground)
   (background :accessor medium-background)
   (ink :accessor medium-ink)

   (line-style :accessor medium-line-style)
   (clipping-region :accessor medium-clipping-region)
   (transformation :accessor medium-transformation)
   (+y-upward-p :initform nil :accessor medium-+y-upward-p)

   (medium-text-style :accessor medium-text-style)
   (default-text-style :accessor medium-default-text-style)
   (merged-text-style-valid :accessor medium-merged-text-style-valid)
   (merged-text-style :accessor medium-merged-text-style)))


(defmethod (setf medium-default-text-style) :before (new (medium basic-medium))
  (declare (ignore new))
  (setf (medium-merged-text-style-valid medium) nil))

(defmethod medium-merged-text-style ((medium basic-medium))
  (with-slots (text-style default-text-style
	       merged-text-style merged-text-style-valid) medium
    (if merged-text-style-valid
	merged-text-style
	(prog1 
	  (setf merged-text-style (merge-text-styles text-style default-text-style))
	  (setf merged-text-style-valid t)))))

(defmacro with-text-style ((medium style) &body body)
  (default-output-stream medium with-text-style)
  `(flet ((with-text-style-body (,medium) ,@body))
     (declare (dynamic-extent #'with-text-style-body))
     (invoke-with-text-style ,medium #'with-text-style-body ,style
			     ,medium)))

(defmacro with-text-family ((medium family) &body body)
  `(with-text-style (,medium (make-text-style ,family nil nil)) ,@body))

(defmacro with-text-face ((medium face) &body body)
  `(with-text-style (,medium (make-text-style nil ,face nil)) ,@body))

(defmacro with-text-size ((medium size) &body body)
  `(with-text-style (,medium (make-text-style nil nil ,size)) ,@body))

(defoperation invoke-with-text-style medium-protocol 
  ((medium medium) style continuation original-stream))
	      
(defmethod invoke-with-text-style ((medium basic-medium)
				   continuation style original-stream)
  (if (or (null style) (eq style *null-text-style*))
      (funcall continuation original-stream)
      (letf-globally (((medium-merged-text-style-valid medium) nil)
		      ((slot-value medium 'merged-text-style)
		       (slot-value medium 'merged-text-style))
		      ((medium-text-style medium)
		       (merge-text-styles style (medium-text-style medium))))
	(funcall continuation original-stream))))

(defmethod invoke-with-text-style ((stream standard-encapsulating-stream)
				   continuation style original-stream)
  (invoke-with-text-style (slot-value stream 'stream)
			  continuation style original-stream))

;; Default method for string streams
(defmethod invoke-with-text-style ((stream t) continuation style original-stream)
  (declare (ignore style))
  (funcall continuation original-stream))


(defmethod graft ((medium basic-medium))
  (graft (medium-sheet medium)))

(defmethod port ((medium basic-medium))
  (port (medium-sheet medium)))


(defoperation text-style-height medium-protocol
  (text-style (medium medium))
  (:no-defgeneric t))

(defoperation text-style-width medium-protocol
  (text-style (medium medium))
  (:no-defgeneric t))

(defoperation text-style-ascent medium-protocol
  (text-style (medium medium))
  (:no-defgeneric t))

(defoperation text-style-descent medium-protocol
  (text-style (medium medium))
  (:no-defgeneric t))

(defoperation text-style-fixed-width-p medium-protocol
  (text-style (medium medium))
  (:no-defgeneric t))

(defoperation text-size medium-protocol
  ((medium medium) string &key text-style start end)
  (declare (values largest-x total-height last-x last-y baseline))
  (:no-defgeneric t))


;; Generate the sheet->medium trampolines now
(generate-trampolines medium-protocol medium standard-sheet-output-mixin
		      `(sheet-medium ,standard-sheet-output-mixin))

