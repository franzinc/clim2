;; -*- mode: common-lisp; package: silica -*-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca.  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, Ca.  All rights reserved.
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
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: medium.cl,v 1.4 92/01/06 20:43:54 cer Exp $

(in-package :silica)

(defmethod engraft-medium (medium port sheet)
  (declare (ignore sheet port))
  nil)

(defmethod degraft-medium (medium port sheet)
  (declare (ignore sheet port))
  nil)



(defun with-sheet-medium-1 (sheet continuation)
  (if (sheet-medium sheet)
      (funcall continuation (sheet-medium sheet))
    (with-temporary-medium 
     (medium sheet)
     (with-sheet-medium-bound (sheet medium)
			      (funcall continuation medium)))))

(defun with-sheet-medium-bound-1 (sheet medium continuation)
  (cond ((sheet-medium sheet)
	 (funcall continuation))
	(medium
	 (letf-globally (((sheet-medium sheet) medium))
			(engraft-medium medium (sheet-port sheet) sheet)
			(funcall continuation)))
	(t
	 (with-sheet-medium-1
	  sheet
	  #'(lambda (medium) 
	      (declare (ignore medium))
	      (funcall continuation))))))


(defgeneric make-medium (port sheet)
  ;; make the right kind of medium for a sheet
  (:method (port sheet)
	   (make-instance 'medium
			  :port port
			  :sheet sheet)))
			  

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
  (if (and (eql unit :normal)
	   (eql joint-shape :miter)
	   (eql cap-shape :butt)
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


(defmethod invoke-with-drawing-options ((sheet sheet) function &rest options)
  (declare (dynamic-extent options))
  (with-sheet-medium (medium sheet)
    (apply #'invoke-with-drawing-options medium function options)))

;; NOTE: if you change the keyword arguments accepted by this method, you
;; also have to change the list of keywords in *ALL-DRAWING-OPTIONS*
(defmethod invoke-with-drawing-options
	   ((medium medium) continuation
	    &key ink clipping-region transformation
		 line-style line-unit line-thickness (line-dashes nil dashes-p)
		 line-joint-shape line-cap-shape
		 (text-style nil text-style-p) (text-family nil text-family-p)
		 (text-face nil text-face-p) (text-size nil text-size-p))
  (with-slots ((medium-ink ink)
	       (medium-transformation transformation)
	       (transformed-clipping-region region)
	       (medium-line-style line-style)) medium
    ;; Close the current output record if the drawing ink is changing
    #-Silica 
    ;;--- Uh-oh, we need to close the stream's text output record, but
    ;;--- at this point we have only a medium.  Add a sheet trampoline.
    (unless (eq medium-ink ink)
      (stream-close-text-output-record medium))
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
  (or (pop (port-media-cache port))
      (make-medium port sheet)))

(defmethod deallocate-medium (port medium)
  (push medium (port-media-cache port)))


;; Make sheets do the medium protocol

(defprotocol medium-protocol
	     (:roles medium))

(defrole medium ()
  ((foreground :accessor medium-foreground)
   (background :accessor medium-background)
   (ink :accessor medium-ink)

   (medium-text-style :accessor medium-text-style)
   (default-text-style :accessor medium-default-text-style)
   (merged-text-style-valid :accessor medium-merged-text-style-valid)
   (merged-text-style :accessor medium-merged-text-style)
   
   (line-size :accessor line-size)
   (line-style :accessor medium-line-style)
   (clipping-region :accessor medium-clipping-region)
   (+y-upward-p :initform nil :accessor medium-+y-upward-p)
   (transformation :accessor medium-transformation)))


(defmethod (setf medium-default-text-style) :before (new (stream medium))
  (declare (ignore new))
  (setf (medium-merged-text-style-valid stream) nil))

(defmethod medium-merged-text-style ((medium medium))
  (with-slots (text-style default-text-style
	       merged-text-style merged-text-style-valid) medium
    (if merged-text-style-valid
	merged-text-style
	(prog1 (setf merged-text-style (merge-text-styles text-style default-text-style))
	  (setf merged-text-style-valid t)))))

(defmacro with-text-style ((stream style) &body body)
  ;;--- Clean up this forward reference by migrating the crucial parts
  ;;--- of CLIM-DEFS into the CLIM-UTILS system
  (default-output-stream stream with-text-style)
  `(flet ((with-text-style-body (,stream) ,@body))
     (declare (dynamic-extent #'with-text-style-body))
     (invoke-with-text-style ,stream #'with-text-style-body ,style
			     ,stream)))

(defmacro with-text-family ((stream family) &body body)
  `(with-text-style (,stream (make-text-style ,family nil nil)) ,@body))

(defmacro with-text-face ((stream face) &body body)
  `(with-text-style (,stream (make-text-style nil ,face nil)) ,@body))

(defmacro with-text-size ((stream size) &body body)
  `(with-text-style (,stream (make-text-style nil nil ,size)) ,@body))

(defoperation invoke-with-text-style medium-protocol 
	      ((stream medium) style continuation original-stream))
	      
(defmethod invoke-with-text-style ((stream medium)
				   continuation style original-stream)
  (if (or (null style) (eql style *null-text-style*))
      (funcall continuation original-stream)
      (letf-globally (((medium-merged-text-style-valid stream) nil)
		      ((slot-value stream 'merged-text-style)
		       (slot-value stream 'merged-text-style))
		      ((medium-text-style stream)
		       (merge-text-styles style (medium-text-style stream))))
	(funcall continuation original-stream))))

(defmethod invoke-with-text-style ((stream standard-encapsulating-stream)
				   continuation style original-stream)
  (invoke-with-text-style (slot-value stream 'stream)
			  continuation style original-stream))

(defmethod invoke-with-text-style ((stream t) continuation style original-stream)
  (declare (ignore style))
  (funcall continuation original-stream))

(generate-trampolines medium-protocol medium standard-sheet-output-mixin
		      `(sheet-medium ,standard-sheet-output-mixin))






    
	
