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
;; $fiHeader$

(in-package :silica)

(defmethod engraft-medium (medium port sheet)
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
			(engraft-medium medium (port sheet) sheet)
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
			  

(defclass line-style ()
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
	   ))

(defun make-line-style-1 (line-unit line-thickness line-dashes
			  line-joint-shape line-cap-shape)
  (make-instance 'line-style
		 :unit line-unit
		 :thickness line-thickness
		 :dashes line-dashes
		 :joint-shape line-joint-shape
		 :cap-shape line-cap-shape))

(defun %make-line-style (&rest x) 
  (apply #'make-instance 'line-style x))

(defmethod invoke-with-drawing-options ((sheet sheet) function &rest options)
  (declare (dynamic-extent options))
  (with-sheet-medium
   (medium sheet)
   (apply #'invoke-with-drawing-options
	  medium
	  function
	  options)))



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
    #+ignore-do-we-have-to-do-this
    (unless (eq medium-ink ink)
      (close-current-text-output-record medium))
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
		  (with-text-style-internal medium text-style #'call-continuation medium))
	        (funcall continuation)))
	(setf medium-line-style saved-line-style)
	(setf transformed-clipping-region saved-clipping-region)
	(setf medium-transformation saved-transformation)
	(setf medium-ink saved-ink)))))

#+ignore
(defmethod invoke-with-drawing-options ((medium medium) function 
					&rest args
					&key 
					(line-cap-shape nil line-cap-shape-p)
					(line-joint-shape nil line-joint-shape-p)
					(line-dashes nil line-dashes-p) 
					(line-thickness nil line-thickness-p)
					(line-unit nil line-unit-p)
					(line-style nil line-style-p)
					(ink nil ink-p)
					(transformation nil trans-p)
					(clipping-region nil clipping-region-p)
					(text-family nil text-family-p)
					(text-face nil text-face-p)
					(text-size nil text-size-p)
					(text-style nil text-style-p))

  (with-rem-keywords (new args '(:ink :text-style :line-style :transformation))
    (when new
      (warn "unhandled option to invoke-with-drawing-options ~S" new)))
  
  (let ((old-ink (if ink-p (medium-ink medium)))
	(old-trans (if trans-p (medium-transformation medium)))
	(old-text-style (if text-style-p (medium-text-style medium)))
	(old-line-style (if line-style-p (medium-line-style medium))))
    (unwind-protect
	(progn
	  (when ink-p 
	    (setf (medium-ink medium) ink))
	  ;; Should we not compose
	  (when trans-p
	    (setf (medium-transformation medium) transformation))
	  (when text-style-p
	    (setf (medium-text-style medium) text-style))
	  (when line-style-p
	    (setf (medium-line-style medium) line-style))
	  (funcall function))
      (when ink-p
	(setf (medium-ink medium) old-ink))
      (when trans-p
	(setf (medium-transformation medium) old-trans))
      (when text-style-p
	(setf (medium-text-style medium) old-text-style))
      (when line-style-p
	(setf  (medium-line-style medium) old-line-style)))))

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

(defmacro with-text-family ((family &optional stream) &body body)
  `(with-text-style ((make-text-style ,family nil nil) ,stream) ,@body))

(defmacro with-text-face ((face &optional stream) &body body)
  `(with-text-style ((make-text-style nil ,face nil) ,stream) ,@body))

(defmacro with-text-size ((size &optional stream) &body body)
  `(with-text-style ((make-text-style nil nil ,size) ,stream) ,@body))

(defmethod with-text-style-internal ((stream medium)
				     style continuation original-stream)
  (if (or (null style) (eql style *null-text-style*))
      (funcall continuation original-stream)
      (letf-globally (((medium-merged-text-style-valid stream) nil)
		      ((slot-value stream 'merged-text-style)
		       (slot-value stream 'merged-text-style))
		      ((medium-text-style stream)
		       (merge-text-styles style (medium-text-style stream))))
		     (funcall continuation original-stream))))

(defmethod with-text-style-internal ((stream t) style continuation original-stream)
  (declare (ignore style))
  (funcall continuation original-stream))

(defoperation with-text-style-internal medium-protocol 
	      ((stream medium) style continuation original-stream))
	      
(generate-trampolines medium-protocol medium standard-sheet-output-mixin
		      `(sheet-medium ,standard-sheet-output-mixin))






    
	
