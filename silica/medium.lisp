(in-package :silica)
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
			  

(defun %make-line-style (&rest x) :line-style)

(defmethod invoke-with-drawing-options ((sheet sheet) function &rest options)
  (declare (dynamic-extent options))
  (with-sheet-medium
   (medium sheet)
   (apply #'invoke-with-drawing-options
	  medium
	  function
	  options)))

(defmethod invoke-with-drawing-options ((medium medium) function 
					&key 
					(line-thickness nil line-thickness-p)
					(ink nil ink-p)
					(text-style nil text-style-p)
					(transformation nil trans-p)
					(clipping-region nil clipping-region-p)
					(line-style nil line-style-p)
					(text-family nil text-family-p)
					(text-face nil text-face-p)
					(text-size nil text-size-p))
  (when (or text-size-p text-face-p text-family-p line-style-p
	    clipping-region-p)
    (warn "unhandled option to invoke-with-drawing-options"))
  
  (let ((old-ink (if ink-p (medium-ink medium)))
	(old-trans (if trans-p (medium-transformation medium)))
	(old-text-style (if text-style-p (medium-text-style medium))))
    (unwind-protect
	(progn
	  (when ink-p 
	    (setf (medium-ink medium) ink))
	  (when trans-p
	    (setf (medium-transformation medium) transformation))
	  (when text-style-p
	    (setf (medium-text-style medium) text-style))
	  (funcall function))
      (when ink-p
	(setf (medium-ink medium) old-ink))
      (when trans-p
	(setf (medium-transformation medium) old-trans))
      (when text-style-p
	(setf (medium-text-style medium) old-text-style)))))

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






    
	
