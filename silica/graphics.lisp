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


(defclass sheet-with-graphics-state ()
	  ((foreground :initform +black+ :accessor sheet-foreground)
	   (background :initform +white+ :accessor sheet-background)
	   (ink :initform +white+ :accessor sheet-ink)))

(defmethod (setf sheet-foreground) :after (nv (sheet sheet-with-graphics-state))
  (when (sheet-medium sheet)
    (setf (medium-foreground (sheet-medium sheet))
      nv)))

;; One would imagine that with-drawing-options affects this rather
;; than the medium

#+ignore
(spread-arguments 
	 spread-argument-names 
	 keyword-argument-names
	 other-keyword-arguments
	 keywords)

(eval-when (compile load eval)
(defun decode-graphics-function-arguments (arguments)
  (let* ((keyn (position '&key arguments))
	 (no-keyword (subseq arguments 0 keyn))
	 (keyword (and keyn (subseq arguments (1+ keyn))))
	 unspread-argument-names
	 spread-arguments 
	 spread-argument-names)
    
    (dolist (x no-keyword)
      (if (consp x)
	  (destructuring-bind
	      (argname type . names)
	      x
	    (push argname unspread-argument-names)
	    (ecase type
	      (point-sequence
	       (destructuring-bind
		   (new-name) names
		 (push `(unspread-point-sequence ,argname) spread-arguments)
		 (push new-name spread-argument-names)))
	      (point
	       (destructuring-bind 
		   (x y) names
		 (push `(point-x ,argname) spread-arguments)
		 (push x spread-argument-names)
		 (push `(point-y ,argname) spread-arguments)
		 (push y spread-argument-names)))))
	(progn
	  (push x  unspread-argument-names)
	  (push x  spread-arguments)
	  (push x  spread-argument-names))))

      
    (values (nreverse unspread-argument-names)
	    (nreverse spread-arguments)
	    (nreverse spread-argument-names)
	    (mapcar #'(lambda (x)
			(if (consp x)
			    (car x)
			  x))
		    keyword)
	    keyword
	    (mapcar #'(lambda (x)
			(intern
			 (if (consp x)
			     (car x)
			   x)
			 'keyword))
		    keyword))))
)					; eval-when

	 
(defmacro define-graphics-function (name arguments &body stuff)
  (let* ((spread-name (intern (format nil "~A*" name)))
	 (drawing-options
	  '(ink 
	    clipping-region
	    transformation
	    line-style
	    text-style
	    line-thickness
	    line-unit
	    line-dashes
	    line-cap-shape
	    line-joint-shape))
	 (medium-graphics-function-name
	  (intern (format nil "~A~A*" 'medium- name)))
	 (port-function-name
	  (intern (format nil "~A~A*" 'port- name))))
    (multiple-value-bind
	(unspread-argument-names
	 spread-arguments 
	 spread-argument-names 
	 keyword-argument-names
	 other-keyword-arguments
	 keywords)
	(decode-graphics-function-arguments arguments)
      `(progn
	 (defun ,name (medium ,@unspread-argument-names &rest args
		       &key ,@drawing-options ,@other-keyword-arguments) 
	   (declare (ignore ,@drawing-options)
		    (dynamic-extent args))
	   (apply #',spread-name 
		  medium
		  ,@spread-arguments
		  ,@keyword-argument-names
		  args))
	 (defun ,spread-name (medium ,@spread-argument-names &rest args 
				     &key ,@drawing-options ,@other-keyword-arguments)
	   (declare (ignore ,@drawing-options)
		    (dynamic-extent args))
	   (with-rem-keywords 
	    (args args ',keywords)
	    (apply #'invoke-with-drawing-options
		   medium
		   #'(lambda ()
		       (,medium-graphics-function-name 
		      medium
			,@spread-argument-names
			,@keyword-argument-names))
		   args)))
	 (setf (get ',medium-graphics-function-name 'args)
	   '(,@spread-argument-names ,@keyword-argument-names))
	 (defmethod ,medium-graphics-function-name ((medium sheet) 
						    ,@spread-argument-names
						    ,@keyword-argument-names)
	   (with-sheet-medium 
	    (medium medium)
	    (,medium-graphics-function-name medium 
					     ,@spread-argument-names
					     ,@keyword-argument-names)))
	 (defmethod ,medium-graphics-function-name ((medium medium) 
						    ,@spread-argument-names
						    ,@keyword-argument-names)
	   (let* ((sheet (medium-sheet medium)))
	     ;; Want to tranform stuff, set up clipping region etc etc
	     ,@stuff
	     (,port-function-name
	      (port medium)
	      sheet
	      medium
	      ,@spread-argument-names
	      ,@keyword-argument-names)))))))

(define-graphics-function draw-line ((point1 point from-x from-y)
				     (point2 point to-x to-y)))

(define-graphics-function draw-rectangle ((point1 point from-x from-y)
					  (point2 point to-x to-y)
					  &key (filled t)))

(define-graphics-function draw-point ((point point x y)))


;; draw-lines
;; draw-points
;; draw-polygon

(define-graphics-function draw-polygon ((points point-sequence list-of-x-and-ys)
					&key 
					(closed t)
					(filled t)))



(define-graphics-function draw-ellipse ((center point center-x center-y)
					radius-1-dx
					radius-1-dy
					radius-2-dx
					radius-2-dy
					&key 
					(start-angle 0)
					(end-angle 2pi)
					(filled t)))

(defun draw-circle (medium center radius &rest args)
  (apply #'draw-ellipse medium center 0 radius radius 0 args))

(defun draw-circle* (medium center-x center-y radius &rest args)
  (apply #'draw-ellipse* medium center-x center-y 0 radius radius 0 args))

(define-graphics-function draw-text (string-or-char
				     (point point x y)
				     &key
				     start
				     end
				     (align-x :left)
				     (align-y :baseline)
				     towards-point
				     towards-x
				     towards-y
				     transform-glyphs))


(defmethod stream-glyph-for-character ((medium medium) 
				       character 
				       appearance &optional our-font)
  (port-glyph-for-character (port medium)
			     character
			     appearance 
			     our-font))


(defmethod stream-write-string-internal ((medium medium) 
					 glyph-buffer 
					 start 
					 end 
					 x-font 
					 color 
					 x
					 y)
  (port-write-string-internal (port medium)
			      medium
			      glyph-buffer start end x-font color x
			      y))

(defun beep (&optional (stream *standard-output*))
  (sheet-beep stream))

(defmethod sheet-beep ((x t))
  x)

(defmethod sheet-beep ((x sheet))
  (port-beep (port x) x))
