;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes; Patch-File: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."


;;; Compatibility stubs for CLIM 1.1

(eval-when (compile load eval)
  ;; We use this below to flag the compatibility code...
  (pushnew :CLIM-1-compatibility *features*))

(defmacro define-compatibility-function ((old-name new-name) arglist &body body)
  `(progn
     #-Genera
     (define-compiler-macro ,old-name (&whole form)
       (warn "The function ~S is now obsolete, use ~S instead.~%~
	      Compatibility code is being generated for the time being."
	     ',old-name ',new-name)
       form)
     #+Genera
     (defun-property (,old-name compiler:style-checker) (form)
       (declare (ignore form))
       (scl:warn '(:obsolete t)
		 "The function ~S is now obsolete, use ~S instead.~%~
		  Compatibility code is being generated for the time being."
		 ',old-name ',new-name))
     (defun-inline ,old-name ,arglist
       ,@body)))


(define-compatibility-function (point-position* point-position)
			       (point)
  (point-position point))

(define-compatibility-function (region-contains-point*-p region-contains-position-p)
			       (region x y)
  (region-contains-position-p region x y))


(defmacro with-bounding-rectangle* ((left top &optional right bottom) region &body body)
  #+Genera (declare (zwei:indentation 1 3 2 1))
  (when (or (null right) (null bottom))
    (setq right '#:right
	  bottom '#:bottom)
    #-Genera (warn "The ~A and ~A arguments to ~S are now required.~%~
		    Compatibility code is being generated for the time being."
		   'right 'bottom 'with-bounding-rectangle*))
  `(multiple-value-bind (,left ,top ,right ,bottom)
       (bounding-rectangle* ,region) 
     (declare (type coordinate ,left ,top ,right ,bottom))
     ,right ,bottom
     ,@body))

#+Genera
(defun-property (with-bounding-rectangle* compiler:style-checker) (form)
  (destructuring-bind ((left top &optional right bottom) region &rest body) (cdr form)
    (declare (ignore left top region body))
    (when (or (null right) (null bottom))
      (scl:warn '(:obsolete t) 
		"The ~A and ~A arguments to ~S are now required.~%~
		 Compatibility code is being generated for the time being."
		'right 'bottom 'with-bounding-rectangle*))))

(define-compatibility-function (bounding-rectangle-position*
				bounding-rectangle-position)
			       (region)
  (bounding-rectangle-position region))

(define-compatibility-function (bounding-rectangle-set-position* 
				bounding-rectangle-set-position)
			       (region x y)
  (bounding-rectangle-set-position region x y))


#-Genera
(define-compiler-macro make-3-point-transformation (&whole form)
  (warn "The function ~S has a different arglist than it did in CLIM 1.1.~%~
	 Please check your code."
	'make-3-point-transformation)
  form)

#+Genera
(defun-property (make-3-point-transformation compiler:style-checker) (form)
  (declare (ignore form))
  (scl:warn '(:obsolete t) 
	    "The function ~S has a different arglist than it did in CLIM 1.1.~%~
	     Please check your code."
	    'make-3-point-transformation))

#-Genera
(define-compiler-macro make-3-point-transformation* (&whole form)
  (warn "The function ~S has a different arglist than it did in CLIM 1.1.~%~
	 Please check your code."
	'make-3-point-transformation*)
  form)

#+Genera
(defun-property (make-3-point-transformation* compiler:style-checker) (form)
  (declare (ignore form))
  (scl:warn '(:obsolete t) 
	    "The function ~S has a different arglist than it did in CLIM 1.1.~%~
	     Please check your code."
	    'make-3-point-transformation*))

(define-compatibility-function (compose-rotation-transformation
				compose-rotation-with-transformation)
			       (transform angle &optional origin)
  (compose-rotation-with-transformation transform angle origin))

(define-compatibility-function (compose-scaling-transformation
				compose-scaling-with-transformation)
			       (transform mx my &optional origin)
  (compose-scaling-with-transformation transform mx my origin))

(define-compatibility-function (compose-translation-transformation
				compose-translation-with-transformation)
			       (transform dx dy)
  (compose-translation-with-transformation transform dx dy))

(define-compatibility-function (transform-point* transform-position)
			       (transform x y)
  (transform-position transform x y))

(define-compatibility-function (untransform-point* untransform-position)
			       (transform x y)
  (transform-position transform x y))


(eval-when (compile load eval)
(defun probably-stream-or-medium-p (symbol)
  (and (symbolp symbol)
       ;;--- This is really not completely safe
       (or (search "MEDIUM" (symbol-name symbol) :test #'string-equal)
	   (search "STREAM" (symbol-name symbol) :test #'string-equal))))
)	;eval-when

(defmacro with-text-style ((medium &optional style) &body body)
  (when (or (member style '(t nil *standard-input* *standard-output* *query-io*))
	    (and (constantp medium)
		 (listp (eval medium))
		 (= (length (eval medium)) 3))
	    (probably-stream-or-medium-p style))
    #-Genera (warn "Converting old style call to ~S to the new style.~%~
		    Please update your code." 'with-text-style)
    (rotatef medium style))
  (default-output-stream medium with-text-style)
  `(flet ((with-text-style-body (,medium) ,@body))
     (declare (dynamic-extent #'with-text-style-body))
     (invoke-with-text-style 
       ,medium #'with-text-style-body ,style ,medium)))

#+Genera
(defun-property (with-text-style compiler:style-checker) (form)
  (destructuring-bind ((medium &optional style) &rest body) (cdr form)
    (declare (ignore body))
    (when (or (member style '(t nil *standard-input* *standard-output* *query-io*))
	      (and (constantp medium)
		   (listp (eval medium))
		   (= (length (eval medium)) 3))
	      (probably-stream-or-medium-p style))
      (scl:warn '(:obsolete t) 
		"Converting old style call to ~S to the new style.~%~
		 Please update your code." 'with-text-style))))

(defmacro with-text-family ((medium &optional family) &body body)
  (when (or (member family '(t nil *standard-input* *standard-output* *query-io*))
	    (and (constantp medium)
		 (keywordp (eval medium)))
	    (probably-stream-or-medium-p family))
    #-Genera (warn "Converting old style call to ~S to the new style.~%~
		    Please update your code." 'with-text-family)
    (rotatef medium family))
  `(with-text-style (,medium (make-text-style ,family nil nil)) ,@body))

#+Genera
(defun-property (with-text-family compiler:style-checker) (form)
  (destructuring-bind ((medium &optional family) &rest body) (cdr form)
    (declare (ignore body))
    (when (or (member family '(t nil *standard-input* *standard-output* *query-io*))
	      (and (constantp medium)
		   (keywordp (eval medium)))
	      (probably-stream-or-medium-p family))
      (scl:warn '(:obsolete t) 
		"Converting old style call to ~S to the new style.~%~
		 Please update your code." 'with-text-family))))

(defmacro with-text-face ((medium &optional face) &body body)
  (when (or (member face '(t nil *standard-input* *standard-output* *query-io*))
	    (and (constantp medium)
		 (keywordp (eval medium)))
	    (probably-stream-or-medium-p face))
    #-Genera (warn "Converting old style call to ~S to the new style.~%~
		    Please update your code." 'with-text-face)
    (rotatef medium face))
  `(with-text-style (,medium (make-text-style nil ,face nil)) ,@body))

#+Genera
(defun-property (with-text-face compiler:style-checker) (form)
  (destructuring-bind ((medium &optional face) &rest body) (cdr form)
    (declare (ignore body))
    (when (or (member face '(t nil *standard-input* *standard-output* *query-io*))
	      (and (constantp medium)
		   (keywordp (eval medium)))
	      (probably-stream-or-medium-p face))
      (scl:warn '(:obsolete t) 
		"Converting old style call to ~S to the new style.~%~
		 Please update your code." 'with-text-face))))

(defmacro with-text-size ((medium &optional size) &body body)
  (when (or (member size '(t nil *standard-input* *standard-output* *query-io*))
	    (and (constantp medium)
		 (or (keywordp (eval medium))
		     (realp (eval medium))))
	    (probably-stream-or-medium-p size))
    #-Genera (warn "Converting old style call to ~S to the new style.~%~
		    Please update your code." 'with-text-size)
    (rotatef medium size))
  `(with-text-style (,medium (make-text-style nil nil ,size)) ,@body))

#+Genera
(defun-property (with-text-size compiler:style-checker) (form)
  (destructuring-bind ((medium &optional size) &rest body) (cdr form)
    (declare (ignore body))
    (when (or (member size '(t nil *standard-input* *standard-output* *query-io*))
	      (and (constantp medium)
		   (or (keywordp (eval medium))
		       (realp (eval medium))))
	      (probably-stream-or-medium-p size))
      (scl:warn '(:obsolete t) 
		"Converting old style call to ~S to the new style.~%~
		 Please update your code." 'with-text-size))))


(define-compatibility-function (add-text-style-mapping (setf text-style-mapping))
			       (device character-set style mapping)
  (setf (text-style-mapping device style character-set) mapping))


(define-compatibility-function (draw-character draw-text)
			       (stream character point &rest args)
  (declare (dynamic-extent args))
  (apply #'draw-text stream character point args))

(define-compatibility-function (draw-character* draw-text)
			       (stream character x y &rest args)
  (declare (dynamic-extent args))
  (apply #'draw-text stream character x y args))

(define-compatibility-function (draw-string draw-text)
			       (stream string point &rest args)
  (declare (dynamic-extent args))
  (apply #'draw-text stream string point args))

(define-compatibility-function (draw-string* draw-text)
			       (stream string x y &rest args)
  (declare (dynamic-extent args))
  (apply #'draw-text stream string x y args))

;; DRAW-ICON* was misnamed DRAW-ICON for a while...
(define-compatibility-function (draw-icon draw-pattern*)
			       (stream icon x y &rest args)
  (declare (dynamic-extent args))
  (apply #'draw-pattern* stream icon x y args))

(define-compatibility-function (draw-icon* draw-pattern*)
			       (stream icon x y &rest args)
  (declare (dynamic-extent args))
  (apply #'draw-pattern* stream icon x y args))


(defvar +foreground+ +foreground-ink+)
(defvar +background+ +background-ink+)

#+Genera (compiler:make-obsolete +foreground+ "Use +FOREGROUND-INK+ instead" defvar)
#+Genera (compiler:make-obsolete +background+ "Use +BACKGROUND-INK+ instead" defvar)

(define-compatibility-function (make-color-rgb make-rgb-color)
			       (red green blue)
  (make-rgb-color red green blue))

(define-compatibility-function (make-color-ihs make-ihs-color)
			       (intensity hue saturation)
  (make-ihs-color intensity hue saturation))


(define-compatibility-function (stream-cursor-position*
				stream-cursor-position)
			       (stream)
  (stream-cursor-position stream))

(define-compatibility-function (stream-set-cursor-position*
				stream-set-cursor-position)
			       (stream x y)
  (stream-set-cursor-position stream x y))

(define-compatibility-function (stream-increment-cursor-position*
				stream-increment-cursor-position)
			       (stream dx dy)
  (stream-increment-cursor-position stream dx dy))


(define-compatibility-function (cursor-position* cursor-position)
			       (cursor)
  (cursor-position cursor))

(define-compatibility-function (cursor-set-position* cursor-set-position)
			       (cursor x y)
  (cursor-set-position cursor x y))


(define-compatibility-function (stream-vsp stream-vertical-spacing)
			       (stream)
  (stream-vertical-spacing stream))

(defmacro with-end-of-page-action (#-CLIM-1-compatibility (stream action)
				   #+CLIM-1-compatibility (stream &optional action)
				   &body body &environment env)
  #+CLIM-1-compatibility
  (when (or (keywordp stream)
	    (null action))
    (rotatef stream action)
    #-Genera (warn "Converting old style call to ~S to the new style.~%~
		    Please update your code." 'with-end-of-page-action))
  (default-output-stream stream)
  (let ((actions '(:wrap :scroll :allow))
	(assert-required t)
	(wrapped-body `(letf-globally (((stream-end-of-page-action ,stream) ,action))
			 ,@body)))
    (when (constantp action #+(or Genera Minima) env)
      (setf action (eval action #+(or Genera Minima-Developer) env))
      (if (member action actions)
	  (setf assert-required nil)
	  (warn "~S action must be one of ~S, not ~S" 'with-end-of-page actions action))
      (setf action `',action))
    (when assert-required
      (setf wrapped-body
	    `(progn (assert (member ,action ',actions))
		    ,wrapped-body)))
    wrapped-body))

#+Genera
(defun-property (with-end-of-page-action compiler:style-checker) (form)
  (destructuring-bind ((stream &optional action) &rest body) (cdr form)
    (declare (ignore body))
    (when (or (keywordp stream)
	      (null action))
      (scl:warn '(:obsolete t) 
		"Converting old style call to ~S to the new style.~%~
		 Please update your code." 'with-end-of-page-action))))

(defmacro with-end-of-line-action (#-CLIM-1-compatibility (stream action)
				   #+CLIM-1-compatibility (stream &optional action)
				   &body body &environment env)
  #+CLIM-1-compatibility
  (when (or (keywordp stream)
	    (null action))
    (rotatef stream action)
    #-Genera (warn "Converting old style call to ~S to the new style.~%~
		    Please update your code." 'with-end-of-line-action))
  (default-output-stream stream)
  (let ((actions '(:wrap :scroll :allow))
	(assert-required t)
	(wrapped-body `(letf-globally (((stream-end-of-line-action ,stream) ,action))
			 ,@body)))
    (when (constantp action #+(or Genera Minima) env)
      (setf action (eval action #+(or Genera Minima-Developer) env))
      (if (member action actions)
	  (setf assert-required nil)
	  (warn "~S action must be one of ~S, not ~S" 'with-end-of-line actions action))
      (setf action `',action))
    (when assert-required
      (setf wrapped-body
	    `(progn (assert (member ,action ',actions))
		    ,wrapped-body)))
    wrapped-body))

#+Genera
(defun-property (with-end-of-line-action compiler:style-checker) (form)
  (destructuring-bind ((stream &optional action) &rest body) (cdr form)
    (declare (ignore body))
    (when (or (keywordp stream)
	      (null action))
      (scl:warn '(:obsolete t) 
		"Converting old style call to ~S to the new style.~%~
		 Please update your code." 'with-end-of-line-action))))


(define-compatibility-function (output-record-position* output-record-position)
			       (record)
  (output-record-position record))

(define-compatibility-function (output-record-set-position* output-record-set-position)
			       (record x y)
  (output-record-set-position record x y))

(define-compatibility-function (output-record-start-position*
				output-record-start-cursor-position)
			       (record)
  (output-record-start-cursor-position record))

(define-compatibility-function (output-record-set-start-position*
				output-record-set-start-cursor-position)
			       (record nx ny)
  (output-record-set-start-cursor-position record nx ny))

(define-compatibility-function (output-record-start-position
				output-record-start-cursor-position)
			       (record)
  (multiple-value-bind (x y)
      (output-record-start-cursor-position record)
    (make-point x y)))

(define-compatibility-function (output-record-end-position*
				output-record-end-cursor-position)
			       (record)
  (output-record-end-cursor-position record))

(define-compatibility-function (output-record-set-end-position*
				output-record-set-end-cursor-position)
			       (record nx ny)
  (output-record-set-end-cursor-position record nx ny))

(define-compatibility-function (output-record-element-count output-record-count)
			       (record)
  (output-record-count record))

(define-compatibility-function (output-record-elements output-record-children)
			       (record)
  (output-record-children record))

(define-compatibility-function (replay-1 replay-output-record)
			       (record stream &optional region (x-offset 0) (y-offset 0))
  (replay-output-record record stream region x-offset y-offset))


(define-compatibility-function (output-record-refined-sensitivity-test
				output-record-refined-position-test)
			       (record x y)
  (output-record-refined-position-test record x y))


(define-compatibility-function (output-recording-stream-output-record
				stream-output-history) 
			       (stream)
  (stream-output-history stream))

(define-compatibility-function (output-recording-stream-current-output-record-stack
				stream-current-output-record)
			       (stream)
  (stream-current-output-record stream))

(define-compatibility-function (output-recording-stream-replay stream-replay)
			       (stream &optional region)
  (stream-replay stream region))

#-Genera
(define-compiler-macro add-output-record (&whole form)
  (warn "The function ~S has a different contract than it did in CLIM 1.1.~%~
	 Please check your code."
	'add-output-record)
  form)

#+Genera
(defun-property (add-output-record compiler:style-checker) (form)
  (declare (ignore form))
  (scl:warn '(:obsolete t) 
	    "The function ~S has a different contract than it did in CLIM 1.1.~%~
	     Please check your code."
	    'add-output-record))

(defmacro with-output-recording-options 
	  ((stream &key (draw nil draw-supplied)
			(record nil record-supplied)
			#+CLIM-1-compatibility (draw-p nil draw-p-supplied)
			#+CLIM-1-compatibility (record-p nil record-p-supplied))
	   &body body)
  #+CLIM-1-compatibility
  (when (or draw-p-supplied record-p-supplied)
    (setq draw draw-p
	  draw-supplied draw-p-supplied
	  record record-p
	  record-supplied record-p-supplied)
    #-Genera (warn "Converting old style call to ~S to the new style.~%~
		    Please update your code." 'with-output-recording-options))
  (let ((new-stream (gensymbol 'stream)))
    `(let ((,new-stream ,stream))
       (flet ((with-output-recording-options-body () ,@body))
	 (declare (dynamic-extent #'with-output-recording-options-body))
	 (invoke-with-output-recording-options
	   ,new-stream #'with-output-recording-options-body
	   ,(if record-supplied record `(stream-recording-p ,new-stream))
	   ,(if draw-supplied draw `(stream-drawing-p ,new-stream)))))))

#+Genera
(defun-property (with-output-recording-options compiler:style-checker) (form)
  (destructuring-bind ((stream 
			&key (draw-p nil draw-p-supplied)
			     (record-p nil record-p-supplied)
			&allow-other-keys)
		       &rest body) (cdr form)
    (declare (ignore draw-p record-p body))
    (when (or draw-p-supplied record-p-supplied)
      (scl:warn '(:obsolete t) 
		"Converting old style call to ~S to the new style.~%~
		 Please update your code." 'with-output-recording-options))))

(define-compatibility-function (stream-draw-p stream-drawing-p)
			       (stream)
  (stream-drawing-p stream))

(define-compatibility-function (stream-record-p stream-recording-p)
			       (stream)
  (stream-recording-p stream))


(define-compatibility-function (add-output-record-element add-output-record)
			       (record child)
  (add-output-record child record))

(define-compatibility-function (delete-output-record-element delete-output-record)
			       (record child &optional (errorp t))
  (delete-output-record child record errorp))

(define-compatibility-function (map-over-output-record-elements
				map-over-output-records)
			       (record function
				&optional x-offset y-offset &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (apply #'map-over-output-records
	 function record x-offset y-offset continuation-args))

(define-compatibility-function (map-over-output-record-elements-overlapping-region
				map-over-output-records-overlapping-region)
			       (record region function
				&optional x-offset y-offset &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (apply #'map-over-output-records-overlapping-region
	 function record region x-offset y-offset continuation-args))

(define-compatibility-function (map-over-output-record-elements-containing-point*
				map-over-output-records-containing-position)
			       (record x y function
				&optional x-offset y-offset &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (apply #'map-over-output-records-containing-position
	 function record x y x-offset y-offset continuation-args))


(defmacro formatting-table ((&optional stream
			     &rest options
			     &key x-spacing y-spacing
				  multiple-columns multiple-columns-x-spacing	
				  equalize-column-widths
				  record-type (move-cursor t)
				  #+CLIM-1-compatibility inter-row-spacing
				  #+CLIM-1-compatibility inter-column-spacing
				  #+CLIM-1-compatibility multiple-columns-inter-column-spacing)
			    &body body)
  (declare (ignore x-spacing y-spacing 
		   multiple-columns multiple-columns-x-spacing
		   equalize-column-widths record-type move-cursor))
  #+Genera (declare (zwei:indentation 0 3 1 1))
  #+CLIM-1-compatibility
  (when (or inter-row-spacing inter-column-spacing multiple-columns-inter-column-spacing)
    (setf (getf options :x-spacing) inter-column-spacing)
    (setf (getf options :y-spacing) inter-row-spacing)
    (setf (getf options :multiple-columns-x-spacing) multiple-columns-inter-column-spacing)
    #-Genera (warn "Converting old style call to ~S to the new style.~%~
		    Please update your code." 'formatting-table))
  (default-output-stream stream formatting-table)
  `(flet ((formatting-table-body (,stream) ,@body))
     (declare (dynamic-extent #'formatting-table-body))
     (invoke-formatting-table ,stream #'formatting-table-body ,@options)))

#+Genera
(defun-property (formatting-table compiler:style-checker) (form)
  (destructuring-bind ((&optional stream
			&key inter-row-spacing inter-column-spacing
			     multiple-columns-inter-column-spacing
			&allow-other-keys)
		       &rest body) (cdr form)
    (declare (ignore stream body))
    (when (or inter-row-spacing inter-column-spacing
	      multiple-columns-inter-column-spacing)
      (scl:warn '(:obsolete t) 
		"Converting old style call to ~S to the new style.~%~
		 Please update your code." 'formatting-table))))

(defmacro formatting-cell ((&optional stream
			    &rest options
			    &key (align-x ':left) (align-y ':top)
				 min-width min-height record-type
				 #+CLIM-1-compatibility minimum-width
				 #+CLIM-1-compatibility minimum-height)
			   &body body)
  (declare (ignore align-x align-y min-width min-height record-type))
  #+Genera (declare (zwei:indentation 0 3 1 1))
  #+CLIM-1-compatibility
  (when (or minimum-width minimum-height)
    (setf (getf options :min-width) minimum-width)
    (setf (getf options :min-height) minimum-height)
    #-Genera (warn "Converting old style call to ~S to the new style.~%~
		    Please update your code." 'formatting-cell))
  (default-output-stream stream formatting-cell)
  `(flet ((formatting-cell-body (,stream) ,@body))
     (declare (dynamic-extent #'formatting-cell-body))
     (invoke-formatting-cell ,stream #'formatting-cell-body ,@options)))

#+Genera
(defun-property (formatting-cell compiler:style-checker) (form)
  (destructuring-bind ((&optional stream
			&key minimum-width minimum-height
			&allow-other-keys)
		       &rest body) (cdr form)
    (declare (ignore stream body))
    (when (or minimum-width minimum-height)
      (scl:warn '(:obsolete t) 
		"Converting old style call to ~S to the new style.~%~
		 Please update your code." 'formatting-cell))))

(defmacro formatting-item-list ((&optional stream
				 &rest options
				 &key record-type
				      x-spacing y-spacing initial-spacing
				      n-columns n-rows
				      max-width max-height
				      stream-width stream-height
				      (move-cursor T)
				      #+CLIM-1-compatibility inter-row-spacing
				      #+CLIM-1-compatibility inter-column-spacing)
				&body body)
  (declare (ignore x-spacing y-spacing initial-spacing
		   record-type n-columns n-rows max-width max-height
		   stream-width stream-height move-cursor))
  #+Genera (declare (zwei:indentation 0 3 1 1))
  #+CLIM-1-compatibility
  (when (or inter-row-spacing inter-column-spacing)
    (setf (getf options :x-spacing) inter-column-spacing)
    (setf (getf options :y-spacing) inter-row-spacing)
    #-Genera (warn "Converting old style call to ~S to the new style.~%~
		    Please update your code." 'formatting-item-list))
  (default-output-stream stream formatting-item-list)
  `(flet ((formatting-item-list-body (,stream) ,@body))
     (declare (dynamic-extent #'formatting-item-list-body))
     (invoke-formatting-item-list ,stream #'formatting-item-list-body ,@options)))

#+Genera
(defun-property (formatting-item-list compiler:style-checker) (form)
  (destructuring-bind ((&optional stream
			&key inter-row-spacing inter-column-spacing
			&allow-other-keys)
		       &rest body) (cdr form)
    (declare (ignore stream body))
    (when (or inter-row-spacing inter-column-spacing)
      (scl:warn '(:obsolete t) 
		"Converting old style call to ~S to the new style.~%~
		 Please update your code." 'formatting-item-list))))

(defun format-items (items &key (stream *standard-output*) printer presentation-type
				x-spacing y-spacing initial-spacing
				n-rows n-columns max-width max-height
				(record-type 'standard-item-list-output-record)
				(cell-align-x ':left) (cell-align-y ':top)
				#+CLIM-1-compatibility inter-row-spacing
				#+CLIM-1-compatibility inter-column-spacing)
  #+CLIM-1-compatibility
  (when (or inter-row-spacing inter-column-spacing)
    (setq x-spacing inter-column-spacing
	  y-spacing inter-row-spacing))
  (when (and printer presentation-type)
    (error "Only one of ~S or ~S can be specified." ':printer ':presentation-type))
  (when (and (null printer) (null presentation-type))
    (error "One of ~S or ~S must be specified." ':printer ':presentation-type))
  (formatting-item-list (stream :record-type record-type
				:n-rows n-rows :n-columns n-columns
				:max-width max-width :max-height max-height
				:x-spacing x-spacing :y-spacing y-spacing
				:initial-spacing initial-spacing)
    (flet ((format-item (item)
	     (formatting-cell (stream :align-x cell-align-x :align-y cell-align-y)
	       (cond (printer
		      (funcall printer item stream))
		     (presentation-type
		      (present item presentation-type :stream stream))))))
      (declare (dynamic-extent #'format-item))
      (map nil #'format-item items))))

#-Genera
(define-compiler-macro format-items
		       (&whole form
			items 
			&rest keys
			&key inter-row-spacing inter-column-spacing
			&allow-other-keys)
  (cond ((or inter-row-spacing inter-column-spacing)
	 (warn "Converting old style call to ~S to the new style.~%~
	        Please update your code." 'format-items)
	 (with-keywords-removed (keys keys '(:x-spacing :y-spacing
					     :inter-row-spacing :inter-column-spacing))
	   `(format-items
	      ,items
	      ,@(and inter-row-spacing `(:y-spacing ,inter-row-spacing))
	      ,@(and inter-column-spacing `(:x-spacing ,inter-column-spacing))
	      ,@keys)))
	(t form)))

#+Genera
(defun-property (format-items compiler:style-checker) (form)
  (when (or (getf (cddr form) :inter-row-spacing)
	    (getf (cddr form) :inter-column-spacing))
    (scl:warn '(:obsolete t) 
	      "Converting old style call to ~S to the new style.~%~
	       Please update your code."
	      'format-items)))


(define-compatibility-function (redisplay-1 redisplay-output-record)
			       (record stream 
				&optional check-overlapping x y parent-x parent-y)
  (redisplay-output-record record stream
			   check-overlapping x y parent-x parent-y))


(define-compatibility-function (event-window event-sheet)
			       (event)
  (event-sheet event))

(define-compatibility-function (pointer-event-shift-mask event-modifier-state)
			       (pointer-event)
  (event-modifier-state pointer-event))

(define-compatibility-function (stream-pointer-position* 
				stream-pointer-position)
			       (stream)
  (stream-pointer-position stream))

(define-compatibility-function (stream-set-pointer-position*
				stream-set-pointer-position)
			       (stream x y)
  (stream-set-pointer-position stream x y))

(define-compatibility-function (pointer-position* pointer-position)
			       (pointer)
  (pointer-position pointer))

(define-compatibility-function (pointer-set-position* pointer-set-position)
			       (pointer x y)
  (pointer-set-position pointer x y))


(define-compatibility-function (dragging-output-record drag-output-record)
			       (stream output-record
				&key (repaint t) (erase #'erase-output-record) feedback
				     (finish-on-release nil))
  (drag-output-record stream output-record
		      :repaint repaint
		      :erase erase
		      :feedback feedback
		      :finish-on-release finish-on-release))


(defmacro with-output-as-presentation ((&rest options) &body body)
  #+Genera (declare (zwei:indentation 0 3 1 1))
  (let (stream object type 
	modifier single-box allow-sensitive-inferiors
	parent record-type
	(asi-p '#:asi))
    (cond ((oddp (length options))
	   (setq stream (pop options)
		 object (pop options)
		 type   (pop options))
	   (setq modifier (getf options :modifier))
	   (setq single-box (getf options :single-box))
	   (setq allow-sensitive-inferiors (getf options :allow-sensitive-inferiors asi-p))
	   (setq parent (getf options :parent nil))
	   (setq record-type (getf options :record-type `'standard-presentation)))
	  (t
	   #-Genera (warn "Converting old style call to ~S to the new style.~%~
			   Please update your code." 'with-output-as-presentation)
	   (setq stream (getf options :stream))
	   (setq object (getf options :object))
	   (setq type (getf options :type))
	   (setq modifier (getf options :modifier))
	   (setq single-box (getf options :single-box))
	   (setq allow-sensitive-inferiors (getf options :allow-sensitive-inferiors asi-p))
	   (setq parent (getf options :parent nil))
	   (setq record-type (getf options :record-type `'standard-presentation))))
    (default-output-stream stream)
    ;; Maybe with-new-output-record should turn record-p on?
    (when (eq allow-sensitive-inferiors asi-p)
      (setq allow-sensitive-inferiors '*allow-sensitive-inferiors*))
    (let ((nobject '#:object)			;(once-only (object type) ...)
	  (ntype '#:type))
      `(with-output-recording-options (,stream :record t)
	 (let ((,nobject ,object)
	       (,ntype ,type))
	   (with-new-output-record (,stream (if *allow-sensitive-inferiors*
						,record-type
						'standard-nonsensitive-presentation) nil
				    :object ,nobject
				    :type (if ,ntype
					      (expand-presentation-type-abbreviation ,ntype)
					      (presentation-type-of ,nobject))
				    :single-box ,single-box
				    ,@(when modifier `(:modifier ,modifier))
				    ,@(when parent `(:parent ,parent)))
	     (let ((*allow-sensitive-inferiors* ,allow-sensitive-inferiors))
	       ,@body)))))))

#+Genera
(defun-property (with-output-as-presentation compiler:style-checker) (form)
  (when (evenp (second form))
    (scl:warn '(:obsolete t) 
	      "Converting old style call to ~S to the new style.~%~
	       Please update your code."
	      'with-output-as-presentation)))


(defun accept (type &rest accept-args
	       &key (stream *standard-input*)
		    (view (stream-default-view stream))
		    (default nil default-supplied-p)
		    (default-type type)
		    (history type)
		    (provide-default nil)
		    (prompt t)
		    (prompt-mode ':normal)
		    (display-default prompt)
		    (query-identifier nil)
		    (activation-gestures nil)
		    (additional-activation-gestures nil)
		    (delimiter-gestures nil)
		    (additional-delimiter-gestures nil)
		    #+CLIM-1-compatibility (activation-characters nil)
		    #+CLIM-1-compatibility (additional-activation-characters nil)
		    #+CLIM-1-compatibility (blip-characters nil)
		    #+CLIM-1-compatibility (additional-blip-characters nil)
		    (insert-default nil) (replace-input t)
		    (present-p nil) (active-p t))
  (declare (dynamic-extent accept-args))
  (declare (values object type))
  (declare (ignore prompt-mode display-default query-identifier
		   activation-gestures additional-activation-gestures
		   delimiter-gestures additional-delimiter-gestures 
		   #+CLIM-1-compatibility activation-characters
		   #+CLIM-1-compatibility additional-activation-characters
		   #+CLIM-1-compatibility blip-characters
		   #+CLIM-1-compatibility additional-blip-characters
		   insert-default replace-input present-p active-p))

  ;; Allow the arguments to be presentation type abbreviations
  (multiple-value-bind (expansion expanded)
      (expand-presentation-type-abbreviation type)
    (when expanded
      (when (eq default-type type)
	(setq default-type expansion))
      (when (eq history type)
	(setq history expansion))
      (setq type expansion)))
  (unless (eq default-type type)
    (multiple-value-bind (expansion expanded)
	(expand-presentation-type-abbreviation default-type)
      (when expanded
	(setq default-type expansion)
	(setq accept-args `(:default-type ,default-type ,@accept-args)))))
  (unless (eq history type)
    (multiple-value-bind (expansion expanded)
	(expand-presentation-type-abbreviation history)
      (when expanded
	(setq history expansion)
	(setq accept-args `(:history ,history ,@accept-args)))))

  (let ((insert-default nil))
    (when (and provide-default (null default-supplied-p))
      ;; If the user wants a default, but provided none, go get it from the history
      (let ((history (if (typep history 'basic-history)
			 history
		         (presentation-type-history history))))
	(when history
	  (let ((element (yank-from-history history)))
	    (when element
	      (setq default (presentation-history-element-object element)
		    default-supplied-p t
		    insert-default t))))))
    (when default-supplied-p
      ;; Massage the default
      (multiple-value-bind (new-default new-type)
	  (presentation-default-preprocessor default type :default-type default-type)
	(when (or (not (eq default new-default))
		  (not (eq default-type new-type)))
	  (setq default new-default
		default-type (or new-type default-type)
		insert-default t))))
    (when insert-default
      (setq accept-args `(:default ,default :default-type ,default-type ,@accept-args))))

  (typecase view
    (null)
    (symbol (setq view (make-instance view)))
    (cons   (setq view (apply #'make-instance view))))
  (setq view (decode-indirect-view type view (frame-manager stream)))

  ;; Call STREAM-ACCEPT to do the work.  It would be nice if we could
  ;; call PROMPT-FOR-ACCEPT to generate the real query-id here, but we
  ;; can't because we want to be able to decide exactly how it is called
  ;; on a case-by-case basis.  For example, within ACCEPTING-VALUES...
  (with-keywords-removed (accept-args accept-args '(:stream :view))
    (apply #'stream-accept (encapsulating-stream stream) type
			   :view view :query-identifier query-identifier
			   accept-args)))

(define-compiler-macro accept 
		       (&whole form
			type
			&rest keys
			&key (activation-characters nil activation-chars-p)
			     additional-activation-characters
			     (blip-characters nil blip-chars-p)
			     additional-blip-characters
			&allow-other-keys)
  (cond ((or activation-chars-p additional-activation-characters
	     blip-chars-p additional-blip-characters)
	 #-Genera (warn "Converting old style call to ~S to the new style.~%~
			 Please update your code." 'accept)
	 (with-keywords-removed (keys keys
				 '(:activation-characters :additional-activation-characters
				   :blip-characters :additional-blip-characters
				   :activation-gestures :additional-activation-gestures
				   :delimiter-gestures :additional-delimiter-gestures))
	   `(accept 
	      ,type
	      ,@(and activation-chars-p
		     `(:activation-gestures ,activation-characters))
	      ,@(and additional-activation-characters 
		     `(:additional-activation-gestures ,additional-activation-characters))
	      ,@(and blip-chars-p
		     `(:delimiter-gestures ,blip-characters))
	      ,@(and additional-blip-characters 
		     `(:additional-delimiter-gestures ,additional-blip-characters))
	      ,@keys)))
	(t form)))

#+Genera
(defun-property (accept compiler:style-checker) (form)
  (when (let ((options (cddr form)))
	  (or (getf options :activation-characters)
	      (getf options :additional-activation-characters)
	      (getf options :blip-characters)
	      (getf options :additional-blip-characters)))
    (scl:warn '(:obsolete t) 
	      "Converting old style call to ~S to the new style.~%~
	       Please update your code."
	      'accept)))

(defun accept-1 (stream type
		 &key (view (stream-default-view stream))
		      (default nil default-supplied-p)
		      (default-type type)
		      ((:history history-type) type)
		      (insert-default nil) (replace-input t replace-supplied-p)
		      (prompt t)
		      (present-p nil)
		      (query-identifier nil)
		      (activation-gestures nil activation-gestures-p)
		      (additional-activation-gestures nil)
		      (delimiter-gestures nil delimiter-gestures-p)
		      (additional-delimiter-gestures nil)
		      #+CLIM-1-compatibility (activation-characters nil activation-chars-p)
		      #+CLIM-1-compatibility (additional-activation-characters nil)
		      #+CLIM-1-compatibility (blip-characters nil blip-chars-p)
		      #+CLIM-1-compatibility (additional-blip-characters nil)
		 &allow-other-keys)

  #+CLIM-1-compatibility
  (when (or activation-chars-p additional-activation-characters
	    blip-chars-p additional-blip-characters)
    (setq activation-gestures activation-characters
	  activation-gestures-p activation-chars-p
	  additional-activation-gestures additional-activation-characters
	  delimiter-gestures blip-characters
	  delimiter-gestures-p blip-chars-p
	  additional-delimiter-gestures additional-blip-characters))

  ;; Set up the input editing environment
  (let ((the-object nil)
	(the-type nil)
	(activated t)
	(history nil))

    (cond ((typep history-type 'basic-history)
	   (setq history history-type
		 history-type type))
	  (history-type
	   (setq history (presentation-type-history history-type))))

    ;; Inside ACCEPTING-VALUES, ACCEPT can turn into PRESENT
    (when present-p
      (return-from accept-1
	(accept-present-default type stream view default default-supplied-p
				present-p query-identifier :prompt prompt)))

    (block input-editing
      (flet ((input-sensitizer (continuation stream)
	       (declare (dynamic-extent continuation))
	       (if (stream-recording-p stream)
		   (with-output-as-presentation (stream the-object (or the-type type))
		     (funcall continuation stream))
		   (funcall continuation stream))))
	(declare (dynamic-extent #'input-sensitizer))
	(with-input-editing (stream :input-sensitizer #'input-sensitizer
				    :initial-contents (and insert-default
							   default-supplied-p
							   (list default default-type)))
	  (let ((start-position (stream-scan-pointer stream)))
	    (with-input-context (type)
				(object presentation-type nil options)
	      (with-activation-gestures ((or activation-gestures
					     additional-activation-gestures
					     *standard-activation-gestures*)
					 :override activation-gestures-p)
		(with-delimiter-gestures ((or delimiter-gestures
					      additional-delimiter-gestures)
					  :override delimiter-gestures-p)
		  (handler-bind ((parse-error
				   #'(lambda (error)
				       (declare (ignore error))
				       (when (and default-supplied-p
						  (check-for-default stream start-position
								     default default-type
								     view))
					 (setq the-object default
					       the-type default-type)
					 (return-from input-editing))
				       ;; Decline to handle the parse error
				       nil)))
		    (flet ((accept-help (stream action string-so-far)
			     (declare (ignore action string-so-far))
			     (write-string "You are being asked to enter " stream)
			     (describe-presentation-type type stream)
			     (write-char #\. stream)))
		      (declare (dynamic-extent #'accept-help))
		      (with-accept-help
			  (((:top-level-help :establish-unless-overridden)
			    ;; :ESTABLISH-... here because we want (SEQUENCE PATHNAME)'s
			    ;; help, not both (SEQUENCE PATHNAME) and PATHNAME.
			    #'accept-help))
			;; Call the presentation type's ACCEPT method
			(multiple-value-setq (the-object the-type)
			  (let ((*presentation-type-for-yanking* (and history history-type)))
			    (if default-supplied-p
				(if history
				    (let ((default-element
					    (make-presentation-history-element
					      :object default :type default-type)))
				      (with-default-bound-in-history history default-element
					(funcall-presentation-generic-function accept
					  type stream view
					  :default default :default-type default-type)))
				    (funcall-presentation-generic-function accept
				      type stream view
				      :default default :default-type default-type))
			        (funcall-presentation-generic-function accept
				  type stream view)))))))))

	       ;; A presentation translator was invoked
	       (t 
		 (setq the-object object
		       the-type presentation-type
		       activated nil)
		 (when (if replace-supplied-p
			   replace-input
			   (getf options :echo t))
		   (presentation-replace-input stream object presentation-type view
					       :buffer-start start-position
					       :query-identifier query-identifier))))))))

    ;; The input has been parsed, moused, or defaulted.
    ;; If we are still inside a WITH-INPUT-EDITING at an outer level, leave the
    ;; delimiter in the stream.  But if this was the top level of input, eat
    ;; the activation gesture instead of leaving it in the stream. Don't eat
    ;; the activation gesture on streams that can't ever support input editing,
    ;; such as string streams.
    ;;--- This is really lousy.  We need a coherent theory here.
    (when activated
      (when (and (not (input-editing-stream-p stream))
		 (stream-supports-input-editing stream))
	(let ((gesture (read-gesture :stream stream :timeout 0)))
	  ;;--- For now, just ignore button release events
	  (when (typep gesture 'pointer-button-release-event)
	    (read-gesture :stream stream :timeout 0)))))
    (when (and history (frame-maintain-presentation-histories *application-frame*))
      ;;--- Should this only record stuff that was input via the keyboard?
      (push-history-element history (make-presentation-history-element
				      :object the-object :type (or the-type type))))
    #+compulsive-type-checking
    (when (and the-type (not (eq the-type type)))
      (unless (presentation-subtypep-1 the-type type)
	;; Catch a common bug by verifying that the returned type is a subtype
	;; of the requested type
	(cerror "Return a second value of ~*~*~*~S"
		"The ~S method for the type ~S returned a second value of ~S, ~
		 which is not a subtype of ~S"
		'accept type the-type type)
	(setq the-type type)))
    ;; Ensure that there are no stale highlighting boxes lying around if
    ;; we are exiting via keyboard input
    (when (output-recording-stream-p stream)
      (unhighlight-highlighted-presentation stream t))
    (values the-object (or the-type type))))


(defclass dialog-view (textual-dialog-view) ())
(defclass menu-view (textual-menu-view) ())
(defclass iconic-view (gadget-view) ())

(defvar +dialog-view+ (make-instance 'dialog-view)) 
(defvar +menu-view+ (make-instance 'menu-view))
(defvar +iconic-view+ (make-instance 'iconic-view))

#+Genera (compiler:make-obsolete +dialog-view+ "Use +TEXTUAL-DIALOG-VIEW+ instead" defvar)
#+Genera (compiler:make-obsolete +menu-view+ "Use +TEXTUAL-MENU-VIEW+ instead" defvar)
#+Genera (compiler:make-obsolete +iconic-view+ "Use +GADGET-VIEW+ instead" defvar)


#+++ignore	;this is needed internally by CLIM, so no can do...
(defmacro call-presentation-generic-function (&rest name-and-args)
  (let* ((apply-p (and (eql (first name-and-args) 'apply)
		       (pop name-and-args)))
	 (name (pop name-and-args))
	 (args name-and-args))
    (warn "The function ~S is now obsolete, use ~S instead.~%~
	   Compatibility code is being generated for the time being."
	  'call-presentation-generic-function 
	  (if apply-p 
	      'apply-presentation-generic-function
	      'funcall-presentation-generic-function))
    (if apply-p
	`(apply-presentation-generic-function ,name ,@args)
	`(funcall-presentation-generic-function ,name ,@args))))


(defun test-presentation-translator (translator presentation context-type
				     frame window x y
				     &key event (modifier-state 0) for-menu
					  #+CLIM-1-compatibility (shift-mask 0 shift-mask-p))
  #+CLIM-1-compatibility
  (when shift-mask-p
    (setq modifier-state shift-mask))
  (and (presentation-translator-matches-event translator event modifier-state for-menu)
       (test-presentation-translator-1 translator presentation context-type
				       frame event window x y)))

(define-compiler-macro test-presentation-translator 
		       (&whole form
			translator presentation context-type frame window x y
			&key event (modifier-state 0) for-menu (shift-mask 0 shift-mask-p))
  (declare (ignore modifier-state))
  (cond (shift-mask-p
	 #-Genera (warn "Converting old style call to ~S to the new style.~%~
			 Please update your code." 'test-presentation-translator)
	 `(test-presentation-translator
	    ,translator ,presentation ,context-type ,frame ,window ,x ,y
	    ,@(and event `(:event ,event))
	    :modifier-state ,shift-mask
	    ,@(and for-menu `(:for-menu ,for-menu))))
	(t form)))

#+Genera
(defun-property (test-presentation-translator compiler:style-checker) (form)
  (when (getf (nthcdr 9 form) :shift-mask)
    (scl:warn '(:obsolete t) 
	      "Converting old style call to ~S to the new style.~%~
	       Please update your code."
	      'test-presentation-translator)))


(defun presentation-matches-context-type (presentation context-type
					  frame window x y
					  &key event (modifier-state 0)
					       #+CLIM-1-compatibility
					       (shift-mask 0 shift-mask-p))
  (declare (values translator any-match-p))
  #+CLIM-1-compatibility
  (when shift-mask-p
    (setq modifier-state shift-mask))
  (let ((one-matched nil)
	(translators 
	  (find-presentation-translators 
	    (presentation-type presentation) context-type (frame-command-table frame))))
    (when translators
      (dolist (translator translators)
	(let ((by-gesture
		(presentation-translator-matches-event translator event modifier-state))
	      (by-tester
		(test-presentation-translator-1 translator presentation context-type
						frame event window x y)))
	  (when (and by-gesture by-tester)
	    ;; Matched by both gesture and by the tester, we're done
	    (return-from presentation-matches-context-type
	      (values translator t)))
	  (when by-tester
	    ;; We matched by the tester, it's OK to try the menu translator
	    ;; unless the translator is not supposed to be in a menu.
	    (setq one-matched (or one-matched 
				  (presentation-translator-menu translator))))))
      ;; If EVENT is non-NIL, then we are running on behalf of the user having
      ;; pressed a pointer button, which means that some translator must have
      ;; matched during the test phase, which means that the PRESENTATION-MENU
      ;; translator might be applicable, even though no others were found.
      (let ((menu-applicable
	      (and one-matched
		   *presentation-menu-translator*
		   (test-presentation-translator *presentation-menu-translator*
						 presentation context-type
						 frame window x y
						 :modifier-state modifier-state
						 :event event))))
	   (if (and event menu-applicable)
	       (values *presentation-menu-translator* t)
	       (values nil (and one-matched
				(presentation-translator-matches-event
				  *presentation-menu-translator* event modifier-state))))))))

(define-compiler-macro presentation-matches-context-type 
		       (&whole form
			presentation context-type frame window x y
			&key event (modifier-state 0) (shift-mask 0 shift-mask-p))
  (declare (ignore modifier-state))
  (cond (shift-mask-p
	 #-Genera (warn "Converting old style call to ~S to the new style.~%~
			 Please update your code." 'presentation-matches-context-type)
	 `(presentation-matches-context-type
	    ,presentation ,context-type ,frame ,window ,x ,y
	    ,@(and event `(:event ,event))
	    :modifier-state ,shift-mask))
	(t form)))

#+Genera
(defun-property (presentation-matches-context-type compiler:style-checker) (form)
  (when (getf (nthcdr 8 form) :shift-mask)
    (scl:warn '(:obsolete t) 
	      "Converting old style call to ~S to the new style.~%~
	       Please update your code."
	      'presentation-matches-context-type)))


(defun find-applicable-translators (presentation input-context frame window x y
				    &key event modifier-state (for-menu nil for-menu-p) fastp
					 #+CLIM-1-compatibility (shift-mask nil shift-mask-p))
  #+CLIM-1-compatibility
  (when shift-mask-p
    (setq modifier-state shift-mask))
  (let ((applicable-translators nil))
    (do ((presentation presentation
		       (parent-presentation-with-shared-box presentation window)))
	((null presentation))
      (let ((from-type (presentation-type presentation)))
	;; Loop over the contexts, from the most specific to the least specific
	(dolist (context input-context)
	  (let ((context-type (pop context))	;input-context-type = first
		(tag (pop context)))		;input-context-tag = second
	    (let ((translators (find-presentation-translators
				 from-type context-type (frame-command-table frame))))
	      (when translators
		(dolist (translator translators)
		  (when (and (or (not for-menu-p)
				 (eq (presentation-translator-menu translator) for-menu))
			     (test-presentation-translator translator
							   presentation context-type
							   frame window x y
							   :event event 
							   :modifier-state modifier-state
							   :for-menu for-menu))
		    (when fastp
		      (return-from find-applicable-translators translator))
		    ;; Evacuate the context-type, but don't bother evacuating the
		    ;; tag since it will get used before its extent expires.
		    (push (list translator presentation
				(evacuate-list context-type) tag)
			  applicable-translators))))
	      ;; If we've accumulated any translators, maybe add on PRESENTATION-MENU.
	      ;; If FASTP is T, we will have returned before we get here.
	      (when (and applicable-translators
			 *presentation-menu-translator*
			 (or (not for-menu-p)
			     (eq (presentation-translator-menu *presentation-menu-translator*)
				 for-menu))
			 (test-presentation-translator *presentation-menu-translator*
						       presentation context-type
						       frame window x y
						       :event event 
						       :modifier-state modifier-state
						       :for-menu for-menu))
		(push (list *presentation-menu-translator* presentation
			    (evacuate-list context-type) tag)
		      applicable-translators)))))))
    ;; Since we pushed translators onto the list, the least specific one
    ;; will be at the beginning of the list.  DELETE-DUPLICATES is defined to
    ;; remove duplicated items which appear earlier in the list, so it will
    ;; remove duplicated less specific translators.  Finally, NREVERSE will
    ;; get the translators in most-specific to least-specific order.
    (nreverse (delete-duplicates applicable-translators
				 :test #'(lambda (x y)
					   (and (eq (first x) (first y))
						(eq (second x) (second y))))))))

(define-compiler-macro find-applicable-translators
		       (&whole form
			presentation input-context frame window x y
			&key event (modifier-state 0) (for-menu nil for-menu-p) fastp
			     (shift-mask 0 shift-mask-p))
  (declare (ignore modifier-state))
  (cond (shift-mask-p
	 #-Genera (warn "Converting old style call to ~S to the new style.~%~
			 Please update your code." 'find-applicable-translators)
	 `(find-applicable-translators
	    ,presentation ,input-context ,frame ,window ,x ,y
	    ,@(and event `(:event ,event))
	    :modifier-state ,shift-mask
	    ,@(and for-menu-p `(:for-menu ,for-menu))
	    ,@(and fastp `(:fastp ,fastp))))
	(t form)))

#+Genera
(defun-property (find-applicable-translators compiler:style-checker) (form)
  (when (getf (nthcdr 8 form) :shift-mask)
    (scl:warn '(:obsolete t) 
	      "Converting old style call to ~S to the new style.~%~
	       Please update your code."
	      'find-applicable-translators)))


(defun find-presentation-translators (from-type to-type command-table)
  #+CLIM-1-compatibility
  (when (application-frame-p command-table)
    (setq command-table (frame-command-table command-table)))
  (setq command-table (find-command-table command-table))
  (with-slots (translators-cache) command-table
    (let ((cache translators-cache))		;for speed...
      (with-presentation-type-translator-key (from-key from-type)
	(with-presentation-type-translator-key (to-key to-type)
	  (with-stack-list (key from-key to-key)
	    (multiple-value-bind (translators found-p)
		(and cache (gethash key cache))
	      (cond ((or (null found-p)
			 (/= (pop translators) *translators-cache-tick*))
		     (let ((translators (find-presentation-translators-1 
					  from-key to-key command-table)))
		       (when (null cache)
			 (setq translators-cache
			       (make-hash-table :size *translators-cache-size*
						:test #'equal))
			 (setq cache translators-cache))
		       ;; Need to copy the whole tree, since the from- and to-keys
		       ;; could themselves be stack-consed. 
		       (setf (gethash (copy-tree key) cache)
			     (cons *translators-cache-tick* translators))
		       translators))
		    (t
		     ;; Already popped above
		     translators)))))))))


(defun find-innermost-applicable-presentation
       (input-context stream x y
	&key (frame *application-frame*) 
	     (modifier-state (window-modifier-state stream)) event
	     #+CLIM-1-compatibility (shift-mask 0 shift-mask-p))
  #+CLIM-1-compatibility
  (when shift-mask-p
    (setq modifier-state shift-mask))
  (let ((x (coordinate x))
	(y (coordinate y)))
    (declare (type coordinate x y))
    ;; Depth first search for a presentation that is both under the pointer and
    ;; matches the input context.
    ;; This relies on MAP-OVER-OUTPUT-RECORDS-CONTAINING-POSITION traversing
    ;; the most recently drawn of overlapping output records first.
    (labels 
      ((mapper (record presentations x-offset y-offset)
	 (declare (type coordinate x-offset y-offset))
	 ;; RECORD is an output record whose bounding rectangle contains (X,Y).
	 ;; PRESENTATIONS is a list of non-:SINGLE-BOX presentations that are
	 ;; ancestors of RECORD.
	 ;; X-OFFSET and Y-OFFSET are the position on the drawing plane of the
	 ;; origin of RECORD's coordinate system, i.e. RECORD's parent's start-position.
	 (multiple-value-bind (sensitive superior-sensitive inferior-presentation)
	     ;; SENSITIVE is true if RECORD is a presentation to test against the context.
	     ;; SUPERIOR-SENSITIVE is true if PRESENTATIONS should be tested also.
	     ;; INFERIOR-PRESENTATION is a presentation to pass down to our children.
	     (if (presentationp record)
		 ;;--- This should call PRESENTATION-REFINED-POSITION-TEST
		 (if (output-record-refined-position-test 
		       record (- x x-offset) (- y y-offset))
		     ;; Passed user-defined sensitivity test for presentations.
		     ;; It might be both a presentation and a displayed output record.
		     ;; It might be sensitive now [:single-box t] or the decision might
		     ;; depend on finding a displayed output record [:single-box nil].
		     (let ((displayed (displayed-output-record-p record))
			   (single-box (presentation-single-box record)))
		       (if (or (eq single-box t) (eq single-box :position))
			   ;; This presentation is sensitive
			   (values t displayed nil)
			   ;; This presentation is not presented as a single box,
			   ;; so it contains the point (X,Y) if and only if a
			   ;; visibly displayed inferior contains that point.
			   (values nil displayed record)))
		     (values nil nil nil))
		 ;; RECORD is not a presentation, but a superior presentation's
		 ;; sensitivity might depend on whether record contains (X,Y)
		 (values nil
			 (and presentations
			      (dolist (presentation presentations nil)
				(unless (null presentation) (return t)))
			      (displayed-output-record-p record)
			      ;; Call the refined position test for displayed
			      ;; output records (e.g., ellipses, text, etc.)
			      (output-record-refined-position-test
				record (- x x-offset) (- y y-offset)))
			 nil))
  
	   ;; Add INFERIOR-PRESENTATION to PRESENTATIONS
	   (with-stack-list* (more-presentations inferior-presentation presentations)
	     (when inferior-presentation
	       (setq presentations more-presentations))
    
	     ;; Depth-first recursion
	     (multiple-value-bind (dx dy) (output-record-position record)
	       (map-over-output-records-containing-position 
		 #'mapper record x y
		 (- x-offset) (- y-offset)
		 presentations (+ x-offset dx) (+ y-offset dy)))
    
	     ;; If we get here, didn't find anything in the inferiors of record so test
	     ;; any presentations that are now known to be sensitive, depth-first
	     (when sensitive
	       (test record))
	     (when superior-sensitive
	       (do* ((presentations presentations (cdr presentations))
		     (presentation (car presentations) (car presentations)))
		    ((null presentations))
		 (when presentation
		   (test presentation)
		   ;; A given presentation only has to be tested once
		   (setf (car presentations) nil)))))))
       (test (presentation)
	 ;; This presentation contains the point (X,Y).  See if there is
	 ;; a translator from it to the input context.
	 (dolist (context input-context)
	   (let ((context-type (input-context-type context)))
	     (multiple-value-bind (translator any-match-p)
		 (presentation-matches-context-type presentation context-type
						    frame stream x y
						    :event event
						    :modifier-state modifier-state)
	       (declare (ignore translator))
	       (when any-match-p
		 (return-from find-innermost-applicable-presentation
		   presentation)))))))
      (declare (dynamic-extent #'mapper #'test))
      (mapper (stream-output-history stream) nil (coordinate 0) (coordinate 0)))))

(define-compiler-macro find-innermost-applicable-presentation
		       (&whole form
			input-context window x y
			&key frame event (modifier-state 0) (shift-mask 0 shift-mask-p))
  (declare (ignore modifier-state))
  (cond (shift-mask-p
	 #-Genera (warn "Converting old style call to ~S to the new style.~%~
			 Please update your code." 'find-innermost-applicable-presentation)
	 `(find-innermost-applicable-presentation
	    ,input-context ,window ,x ,y
	    ,@(and frame `(:frame ,frame))
	    ,@(and event `(:event ,event))
	    :modifier-state ,shift-mask))
	(t form)))

#+Genera
(defun-property (find-innermost-applicable-presentation compiler:style-checker) (form)
  (when (getf (nthcdr 6 form) :shift-mask)
    (scl:warn '(:obsolete t) 
	      "Converting old style call to ~S to the new style.~%~
	       Please update your code."
	      'find-innermost-applicable-presentation)))


(defmacro define-gesture-name (name &rest options)
  (let (type gesture-spec unique)
    (cond ((member (first options) '(:keyboard :pointer-button))
	   (setq type (pop options)
		 gesture-spec (pop options)
		 unique (getf options :unique t)))
	  (t
	   #-Genera (warn "Converting old style call to ~S to the new style.~%~
			   Please update your code." 'define-gesture-name)
	   (setq type :pointer-button
		 gesture-spec (cons (getf options :button :left)
				    (getf options :shifts))
		 unique t)))
  (setf (compile-time-property name 'gesture-name) t)
  `(add-gesture-name ',name ',type ',gesture-spec :unique ',unique)))

#+Genera
(defun-property (define-gesture-name compiler:style-checker) (form)
  (unless (member (third form) '(:keyboard :pointer-button))
    (scl:warn '(:obsolete t) 
	      "Converting old style call to ~S to the new style.~%~
	       Please update your code." 'define-gesture-name)))

(define-compatibility-function (add-pointer-gesture-name add-gesture-name)
			       (name button modifiers &key (action :click) (unique t))
  (add-gesture-name name :pointer-button `(,button ,modifiers) :unique unique))

(define-compatibility-function (remove-pointer-gesture-name delete-gesture-name)
			       (name)
  (delete-gesture-name name))


(defmacro with-activation-characters ((additional-characters &key override) &body body)
  #-Genera (warn "The function ~S is now obsolete, use ~S instead.~%~
		  Compatibility code is being generated for the time being."
		 'with-activation-characters 'with-activation-gestures)
  `(with-activation-gestures (,additional-characters :override ,override) ,@body))

#+Genera
(defun-property (with-activation-characters compiler:style-checker) (form)
  (declare (ignore form))
  (scl:warn '(:obsolete t) 
	     "The function ~S is now obsolete, use ~S instead.~%~
	      Compatibility code is being generated for the time being."
	     'with-activation-characters 'with-activation-gestures))

(define-compatibility-function (activation-character-p activation-gesture-p)
			       (character)
  (activation-gesture-p character))

(defmacro with-blip-characters ((additional-characters &key override) &body body)
  #-Genera (warn "The function ~S is now obsolete, use ~S instead.~%~
		  Compatibility code is being generated for the time being."
		 'with-blip-characters 'with-delimiter-gestures)
  `(with-delimiter-gestures (,additional-characters :override ,override) ,@body))

#+Genera
(defun-property (with-blip-characters compiler:style-checker) (form)
  (declare (ignore form))
  (scl:warn '(:obsolete t) 
	     "The function ~S is now obsolete, use ~S instead.~%~
	      Compatibility code is being generated for the time being."
	     'with-blip-characters 'with-delimiter-gestures))

(define-compatibility-function (blip-character-p delimiter-gesture-p)
			       (character)
  (delimiter-gesture-p character))


(define-compatibility-function (input-position stream-scan-pointer)
			       (stream)
  (stream-scan-pointer stream))

(define-compatibility-function (insertion-pointer stream-insertion-pointer)
			       (stream)
  (stream-insertion-pointer stream))

(define-compatibility-function (rescanning-p stream-rescanning-p)
			       (stream)
  (stream-rescanning-p stream))


(defgeneric menu-choose (items &rest keys
			 &key associated-window text-style default-item
			      label printer presentation-type
			      cache unique-id id-test cache-value cache-test
			      max-width max-height n-rows n-columns
			      x-spacing y-spacing
			      cell-align-x cell-align-y
			      pointer-documentation
			      #+CLIM-1-compatibility default-style
			      #+CLIM-1-compatibility inter-row-spacing
			      #+CLIM-1-compatibility inter-column-spacing))

;; Are these reasonable defaults for UNIQUE-ID, CACHE-VALUE, ID-TEST, and CACHE-TEST?
(defmethod menu-choose ((items t) &rest keys
			&key (associated-window (frame-top-level-sheet *application-frame*))
			     text-style default-item
			     label printer presentation-type
			     (cache nil) (unique-id items) (id-test #'equal)
			     (cache-value items) (cache-test #'equal)
			     max-width max-height n-rows n-columns
			     x-spacing y-spacing 
			     (cell-align-x ':left) (cell-align-y ':top)
			     pointer-documentation
			     #+CLIM-1-compatibility default-style
			     #+CLIM-1-compatibility inter-row-spacing
			     #+CLIM-1-compatibility inter-column-spacing)
  (declare (values value chosen-item gesture))
  (declare (ignore associated-window
		   text-style default-item
		   label printer presentation-type
		   cache unique-id id-test cache-value cache-test
		   max-width max-height n-rows n-columns
		   x-spacing y-spacing cell-align-x cell-align-y
		   #+CLIM-1-compatibility default-style
		   #+CLIM-1-compatibility inter-row-spacing
		   #+CLIM-1-compatibility inter-column-spacing
		   pointer-documentation))
  (declare (dynamic-extent keys))
  (unless (zerop (length items))
    (apply #'frame-manager-menu-choose (frame-manager *application-frame*) items keys)))

(define-compiler-macro menu-choose
		       (&whole form
			items 
			&rest keys
			&key inter-row-spacing inter-column-spacing
			&allow-other-keys)
  (cond ((or inter-row-spacing inter-column-spacing)
	 #-Genera (warn "Converting old style call to ~S to the new style.~%~
			 Please update your code." 'menu-choose)
	 (with-keywords-removed (keys keys '(:x-spacing :y-spacing
					     :inter-row-spacing :inter-column-spacing))
	   `(menu-choose
	      ,items
	      ,@(and inter-row-spacing `(:y-spacing ,inter-row-spacing))
	      ,@(and inter-column-spacing `(:x-spacing ,inter-column-spacing))
	      ,@keys)))
	(t form)))

#+Genera
(defun-property (menu-choose compiler:style-checker) (form)
  (when (or (getf (cddr form) :inter-row-spacing)
	    (getf (cddr form) :inter-column-spacing))
    (scl:warn '(:obsolete t) 
	      "Converting old style call to ~S to the new style.~%~
	       Please update your code."
	      'menu-choose)))

(defmethod frame-manager-menu-choose
	   ((framem standard-frame-manager) items &rest keys
	    &key (associated-window
		   (frame-top-level-sheet *application-frame*))
		 text-style default-item
		 label printer presentation-type
		 (cache nil) (unique-id items) (id-test #'equal)
		 (cache-value items) (cache-test #'equal)
		 max-width max-height n-rows n-columns
		 x-spacing y-spacing 
		 (cell-align-x ':left) (cell-align-y ':top)
		 pointer-documentation
		 #+CLIM-1-compatibility default-style
		 #+CLIM-1-compatibility inter-row-spacing
		 #+CLIM-1-compatibility inter-column-spacing)
  (declare (values value chosen-item gesture))
  (declare (ignore keys))
  #+CLIM-1-compatibility
  (when default-style
    (setq text-style default-style))
  #+CLIM-1-compatibility
  (when (or inter-row-spacing inter-column-spacing)
    (setq x-spacing inter-column-spacing
	  y-spacing inter-row-spacing))
  (flet ((present-item (item stream)
	   (present item presentation-type :stream stream)))
    (declare (dynamic-extent #'present-item))
    (let ((item-printer (cond (presentation-type #'present-item)
			      (printer printer)
			      (t #'print-menu-item)))
	  ;; Lucid production compiler tries to use an undefined internal
	  ;; variable if this LET isn't done.
	  #+Lucid (items items))
      (with-menu (menu associated-window :label label)
	(reset-frame (pane-frame menu) :title label)
	(with-text-style (menu text-style)
	  (with-end-of-line-action (menu :allow)
	    (loop
	      (multiple-value-bind (item gesture)
		  (flet ((menu-choose-body (stream presentation-type)
			   (draw-standard-menu stream presentation-type items default-item
					       :item-printer item-printer
					       :max-width max-width :max-height max-height
					       :n-rows n-rows :n-columns n-columns
					       :x-spacing x-spacing :y-spacing y-spacing 
					       :cell-align-x cell-align-x
					       :cell-align-y cell-align-y)))
		    (declare (dynamic-extent #'menu-choose-body))
		    (menu-choose-from-drawer 
		      menu 'menu-item #'menu-choose-body
		      :cache cache
		      :unique-id unique-id :id-test id-test
		      :cache-value cache-value :cache-test cache-test
		      :pointer-documentation pointer-documentation))
		(cond ((menu-item-items item)
		       ;; Set the new item list, then go back through the loop.
		       ;; Don't cache, because that will cause us to see the same
		       ;; menu items again and again.
		       (setq items (menu-item-items item)
			     default-item nil
			     cache nil)
		       (clear-output-history menu))
		      (t (return-from frame-manager-menu-choose
			   (values (menu-item-value item) item gesture))))))))))))

(defun hierarchical-menu-choose (items
				 &key (associated-window
					(frame-top-level-sheet *application-frame*))
				      text-style default-item
				      label printer presentation-type
				      x-position y-position
				      (cache nil)
				      (unique-id items) (id-test #'equal)
				      (cache-value items) (cache-test #'equal)
				      max-width max-height n-rows n-columns
				      x-spacing y-spacing 
				      (cell-align-x ':left) (cell-align-y ':top)
				      #+CLIM-1-compatibility default-style
				      #+CLIM-1-compatibility inter-row-spacing
				      #+CLIM-1-compatibility inter-column-spacing)
  (declare (values value chosen-item gesture))
  #+CLIM-1-compatibility
  (when default-style
    (setq text-style default-style))
  #+CLIM-1-compatibility
  (when (or inter-row-spacing inter-column-spacing)
    (setq x-spacing inter-column-spacing
	  y-spacing inter-row-spacing))
  (flet ((present-item (item stream)
	   (present item presentation-type :stream stream)))
    (declare (dynamic-extent #'present-item))
    (let ((item-printer (cond (presentation-type #'present-item)
			      (printer printer)
			      (t #'print-menu-item))))
      (with-menu (menu associated-window :label label)
	(with-text-style (menu text-style)
	  (multiple-value-bind (item gesture)
	      (flet ((menu-choose-body (stream presentation-type)
		       (draw-standard-menu stream presentation-type items default-item
					   :item-printer item-printer
					   :max-width max-width :max-height max-height
					   :n-rows n-rows :n-columns n-columns
					   :x-spacing x-spacing :y-spacing y-spacing 
					   :cell-align-x cell-align-x
					   :cell-align-y cell-align-y)))
		(declare (dynamic-extent #'menu-choose-body))
		(menu-choose-from-drawer
		  menu 'menu-item #'menu-choose-body
		  :x-position x-position :y-position y-position
		  :leave-menu-visible t
		  :cache cache
		  :unique-id unique-id :id-test id-test
		  :cache-value cache-value :cache-test cache-test))
	    (cond ((menu-item-items item)
		   (with-bounding-rectangle* (ml mt mr mb) menu
		     (declare (ignore ml mb))
		     ;;--- How to pass on LABEL, PRINTER, and PRESENTATION-TYPE?
		     (hierarchical-menu-choose
		       (menu-item-items item)
		       :associated-window associated-window
		       :text-style text-style
		       :x-position mr :y-position mt
		       :cache cache
		       :unique-id unique-id :id-test id-test
		       :cache-value cache-value :cache-test cache-test)))
		  (t (return-from hierarchical-menu-choose
		       (values (menu-item-value item) item gesture))))))))))

(define-compiler-macro hierarchical-menu-choose
		       (&whole form
			items 
			&rest keys
			&key inter-row-spacing inter-column-spacing
			&allow-other-keys)
  (cond ((or inter-row-spacing inter-column-spacing)
	 (warn "Converting old style call to ~S to the new style.~%~
	        Please update your code." 'hierarchical-menu-choose)
	 (with-keywords-removed (keys keys '(:x-spacing :y-spacing
					     :inter-row-spacing :inter-column-spacing))
	   `(hierarchical-menu-choose
	      ,items
	      ,@(and inter-row-spacing `(:y-spacing ,inter-row-spacing))
	      ,@(and inter-column-spacing `(:x-spacing ,inter-column-spacing))
	      ,@keys)))
	(t form)))

#+Genera
(defun-property (hierarchical-menu-choose compiler:style-checker) (form)
  (when (or (getf (cddr form) :inter-row-spacing)
	    (getf (cddr form) :inter-column-spacing))
    (scl:warn '(:obsolete t) 
	      "Converting old style call to ~S to the new style.~%~
	       Please update your code."
	      'hierarchical-menu-choose)))

(defun draw-standard-menu (menu presentation-type items default-item
			   &key (item-printer #'print-menu-item)
				max-width max-height n-rows n-columns
				x-spacing y-spacing 
				(cell-align-x ':left) (cell-align-y ':top)
				#+CLIM-1-compatibility inter-row-spacing
				#+CLIM-1-compatibility inter-column-spacing
			   &aux default-presentation)
  #+CLIM-1-compatibility
  (when (or inter-row-spacing inter-column-spacing)
    (setq x-spacing inter-column-spacing
	  y-spacing inter-row-spacing))
  (formatting-item-list (menu :max-width max-width :max-height max-height
			      :n-rows n-rows :n-columns n-columns
			      :x-spacing x-spacing :y-spacing y-spacing
			      :move-cursor nil)
    (flet ((format-item (item)
	     (let ((type (menu-item-type item)))
	       (flet ((print-item ()
			(formatting-cell (menu :align-x cell-align-x 
					       :align-y cell-align-y)
			  (funcall item-printer item menu))))
		 (declare (dynamic-extent #'print-item))
		 (ecase type
		   (:item 
		     (if (menu-item-active item)
			 (let ((presentation
				 (with-output-as-presentation (menu item presentation-type
							       :single-box t)
				   (print-item))))
			   (when (and default-item
				      (eq item default-item))
			     (setf default-presentation presentation)))
			 (with-drawing-options (menu :ink *command-table-menu-gray*
						     :text-face :bold)
			   (print-item))))
		   (:label 
		     (print-item))
		   (:divider
		     (let* ((width (menu-item-getf item :width 50))
			    (thickness (menu-item-getf item :thickness 2))
			    (ink (menu-item-getf item :ink *command-table-menu-gray*)))
		       (formatting-cell (menu :align-x cell-align-x
					      :align-y :center)
			 (with-local-coordinates (menu)
			   (draw-line* menu 0 0 width 0 
				       :line-thickness thickness :ink ink))))))))))
      (declare (dynamic-extent #'format-item))
      (map nil #'format-item items)))
  default-presentation)

(define-compiler-macro draw-standard-menu
		       (&whole form
			menu presentation-type items default-item 
			&rest keys
			&key inter-row-spacing inter-column-spacing
			&allow-other-keys)
  (cond ((or inter-row-spacing inter-column-spacing)
	 (warn "Converting old style call to ~S to the new style.~%~
	        Please update your code." 'draw-standard-menu)
	 (with-keywords-removed (keys keys '(:x-spacing :y-spacing
					     :inter-row-spacing :inter-column-spacing))
	   `(draw-standard-menu
	      ,menu ,presentation-type ,items ,default-item
	      ,@(and inter-row-spacing `(:y-spacing ,inter-row-spacing))
	      ,@(and inter-column-spacing `(:x-spacing ,inter-column-spacing))
	      ,@keys)))
	(t form)))

#+Genera
(defun-property (draw-standard-menu compiler:style-checker) (form)
  (when (let ((options (nthcdr 5 form)))
	  (or (getf options :inter-row-spacing)
	      (getf options :inter-column-spacing)))
    (scl:warn '(:obsolete t) 
	      "Converting old style call to ~S to the new style.~%~
	       Please update your code."
	      'draw-standard-menu)))

(defmacro define-static-menu (name root-window items
			      &rest keys
			      &key text-style default-item 
				   printer presentation-type
				   max-width max-height n-rows n-columns
				   x-spacing y-spacing 
				   (cell-align-x ':left) (cell-align-y ':top)
				   #+CLIM-1-compatibility default-style
				   #+CLIM-1-compatibility inter-row-spacing
				   #+CLIM-1-compatibility inter-column-spacing)
  (declare (ignore max-width max-height n-rows n-columns
		   x-spacing y-spacing cell-align-x cell-align-y
		   #+CLIM-1-compatibility inter-row-spacing
		   #+CLIM-1-compatibility inter-column-spacing))
  (with-keywords-removed (drawer-keys keys
			  '(:text-style :default-item :printer :presentation-type
			    #+CLIM-1-compatibility :default-style))
    `(defvar ,name (define-static-menu-1 ',name ,root-window ',items
					 :default-item ',default-item
					 :text-style (or ',default-style ',text-style)
					 :presentation-type ',presentation-type
					 :printer ',printer
					 :drawer-args ,(copy-list drawer-keys)))))


(defvar *unsupplied-argument* *unsupplied-argument-marker*)

(defun display-command-table-menu (command-table stream
				   &key max-width max-height
					n-rows n-columns
					x-spacing y-spacing 
					(cell-align-x ':left) (cell-align-y ':top)
					(initial-spacing t) move-cursor
					#+CLIM-1-compatibility inter-row-spacing
					#+CLIM-1-compatibility inter-column-spacing)
  #+CLIM-1-compatibility
  (when (or inter-row-spacing inter-column-spacing)
    (setq x-spacing inter-column-spacing
	  y-spacing inter-row-spacing))
  (unless (or max-width max-height)
    (multiple-value-bind (width height)
	(bounding-rectangle-size (sheet-region stream))
      (unless max-width (setf max-width width))
      (unless max-height (setf max-height height))))
  (let ((menu (slot-value (find-command-table command-table) 'menu)))
    (if (zerop (count-if #'(lambda (x) (not (null (first x)))) menu))
	(with-text-face (stream :italic)
	  (write-string "[No menu items]" stream))
        (formatting-item-list (stream :max-width max-width :max-height max-height
				      :n-rows n-rows :n-columns n-columns
				      :x-spacing x-spacing :y-spacing y-spacing
				      :initial-spacing initial-spacing
				      :move-cursor move-cursor)
	  (dovector (element menu)
	    (cond ((eq (command-menu-item-type (third element)) :divider)
		   (typecase (first element)
		     (string
		       (let ((text-style 
			       (getf (command-menu-item-options (third element)) :text-style)))
			 (with-text-style (stream text-style)
			   (formatting-cell (stream :align-x cell-align-x
						    :align-y cell-align-y)
			     (write-string (first element) stream)))))
		     (null
		       (let* ((options (command-menu-item-options (third element)))
			      (width (getf options :width 50))
			      (thickness (getf options :thickness 2))
			      (ink (getf options :ink *command-table-menu-gray*)))
			 (formatting-cell (stream :align-x cell-align-x
						  :align-y :center)
			   (with-local-coordinates (stream)
			     (draw-line* stream 0 0 width 0 
					 :line-thickness thickness :ink ink)))))))
		  ((first element)
		   (formatting-cell (stream :align-x cell-align-x :align-y cell-align-y)
		     (present element 'command-menu-element
			      :stream stream :single-box t)))))))))

(define-compiler-macro display-command-table-menu
		       (&whole form
			command-table stream
			&rest keys
			&key inter-row-spacing inter-column-spacing
			&allow-other-keys)
  (cond ((or inter-row-spacing inter-column-spacing)
	 #-Genera (warn "Converting old style call to ~S to the new style.~%~
			 Please update your code." 'display-command-table-menu)
	 (with-keywords-removed (keys keys '(:x-spacing :y-spacing
					     :inter-row-spacing :inter-column-spacing))
	   `(display-command-table-menu
	      ,command-table ,stream
	      ,@(and inter-row-spacing `(:y-spacing ,inter-row-spacing))
	      ,@(and inter-column-spacing `(:x-spacing ,inter-column-spacing))
	      ,@keys)))
	(t form)))

(defun-property (display-command-table-menu compiler:style-checker) (form)
  (when (let ((options (nthcdr 3 form)))
	  (or (getf options :inter-row-spacing)
	      (getf options :inter-column-spacing)))
    (scl:warn '(:obsolete t) 
	      "Converting old style call to ~S to the new style.~%~
	       Please update your code."
	      'display-command-table-menu)))


(defun add-command-to-command-table (command-name command-table
				     &key name menu keystroke (errorp t)
					  #+CLIM-1-compatibility test)
  #+CLIM-1-compatibility (declare (ignore test))
  (check-type command-name symbol)
  (setq command-table (find-command-table command-table))
  (when (eq name t)
    (setq name (command-name-from-symbol command-name)))
  (check-type name (or string null))
  (when keystroke
    (assert (keyboard-gesture-spec-p keystroke) (keystroke)
	    "~S is not a keyboard gesture spec" keystroke))
  (when (command-present-in-command-table-p command-name command-table)
    (when errorp
      (cerror "Remove the command and proceed"
	      'command-already-present
	      :format-string "Command ~S already present in ~S"
	      :format-args (list command-name command-table)))
    (remove-command-from-command-table command-name command-table))
  (let ((menu-name nil)
	(menu-options nil))
    (when menu
      (setq menu-name (if (consp menu) (first menu) menu))
      (when (eq menu-name t)
	(setq menu-name (or name (command-name-from-symbol command-name))))
      (check-type menu-name string)
      (setq menu-options (if (consp menu) (rest menu) nil)))
    (with-slots (commands) command-table
      (if name
	  (add-command-line-name-to-command-table command-table name command-name)
	  (setf (gethash command-name commands) t))
      (cond (menu
	     (apply #'add-menu-item-to-command-table
		    command-table menu-name ':command command-name
		    :keystroke keystroke :errorp errorp
		    menu-options))
	    (keystroke
	     (add-keystroke-to-command-table command-table keystroke ':command command-name
					     :errorp errorp)))))
  command-name)

(define-compiler-macro add-command-to-command-table
		       (&whole form
			command-name command-table
			&rest keys &key test &allow-other-keys)
  (cond (test
	 #-Genera (warn "Converting old style call to ~S to the new style.~%~
			 Please update your code." 'add-command-to-command-table)
	 (with-keywords-removed (keys keys '(:test))
	   `(add-command-to-command-table
	      ,command-name ,command-table ,@keys)))
	(t form)))

(defun-property (add-command-to-command-table compiler:style-checker) (form)
  (when (getf (nthcdr 3 form) :test)
    (scl:warn '(:obsolete t) 
	      "Converting old style call to ~S to the new style.~%~
	       Please update your code."
	      'add-command-to-command-table)))


(defun add-keystroke-to-command-table (command-table keystroke type value
				       &key documentation (errorp t)
					    #+CLIM-1-compatibility test)
  #+CLIM-1-compatibility (declare (ignore test))
  (assert (keyboard-gesture-spec-p keystroke) (keystroke)
	  "~S is not a keyboard gesture spec" keystroke)
  (check-type type (member :command :function :menu))
  (check-type documentation (or string null))
  (setq command-table (find-command-table command-table))
  (let ((old-item (find-keystroke-item keystroke command-table
				       :test #'gesture-spec-eql :errorp nil)))
    (when old-item
      (when errorp
	(cerror "Remove the keystroke item and proceed"
		'command-already-present
		:format-string "Keystroke item ~S already present in ~S"
		:format-args (list keystroke command-table)))
	(remove-keystroke-from-command-table command-table keystroke)))
  (add-menu-item-to-command-table command-table nil type value
				  :documentation documentation
				  :keystroke keystroke :errorp nil))

(define-compiler-macro add-keystroke-to-command-table
		       (&whole form
			command-table keystroke type value
			&rest keys &key test &allow-other-keys)
  (cond (test
	 #-Genera (warn "Converting old style call to ~S to the new style.~%~
			 Please update your code." 'add-keystroke-to-command-table)
	 (with-keywords-removed (keys keys '(:test))
	   `(add-keystroke-to-command-table
	      ,command-table ,keystroke ,type ,value ,@keys)))
	(t form)))

(defun-property (add-keystroke-to-command-table compiler:style-checker) (form)
  (when (getf (nthcdr 5 form) :test)
    (scl:warn '(:obsolete t) 
	      "Converting old style call to ~S to the new style.~%~
	       Please update your code."
	      'add-keystroke-to-command-table)))


(defun remove-keystroke-from-command-table (command-table keystroke
					    &key (errorp t)
						 #+CLIM-1-compatibility test)
  #+CLIM-1-compatibility (declare (ignore test))
  (setq command-table (find-command-table command-table))
  (with-slots (menu keystrokes) command-table
    (let ((index (position keystroke menu :key #'second :test #'gesture-spec-eql)))
      (cond (index
	     (let ((element (aref menu index)))
	       ;; Don't remove the whole item if there's a menu-name,
	       ;; just remove the accelerator
	       (when (stringp (first element))
		 (setf (second element) nil)
		 (return-from remove-keystroke-from-command-table nil)))
	     (unless (= (1+ index) (fill-pointer menu))
	       (replace menu menu :start1 index :start2 (1+ index))
	     (decf (fill-pointer menu))
	     (setq keystrokes nil)))
	    (t
	     (when errorp
	       (error 'command-not-present
		      :format-string "Keystroke item ~S not present in ~S"
		      :format-args (list keystroke command-table))))))))

(define-compiler-macro remove-keystroke-from-command-table
		       (&whole form
			command-table keystroke
			&rest keys &key test &allow-other-keys)
  (cond (test
	 #-Genera (warn "Converting old style call to ~S to the new style.~%~
			 Please update your code." 'remove-keystroke-from-command-table)
	 (with-keywords-removed (keys keys '(:test))
	   `(remove-keystroke-from-command-table
	      ,command-table ,keystroke ,@keys)))
	(t form)))

(defun-property (remove-keystroke-from-command-table compiler:style-checker) (form)
  (when (getf (nthcdr 3 form) :test)
    (scl:warn '(:obsolete t) 
	      "Converting old style call to ~S to the new style.~%~
	       Please update your code."
	      'remove-keystroke-from-command-table)))


(defun read-command (command-table
		     &key (stream *standard-input*)
			  (command-parser *command-parser*)
			  (command-unparser *command-unparser*)
			  (partial-command-parser *partial-command-parser*)
			  (use-keystrokes nil)
			  #+CLIM-1-compatibility keystroke-test)
  #+CLIM-1-compatibility (declare (ignore keystroke-test))
  (if use-keystrokes
      (with-command-table-keystrokes (keystrokes command-table)
	(read-command-using-keystrokes command-table keystrokes
				       :stream stream
				       :command-parser command-parser
				       :command-unparser command-unparser
				       :partial-command-parser partial-command-parser))
      (let ((*command-parser* command-parser)
	    (*command-unparser* command-unparser)
	    (*partial-command-parser* partial-command-parser))
        (values (accept `(command :command-table ,command-table)
			:stream stream :prompt nil)))))

(define-compiler-macro read-command
		       (&whole form
			command-table
			&rest keys &key keystroke-test &allow-other-keys)
  (cond (keystroke-test
	 #-Genera (warn "Converting old style call to ~S to the new style.~%~
			 Please update your code." 'read-command)
	 (with-keywords-removed (keys keys '(:keystroke-test))
	   `(read-command ,command-table ,@keys)))
	(t form)))

(defun-property (read-command compiler:style-checker) (form)
  (when (getf (nthcdr 2 form) :keystroke-test)
    (scl:warn '(:obsolete t) 
	      "Converting old style call to ~S to the new style.~%~
	       Please update your code." 'read-command)))


(defun read-command-using-keystrokes (command-table keystrokes
				      &key (stream *standard-input*)
					   (command-parser *command-parser*)
					   (command-unparser *command-unparser*)
					   (partial-command-parser *partial-command-parser*)
					   #+CLIM-1-compatibility keystroke-test)
  #+CLIM-1-compatibility (declare (ignore keystroke-test))
  (let ((*command-parser* command-parser)
	(*command-unparser* command-unparser)
	(*partial-command-parser* partial-command-parser))
    ;; NUMERIC-ARG only applies when we read a keystroke accelerator
    (multiple-value-bind (command numeric-arg)
	(block keystroke
	  (handler-bind ((accelerator-gesture
			   #'(lambda (c)
			       (return-from keystroke
				(values
				  (accelerator-gesture-event c)
				  (accelerator-gesture-numeric-argument c))))))
	    (let ((*accelerator-gestures* keystrokes))
	      (accept `(command :command-table ,command-table)
		      :stream stream :prompt nil))))
      (if (keyboard-event-p command)
	  (let ((command (lookup-keystroke-command-item command command-table
							:numeric-argument numeric-arg)))
	    (if (partial-command-p command)
		(funcall *partial-command-parser*
			 command command-table stream nil :for-accelerator t)
	        command))
	  command))))

(define-compiler-macro read-command-using-keystrokes
		       (&whole form
			command-table keystrokes
			&rest keys &key keystroke-test &allow-other-keys)
  (cond (keystroke-test
	 #-Genera (warn "Converting old style call to ~S to the new style.~%~
			 Please update your code." 'read-command-using-keystrokes)
	 (with-keywords-removed (keys keys '(:keystroke-test))
	   `(read-command-using-keystrokes ,command-table ,keystrokes ,@keys)))
	(t form)))

(defun-property (read-command-using-keystrokes compiler:style-checker) (form)
  (when (getf (nthcdr 3 form) :keystroke-test)
    (scl:warn '(:obsolete t) 
	      "Converting old style call to ~S to the new style.~%~
	       Please update your code." 'read-command-using-keystrokes)))


(define-compatibility-function (set-frame-layout (setf frame-current-layout))
			       (frame layout)
  (setf (frame-current-layout frame) layout))

(define-compatibility-function (frame-top-level-window frame-top-level-sheet)
			       (frame)
  (frame-top-level-sheet frame))

(define-compatibility-function (command-enabled-p command-enabled)
			       (command-name frame)
  (command-enabled command-name frame))

(define-compatibility-function (enable-command (setf command-enabled))
			       (command-name frame)
  (setf (command-enabled command-name frame) t))

(define-compatibility-function (disable-command (setf command-enabled))
			       (command-name frame)
  (setf (command-enabled command-name frame) nil))


(defmacro with-frame-state-variables
	  ((frame-name &optional (frame '*application-frame*)) &body body)
  #-Genera (warn "The macro ~S is obsolete, use ~S instead."
		 'with-frame-state-variables 'with-slots)
  (let* ((slots (clos:class-slots (clos:find-class frame-name)))
	 (slot-names (mapcar #'clos:slot-definition-name slots)))
    `(with-slots ,slot-names ,frame ,@body)))

#+Genera
(defun-property (with-frame-state-variables compiler:style-checker) (form)
  (scl:warn '(:obsolete t) 
	    "The macro ~S is obsolete, use ~S instead." 
	    'with-frame-state-variables 'with-slots))


(define-compatibility-function (window-viewport-position*
				window-viewport-position)
			       (window)
  (window-viewport-position window))

(define-compatibility-function (window-set-viewport-position* 
				window-set-viewport-position)
			       (window x y)
  (window-set-viewport-position window x y))


(define-compatibility-function (position-window-near-carefully position-sheet-carefully)
			       (window x y)
  (position-sheet-carefully window x y))

(define-compatibility-function (position-window-near-pointer position-sheet-near-pointer)
			       (window &optional x y)
  (position-sheet-near-pointer window x y))

(define-compatibility-function (size-menu-appropriately size-frame-from-contents)
			       (menu &key width height right-margin bottom-margin size-setter)
  (size-frame-from-contents menu
			    :width width :height height
			    :right-margin right-margin :bottom-margin bottom-margin
			    :size-setter size-setter))

(define-compatibility-function (open-root-window find-port)
			       (type &rest args)
  (declare (ignore args))
  (find-port :server-path (if (listp type) type (list type))))
