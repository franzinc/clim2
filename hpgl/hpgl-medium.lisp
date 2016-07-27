;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: hpgl-clim; Base: 10; Lowercase: Yes -*-

;; See the file LICENSE for the full license governing this code.

(in-package :hpgl-clim)


#|
Introduction to the HPGL plotting language

1. Plotter units are 0.025mm which converts to 1016 per inch.

The code sets up a top-left coordinate system where the device-unit is
a point. Hence we have 72dpi resolution printer.


|#


;;; HPGL generation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syntax of the operator is:
;;; () or absent means use the name of the function.
;;; A symbol means use string-downcase on its name
;;; A list means to apply FORMAT to the list (at compile time...)
(defmacro define-hpgl-operation (name &optional operator &rest operands)
  (when (null operator) (setf operator name))
  (if (listp operator)
      (setf operator (apply #'format nil operator))
      (setf operator (string-upcase (string operator))))
  
  (labels ((parse-operand (operand)
	     (if (listp operand)
		 (values (first operand) (second operand))
		 (values operand 'integer)))
	   (generate-operand (operand)
	     (multiple-value-bind (operand type) (parse-operand operand)
	       (ecase type
		 (integer `(progn (hpgl-decimal ,operand stream) (format stream ",")))
		 (string `(progn (hpgl-string ,operand stream) (format stream ",")))
		 (symbol `(progn (hpgl-symbol ,operand stream) (format stream ",")))
		 (vector `(progn (hpgl-vector ,operand stream) (format stream ","))))))
	   (generate-last (operand)
	     (multiple-value-bind (operand type) (parse-operand operand)
	       (ecase type
		 (integer `(hpgl-decimal ,operand stream))
		 (string `(hpgl-string ,operand stream))
		 (symbol `(hpgl-symbol ,operand stream))
		 (vector `(hpgl-vector ,operand stream))))))

    `(define-group ,name define-hpgl-operation
      (defun ,name (stream ,@(mapcar #'parse-operand operands))
	(with-hpgl-color
	    (hpgl-operation ,operator stream)
	  ,@(mapcar #'generate-operand (butlast operands))
	  ,@(mapcar #'generate-last (last operands))
	  (format stream ";~%"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; HP-GL ints must be between -2^26+1 and 2^26-1 with no more that 10 sigfig's after the decimal
;;;
(defun hpgl-decimal (value stream)
  (format stream "~6,6F" value))

;;;
;;; HP-GL Labels must be terminated by an ETX (ascii 3)
;;; 

(defvar *hpgl-label-terminator* "
")

(defun hpgl-string (string stream)
  (format stream "~A~A" string *hpgl-label-terminator*))


;;;
;;; Is this really needed ?????
;;;
;;; Write out an HP-GL command 
;;;
(defun hpgl-operation (op stream)
  (format stream "~:@(~A~)" op))


(defun hpgl-vector (value stream &optional optimal-flonize)
  (declare (ignore optimal-flonize))
  (write-char #\[ stream)
  (doseq (element value)
	 (hpgl-decimal element stream)
	 (format stream " "))
  (write-char #\] stream)
  (write-char #\space stream))

(defun hpgl-symbol (value stream)
  (format stream "/~A " value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Color functions
;;;

(defvar *gray-level* 0)

(defmacro with-hpgl-color (&rest body)
  `(when (not (= *gray-level* 1.0))
    ,@body))

(defun setgray (stream value)
  (declare (ignore stream))
  (setf *gray-level* value))

(define-hpgl-operation setcolor setrgbcolor
  (red integer)
  (green integer)
  (blue integer))

(define-hpgl-operation set-fill-type FT
  (kind integer)
  (space integer)
  (angle integer))

(define-hpgl-operation set-default-fill FT)
  
(defun fill-cross-hatch (printer-stream space &key (on t))
  (if on
      (set-fill-type printer-stream 4 space 45)
      (set-default-fill printer-stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing Functions
;;;

(defun edge-rectangle (stream x y)
  (edge-rectangle-1 stream (trans-x x) (trans-y y))) 

(define-hpgl-operation edge-rectangle-1 EA
  (x integer)
  (y integer))
 
(defun moveto (stream x y)
  (moveto-1 stream (trans-x x) (trans-y y)))

(define-hpgl-operation moveto-1 PU
  (x integer)
  (y integer))

;;---

(defun trans-x (x) x)
(defun trans-y (y) y)

(defun lineto (stream x y)
  (lineto-1 stream (trans-x x) (trans-y y)))

(define-hpgl-operation lineto-1 PD
  (x integer)
  (y integer))

(define-hpgl-operation penup PU)
(define-hpgl-operation pendown PD)

(define-hpgl-operation fill-arc WG 
  (radius integer)
  (start integer)
  (end integer))

(define-hpgl-operation fill-rectangle RA
  (x integer)
  (y integer))

(define-hpgl-operation arc AA
  (x integer)
  (y integer)
  (degrees integer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Util Functions 
;;;

(define-hpgl-operation scale SC
  (x-min integer)
  (x-max integer)
  (y-min integer)
  (y-max integer))

(define-hpgl-operation input-window IW
  (left integer)
  (top integer)
  (right integer)
  (bottom integer))

(define-hpgl-operation input-points IP
  (left integer)
  (top integer)
  (right integer)
  (bottom integer))

;;; Rotate character drawing angle
(defun rotate-90 (stream on-off)
  (case on-off
    ((:on)
     (format stream "DI0,1;~%"))
    ((:off)
     (format stream "DI1,0;~%"))))

(define-hpgl-operation set-absolute-direction-1 DI
  (run integer)
  (rise integer))

(defun set-absolute-direction (stream run rise)
  (set-absolute-direction-1 stream (round run) (round rise)))

(defun move (printer-stream x y)
;;  (format printer-stream "PR")
  (moveto printer-stream x y)
;;  (format printer-stream "PA;~%")
  )

(define-hpgl-operation concat ()
  (transformation-matrix vector))

(define-hpgl-operation setlinewidth ()
  width)
(define-hpgl-operation setlinejoin ()
  join-type-number)
(define-hpgl-operation setlinecap ()
  cap-type-number)

(define-hpgl-operation setdash ()
  (dash-pattern vector)
  initial-dash-phase)

(define-hpgl-operation clip)
(define-hpgl-operation stroke)
(define-hpgl-operation ps-fill fill)

(define-hpgl-operation closepath)
(define-hpgl-operation newpath)
(define-hpgl-operation showpage ("showpage~%%%Page: ? ?")) ;Put %%Page: ? everywhere.


;;; Utility functions.

(defun use-line-style (medium line-style)
  ;;--- Need to implement
  medium
  line-style
  )

(defmethod maybe-set-color ((medium hpgl-medium) (ink t))
  (warn "cannot use ink ~S" ink))

;;;-- This is copied from postscript

(defun color-equal (c1 c2)
  (or (eq c1 c2)
      (and (colorp c1)
	   (colorp c2)
	   (multiple-value-bind (r1 g1 b1) (color-rgb c1)
	     (multiple-value-bind (r2 g2 b2) (color-rgb c2)
	       (and (= r1 r2) (= g1 g2) (= b1 b2)))))))

(defmethod maybe-set-color
	   ((medium hpgl-medium) (ink (eql +foreground-ink+)))
  (maybe-set-color medium (or (medium-foreground medium) +black+)))

(defmethod maybe-set-color
	   ((medium hpgl-medium) (ink (eql +background-ink+)))
  (maybe-set-color medium (or (medium-background medium) +white+)))

(defmethod maybe-set-color
	   ((medium hpgl-medium) (ink flipping-ink))
  (error "Hpgl devices can't draw with flipping inks."))

(defmethod maybe-set-color ((medium hpgl-medium) (ink color))
  (with-slots (current-color printer-stream) medium
    (when (or (null current-color)
	      (not (color-equal current-color ink)))
      (setf current-color ink)
      (multiple-value-bind (r g b) (color-rgb ink)
      #-ignore
      (setgray printer-stream (color-luminosity r g b))
      #+ignore
      (setcolor printer-stream r g b)))))

(defmethod maybe-set-color ((medium hpgl-medium) (ink rectangular-tile))
  ;; Handled in the patterned fill/stroke case
  )

(defmethod maybe-set-color
	   ((medium hpgl-medium) (ink contrasting-ink))
  (maybe-set-color medium (make-color-for-contrasting-ink ink)))


(defmethod maybe-set-font ((medium hpgl-medium) text-style)
  (with-slots (current-text-style printer-stream) medium
    (when (or (null current-text-style)
	      (not (eq current-text-style text-style)))
      (setf current-text-style text-style)
      (use-font medium printer-stream text-style))))

(defmethod use-font ((medium hpgl-medium) printer-stream pfd)
  (let ((size (pfd-point-size pfd)))
    (format printer-stream "SI~D,~D;~%"
		  (/ (* (* size .6) (/ 1016.0 72.0)) 600.0)
		  (/ (* size (/ 1016.0 72.0)) 600.0))))

(defun maybe-set-clipping-region (medium)
  ;;--- Not implemented
  medium
  )
 
;;; Graphics functions

;;;-- Ripped off from the postscript stuff

(defmacro convert-to-hpgl-coordinates (transform &body positions)
  (assert (evenp (length positions)) (positions)
	  "There must be an even number of elements in ~S" positions)
  (let ((forms nil))
    (loop
      (when (null positions)
	(return `(progn ,@(nreverse forms))))
      (let* ((x (pop positions))
	     (y (pop positions)))
	(push `(multiple-value-setq (,x ,y)
		 (transform-position ,transform ,x ,y))
	      forms)))))

(defmacro convert-to-hpgl-distances (transform &body positions)
  (assert (evenp (length positions)) (positions)
	  "There must be an even number of elements in ~S" positions)
  (let ((forms nil))
    (loop
      (when (null positions)
	(return `(progn ,@(nreverse forms))))
      (let* ((x (pop positions))
	     (y (pop positions)))
	(push `(multiple-value-setq (,x ,y)
		 (transform-distance ,transform ,x ,y))
	      forms)))))

(defmacro with-hpgl-drawing-options ((medium printer-stream-var
					    &key ink (filled nil filled-p) line-style)
					   &body body)
  (let ((printer-stream (or printer-stream-var (make-symbol (symbol-name 'printer-stream)))))
    `(let ((,printer-stream (slot-value ,medium 'printer-stream)))
       (when (and ,@(and filled-p `((not ,filled))) ,line-style)
	 (use-line-style ,medium ,line-style))
       (maybe-set-color ,medium ,ink)
       (maybe-set-clipping-region ,medium)
       ,@body)))


;;; Actual graphics code

(defmethod medium-draw-point* ((medium hpgl-medium) x y)
  (let* ((transform (sheet-device-transformation (medium-sheet medium)))
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium)))
    (convert-to-hpgl-coordinates transform x y)
    (with-hpgl-drawing-options (medium printer-stream
				       :ink ink :line-style line-style)
      (moveto printer-stream x y)
      (lineto printer-stream x y))))

(defmethod medium-draw-line* ((medium hpgl-medium) x1 y1 x2 y2)
  (let* ((transform (sheet-device-transformation (medium-sheet medium)))
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium)))
    (convert-to-hpgl-coordinates transform x1 y1 x2 y2)
    (with-hpgl-drawing-options (medium printer-stream
				       :ink ink :line-style line-style)
      (moveto printer-stream x1 y1)
      (lineto printer-stream x2 y2))))

(defmethod medium-draw-lines* ((medium hpgl-medium) position-seq)
  (let* ((transform (sheet-device-transformation (medium-sheet medium)))
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium)))
    (with-hpgl-drawing-options (medium printer-stream
				      :ink ink :line-style line-style)
      (map-endpoint-sequence
       #'(lambda (x1 y1 x2 y2)
	   (convert-to-hpgl-coordinates transform x1 y1 x2 y2)
	   (moveto printer-stream x1 y1)
	   (lineto printer-stream x2 y2))
	position-seq))))


(defmethod medium-draw-text* ((medium hpgl-medium)
			      string-or-char x y start end
			      align-x align-y
			      towards-x towards-y transform-glyphs)
  (if (characterp string-or-char)
      (medium-draw-character* medium string-or-char x y 
			      align-x align-y towards-x towards-y transform-glyphs)
      (medium-draw-string* medium string-or-char x y start end 
			   align-x align-y towards-x towards-y transform-glyphs)))



(defvar ch1buf (make-string 1))

(defmethod medium-draw-character* ((medium hpgl-medium)
				   character x y align-x align-y
				   towards-x towards-y transform-glyphs)
  (declare (ignore transform-glyphs))
  (let* ((transform (sheet-device-transformation (medium-sheet medium)))
	 (ink (medium-ink medium))
	 (text-style (medium-merged-text-style medium)))
    (convert-to-hpgl-coordinates transform x y)
    (when towards-x
      (convert-to-hpgl-coordinates transform towards-x towards-y))
    (with-slots (printer-stream stream) medium
      (setf (aref ch1buf 0) character)
      (let* ((fcs (text-style-mapping (port medium) text-style))
	     (height (text-style-height text-style medium))
	     (descent (text-style-descent text-style medium))
	     (ascent (text-style-ascent text-style medium)))
	(let ((x-adjust 
	       (compute-text-x-adjustment align-x medium character text-style 0 1))
	      (y-adjust 
	       (compute-text-y-adjustment align-y descent ascent height)))
	  (incf x x-adjust)
	  (incf y y-adjust)
	  (when towards-x
	    (incf towards-x x-adjust)
	    (incf towards-y y-adjust)))
	
	(maybe-set-font medium fcs)
	
	(if towards-x
	    (with-hpgl-drawing-options (medium printer-stream :ink ink)
	      (moveto printer-stream x y)
	      (set-absolute-direction printer-stream (- towards-x x) (- towards-y y))
	      (carefully-output-ps-showstring printer-stream ch1buf 0 1)
	      (set-absolute-direction printer-stream 1 0))

	  (with-hpgl-drawing-options (medium printer-stream :ink ink)
	    (moveto printer-stream x y)
	    (carefully-output-ps-showstring printer-stream ch1buf 0 1)))))))

(defmethod medium-draw-string* ((medium hpgl-medium)
				string x y start end align-x align-y
				towards-x towards-y transform-glyphs)
  (declare (ignore transform-glyphs))
  (unless start (setq start 0))
  (unless end (setq end (length string)))
  (let* ((transform (sheet-device-transformation (medium-sheet medium)))
	 (ink (medium-ink medium))
	 (text-style (medium-merged-text-style medium)))

    (convert-to-hpgl-coordinates transform x y)
    (when towards-x
      (convert-to-hpgl-coordinates transform towards-x towards-y))

    (let* ((fcs (text-style-mapping (port medium) text-style))
	   (height (text-style-height text-style medium))
	   (descent (text-style-descent text-style medium))
	   (ascent (text-style-ascent text-style medium)))
      (let ((x-adjust 
	     (compute-text-x-adjustment align-x medium string text-style start end))
	    (y-adjust 
	     (compute-text-y-adjustment align-y descent ascent height)))
	(incf x x-adjust)
	(incf y y-adjust)
	(when towards-x
	  (incf towards-x x-adjust)
	  (incf towards-y y-adjust)))
      ;; do raster/ink stuff.

      (maybe-set-font medium fcs)
      
      (if towards-x
	  (with-hpgl-drawing-options (medium printer-stream :ink ink)
	    (moveto printer-stream x y)
	    (set-absolute-direction printer-stream (- towards-x x) (- towards-y y))
	    (carefully-output-ps-showstring printer-stream string start end)
	    (set-absolute-direction printer-stream 1 0))

	(with-hpgl-drawing-options (medium printer-stream :ink ink)
	  (moveto printer-stream x y)
	  (carefully-output-ps-showstring printer-stream string start end))))))

(defun carefully-output-ps-showstring (printer-stream string-or-char start end)
  (hpgl-operation "LB" printer-stream)
  (etypecase string-or-char
    (string
     (do-delimited-substrings ((string-or-char :start start :end end)
			       (substring-start substring-end delimiter))
       (write-string string-or-char printer-stream
		     :start substring-start :end substring-end)
       ((#\( #\) #\\)
	 (write-char #\\ printer-stream)
	 (write-char delimiter printer-stream))))
    (character (when (or (eql string-or-char #\()
			 (eql string-or-char #\))
			 (eql string-or-char #\\))
		 (write-char #\\ printer-stream)
		 (write-char string-or-char printer-stream))))
  ;(write-char #\) printer-stream)
  ;(write-char #\space printer-stream)
  ;(ps-operation "show" printer-stream)
  (format printer-stream "~%")
  )

;;--- This is a direct ripoff from postscript

(defmethod medium-text-bounding-box ((medium hpgl-medium)
				     string x y start end align-x
				     align-y text-style
				     towards-x towards-y
				     transform-glyphs transformation)
  (declare (ignore string start end transform-glyphs transformation
		   towards-x towards-y text-style align-x align-y x y))
  (multiple-value-bind
      (left top right bottom cx cy towards-x towards-y) (call-next-method)
    (if (and towards-y towards-x)
	(let ((transformation
	       (make-rotation-transformation 
		(atan (- towards-y cy) (- towards-x cx))
		(make-point cx cy))))
	  (multiple-value-setq (left top)
	    (transform-position transformation left top))
	  (multiple-value-setq (right bottom)
	    (transform-position transformation right bottom))
	  (values (min left right) (min top bottom)
		  (max left right) (max top bottom)))
      (values left top right bottom))))

(defmethod medium-draw-rectangle* ((medium hpgl-medium)
				   left top right bottom filled)
  (let* ((transform (sheet-device-transformation (medium-sheet medium)))
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium)))
    (convert-to-hpgl-coordinates transform left top right bottom)
    (with-hpgl-drawing-options (medium printer-stream
				       :filled filled
				       :ink ink :line-style line-style)
      (medium-draw-rectangle*-1 printer-stream left right top bottom filled))))

(defun medium-draw-rectangle*-1 (printer-stream left right top bottom filled)
  (if filled
      (progn
	(fill-cross-hatch printer-stream 5 :on t)
	(moveto printer-stream left top)
	(fill-rectangle printer-stream right bottom)
	(fill-cross-hatch printer-stream 5 :on nil)

	(moveto printer-stream left top)
	(edge-rectangle printer-stream right bottom))

    (progn
      (moveto printer-stream left top)
      (lineto printer-stream right top)
      (lineto printer-stream right bottom)
      (lineto printer-stream left bottom)
      (lineto printer-stream left top)
      #+ignore
      (edge-rectangle printer-stream right bottom))))
  
(defmethod medium-draw-rectangles* ((medium hpgl-medium) position-seq filled)
  (let* ((transform (sheet-device-transformation (medium-sheet medium)))
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium)))
    (with-hpgl-drawing-options (medium printer-stream
				      :ink ink :filled filled :line-style line-style)
      (map-endpoint-sequence
       #'(lambda (left top right bottom)
	   (convert-to-hpgl-coordinates transform left top right bottom)
	   (medium-draw-rectangle*-1 printer-stream left right top bottom filled))
	position-seq))))

(defmethod medium-draw-polygon* ((medium hpgl-medium) position-seq closed filled)
  ;;--- This does not handle filled
  (let* ((transform (sheet-device-transformation (medium-sheet medium)))
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium)))
    (with-hpgl-drawing-options (medium printer-stream
				       :filled filled
				       :ink ink :line-style line-style)
    
      (if (listp position-seq)
	  (let* ((position-seq position-seq)
		 (x (pop position-seq))
		 (y (pop position-seq)))
	    
	    (convert-to-hpgl-coordinates transform x y)
	    (moveto printer-stream x y)
	    (loop (when (null position-seq) (return))
					 (setf x (pop position-seq)
					       y (pop position-seq))
					 (convert-to-hpgl-coordinates transform x y)
					 (lineto printer-stream x y)))
	(progn
	  (let ((x (aref position-seq 0))
		(y (aref position-seq 1)))
	    (convert-to-hpgl-coordinates transform x y)
	    (moveto printer-stream x y))
	  (do ((i 2 (+ i 2))
	       (length (length position-seq)))
	      ((= i length))
	    (let ((x (aref position-seq i))
		  (y (aref position-seq (1+ i))))
	      (convert-to-hpgl-coordinates transform x y)
	      (lineto printer-stream x y)))))
      (when closed
	(let ((x (elt position-seq 0))
	      (y (elt position-seq 1)))
	  (convert-to-hpgl-coordinates transform x y)
	  (lineto printer-stream x y))))))

(defun nyi ()
  (error "This HPGL CLIM operation is NYI (Not Yet Implemented)."))

(defmethod medium-draw-ellipse* ((medium hpgl-medium)
				 center-x center-y 
				 radius-1-dx radius-1-dy radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  (let* ((transform (sheet-device-transformation (medium-sheet medium)))
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium)))
    (convert-to-hpgl-coordinates transform center-x center-y)
    (convert-to-hpgl-distances transform 
			       radius-1-dx radius-1-dy radius-2-dx radius-2-dy)
    (when (null start-angle)
      (setq start-angle 0
	    end-angle 2pi))
    (when (< end-angle start-angle)
      (setq end-angle (+ end-angle 2pi)))
    (multiple-value-bind (x-radius y-radius)
	(cond ((and (= radius-1-dx 0) (= radius-2-dy 0))
	       (values (abs radius-2-dx) (abs radius-1-dy)))
	      ((and (= radius-2-dx 0) (= radius-1-dy 0))
	       (values (abs radius-1-dx) (abs radius-2-dy)))
	      (t (nyi)))
      ;;--- We are cheating here because we dont know how to draw an ellipse.
      ;;--- Perhaps we can try the 0.9 method of establishing a
      ;;--- transformation but how does that work.
      (let ((radius (sqrt (+ (* x-radius x-radius) (* y-radius y-radius)))))
	(with-hpgl-drawing-options (medium printer-stream
					   :filled filled
					   :ink ink :line-style line-style)
	  (if filled
	      (progn
		(moveto printer-stream center-x center-y)
		(fill-arc printer-stream
			  radius
			  (- (radians->degrees start-angle))
			  (- (radians->degrees start-angle)
			     (radians->degrees end-angle))))
	    (progn
	      (moveto printer-stream center-x center-y)
	      (moveto printer-stream (+ center-x radius) center-y)
	      (penup printer-stream)
	      (arc printer-stream center-x center-y (radians->degrees start-angle))
	      (pendown printer-stream)
	      (arc printer-stream center-x center-y (radians->degrees end-angle)))))))))


;;; Text stuff

(defmacro with-hpgl-port-glyph-for-character ((port) &body body)
  `(excl:without-package-locks
    (macrolet ((port-glyph-for-character (port character style &optional our-font)
		 `(multiple-value-bind (character-set index)
		      (char-character-set-and-index ,character)
		    (let* (;; For now we are asserting that each string
			   ;; passed to WRITE-STRING will have no style
			   ;; changes within it.  This is what our-font
			   ;; is all about.
			   (pfd (or ,our-font
				    (text-style-mapping ,',port ,style character-set)))
			   (pface (pfd-face pfd))
			   (point-size (pfd-point-size pfd))
			   (height (* point-size (pface-height pface)))
			   (ascent (* (pface-ascent pface) height))
			   (cwt (pface-width-table pface))
			   (relwidth (if (numberp cwt) 
					 cwt
				       (aref cwt index)))
			   (escapement-x (* height relwidth))
			   (escapement-y 0)
			   (origin-x 0)
			   (origin-y ascent)
			   (bb-x ;; really ought know real dope, but not avl yet.
			    escapement-x)
			   (bb-y height))
		      (values index pfd escapement-x escapement-y origin-x origin-y
			      bb-x bb-y (numberp cwt))))))
      ,@body)))

(defmethod port-glyph-for-character ((port hpgl-port)
				     character appearance &optional our-font)
  #+Genera
  (declare (values index font escapement-x escapement-y origin-x origin-y bb-x bb-y))
  (with-hpgl-port-glyph-for-character (port)
    (port-glyph-for-character medium character appearance our-font)))

;;; --- All the other methods on this specialize the stream as being for
;;; OUTPUT-PROTOCOL.  The reason we don't do this here is because:
;;; a) We don't actually use the stream argument for anything, and
;;; b) One of the important callers of this, STRING-WIDTH for Hpgl
;;;    media, doesn't know what the stream should be.
;;; This generic function (and STREAM-GLYPH-FOR-CHAR) should probably be
;;; booted down into Silica and made into honest-to-goodness methods on
;;; media instead of on streams.  That would remove these mixed-level
;;; methods from the code; we could get rid of all the CLIM-xxx-STUFF
;;; files that way.

#+ignore ;; stream-scan-string-for-writing-1 is undefined!
(defmethod stream-scan-string-for-writing  (stream
					    (medium hpgl-medium)
					    string start end
					    style cursor-x max-x
					    &optional glyph-buffer)
  #+Genera (declare (values write-char next-char-index new-cursor-x
			    new-baseline new-height font))
  ;;(declare (ignore stream))
  (let (#+ignore (port (port medium)))
    (with-hpgl-port-glyph-for-character (port)
      (stream-scan-string-for-writing-1
       stream medium string start end style cursor-x max-x glyph-buffer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  METHODS DEALING WITH SILLICA  TEXT STYLE FUNCTIONALITY
;;;


;;; More breakage of proper protocol.  I'm not sure what this is for.

;;; Who calls:          initialize-graphics-state       medium
;;;                     text-style-ascent               types
;;;                     text-style-decent               types
;;;                     text-style-height               types

(defmethod realize-text-style ((port hpgl-port) text-style)
  (let ((pfd (text-style-mapping port text-style *standard-character-set*)))
    pfd))

;;; Called by:           draw-text            clg
;;;                      draw-rotated-text    clg
         
(defmethod text-style-ascent ((text-style text-style) (medium hpgl-medium))
  (let* ((port (port medium))
	 (pfd (realize-text-style port text-style))
	 (pface (pfd-face pfd))
	 (point-size (pfd-point-size pfd))
	 (height (* point-size (pface-height pface)))
	 (ascent (* (pface-ascent pface) height)))
    ascent))

;;; Called by:           draw-text            clg
;;;                      draw-rotated-text    clg

(defmethod text-style-descent ((text-style text-style) (medium hpgl-medium))
  (let* ((port (port medium))
	 (pfd (realize-text-style port text-style))
	 (pface (pfd-face pfd))
	 (point-size (pfd-point-size pfd))
	 (height (* point-size (pface-height pface)))
	 (descent (* (- 1.0 (pface-ascent pface)) height)))
    descent))

(defmethod text-style-height ((text-style text-style) (medium hpgl-medium))
  (let* ((port (port medium))
	 (pfd (realize-text-style port text-style))
	 (pface (pfd-face pfd))
	 (point-size (pfd-point-size pfd))
	 (height (* point-size (pface-height pface))))
    height))




