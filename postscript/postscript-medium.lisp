;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: POSTSCRIPT-CLIM; Base: 10; Lowercase: Yes -*-

;; $fiHeader: postscript-medium.lisp,v 1.1 92/02/24 13:08:01 cer Exp $

(in-package :postscript-clim)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

(defmacro convert-to-postscript-coordinates (transform &body positions)
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

(defmacro convert-to-postscript-distances (transform &body positions)
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

(defun use-line-style (stream line-style)
  (let ((thickness (case (line-style-unit line-style)
		     (:normal (normal-line-thickness (stream-display-device-type stream)
						     (line-style-thickness line-style)))
		     (:points (line-style-thickness line-style))))
	(dashes (line-style-dashes line-style)))
    (with-slots (printer-stream) stream
      (format printer-stream " ~D setlinewidth~%" thickness)
      (when dashes
	(making-ps-array (printer-stream)
	  (let ((limit (length dashes)))
	    (dotimes (i limit)
	      (write (elt dashes i) :stream printer-stream :escape nil)
	      (unless (>= (1+ i) limit)
		(write-char #\space printer-stream)))))
	(format printer-stream "0 setdash~%")))))

(defun color-equal (c1 c2)
  (or (eq c1 c2)
      (and (colorp c1)
	   (colorp c2)
	   (multiple-value-bind (r1 g1 b1) (color-rgb c1)
	     (multiple-value-bind (r2 g2 b2) (color-rgb c2)
	       (and (= r1 r2) (= g1 g2) (= b1 b2)))))))

(defmethod maybe-set-color
	   ((stream postscript-medium) (ink (eql +foreground-ink+)))
  (maybe-set-color stream (or (slot-value stream 'current-color) +black+)))

(defmethod maybe-set-color
	   ((stream postscript-medium) (ink (eql +background-ink+)))
  (maybe-set-color stream +white+))

(defmethod maybe-set-color
	   ((stream postscript-medium) (ink flipping-ink))
  (error "Postscript devices can't draw with flipping inks."))

(defmethod maybe-set-color ((stream postscript-medium) (ink color))
  (with-slots (current-color printer-stream) stream
    (when (or (null current-color)
	      (not (color-equal current-color ink)))
      (setf current-color ink)
      (multiple-value-bind (r g b) (color-rgb ink)
	(format printer-stream  " ~,2F ~,2F ~,2F setrgbcolor~%" r g b)))))

(defmethod maybe-set-color ((stream postscript-medium) (ink rectangular-tile))
  ;; Handled in the patterned fill/stroke case
  )

(defmethod maybe-set-color
	   ((stream postscript-medium) (ink contrasting-ink))
  (maybe-set-color stream (make-color-for-contrasting-ink ink)))

(defmacro with-postscript-drawing-options ((stream printer-stream-var
					    &key ink line-style epilogue (newpath T))
					   &body body)
  (let ((printer-stream (or printer-stream-var (make-symbol "printer-stream"))))
    `(let ((,printer-stream (slot-value ,stream 'printer-stream)))
       (when ,line-style
	 (use-line-style ,stream ,line-style))
       (maybe-set-color ,stream ,ink)
       ,@(when newpath `((format ,printer-stream " newpath~%")))
       ,@body
       ,@(case epilogue
	   (:default `((if (null ,line-style)
			   (ps-fill ,stream ,printer-stream ,ink)
			   (ps-stroke ,stream ,printer-stream ,ink))))
	   (:fill `((ps-fill ,stream ,printer-stream ,ink)))
	   (:stroke `((ps-stroke ,stream ,printer-stream ,ink)))))))

(defmethod medium-draw-point* ((medium postscript-medium) x y)
  (let* ((transform +identity-transformation+)
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium)))
    (with-postscript-drawing-options (medium printer-stream
				      :epilogue :stroke
				      :ink ink :line-style line-style)
      (ps-pos-op medium "m" x y)
      (ps-rel-pos-op medium "rlineto" 0 0))
    (annotating-postscript (medium printer-stream)
      (format printer-stream "        (medium-draw-point* ~D ~D ...)"
	x y))))

(defmethod medium-draw-line* ((medium postscript-medium) x1 y1 x2 y2)
  (let* ((transform +identity-transformation+)
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium)))
    (convert-to-postscript-coordinates transform
      x1 y1 x2 y2)
    (with-postscript-drawing-options (medium printer-stream
				      :epilogue :stroke
				      :ink ink :line-style line-style)
      (ps-pos-op medium "m" x1 y1)
      (ps-pos-op medium "lineto" x2 y2))
    (annotating-postscript (medium printer-stream)
      (format printer-stream "        (medium-draw-line* ~D ~D ~D ~D ...)"
	x1 y1 x2 y2))))

(defmethod medium-draw-rectangle* ((medium postscript-medium)
				   left top right bottom filled)
  (let* ((transform +identity-transformation+)
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium)))
    (convert-to-postscript-coordinates transform
      left top right bottom)
    (with-postscript-drawing-options (medium printer-stream
				      :epilogue :default
				      :ink ink :line-style line-style)
      (ps-pos-op medium "m" left top)
      (ps-pos-op medium "lineto" right top)
      (ps-pos-op medium "lineto" right bottom)
      (ps-pos-op medium "lineto" left bottom)
      (format printer-stream " closepath "))	;no cr
    (annotating-postscript (medium printer-stream)
      (format printer-stream "        (medium-draw-rectangle* ~D ~D ~D ~D ...)"
	left top right bottom))))

(defmethod medium-draw-polygon* ((medium postscript-medium) position-seq closed filled)
  (let* ((transform +identity-transformation+)
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium))
	 (minx most-positive-fixnum)
	 (miny most-positive-fixnum)
	 (length (length position-seq))
	 (points (make-array length :initial-contents position-seq)))
    (declare (type simple-vector points))
    (do ((i 0 (+ i 2)))
	((>= i length))
      (let ((x (svref points i))
	    (y (svref points (1+ i))))
	(convert-to-postscript-coordinates transform x y)
	(setf (svref points i) x)
	(setf (svref points (1+ i)) y)
	(if (< x minx) (setq minx x))
	(if (< y miny) (setq miny y))))
    (with-postscript-drawing-options (medium printer-stream
				      :epilogue :default
				      :ink ink :line-style line-style)
      (let ((start-x (svref points 0))
	    (start-y (svref points 1)))
	(ps-pos-op medium "m" start-x start-y)
	(do* ((i 2 (+ i 2))
	      (ex (svref points i) (svref points i))
	      (ey (svref points (1+ i)) (svref points (1+ i))))
	     ((>= i length)
	      (when closed
		(format printer-stream " closepath ")))
	  (ps-pos-op medium "lineto" ex ey))))
    (annotating-postscript (medium printer-stream)
      (format printer-stream "        (medium-draw-polygon* ~A ...)"
        position-seq))))

(defmethod medium-draw-ellipse* ((medium postscript-medium)
				 center-x center-y 
				 radius-1-dx radius-1-dy radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  (maybe-send-feature medium 'ellipse *ps-ellipse-code*)
  (let* ((transform +identity-transformation+)
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium)))
    (convert-to-postscript-coordinates transform center-x center-y)
    (convert-to-postscript-distances transform 
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
      (pixels-to-points x-radius y-radius)
      (flet ((skew-angle (a)
	       (atan (* (sin a) x-radius)
		     (* (cos a) y-radius))))
	(with-postscript-drawing-options (medium printer-stream
					  :epilogue :default
					  :ink ink :line-style line-style)
	  (ps-pos-op medium "ellipse" center-x center-y
		     x-radius y-radius
		     ;; don't allow the common full-circle case to be
		     ;; screwed up by floating point error:
		     (if (zerop start-angle)
			 0
			 (radians->degrees (skew-angle start-angle)))
		     (if (= end-angle 2pi)
			 360
			 (radians->degrees (skew-angle end-angle)))))
	(annotating-postscript (medium printer-stream)
	  (format printer-stream
	      "        (medium-draw-ellipse* ~D ~D ~D ~D ~D ~D ~D ~D ...)"
	    center-x center-y
	    radius-1-dx radius-1-dy
	    radius-2-dx radius-2-dy
	    start-angle end-angle))))))

;; These 2 clones of draw-string would be much more modular if there
;; were a reasonable way of passing arguments transparently, so that
;; we might be able to share code.
(defmethod medium-draw-string* ((medium postscript-medium)
				string x y start end align-x align-y
				towards-x towards-y transform-glyphs)
  (unless start
    (setq start 0))
  (unless end
    (setq end (length string)))
  (let* ((transform +identity-transformation+)
	 (ink (medium-ink medium))
	 (text-style (medium-merged-text-style medium)))
    (convert-to-postscript-coordinates transform x y)
    (when towards-x
      (convert-to-postscript-coordinates transform towards-x towards-y))
    (let* ((fcs (get-font-compat-str medium text-style))
	   (height (psfck-clim-height fcs))
	   (descent (psfck-clim-descent fcs))
	   (ascent (- height descent)))
      (let ((x-adjust 
	      (compute-text-x-adjustment align-x medium character text-style))
	    (y-adjust 
	      (compute-text-y-adjustment align-y descent ascent height)))
	(incf x x-adjust)
	(incf y y-adjust)
	(when towards-x
	  (incf towards-x x-adjust)
	  (incf towards-y y-adjust)))
      ;; do raster/ink stuff.
      (set-font-if-needed medium fcs)
      (with-postscript-drawing-options (medium printer-stream
					:epilogue nil :newpath nil
					:ink ink)
	(ps-pos-op medium "m" x y)
	(carefully-output-ps-showstring printer-stream string start end))
      (annotating-postscript (medium printer-stream)
	(format printer-stream "      (medium-draw-string* ~S ~D ~D ~D ~D ~S ~S ...)"
	  string x y start end align-x align-y)))))

(defmethod medium-draw-character* ((medium postscript-medium)
				   character x y align-x align-y
				   towards-x towards-y transform-glyphs)
  (let* ((transform +identity-transformation+)
	 (ink (medium-ink medium))
	 (text-style (medium-merged-text-style medium)))
    (convert-to-postscript-coordinates transform x y)
    (when towards-x
      (convert-to-postscript-coordinates transform towards-x towards-y))
    (with-slots (printer-stream ch1buf) medium
      (setf (aref ch1buf 0) character)
      (let* ((fcs (get-font-compat-str medium text-style))
	     (height (psfck-clim-height fcs))
	     (descent (psfck-clim-descent fcs))
	     (ascent (- height descent)))
	(let ((x-adjust 
		(compute-text-x-adjustment align-x medium character text-style))
	      (y-adjust 
		(compute-text-y-adjustment align-y descent ascent height)))
	(incf x x-adjust)
	(incf y y-adjust)
	(when towards-x
	  (incf towards-x x-adjust)
	  (incf towards-y y-adjust)))
	;; do raster/ink stuff.
	(set-font-if-needed medium fcs)
	(with-postscript-drawing-options (medium printer-stream
					  :epilogue nil :newpath nil
					  :ink ink)
	  (ps-pos-op medium "m" x y)
	  (carefully-output-ps-showstring printer-stream ch1buf 0 1))
	(annotating-postscript (medium printer-stream)
	  (format printer-stream "      (medium-draw-character* ~C ~D ~D ~S ~S ...)"
	    character x y align-x align-y))))))

(defmethod medium-draw-text* ((medium postscript-medium)
			      string-or-char x y start end
			      align-x align-y
			      towards-x towards-y transform-glyphs)
  (if (characterp string-or-char)
      (medium-draw-character* medium string-or-char x y 
			      align-x align-y towards-x towards-y transform-glyphs)
      (medium-draw-string* medium string-or-char x y start end 
			   align-x align-y towards-x towards-y transform-glyphs)))

#+++ignore
(defmethod draw-vertical-string-internal ((stream postscript-medium)
					  x-offset y-offset
					  string x y start end
					  align-x align-y text-style ink)
  (unless end
    (setq end (length string)))
  (unless text-style
    (setq text-style (medium-merged-text-style stream)))
  (translate-positions x-offset y-offset x y)
  (let* ((fcs (get-font-compat-str stream text-style))
	 (height (psfck-clim-height fcs))
	 (descent (psfck-clim-descent fcs))
	 (ascent (- height descent)))
    (let ((x-adjust 
	    (compute-text-x-adjustment align-x stream string text-style start end))
	  (y-adjust 
	    (compute-text-y-adjustment align-y descent ascent height)))
      (set-font-if-needed stream fcs)
      (with-postscript-drawing-options (stream printer-stream
					:epilogue nil :newpath nil
					:ink ink)
	(with-postscript-gsave stream 
	  (ps-pos-op stream "m" x y)
	  (format printer-stream " currentpoint translate 90 rotate ")
	  (ps-rel-pos-op stream "rmoveto" x-adjust y-adjust)
	  (carefully-output-ps-showstring printer-stream string start end))))))

;;; provide a way for the "user" to start a new page.
;;; Should this have a different name?
;;; Should this functionality be invoked by wriuting the #\page character?
(defmethod new-page ((stream postscript-medium))
  (with-slots (printer-stream orientation) stream
    (format printer-stream "new-page~%"))
  ;; simulate WINDOW-CLEAR:
  (clear-output-history stream)
  (when (extended-output-stream-p stream)	;can we assume this?
    (stream-set-cursor-position* stream 0 0)
    (setf (stream-baseline stream) 0
	  (stream-current-line-height stream) 0)))

(defmacro with-ps-stream-glyph-for-character (&body body)
  `(macrolet ((stream-glyph-for-character (stream character style &optional our-font)
		`(with-slots (display-device-type) ,stream
		   (multiple-value-bind (character-set index)
		       (char-character-set-and-index ,character)
		     (declare (ignore character-set))
		     ;; For now we are asserting that each string passed to WRITE-STRING will
		     ;; have no style changes within it.  This is what our-font is all
		     ;; about.
		     (let* ((fcs (or our-font (get-font-compat-str stream ,style)))
			    (CWT (psfck-width-table fcs))
			    (relwidth (if (numberp CWT) 
					  CWT
					  (aref CWT index)))
			    (escapement-x (* (psfck-clim-height fcs) relwidth))
			    (escapement-y 0)
			    (origin-x 0)
			    (origin-y (psfck-clim-ascent fcs))
			    ;; really ought know real dope, but not avl yet
			    (bb-x escapement-x)
			    (bb-y (psfck-clim-height fcs)))
		       (values index fcs escapement-x escapement-y origin-x origin-y
			       bb-x bb-y (numberp cwt)))))))
     ,@body))

(defmethod stream-glyph-for-character ((stream postscript-medium)
				       character style &optional our-font)
  (declare (values index font escapement-x escapement-y origin-x origin-y bb-x bb-y
		   fixed-width-font-p))
  (with-ps-stream-glyph-for-character
    (stream-glyph-for-character stream character style our-font)))

(defmethod stream-scan-string-for-writing ((stream postscript-medium) medium
					    string start end style cursor-x max-x
					    &optional glyph-buffer)
  (declare (type coordinate cursor-x max-x))
  (declare (fixnum start end))
  (with-ps-stream-glyph-for-character 
    (stream-scan-string-for-writing-body)))

(defmethod set-font-if-needed ((stream postscript-medium) fcs)
  (with-slots (printer-stream curfont) stream
    (unless (eq curfont fcs)
      (format printer-stream "~D f " (psfck-index fcs))
      (setf curfont fcs))))

(defun carefully-output-ps-showstring (printer-stream data start end)
  (assert (<= end (length data)))
  (write-char #\( printer-stream)
  (loop
    (let ((next-special (position-if #'(lambda (char)
					 (or (eql char #\( )
					     (eql char #\) )
					     (eql char #\\ )))
				     data :start start :end end)))
      (write-string data printer-stream :start start :end (or next-special end))
      (when next-special
	(write-char #\\ printer-stream)
	(write-char (elt data next-special) printer-stream))
      (unless next-special (return))
      (setq start (1+ next-special))))
  (format printer-stream ") show~%"))

(defmethod get-font-compat-str ((stream postscript-medium) text-style)
  (with-slots (font-map printer-stream draw-p) stream 
    (let* ((styledesc (parse-text-style text-style))
	   (al (length font-map))
	   (fcs
	     (do ((i 0 (1+ i)))
		 ((>= i al)
		  (error "Font map overflow for ~S" stream))
	       (let ((fcs (aref font-map i)))
		 (when (null fcs)
		   (setq fcs (make-new-fcs styledesc i))
		   (setf (aref font-map i) fcs)
		   (return fcs))
		 (when (eq styledesc (psfck-style-descriptor fcs))
		   (return fcs))))))
      (when (and fcs 
		 draw-p
		 (not (psfck-established fcs)))
	(format printer-stream "~D ~D /~A estfont~%"
	  (psfck-index fcs) (psfck-points fcs) (get-ps-fam-face-name fcs))
	(setf (psfck-established fcs) t))
      fcs)))


;; PostScript's "default user space" is measured in printers' points.
;; Should these force the stream to map the font and then get the
;; information from the FCS?

(defmethod text-style-width
	   ((text-style standard-text-style) (medium postscript-medium))
  ;; An 'M' is often square and of height approximating the point size of the font.
  ;;--- This should consult the real metrics.
  (text-style-size-in-points text-style medium))

(defmethod text-style-height 
	   ((text-style standard-text-style) (medium postscript-medium))
  (text-style-size-in-points text-style medium))

(defmethod text-style-ascent
	   ((text-style standard-text-style) (medium postscript-medium))
  (* (text-style-height text-style medium) (- 1 *ps-magic-baseline-factor*)))

(defmethod text-style-descent ((text-style standard-text-style)
			       (medium postscript-medium))
  (* (text-style-height text-style medium) *ps-magic-baseline-factor*))

;;;--- This can probably go away when we have standardized text styles.
(defmethod text-style-size-in-points
	   ((text-style standard-text-style) (medium postscript-medium))
  (let* ((family (text-style-family text-style))
	 (size (text-style-size text-style))
	 (famdat (or (assoc family *postscript-font-translate-data*)
		     (error "Don't have PostScript support for family ~A." family)))
	 (points (if (numberp size)
		     size
		     (point-size-for-size-keyword size famdat))))
    points))

(defmethod text-style-fixed-width-p 
	   ((text-style standard-text-style) (stream postscript-medium))
  (let* ((family (text-style-family text-style))
	 (face (text-style-face text-style))
	 (psfam (or (second (assoc family *postscript-font-translate-data*))
		    (error "Don't have PostScript support for family ~A." family)))
	 (famdat (cdr (or (assoc psfam *ps-font-family-data* :test #'string-equal)
			  (error "No info for PostScript font family ~A?" psfam))))
	 (info (cdr (or (assoc face famdat)
			(error "No info for PostScript family ~A face ~A." psfam face))))
 	 (cname (first info)))
    (numberp cname)))
