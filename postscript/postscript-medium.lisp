;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: POSTSCRIPT-CLIM; Base: 10; Lowercase: Yes -*-


(in-package :postscript-clim)

(defmethod medium-ink :around ((medium postscript-medium))
  "Typecheck medium ink for compatibility and implementedness."
  (let ((ink (call-next-method)))
    (typecase ink
      (rectangular-tile
       (if (decode-tile-as-stipple ink)
           ink
           (restart-case
               (error 'silica::unsupported-ink :ink ink
                      :message "Rectangular tiles other than stipples ~
                                are not supported yet")
             (use-value (other-ink)
               :report "Use a different ink"
               other-ink))))
      (t ink))))

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

(defun use-line-style (medium line-style)
  (let* ((printer-stream (slot-value medium 'printer-stream))
	 (thickness (ecase (line-style-unit line-style)
		      (:normal (normal-line-thickness
				(port medium) (line-style-thickness line-style)))
		      (:point (line-style-thickness line-style))))
	 (dashes (line-style-dashes line-style))
	 (new-line-style (cons thickness dashes)))
    (with-slots (current-line-style) medium
      (unless (equalp current-line-style new-line-style)
	(setq current-line-style new-line-style)
	(format printer-stream " ~D setlinewidth~%"
		
		;;spr27839 bug13511 - allow any line thickness
		(the fixnum (round thickness))
		#+ignore
		(if (< thickness 2)
		    0
		  (the fixnum (round thickness)))
		
		)
	(if dashes
	    (progn
	      (when (eq dashes t)
		(setq dashes '(4 4)))
	      (making-ps-array (printer-stream)
			       (let ((limit (length dashes)))
				 (dotimes (i limit)
				   (write (elt dashes i) :stream printer-stream :escape nil)
				   (unless (>= (1+ i) limit)
				     (write-char #\space printer-stream)))))
	      (format printer-stream "0 setdash~%"))
	  (format printer-stream "[] 0 setdash~%"))))))

(defun use-clip-region (medium clip-region)
  (let ((printer-stream (slot-value medium 'printer-stream))
	(transform (sheet-device-transformation (medium-sheet medium))))
    (with-slots (current-clip-region) medium
      (unless (eq current-clip-region clip-region)
	(setq current-clip-region clip-region)
	(if (eq clip-region +everywhere+)
	    (format printer-stream " initclip ")
	  (with-bounding-rectangle* (left top right bottom)
	      clip-region
	    (convert-to-postscript-coordinates transform
	       left top right bottom)
	    (format printer-stream " newpath ")
	    (ps-pos-op medium "m" left top)
	    (ps-pos-op medium "lineto" right top)
	    (ps-pos-op medium "lineto" right bottom)
	    (ps-pos-op medium "lineto" left bottom)
	    (format printer-stream " closepath clip "))))))) ;no cr

(defun color-equal (c1 c2)
  (or (eq c1 c2)
      (and (colorp c1)
	   (colorp c2)
	   (multiple-value-bind (r1 g1 b1) (color-rgb c1)
	     (multiple-value-bind (r2 g2 b2) (color-rgb c2)
	       (and (= r1 r2) (= g1 g2) (= b1 b2)))))))

(defmethod maybe-set-color
	   ((medium postscript-medium) (ink (eql +foreground-ink+)))
  (maybe-set-color medium (or (medium-foreground medium) +black+)))

(defmethod maybe-set-color
	   ((medium postscript-medium) (ink (eql +background-ink+)))
  (maybe-set-color medium (or (medium-background medium) +white+)))

(defmethod maybe-set-color
	   ((medium postscript-medium) (ink flipping-ink))
  (error "Postscript devices can't draw with flipping inks."))

(defmethod maybe-set-color ((medium postscript-medium) (ink color))
  (with-slots (current-color printer-stream) medium
    (when (or (null current-color)
	      (not (color-equal current-color ink)))
      (setf current-color ink)
      (multiple-value-bind (r g b) (color-rgb ink)
	(format printer-stream  " ~,2F ~,2F ~,2F setrgbcolor~%" r g b)))))

(defmethod maybe-set-color ((medium postscript-medium) (ink rectangular-tile))
  ;; Handled in the patterned fill/stroke case
  )

(defmethod maybe-set-color ((medium postscript-medium) (ink pattern))
  ;; Handled in the patterned fill/stroke case
  )

(defmethod maybe-set-color
	   ((medium postscript-medium) (ink contrasting-ink))
  (maybe-set-color medium (make-color-for-contrasting-ink ink)))

;; a temporary hack

(defmethod maybe-set-color
    ((medium postscript-medium) (ink composite-in))
  (maybe-set-color medium
		   (elt (slot-value ink 'clim-utils::designs) 0)))

(defmacro with-postscript-drawing-options ((medium printer-stream-var
					    &key ink (filled nil
						      filled-p) line-style
						 clip-region
						 epilogue (newpath t))
					   &body body)
  (let ((printer-stream (or printer-stream-var (make-symbol (symbol-name 'printer-stream)))))
    `(let ((,printer-stream (slot-value ,medium 'printer-stream)))
       (when (and ,@(and filled-p `((not ,filled))) ,line-style)
	 (use-line-style ,medium ,line-style))
       (maybe-set-color ,medium ,ink)
       (use-clip-region ,medium ,clip-region)
       ,@(when newpath `((format ,printer-stream " newpath~%")))
       ,@body
       ,@(case epilogue
	   (:default `((if ,(if filled-p `,filled `(not ,line-style))
			   (ps-fill ,medium ,printer-stream ,ink)
			   (ps-stroke ,medium ,printer-stream ,ink))))
	   (:fill `((ps-fill ,medium ,printer-stream ,ink)))
	   (:stroke `((ps-stroke ,medium ,printer-stream ,ink)))))))

(defmethod medium-draw-point* ((medium postscript-medium) x y)
  (let* ((transform (sheet-device-transformation (medium-sheet medium)))
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium))
	 (clip-region (medium-clipping-region medium)))
    (convert-to-postscript-coordinates transform x y)
    (with-postscript-drawing-options (medium printer-stream
				      :epilogue :stroke
				      :ink ink :line-style line-style
				      :clip-region clip-region)
      (ps-pos-op medium "m" x y)
      (ps-rel-pos-op medium "rlineto" 0 0))
    (annotating-postscript (medium printer-stream)
      (format printer-stream "        (medium-draw-point* ~D ~D ...)"
	x y))))

(defmethod medium-draw-points* ((medium postscript-medium) position-seq)
  (let* ((transform (sheet-device-transformation (medium-sheet medium)))
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium))
	 (clip-region (medium-clipping-region medium)))
    (with-postscript-drawing-options (medium printer-stream
				      :epilogue :stroke
				      :ink ink :line-style line-style
				      :clip-region clip-region)
      (map-position-sequence
	#'(lambda (x y)
	    (convert-to-postscript-coordinates transform x y)
	    (ps-pos-op medium "m" x y)
	    (ps-rel-pos-op medium "rlineto" 0 0))
	position-seq))))

(defmethod medium-draw-line* ((medium postscript-medium) x1 y1 x2 y2)
  (let* ((transform (sheet-device-transformation (medium-sheet medium)))
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium))
	 (clip-region (medium-clipping-region medium)))
    (convert-to-postscript-coordinates transform x1 y1 x2 y2)
    (with-postscript-drawing-options (medium printer-stream
				      :epilogue :stroke
				      :ink ink :line-style line-style
				      :clip-region clip-region)
      (ps-pos-op medium "m" x1 y1)
      (ps-pos-op medium "lineto" x2 y2))
    (annotating-postscript (medium printer-stream)
      (format printer-stream "        (medium-draw-line* ~D ~D ~D ~D ...)"
	x1 y1 x2 y2))))

(defmethod medium-draw-lines* ((medium postscript-medium) position-seq)
  (let* ((transform (sheet-device-transformation (medium-sheet medium)))
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium))
	 (clip-region (medium-clipping-region medium)))
    (with-postscript-drawing-options (medium printer-stream
				      :epilogue :stroke
				      :ink ink :line-style line-style
				      :clip-region clip-region)
      (map-endpoint-sequence
	#'(lambda (x1 y1 x2 y2)
	    (convert-to-postscript-coordinates transform x1 y1 x2 y2)
	    (ps-pos-op medium "m" x1 y1)
	    (ps-pos-op medium "lineto" x2 y2))
	position-seq))))

(defmethod medium-draw-rectangle* ((medium postscript-medium)
				   left top right bottom filled)
  (let* ((transform (sheet-device-transformation (medium-sheet medium)))
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium))
	 (clip-region (medium-clipping-region medium)))
    (convert-to-postscript-coordinates transform
      left top right bottom)
    (with-postscript-drawing-options (medium printer-stream
				      :epilogue :default
				      :ink ink :filled filled
				      :line-style line-style
				      :clip-region clip-region)
      (ps-pos-op medium "m" left top)
      (ps-pos-op medium "lineto" right top)
      (ps-pos-op medium "lineto" right bottom)
      (ps-pos-op medium "lineto" left bottom)
      (format printer-stream " closepath "))	;no cr
    (annotating-postscript (medium printer-stream)
      (format printer-stream "        (medium-draw-rectangle* ~D ~D ~D ~D ...)"
	left top right bottom))))

(defmethod medium-draw-rectangles* ((medium postscript-medium) position-seq filled)
  (let* ((transform (sheet-device-transformation (medium-sheet medium)))
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium))
	 (clip-region (medium-clipping-region medium)))
    (with-postscript-drawing-options (medium printer-stream
				      :epilogue :default
				      :ink ink :filled filled
				      :line-style line-style
				      :clip-region clip-region)
      (map-endpoint-sequence
	#'(lambda (left top right bottom)
	    (convert-to-postscript-coordinates transform
	      left top right bottom)
	    (ps-pos-op medium "m" left top)
	    (ps-pos-op medium "lineto" right top)
	    (ps-pos-op medium "lineto" right bottom)
	    (ps-pos-op medium "lineto" left bottom)
	    (format printer-stream " closepath "))
	position-seq))))

(defmethod medium-draw-polygon* ((medium postscript-medium) position-seq closed filled)
  (let* ((transform (sheet-device-transformation (medium-sheet medium)))
	 (ink (medium-ink medium))
	 (line-style (medium-line-style medium))
	 (clip-region (medium-clipping-region medium))
	 (minx most-positive-fixnum)
	 (miny most-positive-fixnum)
	 (length (length position-seq)))
    (with-stack-array (points length :initial-contents position-seq)
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
					:ink ink :filled filled
					:line-style line-style
					:clip-region clip-region)
	(let ((start-x (svref points 0))
	      (start-y (svref points 1)))
	  (ps-pos-op medium "m" start-x start-y)
	  (do ((i 2 (+ i 2)))
	       ((>= i length)
		(when closed
		  (format printer-stream " closepath ")))
	    (let ((ex (svref points i))
		  (ey (svref points (1+ i))))
	      (ps-pos-op medium "lineto" ex ey)))))
      (annotating-postscript (medium printer-stream)
	(format printer-stream "        (medium-draw-polygon* ~A ...)"
		position-seq)))))

(defun nyi ()
  (error "This postscript operation is NYI (Not Yet Implemented)."))

(defmethod medium-draw-ellipse* ((medium postscript-medium)
                                 center-x center-y
                                 radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                                 start-angle end-angle filled)
  (maybe-send-feature medium 'ellipse *ps-ellipse-code*)
  (let* ((transform (sheet-device-transformation (medium-sheet medium)))
         (ink (medium-ink medium))
         (line-style (medium-line-style medium))
         (clip-region (medium-clipping-region medium)))
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
                                          :ink ink :filled filled
                                          :line-style line-style
                                          :clip-region clip-region
                                          :newpath filled)
          (when filled
            (ps-pos-op medium "moveto" center-x center-y))
          (ps-pos-op medium "ellipse" center-x center-y
                     x-radius y-radius
                     ;; Don't allow the common full-circle case to be
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
  (declare (ignore transform-glyphs))
  (unless start
    (setq start 0))
  (unless end
    (setq end (length string)))
  (let* ((transform (sheet-device-transformation (medium-sheet medium)))
	 (ink (medium-ink medium))
	 (text-style (medium-merged-text-style medium))
	 (clip-region (medium-clipping-region medium)))
    (convert-to-postscript-coordinates transform x y)
    (when towards-x
      (convert-to-postscript-coordinates transform towards-x towards-y))
    (let* ((fcs (get-font-compat-str (port medium) medium text-style))
	   (height (psfck-clim-height fcs))
	   (descent (psfck-clim-descent fcs))
	   (ascent (- height descent)))
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
      (set-font-if-needed medium fcs)

      (ps-pos-op medium "m" x y)
      (with-postscript-drawing-options (medium printer-stream
					       :epilogue nil :newpath nil
					       :ink ink
					       :clip-region clip-region)
	(if towards-x
	    (with-postscript-gsave medium
	      (format printer-stream " currentpoint translate ~D rotate "
		      (- 360 (truncate (* (atan (- towards-y y) (- towards-x x)) (/ 360 (* pi 2))))))
	      (carefully-output-ps-showstring printer-stream string start end))
	  (carefully-output-ps-showstring printer-stream string start end)))
      (annotating-postscript (medium printer-stream)
	(format printer-stream "      (medium-draw-string* ~S ~D ~D ~D ~D ~S ~S ...)"
	  string x y start end align-x align-y)))))

(defmethod medium-draw-text* ((medium postscript-medium)
			      string-or-char x y start end
			      align-x align-y
			      towards-x towards-y transform-glyphs)
  (with-slots (ch1buf) medium
    (when (characterp string-or-char)
      (setf (aref ch1buf 0) string-or-char
	    string-or-char ch1buf
	    start 0
	    end 1))
    (medium-draw-string* medium string-or-char x y start end
			 align-x align-y towards-x towards-y transform-glyphs)))

(defmethod medium-text-bounding-box ((medium postscript-medium)
				     string x y start end align-x
				     align-y text-style
				     towards-x towards-y
				     transform-glyphs transformation)
  (declare (ignore string start end transform-glyphs transformation
		   towards-x towards-y text-style
		   x y align-x align-y))
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

#+++ignore
(defmethod draw-vertical-string-internal ((medium postscript-medium)
					  x-offset y-offset
					  string x y start end
					  align-x align-y text-style ink)
  (unless end
    (setq end (length string)))
  (unless text-style
    (setq text-style (medium-merged-text-style medium)))
  (translate-positions x-offset y-offset x y)
  (let* ((fcs (get-font-compat-str (port medium) medium text-style))
	 (height (psfck-clim-height fcs))
	 (descent (psfck-clim-descent fcs))
	 (ascent (- height descent)))
    (let ((x-adjust
	    (compute-text-x-adjustment align-x medium string text-style start end))
	  (y-adjust
	    (compute-text-y-adjustment align-y descent ascent height)))
      (set-font-if-needed medium fcs)
      (with-postscript-drawing-options (medium printer-stream
					:epilogue nil :newpath nil
					:ink ink)
	(with-postscript-gsave medium
	  (ps-pos-op medium "m" x y)
	  (format printer-stream " currentpoint translate 90 rotate ")
	  (ps-rel-pos-op medium "rmoveto" x-adjust y-adjust)
	  (carefully-output-ps-showstring printer-stream string start end))))))

;;; provide a way for the "user" to start a new page.
;;; Should this have a different name?
;;; Should this functionality be invoked by writing the #\page character?
;;;
;;; pnc Dec99: Completely rewritten.  See comments with
;;; draw-postscript-literal, in postscript-port.lisp
(defmethod new-page ((stream postscript-stream))
  
  ;; Throw a newline in the output-record, and 
  ;; insert a call to DRAW-POSTSCRIPT-LITERAL
  ;; at the current location.
  (terpri stream)
  (clim-internals::with-cursor-state (stream nil)
    (multiple-value-bind (cursor-x cursor-y) ;; baseline height style max-x record-p draw-p
        (clim-internals::decode-stream-for-writing stream)      
      (draw-postscript-literal stream "new-page" cursor-x cursor-y)
      )
    )
  
  ;; This will cause the contents current output-record
  ;; to be processed.
  (letf-globally (((stream-generating-postscript stream) t))
    (apply 'invoke-with-output-to-postscript-stream-helper +output-to-postscript-stream-helper-args+)
    )
  
  ;; Now, clean out the output-record.
  (when (stream-output-history stream)
    (clear-output-record (stream-output-history stream)))
  (setf (stream-text-output-record stream) nil)
  (when (extended-output-stream-p stream) ;can we assume this?
    (stream-set-cursor-position stream 0 0)
    (setf (stream-baseline stream) 0
	  (clim-internals::stream-current-line-height stream) 0))

  )

#+OBSOLETE
(defmethod new-page ((medium postscript-medium))
  (error "Obsolete"))

(excl::without-package-locks
;;; XXX: what the dis? Is this intentionally obfuscated?
(defmacro with-postscript-glyph-for-character (&body body)
  `(macrolet ((port-glyph-for-character (port character style &optional our-font)
		 `(multiple-value-bind (character-set index)
		      (char-character-set-and-index ,character)
		    (declare (ignore character-set))
		    ;; For now we are asserting that each string passed to WRITE-STRING will
		    ;; have no style changes within it.  This is what our-font is all
		    ;; about.
		    (let* ((fcs (or ,our-font (get-font-compat-str ,port nil ,style)))
			   (cwt (psfck-width-table fcs))
			   (relwidth (if (numberp cwt)
					 cwt
				       (aref cwt index)))
			   (escapement-x (* (psfck-clim-height fcs) relwidth))
			   (escapement-y 0)
			   (origin-x 0)
			   (origin-y (psfck-clim-ascent fcs))
			   ;; really ought know real dope, but not avl yet
			   (bb-x escapement-x)
			   (bb-y (psfck-clim-height fcs)))
		      (values index fcs escapement-x escapement-y origin-x origin-y
			      bb-x bb-y (numberp cwt))))))
      ,@body))

(defmethod port-glyph-for-character ((port postscript-port)
				     character style &optional our-font)
  (declare (values index font escapement-x escapement-y origin-x origin-y bb-x bb-y
		   fixed-width-font-p))
  (with-postscript-glyph-for-character
    (port-glyph-for-character port character style our-font)))
)

;;-- Is this needed??

#+ignore
(defmethod stream-scan-string-for-writing
	   ((stream clim-internals::output-protocol-mixin) (medium postscript-medium)
	    string start end style cursor-x max-x &optional glyph-buffer)
  (with-postscript-glyph-for-character
    (stream-scan-string-for-writing-1
      stream medium string start end style cursor-x max-x glyph-buffer)))

(defmethod set-font-if-needed ((medium postscript-medium) fcs)
  (with-slots (printer-stream curfont) medium
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

(defmethod get-font-compat-str ((port postscript-port) medium text-style)
  (with-slots (font-map) port
    (let* ((styledesc (parse-text-style text-style))
	   (al (length font-map))
	   (fcs
	     (do ((i 0 (1+ i)))
		 (nil)
	       (when (= i al)
		 (setq font-map (adjust-array font-map (setq al (+ al al)) :initial-element nil)))
	       (let ((fcs (aref font-map i)))
		 (when (null fcs)
		   (setq fcs (make-new-fcs styledesc i))
		   (setf (aref font-map i) fcs)
		   (return fcs))
		 (when (eq styledesc (psfck-style-descriptor fcs))
		   (return fcs))))))
      (when (and fcs
		 medium
		 (let ((stream (medium-sheet medium)))
		   (or (not (output-recording-stream-p stream))
		       (stream-drawing-p stream)))
		 (not (psfck-established fcs)))
	  (with-slots (printer-stream) medium
	    (format printer-stream "~D ~D /~A estfont~%"
		    (psfck-index fcs) (psfck-points fcs) (get-ps-fam-face-name fcs))
	    (setf (psfck-established fcs) t)))
      fcs)))


;; PostScript's "default user space" is measured in printers' points.
;; Should these force the stream to map the font and then get the
;; information from the FCS?

(defmethod text-style-width
    ((text-style standard-text-style) (medium postscript-medium))
  ;; An 'M' is often square and of height approximating the point size of the font.
  ;;--- This should consult the real metrics.
  (nth-value 6 (port-glyph-for-character (port medium) #\M text-style)))

(defmethod text-style-height
	   ((text-style standard-text-style) (medium postscript-medium))
  (nth-value 7 (port-glyph-for-character (port medium) #\M text-style)))

(defmethod text-style-ascent
	   ((text-style standard-text-style) (medium postscript-medium))
  (nth-value 5 (port-glyph-for-character (port medium) #\M text-style)))

(defmethod text-style-descent ((text-style standard-text-style)
			       (medium postscript-medium))
  (- (text-style-height text-style medium)
     (text-style-ascent text-style medium)))

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
	   ((text-style standard-text-style) (medium postscript-medium))
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
