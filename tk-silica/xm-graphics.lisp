(in-package :xm-silica)
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

(defmethod make-medium ((port motif-port) sheet)
  (make-instance 'motif-medium
		 :port port
		 :sheet sheet))

(defclass motif-medium (medium)
  ((foreground-gcontext :reader medium-foreground-gcontext :initform nil)
   (background-gcontext :reader medium-background-gcontext :initform nil)
   (flipping-gcontext :reader medium-flipping-gcontext :initform nil)
   (ink-table :initform (make-hash-table))))



(defmethod engraft-medium :after (medium (port motif-port) sheet)
  (with-slots 
      (foreground-gcontext background-gcontext flipping-gcontext) 
      medium
    (let ((drawable (tk::display-root-window (port-display port))))
      (unless foreground-gcontext
	(setf foreground-gcontext (tk::make-instance 'tk::gcontext :drawable drawable)))
      (unless background-gcontext
	(setf background-gcontext (tk::make-instance 'tk::gcontext
						     :drawable drawable)))
      (unless flipping-gcontext
	(setf flipping-gcontext
	  (tk::make-instance 'tk::gcontext 
			     :function boole-xor
			     :drawable drawable)))
      (recompute-gcs medium))))


(defun recompute-gcs (medium)
  (with-slots 
   (foreground-gcontext background-gcontext flipping-gcontext)
   medium
   (let ((foreground-pixel
	  (clx-decode-color medium (medium-foreground medium)))
	 (background-pixel
	  (clx-decode-color medium (medium-background medium))))
     (setf (tk::gcontext-foreground foreground-gcontext) foreground-pixel
	   (tk::gcontext-background foreground-gcontext) background-pixel
	   (tk::gcontext-foreground background-gcontext) background-pixel
	   (tk::gcontext-foreground flipping-gcontext)
	   (logxor foreground-pixel background-pixel)))))
      
      


(defmethod (setf medium-background) :after (ink (medium motif-medium))
  (recompute-gcs medium))

(defmethod (setf medium-foreground) :after (ink (medium motif-medium))
  (recompute-gcs medium))

(defmethod (setf medium-ink) :after (ink (medium motif-medium))
  (recompute-gcs medium))

;;;  Below is stuff from clx-implementation

(defgeneric clx-decode-ink (ink medium))

(defmethod clx-decode-ink ((ink (eql +foreground+)) medium)
  (slot-value medium 'foreground-gcontext))

(defmethod clx-decode-ink ((ink (eql +background+)) medium)
  (slot-value medium 'background-gcontext))

(defmethod clx-decode-ink ((ink (eql +flipping-ink+)) stream)
  (slot-value stream 'flipping-gcontext))

(defmethod clx-decode-ink ((ink color) medium)
  (let ((ink-table (slot-value medium 'ink-table)))
    (or (gethash ink ink-table)
	(setf (gethash ink ink-table)
	      (let ((new-gc 
		     (make-instance 'tk::gcontext
				    :drawable (tk::display-root-window 
					       (port-display (port medium))))))
		(setf (tk::gcontext-foreground new-gc)
		      (clx-decode-color medium ink))
		new-gc)))))

(defmethod clx-decode-ink ((ink contrasting-ink) stream)
  (clx-decode-ink (make-color-for-contrasting-ink ink) stream))

(defmethod clx-decode-color :around ((medium motif-medium) color)
  (or (gethash color (port-color-cache (port medium)))
      (setf (gethash color (port-color-cache (port medium)))
	(call-next-method))))

(defmethod clx-decode-color ((medium motif-medium) (ink color))
  (multiple-value-bind
      (red green blue)
      (color-rgb ink)
    (tk::allocate-color
     (tk::default-colormap (port-display (port medium)))
     (make-instance 'tk::color
		    :red (truncate (* 65356 red))
		    :green (truncate (* 65356 green))
		    :blue (truncate (* 65356 blue))))))


(defmethod clx-adjust-ink ((medium motif-medium) gc ink line-style)
  ;; This is used to adjust for the line-style
  (declare (ignore ink line-style))

  (let ((thickness (silica::line-style-thickness line-style)))
    (if (< thickness 2)
	(setq thickness 0))
    (setf (tk::gcontext-line-width gc) (round thickness)))
  
  (setf (tk::gcontext-cap-style gc)
    (ecase (silica::line-style-cap-shape line-style)
      (:butt :butt)
      (:square :projecting)
      (:round :round)
      (:no-end-point :not-last)))
  
  (setf (tk::gcontext-join-style gc)
    (ecase (silica::line-style-joint-shape line-style)
      ((:miter :none) :miter)
      (:bevel :bevel)
      (:round :round)))
  gc)

(defmethod clx-decode-ink :around ((ink t) (medium motif-medium))
  (let ((gc (call-next-method)))
    #+ignore
    (setf (tk::gcontext-clip-mask gc) 
	  (compute-gcontext-clip-mask medium))
    gc))

(defmethod compute-gcontext-clip-mask (medium)
  (with-bounding-rectangle* 
   (minx miny maxx maxy)
   (sheet-device-region (medium-sheet medium))
   (list (truncate minx)
	 (truncate miny)
	 (truncate maxx)
	 (truncate maxy))))
      
(defun devicize-point (transform x y)
  (multiple-value-setq (x y)
      (transform-point* transform x y))
  (values (truncate x) (truncate y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod port-draw-line* ((port motif-port)
			    sheet
			    medium
			    x1 y1 x2 y2)
  (let ((transform (compose-transformations
		    (medium-transformation medium)
		    (sheet-device-transformation sheet))))
    (multiple-value-setq (x1 y1) (devicize-point transform x1 y1))
    (multiple-value-setq (x2 y2) (devicize-point transform x2 y2)))
  (tk::draw-line
   (tk::widget-window (sheet-mirror sheet))
   (clx-adjust-ink medium
		   (clx-decode-ink (medium-ink medium) medium)
		   (medium-ink medium)
		   (medium-line-style medium))
   x1
   y1
   x2
   y2))


(defmethod port-draw-rectangle* ((port motif-port)
				 sheet
				 medium
				 x1 y1 x2 y2 filled)
  (let ((transform (compose-transformations
		    (medium-transformation medium)
		    (sheet-device-transformation sheet))))
    (if (rectilinear-transformation-p transform)
	(progn
	  (multiple-value-setq (x1 y1) (devicize-point transform x1 y1))
	  (multiple-value-setq (x2 y2) (devicize-point transform x2 y2))
	  (tk::draw-rectangle
	   (tk::widget-window (sheet-mirror sheet))
	   (clx-adjust-ink medium
		   (clx-decode-ink (medium-ink medium) medium)
		   (medium-ink medium)
		   (medium-line-style medium)) 
	   (min x1 x2)
	   (min y1 y2)
	   (abs (- x2 x1))
	   (abs (- y2 y1))
	   filled))
      (port-draw-transformed-rectangle*
       port sheet medium x1 y1 x2 y2 filled))))
      



(defmethod port-draw-text* ((port motif-port)
			    sheet medium string-or-char x y start end align-x align-y
			    towards-point towards-x towards-y
			    transform-glyphs)
  
  (let ((transform (compose-transformations
		    (medium-transformation medium)
		    (sheet-device-transformation sheet)))
	(font (realize-text-style port (medium-text-style medium))))
    
    (multiple-value-setq (x y) (devicize-point transform x y))
    (when (typep string-or-char 'character)
      (setq string-or-char (string string-or-char)))
    
    (ecase align-x
      (:center (decf x (floor (silica::text-size 
			       sheet
			       string-or-char
			       :text-style
			       (medium-text-style medium)
			       :start start
			       :end end)
			      2)))
      (:left nil))

    (ecase align-y
      (:center (decf y (- (text-style-descent  (medium-text-style medium) medium)
			  (floor (text-style-height  (medium-text-style medium) medium)
				 2))))
      (:base-line nil)
      (:top 
       (incf y (tk::font-ascent font))))

    (let ((gc (clx-decode-ink (medium-ink medium) medium)))
      (setf (tk::gcontext-font gc) font)
      (tk::draw-string
       (tk::widget-window (sheet-mirror sheet))
       gc
       x y
       string-or-char
       start end))))


(defmethod silica::port-write-string-internal ((port motif-port)
					       medium
					       glyph-buffer 
					       start 
					       end 
					       x-font 
					       color 
					       x
					       y)
  (unless (= start end)
    (let* ((sheet (medium-sheet medium))
	   (transform (compose-transformations
		       (medium-transformation medium)
		       (sheet-device-transformation sheet)))
	   (window (tk::widget-window (sheet-mirror sheet) nil))
	   (font x-font))
    
      ;; At one point we checked to see whether the widget is unrealized
      ;; or not.  Can we draw on disabled sheets?
    
      (multiple-value-setq (x y) (devicize-point transform x y))
    
      (incf y (tk::font-ascent font))

      (let ((gc (clx-decode-ink (medium-ink medium) medium)))
	(setf (tk::gcontext-font gc) font)
	(etypecase glyph-buffer
	  ((simple-array (unsigned-byte 16))
	   (x11::xdrawstring16
	    (tk::display-handle (tk::object-display window))
	    (tk::object-handle window)
	    (tk::object-handle gc)
	    x 
	    y
	    glyph-buffer
	    (- end start))))))))


(defmethod port-beep ((port motif-port) (sheet t))
  (x11:xbell (tk::display-handle (port-display port)) 100))

(defmethod silica::port-draw-circle* ((port motif-port)
				      sheet
				      medium
				      center-x
				      center-y
				      radius
				      filled)
  (let ((transform (compose-transformations
		    (medium-transformation medium)
		    (sheet-device-transformation sheet))))
    (multiple-value-setq (center-x center-y) 
      (devicize-point transform center-x center-y))
    (tk::draw-circle
     (tk::widget-window (sheet-mirror sheet))
     (clx-adjust-ink medium
		   (clx-decode-ink (medium-ink medium) medium)
		   (medium-ink medium)
		   (medium-line-style medium))
     center-x
     center-y
     radius
     filled)))
   
(ff::def-c-type (xpoint-array :in-foreign-space) 2 x11::xpoint)
  
(defmethod silica::port-draw-polygon* ((port motif-port)
				       sheet
				       medium
				       list-of-x-and-ys
				       closed
				       filled)
  (let* ((transform (compose-transformations
		     (medium-transformation medium)
		     (sheet-device-transformation sheet)))
	 (npoints (/ (length list-of-x-and-ys) 2))
	 (points (excl::malloc ;; BUG BUG BUG
		  (* 4 (cond 
			((and closed (not filled))
			 (incf npoints))
			(t npoints)))))
	 (window (tk::widget-window (sheet-mirror sheet))))
		 
    (do ((ps list-of-x-and-ys (cddr ps))
	 (i 0 (1+ i))
	 r)
	((null ps)
	 (setq list-of-x-and-ys (nreverse r)))
      (multiple-value-bind
	  (x y)
	  (devicize-point transform (car ps) (cadr ps))
	(setf (xpoint-array-x points i) x
	      (xpoint-array-y points i) y)))
    
    (when (and closed (not filled))
      (setf (xpoint-array-x points (- npoints 1)) (xpoint-array-x points 0)
	    (xpoint-array-y points (- npoints 1)) (xpoint-array-y points 0)))
    
    (if filled
	(x11:xfillpolygon
	 (tk::display-handle (tk::object-display window))
	 (tk::object-handle window)
	 (tk::object-handle (clx-adjust-ink medium
					    (clx-decode-ink (medium-ink medium) medium)
					    (medium-ink medium)
					    (medium-line-style medium)))
	 points
	 npoints
	 x11:complex
	 x11:coordmodeorigin)
      (x11:xdrawlines
       (tk::display-handle (tk::object-display window))
       (tk::object-handle window)
       (tk::object-handle (clx-adjust-ink medium
					  (clx-decode-ink (medium-ink medium) medium)
					  (medium-ink medium)
					  (medium-line-style medium)))
       points
       npoints
       x11:coordmodeorigin))))
      
    
    
