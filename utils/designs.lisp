;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: designs.lisp,v 1.7 92/10/04 14:16:14 cer Exp $

(in-package :clim-utils)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions Copyright (c) 1992 Franz, Inc.  All rights reserved."


;;; Designs

(defconstant ihs-rgb-c1 (sqrt (/ 6f0)))
(defconstant ihs-rgb-c2 (sqrt (/ 2f0)))
(defconstant ihs-rgb-c3 (sqrt (/ 3f0)))

(defconstant sqrt3 (sqrt 3))


;;; Opacity

(defun make-opacity (opacity)
  #+Genera (declare lt:(side-effects simple reducible))
  (assert (and (numberp opacity) (<= 0 opacity 1)) (opacity)
	  "The opacity ~S is not a number between 0 and 1" opacity)
  (cond ((= opacity 0) +nowhere+)
	((= opacity 1) +everywhere+)
	(t (make-standard-opacity-1 opacity))))


;; Black and white are the same everywhere
(defconstant +black+ (make-gray-color-1 0f0))
(defconstant +white+ (make-gray-color-1 1f0))


;;; Gray colors

(defun make-gray-color (luminosity)
  #+Genera (declare lt:(side-effects simple reducible))
  (assert (and (numberp luminosity) (<= 0 luminosity 1)) (luminosity)
	  "The luminosity ~S is not a number between 0 and 1" luminosity)
  (cond ((= luminosity 0) +black+)
	((= luminosity 1) +white+)
	(t (make-gray-color-1 (float luminosity 0f0)))))

(defmethod print-object ((color gray-color) stream)
  (print-unreadable-object (color stream :type t :identity t)
    (with-slots (luminosity) color
      (cond ((= luminosity 0f0)
	     (format stream "Black"))
	    ((= luminosity 1f0)
	     (format stream "White"))
	    (t
	     (format stream "~D% Gray" (round (* 100 luminosity))))))))

(defmethod color-rgb ((color gray-color))
  (let ((luminosity (slot-value color 'luminosity)))
    (values luminosity luminosity luminosity)))

(defmethod color-ihs ((color gray-color))
  (let* ((luminosity (slot-value color 'luminosity))
	 (intensity (* ihs-rgb-c3 (+ luminosity luminosity luminosity))))
    (values intensity 0f0 0f0)))


;;; Colors

(defun make-rgb-color (red green blue)
  (assert (and (numberp red) (<= 0 red 1)) (red)
	  "The red value ~S is not a number between 0 and 1" red)
  (assert (and (numberp green) (<= 0 green 1)) (green)
	  "The green value ~S is not a number between 0 and 1" green)
  (assert (and (numberp blue) (<= 0 blue 1)) (blue)
	  "The blue value ~S is not a number between 0 and 1" blue)
  (if (= red green blue)
      (make-gray-color red)
      (make-rgb-color-1 (float red 0f0) (float green 0f0) (float blue 0f0))))

(defmethod print-object ((color rgb-color) stream)
  (print-unreadable-object (color stream :type t :identity t)
    (with-slots (red green blue) color
      (format stream "R=~F G=~F B=~F" red green blue))))

(defmethod color-rgb ((color rgb-color))
  (with-slots (red green blue) color
    (values red green blue)))

(defmethod color-ihs ((color rgb-color))
  (let* ((red (slot-value color 'red))
	 (green (slot-value color 'green))
	 (blue (slot-value color 'blue))
	 (x (* ihs-rgb-c1 (- (+ red red) blue green)))
	 (y (* ihs-rgb-c2 (- green blue)))
	 (z (* ihs-rgb-c3 (+ red green blue)))
	 (q (+ (* x x) (* y y)))
	 (intensity (sqrt (+ q (* z z)))))	;== (sqrt (+ r^2 g^2 b^2))!
    (if (zerop q)
	(values intensity 0f0 0f0)		;A totally unsaturated color.
	(let* ((hue (mod (/ (atan y x) 2pi) 1f0))
	       (f1 (/ z intensity))
	       (f2 (sqrt (- 1f0 (* f1 f1))))
	       (saturation (atan f2 f1)))
	  (values intensity hue saturation)))))

(defun-inline color-luminosity (r g b)
  ;; From Foley and Van Dam, page 613 (discussion of YIQ color model)...
  (+ (* 0.299f0 r) (* 0.587f0 g) (* 0.114f0 b)))


;;; Color constants

(defmacro define-primary-color (color-name r g b)
  `(defconstant ,color-name 
		(make-rgb-color-1 (float ,r 0f0) (float ,g 0f0) (float ,b 0f0))))

;; The primary colors, constant across all platforms
(define-primary-color +red+     1 0 0)
(define-primary-color +green+   0 1 0)
(define-primary-color +blue+    0 0 1)
(define-primary-color +cyan+    0 1 1)
(define-primary-color +magenta+ 1 0 1)
(define-primary-color +yellow+  1 1 0)


(defvar *default-named-color-table* (make-hash-table :test #+Genera #'string-equal
							   #-Genera #'equalp))

(defmacro define-named-color (color-name r g b)
  `(setf (gethash ,color-name *default-named-color-table*)
	 (make-rgb-color ,(/ r 255.0) ,(/ g 255.0) ,(/ b 255.0))))

;; Simplest possible palette returns canned, silly X Windows colors
(defmethod find-named-color (name (palette basic-palette))
  (let ((name (if (symbolp name) (symbol-name name) name)))
    (values (gethash name *default-named-color-table*))))

;; Default values for named colors
(define-named-color "snow"              255 250 250)
(define-named-color "ghost-white"	248 248 255)
(define-named-color "white-smoke"	245 245 245)
(define-named-color "gainsboro"		220 220 220)
(define-named-color "floral-white"	255 250 240)
(define-named-color "old-lace"		253 245 230)
(define-named-color "linen"		250 240 230)
(define-named-color "antique-white"	250 235 215)
(define-named-color "papaya-whip"	255 239 213)
(define-named-color "blanched-almond"	255 235 205)
(define-named-color "bisque"		255 228 196)
(define-named-color "peach-puff"	255 218 185)
(define-named-color "navajo-white"	255 222 173)
(define-named-color "moccasin"		255 228 181)
(define-named-color "cornsilk"		255 248 220)
(define-named-color "ivory"		255 255 240)
(define-named-color "lemon-chiffon"	255 250 205)
(define-named-color "seashell"		255 245 238)
(define-named-color "honeydew"		240 255 240)
(define-named-color "mint-cream"	245 255 250)
(define-named-color "azure"		240 255 255)
(define-named-color "alice-blue"	240 248 255)
(define-named-color "lavender"		230 230 250)
(define-named-color "lavender-blush"	255 240 245)
(define-named-color "misty-rose"	255 228 225)
(define-named-color "dark-slate-gray"	 47  79  79)
(define-named-color "dim-gray"		105 105 105)
(define-named-color "slate-gray"	112 128 144)
(define-named-color "light-slate-gray"	119 136 153)
(define-named-color "gray"		192 192 192)
(define-named-color "light-gray"	211 211 211)
(define-named-color "midnight-blue"      25  25 112)
(define-named-color "navy-blue"		  0   0 128)
(define-named-color "cornflower-blue"   100 149 237)
(define-named-color "dark-slate-blue"    72  61 139)
(define-named-color "slate-blue"        106  90 205)
(define-named-color "medium-slate-blue" 123 104 238)
(define-named-color "light-slate-blue"	132 112 255)
(define-named-color "medium-blue"	  0   0 205)
(define-named-color "royal-blue"	 65 105 225)
(define-named-color "dodger-blue"	 30 144 255)
(define-named-color "deep-sky-blue"	  0 191 255)
(define-named-color "sky-blue"		135 206 235)
(define-named-color "light-sky-blue"	135 206 250)
(define-named-color "steel-blue"	 70 130 180)
(define-named-color "light-steel-blue"	176 196 222)
(define-named-color "light-blue"	173 216 230)
(define-named-color "powder-blue"	176 224 230)
(define-named-color "pale-turquoise"	175 238 238)
(define-named-color "dark-turquoise"	  0 206 209)
(define-named-color "medium-turquoise"	 72 209 204)
(define-named-color "turquoise"		 64 224 208)
(define-named-color "light-cyan"	224 255 255)
(define-named-color "cadet-blue"	 95 158 160)
(define-named-color "medium-aquamarine"	102 205 170)
(define-named-color "aquamarine"	127 255 212)
(define-named-color "dark-green"	  0 100   0)
(define-named-color "dark-olive-green"	 85 107  47)
(define-named-color "dark-sea-green"	143 188 143)
(define-named-color "sea-green"		 46 139  87)
(define-named-color "medium-sea-green"	 60 179 113)
(define-named-color "light-sea-green"	 32 178 170)
(define-named-color "pale-green"	152 251 152)
(define-named-color "spring-green"	  0 255 127)
(define-named-color "lawn-green"	124 252   0)
(define-named-color "chartreuse"	127 255   0)
(define-named-color "medium-spring-green" 0 250 154)
(define-named-color "green-yellow"	173 255  47)
(define-named-color "lime-green"	 50 205  50)
(define-named-color "yellow-green"	154 205  50)
(define-named-color "forest-green"	 34 139  34)
(define-named-color "olive-drab"	107 142  35)
(define-named-color "dark-khaki"	189 183 107)
(define-named-color "khaki"		240 230 140)
(define-named-color "pale-goldenrod"	238 232 170)
(define-named-color "light-goldenrod-yellow" 250 250 210)
(define-named-color "light-yellow"      255 255 224)
(define-named-color "gold"		255 215   0)
(define-named-color "light-goldenrod"	238 221 130)
(define-named-color "goldenrod"		218 165  32)
(define-named-color "dark-goldenrod"	184 134  11)
(define-named-color "rosy-brown"	188 143 143)
(define-named-color "indian-red"	205  92  92)
(define-named-color "saddle-brown"	139  69  19)
(define-named-color "sienna"		160  82  45)
(define-named-color "peru"		205 133  63)
(define-named-color "burlywood"		222 184 135)
(define-named-color "beige"		245 245 220)
(define-named-color "wheat"		245 222 179)
(define-named-color "sandy-brown"	244 164  96)
(define-named-color "tan"		210 180 140)
(define-named-color "chocolate"		210 105  30)
(define-named-color "firebrick"		178  34  34)
(define-named-color "brown"		165  42  42)
(define-named-color "dark-salmon"	233 150 122)
(define-named-color "salmon"		250 128 114)
(define-named-color "light-salmon"	255 160 122)
(define-named-color "orange"		255 165   0)
(define-named-color "dark-orange"	255 140   0)
(define-named-color "coral"		255 127  80)
(define-named-color "light-coral"	240 128 128)
(define-named-color "tomato"		255  99  71)
(define-named-color "orange-red"	255  69   0)
(define-named-color "hot-pink"		255 105 180)
(define-named-color "deep-pink"		255  20 147)
(define-named-color "pink"		255 192 203)
(define-named-color "light-pink"	255 182 193)
(define-named-color "pale-violet-red"	219 112 147)
(define-named-color "maroon"		176  48  96)
(define-named-color "medium-violet-red" 199  21 133)
(define-named-color "violet-red"	208  32 144)
(define-named-color "violet"		238 130 238)
(define-named-color "plum"		221 160 221)
(define-named-color "orchid"		218 112 214)
(define-named-color "medium-orchid"	186  85 211)
(define-named-color "dark-orchid"	153  50 204)
(define-named-color "dark-violet"	148   0 211)
(define-named-color "blue-violet"	138  43 226)
(define-named-color "purple"		160  32 240)
(define-named-color "medium-purple"	147 112 219)
(define-named-color "thistle"		216 191 216)


(defun make-ihs-color (intensity hue saturation)
  #+Genera (declare lt:(side-effects simple reducible))
  (assert (and (numberp intensity) (<= 0 intensity sqrt3)) (intensity)
	  "The intensity value ~S is not a number between 0 and (SQRT 3)" intensity)
  (assert (and (numberp hue) (<= 0 hue 1)) (hue)
	  "The hue value ~S is not a number between 0 and 1" hue)
  (assert (and (numberp saturation) (<= 0 saturation 1)) (saturation)
	  "The saturation value ~S is not a number between 0 and 1" saturation)
  (cond ((= intensity 0) +black+)
	(t
	 (make-ihs-color-1 (float intensity 0f0) (float hue 0f0) (float saturation 0f0)))))

(defmethod print-object ((color ihs-color) stream)
  (print-unreadable-object (color stream :type t :identity t)
    (with-slots (intensity hue saturation) color
      (format stream "i=~F h=~F s=~F>" intensity hue saturation))))

(defmethod color-rgb ((color ihs-color))
  (let* ((intensity (slot-value color 'intensity))
	 (hue (slot-value color 'hue))
	 (saturation (slot-value color 'saturation))
	 (hh (mod (- hue .5f0) 1.0f0))
	 (hh (- (* hh 2.0f0 3.1415926535f0) 3.1415926535f0))
	 (s3 (sin saturation))
	 (x (* (sqrt ihs-rgb-c1) s3 (cos hh) intensity))
	 (y (* (sqrt ihs-rgb-c2) s3 (sin hh) intensity))
	 (z (* (sqrt ihs-rgb-c3) (cos saturation) intensity)))
    (macrolet ((range (x)
		 `(max 0.0f0 (min 1.0f0 ,x))))
      (values (range (+ x x z))
	      (range (+ y z (- x)))
	      (range (- z x y))))))

(defmethod color-ihs ((color ihs-color))
  (with-slots (intensity hue saturation) color
    (values intensity hue saturation)))


;;; Mutable Colors 

(defun make-mutable-color (color)
  (make-mutable-color-1 color))

(defmethod print-object ((color mutable-color) stream)
  (print-unreadable-object (color stream :type t :identity t)
    (princ (mutable-color-color color) stream)))

(defmethod color-rgb ((color mutable-color))
  (color-rgb (mutable-color-color color)))

(defmethod color-ihs ((color mutable-color))
  (color-ihs (mutable-color-color color)))

(defvar *doing-delayed-mutations* nil)

(defmethod mutate-mutable ((mutable mutable-color) (color color))
  (if *doing-delayed-mutations*
      (dolist (palette (mutable-color-palettes mutable))
	(let ((cell (gethash mutable (palette-mutable-color-cache palette)))
	      (mutations (palette-delayed-mutations palette)))
	  (without-scheduling
	    (vector-push-extend cell mutations)
	    (vector-push-extend color mutations))))
      (dolist (palette (mutable-color-palettes mutable))
	(let ((cell (gethash mutable (palette-mutable-color-cache palette))))
	  (update-palette-entry palette cell color)))))

(defmethod (setf mutable-color-color) :after ((color color) (mutable mutable-color))
  (mutate-mutable mutable color))

(defmethod mutate-color ((mutable mutable-color) (color color))
  (setf (slot-value mutable 'color) color)
  (mutate-mutable mutable color))

;; Note that the actual color mutation occurs on exiting the outermost
;; call to WITH-DELAYED-MUTATIONS
(defmacro with-delayed-mutations (&body body)
  (let ((outer-doing-delayed-mutations '#:outer-doing-delayed-mutations)
	(palette '#:palette)
	(mutations '#:mutations))
    `(let ((,outer-doing-delayed-mutations *doing-delayed-mutations*)
	   (*doing-delayed-mutations* t))
       (unwind-protect
	   ,@body
	 (progn
	   (unless ,outer-doing-delayed-mutations
	     (dolist (,palette *all-palettes*)
	       (let ((,mutations (palette-delayed-mutations ,palette)))
		 (update-palette-entries ,palette ,mutations)
		 (setf (fill-pointer ,mutations) 0)))))))))


;;; Color Groups

(defun map-over-group-colors (function group &optional layers)
  (declare (dynamic-extent function))
  (let ((group-layers (color-group-layers group)))
    ;; It would be nice if this didn't cons so much
    (labels ((iterate (layers group-layers dimensions)
	       (if group-layers
		   (let ((layer (car layers))
			 (group-layer (car group-layers)))
		     (if layer
			 (iterate (cdr layers) 
				  (cdr group-layers) 
				  (cons layer dimensions))
			 (dotimes (i group-layer)
			   (iterate (cdr layers)
				    (cdr group-layers)
				    (cons i dimensions)))))
		   (funcall function dimensions))))
      (iterate (reverse layers) (reverse group-layers) nil))))

(defmethod initialize-instance :after ((group color-group) &key mutable-array)
  (map-over-group-colors
    #'(lambda (dimensions)
	(setf (apply #'aref mutable-array dimensions)
	      (make-mutable-color-1 +black+)))
    group))

(defmethod group-color ((color-group color-group) &rest layers)
  (declare (dynamic-extent layers))
  (let ((cache (color-group-cache color-group)))
    (or (gethash layers cache)
	(let ((layers (copy-list layers)))
	  (setf (gethash layers cache)
		(make-group-color color-group layers))))))

(defmethod mutate-color ((group-color group-color) (color color))
  (with-delayed-mutations
    (dolist (mutable (group-color-mutables group-color))
      (mutate-mutable mutable color))))

;; GROUP-COLOR-MUTABLES should not be exported to the user.  It is important
;; that these mutables are not drawn with.  Instead the fully specified group
;; colors should be used
(defmethod group-color-mutables ((group-color group-color))
  (with-slots (group layers mutables) group-color
    (or mutables
	(setf mutables
	      (let ((mutable-array (color-group-mutable-array group))
		    mutables)
		(map-over-group-colors #'(lambda (dimensions)
					   (push (apply #'aref mutable-array dimensions)
						 mutables))
				       group
				       layers)
		mutables)))))


;;; Foreground and background (indirect) inks

(defmethod print-object ((design (eql +foreground-ink+)) stream)
  (print-unreadable-object (design stream)
    (write-string "CLIM Foreground" stream)))

(defmethod print-object ((design (eql +background-ink+)) stream)
  (print-unreadable-object (design stream)
    (write-string "CLIM Background" stream)))


;;; Flipping inks

(defmethod print-object ((design flipping-ink) stream)
  (print-unreadable-object (design stream :type t :identity t)
    (with-slots (design1 design2) design
      (format stream "~A and ~A" design1 design2))))

(defconstant +flipping-ink+ (make-flipping-ink-1 +foreground-ink+ +background-ink+))

(defmethod make-flipping-ink (design1 design2)
  (cond ((eq design1 design2)
	 +nowhere+)
	((or (and (eq design1 +foreground-ink+) (eq design2 +background-ink+))
	     (and (eq design2 +foreground-ink+) (eq design1 +background-ink+)))
	 +flipping-ink+)
	(t
	 (make-flipping-ink-1 design1 design2))))

(defmethod decode-flipping-ink ((design flipping-ink))
  (with-slots (design1 design2) design
    (values design1 design2)))


;;; Contrasting inks

(defun make-contrasting-inks (n &optional k)
  (check-type n (integer 2 8))			;--- 8 is pretty small
  (etypecase k
    (null
      (let ((result (make-array n)))
	(dotimes (k n)
	  (setf (svref result k) (make-contrasting-ink-1 k n)))
	result))
    ((integer 0 *)
     (assert (< k n))
     (make-contrasting-ink-1 k n))))

(defmethod contrasting-inks-limit (port)
  (declare (ignore port))
  8)						;--- 8 is pretty small

(defmethod print-object ((design contrasting-ink) stream)
  (print-unreadable-object (design stream :type t :identity t)
    (with-slots (which-one how-many) design
      (format stream "~D of ~D" which-one how-many))))

(defmethod contrasting-ink-index ((ink contrasting-ink))
  (with-slots (how-many which-one) ink
    (values which-one how-many)))

(defparameter *contrasting-inks*
	      (list +red+ +blue+ +green+ +yellow+ +cyan+ +magenta+ +black+ +white+))

(defmethod make-color-for-contrasting-ink ((ink contrasting-ink))
  (with-slots (which-one) ink
    (nth which-one *contrasting-inks*)))

(defmethod make-gray-color-for-contrasting-ink ((ink contrasting-ink))
  (with-slots (how-many which-one) ink
    (make-gray-color (/ which-one how-many))))


;;; Contrasting dash patterns

(defparameter *dash-pattern-grain-size* 3)
(defparameter *contrasting-dash-patterns*
	      '((1 1)					;2
		(2 1) (1 2)				;3
		(3 1) (2 2)				;4
		(2 3) (1 4) (2 1 1 1) (1 2 1 1)		;5
		(4 2) (3 3) (2 4) (3 1 1 1) (2 2 1 1)	;6
		(3 2 1 1) (3 1 2 1)))			;7

(defun make-contrasting-dash-patterns (n &optional k)
  (check-type n (integer 2 16))
  (flet ((make-dash-pattern (index)
	   (let* ((known (nth index *contrasting-dash-patterns*))
		  (pattern (make-array (length known)
				       :element-type 'fixnum :initial-contents known)))
	     (dotimes (i (length known))
	       (setf (aref pattern i) (* (aref pattern i) *dash-pattern-grain-size*)))
	     pattern)))
    (declare (dynamic-extent #'make-dash-pattern))
    (etypecase k
      (null 
	(let ((patterns (make-array n)))
	  (dotimes (i n)
	    (setf (aref patterns i) (make-dash-pattern i)))
	  patterns))
      (integer
	(assert (< k n))
	(make-dash-pattern k)))))

(defmethod contrasting-dash-patterns-limit (port)
  (declare (ignore port))
  16)


;;; Patterns

(defmethod print-object ((design pattern) stream)
  (print-unreadable-object (design stream :type t :identity t)
    (with-slots (array designs) design
      (format stream "~Dx~D n=~D"
	      (array-dimension array 1) (array-dimension array 0) (length designs)))))

(defmethod decode-pattern ((pattern pattern))
  (with-slots (array designs) pattern
    (values array designs)))

(defmethod pattern-width ((pattern pattern))
  (with-slots (array) pattern
    (array-dimension array 1)))

(defmethod pattern-height ((pattern pattern))
  (with-slots (array) pattern
    (array-dimension array 0)))


;;; Stencils

(defmethod print-object ((design stencil) stream)
  (print-unreadable-object (design stream :type t :identity t)
    (with-slots (array) design
      (format stream "~Dx~D" (array-dimension array 1) (array-dimension array 0)))))


;;; Tiles

(defmethod print-object ((tile rectangular-tile) stream)
  (print-unreadable-object (tile stream :type t :identity t)
    (with-slots (design width height) tile
      (format stream "~Dx~D of " width height)
      (write design :stream stream))))

(defmethod decode-rectangular-tile ((tile rectangular-tile))
  (with-slots (design width height) tile
    (values design width height)))

;;; Compatibility with the old stipple feature, perhaps temporary until
;;; rendering of tiles and patterns is fully implemented
;;; This could be done with methods, but there is very little point to that
(defun decode-tile-as-stipple (rectangular-tile)
  (declare (values array width height))
  (multiple-value-bind (pattern width height)
      (decode-rectangular-tile rectangular-tile)
    (when (typep pattern 'pattern)
      (multiple-value-bind (array designs)
	  (decode-pattern pattern)
	(when (and (= width (array-dimension array 1))
		   (= height (array-dimension array 0))
		   (= (length designs) 2)
		   (eq (aref designs 0) +background-ink+)
		   (eq (aref designs 1) +foreground-ink+))
	  (values array width height))))))


;;; Composite designs

(defmethod print-object ((design composite-over) stream)
  (print-unreadable-object (design stream :type t :identity t)
    (with-slots (designs) design
      (map nil #'(lambda (design)
		   (write-char #\space stream)
		   (write design :stream stream))
	   designs))))

(defmethod transform-region ((transformation transformation) (design composite-over))
  (with-slots (designs) design
    (flet ((transform (design)
	     (transform-region transformation design)))
      (declare (dynamic-extent #'transform))
      (make-instance 'composite-over :designs (map 'vector #'transform designs)))))

(defmethod compose-over (design1 design2)
  (make-instance 'composite-over :designs (vector design1 design2)))

(defmethod compose-over ((region1 region) (region2 region))
  (region-union region2 region1))


(defmethod print-object ((design composite-in) stream)
  (print-unreadable-object (design stream :type t :identity t)
    (with-slots (designs) design
      (map nil #'(lambda (design)
		   (write-char #\space stream)
		   (write design :stream stream))
	   designs))))

(defmethod transform-region ((transformation transformation) (design composite-in))
  (with-slots (designs) design
    (compose-in (transform-region transformation (aref designs 0))
		(transform-region transformation (aref designs 1)))))

(defmethod compose-in (ink design)
  (make-instance 'composite-in :designs (vector ink design)))

(defmethod compose-in ((region1 region) (region2 region))
  (region-intersection region2 region1))


(defmethod print-object ((design composite-out) stream)
  (print-unreadable-object (design stream :type t :identity t)
    (with-slots (designs) design
      (map nil #'(lambda (design)
		   (write-char #\space stream)
		   (write design :stream stream))
	   designs))))

(defmethod transform-region ((transformation transformation) (design composite-out))
  (with-slots (designs) design
    (compose-out (transform-region transformation (aref designs 0))
		 (transform-region transformation (aref designs 1)))))

(defmethod compose-out (ink design)
  (make-instance 'composite-out :designs (vector ink design)))

(defmethod compose-out ((region1 region) (region2 region))
  (region-difference region2 region1))
