;; -*- mode: common-lisp; package: tk -*-
;;
;;				-[]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, CA  All rights reserved.
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
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: graphics.lisp,v 1.4 92/02/24 13:03:04 cer Exp Locker: cer $

(in-package :tk)

(defun draw-point (drawable gcontext x y)
  (x11:xdrawpoint 
   (object-display drawable)
   drawable
   gcontext
   x
   y))

(defmacro defstub (name arglist)
  `(defun ,name ,arglist
     (error "~S not done yet" ',name)))

(defstub draw-points (drawable gcontext points &optional relative-p)
  )

(defun draw-line (drawable gcontext x1 y1 x2 y2 &optional
			     relative-p)
  (x11:xdrawline
   (object-display drawable)
   drawable
   gcontext
   x1
   y1
   x2
   y2))


(defun draw-ellipse (drawable gcontext center-x 
		     center-y
		     radius-1-dx 
		     radius-1-dy 
		     radius-2-dx
		     radius-2-dy 
		     start-angle 
		     end-angle 
		     filled)
  (multiple-value-bind (x-radius y-radius)
      (cond ((and (= radius-1-dx 0) (= radius-2-dy 0))
	     (values (abs radius-2-dx) (abs radius-1-dy)))
	    ((and (= radius-2-dx 0) (= radius-1-dy 0))
	     (values (abs radius-1-dx) (abs radius-2-dy)))
	    (t
	     (let ((s-1 (+ (* radius-1-dx radius-1-dx) 
			   (* radius-1-dy radius-1-dy)))
		   (s-2 (+ (* radius-2-dx radius-2-dx) 
			   (* radius-2-dy radius-2-dy))))
	       (if (= s-1 s-2)
		   (let ((r (truncate (sqrt s-1))))
		     (values r r))
		 ;; Degrade to drawing a rectilinear ellipse
		 (values (truncate (sqrt s-1)) 
			 (truncate (sqrt s-2)))))))
    (if filled
	(x11:xfillarc
	 (object-display drawable)
	 drawable
	 gcontext
	 (- center-x x-radius)
	 (- center-y y-radius)
	 (* 2 x-radius)
	 (* 2 y-radius)
	 0
	 (* 360 64))
      (x11:xdrawarc
       (object-display drawable)
       drawable
       gcontext
       (- center-x x-radius)
       (- center-y y-radius)
       (* 2 x-radius)
       (* 2 y-radius)
       0
       (* 360 64)))))

(defstub draw-lines (drawable gcontext points &key relative-p fill-p
			    (shape :complex))
  )


(defstub draw-segments (drawable gcontext segments)
  )

(defun draw-rectangle (drawable gcontext x y width height &optional
				  fill-p)
  (if fill-p
      (x11::xfillrectangle
       (object-display drawable)
       drawable
       gcontext
       x
       y
       width height)
    (x11::xdrawrectangle
     (object-display drawable)
     drawable
     gcontext
     x
     y
     width height)))

(defun text-extents (font sequence &key (start 0) end translate)
  (unless end (setq end (length sequence)))
  (with-ref-par ((direction 0)
		 (ascent 0)
		 (descent 0))
		(let ((overall (make-x-char-struct)))
		  (x11:xtextextents
		   font
		   (+ start (string-to-char* sequence))
		   (- end start)
		   direction
		   ascent
		   descent
		   overall)
		  (values
		   (x11::xcharstruct-width overall)
					; ascent
					; descent
					; left
					; right
					; font-ascent
					; font-descent
					; direction
					; something-else
		   ))))
		   




(defun text-width (font sequence &key (start 0) end translate)
  (unless end (setq end (length sequence)))
  (x11:xtextwidth font
		  (+ start (string-to-char* sequence))
		  (- end start)))


(defstub draw-glyph (drawable gcontext x y elt
			    &key translate width (size :default))
  )


(defstub draw-glyphs (drawable gcontext x y sequence
			     &key (start 0) end translate width (size
								 :default))
  )


(defstub draw-image-glyph (drawable gcontext x y elt
				  &key translate width (size :default))
  )


(defstub draw-rectangles (drawable gcontext rectangles &optional fill-p)
  )

(defstub draw-arcs (drawable gcontext arcs &optional fill-p)
  )

(defun draw-string (drawable gc x y string &optional (start 0) end)
  (unless start (setq start 0))
  (unless end (setq end (length string)))
  (x11::xdrawstring
   (object-display drawable)
   drawable
   gc
   x y 
   (+ start (string-to-char* string))
   (- end start)))



(defun draw-image-string (drawable gc x y string 
			  &optional (length (length string)))
  (x11::drawimagestring
   (object-display drawable)
   drawable
   gc
   x y string length))


(defun copy-area (src gcontext src-x src-y width height dst dst-x dst-y)
  (x11:xcopyarea
   (object-display src)
   src
   dst
   gcontext
   src-x
   src-y
   width
   height
   dst-x
   dst-y))
