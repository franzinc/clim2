(in-package :tk)

(defun draw-point (drawable gcontext x y)
  (x11:xdrawpoint 
   (display-handle (object-display drawable))
   (object-handle drawable)
   (object-handle gcontext)
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
   (display-handle (object-display drawable))
   (object-handle drawable)
   (object-handle gcontext)
   x1
   y1
   x2
   y2))


(defstub draw-lines (drawable gcontext points &key relative-p fill-p
			    (shape :complex))
  )


(defstub draw-segments (drawable gcontext segments)
  )

(defun draw-rectangle (drawable gcontext x y width height &optional
				  fill-p)
  (if fill-p
      (x11::xfillrectangle
       (display-handle (object-display drawable))
       (object-handle drawable)
       (object-handle gcontext)
       x
       y
       width height)
    (x11::xdrawrectangle
     (display-handle (object-display drawable))
     (object-handle drawable)
     (object-handle gcontext)
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
		   (object-handle font)
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
  (x11:xtextwidth (object-handle font)
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
   (display-handle (object-display drawable))
   (object-handle drawable)
   (object-handle gc)
   x y 
   (+ start (string-to-char* string))
   (- end start)))



(defun draw-image-string (drawable gc x y string 
			  &optional (length (length string)))
  (x11::drawimagestring
   (display-handle (object-display drawable))
   (object-handle drawable)
   (object-handle gc)
   x y string length))

