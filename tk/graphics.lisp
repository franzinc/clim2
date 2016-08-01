;; -*- mode: common-lisp; package: tk -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :tk)

(defun draw-point (drawable gcontext x y)
  (x11:xdrawpoint
   (object-display drawable)
   drawable
   gcontext
   x
   y))

(defun draw-line (drawable gcontext x1 y1 x2 y2)
  (x11:xdrawline
   (object-display drawable)
   drawable
   gcontext
   x1
   y1
   x2
   y2))


;(defun draw-ellipse (drawable gcontext center-x
;		     center-y
;		     radius-1-dx
;		     radius-1-dy
;		     radius-2-dx
;		     radius-2-dy
;		     start-angle
;		     end-angle
;		     filled)
;  (multiple-value-bind (x-radius y-radius)
;      (cond ((and (= radius-1-dx 0) (= radius-2-dy 0))
;	     (values (round (abs radius-2-dx))
;		     (round (abs radius-1-dy))))
;	    ((and (= radius-2-dx 0) (= radius-1-dy 0))
;	     (values (round (abs radius-1-dx))
;		     (round (abs radius-2-dy))))
;	    (t
;	     (let ((s-1 (+ (* radius-1-dx radius-1-dx)
;			   (* radius-1-dy radius-1-dy)))
;		   (s-2 (+ (* radius-2-dx radius-2-dx)
;			   (* radius-2-dy radius-2-dy))))
;	       (if (= s-1 s-2)
;		   (let ((r (round (sqrt s-1))))
;		     (values r r))
;		 ;; Degrade to drawing a rectilinear ellipse
;		 (values (round (sqrt s-1))
;			 (round (sqrt s-2)))))))
;    (setq start-angle (round (* start-angle (/ (* 360 64) (* 2 pi)))))
;    (setq end-angle (round (* end-angle (/ (* 360 64) (* 2 pi)))))
;    ;;--rounding
;    (if (> end-angle start-angle)
;	(setq end-angle (- end-angle start-angle))
;      (setq end-angle (- start-angle end-angle)))
;    (draw-ellipse-1 drawable
;		    gcontext
;		    (- center-x x-radius)
;		    (- center-y y-radius)
;		    (* 2 x-radius)
;		    (* 2 y-radius)
;		    start-angle end-angle filled)))


(defun draw-ellipse-1 (drawable gcontext x y width height start-angle end-angle filled)
  (if filled
      (x11:xfillarc
       (object-display drawable)
       drawable
       gcontext
       x y width height
       start-angle end-angle)
    (x11:xdrawarc
     (object-display drawable)
     drawable
     gcontext
     x y width height
     start-angle end-angle)))


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

(defun draw-rectangles (drawable gcontext rects nrects &optional
				  fill-p)
  (if fill-p
      (x11::xfillrectangles
       (object-display drawable)
       drawable
       gcontext rects nrects)
    (x11::xdrawrectangles
     (object-display drawable)
     drawable
     gcontext rects nrects)))


#+ignore
(defun text-extents (font sequence &key (start 0) end translate)
  (unless end (setq end (length sequence)))
  (with-ref-par ((direction 0 :int)
		 (ascent 0 :int)
		 (descent 0 :int))
		(let ((overall (make-x-char-struct)))
		  (x11:xtextextents
		   font
		   (+ start (clim-utils:string-to-foreign sequence))
		   (- end start)
		   &direction
		   &ascent
		   &descent
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

#+ignore
(defun text-width (font sequence &key (start 0) end translate)
  (unless end (setq end (length sequence)))
  (x11:xtextwidth font
		  (+ start (clim-utils:string-to-foreign sequence))
		  (- end start)))

(defun draw-multibyte-string (drawable font-set gc x y string
                              &optional start end)
  (setf start (or start 0)
        end (or end (length string)))
  (multiple-value-bind (native-string nbytes)
      (excl:string-to-native string :start start :end end)
    (x11:xmbdrawstring
     (object-display drawable)
     drawable
     font-set
     gc
     x y
     native-string (1- nbytes))
    (excl:aclfree native-string)))

(defun draw-string (drawable gc x y string &optional (start 0) end)
  (unless start (setq start 0))
  (unless end (setq end (length string)))
  (x11:lisp-xdrawstring
   (object-display drawable)
   drawable
   gc
   x y
   (lisp-string-to-string8 string)
   start
   end))

(defun draw-string16 (drawable gc x y string &optional (start 0) end)
  (unless start (setq start 0))
  (unless end (setq end (length string)))
  (x11:lisp-xdrawstring16
   (object-display drawable)
   drawable
   gc
   x y
   (lisp-string-to-string16 string)
   start
   end))

(defun draw-image-string (drawable gc x y string
			  &optional (length (length string)))
  (x11::xdrawimagestring
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
