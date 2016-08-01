;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-


(in-package :clim-internals)

"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved."

;;; A utility for placing glyphs along a vertical path.  Does not do any work yet
;;; to obtain a rotated font; but Bob said that could be a "future".
(defun draw-vertical-string* (stream string x y
				     &key (start 0) end
				     (align-x ':left) (align-y ':top))
  (when (null end) (setq end (length string)))
    ;; What kind of font metrics do we get for the rotated font?  Do we
    ;; get "normal" metrics and then rotate them 90 degrees (i.e. use
    ;; the char width for vertical alignment spacing) or use the font's
    ;; height as the spacing increment?  This currently just uses the
    ;; height.
  (let* ((y-span (stream-line-height stream))
	 (x-span (stream-string-width stream "W"))
	 (count (- end start))
	 (total-y-span (* count y-span))
	 start-x start-y)
    (ecase align-x
      (:left (setq start-y y))
      (:right (setq start-y (+ y total-y-span)))
      (:center (setq start-y (+ y (round total-y-span 2)))))
    (ecase align-y
      (:top (setq start-x x))
      (:bottom (setq start-x (- x x-span)))
      (:center (setq start-x (- x (round x-span 2))))
      ;; --- just using "bottom" for now
      (:baseline (setq start-x (- x x-span))))
    ;; draw string from bottom to top, assuming glyphs will be rotated
    ;; 90 degrees left.
    (dovector (char string :start start :end end)
      (draw-character* stream char start-x start-y)
      (decf start-y y-span))))

(defun draw-vertical-string (stream string point &key (start 0) end
				    (align-x ':left) (align-y ':top))
  (draw-vertical-string* stream string (point-x point) (point-y point)
			 :start start :end end
			 :align-x align-x :align-y align-y))
