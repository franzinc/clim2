;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: xt-stipples.lisp,v 1.4.36.1 2002/02/08 19:11:25 layer Exp $

;;; stipples are bitmaps used for monochrome half-toning and to
;;; implement opacities.

(in-package :xm-silica)

(defvar *stipple-data*
    '((0.05 #2A((0)))
      (0.1  #2A((1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
		(0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0)
		(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0)
		(0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
		(0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0)
		(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0)
		(0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0)
		(0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0)))
      (0.2  #2A((1 0 0 0 0 0 0 0)
		(0 0 0 1 0 0 0 0)
		(0 0 0 0 0 0 1 0)
		(0 1 0 0 0 0 0 0)
		(0 0 0 0 1 0 0 0)
		(0 0 0 0 0 0 0 1)
		(0 0 1 0 0 0 0 0)
		(0 0 0 0 0 1 0 0)))
      (0.3  #2A((1 0 0 0)
		(0 0 1 0)
		(0 1 0 0)
		(0 0 0 1)))
      (0.4  #2A((1 0 0)
		(0 1 0)
		(0 0 1)))
      (0.6  #2A((1 0)
		(0 1)))
      (0.7  #2A((0 1 1)
		(1 0 1)
		(1 1 0)))
      (0.8  #2A((0 1 1 1)
		(1 1 0 1)
		(1 0 1 1)
		(1 1 1 0)))
      (0.9  #2A((0 1 1 1 1 1 1 1)
		(1 1 1 0 1 1 1 1)
		(1 1 1 1 1 1 0 1)
		(1 0 1 1 1 1 1 1)
		(1 1 1 1 0 1 1 1)
		(1 1 1 1 1 1 1 0)
		(1 1 0 1 1 1 1 1)
		(1 1 1 1 1 0 1 1)))
      (0.95 #2A((0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
		(1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1)
		(1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1)
		(1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1)
		(1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1)
		(1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1)
		(1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1)
		(1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1)))
      (1   #2A((1)))))

(defun decode-stipple (luminosity port)
  (dolist (entry (port-stipples port))
    (let ((l (car entry))
	  (stipple (cdr entry)))
      (when (<= luminosity l)
	(return-from decode-stipple stipple)))))

(defun setup-stipples (port display)
  (let ((stipples nil)
	(root (tk::display-root-window display))
	(gc (port-copy-gc-depth-1 port)))
    (dolist (entry *stipple-data*)
      (let* ((intensity (first entry))
	     (data (second entry))
	     (height (array-dimension data 0))
	     (width (array-dimension data 1))
	     (image (make-instance 'tk::image
		      :display display :depth 1
		      :width width :height height
		      :data data))
	     (pixmap (make-instance 'tk::pixmap
		       :drawable root :depth 1
		       :width width :height height)))
	(tk::put-image pixmap gc image)
	(tk::destroy-image image)
	(push (cons intensity pixmap) stipples)))
    (setf (port-stipples port) (nreverse stipples))))
