;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: clim-user; Base: 10; Lowercase: Yes -*-
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
;; $fiHeader: simple-test.lisp,v 1.7 92/07/20 16:00:48 cer Exp $


(defpackage :clim-user (:use :clim-silica))

(in-package :clim-user)

(defclass simple-test-sheet (silica::permanent-medium-sheet-output-mixin
			     silica::sheet-multiple-child-mixin
			     silica::sheet-transformation-mixin
			     silica::standard-repainting-mixin
			     silica::immediate-sheet-input-mixin
			     sheet)
  ())

(defclass even-simpler (simple-test-sheet) ())

(defun simple-test ()
  (let* ((port (find-port))
	 (graft (find-graft))
	 (sheet (make-instance 'standard-sheet
			       :parent graft
			       :region (make-bounding-rectangle 0 0 300 300))))
    (dotimes (i 5)
      (dotimes (j 5)
	(let ((s (make-instance 'simple-test-sheet
				:parent sheet
				:region (make-bounding-rectangle 0 0 60 60)
				:transformation
				(make-translation-transformation (* i 60) (* j 60)))))
	  (setf (sheet-enabled-p s) t)
	  (let ((s2 (make-instance 'even-simpler
				   :parent s
				   :region (make-bounding-rectangle 0 0 40 40)
				   :transformation
				   (make-translation-transformation 10 10))))
	    (setf (sheet-enabled-p s2) t)))))
    (setf (sheet-enabled-p sheet) t)
    sheet))

(defmethod handle-event ((sheet simple-test-sheet) (event pointer-enter-event))
  (outline-sheet sheet)
  (setf (port-keyboard-input-focus (port sheet)) sheet))

(defun outline-sheet (sheet)
  (with-bounding-rectangle* (a b c d) sheet
			    (draw-rectangle* 
			     sheet 
			     a b (1- c) (1- d)
			     :filled nil
			     :ink +flipping-ink+)))

(defmethod handle-event ((sheet simple-test-sheet) (event pointer-exit-event))
    (outline-sheet sheet))


(defmethod handle-event ((sheet simple-test-sheet) (event pointer-button-event))
  (multiple-value-call
      #'draw-rectangle*
    sheet
    (bounding-rectangle* sheet)
    :ink +flipping-ink+))

(defmethod handle-event ((sheet simple-test-sheet) (event keyboard-event))
  (draw-line* sheet -200 -200 200 200 :ink +flipping-ink+
	      :line-thickness 10))


(defmethod repaint-sheet ((sheet even-simpler) region)
  (declare (ignore region))
  (multiple-value-call #'draw-rectangle* 
    sheet 
    (bounding-rectangle* sheet)
    :filled t
    :ink +red+))

(defmethod repaint-sheet ((sheet standard-sheet) (region t))
  )

(defmethod repaint-sheet ((sheet simple-test-sheet) (region t))
  )


