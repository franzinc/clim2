;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-USER; Base: 10; Lowercase: Yes -*-
;;
;;
;; See the file LICENSE for the full license governing this code.
;;

#-acl86win32
(defpackage :clim-user (:use :clim-silica))

(in-package :clim-user)


(defclass simple-test-sheet (silica::permanent-medium-sheet-output-mixin
			     silica::sheet-multiple-child-mixin
			     silica::sheet-transformation-mixin
			     silica::standard-repainting-mixin
			     silica::immediate-sheet-input-mixin
			     silica::basic-sheet)
  ())

(defmethod sheet-event-queue ((sheet simple-test-sheet)) nil)

(defclass even-simpler (simple-test-sheet) ())

(defun simple-test ()
  (let* ((port (find-port))
	 (graft (find-graft))
	 (sheet (make-instance 'silica::standard-mirrored-sheet
		  :parent graft
		  :region (make-bounding-rectangle 0 0 300 300))))
    (dotimes (i 5)
      (dotimes (j 5)
	(let ((s (make-instance 'simple-test-sheet
		   :parent sheet
		   :region (make-bounding-rectangle 0 0 60 60)
		   :transformation (make-translation-transformation (* i 60) (* j 60)))))
	  (setf (sheet-enabled-p s) t)
	  (let ((s2 (make-instance 'even-simpler
		      :parent s
		      :region (make-bounding-rectangle 0 0 40 40)
		      :transformation (make-translation-transformation 10 10))))
	    (setf (sheet-enabled-p s2) t)))))
    (setf (sheet-enabled-p sheet) t)
    sheet))

(defmethod handle-event ((sheet simple-test-sheet) (event pointer-enter-event))
  (outline-sheet sheet)
  (setf (port-keyboard-input-focus (port sheet)) sheet))

(defun outline-sheet (sheet)
  (with-bounding-rectangle* (a b c d) sheet
    (draw-rectangle* sheet
		     a b (1- c) (1- d)
		     :filled nil
		     :ink +flipping-ink+)))

(defmethod handle-event ((sheet simple-test-sheet) (event pointer-exit-event))
  (outline-sheet sheet))

(defmethod handle-event ((sheet simple-test-sheet) (event pointer-button-event))
  (multiple-value-call #'draw-rectangle*
		       sheet
		       (bounding-rectangle* sheet)
		       :ink +flipping-ink+))

(defmethod handle-event ((sheet simple-test-sheet) (event keyboard-event))
  (draw-line* sheet -200 -200 200 200 :ink +flipping-ink+
	      :line-thickness 10))

(defmethod handle-repaint ((sheet silica::standard-sheet) region)
  (declare (ignore region))
  )

(defmethod handle-repaint ((sheet simple-test-sheet) region)
  (declare (ignore region))
  )

(defmethod handle-repaint ((sheet even-simpler) region)
  (declare (ignore region))
  (multiple-value-call #'draw-rectangle*
		       sheet
		       (bounding-rectangle* sheet)
		       :filled t
		       :ink +red+))
