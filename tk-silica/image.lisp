;; -*- mode: common-lisp; package: xm-silica -*-
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
;; $fiHeader$


(in-package :xm-silica)


(defun correct-case (x) x)
(defun kintern (x) (intern x :keyword))
(defun make-pattern-from-file (file designs)
  (multiple-value-bind
      (array width height depth)
      (read-bitmap-file file)
    (make-pattern array designs)))

(defun read-bitmap-file (pathname)
  ;; Creates an image from a C include file in standard X11 format
  (declare (type (or pathname string stream) pathname))
  (declare (values image))
  (with-open-file (fstream pathname :direction :input)
    (let ((line "")
	  (properties nil)
	  (name nil)
	  (name-end nil))
      (declare (type string line)
	       (type stringable name)
	       (type list properties))
      ;; Get properties
      
      (loop
	(setq line (read-line fstream))
	(when  (> (length line) 0)
	  (unless (char= (aref line 0) #\#)
	    (return))
	  (flet ((read-keyword (line start end)
		   (kintern
		    (substitute
		     #\- #\_
		     (#-excl string-upcase
			     #+excl correct-case
			     (subseq line start end))
		     :test #'char=))))
	    (when (null name)
	      (setq name-end (position #\_ line :test #'char= :from-end t)
		    name (read-keyword line 8 name-end))
	      (unless (eq name :image)
		(setf (getf properties :name) name)))
	    (let* ((ind-start (1+ name-end))
		   (ind-end (position #\Space line :test #'char=
				      :start ind-start))
		   (ind (read-keyword line ind-start ind-end))
		   (val-start (1+ ind-end))
		   (val (parse-integer line :start val-start)))
	      (setf (getf properties ind) val)))))
      
      (multiple-value-bind (width height depth left-pad)
	  (flet ((extract-property (ind &rest default)
		   (prog1 (apply #'getf properties ind default)
		     (remf properties ind))))
	    (values (extract-property :width)
		    (extract-property :height)
		    (extract-property :depth 1)
		    (extract-property :left-pad 0)))
	(unless (and width height) (error "Not a BITMAP file"))
	(let* ((bits-per-pixel
		(cond ((> depth 24) 32)
		      ((> depth 16) 24)
		      ((> depth 8)  16)
		      ((> depth 4)   8)
		      ((> depth 2)   4)
		      ((> depth 1)   2)
		      (t 1)))
	       (data (make-array (list height width)))
	       (w 0)
	       (h 0))
	  (labels ((read-a-byte ()
		     (peek-char #\x fstream)
		     (read-char fstream)
		     (+ (* 16 (digit-char-p (read-char fstream) 16))
			(digit-char-p (read-char fstream) 16)))
		   (store-byte (x)
		     (setf (aref data h w) x)
		     (incf w)
		     (when (= w width)
		       (setq w 0)
		       (incf h))
		     (when (= h height)
		       (return-from read-bitmap-file
			 (values data
				 width 
				 height
				 depth)))))
	    (loop
	      (let ((x 0))
		(dotimes (i (ceiling bits-per-pixel 8))
		  (setf x (ash x 8))
		  (incf x (read-a-byte)))
		(if (>= bits-per-pixel 8)
		    (store-byte x)
		  (dotimes (j (/ 8 bits-per-pixel))
		    (store-byte
		     (ldb (byte bits-per-pixel (* j bits-per-pixel))
			  x))))))))))))
