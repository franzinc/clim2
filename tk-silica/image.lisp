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
;; $fiHeader: image.lisp,v 1.8 92/12/14 15:04:25 cer Exp $


(in-package :xm-silica)


(defun make-pattern-from-bitmap-file (file &rest args &key ((:designs supplied-designs)) format)
  (multiple-value-bind
      (array designs)
      (with-keywords-removed (args args '(:designs))
	(apply #'read-bitmap-file file args))
    (make-pattern array 
		  (or designs 
		      supplied-designs 
		      (error "Designs must be specified for this format: ~A" format)))))

;;; 

(defparameter *bitmap-search-path* '("/usr/include/X11/bitmaps/"))

(defun read-bitmap-file (pathname &key (format :bitmap))
  ;; Creates an image from a C include file in standard X11 format
  (declare (type (or pathname string stream) pathname))
  (unless (probe-file pathname)
    (dolist (dir *bitmap-search-path*)
      (let ((p (merge-pathnames pathname dir))) 
	(when (probe-file p)
	  (return (setq pathname p))))))
  (if (member format '(:bitmap :pixmap))
      (with-open-file (fstream pathname :direction :input)
	(case format
	  (:bitmap
	   (read-bitmap-file-1 fstream))
	  (:pixmap
	   (read-pixmap-file-1 fstream))))
    (multiple-value-bind
	(format filter)
	(compute-filter-for-bitmap-format format)
      (with-open-stream (fstream (excl:run-shell-command
				  (format nil "cat ~A | ~A" pathname filter)
				  :wait nil
				  :error-output :stream
				  :output :stream))
	(ecase format
	  (:bitmap
	   (read-bitmap-file-1 fstream))
	  (:pixmap
	   (read-pixmap-file-1 fstream)))))))

(defmethod compute-filter-for-bitmap-format (format)
  (error "Dont know how to convert from the format ~A" format))

(defmethod compute-filter-for-bitmap-format ((format (eql :gif)))
  (values :pixmap "giftoppm | ppmtoxpm"))

;;;

(defun read-bitmap-file-1 (fstream)
  (multiple-value-bind (width height depth left-pad format chars-per-pixel line)
      (get-bitmap-file-properties fstream)
    (declare (ignore format  chars-per-pixel line))
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
	   (bits-per-line (* width bits-per-pixel))
	   (bytes-per-line (ceiling bits-per-line 8))
	   #+ignore
	   (padded-bits-per-line
	    (* (ceiling bits-per-line 32) 32))
	   #+ignore
	   (padded-bytes-per-line
	    (ceiling padded-bits-per-line 8)))

      (labels ((read-a-byte ()
		 (peek-char #\x fstream)
		 (read-char fstream)
		 (+ (* 16 (digit-char-p (read-char fstream) 16))
		    (digit-char-p (read-char fstream) 16))))

	(dotimes (i height)
	  (dotimes (j bytes-per-line)
	    (let* ((byte (read-a-byte))
		   (bit-index (* j 8)))
	      (multiple-value-bind (pixel-offset offset-in-pixel)
		  (truncate bit-index bits-per-pixel)
		(if (<= bits-per-pixel 8)
		    (dotimes (bit (/ 8 bits-per-pixel))
		      (let ((zz (+ pixel-offset bit)))
			(when (< zz width)
			  (setf (aref data i zz)
			    (ldb (byte bits-per-pixel (* bits-per-pixel bit)) byte)))))
		  (progn
		    (assert (zerop (mod bits-per-pixel 8)))
		    (when (< pixel-offset width)
		      (setf (aref data i pixel-offset)
			(dpb byte (byte 8 offset-in-pixel) (aref data i pixel-offset))))))))))

	(values data)))))

(defun read-pixmap-file-1 (fstream)
  (multiple-value-bind (width height depth left-pad format
			chars-per-pixel line)
      (get-bitmap-file-properties fstream)
    (declare (ignore line depth left-pad))
    (assert (= chars-per-pixel 1))
    (assert (= format 1))
    (flet ((read-strings ()
	     (let ((strings nil))
	       (loop
		 (peek-char #\" fstream)
		 (push (read fstream) strings)
		 (let ((next (peek-char t fstream)))
		   (if (eq next #\,) (read-char fstream)
		     (return (nreverse strings)))))))
	   (convert-color (x)
	     (cond ((eq (schar x 0) #\#)
		    (let ((*read-base* 16))
		      (assert (+ (length x) 7))
		      (do ((i 1 (+ i 2))
			   (r nil))
			  ((= i 7)
			   (apply #'make-rgb-color (nreverse r)))
			(push (/ (read-from-string x nil nil :start i :end (+ i 2)) 255.0)
			      r))))
		   ((string= x "white") +white+)
		   ((string= x "black") +black+)
		   (t (error "Dont know the color ~A" x)))))
      (let ((colors (do ((colors (read-strings) (cddr colors))
			 (r nil))
			((null colors) r)
		      (push (cons (coerce (first colors) 'character) 
				  (convert-color (second colors))) r)))
	    (pixels (read-strings))
	    (array (make-array (list height width)))
	    (i 0))
	(declare (type (simple-array t (* *)) array)
		 (string row))
	(excl::fast
	 (dolist (row pixels)
	   (dotimes (j width)
	     (setf (aref array i j)
	       (position (schar row j) colors :key #'car)))
	   (incf i)))
	(values array (mapcar #'cdr colors))))))

(defun get-bitmap-file-properties (fstream)
  (let ((line "")
	(properties nil)
	(name nil)
	(name-end nil))
    (loop
      (setq line (read-line fstream))
      (when  (> (length line) 0)
	(when (char= (aref line 0) #\#)
	  (return))))
  
    (loop
      (when  (> (length line) 0)
	(unless (char= (aref line 0) #\#)
	  (return))
	(flet ((read-keyword (line start end)
		 (cdr (find-if #'(lambda (pair)
				   (string= (car pair)
					    line :start2 start
					    :end2 end))
			       '(("image" . :image)
				 ("width" . :width)
				 ("height". :height)
				 ("depth" . :depth)
				 ("format" . :format)
				 ;;--- 
				 ("pixel" . :chars-per-pixel)
				 ("left_pad" . :left-pad))))))
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
	    (when ind
	      (setf (getf properties ind) val)))))
      (setq line (read-line fstream)))
  
    (flet ((extract-property (ind &rest default)
	     (prog1 (apply #'getf properties ind default)
	       (remf properties ind))))
      (values (extract-property :width)
	      (extract-property :height)
	      (extract-property :depth 1)
	      (extract-property :left-pad 0)
	      (extract-property :format nil)
	      (extract-property :chars-per-pixel nil)
	      line))))
  
  

