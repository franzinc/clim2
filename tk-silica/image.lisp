;; -*- mode: common-lisp; package: xm-silica -*-
;;
;;				-[Mon Aug  2 10:00:30 1993 by colin]-
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
;; $fiHeader: image.lisp,v 1.17 1993/11/18 18:45:30 cer Exp $

(in-package :xm-silica)


(defun make-pattern-from-bitmap-file (file &rest args &key ((:designs supplied-designs)) format)
  (multiple-value-bind (array designs)
      (with-keywords-removed (args args '(:designs))
	(apply #'read-bitmap-file file args))
    (make-pattern array 
		  (or designs 
		      supplied-designs 
		      (error "Designs must be specified for this format: ~A"
			     format)))))

;;; 

(defparameter *bitmap-search-path* '("/usr/include/X11/bitmaps/"))

(defun read-bitmap-file (pathname &key (format :bitmap) (port (find-port)))
  ;; Creates an image from a C include file in standard X11 format
  (declare (type (or pathname string stream) pathname))
  (unless (probe-file pathname)
    (dolist (dir *bitmap-search-path*)
      (let ((p (merge-pathnames pathname dir))) 
	(when (probe-file p)
	  (return (setq pathname p))))))
  (let ((palette (and port (port-default-palette port))))
    (read-image-file format pathname palette)))

(defmethod read-image-file ((format (eql :bitmap)) pathname palette)
  (declare (ignore palette))
  (if (streamp pathname)
      (read-bitmap-file-1 pathname)
    (with-open-file (fstream pathname :direction :input)
      (read-bitmap-file-1 fstream))))

(defmethod read-image-file ((format (eql :pixmap)) pathname palette)
  (if (streamp pathname)
      (read-pixmap-file-1 pathname palette)
    (with-open-file (fstream pathname :direction :input)
      (read-pixmap-file-1 fstream palette))))

(defmethod read-image-file ((format t) pathname palette)
  (multiple-value-bind (format filter)
      (compute-filter-for-bitmap-format format)
    (let* ((truename (truename pathname))
	   (command (format nil "cat ~A | ~A" truename filter)))
      (unwind-protect
	  (with-open-stream (fstream (excl:run-shell-command
				      command
				      :wait nil
				      ;;:error-output :stream
				      :output :stream))
	    (handler-case (read-image-file format fstream palette)
	      (error (c)
		(error "Unable to read image file: (~s ~s ~s) signalled \"~a\" ~
			while executing ~s. ~
			Make sure programs like xpm are executable in your path."
		       'read-image-file format pathname palette command)
		)))
	(sys:os-wait)))))

(defmethod compute-filter-for-bitmap-format (format)
  (error "Dont know how to convert from the format ~A" format))

(defmethod compute-filter-for-bitmap-format ((format (eql :gif)))
  (values :pixmap "giftoppm | ppmtoxpm"))

;;;

(defun read-bitmap-file-1 (fstream)
  (multiple-value-bind (width height depth left-pad format chars-per-pixel line)
      (get-bitmap-file-properties fstream)
    (declare (ignore format  chars-per-pixel line left-pad))
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

(defun read-pixmap-file-1 (fstream palette)
  (multiple-value-bind (width height depth left-pad format
			chars-per-pixel line)
      (get-bitmap-file-properties fstream)
    (declare (ignore line depth left-pad))
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
		(if palette
		    (find-named-color x palette)
		  x)))
      (let ((colors (do ((colors (read-strings) (cddr colors))
			 (r nil))
			((null colors) r)
		      (push (cons (first colors) 
				  (convert-color (second colors))) r)))
	    (pixels (read-strings))
	    (array (make-array (list height width)))
	    (i 0))
	(declare (type (simple-array t (* *)) array)
		 (string row))
	(excl::fast
	 (dolist (row pixels)
	   (dotimes (j width)
	     (let ((index (* j chars-per-pixel)))
	       (setf (aref array i j)
		 (position
		  (subseq row index (+ index chars-per-pixel))
		  colors :key #'car :test #'equal))))
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
	  ;; Get the name of the bitmaps
	  ;; #define THENANME_some_attribute
	  (when (null name)
	    (setq name-end (position #\_ line :test #'char= :from-end t)
		  name (read-keyword line 8 name-end))
	    (unless (eq name :image)
	      (setf (getf properties :name) name)))
	  ;; Get the name of the attribute.
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
  
  



;; Support for the xpm version 3 format

(defmethod read-image-file ((format (eql :pixmap-3)) pathname palette)
  (if (streamp pathname)
      (read-xpm-file-1 pathname palette)
    (with-open-file (fstream pathname :direction :input)
      (read-xpm-file-1 fstream palette))))

(defun read-xpm-file-1 (stream palette)
  (labels ((ensure-next-char (c)
	     (assert (eql c (skip-whitespace)) () "Expected ~S" c)
	     (read-char stream))
	   (read-a-token (predicate &optional (stream stream))
	     (skip-whitespace)
	     (let ((chars (make-array 0 :fill-pointer 0 
				      :adjustable t
				      :element-type 'string-char)))
	       (loop
		 (let ((c (peek-char nil stream nil nil)))
		   (unless (and c (funcall predicate c))
		     (return (coerce chars 'simple-string)))
		   (vector-push-extend c chars))
		 (read-char stream))))
	   (skip-comment ()
	     (when (eql #\/ (skip-whitespace))
	       (read-char stream) ; /
	       (read-char stream) ; *
	       (loop
		 (peek-char #\* stream)
		 (read-char stream)
		 (when (eql #\/ (read-char stream))
		   (return)))))
	   (skip-trailing-crap ()
	     (loop
	       (case (skip-whitespace)
		 (#\, (read-char stream))
		 (#\/ (skip-comment))
		 (t (return)))))
	   (read-a-string ()
	     (read stream))
	   (skip-whitespace ()
	     (let (c)
	       (loop
		 (unless  (eql (setq c (peek-char t stream)) #\newline)
		   (return c))))))

    (let (name width height ncolors pixels colors cpp)

      (assert (eql #\/ (skip-whitespace)) () "File must begin with a comment")

      (skip-comment)
      (assert (string= (read-a-token #'alpha-char-p) "static")
	  () "Expected static keyword")
      (assert (string= (read-a-token #'alpha-char-p) "char")
	  () "Expected char keyword" )
      (ensure-next-char #\*)

      (setq name (read-a-token #'(lambda (c) (or (alphanumericp c) (eql c #\_)))))
  
      (ensure-next-char #\[)
      (ensure-next-char #\])
      (ensure-next-char #\=)
      (ensure-next-char #\{)
    
      (skip-comment)
    
      (let ((values (read-a-string)))
	(with-input-from-string (s values)
	  (setq width (read s)
		height (read s)
		ncolors (read s)
		cpp (read s))))
    
      (skip-trailing-crap)

      (let ((array (make-array (list height width))))
	
	(dotimes (i ncolors)
	  (let* ((string (prog1 (read-a-string) (skip-trailing-crap)))
		    (chars (subseq string 0 cpp))
		  (values nil))
	    (with-input-from-string (s string :start cpp)
	      (loop
		(let ((key (read s nil nil)))
		  (when (eq key nil) (return))
		  (assert (member key '(m s g4 g c) :test #'string-equal)
		      () "Expected either m, s, g4, g or . Got ~S" key)
		  (push (cons key
			      (case (peek-char t s)
				(#\# ; rgb
				 (read-char s)
				 (let ((number (read-a-token #'(lambda (c) (digit-char-p c 16)) s)))
				   (assert (= (length number) 12) ()
				     "Expected 12 character hex string. Got ~S" number)
				   (flet ((get-integer (i)
					    (/ (parse-integer number
							   :start (* i 4)
							   :end (* (1+ i) 4)
							   :radix 16)
					       #.(1- (ash 1 16)))))
				     (make-rgb-color
				      (get-integer 0)
				      (get-integer 1)
				      (get-integer 2)))))
				(#\% ; hsv
				 (read-char s)
				 (error "HSV color spec not implemented")
				 )
				(t ; color-name
				 (read s))))
			values))))
	    (assert values () "Expected  key,color for ~S" chars)
	    (push (cons chars values) colors)))
			      
    
	(setq pixels (nreverse pixels))
    
	(dotimes (i height)
	  (let ((string (read-a-string)))
	    (skip-trailing-crap)
	    (dotimes (j width)
	      (setf (aref array i j)
		(position
		 (let ((index (* cpp j)))
		   (subseq string index (+ index cpp)))
		 colors
		 :key #'car
		 :test #'string=)))))
      
	(values array
		(mapcar #'(lambda (name-and-versions)
			    (let ((color (or (cdr (assoc "c" (cdr name-and-versions) :test #'string-equal))
					     (cdr (car (cdr name-and-versions))))))
			      (etypecase color
				(color color)
				(symbol (cond ((string-equal color 'none)
					       +transparent-ink+)
					      (palette 
						(find-named-color color palette))
					      (t color))))))
			colors))))))
