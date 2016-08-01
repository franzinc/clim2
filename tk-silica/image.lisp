;; See the file LICENSE for the full license governing this code.
;;

(in-package :xm-silica)

;; make-pattern-from-bitmap-file and read-bitmap-file constitute the API

;; write-bitmap-file is partially broken

(defparameter *bitmap-file-types*
    '((:bitmap nil "xbm")
      (:pixmap "xpm")
      (:gif  "gif")
      (:tiff "tiff" "tif")))

(defun make-pattern-from-bitmap-file (file &rest args
				      &key ((:designs supplied-designs))
					   format)
  (unless format
    (let ((type (pathname-type file)))
      (setq format
	(or (car (find-if #'(lambda (x)
			      (member type (cdr x) :test #'equal))
			  *bitmap-file-types*))
	    (error "Unknown bitmap file extension ~S, and no ~S specified"
		   type :format)))))
  (multiple-value-bind (array designs)
      (with-keywords-removed (args args '(:designs))
	(apply #'read-bitmap-file file :format format args))
    (make-pattern array
		  (or designs
		      supplied-designs
		      (list +background-ink+ +foreground-ink+)))))

(defun make-bitmap-file-from-pattern (file pattern &rest args)
  (multiple-value-bind (array designs)
      (decode-pattern pattern)
    (apply #'write-bitmap-file file array :designs designs args)))

;;;

(defparameter *bitmap-search-path* '("/usr/include/X11/bitmaps/"))

(defun read-bitmap-file (pathname &key (format :bitmap) (port (find-port)))
  (declare (type (or pathname string stream) pathname))
  (unless (probe-file pathname)
    (dolist (dir *bitmap-search-path*)
      (let ((p (merge-pathnames pathname dir)))
	(when (probe-file p)
	  (return (setq pathname p))))))
  (let ((palette (and port (port-default-palette port))))
    (read-image-file format pathname palette)))

(defun write-bitmap-file (pathname array &key designs (format :bitmap))
  (write-image-file format pathname array designs))

;;; read-image file methods dispatch to depending on format

;;; X11 bitmap file

(defmethod read-image-file ((format (eql :bitmap)) pathname palette)
  (declare (ignore palette))
  (if (streamp pathname)
      (read-x11-bitmap-file pathname)
    (with-open-file (fstream pathname :direction :input)
      (read-x11-bitmap-file fstream))))

(defmethod write-image-file ((format (eql :bitmap)) pathname array designs)
  (declare (ignore designs))
  (if (streamp pathname)
      (write-x11-bitmap-file pathname array)
    (with-open-file (fstream pathname :direction :output
		     :if-exists :overwrite :if-does-not-exist :create)
      (write-x11-bitmap-file fstream array))))

;;; XPM (X Window System ASCII pixmaps)

(defmethod read-image-file ((format (eql :pixmap)) pathname palette)
  (if (streamp pathname)
      (read-xpm-file pathname palette)
    (with-open-file (fstream pathname :direction :input)
      (read-xpm-file fstream palette))))

(defmethod write-image-file ((format (eql :pixmap)) pathname array designs)
  (if (streamp pathname)
      (write-xpm-file pathname array designs)
    (with-open-file (fstream pathname :direction :output
		     :if-exists :overwrite :if-does-not-exist :create)
      (write-xpm-file fstream array designs))))

;;; XPM version 3

(defmethod read-image-file ((format (eql :pixmap-3)) pathname palette)
  (if (streamp pathname)
      (read-xpm-v3-file pathname palette)
    (with-open-file (fstream pathname :direction :input)
      (read-xpm-v3-file fstream palette))))

;;; Use pbm filters for everything else

(defmethod read-image-file ((format t) pathname palette)
  (multiple-value-bind (format filter)
      (compute-filter-for-bitmap-format format)
    (let* ((tempname (system:make-temp-file-name))
	   (truename (truename pathname))
	   (command (format nil "cat ~A | ~A" tempname filter)))
      (unwind-protect
	  (progn
	    (system:copy-file truename tempname
			      :link t)
	    (with-open-stream (fstream (excl:run-shell-command
					command
					:wait nil
					:output :stream))
	      (handler-case (read-image-file format fstream palette)
		(error (c)
		  (error "Unable to read image file: ~s (from ~s), \"~a\" ~
			while executing ~s." tempname pathname c command)))))
	(sys:os-wait)
	(delete-file tempname)))))

(defmethod write-image-file ((format t) pathname array designs)
  (multiple-value-bind (format read-filter filter)
      (compute-filter-for-bitmap-format format)
    (declare (ignore read-filter))
    ;; copied from code/streamc.cl - basically a truename but without
    ;; the probe-file
    (let* ((tempname (system:make-temp-file-name))
	   (truename (translate-logical-pathname
		      (merge-pathnames (pathname pathname))))
	   (command (format nil "~A > ~A" filter tempname)))
      (unwind-protect
	  (progn
	    (with-open-stream (fstream (excl:run-shell-command
					command
					:wait nil
					:input :stream))
	      (handler-case (write-image-file format fstream array designs)
		(error (c)
		  (error "Unable to write image file: ~s (from ~s), \"~a\" ~
			while executing ~s." tempname pathname c command))))
	    (system:copy-file tempname truename))
	(sys:os-wait)
	(delete-file tempname)))))
	 

(defmethod compute-filter-for-bitmap-format (format)
  (error "Dont know how to convert from the format ~A" format))

(defmethod compute-filter-for-bitmap-format ((format (eql :gif)))
  (values :pixmap-3 "giftopnm | ppmtoxpm" "xpmtoppm | ppmtogif"))

(defmethod compute-filter-for-bitmap-format ((format (eql :tiff)))
  (values :pixmap-3 "tifftopnm | ppmtoxpm" "xpmtoppm | pnmtotiff"))

;;;

(defun read-x11-bitmap-file (fstream)
  ;; Creates an image from a C include file in standard X11 format
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
			(dpb byte
			     (byte 8 offset-in-pixel)
			     (aref data i pixel-offset))))))))))

	(values data)))))

(defconstant +bytes-per-output-line+ 12)

(defun write-x11-bitmap-file (fstream array)
  (let ((byte-count 0)
	(height (array-dimension array 0))
	(width (array-dimension array 1)))
    (format fstream "#define noname_width ~D~%" width)
    (format fstream "#define noname_height ~D~%" height)
    (format fstream "static char noname_bits[] = {")
    (flet ((output-byte (byte)
	     (format fstream
		     (cond
		      ((zerop byte-count) "~%   ")
		      ((zerop (mod byte-count +bytes-per-output-line+)) ",~%   ")
		      (t ", ")))
	     (incf byte-count)
	     (format fstream "0x~2,'0X" byte)))
      (dotimes (i height)
	(let ((byte 0)
	      (bit 0))
	  (dotimes (j width)
	    (let ((pixel (aref array i j)))
	      (assert (< pixel 2) ()
		"~s format expects only two colors, try using ~s format"
		:bitmap :pixmap)
	      (setf (ldb (byte 1 bit) byte) pixel)
	      (incf bit)
	      (when (eql bit 8)
		(output-byte byte)
		(setq byte 0 bit 0))))
	  (unless (zerop bit)
	    (output-byte byte))))
      (format fstream "};~%"))))

(defun read-xpm-file (fstream palette)
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
      ;;--- char-code-limit is 2^16, but the chars in xpm palettes will
      ;;--- always be printable ascii.  use of char-code-limit here was
      ;;--- creating prohibitively large arrays.  -tjm 3Mar97
      (let* ((xpm-char-code-limit 256)
	     (codes (make-array (expt xpm-char-code-limit chars-per-pixel)))
	     (designs
	      (do ((color-specs (read-strings) (cddr color-specs))
		   (designs nil)
		   (index 0 (1+ index)))
		  ((null color-specs) (nreverse designs))
		(let ((code (first color-specs))
		      (color (convert-color (second color-specs)))
		      (key 0))
		  (dotimes (i chars-per-pixel)
		    (setq key (+ (char-code (schar code i))
				 (* key xpm-char-code-limit))))
		  (setf (svref codes key) index)
		  (push color designs))))
	     (array (make-array (list height width)))
	     (pixels (read-strings))
	     (i 0))
	(declare (type (simple-array t (* *)) array))
	(excl::fast
	 (dolist (row pixels)
	   (let ((index 0))
	     (dotimes (j width)
	       (let ((key 0))
		 (dotimes (i chars-per-pixel)
		   (setq key (+ (char-code (schar row index))
				(* key xpm-char-code-limit)))
		   (incf index))
		 (setf (aref array i j) (svref codes key)))))
	   (incf i)))
	(values array designs)))))

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

(defun write-xpm-file (fstream array designs)
  (let* ((ncolors (length designs))
	 (chars-per-pixel (ceiling (log ncolors 36)))
	 (codes (make-array ncolors))
	 (height (array-dimension array 0))
	 (width (array-dimension array 1)))
    (format fstream "#define noname_format 1~%")
    (format fstream "#define noname_width ~D~%" width)
    (format fstream "#define noname_height ~D~%" height)
    (format fstream "#define noname_ncolors ~D~%" ncolors)
    (format fstream "#define noname_chars_per_pixel ~D~%" chars-per-pixel)
    (format fstream "static char *noname_colors[] = {")
    (dotimes (i ncolors)
      (unless (zerop i)
	(write-char #\, fstream))
      (let* ((code (format nil "~36,V,'0R" chars-per-pixel i))
	     (color (elt designs i))
	     (color-name (cond
			  ((eql color +nowhere+) "none")
			  (t (multiple-value-bind (red green blue)
				 (color-rgb color)
			       (let ((x #.(1- (ash 1 16))))
				 (format nil "#~4,'0X~4,'0X~4,'0X"
					 (truncate (* x red))
					 (truncate (* x green))
					 (truncate (* x blue)))))))))
	(format fstream "~%   ~S, ~S" code color-name)
	(setf (svref codes i) code)))
    (format fstream "};~%")
    (format fstream "static char *noname_pixels[] = {")
    (dotimes (i height)
      (unless (zerop i)
	(write-char #\, fstream))
      (terpri fstream)
      (write-char #\" fstream)
      (dotimes (j width)
	(let ((code (svref codes (aref array i j))))
	  (write-string code fstream)))
      (write-char #\" fstream))
    (format fstream "};~%")))



;; Support for the xpm version 3 format

(defun read-xpm-v3-file (stream palette)
  (labels ((ensure-next-char (c)
	     (assert (eql c (skip-whitespace)) () "Expected ~S" c)
	     (read-char stream))
	   (read-a-token (predicate &optional (stream stream))
	     (skip-whitespace)
	     (let ((chars (make-array 0 :fill-pointer 0
				      :adjustable t
				      :element-type 'character)))
	       (loop
		 (let ((c (peek-char nil stream nil nil)))
		   (unless (and c (funcall predicate c))
		     (return (coerce chars 'simple-string)))
		   (vector-push-extend c chars))
		 (read-char stream))))
	   (skip-comment ()
	     (when (eql #\/ (skip-whitespace))
	       (read-char stream)	; /
	       (read-char stream)	; *
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

    (let (width height ncolors pixels colors cpp)

      (assert (eql #\/ (skip-whitespace)) () "File must begin with a comment")

      (skip-comment)
      (assert (string= (read-a-token #'alpha-char-p) "static")
	  () "Expected static keyword")
      (assert (string= (read-a-token #'alpha-char-p) "char")
	  () "Expected char keyword" )
      (ensure-next-char #\*)

      (read-a-token #'(lambda (c) (or (alphanumericp c) (eql c #\_))))

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
				(#\#	; rgb
				 (read-char s)
				 (let* ((number (read-a-token #'(lambda (c) (digit-char-p c 16)) s))
					(color-string-length (length number)))
				   (assert (or (= color-string-length 12)
					       (= color-string-length 6)) ()
				     "Expected 6 or 12 character hex string. Got ~S" number)
				   (let ((comp-length (/ color-string-length 3)))
				     (flet ((get-integer (i)
					      (/ (parse-integer number
								:start (* i comp-length)
								:end (* (1+ i) comp-length)
								:radix 16)
						 (if (= comp-length 2) 255 65535))))
				       (make-rgb-color
					(get-integer 0)
					(get-integer 1)
					(get-integer 2))))))
				(#\%	; hsv
				 (read-char s)
				 (error "HSV color spec not implemented")
				 )
				(t	; color-name
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
			    (let ((color (or (cdr (assoc "c" (cdr name-and-versions)
							 :test #'string-equal))
					     (cdr (car (cdr name-and-versions))))))
			      (etypecase color
				(color color)
				(symbol (cond ((string-equal color 'none)
					       +transparent-ink+)
					      (palette
					       (find-named-color (string color) palette))
					      (t color))))))
			colors))))))

