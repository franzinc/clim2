(in-package :tk)

(defclass font (handle-class)
  ((name :initarg :name :reader font-name)))

(defmethod print-object ((x font) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream "~A ~X" (font-name x) (object-handle x))))
(defmethod initialize-instance :after ((f font) &key handle display name)
  (unless handle
    (let ((x (x11:xloadqueryfont
	      (display-handle display)
	      (string-to-char* name))))
      (when (zerop x) (error "Cannot find font: ~S" name))
      (setf (slot-value f 'handle) x))
    (register-address f)))


(defun query-font (display font-id)
  (let ((h (x11:xqueryfont (display-handle display) font-id)))
    (when (zerop h)
      (error "Cannot query font: ~D" font-id))
    (make-instance 'font
		    :display display
		    :handle h)))

(def-c-type (xcharstruct-vector :in-foreign-space) 1 x11:xcharstruct)

(defmethod font-ascent (font)
  (x11:xfontstruct-ascent (object-handle font)))

(defmethod font-descent (font)
  (x11:xfontstruct-descent (object-handle font)))

(defmethod font-all-chars-exist-p (font)
  (x11:xfontstruct-all-chars-exist (object-handle font)))

(defmethod font-range (font)
  (let* ((h (object-handle font))
	 (min-byte-1 (x11:xfontstruct-min-byte1 h))
	 (max-byte-1 (x11:xfontstruct-max-byte1 h)))
    (cond ((and (zerop min-byte-1)
		(zerop max-byte-1))
	   (values (x11:xfontstruct-min-char-or-byte2 h)
		   (x11:xfontstruct-max-char-or-byte2 h))))))

(defmethod char-width (font index)
  (multiple-value-bind
      (min max) (font-range font)
    (if (and (<= min index)
	     (<= index max))
	(xcharstruct-vector-width
	 (x11:xfontstruct-per-char (object-handle font))
	 (- index min)))))

(def-c-type (xfontname-list :in-foreign-space) 1 * :char)

(defun list-font-names (display pattern &key (max-fonts 65535) (result-type 'list))
  (with-ref-par ((n 0))
    (let* ((names (x11:xlistfonts (display-handle display)
				  pattern
				  max-fonts
				  n))
	   (n (aref n 0))
	   (seq (make-sequence result-type n)))
      (prog1
	  (dotimes (i n seq)
	    (setf (elt seq i) (char*-to-string (xfontname-list names i))))
	(x11::xfreefontnames names)))))

(def-c-type (xfontstruct-array :in-foreign-space) 1 x11::xfontstruct)

(defun list-font-names-with-info (display pattern &key (max-fonts 65535) (result-type 'list))
  (with-ref-par ((n 0)
		 (fonts 0))
    (let* ((names (x11:xlistfontswithinfo (display-handle display)
					  pattern
					  max-fonts
					  n
					  fonts))
	   (n (aref n 0))
	   (fonts (aref fonts 0))
	   (seq (make-sequence result-type n)))
      (prog1
	  (dotimes (i n seq)
	    (setf (elt seq i) 
	      (make-instance 'font
			     :handle (xfontstruct-array fonts i)
			     :name (char*-to-string (xfontname-list
						     names i)))))
	(x11:xfreefontnames names)))))

(defun tk::font-property (font which)
  (declare (ignore font which))
  nil)


      

      
    
