;; -*- mode: common-lisp; package: postscript-clim -*-
;;
;;
;; See the file LICENSE for the full license governing this code.
;;

(in-package :postscript-clim)

(defun read-afm-file (file)
  (labels ((afm-whitespace-char-p (char)
	     (or (eq char #\tab)
		 (eq char #\space)
		 (eq char #\newline)))
	   (read-keyname ()
	     (let ((chars (make-array 0 :element-type 'character
				      :fill-pointer 0 :adjustable t)))
	       (loop
		 (let ((char (read-char)))
		   (when (afm-whitespace-char-p char)
		     (unread-char char)
		     (return-from read-keyname (intern chars :keyword)))
		   (vector-push-extend char chars))))))
    (with-open-file (*standard-input* file :direction :input)
      (let (font-name
	    font-bbox
	    character-metrics
	    fixed-pitch)
	(loop
	  (case (read-keyname)
	    (:|FontName|
	      (peek-char t)
	      (setq font-name (read-keyname))
	      (read-line))
	    (:|IsFixedPitch|
	      (peek-char t)
	      (setq fixed-pitch
		(ecase (read-keyname)
		  (:|true| t)
		  (:|false| nil))))
	    (:|CharWidth|
	      (setq fixed-pitch (read))
	      (read))
	    (:|FontBBox|
	      (setq font-bbox
		(list (read) (read) (read) (read))))
	    (:|StartCharMetrics|
	      (loop
		(read-line)
		(let ((key (read-keyname))
		      code wx name bounding-box)
		  (when (eq key :|EndCharMetrics|) (return nil))
		  (loop
		    (peek-char t)
		    (ecase key
		      (:|L|
			(read-keyname) (peek-char t) (read-keyname))
		      (:|C| (setq code (read)))
		      (:|WX| (setq wx (read)))
		      (:|N| (peek-char t)
			(setq name (read-keyname)))
		      (:|B|
			(setq bounding-box (list (read) (read) (read) (read)))))
		    (assert (eq (peek-char t) #\;))
		    (read-char)
		    (when (eq (peek-char) #\newline)
		      (push (list code wx name bounding-box)
			    character-metrics)
		      (return))
		    (peek-char t)
		    (setq key (read-keyname)))))
	      (return))
	    (t (read-line))))
	(values font-name font-bbox fixed-pitch (nreverse character-metrics))))))

(defun load-metrics-from-afm-file (file)
  (setup-laserwriter-metrics
   (load-metrics-from-afm-file-1 file)))

(defun load-metrics-from-afm-file-1 (file)
  (multiple-value-bind (name bounding-box fixed-pitch character-metrics) (read-afm-file file)
    (let ((height (- (fourth bounding-box) (second bounding-box))))
      `((,(symbol-name name) 1000 ,bounding-box)
	,@(cond ((null fixed-pitch)
		 (mapcan #'(lambda (x)
			     (destructuring-bind (code width name . ignore) x
			       (declare (ignore ignore))
			       (and (plusp code)
				    `((,code ,(round (* 1000 (/ width height))) ,(symbol-name name))))))
			 character-metrics))
		((numberp fixed-pitch)
		 (round (* 1000 (/ fixed-pitch height))))
		(t
		 (round (* 1000 (/ (second (car character-metrics)) height)))))))))

(defmacro load-metrics-from-afm-file-macro (file)
  `(setup-laserwriter-metrics
    ',(load-metrics-from-afm-file-1 file)))

(defmacro load-font-metrics-from-files (&rest files)
  `(progn
     ,@(mapcar #'(lambda (file)
		   `(load-metrics-from-afm-file-macro
		     ,(format nil "/src_fi/tran/sun4/sparc/lib/~A.afm" file)))
	       files)))

#+ignore
(load-font-metrics-from-files
 "Times-Roman"
 "Times-Italic"
 "Times-Bold"
 "Times-BoldItalic"
 "Helvetica"
 "Helvetica-Oblique"
 "Helvetica-Bold"
 "Helvetica-BoldOblique"
 "Courier"
 "Courier-Oblique"
 "Courier-Bold"
 "Courier-BoldOblique"
 "Symbol")

