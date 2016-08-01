;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM-USER; Base: 10; Lowercase: Yes -*-


(in-package :clim-user)

;;; This file contains a whole bunch of useful graphics and text
;;; output tests that can be used for generating postscript also.

(define-application-frame clim-postscript-tests ()
			  ((printer-type :initarg :printer-type
					 :initform :postscript
					 :reader frame-printer-type))
			   
  (:panes
    (interactor :interactor)
    (display :application))
  (:layouts 
    (default 
      (vertically ()
	(:fill display)
	(1/4 interactor)))))

(define-clim-postscript-tests-command (com-exit-clim-postscript-tests :name t) 
    ()
  (frame-exit *application-frame*))

(defvar *1-inch=points* 72)
(defvar *ps-page-width* 10)
(defvar *ps-page-top* 1)

(defun get-drawing-display-window ()
  (get-frame-pane *application-frame* 'display))


(defmacro with-drawing-display-window ((stream) &body body)
  `(let ((,stream (get-drawing-display-window)))
     (window-clear ,stream)
     (window-expose ,stream)
     ,@body))

(defmacro sending-printer-to-editor-buffer ((stream-var buffer-name &rest options)
					    &body body)
  (declare (ignore options))
  #+Allegro
  (let ((editor-stream-var '#:editor-stream))
    `(lep:with-output-to-temp-buffer (,editor-stream-var ,buffer-name)
       (invoke-with-output-to-printer-stream 
	(frame-printer-type *application-frame*)
	,editor-stream-var
	#'(lambda () (let ((,stream-var *standard-output*)) ,@body))))
    #+Genera
    (let ((editor-stream-var (make-symbol "EDITOR-STREAM")))
      `(with-open-stream (,editor-stream-var
			  (zwei:open-interval-stream (zwei:find-buffer-named ,buffer-name t)))
	 (invoke-with-output-to-printer-stream 
	  (frame-printer-type *application-frame*)
	  ,editor-stream-var
	  #'(lambda () (let ((,stream-var *standard-output*)) ,@body)))))))

(defparameter *postscript-test-buffer-name* "postscript-test")

(defmacro with-printer-test-output-to-buffer ((stream-var &rest options)
						 &body body)
  `(sending-printer-to-editor-buffer (,stream-var *postscript-test-buffer-name*
					 ,@options)
				     ,@body))

#+Genera (zwei:defindentation (define-postscript-test 0 5 1 5 2 1))

#+Allegro
(defvar *allegro-printer* "lw2")

(defvar *postscript-test-default-output* :view)

(defvar *postscript-tests* nil)

(defmacro define-postscript-test
	  ((root-name description &optional (group nil group?)) arguments
	   &body body)
  (let* ((command-name-string (command-name-from-symbol root-name))
	 (command-body-function-name (clim-utils:fintern "~A~A" (string root-name) '-function))
	 (additional-key-args '((output '(member :view :file :buffer #+Allegro :printer)
					:default *postscript-test-default-output*)))
	 (postscript-test-name (clim-utils:fintern "~A~A" 'pcom- (string root-name)))
	 (args arguments)
	 (command-body-form
	   `(,command-body-function-name
	     ,@(loop for arg in args
		     unless (clim-internals::valid-cp-lambda-list-keyword-p arg)
		       collect (if (listp arg) (car arg) arg)))))
    (let ((hook (member '&key args)))
      (if hook
	  (setf (cdr hook) (append additional-key-args (cdr hook)))
	  (setq args (append args '(&key) additional-key-args))))
    `(#+Genera sys:multiple-definition #+Genera ,root-name #+Genera define-postscript-test
      #-Genera progn
      (defun ,command-body-function-name ,(clim-internals::deduce-body-arglist arguments)
	,@body)
      (pushnew ',postscript-test-name *postscript-tests*)
      (define-command (,postscript-test-name
		       :command-table clim-postscript-tests
		       :menu (,command-name-string :documentation ,description)
		       :name ,command-name-string)
	  ,args
	(ecase output
	  (:view
	    (with-drawing-display-window (*standard-output*)
	      ,command-body-form))
	  (:file
	   (send-output-to-file #'(lambda () ,command-body-form)))
	  (:printer 
	   #+Allegro
	   (send-output-to-printer #'(lambda () ,command-body-form))
	    #-Allegro
	    (error "Sorry, don't know how to send to printer"))
	  (:buffer
	   (send-output-to-buffer #'(lambda () ,command-body-form) ,command-name-string))))
      ;; If the CLIM test suite is loaded, define a regular CLIM test as well.
      ,@(when (and (fboundp 'clim-user::define-test) group?)
	  ;; CLIM-USER::DEFINE-TEST doesn't provide for arguments to tests.
	  ;; If there are required arguments, use their specified defaults. 
	  ;; If there is no specified default for a required argument, warn and
	  ;; don't bother with the CLIM-USER::DEFINE-TEST.
	  (let ((command-body-form
		  (do* ((args arguments (cdr args))
			(arg (car args) (car args))
			(form-args nil))
		       ((or (null args) (eq arg '&key))
			(cons command-body-function-name (nreverse form-args)))
		    (unless (consp arg) (return nil))
		    (let* ((none '#:none)
			   (default (getf (cddr arg) ':default none)))
		      (when (eq default none) (return nil))
		      (push default form-args)))))
	    (if command-body-form
		`((clim-user::define-test (,(clim-utils:fintern "~A~A" 'com (string root-name))
					   ,group)
					  (*standard-output*)
		    ,description
		    ,command-body-form))
		(prog1 nil
		       (warn "'~S (~A) can't define a CLIM test with arguments unless defaults are specified"
			     'define-postscript-tests root-name))))))))



(defun send-output-to-file (continuation)
  (let ((type (frame-printer-type *application-frame*)))
    (with-open-file (file-stream (format nil "~A.output" (string-downcase type)) :direction :output :if-exists :supersede)
      (invoke-with-output-to-printer-stream 
       type
       file-stream
       continuation))))

(defun send-output-to-printer (continuation)
  (invoke-with-pipe-to-printer
   (frame-printer-type *application-frame*)
   #'(lambda (printer-stream)
       (invoke-with-output-to-printer-stream 
	(frame-printer-type *application-frame*)
	printer-stream
	continuation))))

(defun send-output-to-buffer (continuation command-name-string)
  (with-printer-test-output-to-buffer
      (*standard-output* :header-comments `(:title ,command-name-string))
    (funcall continuation)))



(defmethod invoke-with-output-to-printer-stream ((type (eql :postscript)) stream continuation)
  (with-output-to-postscript-stream (*standard-output* stream)
    (funcall continuation)))

(defmethod invoke-with-pipe-to-printer ((type (eql :postscript)) continuation)
  (with-open-stream 
      (printer-stream
       (excl:run-shell-command (format nil "lpr -P~A" *allegro-printer*)
			       :input :stream :wait
			       nil))
    (funcall continuation printer-stream)))


(defmacro placing-output ((stream width height) &body body)
  `(let ((output-records nil))
     (macrolet ((placing-output-block (ignore &body body)
		  (declare (ignore ignore))
		  `(with-output-recording-options (,',stream :draw nil :record t)
		     (push (with-new-output-record (,',stream)
			     ,@body)
			   output-records))))
       ,@body
       (place-output-records ,stream output-records ,width ,height))))

(defun place-output-records (stream output-records max-x max-y)
  ;; fit as many of the output records as we can in the space provided.
  ;; return a list of the records which didn't fit.
  (let ((remaining-records (copy-list output-records)))
    (labels ((record-area (record)
	       (multiple-value-bind (width height)
		   (bounding-rectangle-size record)
		 (* width height)))
	     (record-fits (record width height)
	       (multiple-value-bind (w h)
		   (bounding-rectangle-size record)
		 (and (< w width) (< h height))))
	     (maximal-record (records metric max-width max-height)
	       (loop with maximist
		     with its-measure = -1
		     for record in records
		     for measure = (funcall metric record)
		     do (when (and (record-fits record max-width max-height)
				   (> measure its-measure))
			  (setq maximist record
				its-measure measure))
		     finally (return maximist)))
	     (put-record (record x y)
	       (setq remaining-records (delete record remaining-records))
	       (output-record-set-position record x y)
	       (clim-internals::recompute-extent record)
	       (with-output-recording-options (stream :draw t :record nil)
		 (replay record stream nil)))
	     (do-region (offset-x offset-y width height)
	       ;; Find the largest remaining record that will fit in the
	       ;; region and put it.
	       (let ((record (maximal-record remaining-records
					     #'record-area
					     width height)))
		 ;; BOUNDING-RECTANGLE-HEIGHT might be a better choice? 
		 (when record
		   (put-record record offset-x offset-y)
		   (multiple-value-bind (w h)
		       (bounding-rectangle-size record)
		     (do-region (+ offset-x w) offset-y
				(- width w) h)
		     (do-region offset-x (+ offset-y h)
				width (- height h)))))))
      (do-region 0 0 max-x max-y))
    remaining-records))


;;; Simple tests.
;;; More tests in clim:demo;graphics-demos.

;(defmacro ps-test (&body body)
;  `(flet ((doit (stream)
;		(let ((*standard-output* stream))
;		  (progn ,@body))))
;     (ps-test-doer #'doit file)))

;(defun ps-test-doer (doit file)
;  (cond ((eq file t)
;	 (let ((w (symbol-value 'win)))
;	   (window-clear w)
;	   (window-expose w)
;	   (funcall doit w)))
;	((null file)
;	 (with-output-to-postscript-stream (s *standard-output*)
;	   (funcall doit s)))
;	(t
;	 (with-open-file (file file :direction :output)
;	   (with-output-to-postscript-stream (s file)
;	     (funcall doit s))))))

;(defun test-ps-ds (&optional file)  ;"pierced ears"
;  (ps-test
;    (stream-set-cursor-position stream 300 300)
;    (draw-line* stream 100 100 200 200)
;    (with-text-family (:serif stream)
;      (write-string "This string hopefully will be written, ")
;      (write-string "As will this")
;      (write-string " And also this.")
;      (draw-text* stream "Quid fecit Dominus!?!" 200 200
;		    :text-style '(:sans-serif :italic :large)))))

(defvar *hearts-tile*
    (make-rectangular-tile
     (make-pattern #2A((0 1 1 0 1 1 0 0)
		       (1 0 0 1 0 0 1 0)
		       (1 0 0 1 0 0 1 0)
		       (0 1 0 0 0 1 0 0)
		       (0 0 1 0 1 0 0 0)
		       (0 0 0 1 0 0 0 0)
		       (0 0 0 0 0 0 0 0)
		       (0 0 0 0 0 0 0 0))
		   (list +background-ink+ +foreground-ink+))
     8 8))

;(defun pat-ps (&optional file)
;  (ps-test
;    (draw-rectangle* stream 100 100 200 200 :ink *hearts-tile*)))

(define-postscript-test (pattern-test "Draws a heart patterned rectangle"
				      clim-user::graphics)
			()
  (draw-rectangle* *standard-output* 100 100 200 200 :ink *hearts-tile*))

;(defun test-l-ps (&optional file)
;  (ps-test
;    (draw-line* stream 100 100 200 200)))

(define-postscript-test (draw-line-test "Draws a line" )
			()
  (draw-line* *standard-output* 100 100 200 200))

;(defun test-spin (&optional file)
;  (ps-test
;    (with-translation* (stream 150 150)
;      (flet ((draw (stream)
;		   (draw-rectangle* stream 0 0 50 50 :ink +blue+)
;		   #+Ignore
;		   (draw-triangle* stream 50 50 50 75 75 50 :ink +cyan+)
;		   (draw-circle* stream 70 30 20 :filled t
;				  :end-angle 5.00
;				  :ink +cyan+)))
;	(draw-text* stream "Spin!" 0 0)
;	(do ((angle 0 (+ angle (/ pi 4))))
;	    ((> angle (* 2 pi)) nil)
;	(with-rotation (stream angle)
;	  (with-translation* (stream 100 0)
;	    (draw stream))))))))

;(define-postscript-test (spin "???")
;			()
;  (let ((stream *standard-output*))
;    (with-translation* (stream 150 150)
;      (flet ((draw (stream)
;	       (draw-rectangle* stream 0 0 50 50 :ink +blue+)
;	       #+Ignore
;	       (draw-triangle* stream 50 50 50 75 75 50 :ink +cyan+)
;	       (draw-circle* stream 70 30 20 :filled t
;			     :end-angle 5.00
;			     :ink +cyan+)))
;	(draw-text* stream "Spin!" 0 0)
;	(do ((angle 0 (+ angle (/ pi 4))))
;	    ((> angle (* 2 pi)) nil)
;	  (with-rotation (stream angle)
;	    (with-translation* (stream 100 0)
;	      (draw stream))))))))

;(defun spin-pat (file)
;  (ps-test
;    (flet ((draw (stream)
;		 (draw-rectangle* stream 0 0 50 50
;				  :ink *hearts-tile*)
;		 #+Ignore
;		 (draw-triangle* stream 50 50 50 75 75 50 :ink +cyan+)
;		 (draw-circle* stream 70 30 20)))
;      (do ((angle 0 (+ angle (/ pi 4))))
;	  ((> angle (* 2 pi)) nil)
;	(with-rotation (stream angle)
;	(with-translation* (stream 100 0)
;	  (draw stream)))))))

;(defun test-ellipse (&optional file)
;  (ps-test
;    (draw-ellipse* stream 200 200 50 0 0 100 
;		   :start-angle (/ pi 4)
;		   :end-angle (* 5 (/ pi 4))
;		   :filled t)))

(define-postscript-test (test-ellipse "Draw an ellipse")
			()
  (draw-ellipse* *standard-output* 200 200 50 0 0 100 
		 :start-angle (/ pi 4)
		 :end-angle (* 5 (/ pi 4))
		 :filled t))

(define-postscript-test (test-graphics "Test graphics primitives" clim-user::graphics)
			()
  (let ((stream *standard-output*))
    (draw-point* stream 30 30)
    (draw-line* stream 35 30 50 50)
    (draw-rectangle* stream 100 30 200 100 :filled nil)
    (draw-rectangle* stream 120 50 180 80 :filled t)
    (draw-polygon* stream '(60 150 40 170 40 190 60 210 80 210 60 180 80 150)
		   :filled nil)
    (draw-polygon* stream '(120 150 100 170 100 190 120 210 140 210 120 180 140 150)
		   :filled t)
    (draw-ellipse* stream 100 250 -20 0 0 10)))

;(defun ps-test-align (&optional file)
;  (ps-test 
;    (draw-line* stream 100 100 200 100)
;    (draw-text* stream "'ts U, dear align!" 100 100
;		  :align-x :center
;		  :text-style '(:fix :roman :large))
;    (draw-text* stream "'ts U, dear align!" 100 200
;		  :align-x :right
;		  :text-style '(:fix :roman :large))
;    (draw-line* stream 100 50 100 300)))

(define-postscript-test (test-alignment "See if text falls on rules")
			()
  (let ((stream *standard-output*))
    (draw-line* stream 100 100 300 100)
    (draw-text* stream "X Center Aligned" 200 100
		:align-x :center
		:text-style '(:fix :roman :large))
    (draw-line* stream 50 200 250 200)
    (draw-text* stream "X Right Aligned" 200 200
		:align-x :right
		:text-style '(:fix :roman :large))
    (draw-line* stream 200 50 200 300)))

;(defun ps-test-sw (&optional file)
;  (ps-test
;    (draw-point* stream 100 100)
;    (stream-set-cursor-position stream 100 100)
;    (write-string "0123456789" stream)
;    (write-string "Here is some stuff" stream)
;    (write-string " .. and here is some more." stream)))

(define-postscript-test (write-multiple-strings
			  "Test set-cursor-position and writeing of multiple strings and characters" clim-user::graphics)
			()
  (let ((stream *standard-output*)
	(x 100)
	(y 100))
    #+ignore (draw-point* stream 100 100)
    (draw-line* stream (- x 10) y (+ x 10) y)
    (draw-line* stream x (- y 10) x (+ y 10))
    (stream-set-cursor-position stream x y)
    (write-string "0123456789" stream)
    (write-string "Here is some stuff" stream)
    (write-char #\space stream)
    (write-char #\. stream)
    (write-string ". and here is some more." stream)))

;(defun ps-test-ct2 (&optional file)
;  (ps-test
;    (draw-line* stream 100 100 100 300)
;    (draw-text* stream "0123456789" 100 100 :align-x :center)))

;(defun ps-test-dvs (&optional file)
;  (ps-test
;    (draw-line* stream 100 100 100 300)
;    (draw-line* stream 50  200 300 200)
;    (dolist (x '((100 100) (100 300) (50 200) (300 200) (200 200)))
;      (stream-set-cursor-position stream (car x)(cadr x))
;      (format t "(~D, ~D)" (car x)(cadr x)))
;    ;;; ***WHEN DRAW-VERTICAL-STRING(*) is METHODIZED, FIX THIS.
;    ;;;
;    (clim::draw-vertical-string-internal 
;      stream 0 0
;      "Are you on my side, too?" 200 200
;      0 nil :left :baseline '(:fix :roman :very-large) +foreground+)))

;(defun ps-test-dc (&optional file)
;  (ps-test
;    (draw-line* stream 100 100 100 300)
;    (draw-line* stream 50 200 300 200)
;    (draw-character* stream #\A 100 200 :text-style '(:fix :roman :very-large))))

(define-postscript-test (test-character-positioning "Draw a big character"
						    clim-user::graphics)
			()
  (let ((stream *standard-output*)
	(x 100)
	(y 200))
    (draw-line* stream x (- y 100) y (+ y 100))
    (draw-line* stream (- x 50) y (+ x 200) y)
    (draw-text* stream #\A x y :text-style '(:fix :roman :very-large))))

(define-postscript-test (test-text-vertical-alignment
			  "Draw characters and text with fifferent vertical alignments"
			  clim-user::graphics)
			()
  (let ((stream *standard-output*)
	(x 50)
	(y 200)
	(x-incr 100))
    (flet ((text-with-alignment (char string alignment)
	     (let ((x (+ x 5)))
	       (draw-text* stream char x y :align-x :right :align-y alignment)
	       (draw-text* stream string x y :align-x :left :align-y alignment))))
      (draw-line* stream x y 500 y)
      (text-with-alignment #\_ "baseline Ig" :baseline)
      (incf x x-incr)
      (text-with-alignment #\_ "top Ig" :top)
      (incf x x-incr)
      (text-with-alignment #\_ "center -Ig" :center)
      (incf x x-incr)
      (text-with-alignment #\_ "bottom Igy" :bottom))))

;(defun ps-test-table (&optional file)
;  (ps-test
;    (formatting-table ()
;      (dotimes (i 20)
;	(formatting-row ()
;	  (formatting-cell () (format t "~R" i))
;	  (formatting-cell () (format t "~D" i)))))))

(define-postscript-test (test-table "Draw a table" clim-user::formatted-output)
			()
  (let ((stream *standard-output*))
    (fresh-line stream)
    (formatting-table (stream)
      (dotimes (i 20)
	(formatting-row (stream)
	  (formatting-cell (stream) (format stream "~R" i))
	  (formatting-cell (stream) (format stream "~D" i)))))))

;(defun ps-test-text-size (&optional file)
;  (ps-test
;    (draw-line* stream 100 100 300 100)
;    (draw-text* stream "Foobar" 100 100)
;    (draw-line* stream 100 110 300 110)))

(define-postscript-test (test-text-size "Is text 10 units high?")
			()
  (let ((stream *standard-output*))
    (draw-line* stream 100 100 300 100)
    (draw-text* stream "Foobar" 100 100 :align-y :top)
    (draw-line* stream 100 110 300 110)))

(define-postscript-test (test-text-sizes "Output text in several sizes"
					 clim-user::graphics)
			()
  (let ((stream *standard-output*))
    (flet ((out (size)
	     (with-text-size (stream size)
	       (etypecase size
		 (number (format stream "~%~d points~%" size))
		 (symbol (format stream "~%~a~%" (symbol-name size)))))))
      (terpri stream)
      (with-text-style (stream *undefined-text-style*)
	(write-string "undefined style" stream))
      (terpri stream)
      (dolist (size '(6 10 12 14 20))
	(out size))
      (dolist (size silica::*valid-text-style-sizes*)
	(out size)))))

(define-postscript-test (test-text-baselines
			  "Outputs text with differing sizes and families to make sure balelines line up"
			  clim-user::graphics)
			()
  ;; test for both wrtie-string and draw-string
  (let ((stream *standard-output*)
	(sizes '(:very-small :small :normal :large :very-large))
	(families '(:fix :serif :sans-serif))
	(faces '(:roman :bold :italic :bold-italic)))
    ;; for each family, vary size:
    (flet ((loop-loop (outer-list inner-list style-constructor stream how before-row)
	     (dolist (outer outer-list)
	       (funcall before-row stream)
	       (funcall how (format nil "~s:  " outer)
			stream)
	       (dolist (inner inner-list)
		 (with-text-style (stream (funcall style-constructor outer inner))
		   (funcall how (format nil "  _Ig_~s" inner)
			    stream)))))
	   (family-size-constructor (family size)
	     (make-text-style family :roman size))
	   (face-family-constructor (face family)
	     (make-text-style family face :large)))
      (flet ((before-row-for-write (stream)
	       (fresh-line stream)
	       (terpri stream)))
	(with-text-style (stream (make-text-style nil :bold :very-large))
	  (write-string "write-string:" stream))
	(loop-loop families sizes #'family-size-constructor
		   stream #'write-string #'before-row-for-write)
	(before-row-for-write stream)
	(loop-loop faces families #'face-family-constructor
		   stream #'write-string #'before-row-for-write))
      (let ((x 0)
	    (y 300)
	    (x-inc 120)
	    (y-inc 30))
	(flet ((before-row-for-draw (stream)
		 (declare (ignore stream))
		 (incf y y-inc)
		 (setq x 0))
	       (draw-string-how-function (string stream)
		 (draw-text* stream string x y :align-y :baseline)
		 (incf x x-inc)))
	  (with-text-style (stream (make-text-style nil :bold :very-large))
	    (draw-string-how-function "draw-string:" stream))
	  (loop-loop families sizes #'family-size-constructor
		     stream #'draw-string-how-function #'before-row-for-draw)
	  (before-row-for-draw nil)
	  (loop-loop faces families #'face-family-constructor
		     stream #'draw-string-how-function #'before-row-for-draw))))))

(define-postscript-test (test-text-alignment "Text drawing and alignment"
					     clim-user::graphics)
			((sample-text 'string :default "Ignatz"))
  (let* ((align-x-values '(:left :center :right))
	 (align-y-values '(:top :center :baseline :bottom))
	 (stream *standard-output*)
	 (point (make-point 0 0))
	 (crosshair-lines (list (make-point -10 0) (make-point 10 0)
				(make-point 0 -10) (make-point 0 10))))
    (formatting-table (stream :y-spacing 20 :x-spacing 20)
      (formatting-row (stream)
	(formatting-cell (stream)
	  (declare (ignore stream)))
	(with-text-face (stream :bold)
	  (dolist (align-x align-x-values)
	    (formatting-cell (stream)
	      (princ align-x stream)))))
      (dolist (align-y align-y-values)
	(formatting-row (stream)
	  (with-text-face (stream :bold)
	    (formatting-cell (stream)
	      (princ align-y stream)))
	  (dolist (align-x align-x-values)
	    (formatting-cell (stream)
	      (draw-text stream sample-text point :align-x align-x :align-y align-y)
	      (draw-lines stream crosshair-lines))) )) )))

(define-postscript-test (test-transformations "")
			()
  (let ((stream *standard-output*)
	(center (make-point 20 20)))
    (flet ((draw-thing (stream)
	     (draw-polygon* stream '(0 0 0 40 40 40 40 0) :closed t :filled nil)))
      (draw-thing stream)
      (with-drawing-options (stream :transformation (make-translation-transformation 60 60))
	(draw-thing stream))
      (with-drawing-options (stream :transformation (make-translation-transformation 60 140))
	(with-drawing-options (stream :transformation
				      (make-rotation-transformation (/ pi 4) center))
	  (draw-thing stream))))))

(define-postscript-test (test-record-and-replay
			  "Record and output record without drawing and then replay it"
			  clim-user::output-recording)
			()
  (let* ((stream *standard-output*)
	 (output-record (with-output-recording-options (stream :draw nil :record t)
			  (with-new-output-record (stream)
			    (draw-polygon* stream '(0 0 0 40 40 40 40 0)
					   :closed t :filled nil)))))
    (draw-point* stream 100 100)
    (replay output-record stream)))

(define-postscript-test (test-with-room-for-graphics "" clim-user::formatted-output)
			()
  (let ((stream *standard-output*))
    (fresh-line stream)
    (with-room-for-graphics (stream)
      (draw-polygon* stream '(0 0 100 0 100 100) :filled nil :closed t))
    (fresh-line stream)
    (with-room-for-graphics (stream)
      (draw-polygon* stream '(0 0 100 0 100 100 0 100) :filled nil :closed t))))

(defun dash-pattern-name (dash-pattern)
  ;; Return a string describing DASH-PATTERN
  (with-output-to-string (stream)
    (let* ((length (length dash-pattern))
	   (index 0))
      (write-char #\[ stream)
      (when (< index length)
	(loop
	  (format stream "~d" (elt dash-pattern index))
	  (incf index)
	  (when (>= index length) (return))
	  (write-char #\space stream)))
      (write-char #\] stream))))

(define-postscript-test (show-dash-patterns
			  "Show several different dash patterns at different angles"
			  clim-user::graphics)
			()
  (let ((stream *standard-output*)
	(dash-patterns (make-contrasting-dash-patterns 15))
	(angles '(0 5 30 45))
	(output-records nil))
    (labels ((simple-draw-dash-patterns (stream dash-patterns)
	       (do ((index 0 (1+ index))
		    (x 0) (length 100)
		    (y 0 (- y 10)))
		   ((>= index (length dash-patterns)))
		 (let ((dash-pattern (elt dash-patterns index)))
		   (draw-line* stream x y (+ x length) y
			       :line-dashes dash-pattern)
		   (draw-text* stream (dash-pattern-name dash-pattern)
			       (+ x 10 length) y
			       :align-y :center))))
	     (angled-draw-dash-patterns (stream dash-patterns angle)
	       (let ((transformation (make-rotation-transformation angle)))
		 (with-drawing-options (stream :text-size :very-small
					       :line-unit :normal
					       :transformation transformation)
		   (simple-draw-dash-patterns stream dash-patterns)))))
      (dolist (angle angles)
	(let ((angle (* clim-utils:2pi (/ angle 360))))
	  (with-output-recording-options (stream :draw nil :record t)
	    (push (with-new-output-record (stream)
		    (angled-draw-dash-patterns stream dash-patterns angle))
		  output-records)))))
    (place-output-records stream output-records
			  ;;---
			  (* *1-inch=points* *ps-page-width*)
			  (* *1-inch=points* *ps-page-top*))))

(define-postscript-test (show-dash-patterns-some-more
			  "Bogus rainbow effect" clim-user::graphics)
			()
  (let ((stream *standard-output*)
	(dash-patterns (make-contrasting-dash-patterns 16))
	(max-x 400)
	(max-y 700)
	(center-x 115)
	(center-y 115)
	(increment 10)
	(radius 120))
    (flet ((draw-it (center-x center-y dash-pattern)
	     (with-drawing-options (stream :line-dashes dash-pattern
					   :line-unit :normal
					   :text-size :very-small)
	       (draw-text* stream (dash-pattern-name dash-pattern)
			   (+ max-x 5) (- center-y radius)
			   :align-y :center)
	       (draw-line* stream
			   max-x (- center-y radius)
			   center-x (- center-y radius))
	       (draw-circle* stream center-x center-y radius
			     :start-angle (/ pi 2)
			     :end-angle pi
			     :filled nil)
	       (draw-line* stream
			   (- center-x radius) center-y
			   (- center-x radius) max-y))))
      (dotimes (index (length dash-patterns))
	(draw-it (incf center-x increment)
		 (incf center-y increment)
		 (elt dash-patterns index))))))

(define-postscript-test (partial-circle "circle with start angle and end angle in degrees")
			((start-angle '(real 0 360) :default 0)
			 (end-angle '(real 0 360) :default 360))
  (let ((stream *standard-output*)
	(radius 200)
	(center-x 250)
	(center-y 250))
    (draw-circle* stream center-x center-y 3)
    (draw-circle* stream center-x center-y radius
		  :start-angle (* 2 pi (/ start-angle 360))
		  :end-angle (* 2 pi (/ end-angle 360))
		  :filled nil)
    (draw-circle* stream center-x center-y (/ radius 2)
		  :start-angle (/ pi 2)
		  :end-angle pi
		  :filled nil)
    (draw-circle* stream center-x center-y (/ radius 3)
		  :start-angle (- (/ pi 2))
		  :end-angle 0
		  :filled nil)))

(define-postscript-test
  (test-set-1
    "Combination of pattern Test, Test Text Sizes and Test Text Alignment")
  ()
  (let ((stream *standard-output*))
    (placing-output (stream (* *1-inch=points* *ps-page-width*)
			    (* *1-inch=points* *ps-page-top*))
      (placing-output-block () (pattern-test-function))
      (placing-output-block () (test-ellipse-function))
      (placing-output-block () (test-graphics-function))
      (placing-output-block () (test-text-sizes-function))
      (placing-output-block () (test-text-alignment-function "Ignatzy"))
      (placing-output-block () (test-table-function))
;      (placing-output-block () (test-alignment-function))
;      (placing-output-block () (test-text-baselines-function))
      )))

;;; test output borders and margins

;;; test line thickness
