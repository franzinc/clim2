;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: hpgl-clim; Base: 10; Lowercase: Yes -*-

;; See the file LICENSE for the full license governing this code.

(in-package :hpgl-clim)

;;; definitions of plotters

(defstruct (hp-ploter-information
	     (:conc-name hpi-)
	     (:print-function (lambda (hpi stream depth)
				(declare (ignore depth))
				(print-unreadable-object (hpi stream :type t :identity t)
				  (format stream "~A" (hpi-name hpi))))))
  name
  outside-width
  outside-height
  left-margin
  top-margin
  right-margin
  bottom-margin
  device-scale-factor  ;; pixels per inch
  metric-scale-factor
  font-table)

#+Genera (scl:defprop define-hp-ploter "HP ploter description"
		      si:definition-type-name)

(defmacro define-hp-ploter (name &body options)
  (let ((like nil)
	(outside-width nil)
	(outside-height nil)
	(margins nil)
	(left-margin nil)
	(top-margin nil)
	(right-margin nil)
	(bottom-margin nil)
	(device-scale-factor nil)
	(metric-scale-factor nil))
    (dolist (option options)
      (multiple-value-bind (option value value-p)
	  (if (listp option)
	      (values (car option) (cdr option) t)
	      (values option nil nil))
	(macrolet ((option-error (format-string &rest format-args)
		     `(warn "For ~S ~S: ~@?." 'define-hp-ploter name ,format-string
			    option ,@format-args))
		   (one-value () `(if value-p
				      (progn (when (cdr value)
					       (option-error "Option ~S only takes one value"))
					     (first value))
				      (progn (option-error "No value supplied for option ~S")
					     1)))
		   (float-one-value () `(float (one-value) 1.0s0))
		   (no-values () `(when value (option-error "Option ~S takes no values")))
		   (n-values () `(if value-p value 
				     (progn (option-error "No values supplied for option ~S")
					    nil))))
	  (case option
	    (:like (setf like (one-value)))
	    (:width (setf outside-width (float-one-value)))
	    (:height (setf outside-height (float-one-value)))
	    (:margins (setf margins (float-one-value)))
	    (:left-margin (setf left-margin (float-one-value)))
	    (:top-margin (setf top-margin (float-one-value)))
	    (:right-margin (setf right-margin (float-one-value)))
	    (:bottom-margin (setf bottom-margin (float-one-value)))	
	    (:device-scale-factor (setf device-scale-factor (float-one-value)))	
	    (:metric-scale-factor (setf metric-scale-factor (float-one-value)))
	    (otherwise (option-error "Option ~S not recognized"))))))
    `(define-group ,name define-hp-ploter
       (define-hp-ploter-load-time
	 ',name
	 :like ',like
	 :outside-width ',outside-width
	 :outside-height ',outside-height
	 :left-margin ',(or left-margin margins)
	 :top-margin ',(or top-margin margins)
	 :right-margin ',(or right-margin margins)
	 :bottom-margin ',(or bottom-margin margins)
	 :device-scale-factor ',device-scale-factor
	 :metric-scale-factor ',metric-scale-factor))))

(defvar *hp-ploter-descriptions* nil)

(defun find-hp-ploter-description (name &key (if-does-not-exist :error))
  (or (and (typep name 'hp-ploter-information) name)
      (find name *hp-ploter-descriptions* :key #'hpi-name)
      (ecase if-does-not-exist
	(:error (error "Can't find hp ploter description ~S" name))
	(:create (merge-hp-ploter-information
		   (make-hp-ploter-information :name name :font-table nil)
		   (find-hp-ploter-description :default :if-does-not-exist nil)))
	((nil) nil))))

(defun hpi-device-margins (hpi)
  (let ((device-scale-factor (hpi-device-scale-factor hpi)))
    (values (* (hpi-left-margin hpi) device-scale-factor)
	    (* (hpi-top-margin hpi) device-scale-factor)
	    (* (hpi-right-margin hpi) device-scale-factor)
	    (* (hpi-bottom-margin hpi) device-scale-factor))))

(defun hpi-device-inside-edges (hpi)
  (let* ((left   (- (hpi-left-margin hpi) (/ (hpi-outside-height hpi) 2)))
	 (top    (- (hpi-top-margin hpi)  (/ (hpi-outside-width hpi) 2)))
	 (right  (- (/ (hpi-outside-height hpi) 2) (hpi-right-margin hpi)))
	 (bottom (- (/ (hpi-outside-width hpi) 2) (hpi-bottom-margin hpi)))
	 (scale-factor (hpi-device-scale-factor hpi)))
    (values (round (* left scale-factor)) (round (* top scale-factor))
	    (round (* right scale-factor)) (round (* bottom scale-factor)))))

(defun hpi-device-outside-edges (hpi)
  (let ((scale-factor (hpi-device-scale-factor hpi)))
    (values 0 0
	    (round (* (hpi-outside-width hpi) scale-factor))
	    (round (* (hpi-outside-height hpi) scale-factor)))))

(defun hpi-device-metric-scale-factor (hpi)
  (* (hpi-device-scale-factor hpi) (hpi-metric-scale-factor hpi)))

(defun hpi-metric-margins (hpi)
  (let ((metric-scale-factor (hpi-metric-scale-factor hpi)))
    (values (* (hpi-left-margin hpi) metric-scale-factor)
	    (* (hpi-top-margin hpi) metric-scale-factor)
	    (* (hpi-right-margin hpi) metric-scale-factor)
	    (* (hpi-bottom-margin hpi) metric-scale-factor))))

(defun define-hp-ploter-load-time (name &key
						 like
						 outside-width
						 outside-height
						 left-margin
						 top-margin
						 right-margin
						 bottom-margin
						 device-scale-factor
						 metric-scale-factor)
  (let ((hpi (find-hp-ploter-description name :if-does-not-exist nil)))
    (unless hpi
      (push 
       (setf hpi (find-hp-ploter-description name :if-does-not-exist :create))
	*hp-ploter-descriptions*))
    (when like
      (merge-hp-ploter-information
	hpi (find-hp-ploter-description like)))
    (setf (hpi-outside-width hpi) (or outside-width (hpi-outside-width hpi) 0.0)
	  (hpi-outside-height hpi) (or outside-height (hpi-outside-height hpi) 0.0)
	  (hpi-left-margin hpi) (or left-margin (hpi-left-margin hpi) 0.0)
	  (hpi-top-margin hpi) (or top-margin (hpi-top-margin hpi) 0.0)
	  (hpi-right-margin hpi) (or right-margin (hpi-right-margin hpi) 0.0)
	  (hpi-bottom-margin hpi) (or bottom-margin (hpi-bottom-margin hpi) 0.0)
	  (hpi-device-scale-factor hpi) (or device-scale-factor
					    (hpi-device-scale-factor hpi) 0.0)
	  (hpi-metric-scale-factor hpi)
	    (or metric-scale-factor (hpi-metric-scale-factor hpi) 1.0)))
  name)

(defun merge-hp-ploter-information (into-hpi from-hpi)
  (when from-hpi
    ;; Copy data from one PPI into another.
    (setf (hpi-outside-width into-hpi) (hpi-outside-width from-hpi)
	  (hpi-outside-height into-hpi) (hpi-outside-height from-hpi)
	  (hpi-left-margin into-hpi) (hpi-left-margin from-hpi)
	  (hpi-top-margin into-hpi) (hpi-top-margin from-hpi)
	  (hpi-right-margin into-hpi) (hpi-right-margin from-hpi)
	  (hpi-bottom-margin into-hpi) (hpi-bottom-margin from-hpi)
	  (hpi-device-scale-factor into-hpi) (hpi-device-scale-factor from-hpi)
	  (hpi-metric-scale-factor into-hpi) (hpi-metric-scale-factor from-hpi)
	  ;; Merge font table
	  (hpi-font-table into-hpi) (append (hpi-font-table into-hpi)
					    (hpi-font-table from-hpi))))
  into-hpi)

;;; Size A is 8-1/2 by 11 inch paper.
(define-hp-ploter :8.5x11
  (:width 8.5)
  (:height 11)
  (:margins 1.0)
  (:device-scale-factor 1016.0)
  (:metric-scale-factor 25.4))

;;; Size B is 11 by 17 inch paper
(define-hp-ploter :11x17
  (:width 11)
  (:height 17)
  (:margins 1.0)
  (:device-scale-factor 1016.0)
  (:metric-scale-factor 25.4))

;;; Size C is 17 by 22 inch paper
(define-hp-ploter :17x22
  (:width 17)
  (:height 22)
  (:margins 1.0)
  (:device-scale-factor 1016.0)
  (:metric-scale-factor 25.4))

;;; Size D is 22 by 34 inch paper
(define-hp-ploter :22x34
  (:width 22)
  (:height 34)
  (:margins 1.0)
  (:device-scale-factor 1016.0)
  (:metric-scale-factor 25.4))

;;; Size E is 34 by 44 inch paper
(define-hp-ploter :34x44
  (:width 34)
  (:height 44)
  (:margins 1.0)
  (:left-margin 1.8)
  (:right-margin 1.8)
  (:device-scale-factor 1016.0)
  (:metric-scale-factor 25.4))

;;; The default size is E, for now.
(define-hp-ploter :default
  (:like :34x44))

#+ignore
(progn
  (define-hp-ploter :a3
      (:width 16)
    (:height 11)
    (:margins 1.0)
    (:left-margin 1.8)
    (:right-margin 1.8)
    (:device-scale-factor 1016.0)
    (:metric-scale-factor 25.4))
  
  (define-hp-ploter :default
      (:like :a3))
  )


;;; Basic port definition

(defclass hpgl-port (basic-port)
	  ((stream-realizer :accessor port-stream-realizer)
	   (stream-closer)
	   (port-matcher)
	   (printer-description :accessor port-printer-description)
	   (color-printer-p :accessor port-color-printer-p :initform nil)
	   (hpi :accessor port-hpi)))

(defmethod port-type ((port hpgl-port))
  ':hpgl)

(defmethod initialize-instance :after ((port hpgl-port) &key device-type)
  (setf (port-hpi port) (find-hp-ploter-description device-type))
  (initialize-hpgl-display-device port)
  (with-slots (silica::default-palette) port
    (setf silica::default-palette (make-palette port))))

(defmethod make-medium ((port hpgl-port) sheet)
  (make-instance 'hpgl-medium
    :port port
    :sheet sheet))

(defmethod restart-port ((port hpgl-port))
  ;; We don't need no stinking events...
  )

;;--- Eventually do better than this
(defclass hpgl-palette (basic-palette) ())

(defmethod make-palette ((port hpgl-port) &key)
  (make-instance 'hpgl-palette
    :port port 
    :color-p t
    :dynamic-p nil))


(defmethod realize-graft ((port hpgl-port) (graft standard-graft))
  (with-slots (silica::mm-height silica::mm-width
		 silica::pixel-height silica::pixel-width
		 silica::pixels-per-point) 
      graft
    (let ((hpi (port-hpi port)))
      ;;--- In HPGL pixels are points
      (setf silica::pixel-width    (* (hpi-outside-width hpi) 72)
	    silica::pixel-height   (* (hpi-outside-height hpi) 72)
	    silica::mm-width	   (* (hpi-outside-width hpi) (hpi-metric-scale-factor hpi))
	    silica::mm-height      (* (hpi-outside-height hpi) (hpi-metric-scale-factor hpi))
	    silica::pixels-per-point 1)
      
      (setf (sheet-region graft)
	(ecase (graft-units graft)
	  (:device (make-rectangle* 0 0 silica::pixel-width silica::pixel-height))
	  (:mm    (make-rectangle* 0 0 silica::mm-width silica::mm-height))
	  (:homogenous (make-rectangle* 0.0 0.0 1.0 1.0))))

      (setf (sheet-native-transformation graft) +identity-transformation+)
      ;;  (setf (sheet-mirror graft) (realize-mirror port graft)) ;;Is the mirror already realized?
      ;;(update-native-transformation port graft)
      )))



;;; Hpgl font hacking.

(defstruct (hpgl-font-family
	     (:conc-name pfam-)
	     (:print-function (lambda (pfam stream depth)
				(declare (ignore depth))
				(print-unreadable-object (pfam stream :type t)
				  (format stream "~A [~S]"
					  (pfam-name pfam)
					  (pfam-text-family pfam))))))
  name
  text-family
  (faces nil))

(defstruct (hpgl-font-face
	     (:conc-name pface-)
	     (:print-function (lambda (pface stream depth)
				(declare (ignore depth))
				(print-unreadable-object (pface stream :type t)
				  (format stream "~A (~A.~A.*)"
					  (pface-name pface)
					  (pfam-text-family (pface-family pface))
					  (pface-text-face pface))))))
  family
  text-face
  name
  (size-table (make-array 20. :initial-element nil))
  (width-table nil)
  ;; These are correct for Courier:
  (height 0.951)
  (ascent 0.7))

(defstruct (hpgl-font-descriptor
	     (:conc-name pfd-)
	     (:print-function (lambda (pfd stream depth)
				(declare (ignore depth))
				(print-unreadable-object (pfd stream :type t)
				  (format stream "\"~D point ~A\"~@[ (~S)~]"
					  (pfd-point-size pfd)
					  (pface-name (pfd-face pfd))
					  (pfd-logical-size pfd))))))
  face
  point-size
  logical-size
  (cached-pretty-name nil))

(defvar *hpgl-font-families* nil)

(defun find-hpgl-font-family (text-family)
  (find text-family *hpgl-font-families* :key #'pfam-text-family))

(defun find-hpgl-font-face (pfam text-face)
  (getf (pfam-faces pfam) text-face))

(defun find-hpgl-font-logical-size (pface logical-size)
  (flet ((pfd-logical-size-no-error (pfd)
	   (if pfd
	       (pfd-logical-size pfd)
	       '#:this-is-not-in-the-table)))
    (find logical-size (pface-size-table pface) :key #'pfd-logical-size-no-error)))

(defun find-hpgl-font-point-size (pface point-size)
  (let ((size-table (pface-size-table pface)))
    (and (integerp point-size)
	 (< 0 point-size (length size-table))
	 (aref size-table point-size))))

(defun find-hpgl-font-descriptor (text-style)
  (multiple-value-bind (family face size)
      (text-style-components (parse-text-style text-style))
    (let ((pfam (find-hpgl-font-family family)))
      (when (null pfam) (return-from find-hpgl-font-descriptor nil))
      (let ((pface (find-hpgl-font-face pfam face)))
	(when (null pface) (return-from find-hpgl-font-descriptor nil))
	(etypecase size
	  (symbol (find-hpgl-font-logical-size pface size))
	  ;; Always legal to introduce a new numerical size.
	  ;; --- Should sizes be only integers?  
	  ;; --- They have to be for now; they're array indices.
	  ((integer 1) (or (find-hpgl-font-point-size pface size)
			   (create-hpgl-font-size pface size nil))))))))

(defun pfd-pretty-name (pfd)
  (or (pfd-cached-pretty-name pfd)
      (setf (pfd-cached-pretty-name pfd)
	      (format nil "~A-~D" (pface-name (pfd-face pfd)) (pfd-point-size pfd)))))


;;; Initialization of font tables.

(defparameter *psftd-keywords* '(:tiny :very-small :small :normal :large :very-large :huge))

(defun create-hpgl-font-family (text-family name)
  (let ((pfam (find-hpgl-font-family text-family)))
    (unless pfam
      (push (setf pfam (make-hpgl-font-family :name name :text-family text-family))
	    *hpgl-font-families*))
    pfam))

(defun create-hpgl-font-face (pfam text-face face-name sizes)
  (let ((pface (getf (pfam-faces pfam) text-face)))
    (if (null pface)
	(setf pface (make-hpgl-font-face
		      :family pfam :text-face text-face :name face-name)
	      (getf (pfam-faces pfam) text-face) pface)
	(setf (pface-name pface) face-name))
    (mapcar #'(lambda (logical-size point-size)
		(create-hpgl-font-size pface point-size logical-size))
	    *psftd-keywords* sizes)
    pface))

(defun create-hpgl-font-size (pface point-size logical-size)
  (check-type point-size (integer 1))
  (let ((size-table (pface-size-table pface)))
    (when (>= point-size (length size-table))
      (setf size-table (replace (make-array (+ point-size 10.)) size-table)
	    (pface-size-table pface) size-table))
    (let ((pfd (aref size-table point-size)))
      (when (null pfd)
	(when (and logical-size
		   (setf pfd (find-hpgl-font-logical-size pface logical-size)))
	  (error "Trying to create multiple sizes for same logical size ~S.~S.~S: ~D and ~D"
		 (pfam-text-family (pface-family pface))
		 (pface-text-face pface)
		 logical-size
		 point-size
		 (pfd-point-size pfd)))
	(setf pfd (make-hpgl-font-descriptor :face pface :point-size point-size
						   :logical-size logical-size)
	      (aref size-table point-size) pfd))
      pfd)))

(defparameter *hpgl-font-translate-data*
	      '(("Courier" :fix (4 6 7 9 11 14)
		 (:roman .6 "Courier")
		 (:bold .6 "Courier-Bold")
		 (:italic .6 "Courier-Oblique")
		 ((:bold :italic) .6 "Courier-BoldOblique"))
		("Helvetica" :sans-serif (5 7 8 10 12 16)
		 (:roman .6 "Helvetica")
		 (:bold .6 "Helvetica-Bold")
		 (:italic .6 "Helvetica-Oblique")
		 ((:bold :italic) .6 "Helvetica-BoldOblique"))
		("Times" :serif (5 7 8 10 12 16)
		 (:roman .6 "Times-Roman")
		 (:bold .6 "Times-Bold")
		 (:italic .6 "Times-Italic")
		 ((:bold :italic) .6 "Times-BoldItalic"))))

(defmethod text-style-mapping :around ((device hpgl-port) text-style &optional 
							  (character-set *standard-character-set*)
							  window)
  (declare (ignore window))
  (let ((pfd (call-next-method)))
    (when (null pfd)
      (setq text-style (standardize-text-style device (parse-text-style text-style) character-set))
      (setf pfd (find-hpgl-font-descriptor text-style))
      (when (null pfd) (return-from text-style-mapping nil))
      (setf (text-style-mapping device text-style character-set) pfd))
    (when (null (pface-width-table (pfd-face pfd)))
      (read-hpgl-font-width-table (pfd-face pfd)))
    pfd))

(defun read-hpgl-font-width-table (x)
  (declare (ignore x))
  (nyi))

(defmethod text-style-mapping-exists-p :around ((device hpgl-port) text-style &optional 
								   (character-set *standard-character-set*)
								   window)
  (declare (ignore window))
  (or (call-next-method)
      (let (pfd)
	(setq text-style (standardize-text-style device (parse-text-style text-style) character-set))
	(setf pfd (find-hpgl-font-descriptor text-style))
	(when (null pfd) (return-from text-style-mapping-exists-p nil))
	(setf (text-style-mapping device text-style character-set) pfd)
	pfd)))
      
(defmethod initialize-hpgl-display-device ((display-device hpgl-port))
  (dolist (pftd *hpgl-font-translate-data*)
    (let* ((family-name (pop pftd))
	   (text-family (pop pftd))
	   (text-sizes (pop pftd))
	   (pfam (create-hpgl-font-family text-family family-name)))
      (dolist (face-stuff pftd)
	(let* ((text-face (pop face-stuff))
	       (width-table (pop face-stuff))
	       (font-name (pop face-stuff))
	       (text-style (make-text-style text-family text-face nil))
	       (canonical-face-code (text-style-face text-style))
	       (pface (create-hpgl-font-face pfam canonical-face-code
					     font-name text-sizes)))
	  ;; Might only be the name of a width table:
	  (setf (pface-width-table pface) width-table)
	  (dolist (text-size text-sizes)
	    (let* ((text-style (make-text-style text-family text-face text-size))
		   (pfd (find-hpgl-font-descriptor text-style)))
	      (setf (text-style-mapping display-device
					text-style) pfd)))))
      
      (dolist (family *hpgl-font-translate-data* (error "dont know how to set up undefined-text-style"))
	(when (text-style-mapping-exists-p display-device `(,(second family) :roman 10))
	  (setf (text-style-mapping display-device *undefined-text-style*) 
	    (make-text-style (second family) :roman 10))
	  (return nil))))))
	

(defparameter *hpgl-logical-size-alist*
	      '((:tiny       6)
		(:very-small 8)
		(:small	     10)
		(:normal     12)
		(:large	     14)
		(:very-large 18)
		(:huge	     24)))

(defmethod standardize-text-style ((port hpgl-port) style
				   &optional (character-set *standard-character-set*))
  (standardize-text-style-1
    port style character-set *hpgl-logical-size-alist*))


;;; Top level interface

(defmacro with-output-to-hpgl-stream ((stream-var file-stream &rest args) &body body)
  (declare (arglist (stream-var file-stream
		     &key (device-type 'apple-laser-writer))
		    &body body))
  `(flet ((hpgl-output-body (,stream-var) ,@body))
     (declare (dynamic-extent #'hpgl-output-body))
     (invoke-with-output-to-hpgl-stream ,file-stream #'hpgl-output-body ,@args)))

;; This could really be WITH-OPEN-STREAM, but that isn't going to call CLIM:CLOSE.
;; Fixed in the CLOS stream system.

(defun invoke-with-output-to-hpgl-stream (file-stream continuation
					  &key (device-type ':default))
  (let* ((port (make-instance 'hpgl-port :device-type device-type))
	 (stream (make-instance 'hpgl-stream))
	 (abort-p t))
    (setf (port stream :graft (find-graft :port port)) port)
    (let* ((medium (sheet-medium stream))
	   (hpi (port-hpi (port medium))))
      (setf (slot-value medium 'printer-stream) file-stream)
      (multiple-value-bind (left top right bottom) (hpi-device-inside-edges hpi)
	(multiple-value-bind (max-x max-y)
	    (progn
	      ;;(translate printer-stream left bottom)
	      (let ((scale-factor (/ (hpi-device-scale-factor hpi) 72.0)))
		(values (/ (- right left) scale-factor)
			(/ (- bottom top) scale-factor))))
	  ;;--- Amazingly enough the result of all of this to set up a
	  ;;--- top-left coordinate system where a device unit is
	  ;;--- actually a point.
	  ;;--- Of course this means that the values in realize graft
	  ;;-- are wrong
	  #+ignore
	  (setf (sheet-device-transformation stream)
	    (make-3-point-transformation*
	     0 0      0 max-y max-x max-y
	     0 max-y  0 0     max-x 0))
	  (with-output-recording-options (stream :record t :draw nil)
	    (unwind-protect
		(multiple-value-prog1
		    (funcall continuation stream)
		  (force-output stream)
		  (let ((string
			 (with-output-to-string (string-stream)
			   (letf-globally (((slot-value medium 'printer-stream) string-stream))
			     (with-output-recording-options (stream :record nil :draw t)
			       (stream-replay stream nil))))))
		    (hpgl-prologue medium)
		    (input-points (slot-value medium 'printer-stream) left bottom right top)
		    (scale (slot-value medium 'printer-stream) 0 max-x 0 max-y)
		    (write-string string (slot-value medium 'printer-stream)))
		  (setq abort-p nil))
	      (close stream :abort abort-p)
	      (destroy-port port))))))))


;;; Hpgl stream definition

;;;--- These definitions could be shared with the postscript 

(defclass hpgl-stream
	  (sheet-permanently-enabled-mixin
	   permanent-medium-sheet-output-mixin
	   sheet-transformation-mixin
	   clim-internals::graphics-output-recording
	   clim-internals::output-recording-mixin
	   clim-internals::output-protocol-mixin
	   basic-sheet)
	  ((multi-page :initform nil :initarg :multi-page)
	   ;; Need this for hacking "scrolling" of multi-page output
	   (device-transformation :accessor sheet-device-transformation
				  :initform +identity-transformation+))
  (:default-initargs :default-text-margin 1000))

(defmethod close ((stream hpgl-stream) &key abort)
  (unless abort
    (hpgl-epilogue (sheet-medium stream))))

(defmethod pane-viewport-region ((stream hpgl-stream))
  +everywhere+)

(defmethod window-inside-width ((stream hpgl-stream))
  (graft-pixel-width (graft stream)))

(defmethod window-inside-height ((stream hpgl-stream))
  (graft-pixel-height (graft stream)))

(defmethod stream-ensure-cursor-visible ((stream hpgl-stream)
					 &optional cx cy)
  (declare (ignore cx cy))
  nil)

(defmethod stream-move-for-line-height-change ((stream hpgl-stream)
					       movement old-height cursor-x cursor-y)
  (declare (ignore movement old-height cursor-x cursor-y)))

(defmethod stream-replay ((stream hpgl-stream) &optional region)
  (let* ((output-record (stream-output-history stream)))
    (when (stream-drawing-p stream)
      (when output-record
	(letf-globally (((stream-recording-p stream) nil))
	  (replay output-record stream (or region +everywhere+)))))))


;;; Medium definitions

;;;--- These definitions could be shared with the postscript 

(defclass printer-stream-medium (basic-medium)
	  ((printer-stream :initarg :stream)))

(defclass hpgl-medium (printer-stream-medium) 
	  ((current-color :initform nil)
	   (current-text-style :initform nil)))


(defmethod medium-force-output ((medium hpgl-medium))
  (force-output (slot-value medium 'printer-stream)))

(defmethod medium-finish-output ((medium hpgl-medium))
  (finish-output (slot-value medium 'printer-stream)))

(defmethod medium-clear-output ((medium hpgl-medium))
  (clear-output (slot-value medium 'printer-stream)))



;;; Prologue.  Always sent to any hpgl output device.
(defparameter *hpgl-prologue*
  (format nil "~{~A~%~}"
	  ;; HPGL Magic: 
	  (list "IN;"
		(concatenate 'string "DT" (string #\return) ";"))))

(defmethod hpgl-prologue ((medium hpgl-medium) &key)
  (with-slots (printer-stream) medium
    (write-string *hpgl-prologue* printer-stream)) )

(defparameter *hpgl-epilogue*
  (format nil "~{~A~%~}"
	  '("PU;")))

(defmethod hpgl-epilogue ((medium hpgl-medium))
  (write-string *hpgl-epilogue* (slot-value medium 'printer-stream)))

#+ignore
(defun test-it ()
  (with-open-file (fstream "hpgl.output" :direction :output
		   :if-exists :supersede)
    (with-output-to-hpgl-stream (stream (make-broadcast-stream fstream *standard-output*))
      (progn
	(draw-line* stream 0 0 10 10)
	(draw-text* stream #\a 100 100)
	(draw-text* stream "hello" 50 50)
	(draw-point* stream 500 500)
	(draw-rectangle* stream 40 40 80 80)
	(draw-rectangle* stream 300 300 400 400 :filled t)
	(draw-triangle* stream 600 600 700 700 700 600))
      (draw-circle* stream 800 800 75 :filled nil)
      )))


#+ignore
(defmacro test-it (form)
  `(with-open-stream (fstream
		     (excl:run-shell-command "/usr/tech/cer/3rd/hpgl2ps-v2/hpgl2ps | lpr -Plw2"
					     :input :stream :wait nil))
    (with-output-to-hpgl-stream (stream (make-broadcast-stream fstream *standard-output*))
      (surrounding-output-with-border (stream)
	,form))))


#+ignore
(draw-text* stream "Hello" 500 500 :towards-x 500 :towards-y 700)
