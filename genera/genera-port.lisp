;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: GENERA-CLIM; Base: 10; Lowercase: Yes -*-

(in-package :genera-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."
;;; $fiHeader$


(defvar *genera-default-server-path* `(:genera :host ,net:*local-host*
					       :screen ,tv:main-screen))
(warn "Changing the default server path to ~S"
      (setq *default-server-path* *genera-default-server-path*))


(defclass genera-port (port)
    ((screen :initform nil)
     (console :initform nil)
     (color-p :initform nil)
     (embedded-p :initform nil)
     (cursor-font)
     (cursor-cache :initform nil :type nil)
     (height-pixels)
     (width-pixels)
     (type :allocation :class 
	   :initform :genera
	   :reader port-type))
  (:default-initargs :allow-loose-text-style-size-mapping nil))

(defmethod find-port-type ((type (eql ':genera)))
  'genera-port)

(defmethod initialize-instance :after ((port genera-port)
				       &key (server-path *genera-default-server-path*))
  (destructuring-bind (ignore &key (host net:*local-host*)
				   (screen tv:main-screen)) server-path
    (declare (ignore ignore host))
    (with-slots ((this-screen screen) console color-p embedded-p
		 cursor-font height-pixels width-pixels) port
      (setf this-screen     screen 
	    console	    (tv:sheet-console screen)
	    color-p	    (color:color-stream-p screen)
	    embedded-p	    (typep screen 'tv:basic-remote-screen)
	    cursor-font     fonts:mouse
	    height-pixels   (tv:sheet-inside-height screen)
	    width-pixels    (tv:sheet-inside-width screen)))
    (initialize-genera-port port)))


;;; Text styles for Genera sheets

(defmethod initialize-genera-port ((port genera-port))
  (let ((screen (slot-value port 'screen)))
    (let ((device (scl:send screen :display-device-type)))
      (cond
	#+Imach
	((and (find-package 'mtb)
	      (typep device (intern 
			      (symbol-name 
				'small-screen-genera-fonts-mac-display-device)
			      :mtb)))
	 (define-small-screen-text-style-mappings port))
	(t (define-normal-screen-text-style-mappings port))))))

(defmethod text-style-mapping :around ((port genera-port) style
				       &optional (character-set *standard-character-set*) 
						 window)
  (declare (ignore window))
  (let ((font (call-next-method)))
    (etypecase font
      (null nil)
      (sys:font font)
      (symbol
	(when (not (boundp font))
	  (handler-case (fed:find-and-load-font font)
	    (fed:font-not-found ()
	      (error "Mapping ~S for ~A characters in ~A character set could not be found."
		     font style (or character-set :default)))))
	(let ((font (symbol-value font)))
	  (unless (typep font 'sys:font)
	    (error "Mapping for ~A characters in ~A character set is not a font."
		   style (or character-set :default)))
	  ;; Simple cache: hope nobody changes the font later
	  (setf (text-style-mapping port (parse-text-style style)) font)
	  font)))))

(defvar *sheet-logical-size-alist*)

(defmethod standardize-text-style ((port genera-port) style 
				   &optional (character-set *standard-character-set*))
  (standardize-text-style-1
    port style character-set *sheet-logical-size-alist*))

(defun define-normal-screen-text-style-mappings (display-device)
  (setq *sheet-logical-size-alist*
	'((:tiny        6)
	  (:very-small  8)
	  (:small      10)
	  (:normal     12)
	  (:large      14)
	  (:very-large 20)
	  (:huge       24)))
  (define-text-style-mappings-1 display-device *standard-character-set*
    '((:family :fix (:face :roman (:size 20 fonts:bigfnt
					 14 fonts:medfnt
					 12 fonts:cptfont
					 10 fonts:tvfont
					 8  fonts:einy7
					 6  fonts:tiny)
			   :italic (:size 20 fonts:bigfnti
					  14 fonts:medfnti
					  12 fonts:cptfonti
					  10 fonts:tvfonti
					  8  fonts:einy7
					  6  fonts:tiny)
			   :bold (:size 20 fonts:bigfntb
					14 fonts:medfntb
					12 fonts:cptfontcb
					10 fonts:tvfontb
					8  fonts:einy7
					6  fonts:tiny)
			   (:bold :italic) (:size 20 fonts:bigfntbi
						  14 fonts:medfntbi
						  12 fonts:cptfontbi
						  10 fonts:tvfontbi
						  8  fonts:einy7
						  6  fonts:tiny)
			   (:bold :extended) (:size 12 fonts:cptfontb
						    10 fonts:tvfontb)
			   :condensed (:size 12 fonts:cptfontc)
			   (:extra :condensed) (:size 12 fonts:cptfontcc
						      10 fonts:tvfont)))))
  (define-text-style-mappings-1 display-device *standard-character-set*
    '((:family :serif (:face :roman (:size 20 fonts:dutch20
					   14 fonts:dutch14
					   12 fonts:tr12
					   10 fonts:tr10
					   8  fonts:tr8)
			     :italic (:size 20 fonts:dutch20i
					    14 fonts:dutch14i
					    12 fonts:tr12i
					    10 fonts:tr10i
					    8  fonts:tr8i)
			     :bold (:size 20 fonts:dutch20b
					  14 fonts:dutch14b
					  12 fonts:tr12b
					  10 fonts:tr10b
					  8  fonts:tr8b)
			     (:bold :italic) (:size 20 fonts:dutch20bi
						    14 fonts:dutch14bi
						    12 fonts:tr12bi
						    10 fonts:tr10bi
						    8  fonts:tr8bi)))))
  (define-text-style-mappings-1 display-device *standard-character-set*
    '((:family :sans-serif (:face :roman (:size 20 fonts:swiss20
						14 fonts:hl14
						12 fonts:hl12
						10 fonts:hl10
						8  fonts:hl8)
				  :italic (:size 20 fonts:swiss20i
						 14 fonts:hl14i
						 12 fonts:hl12i
						 10 fonts:hl10i
						 8  fonts:hl8i)
				  :bold (:size 20 fonts:swiss20b
					       14 fonts:hl14b
					       12 fonts:hl12b
					       10 fonts:hl10b
					       8  fonts:hl8b)
				  (:bold :italic) (:size 20 fonts:swiss20bi
							 14 fonts:hl14bi
							 12 fonts:hl12bi
							 10 fonts:hl10bi
							 8  fonts:hl8bi))))))

#+IMach
(defun define-small-screen-text-style-mappings (display-device)
  (setq *sheet-logical-size-alist*
	'((:tiny        6)
	  (:very-small  8)
	  (:small      10)
	  (:normal     12)
	  (:large      14)
	  (:very-large 20)
	  (:huge       24)))
  (define-text-style-mappings-1 display-device *standard-character-set*
    '((:family :fix (:face :roman (:size 20 fonts:medfnt
					 14 fonts:cptfont
					 12 fonts:tvfont
					 10 fonts:einy7
					 8  fonts:einy7
					 6  fonts:einy7)
			   :italic (:size 20 fonts:medfnti
					  14 fonts:cptfonti
					  12 fonts:tvfonti 
					  10 fonts:einy7
					  8  fonts:einy7
					  6  fonts:einy7)
			   :bold (:size 20 fonts:medfntb
					14 fonts:cptfontcb
					12 fonts:tvfontb 
					10 fonts:einy7
					8  fonts:einy7
					6  fonts:einy7)
			   (:bold :italic) (:size 20 fonts:medfntbi
						  14 fonts:cptfontbi
						  12 fonts:tvfontbi
						  10 fonts:einy7
						  8  fonts:einy7
						  6  fonts:einy7)
			   (:bold :extended) (:size 12 fonts:tvfontb
						    10 fonts:cptfontb)
			   :condensed (:size 12 fonts:cptfontc)
			   (:extra :condensed) (:size 12 fonts:cptfontcc
						      10 fonts:tvfont)))))
  (define-text-style-mappings-1 display-device *standard-character-set*
    '((:family :serif (:face :roman (:size 20 fonts:dutch14
					   14 fonts:tr12
					   12 fonts:tr10
					   10 fonts:tr8
					   8  fonts:tr8)
			     :italic (:size 20 fonts:dutch14i
					    14 fonts:tr12i
					    12 fonts:tr10i
					    10 fonts:tr8i
					    8  fonts:tr8i)
			     :bold (:size 20 fonts:dutch14b
					  14 fonts:tr12b
					  12 fonts:tr10b
					  10 fonts:tr8b
					  8  fonts:tr8b)
			     (:bold :italic) (:size 20 fonts:dutch14bi
						    14 fonts:tr12bi
						    12 fonts:tr10bi
						    10 fonts:tr8bi
						    8  fonts:tr8bi)))))
  (define-text-style-mappings-1 display-device *standard-character-set*
    '((:family :sans-serif (:face :roman (:size 20 fonts:hl14
						14 fonts:hl12
						12 fonts:hl10
						10 fonts:hl8
						8  fonts:hl8)
				  :italic (:size 20 fonts:hl14i
						 14 fonts:hl12i
						 12 fonts:hl10i
						 10 fonts:hl8i
						 8  fonts:hl8i)
				  :bold (:size 20 fonts:hl14b
					       14 fonts:hl12b
					       12 fonts:hl10b
					       10 fonts:hl8b
					       8  fonts:hl8b)
				  (:bold :italic) (:size 20 fonts:hl14bi
							 14 fonts:hl12bi
							 12 fonts:hl10bi
							 10 fonts:hl8bi
							 8  fonts:hl8bi))))))


(defvar *clim-window-count* 0)
(defvar *temporary-genera-mirror-desired* nil)

(defmethod realize-mirror ((port genera-port) sheet)
  (with-slots (screen height-pixels) port
    (multiple-value-bind (left top right bottom)
	(sheet-native-region* sheet)
      (setq left (integerize-coordinate left)
	    top (integerize-coordinate top)
	    right (integerize-coordinate right)
	    bottom (integerize-coordinate bottom))
      (let* ((genera-parent (sheet-mirror sheet))
	     (name (format nil "CLIM window ~D" (incf *clim-window-count*)))
	     (mirror
	       (tv:make-window
		 (if *temporary-genera-mirror-desired* 'temporary-genera-window 'genera-window)
		 ;; probably the screen
		 :superior genera-parent
		 :name name
		 :sheet sheet
		 :save-bits t
		 :deexposed-typeout-action :permit
		 :label nil  ;;name
		 ;; Want inside/outside coord system to match if possible
		 :borders nil
		 :x left :y top
		 :width (- right left) :height (- bottom top))))
	(multiple-value-bind (mleft mtop mright mbottom)
	    (scl:send mirror :margins)
	  (decf left mleft)
	  (decf top mtop)
	  (incf right mright)
	  (incf bottom mbottom))
	(multiple-value-bind (clip-left clip-top clip-right clip-bottom)
	    (scl:send (tv:sheet-superior mirror) :inside-edges)
	  (multiple-value-bind (left top right bottom)
	      (fit-region*-in-region* left top right bottom
				      clip-left clip-top clip-right clip-bottom)
	    (scl:send mirror :set-edges left top right bottom)))
	;;--- Do we need to do this?
	(setf (sheet-native-transformation sheet) +identity-transformation+)
	mirror))))

(defmethod realize-mirror :around ((port genera-port) (sheet top-level-sheet))
  (let ((*temporary-genera-mirror-desired* 
	  (getf (frame-properties (pane-frame sheet)) :save-under)))
    (call-next-method)))

(defmethod destroy-mirror ((port genera-port) sheet)
  (let ((window (sheet-direct-mirror sheet)))
    (when window
      (scl:send window :kill))))

(defmethod enable-mirror ((port genera-port) sheet)
  (let ((mirror (sheet-mirror sheet)))
    (scl:send mirror :expose)))

(defmethod disable-mirror ((port genera-port) sheet)
  (let ((mirror (sheet-mirror sheet)))
    (scl:send mirror :deactivate)))

;;--- Is this the same as WINDOW-STACK-ON-TOP?
(defmethod raise-mirror ((port genera-port) (sheet mirrored-sheet-mixin))
  (let ((window (sheet-mirror sheet)))
    (unless (scl:send window :exposed-p)
      (scl:send window :expose))
    (let ((screen (tv:sheet-screen window))
	  (console (tv:sheet-console window))
	  (mouse (tv:sheet-mouse window)))
      ;; SCREEN-MANAGE-AUTOEXPOSE-INFERIORS doesn't reliably select a window
      ;; that fills the entire screen
      (let ((sw (tv:console-selected-window console)))
	(unless (and sw (atom (tv:sheet-lock sw)))
	  (scl:send window :select)))
      (scl:send screen :screen-select)
      (unless (eq screen (tv:sheet-screen (tv:mouse-sheet mouse)))
	(tv:mouse-set-sheet screen)))))

;;--- Is this the same as WINDOW-STACK-ON-BOTTOM?
(defmethod bury-mirror ((port genera-port) (sheet mirrored-sheet-mixin))
  (let ((window (sheet-mirror sheet)))
    (scl:send window :bury)))

(defmethod realize-graft ((port genera-port) graft)
  (with-slots (silica::pixels-per-point silica::pixel-width silica::pixel-height
	       silica::mm-width silica::mm-height silica::units) graft
    (let ((screen (slot-value port 'screen)))
      (setf silica::pixel-width  (scl:send screen :inside-width)
	    silica::pixel-height (scl:send screen :inside-height)
	    ;;--- Bogus numbers
	    silica::mm-width     360.0
	    silica::mm-height	 280.0
	    silica::pixels-per-point 1)
      (setf (sheet-region graft)
	    (ecase silica::units
	      ((:device :pixel)
	       (make-rectangle* 0 0 
				silica::pixel-width silica::pixel-height))))
      (setf (sheet-native-transformation graft) +identity-transformation+)
      (setf (sheet-direct-mirror graft) screen)
      (update-mirror-transformation port graft))))

(defmethod genera-mirror-native-edges* ((port genera-port) sheet &optional mirror)
  (let ((mirror (or mirror (sheet-mirror sheet))))
    (multiple-value-bind (width height)
	(scl:send mirror :size)
      (multiple-value-bind (xoff yoff)
	  (tv:sheet-calculate-offsets mirror (tv:sheet-screen mirror))
	(values xoff
		yoff
		(+ xoff width)
		(+ yoff height))))))

(defmethod mirror-region* ((port genera-port) sheet)
  (let ((mirror (sheet-mirror sheet)))
    (when mirror
      (genera-mirror-native-edges* port sheet mirror))))

(defmethod mirror-region ((port genera-port) sheet)
  (multiple-value-call #'make-bounding-rectangle
    (mirror-region* port sheet)))

(defmethod mirror-inside-region* ((port genera-port) sheet)
  (scl:send (sheet-mirror sheet) :inside-edges))

;;--- Shouldn't this be the same as MIRROR-REGION*?
(defmethod mirror-native-edges* ((port genera-port) sheet)
  (let ((mirror (sheet-direct-mirror sheet)))
    (genera-mirror-native-edges* port sheet mirror)))

;;--- Shouldn't this be the same as MIRROR-INSIDE-REGION*?
(defmethod mirror-inside-edges* ((port genera-port) sheet)
  (multiple-value-bind (left top right bottom)
      (mirror-native-edges* port sheet)
    (values 0 0 (- right left) (- bottom top))))
 
(defmethod set-sheet-mirror-edges* ((port genera-port) sheet 
				    left top right bottom)
  (let ((mirror (sheet-direct-mirror sheet)))
    (multiple-value-bind (mleft mtop mright mbottom)
	(scl:send mirror :margins)
      (decf left mleft)
      (decf top mtop)
      (incf right mright)
      (incf bottom mbottom))
    (multiple-value-bind (clip-left clip-top clip-right clip-bottom)
	(scl:send (tv:sheet-superior mirror) :inside-edges)
      (multiple-value-bind (left top right bottom)
	  (fit-region*-in-region* left top right bottom
				  clip-left clip-top clip-right clip-bottom)
	(scl:send mirror :set-edges (integerize-coordinate left)
				    (integerize-coordinate top)
				    (integerize-coordinate right)
				    (integerize-coordinate bottom))))))


(defmacro with-genera-glyph-for-character (&body body)
  `(macrolet ((port-glyph-for-character (port character style &optional our-font)
		`(multiple-value-bind (character-set index)
		     (char-character-set-and-index ,character)
		   ;; For now we are asserting that each string passed to WRITE-STRING will
		   ;; have no style changes within it.  This is what our-font is all
		   ;; about.
		   (let* ((font (or our-font 
				    (text-style-mapping port ,style character-set)))
			  (fit (sys:font-indexing-table font))
			  (lkt (sys:font-left-kern-table font))
			  (cwt (sys:font-char-width-table font))
			  (escapement-x (if (diacritic-char-p ,character)
					    0
					    (if cwt 
						(aref cwt index)
						(sys:font-char-width font))))
			  (escapement-y 0)
			  (origin-x (if lkt (aref lkt index) 0))
			  (origin-y (sys:font-baseline font))
			  (bb-x (if fit (- (aref fit (1+ index)) (aref fit index))
				    (sys:font-char-width font)))
			  (bb-y (sys:font-char-height font)))
		     (values index font escapement-x escapement-y origin-x origin-y
			     bb-x bb-y (not (or fit lkt cwt)))))))
     ,@body))

(defmethod port-glyph-for-character ((port genera-port)
				     character style &optional our-font)
  (declare (values index font escapement-x escapement-y origin-x origin-y bb-x bb-y
		   fixed-width-font-p))
  (with-genera-glyph-for-character
    (port-glyph-for-character stream character style our-font)))

(defmethod stream-scan-string-for-writing ((port genera-port) #+Silica medium 
					   string start end style cursor-x max-x
					   &optional glyph-buffer)
  (with-genera-glyph-for-character 
    (clim-internals::stream-scan-string-for-writing-body)))

;; This seem specific to the type of the medium

(defmethod text-style-width ((text-style standard-text-style) (port genera-port))
  (let ((font (text-style-mapping port text-style)))
    (sys:font-char-width font)))

(defmethod text-style-height ((text-style standard-text-style) (port genera-port))
  (let ((font (text-style-mapping port text-style)))
    (sys:font-char-height font)))

(defmethod text-style-ascent ((text-style standard-text-style) (port genera-port))
  (let ((font (text-style-mapping port text-style)))
    (let* ((height (sys:font-char-height font))
	   (descent (- height (sys:font-baseline font)))
	   (ascent (- height descent)))
      ascent)))

(defmethod text-style-descent ((text-style standard-text-style) (port genera-port))
  (let ((font (text-style-mapping port text-style)))
    (let* ((height (sys:font-char-height font))
	   (descent (- height (sys:font-baseline font))))
      descent)))

(defmethod text-style-fixed-width-p ((text-style standard-text-style) (port genera-port))
  (let ((font (text-style-mapping port text-style)))
      (null (sys:font-char-width-table font))))


#||
(define-keysym #\Clear-Input :clear-input)
(define-keysym #\Help :Help)
(define-keysym #\Refresh :Refresh)
(define-keysym #\Abort :Abort)
(define-keysym #\End :End)
(define-keysym #\Scroll :Scroll)
(define-keysym #\Complete :Complete)
||#


;; Port trigger not nil means that the port is triggering the
;; enabling/disabling.  This allows port enabled/disabled methods
;; to distinguish which way the flow of control is going.
(defvar *port-trigger* nil)

(scl:defflavor genera-window
	((sheet nil)
	 (mouse-x nil) 
	 (mouse-y nil)
	 (mouse-moved nil)
	 (blinker-table nil)
	 (selected-inferior nil))
	(dw::margin-mixin			;scroll bars, labels, borders
	 tv:no-screen-managing-mixin		;don't auto-select a pane
	 tv:changeable-name-mixin
	 tv:window)
  (:initable-instance-variables sheet)
  (:writable-instance-variables sheet)
  (:default-init-plist :borders nil
		       :save-bits nil		;adjusted later if top-level
		       :deexposed-typeout-action :permit
		       :blinker-p nil
		       :label nil))

(scl:defflavor temporary-genera-window
	()
	(tv:temporary-window-mixin genera-window))

;; All the sheet windows in a given activity (top level window) share the same io-buffer, and
;; CLIM decides which CLIM window to give the input to.
;; A top-level window should have a bit save array if we want the screen manager
;; to do what users expect.
(scl:defmethod (:init genera-window :before) (args)
  (declare (ignore args))
  (let ((top-level (not (typep tv:superior 'genera-window))))
    (unless tv:io-buffer
      (setq tv:io-buffer (if top-level
			     (tv:make-io-buffer 100)
			     (scl:send tv:superior :io-buffer))))
    (when top-level
      ;; This is like :SAVE-BITS :DELAYED
      (setf (tv:sheet-save-bits) 1))))

(scl:defmethod (:init temporary-genera-window :after) (args)
  (declare (ignore args))
  (setf (tv:sheet-save-bits) 0))

;;; The window manager should manipulate the highest "CLIM sheet" when invoked
;;; on an inferior.
(scl:defmethod (:alias-for-selected-windows genera-window) ()
  (if (typep tv:superior 'genera-window)
      (scl:send tv:superior :alias-for-selected-windows)
      scl:self))

(scl:defmethod (:name-for-selection genera-window) ()
  (or (let ((frame (pane-frame sheet)))
	(and frame (frame-pretty-name frame)))
      (let ((label (scl:send scl:self :label)))
	(and label (scl:string-search-not-char #\sp label) label))
      tv:name))

;;; A combination of the methods of pane-mixin and basic-frame
(scl:defwhopper (:select genera-window) (&rest args)
  (when (scl:send tv:superior :inferior-select scl:self)
    (cond ((and selected-inferior
		(scl:send selected-inferior :exposed-p))
	   (scl:lexpr-send selected-inferior :select args))
	  ((and (null selected-inferior)
		tv:inferiors
		(let ((frame (window-stream-to-frame sheet)))
		  (when frame
		    ;; This is a frame whose input focus has not yet been directed to any pane
		    ;; Choose the interactor pane by default
		    (let ((stream (frame-query-io frame)))
		      (when stream
			(scl:lexpr-send (slot-value (sheet-medium stream) 'window)
					:select args)
			t))))))
	  (t
	   (scl:lexpr-continue-whopper args)))))

;;; As for basic-frame
(scl:defmethod (:inferior-select genera-window) (pane)
  (setq selected-inferior pane)
  (scl:send tv:superior :inferior-select scl:self))

;;; As for basic-frame
(scl:defwhopper (:select-relative genera-window) ()
  (if (and selected-inferior
	   (scl:send selected-inferior :exposed-p))
      (scl:send selected-inferior :select-relative)
      (scl:continue-whopper)))

;;; As for pane-mixin
;;; "When selecting a pane with the mouse, pass the selection request to the frame."
(scl:defwhopper (:mouse-select genera-window) (&rest args)
  (if (typep tv:superior 'genera-window)
      (scl:lexpr-send tv:superior ':mouse-select args)
      (scl:lexpr-continue-whopper args)))

;; When the window is first activated ("mapped") run the repaint handler
(scl:defmethod (:expose genera-window :after) (&rest args)
  (declare (ignore args))
  (scl:send tv:superior :inferior-expose scl:self)
  (unless *port-trigger*
    (let ((*port-trigger* t))
      (setf (sheet-enabled-p sheet) t))))

(scl:defmethod (:inferior-expose genera-window) (inferior)
  (when (eq inferior selected-inferior)
    (scl:send inferior :select-relative)))

(scl:defwhopper (:set-reverse-video-p genera-window) (reverse-video-p)
  (let ((old-reverse-video-p (scl:send scl:self :reverse-video-p)))
    (multiple-value-prog1
      (scl:continue-whopper reverse-video-p)
      (unless (eql reverse-video-p old-reverse-video-p)
	(labels ((switch-inks (sheet)
		   (let ((medium (sheet-medium sheet)))
		     ;; Inhibit refreshing until we're done
		     (rotatef (slot-value medium 'foreground)
			      (slot-value medium 'background)))
		   (dolist (sheet (sheet-children sheet))
		     (switch-inks sheet))))
	  (declare (dynamic-extent #'switch-inks))
	  (switch-inks sheet)
	  (scl:send scl:self :refresh))))))

(defmethod genera-mouse-char-for-cursor ((cursor t))
  ;; any other cursor we don't know about yet
  #\mouse:ne-arrow)

(defvar *genera-cursor-type-alist*
	'((:default #\mouse:nw-arrow)
	  (:vertical-scroll #\mouse:vertical-double-arrow)
	  (:scroll-up #\mouse:up-arrow)
	  (:scroll-down #\mouse:down-arrow)
	  (:horizontal-scroll #\mouse:horizontal-double-arrow)
	  (:scroll-left #\mouse:left-arrow)
	  (:scroll-right #\mouse:right-arrow)
	  (:busy #\mouse:hourglass)
	  (:upper-left #\mouse:nw-corner)
	  (:lower-right #\mouse:se-corner)))

(defmethod install-port-cursor ((port genera-port) sheet cursor)
  (declare (ignore sheet))
  (let ((char (or (second (assoc cursor *genera-cursor-type-alist*))
		  #\mouse:ne-arrow)))
    ;; Note that this means that CLIM requires DW for the nonce...
    ;; It would be trivial to encode this information in the above methods, though
    (let ((entry (assoc char dw::*mouse-blinker-characters*))
	  (x 0) (y 0))
      (when entry
	(scl:destructuring-bind (nil xx yy &optional nil) entry
	  (setq x xx y yy)))
      (tv:mouse-set-blinker-definition ':character x y ':on ':set-character char))))

(defmethod set-cursor-location ((port genera-port) sheet location)
  ;; we don't do anything about this yet.
  (declare (ignore sheet location))
  )

(scl:defmethod (ensure-blinker-for-cursor genera-window) (cursor)
  (unless blinker-table (setq blinker-table (make-hash-table)))
  (or (gethash cursor blinker-table)
      (let ((blinker (tv:make-blinker scl:self 'tv:rectangular-blinker :follow-p t)))
	(setf (gethash cursor blinker-table) blinker)
	(scl:send blinker :set-visibility nil)
	blinker)))

(defun ensure-blinker-matches-cursor (cursor stream)
  (let ((active (clim-internals::cursor-active cursor))
	(state (clim-internals::cursor-state cursor))
	(focus (clim-internals::cursor-focus cursor)))
    (let* ((mirror (sheet-mirror stream))
	   (blinker (ensure-blinker-for-cursor mirror cursor)))
      (when blinker
	(let ((transformation (sheet-native-transformation stream)))
	  (multiple-value-bind (x y) (bounding-rectangle* cursor)
	    (multiple-value-setq (x y)
	      (transform-point* transformation x y))
	    (cond ((and active state focus)
		   (scl:send mirror :set-cursorpos x y)
		   (scl:send blinker :set-visibility :blink))
		  ((and active state)
		   (scl:send mirror :set-cursorpos x y)
		   (scl:send blinker :set-visibility T))
		  (t (scl:send blinker :set-visibility nil)))))))))

(defmethod (setf port-keyboard-input-focus) :around (focus (port genera-port))
  (let ((old-focus (port-keyboard-input-focus port)))
    (call-next-method)
    (let ((mirror (and focus (sheet-mirror focus)))
	  (old-mirror (and old-focus (sheet-mirror old-focus))))
      (when (and mirror 
		 old-mirror
		 (not (eql focus old-focus)))
	(if (eql (scl:send mirror :alias-for-selected-windows)
		 (scl:send old-mirror :alias-for-selected-windows))
	  (scl:send old-mirror :select-relative)
	  (scl:send mirror :deselect t))))))

(defmethod port-note-cursor-change ((port genera-port) cursor stream type old new)
  (declare (ignore type old))
  ;;--- Where should this be, really?
  (setf (port-keyboard-input-focus port) (and new stream))
  (ensure-blinker-matches-cursor cursor stream))


(scl:defmethod (:refresh genera-window :before) (&optional (type :complete-redisplay))
  (declare (ignore type))
  ;; Invalidate all cached inks, so they'll be redecoded taking into account any
  ;; changed parameters such as reversed video.
  (let* ((medium (sheet-medium sheet))
	 (cache (and medium (slot-value medium 'ink-cache))))
    (when cache
      (dotimes (i (array-total-size cache))
	(setf (aref cache i) nil)))
    (let ((foreground (genera-decode-color (medium-foreground medium) medium nil))
	  (background (genera-decode-color (medium-background medium) medium nil)))
      (funcall scl:self :set-char-aluf foreground)
      (funcall scl:self :set-erase-aluf background))))

(scl:defmethod (:refresh genera-window :after) (&optional (type ':complete-redisplay))
  (declare (ignore type))
  (unless *port-trigger*
    (let ((*port-trigger* t))
      (with-sheet-medium (medium sheet)
	(handle-repaint sheet medium (sheet-region sheet))))))

(scl:defmethod (tv:refresh-rectangle genera-window) (left top right bottom)
  #+++ignore (setf (slot-value sheet 'highlighted-presentation) nil)
  (frame-replay *application-frame*
		sheet (or (pane-viewport-region sheet)
			  (sheet-region sheet))))

;;; Called on position changes as well as size?
(scl:defmethod (:change-of-size-or-margins genera-window :after) (&rest options)
  (declare (ignore options))
  (let ((*port-trigger* t)
	(port (sheet-port sheet)))
    (dispatch-event
      sheet
      (let ((region (mirror-region port sheet)))
	(make-instance 'window-configuration-event
		       :native-region region
		       :region (untransform-region (sheet-native-transformation sheet) region)
		       :sheet sheet)))))

;;--- Problem: we disable the viewport that corresponds to the Genera
;;--- window, but that leaves the window stream enabled.  Should en/disable
;;--- on the viewport forward to the drawing-surface?
(scl:defmethod (:deexpose genera-window :after) (&rest ignore)
  (unless *port-trigger*
    (let ((*port-trigger* t))
      #+++ ;; want to call shrink-sheet, but that doesn't appear to be supported
      (disable-sheet sheet))))

;;; Save this state (rather than using (mouse-x mouse)) 'cause the coords
;;; are in window coordinates already.
(defvar *mouse-moved* nil)
(defvar *mouse-x* nil)
(defvar *mouse-y* nil)
(defvar *mouse-buttons* 0)
(defvar *mouse-window* nil)
(defvar *mouse-button-released* nil)

;;; Really should be per console, as with all of these special variables.
(defvar *old-mouse-chord-shifts* 0)

(defun-inline buttons-up (old-buttons new-buttons)
  #+Genera (declare (values buttons-up buttons-down))
  (values (boole boole-andc2 old-buttons new-buttons)
	  (boole boole-andc2 new-buttons old-buttons)))

(defun invoke-on-genera-shift-keysyms (genera-shift-mask continuation)
  (declare (dynamic-extent continuation))
  (macrolet ((do-shift (shift shift-name)
	       `(when (si:bit-test (si:name-bit ,shift) genera-shift-mask)
		  (funcall continuation ,shift-name))))
    ;; why is SHIFT different from sys:%kbd-shifts-shift
    (when (ldb-test (byte 1 4) genera-shift-mask)
      (funcall continuation :left-shift))
    (do-shift :control :left-control)
    (do-shift :meta :left-meta)
    (do-shift :super :left-super)
    (do-shift :hyper :left-hyper))
  nil)

(defmacro map-over-genera-shift-keysyms ((keysym-var genera-shift-mask) &body body)
  `(flet ((map-over-genera-shift-keysyms
	     (,keysym-var)
	    ,@body))
     (declare (dynamic-extent #'map-over-genera-shift-keysyms))
     (invoke-on-genera-shift-keysyms
       ,genera-shift-mask #'map-over-genera-shift-keysyms)))

;;; This is a horrible kludge for CLIM purposes.
(scl:advise tv:mouse-set-blinker-cursorpos-internal :after compute-buttons-released nil
  (kludge-buttons-released-when-mouse-over-no-window (first scl:arglist)))

;;; This is the guts of the horrible kludge for CLIM purposes.
(defun kludge-buttons-released-when-mouse-over-no-window (mouse)
  (multiple-value-bind (x y)
      (if (tv:mouse-warp-internal mouse)
	  (values (tv:mouse-last-x mouse)
		  (tv:mouse-last-y mouse))
	  (values (tv:mouse-x mouse)
		  (tv:mouse-y mouse)))
    (without-interrupts
      (let* ((old-buttons *mouse-buttons*)
	     (new-buttons tv:mouse-last-buttons)
	     (buttons-up (buttons-up old-buttons new-buttons))
	     ;; merge them all, for now.  This might drop a second up
	     ;; transition before the first one is noticed.  Even if we
	     ;; handled that here (by delaying updating *mouse-buttons*
	     ;; (actually, LOGIORing old and new) and
	     ;; *mouse-button-released*, it could require a mouse motion
	     ;; to actually notice the button up, which is gross!
	     (merged-up (logior buttons-up (or *mouse-button-released* 0)))
	     (new-released (and (not (zerop merged-up)) merged-up)))
	(setq *mouse-buttons* new-buttons
	      ;; doesn't yet handle multiple buttons released at once...
	      *mouse-button-released* new-released
	      *mouse-window* (let ((win
				     (tv:window-under-mouse-internal
				       mouse ':mouse-select ':active x y)))
			       (when (typep win 'genera-window) win))
	      *mouse-x* x
	      *mouse-y* y
	      *mouse-moved* ':pointer-motion)))))

(si:compile-advice 'tv:mouse-set-blinker-cursorpos-internal)

(scl:defmethod (:mouse-moves genera-window :after) (x y)
  (without-interrupts
    (let* ((old-buttons *mouse-buttons*)
	   (new-buttons tv:mouse-last-buttons)
	   (buttons-up (buttons-up old-buttons new-buttons))
	   ;; merge them all, for now.  This might drop a second up
	   ;; transition before the first one is noticed.  Even if we
	   ;; handled that here (by delaying updating *mouse-buttons*
	   ;; (actually, LOGIORing old and new) and
	   ;; *mouse-button-released*, it could require a mouse motion
	   ;; to actually notice the button up, which is gross!
	   (merged-up (logior buttons-up (or *mouse-button-released* 0)))
	   (new-released (and (not (zerop merged-up)) merged-up)))
      (setq *mouse-window* scl:self
	    *mouse-x* x
	    *mouse-y* y
	    *mouse-buttons* new-buttons
	    ;; doesn't yet handle multiple buttons released at once...
	    *mouse-button-released* new-released
	    *mouse-moved* ':pointer-motion))))

(scl:defmethod (:handle-mouse genera-window :before) ()
  (scl:send scl:self :select)			;---why??
  (without-interrupts
    (setq *mouse-window* scl:self
	  *mouse-moved* ':enter-notify
	  *mouse-x* -1
	  *mouse-y* -1)))			;--- Unsure this is right.

(scl:defmethod (:handle-mouse genera-window :after) ()
  (ecase :give-up-kbd-focus			;!!
    (:deselect-too-big-a-hammer (scl:send scl:self :deselect))
    (:give-up-kbd-focus
      (let* ((exposed (scl:send (tv:sheet-screen scl:self) :exposed-inferiors))
	     (window (find-if #'(lambda (window) (member window exposed))
			      tv:previously-selected-windows)))
	(when window (scl:send window :select)))))
  (without-interrupts
    (setq *mouse-window* scl:self		;--- Which window is this notification for?
	  *mouse-moved* ':leave-notify
	  *mouse-x* -1
	  *mouse-y* -1)))			;--- ???

#+++ignore
(scl:defmethod (:who-line-documentation-string genera-window) ()
  (port-pointer-documentation-string (port sheet)))

(scl:compile-flavor-methods genera-window temporary-genera-window)


(defmacro let-state ((&rest let-clauses) &body body)
  `(multiple-value-bind ,(mapcar #'first let-clauses)
       (without-scheduling
	 (values ,@(mapcar #'second let-clauses)))
     ,@body))

(defun-inline genera-button-number->standard-button-name (code)
  (aref `#(,+pointer-left-button+
	   ,+pointer-middle-button+
	   ,+pointer-right-button+) code))

;;; Take events out of the global queue and distribute them, and get key
;;; press "events" as characters out of the window's io-buffer.
;;;
;;; This method communicates with the dispatch-device-event method below through
;;; distribute-device-event.  It does this by passing in extra keywords, which
;;; are then passed back to dispatch.  I use the event-naming keywords used by
;;; the QUEUE-INPUT method.

;;; --- What should we do with non-:MOUSE-BUTTON blips?  What we
;;; --- currently do is just drop them on the floor...
(defmethod process-next-event ((port genera-port) 
			       &key (timeout nil) (wait-function nil)
				    ((:state whostate) "Genera Event"))
  ;; Motion events are queued by the mouse process (in the :mouse-moves method above)
  ;; but the character input side isn't so well defined.  Rely on the fact that
  ;; only the selected window can be receiving input.
  (let ((genera-window nil)
	(shifts-up nil)
	(shifts-down nil))
    (flet ((await-Genera-event ()
	     (or *mouse-moved*
		 ;; which mouse to pass off to MOUSE-CHORD-SHIFTS?
		 (let ((shifts (tv:mouse-chord-shifts))
		       (old *old-mouse-chord-shifts*))
		   (when (/= old shifts)
		     (multiple-value-setq (shifts-up shifts-down)
		       (buttons-up old shifts))
		     (setq *old-mouse-chord-shifts* shifts)
		     t))
		 (when (and (typep tv:selected-window 'genera-window)
			    (scl:send tv:selected-window :listen))
		   (setq genera-window tv:selected-window)
		   t)
		 (when wait-function
		   (funcall wait-function)))))
      (declare (dynamic-extent #'await-Genera-event))
      (sys:process-wait-with-timeout whostate timeout #'await-Genera-event))
    (cond (*mouse-moved*
	   (let-state ((mouse-moved (shiftf *mouse-moved* nil))	;capture and reset
		       (mouse-window	*mouse-window*)
		       (mouse-x		*mouse-x*)
		       (mouse-y		*mouse-y*)
		       (mouse-buttons	*mouse-buttons*)
		       (mouse-button-released (shiftf *mouse-button-released* nil)))
	     (multiple-value-bind (left top)
		 (if *mouse-window*
		     (genera-window-margins *mouse-window*)
		     (values 0 0))
	       (let ((native-x (- mouse-x left))
		     (native-y (- mouse-y top)))
		 (when mouse-moved		;check again
		   (let ((sheet (and mouse-window (genera-window-sheet mouse-window))))
		     (when sheet
		       (distribute-event
			 (sheet-port sheet)
			 (make-instance 'pointer-motion-event
					:x mouse-x
					:y mouse-y
					:native-x native-x
					:native-y native-y
					:modifiers (current-modifier-state
						     (make-state-from-buttons mouse-buttons)
						     (tv:sheet-mouse mouse-window))
					:sheet sheet)))))
		 (when mouse-button-released
		   (let ((sheet (and mouse-window (genera-window-sheet mouse-window))))
		     ;; hmm?
		     (when sheet
		       (distribute-event
			 (sheet-port sheet)
			 (make-instance 'pointer-button-release-event
					:x mouse-x
					:y mouse-y
					:native-x native-x
					:native-y native-y
					:button (genera-button-number->standard-button-name
						  (ash mouse-button-released -1))
					:modifiers (current-modifier-state
						     (make-state-from-buttons mouse-buttons)
						     (tv:sheet-mouse mouse-window))
					:sheet sheet)))))))))
	  ;; Handle shift press and release events
	  ((or shifts-up shifts-down)
	   (when (typep tv:selected-window 'genera-window)
	     (let* ((genera-window tv:selected-window)
		    (sheet (genera-window-sheet genera-window)))
	       (when sheet
		 (let ((state (current-modifier-state 0 (tv:sheet-mouse genera-window))))
		   #+++ignore
		   (when shifts-up
		     (map-over-genera-shift-keysyms (shift-keysym shifts-up)
		       (distribute *mouse-x* *mouse-y*
				   :event-key ':key-release
				   :keysym shift-keysym
				   :char nil
				   :keyboard-p T
				   :state state)))
		   #+++ignore
		   (when shifts-down
		     (map-over-genera-shift-keysyms (shift-keysym shifts-down)
		       (distribute *mouse-x* *mouse-y*
				   :event-key ':key-press
				   :keysym shift-keysym
				   :char nil
				   :keyboard-p T
				   :state state))))))))
	  ;; Make sure we read from the same window that :LISTEN return T on, even
	  ;; if the selected window state has changed.
	  ((and genera-window			; must be a genera-window
		(scl:send genera-window :listen))
	   (let ((thing (let ((sys:kbd-intercepted-characters
				(remove #\Abort sys:kbd-intercepted-characters)))
			  (scl:send genera-window :any-tyi-no-hang)))
		 (sheet (genera-window-sheet genera-window)))
	     (typecase thing
	       (character
		 (when sheet
		   ;;--- Not snapshotting simultaneous X and Y, but I don't
		   ;; think it matters here.
		   ;; remember that state is always the state before
		   ;; this event was processed.  Since we're not yet distributing
		   ;; the key presses for the shifts, no big deal.
		   (let ((keysym #+++ignore (genera-character->keysym thing)
				 #---ignore thing)
			 (char thing))
		     (when keysym
		       (distribute-event
			 (sheet-port sheet)
			 (make-instance 'key-press-event
					:key-name keysym
					:character char
					:modifiers (current-modifier-state 
						     0 (tv:sheet-mouse genera-window))
					:sheet sheet))
		       (distribute-event
			 (sheet-port sheet)
			 (make-instance 'key-release-event
					:key-name keysym
					:character char
					:modifiers (current-modifier-state 
						     0 (tv:sheet-mouse genera-window))
					:sheet sheet))))))
	       ;; See if it is a button-click blip
	       (list
		 (when (eql (first thing) ':mouse-button)
		   ;; (:mouse-button #\mouse-left window x y)
		   (let* ((mouse-x (fourth thing))
			  (mouse-y (fifth thing))
			  (code (tv:char-mouse-button (second thing)))
			  (window (third thing))
			  (button (genera-button-number->standard-button-name code)))
		     (when sheet
		       (multiple-value-bind (left top)
			   (if *mouse-window*
			       (genera-window-margins *mouse-window*)
			       (values 0 0))
			 (let ((native-x (- mouse-x left))
			       (native-y (- mouse-y top)))
			   (distribute-event
			     (sheet-port sheet)
			     (make-instance 'pointer-button-press-event
					    :x mouse-x
					    :y mouse-y
					    :native-x native-x
					    :native-y native-y
					    :button button
					    :modifiers (current-modifier-state
							 0 (tv:sheet-mouse window))
					    :sheet sheet))))))))))))))

;; See general comments about sheet-native-native-region* and
;; sheet-native-region* about functions vs macros, etc.
(defun genera-window-margins (mirror)
  (declare (values left top inside-width inside-height))
  (multiple-value-bind (left top)
      (scl:send mirror :margins)
    (multiple-value-bind (width height)
	(scl:send mirror :inside-size)
      (values left top width height))))

(defun make-state-from-buttons (buttons)
  (ash 1 buttons))

(defun current-modifier-state (&optional (state 0) (mouse tv:main-mouse))
  ;; Take only the upper bits of state, compute the lower bits from the current
  ;; shifts.
  (let ((shifts (tv:mouse-chord-shifts mouse))
	(state (logand state #xFF00)))
    (macrolet ((do-shift (shift)
		 `(when (si:bit-test (si:name-bit ,shift) shifts)
		    (let ((bit (clim-internals::modifier-key-index ,shift)))
		      (setf state (dpb 1 (byte 1 bit) state))))))
      ;; why is SHIFT different from sys:%kbd-shifts-shift
      (when (ldb-test (byte 1 4) shifts)
	(let ((bit (clim-internals::modifier-key-index :shift)))
	  (setf state (dpb 1 (byte 1 bit) state))))
      (do-shift :control)
      (do-shift :meta)
      (do-shift :super)
      (do-shift :hyper))
    state))

;;--- Doesn't this need a sheet/stream argument?
(defmethod port-force-output ((port genera-port))
  #+++ignore (stream-force-output stream))

;;--- Doesn't this need a sheet/stream argument?
(defmethod port-finish-output ((port genera-port))
  #+++ignore (stream-force-output stream))


;;;--- From ILA stream
#||
(defmethod destroy-port :after ((port genera-port))
  (with-slots (genera-screen) port
    ;;--- Needs to be a defined accessor (or mapping function)
    (do* ((tail (slot-value port 'mirror->sheet-table)
		;; Really a PLIST
		(cddr tail))
	  (genera-window (first tail) (first tail)))
	 ((null tail) nil)
      (scl:send genera-window :kill))))

||#
