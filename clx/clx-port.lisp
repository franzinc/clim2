;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLX-CLIM; Base: 10; Lowercase: Yes -*-

(in-package :clx-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."
;;; $fiHeader$


(defclass clx-port (port)
    ((application-shell :reader port-application-shell)
     (display :reader port-display)
     (context :reader port-context))
  (:default-initargs :allow-loose-text-style-size-mapping t))

(defmethod find-port-type ((type (eql ':clx)))
  'clx-port)

(defmethod initialize-instance :after ((port clx-port) &key server-path)
  (destructuring-bind (ignore &key display) server-path
    (declare (ignore ignore))
    (multiple-value-bind (context display application-shell)
	(initialize-motif-toolkit display)
      (setf (slot-value port 'application-shell) application-shell
	    (slot-value port 'context) context
	    (slot-value port 'display) display)
      (initialize-clx-port port display))))


;;; Text styles for CLX windows

(defvar *clx-font-families* '((:fix "*-courier-*")
			      (:sans-serif "*-helvetica-*")
			      (:serif "*-charter-*" "*-new century schoolbook-*" "*-times-*")))

(defun disassemble-x-font-name (name)
  (let ((cpos 0)
	(tokens nil))
    (loop
      (let ((dpos (position #\- name :start cpos)))
	(when (null dpos)
	  (push (subseq name cpos) tokens)
	  (return))
	(push (if (= cpos dpos)
		  nil
		  (subseq name cpos dpos))
	      tokens)
	(setf cpos (1+ dpos))))
    (reverse tokens)))

(defvar *clx-fallback-font* "8x13"
  "When non NIL and nothing better exists use this as the fallback font")

(defmethod initialize-instance :after ((port clx-port) 
				       &key display screen)
  (let* ((screen-height (xlib:screen-height screen))
	 ;; Use a float value so formula below will be accurate.
	 (screen-pixels-per-inch
	   (* 25.4 (/ screen-height
		      (xlib:screen-height-in-millimeters screen)))))
    (flet ((font->text-style (font family)
	     (let* ((tokens (disassemble-x-font-name font))
		    (italic (member (fifth tokens) '("i" "o") :test #'equalp))
		    (bold (equalp (fourth tokens) "Bold"))
		    (face (if italic
			      (if bold '(:bold :italic) :italic)
			      (if bold :bold :roman)))
		    (designed-point-size (parse-integer (ninth tokens)))
		    (designed-y-resolution (parse-integer (nth 10 tokens)))
		    (point-size (* (float designed-point-size)
				   (/ designed-y-resolution
				      screen-pixels-per-inch)))
		    (size (/ point-size 10)))
	       (make-text-style family face size))))
      (dolist (family-stuff *clx-font-families*)
	(let ((family (car family-stuff)))
	  (dolist (font-pattern (cdr family-stuff))
	    (dolist (xfont (xlib:list-font-names display font-pattern))
	      (let ((text-style (font->text-style xfont family)))
		;; prefer first font satisfying this text style, so
		;; don't override if we've already defined one.
		(unless (text-style-mapping-exists-p
			  port text-style *standard-character-set* t)
		  (setf (text-style-mapping port text-style) xfont)))))
	  ;; Now build the logical size alist for the family
	  ))))
  (setq *cursor-font* (xlib:open-font display "cursor"))
  ;; We now need to find to establish the
  ;; *undefined-text-style* -> some abstract font mapping -> some real font
  ;; doing something horrible if that fails.
  (let (temp)
    (cond ((setq temp
		 (dolist (family *clx-font-families*)
		   (when (text-style-mapping-exists-p 
			   port `(,(car family) :roman 10))
		     (return (make-text-style (car family) :roman 10)))))
	   (setf (text-style-mapping
		   port (port-undefined-text-style port))
		 temp))
	  ;; Perhaps we should look for some other conveniently sized
	  ;; fonts.
	  (*clx-fallback-font*
	   (setf (text-style-mapping
		   port (port-undefined-text-style port))
		 (xlib:open-font display
				 *clx-fallback-font*)))
	  ;; Perhaps we should just grab the first font we can find.
	  (t
	   (error "Unable to determine default font")))))

(defparameter *clx-logical-size-alist*
	      '((:tiny       6)
		(:very-small 8)
		(:small	     10)
		(:normal     12)
		(:large	     14)
		(:very-large 18)
		(:huge	     24)))

(defmethod standardize-text-style ((port clx-port) style 
				   &optional (character-set *standard-character-set*))
  (standardize-text-style-1
    port style character-set *clx-logical-size-alist*))


(defmethod silica::destroy-mirror ((port clx-port) sheet)
  (tk::destroy-widget (sheet-direct-mirror sheet)))

(defmethod realize-mirror ((port clx-port) sheet)
  (multiple-value-bind (class initargs)
      (find-widget-class-and-initargs-for-sheet port sheet)
    (let ((widget (apply #'make-instance class
			 :parent (find-widget-parent port sheet)
			 :managed (sheet-enabled-p sheet)
			 initargs)))
      (add-sheet-callbacks port sheet widget)
      widget)))

(defmethod add-sheet-callbacks ((port clx-port) sheet (widget t))
  (declare (ignore sheet)))

(defmethod sheet-mirror-event-handler (widget event sheet)
  (multiple-value-bind
      (same-p root child root-x root-y native-x native-y mask)
      (tk::query-pointer (tk::widget-window widget))
    (declare (ignore same-p root child root-x root-y))
    (let ((modifiers (logand #16rff mask))
	  (button (ash mask -8)))
      (distribute-event
       (sheet-port sheet)
       (ecase (tk::event-type event)
	 (:leave-notify
	  (make-instance 'pointer-exit-event
			 :native-x native-x
			 :native-y native-y
			 :button (x-button->silica-button button)
			 :modifiers modifiers
			 :sheet sheet))
	 (:enter-notify
	  (make-instance 'pointer-enter-event
			 :native-x native-x
			 :native-y native-y
			 :button (x-button->silica-button button)
			 :modifiers modifiers
			 :sheet sheet))
	 (:motion-notify
	  (make-instance 'pointer-motion-event
			 :native-x native-x
			 :native-y native-y
			 :button (x-button->silica-button button)
			 :modifiers modifiers
			 :sheet sheet)))))))

(defun x-button->silica-button (button)
  (ecase button
    (0 nil)
    (4 
     (warn "got an event 4")
     silica::+pointer-right-button+)
    (#.x11::button3 +pointer-right-button+)
    (#.x11::button2 +pointer-middle-button+)
    (#.x11::button1 +pointer-left-button+)))

(defmethod sheet-mirror-resized-callback (widget window event sheet)
  (declare (ignore widget window event))
  (dispatch-event
   sheet
   (let ((r (mirror-region (sheet-port sheet) sheet)))
     (make-instance 'window-configuration-event
		    :native-region r
		    :region (untransform-region
			     (sheet-native-transformation
			      sheet)
			     r)
		    :sheet sheet))))

(defmethod sheet-mirror-exposed-callback (widget window event sheet)
  (declare (ignore widget window))
  (let* ((minx (x11::xexposeevent-x event))
	 (miny (x11::xexposeevent-y event))
	 (width (x11::xexposeevent-width event))
	 (height (x11::xexposeevent-height event))
	 (maxx (+ minx width))
	 (maxy (+ miny height)))
    (dispatch-repaint
     sheet
     (make-instance 'window-repaint-event
		    :native-region (make-bounding-rectangle minx miny maxx maxy)
		    :region (untransform-region
			     (sheet-native-transformation sheet)
			     (make-bounding-rectangle minx miny maxx maxy))
		    :sheet sheet))))

(defmethod sheet-mirror-input-callback (widget window event sheet)
  (declare (ignore widget window))

  (ecase (tk::event-type event)
    (:key-press
     (distribute-event
      (sheet-port sheet)
      (multiple-value-bind
	  (ignore character keysym)
	  (tk::lookup-string event)
	(declare (ignore ignore))
	(make-instance 'key-press-event
		       :key-name keysym
		       :character (and (= (length character) 1)
				       (aref character 0))
		       :sheet sheet
		       :modifiers (x11::xkeyevent-state event)))))
    
    (:key-release
     (distribute-event
      (sheet-port sheet)
      (multiple-value-bind
	  (ignore character keysym)
	  (tk::lookup-string event)
	(declare (ignore ignore))
	(make-instance 'key-release-event
		       :key-name keysym
		       :character (and (= (length character) 1)
				       (aref character 0))
		       :sheet sheet
		       :modifiers (x11::xkeyevent-state event)))))
    
    (:button-press
     (distribute-event
      (sheet-port sheet)
      (make-instance 'pointer-button-press-event
		     :sheet sheet
		     :x :??
		     :y :??
		     :modifiers (x11::xkeyevent-state event)
		     :button (x-button->silica-button 
			      (x11::xbuttonevent-button event))
		     :native-x (x11::xbuttonevent-x event)
		     :native-y (x11::xbuttonevent-y event))))
    (:button-release
     (distribute-event
      (sheet-port sheet)
      (make-instance 'pointer-button-release-event
		     :sheet sheet
		     :x :??
		     :y :??
		     :modifiers (x11::xkeyevent-state event)
		     :button (x-button->silica-button 
			      (x11::xbuttonevent-button event))
		     :native-x (x11::xbuttonevent-x event)
		     :native-y (x11::xbuttonevent-y event))))))

(defmethod find-widget-class-and-initargs-for-sheet 
	   ((port clx-port) (parent t) (sheet sheet))
  (declare (ignore port))
  (values 'xm-drawing-area (list :resize-policy :grow)))

(defmethod find-widget-class-and-initargs-for-sheet :around 
	   ((port clx-port) (parent t) (sheet sheet))
  (declare (ignore port))
  (multiple-value-bind
      (class initargs)
      (call-next-method)
    (setq initargs (set-mirror-geometry sheet initargs))
    (values class initargs)))


(defmethod set-mirror-geometry (sheet initargs)
  (unless (getf initargs :x)
    (multiple-value-bind (left top right bottom)
	(sheet-actual-native-edges* sheet)
      (setf (getf initargs :x) (floor left)
	    (getf initargs :y) (floor top)
	    (getf initargs :width) (floor (- right left))
	    (getf initargs :height) (floor (- bottom top)))))
  initargs)

;; If we are creating a top level sheet then we have to create a shell
;; for it

(defmethod find-widget-parent ((port clx-port) sheet)
  (let ((ma (sheet-mirrored-ancestor sheet)))
    (if (graftp ma)
	(multiple-value-bind
	    (class initargs)
	    (find-shell-class-and-initargs port sheet)
	  (apply #'make-instance class
		 :parent (port-application-shell port)
		 initargs))
      (sheet-mirror ma))))

(defmethod find-shell-class-and-initargs ((port clx-port) sheet)
  (values 'top-level-shell 
	  ;; Need this so that an interactive pane can have children
	  ;; but still accept the focus
	  '(:keyboard-focus-policy :pointer)))

(defmethod enable-mirror ((port clx-port) sheet)
  (declare (ignore port))
  (let ((mirror (sheet-mirror sheet)))
    (typecase (widget-parent mirror)
      (null)
      (top-level-shell
       ;; this is a nasty hack just to make sure that the child is managed.
       ;; top-level-sheets are created unmanaged because they are
       ;; disabled to we have to do not!
       (manage-child mirror)
       (popup (widget-parent mirror)))
      (t
       (manage-child mirror)))))

(defmethod disable-mirror ((port clx-port) sheet)
  (declare (ignore port))
  (let ((mirror (sheet-mirror sheet)))
    (when mirror
      (typecase (widget-parent mirror)
	(null)
	(top-level-shell
	 (tk::popdown (widget-parent mirror)))
	(t
	 (tk::unmanage-child mirror))))))

(defmethod realize-graft ((port clx-port) graft)
  ;; Set the width etc etc
  (setf (sheet-direct-mirror graft) (port-application-shell port))
  ;; Mess with the region
  (warn "Do something about the graft")
  (setf (sheet-region graft)
    (make-bounding-rectangle 0 0 1100 850))
  ;; Mess with the native transformation
  )

(defmethod mirror-region* ((port clx-port) sheet)
  (when (sheet-mirror sheet)
    (multiple-value-bind
	(x y width height)
	(get-values (sheet-mirror sheet) :x :y :width :height)
      (values x y 
	      (+ x width)
	      (+ y height)))))

(defmethod mirror-region ((port clx-port) sheet)
  (multiple-value-call #'make-bounding-rectangle
    (mirror-region* port sheet)))

(defmethod mirror-inside-region* ((port clx-port) sheet)
  (multiple-value-bind
      (minx miny maxx maxy)
      (mirror-region* port sheet)
    (values 0 0 (- maxx minx) (- maxy miny))))

(defmethod mirror-native-edges* ((port clx-port) sheet)
  (let* ((mirror (sheet-direct-mirror sheet)))
    (multiple-value-bind
	(x y width height)
	(get-values mirror :x :y :width :height)
      (values x y (+ x width) (+ y height)))))

(defmethod mirror-inside-edges* ((port clx-port) sheet)
  (multiple-value-bind (left top right bottom)
      (mirror-native-edges* port sheet)
    (values 0 0 (- right left) (- bottom top))))

(defmethod set-sheet-mirror-edges* ((port clx-port) sheet 
				    target-left target-top
				    target-right target-bottom)
  (let ((w (- target-right  target-left))
	(h (- target-bottom target-top)))
    (tk::set-values (sheet-direct-mirror sheet)
		    :x (floor target-left)
		    :y (floor target-top)
		    :width (floor w)
		    :height (floor h))
    (multiple-value-bind
	(nx ny nw nh)
	(tk::get-values (sheet-direct-mirror sheet)
			:x :y :width :height)
      (when (or (/= target-left nx)
		(/= target-top ny)
		(/= w nw)
		(/= h nh))
	(warn "Geo set fail, ~S, ~S,~S"
	      sheet
	       (list  target-left  target-top w h)
	       (list nx ny nw nh))))))

(defmethod process-next-event (port &key wait-function timeout)
  (tk::process-one-event (port-context port)
			 :wait-function wait-function
			 :timeout timeout))


(defmethod port-force-output ((port clx-port))
  nil)

(defmethod port-finish-output ((port clx-port))
  nil)


(defmethod port-glyph-for-character ((port clx-port)
				     character style &optional our-font)
  (let* ((index (char-int character))
	 (x-font (or our-font
		     (realize-text-style port appearance)))
	 (escapement-x (tk::char-width x-font index))
	 (escapement-y 0)
	 (origin-x 0)
	 (origin-y (tk::font-ascent x-font))
	 (bb-x escapement-x)
	 (bb-y (+ origin-y (tk::font-descent x-font))))
    (when (zerop escapement-x) (break))
    (values index x-font escapement-x escapement-y
	    origin-x origin-y bb-x bb-y)))


;; This seem specific to the type of the medium

(defmethod text-style-mapping :around ((port clx-port) text-style
				       &optional (character-set *standard-character-set*) etc)
  (declare (ignore etc))
  (let ((font (call-next-method)))
    (when (or (stringp font) (symbolp font))
      (let* ((font-name (string font)))
	(with-slots (display) port
	  (setf font (xlib:open-font display font-name)))
	(setf (text-style-mapping
		port (parse-text-style text-style) character-set)
	      font)))
    font))

(defmethod text-style-width ((text-style standard-text-style) (port clx-port))
  (let ((font (text-style-mapping port text-style)))
    ;; Disgusting, but probably OK
    (xlib:char-width font (xlib:char->card8 #\A))))

(defmethod text-style-height ((text-style standard-text-style) (port clx-port))
  (let ((font (text-style-mapping port text-style)))
    (let* ((ascent (xlib:font-ascent font))
	   (descent (xlib:font-descent font))
	   (height (+ ascent descent)))
      height)))

(defmethod text-style-ascent ((text-style standard-text-style) (port clx-port))
  (let ((font (text-style-mapping port text-style)))
    (xlib:font-ascent font)))

(defmethod text-style-descent ((text-style standard-text-style) (port clx-port))
  (let ((font (text-style-mapping port text-style)))
    (xlib:font-descent font)))

(defmethod text-style-fixed-width-p ((text-style standard-text-style) (port clx-port))
  (let ((font (text-style-mapping port text-style)))
    ;; Really disgusting, but probably OK
    (= (xlib:char-width font (xlib:char->card8 #\.))
       (xlib:char-width font (xlib:char->card8 #\M)))))

#+ignore
(ff::defforeign 'xtsetkeyboardfocus
    :entry-point "_XtSetKeyboardFocus")

;;--- This is on the port and a sheet??
(defmethod stream-set-input-focus (stream)
  nil)

;;--- This is on the port and a sheet??
(defmethod stream-restore-input-focus (stream)
  nil)

(defmethod port-note-cursor-change ((port motif-port)
				    cursor stream type old new)
  (declare (ignore old type cursor))
  (call-next-method)
  (when new
    (xmprocesstraversal (tk::object-handle (sheet-mirror stream)) 0)
    #+ignore
    (xtsetkeyboardfocus #+ignroe(tk::object-handle (sheet-mirror (sheet-top-level-mirror stream)))
			(tk::object-handle (sheet-mirror stream))
			(tk::object-handle (sheet-mirror stream))))
  (setf (port-keyboard-input-focus port) 
	(and new stream)))
