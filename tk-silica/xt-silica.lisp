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
;; $fiHeader: xt-silica.lisp,v 1.27 92/05/13 17:11:24 cer Exp Locker: cer $

(in-package :xm-silica)

(defclass xt-port (port)
    ((application-shell :reader port-application-shell)
     (display :reader port-display)
     (context :reader port-context)     
     (copy-gc :initform nil)
     (opacities :initform nil)
     (event-lock :initform (clim-sys:make-lock "port event lock")
		 :reader port-event-lock)
     (rotated-font-cache :initform nil :accessor port-rotated-font-cache))
  (:default-initargs :allow-loose-text-style-size-mapping t)
  (:documentation "The port for X intrinsics based ports"))

(defmethod port-type ((port xt-port))
  ':xt)

(defmethod port-copy-gc ((port xt-port))
  (with-slots (copy-gc display) port
    (or copy-gc
	(setf copy-gc
	  (make-instance 'tk::gcontext
	    :display display
	    :graphics-exposures :on
	    :foreign-address (x11:screen-default-gc
 			      (x11:xdefaultscreenofdisplay display)))))))



(defmethod restart-port ((port xt-port))
  (let ((process (silica::port-process port)))
    (when process
      (clim-sys:destroy-process process)))
  (setf (silica::port-process port)
    (mp:process-run-restartable-function
     (list :name (format nil "CLIM Event Dispatcher for ~A"
			 (port-server-path port))
	   :priority 1000)
     #'silica::port-event-loop port)))

(defmacro destructure-x-server-path ((&key display) path &body body)
  ;;-- Of course the port ends up with an unspecified server-path.
  ;;-- Perhaps this is OK or perhaps we have to default the components before
  ;;-- putting it in the port???
  (let ((ignore (gensym)))
    `(destructuring-bind
	 (,ignore &key ((:display ,display)
			(or (sys::getenv "DISPLAY")
			    "localhost:0")))
	 ,path
	 (declare (ignore ,ignore))
       ,@body)))
       
(defmethod initialize-instance :after ((port xt-port) &key server-path)
  (destructuring-bind
      (&key (display nil display-p)
	    (application-name nil application-name-p)
	    (application-class nil application-class-p))
      (cdr server-path)
    (let ((args nil))
      (when display-p (setf (getf args :host) display))
      (when application-name-p (setf (getf args :application-name) application-name))
      (when application-class-p (setf (getf args :application-class) application-class))
      (multiple-value-bind (context display application-shell)
	  (apply #'tk::initialize-toolkit args)
	(setf (slot-value port 'application-shell) application-shell
	      (slot-value port 'context) context
	      (slot-value port 'display) display)
	(initialize-xlib-port port display)))))

(defvar *xt-font-families* '((:fix "*-*-courier-*-*-*-*-*-*-*-*-*-*-*-*")
			     (:sans-serif "*-*-helvetica-*-*-*-*-*-*-*-*-*-*-*-*")
			     (:serif "*-*-charter-*-*-*-*-*-*-*-*-*-*-*-*"
			      "*-*-new century schoolbook-*-*-*-*-*-*-*-*-*-*-*-*"
			      "*-*-times-*-*-*-*-*-*-*-*-*-*-*-*")))

(defun disassemble-x-font-name (name)
  (let ((cpos 0)
	(tokens nil))
    (loop
      (let ((dpos (position #\- name :start cpos)))
	(when (null dpos)
	  (push (subseq name cpos) tokens)
	  (return))
	(push (if (= cpos dpos) nil (subseq name cpos dpos))
	      tokens)
	(setf cpos (1+ dpos))))
    (reverse tokens)))


(defvar *xt-fallback-font* "8x13"
  "When non NIL and nothing better exists use this as the fallback font")

(defmethod initialize-xlib-port (port display)
  (setf (port-undefined-text-style port)
	(standardize-text-style 
	  port (make-text-style :stand-in-for-undefined-style :roman 10)))
  (flet ((font->text-style (font family)
	   (let* ((tokens (disassemble-x-font-name font))
		  (italic (member (fifth tokens) '("i" "o") :test #'equalp))
		  (bold (equalp (fourth tokens) "Bold"))
		  (face (if italic
			    (if bold '(:bold :italic) :italic)
			    (if bold :bold :roman)))
		  (designed-point-size (parse-integer (ninth tokens)))
		  (designed-y-resolution (parse-integer (nth 10 tokens)))
		  (point-size (float designed-point-size))
		  (size (/ point-size 10)))
	     (make-text-style family face size))))
    (dolist (family-stuff *xt-font-families*)
      (let ((family (car family-stuff)))
	(dolist (font-pattern (cdr family-stuff))
	  (dolist (xfont (tk::list-font-names display font-pattern))
	    (let ((text-style (font->text-style xfont family)))
	      ;; prefer first font satisfying this text style, so
	      ;; don't override if we've already defined one.
	      (unless (text-style-mapping-exists-p
			port text-style *standard-character-set* t)
		(setf (text-style-mapping port text-style) xfont)))))
	;; Now build the logical size alist for the family
	))
    (let (temp)
      (cond ((setq temp
		   (dolist (family *xt-font-families*)
		     (when (text-style-mapping-exists-p port `(,(car family) :roman 10))
		       (return (make-text-style (car family) :roman 10)))))
	     (setf (text-style-mapping port (port-undefined-text-style port)) temp))
	    ;; Perhaps we should look for some other conveniently sized
	    ;; fonts.
	    (*xt-fallback-font*
	     (setf (text-style-mapping port (port-undefined-text-style port))
		   (make-instance 'tk::font 
				  :display display
				  :name *xt-fallback-font*)))
	    ;;; Perhaps we should just grab the first font we can find.
	    (t
	     (error "Unable to determine default font")))))
  (setup-opacities port display))

(defparameter *xt-logical-size-alist*
	      '((:tiny       6)
		(:very-small 8)
		(:small	     10)
		(:normal     12)
		(:large	     14)
		(:very-large 18)
		(:huge	     24)))

(defmethod standardize-text-style ((port xt-port) style
				   &optional (character-set *standard-character-set*))
  (standardize-text-style-1
    port style character-set *xt-logical-size-alist*))

(defun make-stipple-image (height width patterns)
  (make-instance 'tk::image :width width :height height
		 :data (clim-internals::make-stipple-array height width patterns)
		 :depth 1))

(defvar *opacity-stipples*
	(mapcar #'(lambda (entry)
		    (cons (first entry)
			  (apply #'make-stipple-image (second entry))))
		'((+nowhere+ (1 1 (#b0)))
		  (0.05 (8 06 (#b1000000000000000
			      #b0000001000000000
			      #b0000000000001000
			      #b0010000000000000
			      #b0000000010000000
			      #b0000000000000010
			      #b0000100000000000
			      #b0000000000100000)))
		  (0.1 (8 8 (#b10000000
			     #b00010000
			     #b00000010
			     #b01000000
			     #b00001000
			     #b00000001
			     #b00100000
			     #b00000100)))
		  (0.2 (4 4 (#b1000
			     #b0010
			     #b0100
			     #b0001)))
		  (0.3 (3 3 (#b100
			     #b010
			     #b001)))
		  (0.4 (2 2 (#b10
			     #b01)))
		  (0.6 (3 3 (#b011
			     #b101
			     #b110)))
		  (0.7 (4 4 (#b0111
			     #b1101
			     #b1011
			     #b1110)))
		  (0.8 (8 8 (#b01111111
			     #b11101111
			     #b11111101
			     #b10111111
			     #b11110111
			     #b11111110
			     #b11011111
			     #b11111011)))
		  (0.9 (8 06 (#b0111111111111111
			       #b1111110111111111
			       #b1111111111110111
			       #b1101111111111111
			       #b1111111101111111
			       #b1111111111111101
			       #b1111011111111111
			       #b1111111111011111)))
		  (+everywhere+ (1 1 (#b1))))))


(defun setup-opacities (port display)
  (let ((opacities nil)
	(root (tk::display-root-window display))
	gc)
    (dolist (ls *opacity-stipples*)
      (let ((pixmap (make-instance 'tk::pixmap
		      :drawable root
		      :width (tk::image-width (cdr ls))
		      :height (tk::image-height (cdr ls))
		      :depth 1)))
	(unless gc
	  (setq gc (make-instance 'tk::gcontext :drawable pixmap
				  :foreground 1 :background 0)))
	 (tk::put-image pixmap gc (cdr ls))
	(push (cons (car ls) pixmap) opacities)))
    (when gc
      (tk::free-gcontext gc))
    (setf (slot-value port 'opacities) (nreverse opacities))))

;;
;; Takes an opacity, and returns a clip mask (pixmap) for that opacity.
;;
(defun decode-opacity (opacity port)
  (let ((pops (slot-value port 'opacities)))
    (cond ((eq opacity +nowhere+)
	   (cdar pops))
	  ((eq opacity +everywhere+)
	   (cdar (last pops)))
	  (t
	   (let ((lastpop (cdar pops))
		 (value (opacity-value opacity)))
	     (dolist (pop (cdr pops) lastpop)
	       (if (< value (car pop))
		   (return lastpop))
	       (setq lastpop (cdr pop))))))))



(defmethod destroy-mirror ((port xt-port) sheet)
  (tk::destroy-widget (sheet-direct-mirror sheet)))

(defmethod realize-mirror ((port xt-port) sheet)
  (let ((parent (find-widget-parent port sheet)))
    (multiple-value-bind (class initargs)
	(find-widget-class-and-initargs-for-sheet port parent sheet)
      (let ((widget (apply #'make-instance class
			   :parent parent
			   :managed (sheet-enabled-p sheet)
			   initargs)))
	(initialize-mirror port sheet widget)
	widget))))

(defmethod initialize-mirror ((port xt-port) sheet widget)
  (add-sheet-callbacks port sheet widget))

(defmethod add-sheet-callbacks ((port xt-port) sheet (widget t))
  (declare (ignore sheet)))

(defmethod sheet-mirror-event-handler (widget event sheet)
  (multiple-value-bind (same-p root child root-x root-y native-x native-y mask)
      (tk::query-pointer (tk::widget-window widget))
    (declare (ignore same-p root child root-x root-y))
    (let ((modifiers (logand #16rff mask))
	  (button (ash mask -8)))
      #+ignore
      (format excl:*initial-terminal-io* "Got event ~s~%" (tk::event-type event))
      (let ((clim-event
	     (ecase (tk::event-type event)
	       ((:map-notify :unmap-notify :selection-clear :selection-request
		 :selection-notify :client-message :mapping-notify)
		nil)
	       (:configure-notify
		(sheet-mirror-resized-callback
		 widget nil event sheet)
		nil)
	       ;; This is handled by the expose callback.
	       (:expose
		nil)
	       (:expose
		;; This gets called only for an XmBulletinBoard widget.
		(sheet-mirror-exposed-callback
		 widget 
		 nil ; window
		 event
		 sheet)
		nil)
	       (:key-press
		(multiple-value-bind (character keysym)
		    (lookup-character-and-keysym sheet widget event)
		  (make-instance 'key-press-event
				 :key-name keysym
				 :character character
				 :sheet sheet
				 :modifiers
				 (state->modifiers
				  (x11::xkeyevent-state event)
				  nil))))
	       (:key-release
		(multiple-value-bind (character keysym)
		    (lookup-character-and-keysym sheet widget event)
		  (make-instance 'key-release-event
				 :key-name keysym
				 :character character
				 :sheet sheet
				 :modifiers 
				 (state->modifiers
				  (x11::xkeyevent-state event)
				  nil))))
	       (:button-press
		(make-instance 'pointer-button-press-event
			       :sheet sheet
			       :x :??
			       :y :??
			       :modifiers
			       (state->modifiers
				(x11::xkeyevent-state event))
			       :button (x-button->silica-button 
					(x11::xbuttonevent-button event))
			       :native-x (x11::xbuttonevent-x event)
			       :native-y (x11::xbuttonevent-y event)))
	       (:button-release
		(make-instance 'pointer-button-release-event
			       :sheet sheet
			       :x :??
			       :y :??
			       :modifiers
			       (state->modifiers
				(x11::xkeyevent-state event))
			       :button (x-button->silica-button 
					(x11::xbuttonevent-button event))
			       :native-x (x11::xbuttonevent-x event)
			       :native-y (x11::xbuttonevent-y event)))
	       (:leave-notify
		(make-instance 'pointer-exit-event
			       :native-x native-x
			       :native-y native-y
			       :button (x-button->silica-button button)
			       :modifiers
			       (state->modifiers
				(x11::xkeyevent-state event))
			       :sheet sheet))
	       (:enter-notify
		(make-instance 'pointer-enter-event
			       :native-x native-x
			       :native-y native-y
			       :button (x-button->silica-button button)
			       :modifiers 
			       (state->modifiers
				(x11::xkeyevent-state event))
			       :sheet sheet))
	       (:motion-notify
		(make-instance 'pointer-motion-event
			       :native-x native-x
			       :native-y native-y
			       :button (x-button->silica-button button)
			       :modifiers 
			       (state->modifiers
				(x11::xkeyevent-state event))
			       :sheet sheet)))))
	(when clim-event
	  (distribute-event
	   (port sheet)
	   clim-event))))))

(defun x-button->silica-button (button)
  (ecase button
    (0 nil)
    (4 (warn "got an event 4")
       +pointer-right-button+)
    (#.x11::button3 +pointer-right-button+)
    (#.x11::button2 +pointer-middle-button+)
    (#.x11::button1 +pointer-left-button+)))

(defmethod sheet-mirror-resized-callback (widget window event sheet)
  (declare (ignore widget window event))
  (dispatch-event
    sheet
    (let ((r (mirror-region (port sheet) sheet)))
      (make-instance 'window-configuration-event
		     :native-region r
		     :region (untransform-region (sheet-native-transformation sheet) r)
		     :sheet sheet))))

(defmethod sheet-mirror-exposed-callback (widget window event sheet)
  (declare (ignore widget window))
  (let* ((minx (x11::xexposeevent-x event))
	 (miny (x11::xexposeevent-y event))
	 (width (x11::xexposeevent-width event))
	 (height (x11::xexposeevent-height event))
	 (maxx (+ minx width))
	 (maxy (+ miny height)))
    #+ignore
    (format excl:*initial-terminal-io* "Got expose event ~s~%"
	    (tk::event-type event))
    (dispatch-repaint
      sheet
      (make-instance 'window-repaint-event
		     :native-region (make-bounding-rectangle minx miny maxx maxy)
		     :region (untransform-region
			       (sheet-native-transformation sheet)
			       (make-bounding-rectangle minx miny maxx maxy))
		     :sheet sheet))))

(defmethod sheet-mirror-input-callback (widget window event sheet)
  (declare (ignore window))
  (ecase (tk::event-type event)
    (:key-press
      (distribute-event
	(port sheet)
	(multiple-value-bind (character keysym)
	    (lookup-character-and-keysym sheet widget event)
	  (make-instance 'key-press-event
			 :key-name keysym
			 :character character
			 :sheet sheet
			 :modifiers
			 (state->modifiers
			  (x11::xkeyevent-state event)
			  nil)))))
    (:key-release
      (distribute-event
	(port sheet)
	(multiple-value-bind (character keysym)
	    (lookup-character-and-keysym sheet widget event)
	  (make-instance 'key-release-event
			 :key-name keysym
			 :character character
			 :sheet sheet
			 :modifiers
			 (state->modifiers
			  (x11::xkeyevent-state event)
			  nil)))))
    (:button-press
      (distribute-event
	(port sheet)
	(make-instance 'pointer-button-press-event
		       :sheet sheet
		       :x :??
		       :y :??
		       :modifiers 
		       (state->modifiers
			(x11::xkeyevent-state event))
		       :button (x-button->silica-button 
				 (x11::xbuttonevent-button event))
		       :native-x (x11::xbuttonevent-x event)
		       :native-y (x11::xbuttonevent-y event))))
    (:button-release
      (distribute-event
	(port sheet)
	(make-instance 'pointer-button-release-event
		       :sheet sheet
		       :x :??
		       :y :??
		       :modifiers
		       (state->modifiers
			(x11::xkeyevent-state event))
		       :button (x-button->silica-button 
				 (x11::xbuttonevent-button event))
		       :native-x (x11::xbuttonevent-x event)
		       :native-y (x11::xbuttonevent-y event))))))

(defmethod find-widget-class-and-initargs-for-sheet
    ((port xt-port) (parent t) (sheet sheet))
  (error "we should not be here"))

(defmethod find-widget-class-and-initargs-for-sheet :around 
	   ((port xt-port) (parent t) (sheet sheet))
  (multiple-value-bind (class initargs)
      (call-next-method)
    (setq initargs (compute-initial-mirror-geometry parent sheet initargs))
    (values class initargs)))


(defmethod compute-initial-mirror-geometry (parent sheet initargs)
  ;;--- Should we pass in the size of the sheet even though it is
  ;; liable to be quite stupid
  ;; We really want to just create the gadgets and then let the layout
  ;; stuff do everything
  (unless (getf initargs :x)
    (multiple-value-bind (left top right bottom)
	(sheet-actual-native-edges* sheet)
      ;;--- We do not want to specify the x,y if this is a top-level
      ;;sheet.
      (unless (typep parent 'tk::shell)
	(setf (getf initargs :x) (fix-coordinate left)
	      (getf initargs :y) (fix-coordinate top)))
      ;;--- We should not do this, see realize-mirror :around in mirror
      #+ignore
      (setf (getf initargs :width)  (fix-coordinate (- right left))
	    (getf initargs :height) (fix-coordinate (- bottom top)))))
  initargs)

;; If we are creating a top level sheet then we have to create a shell for it
(defmethod find-widget-parent ((port xt-port) sheet)
  (let ((ma (sheet-mirrored-ancestor sheet)))
    (if (graftp ma)
	(multiple-value-bind (class initargs)
	    (find-shell-class-and-initargs port sheet)
	  (apply #'make-instance class
		 :parent (find-shell-parent port sheet)
		 :name (string (frame-name (pane-frame sheet)))
		 initargs))
        (sheet-mirror ma))))

(defmethod find-shell-of-calling-frame ((sheet sheet))
  (find-shell-of-calling-frame (pane-frame sheet)))

(defmethod find-shell-of-calling-frame ((frame application-frame))
  (let (cf)
    (and (setq cf (frame-calling-frame frame))
	 (setq cf (frame-top-level-sheet cf))
	 (sheet-shell cf))))

(defmethod find-shell-parent (port sheet)
  (or (and  ;;--- hack alert
       (popup-frame-p sheet)
       (find-shell-of-calling-frame sheet))
      (port-application-shell port)))

(defmethod find-shell-class-and-initargs ((port xt-port) (sheet t))
  (values 'top-level-shell 
	  ;; Need this so that an interactive pane can have children
	  ;; but still accept the focus
	  `(:keyboard-focus-policy :pointer)))

(defmethod find-shell-class-and-initargs :around ((port xt-port) (sheet pane))
  (multiple-value-bind
      (class initargs)
      (call-next-method)
    (values class
	    `(:allow-shell-resize ,(clim-internals::frame-resizable (pane-frame sheet))
				  ,@initargs))))

(defmethod enable-mirror ((port xt-port) sheet)
  (let ((mirror (sheet-mirror sheet)))
    (enable-xt-widget (widget-parent mirror) mirror)))

(defmethod enable-xt-widget ((parent t) (mirror t))
  (manage-child mirror))

(defmethod enable-xt-widget ((parent top-level-shell) (mirror t))
  (manage-child mirror)
  (popup (widget-parent mirror)))

(defmethod disable-mirror ((port xt-port) sheet)
  (declare (ignore port))
  (let ((mirror (sheet-mirror sheet)))
    (when mirror
      (disable-xt-mirror (widget-parent mirror) mirror))))

(defmethod disable-xt-mirror ((parent t) (mirror t))
  (tk::unmanage-child mirror))

(defmethod disable-xt-mirror ((parent top-level-shell) (mirror t))
  (tk::popdown parent))

(defmethod realize-graft ((port xt-port) graft)
  ;; Set the width etc etc
  (setf (sheet-direct-mirror graft) (port-application-shell port))
  (let* ((display (port-display port))
	 (screen (x11:xdefaultscreen display)))
    (with-slots (silica::mm-height silica::mm-width
		 silica::pixel-height silica::pixel-width
		 silica::pixels-per-point) graft
      ;;-- If anyone cared we could just grab the screen and call the
      ;;-- accessors on that
      (setq silica::mm-width (x11:xdisplaywidthmm display screen)
	    silica::mm-height (x11:xdisplayheightmm display screen)
	    silica::pixel-width (x11::xdisplaywidth display screen)
	    silica::pixel-height (x11::xdisplayheight display screen)
	    silica::pixels-per-point (float (/ silica::pixel-width
					       (* 72 (/ silica::mm-width 25.4)))))
      ;;--- Mess with the region
      (setf (sheet-region graft)
	(ecase (graft-units graft)
	  ((:device :pixel)
	   (make-bounding-rectangle 0 0
				    silica::pixel-width silica::pixel-height))))
      ;;-- what about the transformation
      (setf (sheet-native-transformation graft) +identity-transformation+))))

(defmethod mirror-region* ((port xt-port) sheet)
  (when (sheet-mirror sheet)
    (multiple-value-bind (x y width height)
	(get-values (sheet-mirror sheet) :x :y :width :height)
      (values (coordinate x) (coordinate y) 
	      (coordinate (+ x width)) (coordinate (+ y height))))))

(defmethod mirror-region ((port xt-port) sheet)
  (multiple-value-call #'make-bounding-rectangle
    (mirror-region* port sheet)))

(defmethod mirror-inside-region* ((port xt-port) sheet)
  (multiple-value-bind (minx miny maxx maxy)
      (mirror-region* port sheet)
    (values (coordinate 0) (coordinate 0) 
	    (- maxx minx) (- maxy miny))))

(defmethod mirror-native-edges* ((port xt-port) sheet)
  (let* ((mirror (sheet-direct-mirror sheet)))
    (multiple-value-bind (x y width height)
	(get-values mirror :x :y :width :height)
      (values (coordinate x) (coordinate y)
	      (coordinate (+ x width)) (coordinate (+ y height))))))

(defmethod mirror-inside-edges* ((port xt-port) sheet)
  (multiple-value-bind (a b c d)
      (mirror-native-edges* port sheet)
    (values (coordinate 0) (coordinate 0)
	    (- c a) (- d b))))

(defparameter  *compare-widget-geometry-with-intention* nil)

(defmethod set-sheet-mirror-edges* ((port xt-port) sheet 
						   target-left target-top
						   target-right target-bottom)
  (let ((w (- target-right  target-left))
	(h (- target-bottom target-top))
	(mirror (sheet-direct-mirror sheet)))
    (setf target-left (fix-coordinate target-left)
	  target-top  (fix-coordinate target-top)
	  w (fix-coordinate w)
	  h (fix-coordinate h))
    (change-widget-geometry
     ;;--- For top level sheets the sheet-parent is the graft whose
     ;; mirror is the application shell
     (tk::widget-parent mirror)
     mirror
     :x target-left
     :y target-top
     :width w
     :height h)
    (when *compare-widget-geometry-with-intention*
      (multiple-value-bind
	  (nx ny nw nh)
	  (tk::get-values mirror :x :y :width :height)
	(when (or (/= target-left nx)
		  (/= target-top ny)
		  (/= w nw)
		  (/= h nh))
	  (let ((*error-output* excl:*initial-terminal-io*))
	    (warn "Geo set fail, ~S, ~S,~S"
		  sheet
		  (list  target-left  target-top w h)
		  (list nx ny nw nh))))))))

(defmacro with-port-event-lock ((port) &body body)
  `(clim-sys:with-lock-held ((port-event-lock ,port))
     ,@body))

(defmethod process-next-event ((port xt-port) &key wait-function timeout)
  (with-slots (context event-lock) port
    (let ((context context))
      (multiple-value-bind (mask reason)
	  (tk::wait-for-event context
			      :wait-function wait-function
			      :timeout timeout)
	(clim-sys:with-lock-held (event-lock)
	  ;; Make sure there is still an event ready, so we don't block in C.
	  (multiple-value-setq (mask reason)
	    (tk::wait-for-event context
				:wait-function wait-function
				:timeout timeout))
	  (tk::process-one-event context mask reason))))))



(defmethod port-glyph-for-character ((port xt-port)
				     character text-style 
				     &optional our-font)
  (let* ((index (char-int character))
	 (x-font (or our-font
		     (text-style-mapping port text-style)))
	 (escapement-x (tk::char-width x-font index))
	 (escapement-y 0)
	 (origin-x 0)
	 (origin-y (tk::font-ascent x-font))
	 (bb-x escapement-x)
	 (bb-y (+ origin-y (tk::font-descent x-font))))
    (when (zerop escapement-x) (break))
    (values index x-font escapement-x escapement-y
	    origin-x origin-y bb-x bb-y)))

(defmethod text-style-mapping :around ((device xt-port) text-style
				       &optional (character-set *standard-character-set*) etc)
  (declare (ignore etc))
  (let ((font (call-next-method)))
    (when (or (stringp font) (symbolp font))
      (let* ((font-name (string font)))
	(setf font (make-instance 'tk::font 
				  :display (port-display device)
				  :name font-name))
	(setf (text-style-mapping device (parse-text-style text-style) character-set)
	      font)))
    font))


(defmethod text-style-width ((text-style standard-text-style) (port xt-port))
  (tk::font-width (text-style-mapping port text-style)))

(defmethod text-style-height ((text-style standard-text-style) (port xt-port))
  (+ (text-style-ascent text-style port)
     (text-style-descent text-style port)))

(defmethod text-style-ascent ((text-style standard-text-style) (port xt-port))
  (tk::font-ascent (text-style-mapping port text-style)))
					
(defmethod text-style-descent ((text-style standard-text-style) (port xt-port))
  (tk::font-descent (text-style-mapping port text-style)))
					
(defmethod stream-set-input-focus (stream)
  nil)

(defmethod port-finish-output ((port xt-port))
  nil)

(defmethod change-widget-geometry (parent child &rest args)
  (declare (ignore parent))
  ;; In this case let the parent deal with it
  (apply #'tk::set-values child args))


(defmethod popup-frame-p ((frame application-frame))
  (typep frame '(or clim-internals::menu-frame clim-internals::accept-values-own-window)))

(defmethod popup-frame-p ((sheet sheet))
  (let ((frame  (pane-frame sheet)))
    (and frame
	 (popup-frame-p frame))))


;;; Keysym stuff

(defvar *xt-keysym->clim-keysym-table* (make-hash-table))
(defvar *clim-keysym->xt-keysym-table* (make-hash-table))

(defmacro define-xt-keysym (xt-keysym clim-keysym)
  `(progn 
     (setf (gethash ,xt-keysym *xt-keysym->clim-keysym-table*) ',clim-keysym)
     (unless (gethash ,clim-keysym *clim-keysym->xt-keysym-table*)
       (setf (gethash ,clim-keysym *clim-keysym->xt-keysym-table*) ',xt-keysym))))

(defun-inline xt-keysym->keysym (xt-keysym)
  (gethash xt-keysym *xt-keysym->clim-keysym-table*))

(defun-inline keysym->xt-keysym (keysym)
  (gethash keysym *clim-keysym->xt-keysym-table*))

;; The standard characters
(define-xt-keysym 032 :space)
(define-xt-keysym 033 :\!)
(define-xt-keysym 034 :\")
(define-xt-keysym 035 :\#)
(define-xt-keysym 036 :\$)
(define-xt-keysym 037 :\%)
(define-xt-keysym 038 :\&)
(define-xt-keysym 039 :\')
(define-xt-keysym 040 :\()
(define-xt-keysym 041 :\))
(define-xt-keysym 042 :\*)
(define-xt-keysym 043 :\+)
(define-xt-keysym 044 :\,)
(define-xt-keysym 045 :\-)
(define-xt-keysym 046 :\.)
(define-xt-keysym 047 :\/)
(define-xt-keysym 048 :\0)
(define-xt-keysym 049 :\1)
(define-xt-keysym 050 :\2)
(define-xt-keysym 051 :\3)
(define-xt-keysym 052 :\4)
(define-xt-keysym 053 :\5)
(define-xt-keysym 054 :\6)
(define-xt-keysym 055 :\7)
(define-xt-keysym 056 :\8)
(define-xt-keysym 057 :\9)
(define-xt-keysym 058 :\:)
(define-xt-keysym 059 :\;)
(define-xt-keysym 060 :\<)
(define-xt-keysym 061 :\=)
(define-xt-keysym 062 :\>)
(define-xt-keysym 063 :\?)
(define-xt-keysym 064 :\@)
(define-xt-keysym 065 :\A)
(define-xt-keysym 097 :\A)
(define-xt-keysym 066 :\B)
(define-xt-keysym 098 :\B)
(define-xt-keysym 067 :\C)
(define-xt-keysym 099 :\C)
(define-xt-keysym 068 :\D)
(define-xt-keysym 100 :\D)
(define-xt-keysym 069 :\E)
(define-xt-keysym 101 :\E)
(define-xt-keysym 070 :\F)
(define-xt-keysym 102 :\F)
(define-xt-keysym 071 :\G)
(define-xt-keysym 103 :\G)
(define-xt-keysym 072 :\H)
(define-xt-keysym 104 :\H)
(define-xt-keysym 073 :\I)
(define-xt-keysym 105 :\I)
(define-xt-keysym 074 :\J)
(define-xt-keysym 106 :\J)
(define-xt-keysym 075 :\K)
(define-xt-keysym 107 :\K)
(define-xt-keysym 076 :\L)
(define-xt-keysym 108 :\L)
(define-xt-keysym 077 :\M)
(define-xt-keysym 109 :\M)
(define-xt-keysym 078 :\N)
(define-xt-keysym 110 :\N)
(define-xt-keysym 079 :\O)
(define-xt-keysym 111 :\O)
(define-xt-keysym 080 :\P)
(define-xt-keysym 112 :\P)
(define-xt-keysym 081 :\Q)
(define-xt-keysym 113 :\Q)
(define-xt-keysym 082 :\R)
(define-xt-keysym 114 :\R)
(define-xt-keysym 083 :\S)
(define-xt-keysym 115 :\S)
(define-xt-keysym 084 :\T)
(define-xt-keysym 116 :\T)
(define-xt-keysym 085 :\U)
(define-xt-keysym 117 :\U)
(define-xt-keysym 086 :\V)
(define-xt-keysym 118 :\V)
(define-xt-keysym 087 :\W)
(define-xt-keysym 119 :\W)
(define-xt-keysym 088 :\X)
(define-xt-keysym 120 :\X)
(define-xt-keysym 089 :\Y)
(define-xt-keysym 121 :\Y)
(define-xt-keysym 090 :\Z)
(define-xt-keysym 122 :\Z)
(define-xt-keysym 091 :\[)
(define-xt-keysym 092 :\\)
(define-xt-keysym 093 :\])
(define-xt-keysym 094 :\^)
(define-xt-keysym 095 :\_)
(define-xt-keysym 096 :\`)
(define-xt-keysym 123 :\{)
(define-xt-keysym 124 :\|)
(define-xt-keysym 125 :\})
(define-xt-keysym 126 :\~)

;; The semi-standard characters
(defmacro keysym (b &rest more)
  (dolist (n more)
    (setq b (+ (ash b 8) n)))
  b)

(define-xt-keysym (keysym 255 013) :return)
(define-xt-keysym (keysym 255 009) :tab)
(define-xt-keysym (keysym 255 255) :rubout)
(define-xt-keysym (keysym 255 008) :backspace)
(define-xt-keysym (keysym 009 227) :page)
(define-xt-keysym (keysym 255 010) :linefeed)

;; Other useful characters
(define-xt-keysym (keysym 255 087) :end)
(define-xt-keysym (keysym 255 105) :abort)
(define-xt-keysym (keysym 255 106) :help)
(define-xt-keysym (keysym 255 104) :complete)
(define-xt-keysym (keysym 255 086) :scroll)
(define-xt-keysym (keysym 255 097) :refresh)
(define-xt-keysym (keysym 255 011) :clear-input)

;; Finally, the shifts
;; snarfed from translate.cl

(defconstant left-shift-keysym  (keysym 255 225))
(defconstant right-shift-keysym (keysym 255 226))
(defconstant left-control-keysym  (keysym 255 227))
(defconstant right-control-keysym (keysym 255 228))
(defconstant caps-lock-keysym  (keysym 255 229))
(defconstant shift-lock-keysym (keysym 255 230))
(defconstant left-meta-keysym  (keysym 255 231))
(defconstant right-meta-keysym (keysym 255 232))
(defconstant left-alt-keysym  (keysym 255 233))
(defconstant right-alt-keysym (keysym 255 234))
(defconstant left-super-keysym  (keysym 255 235))
(defconstant right-super-keysym (keysym 255 236))
(defconstant left-hyper-keysym  (keysym 255 237))
(defconstant right-hyper-keysym (keysym 255 238))

(define-xt-keysym left-shift-keysym  :left-shift)
(define-xt-keysym right-shift-keysym :right-shift)
(define-xt-keysym left-control-keysym  :left-control)
(define-xt-keysym right-control-keysym :right-control)
(define-xt-keysym caps-lock-keysym  :caps-lock)
(define-xt-keysym shift-lock-keysym :shift-lock)
(define-xt-keysym left-meta-keysym  :left-meta)
(define-xt-keysym right-meta-keysym :right-meta)
(define-xt-keysym left-super-keysym  :left-super)
(define-xt-keysym right-super-keysym :right-super)
(define-xt-keysym left-hyper-keysym  :left-hyper)
(define-xt-keysym right-hyper-keysym :right-hyper)

(defun lookup-character-and-keysym (sheet mirror event)
  (declare (ignore sheet mirror))
  (multiple-value-bind (character keysym)
      (tk::lookup-string event)
    (setq character (and (= (length character) 1) (aref character 0)))
    ;;--- Map the asci control-characters into the common lisp
    ;;--- control characters except where they are special!
    ;; Also deal with the modifier bits
    ;; This gets stuff wrong because for example to type-< you have to
    ;; use the shift key and so instead for m-< you aget m-sh-<
    ;; Perhaps there is a way of checking to see whether shifting
    ;; makes sense given the keyboard layout!
    (when character
      (let ((x (state->modifiers
		(x11::xkeyevent-state event))))
	(setq character (cltl1:make-char
			 (if (and (<= (char-int character) 26)
				  (not (member character 
					       '(#\return 
						 #\tab
						 #\page
						 #\backspace
						 #\linefeed
						 #\escape))))
			     (cltl1::int-char
			      (+ (cltl1::char-int character)
				 (1- (if (logtest x +shift-key+)
					 (char-int #\A)
				       (char-int #\a)))))
			   character)
			 ;;-- Should deal with hyper and super also
			 (logior
			  (if (logtest x +control-key+)
			      cltl1:char-control-bit 0)
			  (if (logtest x +meta-key+)
			      cltl1:char-meta-bit 0))))))
    (values character (xt-keysym->keysym keysym))))


(defun state->modifiers (x &optional (shift-it t))
  (logior
    (if (and shift-it (logtest x x11:shiftmask)) +shift-key+ 0)
    (if (logtest x x11:controlmask) +control-key+ 0)
    (if (logtest x x11:mod1mask) +meta-key+ 0)
    (if (logtest x x11:mod2mask) +super-key+ 0)
    (if (logtest x x11:mod3mask) +hyper-key+ 0)))

;;;---- This is just an experiment

;(defmethod engraft-medium :before ((medium t) (port xt-port) (pane clim-stream-pane))
;  (default-from-mirror-resources port pane))
;
;(defmethod engraft-medium :before ((medium t) (port xt-port) (pane top-level-sheet))
;  (default-from-mirror-resources port pane))


(defmethod engraft-medium :before ((medium t) (port xt-port) 
				   (pane clim-internals::output-protocol-mixin))
  (default-from-mirror-resources port pane))

(defmethod engraft-medium :before ((medium t) (port xt-port) (pane viewport))
  (default-from-mirror-resources port pane))


;;-- What do we do about pixmap streams. I guess they should inherit
;;-- properties from the parent.

;;--- Unless the foreground, background and default-text-style have
;;--- been set we want to query the resource database for the values
;; Either (a) ask a widget or (b) Do it directly.
;; Well it looks like we have to use a widget
;; If we wanted to get a font then we are in trouble because

(defun default-from-mirror-resources (port pane)
  (unless (and (medium-foreground pane)
	       (medium-background pane))
    (let* ((w (sheet-mirror pane))
	   (shellp (typep w 'tk::shell)))
      ;;; Make sure we dont have a pixmp
      (when (typep w 'xt::xt-root-class)
	;;-- What about the case when there is a pixmap
	(multiple-value-bind
	    (foreground background)
	    (if shellp
		(values nil (tk::get-values w :background))
	      (tk::get-values w :foreground :background))
	  ;; Now we have to convert into CLIM colors.
	  (flet ((ccm (x)
		   (multiple-value-bind
		       (r g b)
		       (tk::query-color (tk::default-colormap (port-display port)) x)
		     (let ((x #.(1- (ash 1 16))))
		       (make-rgb-color 
			(/ r x)
			(/ g x)
			(/ b x))))))
	    (when foreground
	      (unless (medium-foreground pane)
		(setf (medium-foreground pane) (ccm foreground))))
	    (unless (medium-background pane)
	      (setf (medium-background pane) (ccm background)))))))))

;;;--- Gadget activation deactivate

(defmethod realize-mirror :around ((port xt-port) (sheet gadget))
  (let ((widget (call-next-method)))
    (unless (gadget-active-p sheet)
      (xt::set-sensitive widget nil))
    widget))


(defmethod port-force-output ((port xt-port))
  (x11:xflush (port-display port)))



;;;


(defclass xt-geometry-manager ()
	  ;; --- This is probably all
	  ;; composites excepts drawing-area and shell
	  ()
  (:documentation "These are all parents that have strong feelings
about their children"))


(defmethod update-mirror-transformation-1 ((port port) sheet 
					   (parent xt-geometry-manager))
  nil)

(defmethod update-mirror-region-1 ((port port) sheet 
				   (parent xt-geometry-manager))
  nil)
	    

(defmethod update-mirror-transformation-1 :after ((port port)
						  (sheet xt-geometry-manager)
						  (parent t))
  (update-geo-manager-sheet-children sheet))


(defmethod update-mirror-region-1 :after ((port port)
					  (sheet xt-geometry-manager)
					  (parent t))
  (update-geo-manager-sheet-children sheet))

(defmethod update-geo-manager-sheet-children (geo-manager)
  (dolist (child (sheet-children geo-manager))
    ;;--- Yuck!
    (when (typep child 'mirrored-sheet-mixin)
      (mirror-region-updated (port geo-manager) child))))
