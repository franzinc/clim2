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
;; $fiHeader: xt-silica.lisp,v 1.5 92/02/08 14:51:43 cer Exp Locker: cer $

(in-package :xm-silica)

(defclass xt-port (port)
	  ((application-shell :reader port-application-shell)
	   (display :reader port-display)
	   (context :reader port-context)
	   (copy-gc :initform nil)
	   )
  (:documentation "The port for X intrinsics based ports"))

(defmethod port-copy-gc ((port xt-port))
  (with-slots (copy-gc display) port
    (or copy-gc
	(setf copy-gc
	  (make-instance 'tk::gcontext
			 :display display
			 :handle (x11:screen-default-gc (tk::display-default-screen (port-display port))))))))

(defmethod initialize-instance :after ((port xt-port) &key server-path)
  (destructuring-bind
      (ignore &key display) server-path
    (declare (ignore ignore))
    (multiple-value-bind
	(context display application-shell)
	(initialize-motif-toolkit display)
      (setf (slot-value port 'application-shell) application-shell
	    (slot-value port 'context) context
	    (slot-value port 'display) display)
      (initialize-xlib-display-device port display))))

(defvar *clx-font-families* '((:fix "*-courier-*")
			      (:sans-serif "*-helvetica-*")
			      (:serif "*-charter-*" "*-new century schoolbook-*"
			       "*-times-*")))

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

(defmethod initialize-xlib-display-device (display-device display)
  
  (setf (silica::device-undefined-text-style display-device)
    (silica::standardize-text-style display-device
			    *standard-character-set*
			    (make-text-style
			     :stand-in-for-undefined-style :roman
			     10)))
  
  (let* ()
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
      (dolist (family-stuff *clx-font-families*)
	(let ((family (car family-stuff)))
	  (dolist (font-pattern (cdr family-stuff))
	    (dolist (xfont (tk::list-font-names display font-pattern))
	      (let ((text-style (font->text-style xfont family)))
		;; prefer first font satisfying this text style, so
		;; don't override if we've already defined one.
		(unless (silica::text-style-mapping-exists-p
			 display-device *standard-character-set*
			 text-style t)
		  (setf (text-style-mapping  display-device *standard-character-set*
					     text-style)
		    xfont)))))
	  ;; Now build the logical size alist for the family
	  
	  
	  ))
      
      (let (temp)
	(cond ((setq temp
		 (dolist (family *clx-font-families*)
		   (when (silica::text-style-mapping-exists-p display-device 
						      *standard-character-set*
						      `(,(car family) :roman 10))
		     (return (make-text-style (car family) :roman
					      10)))))
		  (setf (text-style-mapping display-device *standard-character-set*
				       (silica::device-undefined-text-style display-device))
				       #+ignore *undefined-text-style*
				       temp))

	      ;; Perhaps we should look for some other conveniently sized
	      ;; fonts.
	
	      (*clx-fallback-font*
	       (setf (text-style-mapping display-device 
				       *standard-character-set*
				       (silica::device-undefined-text-style display-device))
				       #+ignore *undefined-text-style*
				       (make-instance 'tk::font 
						      :display display
						       :name *clx-fallback-font*)))
	
	;;; Perhaps we should just grab the first font we can find.
	
	      (t
	       (error "Unable to determine default font")))))))

(defparameter *clx-logical-size-alist*
	      '((:tiny       6)
		(:very-small 8)
		(:small	     10)
		(:normal     12)
		(:large	     14)
		(:very-large 18)
		(:huge	     24)))

(defmethod silica::standardize-text-style ((display-device xt-port) character-set style)
  (silica::standardize-text-style-1
   display-device style character-set *clx-logical-size-alist*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod silica::destroy-mirror ((port xt-port) sheet)
  (tk::destroy-widget (sheet-direct-mirror sheet)))

(defmethod realize-mirror ((port xt-port) sheet)
  (let ((parent (find-widget-parent port sheet)))
    (multiple-value-bind
	(class initargs)
	(find-widget-class-and-initargs-for-sheet port parent sheet)
      (let ((widget (apply #'make-instance class
			   :parent parent
			   :managed (sheet-enabled-p sheet)
			   initargs)))
	(add-sheet-callbacks port sheet widget)
	widget))))

(defmethod add-sheet-callbacks ((port xt-port) sheet (widget t))
  (declare (ignore sheet)))

(defmethod sheet-mirror-event-handler (widget event sheet)
  (multiple-value-bind
      (same-p root child root-x root-y native-x native-y mask)
      (tk::query-pointer (tk::widget-window widget))
    (declare (ignore same-p root child root-x root-y))
    (let ((modifiers (logand #16rff mask))
	  (button (ash mask -8)))
      (let ((clim-event
	     (ecase (tk::event-type event)
	       ((:map-notify :unmap-notify)
		nil)
	       (:configure-notify
		(sheet-mirror-resized-callback
		 widget nil event sheet)
		nil)
	       (:expose
		(sheet-mirror-exposed-callback
		 widget 
		 nil ; window
		 event
		 sheet)
		nil)
	       (:key-press
		(multiple-value-bind
		    (ignore character keysym)
		    (tk::lookup-string event)
		  (declare (ignore ignore))
		  (make-instance 'key-press-event
				 :key-name keysym
				 :character (and (= (length character) 1)
						 (aref character 0))
				 :sheet sheet
				 :modifiers (x11::xkeyevent-state event))))
    
	       (:key-release
		(multiple-value-bind
		    (ignore character keysym)
		    (tk::lookup-string event)
		  (declare (ignore ignore))
		  (make-instance 'key-release-event
				 :key-name keysym
				 :character (and (= (length character) 1)
						 (aref character 0))
				 :sheet sheet
				 :modifiers (x11::xkeyevent-state event))))
    
	       (:button-press
		(make-instance 'pointer-button-press-event
			       :sheet sheet
			       :x :??
			       :y :??
			       :modifiers (x11::xkeyevent-state event)
			       :button (x-button->silica-button 
					(x11::xbuttonevent-button event))
			       :native-x (x11::xbuttonevent-x event)
			       :native-y (x11::xbuttonevent-y event)))
	       (:button-release
		(make-instance 'pointer-button-release-event
			       :sheet sheet
			       :x :??
			       :y :??
			       :modifiers (x11::xkeyevent-state event)
			       :button (x-button->silica-button 
					(x11::xbuttonevent-button event))
			       :native-x (x11::xbuttonevent-x event)
			       :native-y (x11::xbuttonevent-y event)))
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
			       :sheet sheet)))))
	(when clim-event
	  (distribute-event
	   (sheet-port sheet)
	   clim-event))))))

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

(defmethod find-widget-class-and-initargs-for-sheet (port
						     (parent t)
						     (sheet sheet))
  (declare (ignore port))
  (values 'xm-drawing-area (list :resize-policy :grow)))

(defmethod find-widget-class-and-initargs-for-sheet :around (port
							     (parent t)
							     (sheet sheet))
  (declare (ignore port))
  (multiple-value-bind
      (class initargs)
      (call-next-method)
    (setq initargs (set-mirror-geometry parent sheet initargs))
    (values class initargs)))


(defmethod set-mirror-geometry (parent sheet initargs)
  ;;--- Should we pass in the size of the sheet even though it is
  ;; liable to be quite stupid
  ;; We really want to just create the gadgets and then let the layout
  ;; stuff do everything
  (unless (getf initargs :x)
    (multiple-value-bind
	(left top right bottom)
	(sheet-actual-native-edges sheet)
      ;;--- We do not want to specify the x,y if this is a top-level
      ;;sheet.
      (unless (typep parent 'tk::shell)
	(setf (getf initargs :x) (floor left)
	      (getf initargs :y) (floor top)))
      (setf (getf initargs :width) (floor (- right left))
	    (getf initargs :height) (floor (- bottom top)))))
  initargs)

;; If we are creating a top level sheet then we have to create a shell
;; for it

(defmethod find-widget-parent (port sheet)
  (let ((ma (sheet-mirrored-ancestor sheet)))
    (if (graftp ma)
	(multiple-value-bind
	    (class initargs)
	    (find-shell-class-and-initargs port sheet)
	  (apply #'make-instance class
		 :parent (find-shell-parent port sheet)
		 initargs))
      (sheet-mirror ma))))

(defmethod find-shell-of-calling-frame ((sheet sheet))
  (find-shell-of-calling-frame (pane-frame sheet)))

(defmethod find-shell-of-calling-frame ((frame application-frame))
  (let (cf)
    (and (setq cf (clim-internals::frame-calling-frame frame))
	 (sheet-shell (frame-top-level-sheet cf)))))

(defmethod find-shell-parent (port sheet)
  (or (and  ;;--- hack alert
       (popup-frame-p sheet)
       (find-shell-of-calling-frame sheet))
      (port-application-shell port)))


(defmethod find-shell-class-and-initargs (port sheet)
  (declare (ignore port))
  (values 'top-level-shell 
	  ;; Need this so that an interactive pane can have children
	  ;; but still accept the focus
	  '(:keyboard-focus-policy :pointer)))




(defmethod enable-mirror (port sheet)
  (declare (ignore port))
  (let ((mirror (sheet-mirror sheet)))
    (typecase (widget-parent mirror)
      (null)
      ((or top-level-shell tk::xm-dialog-shell)
       ;;--- this is a nasty hack just to make sure that the child is managed.
       ;; top-level-sheets are created unmanaged because they are
       ;; disabled to we have to do not!
       (manage-child mirror)
       (popup (widget-parent mirror)))
      (t
       (manage-child mirror)))))

(defmethod disable-mirror (port sheet)
  (declare (ignore port))
  (let ((mirror (sheet-mirror sheet)))
    (when mirror
      (typecase (widget-parent mirror)
	(null)
	(top-level-shell
	 (tk::popdown (widget-parent mirror)))
	(t
	 (tk::unmanage-child mirror))))))

(defmethod realize-graft ((port xt-port) graft)
  ;; Set the width etc etc
  (setf (sheet-direct-mirror graft) (port-application-shell port))
  ;; Mess with the region
  (warn "Do something about the graft")
  (setf (sheet-region graft)
    (make-bounding-rectangle 0 0 1100 850))
  ;; Mess with the native transformation
  )

(defmethod mirror-region* ((port xt-port) sheet)
  (when (sheet-mirror sheet)
    (multiple-value-bind
	(x y width height)
	(get-values (sheet-mirror sheet) :x :y :width :height)
      (values x y 
	      (+ x width)
	      (+ y height)))))

(defmethod mirror-region ((port xt-port) sheet)
  (multiple-value-call #'make-bounding-rectangle
    (mirror-region* port sheet)))

(defmethod mirror-inside-region* ((port xt-port) sheet)
  (multiple-value-bind
      (minx miny maxx maxy)
      (mirror-region* port sheet)
    (values 0 0 (- maxx minx) (- maxy miny))))

(defmethod mirror-native-edges* ((port xt-port) sheet)
  (let* ((mirror (sheet-direct-mirror sheet)))
    (multiple-value-bind
	(x y width height)
	(get-values mirror :x :y :width :height)
      (values x y (+ x width) (+ y height)))))

(defmethod mirror-inside-edges* ((port xt-port) sheet)
  (multiple-value-bind
      (a b c d)
      (mirror-native-edges* port sheet)
    (values 0 0 (- c a) (- d b))))

(defmethod set-sheet-mirror-edges* ((port xt-port) sheet 
				    target-left target-top
				    target-right target-bottom)
  (let ((w (- target-right  target-left))
	(h (- target-bottom target-top)))
    (setf target-left (floor target-left)
	  target-top (floor target-top)
	  w (floor w)
	  h (floor h))
    (change-widget-geometry
     ;;--- For top level sheets the sheet-parent is the graft whose
     ;; mirror is the application shell
     (sheet-mirror (sheet-parent sheet))
     (sheet-direct-mirror sheet)
     :x target-left
     :y target-top
     :width w
     :height h)
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


(defmethod port-force-output ((port xt-port))
  nil)


(defmethod port-glyph-for-character ((port xt-port)
					     character
					     appearance 
					     &optional
					     our-font)
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

(defmethod text-size (medium string &key text-style start end)
  (when (characterp string)
    (setq string (string string)
	  start 0
	  end nil))
  (unless start (setq start 0))
  (unless end (setq end (length string)))
  (clim-internals::stream-string-output-size medium string
			     :start start :end end :text-style text-style))
  

  
(defmethod realize-text-style (port font)
  (text-style-mapping port nil font nil))



(defmethod text-style-mapping :around ((device xt-port)
				       character-set 
				       text-style &optional etc)
  (declare (ignore etc))
  (let ((font (call-next-method)))
    (when (or (stringp font) (symbolp font))
      (let* ((font-name (string font)))
	(setf font (make-instance 'tk::font 
				    :display (port-display device)
				    :name font-name))
	(setf (text-style-mapping
	       device character-set (parse-text-style text-style))
	  font)))
    font))


;; This seem specific to the type of the medium

(defmethod text-style-width ((text-style text-style) medium)
  (tk::font-width (realize-text-style (sheet-port medium) text-style)))

(defmethod text-style-ascent ((text-style text-style) medium)
  (tk::font-ascent (realize-text-style (sheet-port medium) text-style)))
					
(defmethod text-style-descent ((text-style text-style) medium)
  (tk::font-descent (realize-text-style (sheet-port medium) text-style)))
					
(defmethod text-style-height ((text-style text-style) medium)
  (+ (text-style-ascent text-style medium)
     (text-style-descent text-style medium)))

#+ignore
(ff::defforeign 'xtsetkeyboardfocus
    :entry-point "_XtSetKeyboardFocus")

(defun sheet-top-level-mirror (stream)
  (let ((last-mirror nil))
    (loop
      (when (graftp stream) (return last-mirror))
      (when (sheet-direct-mirror stream) 
	(setq last-mirror stream))
      (setq stream (sheet-parent stream)))))

(defmethod stream-set-input-focus (stream)
  nil)


(defmethod clim-internals::port-finish-output ((port xt-port))
  nil)

(defmethod change-widget-geometry (parent child &rest args)
  (declare (ignore parent))
  ;; In this case let the parent deal with it
  (apply #'tk::set-values child args))


(defmethod popup-frame-p ((frame application-frame))
  (typep frame '(or clim-internals::menu-frame clim-internals::accept-values-own-window)))

(defmethod popup-frame-p ((sheet sheet))
  (popup-frame-p (pane-frame sheet)))
