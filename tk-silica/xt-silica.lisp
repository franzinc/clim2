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
;; $fiHeader: xt-silica.lisp,v 1.70 93/03/04 19:02:28 colin Exp $

(in-package :xm-silica)

(defclass xt-port (basic-port)
    ((application-shell :reader port-application-shell)
     (display :reader port-display)
     (context :reader port-context)     
     (copy-gc :initform nil)
     (copy-gc-depth-1 :initform nil)
     (stipples :initform nil :accessor port-stipples)
     ;; This next is true for servers like Suns, which (pretty much) always
     ;; can safely copy-area without generating graphics-exposures.
     (safe-backing-store :initform nil :accessor port-safe-backing-store)
     (event-lock :initform (clim-sys:make-lock "port event lock")
		 :reader port-event-lock)
     (rotated-font-cache :initform nil :accessor port-rotated-font-cache)
     (depth :accessor port-depth)
     (visual-class :accessor port-visual-class)
     (cursor-font :initform nil)
     (cursor-cache :initform nil))
  (:default-initargs :allow-loose-text-style-size-mapping t
		     :deep-mirroring t)
  (:documentation "The port for X intrinsics based ports"))

(defmethod port-type ((port xt-port))
  ':xt)


(defmethod port-copy-gc ((port xt-port))
  ;;-- Assume 1-1 port-graft mapping?
  (with-slots (copy-gc display) port
    (or copy-gc
	(setf copy-gc
	  (make-instance 'tk::gcontext
	    :display display
	    :graphics-exposures :on
	    :foreign-address (x11:screen-default-gc
 			      (x11:xdefaultscreenofdisplay display)))))))

;;--- I don't know of a better way of getting a depth 1 drawable
;;other than by making a dummy depth 1 pixmap
(defmethod port-copy-gc-depth-1 ((port xt-port))
  ;;-- Assume 1-1 port-graft mapping?
  (with-slots (copy-gc-depth-1 display) port
    (or copy-gc-depth-1
	(let ((pixmap (make-instance 'tk::pixmap
				     :drawable
				     (tk::display-root-window display)
				     :depth 1 :width 1 :height 1)))
	  (prog1
	      (setf copy-gc-depth-1
		(make-instance 'tk::gcontext :drawable pixmap))
	    (tk::destroy-pixmap pixmap))))))


(defmethod restart-port ((port xt-port))
  (flet ((cleanup-port-after-dumplisp (process)
	   (mp::process-kill process)))
    (let ((process (port-process port)))
      (when process
	(clim-sys:destroy-process process))
      (setq process
	(mp:process-run-restartable-function
	 (list :name (format nil "CLIM Event Dispatcher for ~A"
			     (port-server-path port))
	       :priority 1000)
	 #'port-event-loop port))
      (setf (getf (mp:process-property-list process) ':survive-dumplisp) #'cleanup-port-after-dumplisp)
      (setf (port-process port) process))))

(defmethod port-event-loop :around ((port xt-port))
  (handler-case (call-next-method)
    (tk::x-connection-lost (condition)
      (port-terminated port condition)
      (mp:process-kill mp:*current-process*))))

       
(defparameter *use-color* t)		; For debugging monochrome

(defvar *unreliable-server-vendors*
    '("Solbourne Computer, Inc" "Network Computing Devices"
      "Tektronix"))
 
(defmethod initialize-instance :after ((port xt-port) &key server-path)
  (handler-case
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
		  (slot-value port 'display) display
		  (port-depth port) (x11:xdefaultdepth display (tk::display-screen-number display))
		  (port-visual-class port) (tk::screen-root-visual-class (tk::default-screen display))
		  (slot-value port 'silica::default-palette) 
		  (make-xt-palette port (tk::default-colormap (port-display port))))
	    (let* ((screen (x11:xdefaultscreenofdisplay display))
		   (bs-p (not (zerop (x11::screen-backing-store screen))))
		   (su-p (not (zerop (x11::screen-save-unders screen))))
		   (vendor (ff:char*-to-string (x11::display-vendor display))))
	      (if (and bs-p su-p
		       ;; An amazing crock.  XXX
		       (notany #'(lambda (x) (search x vendor)) *unreliable-server-vendors*))
		  (setf (slot-value port 'safe-backing-store) t)))
	    (initialize-xlib-port port display))))
    (tk::x-connection-lost (condition)
      (port-terminated port condition))))


(defmethod port-color-p ((port xt-port))
  (and *use-color*
       (> (port-depth port) 2)
       (member (port-visual-class port)
	       '(:static-color :true-color :pseudo-color :direct-color))
       t))

(defvar *xt-font-families* '((:fix "*-*-courier-*-*-*-*-*-*-*-*-*-*-*-*")
			     (:sans-serif "*-*-helvetica-*-*-*-*-*-*-*-*-*-*-*-*")
			     (:serif 
			      ;; This causes problems on OpenWindows 3.0!
			      ;; "*-*-charter-*-*-*-*-*-*-*-*-*-*-*-*"
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

(defmethod initialize-xlib-port ((port xt-port) display)
  (let* ((screen (x11:xdefaultscreen display))
	 ;;-- This is a property of the graft
	 (screen-pixels-per-inch
	  (* 25.4 (/ (x11::xdisplayheight display screen)
		     (x11:xdisplayheightmm display screen)))))
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
		    (point-size (* (float designed-point-size)
				   (/ designed-y-resolution
				      screen-pixels-per-inch)))
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
	       (error "Unable to determine default font"))))))
  (setup-stipples port display))

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



(defmethod destroy-mirror ((port xt-port) (sheet mirrored-sheet-mixin))
  ;; Only do this if its the top most widget being destroyed or we are
  ;; screwing around with the tree in someway
  (tk::destroy-widget (sheet-direct-mirror sheet)))

(defmethod destroy-mirror :after ((port xt-port) (sheet sheet-parent-mixin))
  (labels ((loose-em (sheet)
	   (dolist (child (sheet-children sheet))
	     (when (sheet-direct-mirror child)
	       (setf (sheet-direct-mirror child) nil))
	     (when (typep child 'sheet-parent-mixin)
	       (loose-em child)))))
    (loose-em sheet)))

(defmethod realize-widget-mirror ((port xt-port) (parent-sheet t) parent-widget sheet)
  (multiple-value-bind (class initargs)
      (find-widget-class-and-initargs-for-sheet port parent-widget sheet)
    (apply #'make-instance class
	   :parent parent-widget
	   :managed nil			; See note below
	   initargs)))

(defmethod realize-mirror ((port xt-port) sheet)
  (let* ((parent-widget (find-widget-parent port sheet))
	 (parent (sheet-parent sheet))
	 (widget (realize-widget-mirror port parent parent-widget sheet)))
    (initialize-mirror port parent parent-widget sheet widget)
    widget))

;; You may wonder why this is being done in this perverse way
;; Well its because if you create a managed child of ScrollingWindow then the
;; scrolling window calls its query geometry method which can result
;; in compose-space being invoked and the mirror<->sheet mapping has
;; not been established nor has the rest of tree been mirrored.
;; So it seems to work out really well to do this bottom up.

(defmethod sheet-and-ancestors-enabled-p ((sheet basic-sheet))
  ;; If we have an non-mirrored ancestor that is between us and our
  ;; mirrored-ancestor that is disabled then we should not manage this gadget.
  ;; This should happen
  (and (sheet-enabled-p sheet)
       (do ((parent (sheet-parent sheet) (sheet-parent parent)))
	   (nil)
	 (when (or (null parent) (sheet-direct-mirror parent)) (return t))
	 (unless (sheet-enabled-p parent) (return nil)))))


(defmethod note-sheet-tree-grafted :after ((port xt-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (sheet-direct-mirror sheet)))
    (when (and (sheet-and-ancestors-enabled-p sheet)
	       (typep mirror 'xt::xt-root-class)) ; Pixmap streams are
						  ; mirrored by a
						  ; pixmap rather than
						  ; by a widget
      (tk::manage-child mirror))))

(defmethod initialize-mirror ((port xt-port) (parent basic-sheet) (parent-widget t)
			      (sheet basic-sheet) widget)
  (add-sheet-callbacks port sheet widget))

(defmethod add-sheet-callbacks ((port xt-port) sheet (widget t))
  (declare (ignore sheet)))

(defmethod sheet-mirror-event-handler (widget event sheet)
  #+ignore
  (format excl:*initial-terminal-io* "Got event ~s~%" (tk::event-type event))
  (let ((port (port sheet))
	(clim-event nil))
    (when port
      (setq clim-event
	(ecase (tk::event-type event)
	  ((:map-notify :unmap-notify :selection-clear :selection-request
	    :selection-notify :client-message :mapping-notify :no-expose)
	   nil)
	  (:configure-notify
	   (sheet-mirror-resized-callback widget nil event sheet)
	   nil)
	  (:graphics-expose
	   ;; Tricky!
	   (setf (port-safe-backing-store port) nil)
	   nil)
	  (:expose
	   ;; This gets called only for an XmBulletinBoard widget.
	   (sheet-mirror-exposed-callback widget nil event sheet)
	   nil)
	  (:key-press
	   (multiple-value-bind (character keysym)
	       (lookup-character-and-keysym sheet widget event)
	     (let ((keysym-shift-mask
		    (if (typep keysym 'modifier-keysym)
			(make-modifier-state
			 (case keysym
			   ((:left-shift :right-shift) :shift)
			   ((:left-control :right-control) :control)
			   ((:left-meta :right-meta) :meta)
			   ((:left-super :right-super) :super)
			   ((:left-hyper :right-hyper) :hyper)))
		      0)))
	       (allocate-event 'key-press-event
			       :sheet sheet
			       :key-name keysym
			       :character character
			       :modifier-state
			       (logior
				  (state->modifiers (x11::xkeyevent-state event))
				  keysym-shift-mask)))))
	  (:key-release
	   (multiple-value-bind (character keysym)
	       (lookup-character-and-keysym sheet widget event)
	     (let ((keysym-shift-mask
		    (if (typep keysym 'modifier-keysym)
			(make-modifier-state
			 (case keysym
			   ((:left-shift :right-shift) :shift)
			   ((:left-control :right-control) :control)
			   ((:left-meta :right-meta) :meta)
			   ((:left-super :right-super) :super)
			   ((:left-hyper :right-hyper) :hyper)))
		      0)))
	       (allocate-event 'key-release-event
			       :sheet sheet
			       :key-name keysym
			       :character character
			       :modifier-state 
			       (logandc2
				  (state->modifiers (x11::xkeyevent-state event))
				  keysym-shift-mask)))))
	  (:button-press
	   (let ((button (x-button->silica-button 
			  (x11::xbuttonevent-button event)))
		 (pointer (port-pointer port)))
	     (allocate-event 'pointer-button-press-event
			     :sheet sheet
			     :pointer pointer
			     :button button
			     :native-x (x11::xbuttonevent-x event)
			     :native-y (x11::xbuttonevent-y event)
			     :x :?? :y :??
			     :modifier-state 
			     (state->modifiers
				(x11::xbuttonevent-state event)))))
	  (:button-release
	   (let ((button (x-button->silica-button 
			  (x11::xbuttonevent-button event)))
		 (pointer (port-pointer port)))
	     (allocate-event 'pointer-button-release-event
			     :sheet sheet
			     :pointer pointer
			     :button button
			     :native-x (x11::xbuttonevent-x event)
			     :native-y (x11::xbuttonevent-y event)
			     :x :?? :y :??
			     :modifier-state 
			     (state->modifiers
				(x11::xkeyevent-state event)))))
	  (:leave-notify
	   (allocate-event 'pointer-exit-event
			   :sheet sheet
			   :native-x (x11:xcrossingevent-x event)
			   :native-y (x11:xcrossingevent-y event)
			   :kind (boundary-detail->kind
				  (x11:xcrossingevent-detail event))
			   :pointer (port-pointer port)
			   :modifier-state 
			   (state->modifiers (x11::xcrossingevent-state event))))
	  (:enter-notify
	   (allocate-event 'pointer-enter-event
			   :pointer (port-pointer port)
			   :sheet sheet
			   :native-x (x11:xcrossingevent-x event)
			   :native-y (x11:xcrossingevent-y event)
			   :kind (boundary-detail->kind
				  (x11:xcrossingevent-detail event))
			   :modifier-state
			   (state->modifiers (x11::xcrossingevent-state event))))
	  (:motion-notify
	   (multiple-value-bind (same-p root child root-x root-y
				 native-x native-y state)
	       (tk::query-pointer (tk::widget-window widget))
	     (declare (ignore same-p root child))
	     (allocate-event 'pointer-motion-event
			     :pointer (port-pointer port)
			     :sheet sheet
			     :x root-x	; These are actually unused...
			     :y root-y	; ""
			     :native-x native-x
			     :native-y native-y
			     :modifier-state 
			     (state->modifiers state)))))))
    (when clim-event
      (distribute-event port clim-event))))

(defun x-button->silica-button (button)
  (case button
    (#.x11::button1 +pointer-left-button+)
    (#.x11::button2 +pointer-middle-button+)
    (#.x11::button3 +pointer-right-button+)
    (#.x11::button4 +pointer-left-button+) ; These two are arbitrary.
    (#.x11::button5 +pointer-left-button+)))

(defun boundary-detail->kind (detail)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum detail))
  (case detail
    (#.x11:notifyancestor :ancestor)
    (#.x11:notifyvirtual  :virtual)
    (#.x11:notifyinferior :inferior)
    (#.x11:notifynonlinear :nonlinear)
    (#.x11:notifynonlinearvirtual :nonlinear-virtual)))


(defmethod sheet-mirror-resized-callback (widget window event sheet)
  (declare (ignore widget window event))
  (dispatch-event
    sheet
    (let ((r (mirror-region (port sheet) sheet)))
      (allocate-event 'window-configuration-event
	:native-region r
	:region (untransform-region (sheet-native-transformation sheet) r)
	:sheet sheet))))

(defmethod sheet-mirror-exposed-callback (widget window event sheet)
  ;; This isn't really the right place to do this, but it's better than
  ;; in ensure-blinker-for-cursor.
  (declare (ignore window))
  (let ((window (tk::widget-window widget)))
    (unless (getf (window-property-list window) 'backing-store-on)
      (setf (getf (window-property-list window) 'backing-store-on) t
	    (xt::window-backing-store window)                      t)))
  (let* ((minx (x11::xexposeevent-x event))
	 (miny (x11::xexposeevent-y event))
	 (width (x11::xexposeevent-width event))
	 (height (x11::xexposeevent-height event))
	 (maxx (+ minx width))
	 (maxy (+ miny height)))
    #+ignore
    (format excl:*initial-terminal-io* "Got expose event ~s~%"
	    (tk::event-type event))
    (queue-repaint
      sheet
      (allocate-event 'window-repaint-event
	:native-region (make-bounding-rectangle minx miny maxx maxy)
	:region (untransform-region
		  (sheet-native-transformation sheet)
		  (make-bounding-rectangle minx miny maxx maxy))
	:sheet sheet))))

(defmethod sheet-mirror-input-callback (widget window event sheet)
  (declare (ignore window))
  (let ((port (port sheet))
	(event-key (tk::event-type event)))
    (ecase event-key
      ((:key-press :key-release)
       (distribute-event
	port
	(multiple-value-bind (character keysym)
	    (lookup-character-and-keysym sheet widget event)
	  (let* ((keysym-shift-mask
		  (if (typep keysym 'modifier-keysym)
		      (make-modifier-state
		       (case keysym
			 ((:left-shift :right-shift) :shift)
			 ((:left-control :right-control) :control)
			 ((:left-meta :right-meta) :meta)
			 ((:left-super :right-super) :super)
			 ((:left-hyper :right-hyper) :hyper)))
		    0))
		 (modifier-state
		  (if (eq event-key :key-press)
		      (logior (state->modifiers (x11::xkeyevent-state event))
			      keysym-shift-mask)
		    (logandc2 (state->modifiers (x11::xkeyevent-state event))
			      keysym-shift-mask)))
		 ;;-- Canonicalize the only interesting key right here.
		 ;;--  If we get a key labelled "Return", we canonicalize it
		 ;;-- into #\Newline.
		 ;;-- This may be misguided, but it'll almost certainly help us
		 ;;-- in the short run.
		 ;;-- This code copied from clx-mirror.
		 (char (cond ((and (eq keysym ':return)
				   (or (zerop modifier-state)
				       (= modifier-state
					  (make-modifier-state :shift))))
			      #\Newline)
			     (t character))))
	    (allocate-event (if (eq event-key :key-press)
				'key-press-event
			      'key-release-event)
			    :sheet sheet
			    :key-name keysym
			    :character char
			    :modifier-state modifier-state)))))
      (:button-press
       (let ((button (x-button->silica-button 
		      (x11::xbuttonevent-button event)))
	     (pointer (port-pointer port)))
	 (distribute-event
	  port
	  (allocate-event 'pointer-button-press-event
			  :sheet sheet
			  :pointer pointer
			  :button button
			  :native-x (x11::xbuttonevent-x event)
			  :native-y (x11::xbuttonevent-y event)
			  ;;-- Filled in by distributor
			  :x :?? :y :??
			  :modifier-state (state->modifiers
					     (x11::xbuttonevent-state event))))))
      (:button-release
       (let ((button (x-button->silica-button 
		      (x11::xbuttonevent-button event)))
	     (pointer (port-pointer port)))
	 (distribute-event
	  port
	  (allocate-event 'pointer-button-release-event
			  :sheet sheet
			  :pointer pointer
			  :button button
			  :native-x (x11::xbuttonevent-x event)
			  :native-y (x11::xbuttonevent-y event)
			  :x :?? :y :??
			  :modifier-state (state->modifiers
					     (x11::xbuttonevent-state event))))))
      )))

(defmethod find-widget-class-and-initargs-for-sheet
    ((port xt-port) (parent t) (sheet basic-sheet))
  (error "we should not be here"))

(defmethod find-widget-class-and-initargs-for-sheet :around 
	   ((port xt-port) (parent t) (sheet basic-sheet))
  (multiple-value-bind (class initargs)
      (call-next-method)
    (setq initargs (compute-initial-mirror-geometry parent sheet
						    initargs)) 
    ;;; it seems that xt or motif or someone doesn't know about
    ;;; ParentRelative and tries to give treat this as a pixmap
    ;;; and put it into a gcontext tile
    #+ignore (setf (getf initargs :background-pixmap) x11:parentrelative)
    (values class initargs)))


(defmethod compute-initial-mirror-geometry (parent sheet initargs)
  ;;--- Should we pass in the size of the sheet even though it is
  ;; liable to be quite stupid
  ;; We really want to just create the gadgets and then let the layout
  ;; stuff do everything
  (unless (getf initargs :x)
    (multiple-value-bind (left top right bottom)
	(sheet-actual-native-edges* sheet)
      bottom right
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
	  (let ((frame (pane-frame sheet)))
	    (when frame
	      (setf (getf initargs :name) (string (frame-name frame))
		    (getf initargs :title) (frame-pretty-name frame))))
	  (apply #'make-instance class :parent (find-shell-parent port sheet) initargs))
        (sheet-mirror-for-parenting ma))))

(defmethod sheet-mirror-for-parenting ((sheet basic-sheet))
  ;; There might be a situation where a sheet is mirrored by a whil
  (sheet-mirror sheet))

(defmethod find-shell-of-calling-frame ((sheet basic-sheet))
  (and (pane-frame sheet)
       (find-shell-of-calling-frame (pane-frame sheet))))

(defmethod find-shell-of-calling-frame ((frame application-frame))
  (let (cf)
    (and (setq cf (frame-calling-frame frame))
	 (setq cf (frame-top-level-sheet cf))
	 (sheet-shell cf))))

(defmethod find-shell-parent ((port xt-port) sheet)
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
  (let* ((palette (frame-manager-palette (frame-manager (pane-frame sheet)))))
    (multiple-value-bind
	(class initargs)
	(call-next-method)
      (values class
	      `(:allow-shell-resize ,(and (pane-frame sheet)
					  (clim-internals::frame-resizable (pane-frame sheet)))
				    ,@initargs 
				    ,@(and (not (eq (port-default-palette port) palette))
					   `(:colormap ,(palette-colormap palette))))))))


(defmethod enable-mirror ((port xt-port) (sheet t))
  (let ((mirror (sheet-mirror sheet)))
    (enable-xt-widget (widget-parent mirror) mirror)))

(defmethod enable-xt-widget ((parent t) mirror)
  (unless (xt::is-managed-p mirror)
    (manage-child mirror)))

;(defmethod enable-mirror ((port xt-port) (sheet top-level-sheet))
;  (let* ((mirror (sheet-mirror sheet))
; 	 (parent (widget-parent mirror)))
;    (manage-child mirror)
;    (xt:realize-widget parent)		; Make sure widget is realized.
;    (let ((ussp (slot-value sheet 'silica::user-specified-size-p))
; 	  (uspp (slot-value sheet 'silica::user-specified-position-p)))
;      (unless (and (eq ussp :unspecified)
; 		   (eq uspp :unspecified))
; 	(let* ((window (tk::widget-window parent))
; 	       (display (tk::widget-display parent))
; 	       (size-hints (x11::xallocsizehints)))
; 	  (tk::with-ref-par ((supplied 0))
; 	    (when (zerop
; 		   (x11::xgetwmnormalhints display window size-hints supplied))
; 	      (warn "top-level-sheet had no size hints?!")
; 	      (return-from enable-mirror))
; 	    (let ((flags (x11::xsizehints-flags size-hints)))
; 	      (if* (and uspp (not (eq uspp :unspecified)))
; 		 then (setf flags (logior flags x11::uspositionhint))
; 	       elseif (null uspp)
; 		 then (setf flags (logandc2 flags x11::uspositionhint)))
; 	      (if* (and ussp (not (eq ussp :unspecified)))
; 		 then (setf flags (logior flags x11::ussizehint))
; 	       elseif (null ussp)
; 		 then (setf flags (logandc2 flags x11::ussizehint)))
; 	      (setf (x11::xsizehints-flags size-hints) flags))
; 	    (x11::xsetwmnormalhints display window size-hints)))))
;    (popup parent)))
 
;(defmethod enable-xt-widget ((parent top-level-shell) mirror)
;  (manage-child mirror)
;  (popup (widget-parent mirror)))

(defmethod disable-mirror ((port xt-port) sheet)
  (let ((mirror (sheet-mirror sheet)))
    (when mirror
      (disable-xt-mirror (widget-parent mirror) mirror))))

(defmethod disable-xt-mirror ((parent t) mirror)
  (tk::unmanage-child mirror))

(defmethod disable-xt-mirror ((parent top-level-shell) mirror)
  (declare (ignore mirror))
  (popdown parent))

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
				:timeout 0))
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
    (when (zerop escapement-x) 
      (setq escapement-x (tk::font-width x-font)))
    (values index x-font escapement-x escapement-y
	    origin-x origin-y bb-x bb-y)))


(defmethod text-style-mapping :around ((port xt-port) text-style
				       &optional (character-set *standard-character-set*) etc)
  (declare (ignore etc))
  (let ((font (call-next-method)))
    (when (or (stringp font) (symbolp font))
      (let* ((font-name (string font)))
	(setf font (make-instance 'tk::font 
				  :display (port-display port)
				  :name font-name))
	(setf (text-style-mapping port (parse-text-style text-style) character-set)
	      font)))
    font))

(defmethod change-widget-geometry (parent child &rest args)
  (declare (ignore parent))
  ;; In this case let the parent deal with it
  (apply #'tk::set-values child args))


(defmethod change-widget-geometry :around (parent child &key x y width height)
  (declare (ignore parent))
  ;;-- Nasty use of (widget-callback-data child) field
  (unless (and (typep x '(signed-byte 16))
	       (typep (+ x width) '(signed-byte 16))
	       (typep y '(signed-byte 16))
	       (typep (+ y height) '(signed-byte 16)))
    (let ((window (tk::widget-window child nil)))
      (when (and window (not (assoc 'widget-is-cached (tk::widget-callback-data child))))
	(push (cons 'widget-is-cached t) (tk::widget-callback-data child))
	(x11::xunmapwindow (xt::widget-display child) window)))
    (return-from change-widget-geometry nil))
  ;; Make sure its mapped
  (let ((x (assoc 'widget-is-cached (tk::widget-callback-data child))))
    (when x 
      (setf (tk::widget-callback-data child)  (delete x (tk::widget-callback-data child)))
      (let ((window (tk::widget-window child nil)))
	(when window (x11::xmapwindow (xt::widget-display child) window)))))
  ;; 
  (call-next-method))

(defmethod popup-frame-p ((frame application-frame))
  (typep frame '(or clim-internals::menu-frame
		 clim-internals::accept-values-own-window)))

;;-- Is this the right thing to do?
;;-- In particular it causes the frame to have a dialog shell.
;;-- It should really be a secondary shell.
#+ignore
(defmethod popup-frame-p ((frame activity-frame))
  t)

(defmethod popup-frame-p ((sheet basic-sheet))
  (let ((frame (pane-frame sheet)))
    (and frame
	 (popup-frame-p frame))))

;;; Keysym stuff

(defvar *xt-keysym->clim-keysym-table* (make-hash-table))
(defvar *clim-keysym->xt-keysym-table* (make-hash-table))

(defmacro define-xt-keysym (xt-keysym clim-keysym)
  `(progn 
     (setf (gethash ,xt-keysym *xt-keysym->clim-keysym-table*) ,clim-keysym)
     (unless (gethash ,clim-keysym *clim-keysym->xt-keysym-table*)
       (setf (gethash ,clim-keysym *clim-keysym->xt-keysym-table*) ,xt-keysym))))

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
(define-xt-keysym 065 :a)
(define-xt-keysym 097 :a)
(define-xt-keysym 066 :b)
(define-xt-keysym 098 :b)
(define-xt-keysym 067 :c)
(define-xt-keysym 099 :c)
(define-xt-keysym 068 :d)
(define-xt-keysym 100 :d)
(define-xt-keysym 069 :e)
(define-xt-keysym 101 :e)
(define-xt-keysym 070 :f)
(define-xt-keysym 102 :f)
(define-xt-keysym 071 :g)
(define-xt-keysym 103 :g)
(define-xt-keysym 072 :h)
(define-xt-keysym 104 :h)
(define-xt-keysym 073 :i)
(define-xt-keysym 105 :i)
(define-xt-keysym 074 :j)
(define-xt-keysym 106 :j)
(define-xt-keysym 075 :k)
(define-xt-keysym 107 :k)
(define-xt-keysym 076 :l)
(define-xt-keysym 108 :l)
(define-xt-keysym 077 :m)
(define-xt-keysym 109 :m)
(define-xt-keysym 078 :n)
(define-xt-keysym 110 :n)
(define-xt-keysym 079 :o)
(define-xt-keysym 111 :o)
(define-xt-keysym 080 :p)
(define-xt-keysym 112 :p)
(define-xt-keysym 081 :q)
(define-xt-keysym 113 :q)
(define-xt-keysym 082 :r)
(define-xt-keysym 114 :r)
(define-xt-keysym 083 :s)
(define-xt-keysym 115 :s)
(define-xt-keysym 084 :t)
(define-xt-keysym 116 :t)
(define-xt-keysym 085 :u)
(define-xt-keysym 117 :u)
(define-xt-keysym 086 :v)
(define-xt-keysym 118 :v)
(define-xt-keysym 087 :w)
(define-xt-keysym 119 :w)
(define-xt-keysym 088 :x)
(define-xt-keysym 120 :x)
(define-xt-keysym 089 :y)
(define-xt-keysym 121 :y)
(define-xt-keysym 090 :z)
(define-xt-keysym 122 :z)
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
(define-xt-keysym (keysym 255 027) :escape)
;;;---
(define-xt-keysym (keysym 255 010) :newline)

;; Other useful characters
(define-xt-keysym (keysym 255 #x6a) :help)
(define-xt-keysym (keysym 255 #xde) :end)

(define-xt-keysym (keysym 255 #x68) :complete)
(define-xt-keysym (keysym 255 #x69) :abort)
(define-xt-keysym (keysym 255 #x56) :scroll)
(define-xt-keysym (keysym 255 #x61) :refresh)
(define-xt-keysym (keysym 255 #x0b) :clear-input)

(define-xt-keysym (keysym 255 #x51) :left)
(define-xt-keysym (keysym 255 #x52) :up)
(define-xt-keysym (keysym 255 #x53) :right)
(define-xt-keysym (keysym 255 #x54) :down)


;;;


;;; Some nonstandard keys

(define-xt-keysym (keysym 255 #xbe) :f1)
(define-xt-keysym (keysym 255 #xbf) :f2)
(define-xt-keysym (keysym 255 #xc0) :f3)
(define-xt-keysym (keysym 255 #xc1) :f4)
(define-xt-keysym (keysym 255 #xc2) :f5)
(define-xt-keysym (keysym 255 #xc3) :f6)
(define-xt-keysym (keysym 255 #xc4) :f7)
(define-xt-keysym (keysym 255 #xc5) :f8)
(define-xt-keysym (keysym 255 #xc6) :f9)
(define-xt-keysym (keysym 255 #xc7) :f10)

(define-xt-keysym (keysym 255 #xc8) :l1)
(define-xt-keysym (keysym 255 #xc9) :l2)
(define-xt-keysym (keysym 255 #xca) :l3)
(define-xt-keysym (keysym 255 #xcb) :l4)
(define-xt-keysym (keysym 255 #xcc) :l5)
(define-xt-keysym (keysym 255 #xcd) :l6)
(define-xt-keysym (keysym 255 #xce) :l7)
(define-xt-keysym (keysym 255 #xcf) :l8)
(define-xt-keysym (keysym 255 #xd0) :l9)
(define-xt-keysym (keysym 255 #xd1) :l10)

(define-xt-keysym (keysym 255 #xd2) :r1)
(define-xt-keysym (keysym 255 #xd3) :r2)
(define-xt-keysym (keysym 255 #xd4) :r3)
(define-xt-keysym (keysym 255 #xd5) :r4)
(define-xt-keysym (keysym 255 #xd6) :r5)
(define-xt-keysym (keysym 255 #xd7) :r6)
(define-xt-keysym (keysym 255 #xd8) :r7)
(define-xt-keysym (keysym 255 #xd9) :r8)
(define-xt-keysym (keysym 255 #xda) :r9)
(define-xt-keysym (keysym 255 #xdb) :r10)
(define-xt-keysym (keysym 255 #xdc) :r11)
(define-xt-keysym (keysym 255 #xdd) :r12)
(define-xt-keysym (keysym 255 #xde) :r13)
(define-xt-keysym (keysym 255 #xdf) :r14)
(define-xt-keysym (keysym 255 #xe0) :r15)

;; Finally, the shifts
;; snarfed from translate.cl

(defconstant left-shift-keysym    (keysym 255 225))
(defconstant right-shift-keysym   (keysym 255 226))
(defconstant left-control-keysym  (keysym 255 227))
(defconstant right-control-keysym (keysym 255 228))
(defconstant caps-lock-keysym	  (keysym 255 229))
(defconstant shift-lock-keysym	  (keysym 255 230))
(defconstant left-meta-keysym	  (keysym 255 231))
(defconstant right-meta-keysym	  (keysym 255 232))
(defconstant left-alt-keysym	  (keysym 255 233))
(defconstant right-alt-keysym	  (keysym 255 234))
(defconstant left-super-keysym	  (keysym 255 235))
(defconstant right-super-keysym	  (keysym 255 236))
(defconstant left-hyper-keysym	  (keysym 255 237))
(defconstant right-hyper-keysym	  (keysym 255 238))

(define-xt-keysym left-shift-keysym    :left-shift)
(define-xt-keysym right-shift-keysym   :right-shift)
(define-xt-keysym left-control-keysym  :left-control)
(define-xt-keysym right-control-keysym :right-control)
(define-xt-keysym caps-lock-keysym     :caps-lock)
(define-xt-keysym shift-lock-keysym    :shift-lock)
(define-xt-keysym left-meta-keysym     :left-meta)
(define-xt-keysym right-meta-keysym    :right-meta)
(define-xt-keysym left-super-keysym    :left-super)
(define-xt-keysym right-super-keysym   :right-super)
(define-xt-keysym left-hyper-keysym    :left-hyper)
(define-xt-keysym right-hyper-keysym   :right-hyper)

;;;
(defun lookup-character-and-keysym (sheet mirror event)
  (declare (ignore sheet mirror)
	   (optimize (speed 3) (safety 0)))
  (multiple-value-bind (character keysym)
      (tk::lookup-string event)
    (setq character (and (= (length (the simple-string character)) 1)
			 (aref (the simple-string character) 0)))
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
						 #\escape)
					       :test #'eq)))
			     (cltl1::int-char
			      (+ (cltl1::char-int character)
				 (1- (if (logtest x +shift-key+)
					 (char-int #\A)
				       (char-int #\a)))))
			   character)
			 (logior
			  (if (logtest x +control-key+)
			      cltl1:char-control-bit 0)
			  (if (logtest x +meta-key+)
			      cltl1:char-meta-bit 0)
			  (if (logtest x +super-key+)
			      cltl1:char-super-bit 0)
			  (if (logtest x +hyper-key+)
			      cltl1:char-hyper-bit 0))))))
    (values character (xt-keysym->keysym keysym))))


(defun state->modifiers (x)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum x))
  (logior
    (if (logtest x x11:shiftmask) +shift-key+ 0)
    (if (logtest x x11:controlmask) +control-key+ 0)
    (if (logtest x x11:mod1mask) +meta-key+ 0)
    (if (logtest x x11:mod2mask) +super-key+ 0)
    (if (logtest x x11:mod3mask) +hyper-key+ 0)))


(defmethod find-widget-class 
    ((port xt-port) (parent t) (sheet mirrored-sheet-mixin))
  (multiple-value-bind (class initargs)
      (find-widget-class-and-initargs-for-sheet port parent sheet)
    (declare (ignore initargs))
    class))

(defmethod find-widget-class
    ((port xt-port) (parent t) (sheet basic-sheet))
  (class-name (class-of sheet)))

;;-- What do we do about pixmap streams. I guess they should inherit
;;-- properties from the parent.

;;; what's the deal with pixmap streams they don't seem to have
;;; parents??? This next method is probably wrong but at the moment
;;; it's the best I can come up with (cim)

(defmethod get-sheet-resources ((port xt-port) (sheet pixmap-stream))
  (multiple-value-bind (names classes)
      (xt::widget-resource-name-and-class (port-application-shell port))
    (get-xt-resources port names classes)))
  
(defmethod get-sheet-resources ((port xt-port) sheet)
  (let ((parent-widget (sheet-mirror (sheet-parent sheet))))
    (multiple-value-bind (parent-names parent-classes)
	(xt::widget-resource-name-and-class parent-widget)
      (let ((names
	     (append parent-names
		     (list (string (or (pane-name sheet) "")))))
	    (classes 
	     (append parent-classes
		     (list 
		      (string 
		       (or (find-widget-class port parent-widget sheet) ""))))))
	(get-xt-resources port names classes)))))

(defun get-xt-resources (port names classes)
  (let* ((display (silica::port-display port))
	 (db (tk::display-database display))
	 (palette (port-default-palette port))
	 (background (xt::get-resource db names "background"
				       classes "Background"))
	 (foreground (xt::get-resource db names "foreground" 
				       classes "Foreground"))
	 (text-style (xt::get-resource db names "textStyle"
				       classes "TextStyle")))
    `(,@(and background `(:background 
			  ,(find-named-color background palette)))
      ,@(and foreground `(:foreground 
			  ,(find-named-color foreground palette)))
      ,@(and text-style
	     `(:text-style
	       ,(let ((spec (read-from-string text-style)))
		  (etypecase spec
		    (cons (parse-text-style spec))
		    (string (silica::make-device-font 
			     port 
			     (make-instance 'tk::font 
					    :display display
					    :name (car (tk::list-font-names
							display spec))))))))))))

;;;--- Gadget activation deactivate

(defmethod realize-mirror :around ((port xt-port) (sheet basic-gadget))
  (let ((widget (call-next-method)))
    (unless (gadget-active-p sheet)
      (xt::set-sensitive widget nil))
    widget))

;; No longer a protocol function, but we need it internally
(defmethod port-force-output ((port xt-port))
  (x11:xflush (port-display port)))




;;; Tricky ground ahead!

(defclass xt-geometry-manager ()
	  ;; --- This is probably all
	  ;; composites excepts drawing-area and shell
	  ()
  (:documentation "These are all parents that have strong feelings
about their children. What this means is that CLIM does not control
the geometry of the children. Instead the parent has control. "))


;;; If you get the urge to change the geometry of the children dont.

(defmethod update-mirror-transformation-1 ((port port) sheet (parent xt-geometry-manager))
  (declare (ignore sheet))
  ;; This gets called by the mirror-region-updated code.
  ;; There is probably no harm in doing this anyway
  nil)

(defmethod update-mirror-region-1 ((port port) sheet (parent xt-geometry-manager))
  (declare (ignore sheet))
  ;;--- This gets called by the invalidate-cached .. code.
  ;;---  Surely if a sheet is mirrored then you do not need to
  ;;--- invalidate any caches below that point
  nil)

#+ignore
(defmethod initialize-mirror :after ((port xt-port)
				     (parent xt-geometry-manager)
				     (parent-widget t)
				     (sheet t)
				     (widget t))
  ;; A bogus attempt to get the initial sheet-region right.
  (sheet-mirror-resized-callback nil nil nil sheet))


;; Instead if the geometry of the parent has changed, I guess this
;; suggests that the children have changed shape and that we need to
;; update their geometry.  
;;;--- This seems quite bogus and what we actually need to have a
;;;--- configure-notify event handlers that deal with this.

(defmethod update-mirror-transformation-1 :after ((port port) (sheet xt-geometry-manager) (parent t))
  (update-geo-manager-sheet-children sheet))


(defmethod update-mirror-region-1 :after ((port port)
					  (sheet xt-geometry-manager)
					  (parent t))
  (update-geo-manager-sheet-children sheet))

(defmethod update-geo-manager-sheet-children (geo-manager)
  (declare (ignore geo-manager))
  ;;--- Should this really do anything???????
  #+ignore
  (dolist (child (sheet-children geo-manager))
    ;;--- Yuck!
    (when (typep child 'mirrored-sheet-mixin)
      (mirror-region-updated (port geo-manager) child))))

#|
;;


;; note-space-space-requirements-changed seems to do absolutely
;; nothing except recurse all of the way to the top.

(defmethod note-space-requirements-changed ((parent xt-geometry-manager) child)
  ;; We should now ask the parent to relayout the children
  ;; (compose-space child)
  ;; (xt-make-geometry-request), or make-resize-request...
  )

;; We want XtQueryGeometry to call out to Lisp do a compose-space and
;; return something meaningful.

;; If the parents QueryGeometry does the right thing and asks the
;; child then we are winning because we get the right numbers, except
;; this does not take into account min/max stuff.
  
|#


(defvar *dont-invoke-callbacks* nil)

(defmacro with-no-value-changed-callbacks (&body body)
  `(let ((*dont-invoke-callbacks* t))
     ,@body))

(defmethod queue-value-changed-event (widget sheet &optional (value (gadget-value sheet)))
  (declare (ignore widget))
  (unless *dont-invoke-callbacks*
    (distribute-event
     (port sheet)
     (allocate-event 'value-changed-gadget-event
		     :gadget sheet
		     :value value))))

(defmethod queue-losing-focus-event (widget sheet)
  (declare (ignore widget))
  (unless *dont-invoke-callbacks*
    (distribute-event
     (port sheet)
     (allocate-event 'focus-out-gadget-event
		     :gadget sheet))))

(defmethod queue-gaining-focus-event (widget sheet)
  (declare (ignore widget))
  (unless *dont-invoke-callbacks*
    (distribute-event
     (port sheet)
     (allocate-event 'focus-in-gadget-event
		     :gadget sheet))))

(defmethod queue-drag-event (widget sheet &optional (value (gadget-value sheet)))
  (declare (ignore widget))
  (distribute-event
   (port sheet)
   (allocate-event 'drag-gadget-event
     :gadget sheet
     :value value)))


(defun find-sheet-from-widget-address (address)
  (let* ((widget (tk::find-object-from-address address))
	 (display (tk::widget-display widget))
	 (port (find-port-from-display display)))
    (find-widget-sheet port widget)))

(defun find-widget-sheet (port widget &optional (errorp t))
  (cond ((gethash widget (port-mirror->sheet-table port)))
	((not errorp))
	(t (error "Could not find sheet for widget ~S" widget))))

      
(defun find-port-from-display (display)
  (find-if #'(lambda (port)
	       (and (typep port 'xt-port)
		    (eq (port-display port) display)))
	   *ports*))


(defmethod port-canonicalize-gesture-spec 
    ((port xt-port) gesture-spec &optional modifier-state)
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (keysym shifts)
      (if modifier-state
	  (values gesture-spec modifier-state)
	  (parse-gesture-spec gesture-spec))
    ;; Here, we must take the gesture spec, turn it back into
    ;; a keycode, then see what the keysyms are for that keycode
    (let ((x-keysym 
	   (if (characterp keysym)
	       (case keysym
		 (#\newline (keysym->xt-keysym :newline))
		 (#\tab (keysym->xt-keysym :tab))
		 (#\rubout (keysym->xt-keysym :rubout))
		 (#\return (keysym->xt-keysym :return))
		 (t (and (standard-char-p keysym)
			 (keysym->xt-keysym (xt-keysym->keysym (char-code keysym))))))
	     (keysym->xt-keysym keysym)))
	  (x-keycode nil))
      (declare (ignore x-keycode))
      ;;-- Is this correct????
      (unless x-keysym 
	(return-from port-canonicalize-gesture-spec nil))
      (cons (xt-keysym->keysym x-keysym) shifts))))


(defmethod port-set-pointer-position ((port xt-port) pointer x y)
  (let* ((sheet (pointer-sheet pointer))
	 (m (and (port sheet) (sheet-mirror sheet))))
    (when m
      (x11:xwarppointer
       (port-display port)
       0				; src
       (tk::widget-window m)		; dest
       0				; src-x
       0				; src-y
       0				; src-width
       0				; src-height
       (fix-coordinate x)
       (fix-coordinate y)))))


(defmethod raise-mirror ((port xt-port) sheet)
  (x11:xraisewindow (port-display port) (tk::widget-window (sheet-mirror sheet))))

(defmethod raise-mirror ((port xt-port) (sheet top-level-sheet))
  ;; Compensate for the top-level-sheet's mirror not being the right window.
  (x11:xraisewindow (port-display port)
		    (tk::widget-window (tk::widget-parent (sheet-mirror sheet)))))

(defmethod bury-mirror ((port xt-port) sheet)
  (x11:xlowerwindow (port-display port) (tk::widget-window (sheet-mirror sheet))))

(defmethod bury-mirror ((port xt-port) (sheet top-level-sheet))
  ;; Compensate for the top-level-sheet's mirror not being the right window.
  (x11:xlowerwindow (port-display port)
		    (tk::widget-window (tk::widget-parent (sheet-mirror sheet)))))


(defmethod enable-mirror ((port xt-port) (sheet top-level-sheet))
  (let* ((mirror (sheet-mirror sheet))
 	 (parent (widget-parent mirror)))
    (manage-child mirror)
    (xt:realize-widget parent)		; Make sure widget is realized.
    (let ((ussp (slot-value sheet 'silica::user-specified-size-p))
 	  (uspp (slot-value sheet 'silica::user-specified-position-p)))
      (unless (and (eq ussp :unspecified)
 		   (eq uspp :unspecified))
 	(let* ((window (tk::widget-window parent))
 	       (display (tk::widget-display parent))
 	       (size-hints (x11::xallocsizehints)))
 	  (tk::with-ref-par ((supplied 0))
 	    (when (zerop
 		   (x11::xgetwmnormalhints display window size-hints supplied))
 	      (warn "top-level-sheet had no size hints?!")
 	      (return-from enable-mirror))
 	    (let ((flags (x11::xsizehints-flags size-hints)))
 	      (if* (and uspp (not (eq uspp :unspecified)))
 		 then (setf flags (logior flags x11::uspositionhint))
 	       elseif (null uspp)
 		 then (setf flags (logandc2 flags x11::uspositionhint)))
 	      (if* (and ussp (not (eq ussp :unspecified)))
 		 then (setf flags (logior flags x11::ussizehint))
 	       elseif (null ussp)
 		 then (setf flags (logandc2 flags x11::ussizehint)))
 	      (setf (x11::xsizehints-flags size-hints) flags))
 	    (x11::xsetwmnormalhints display window size-hints)))))
    (popup parent)))
 
(defmethod enable-xt-widget ((parent top-level-shell) (mirror t))
  (manage-child mirror)
  (popup (widget-parent mirror)))


(defmethod mirror-visible-p ((port xt-port) sheet)
  (let ((mirror (sheet-mirror sheet)))
    ;;--- This costs a round trip to the display server.  A better
    ;;--- scheme would be to have the map/unmap-notify set a flag
    ;;--- that we could simply read here, as in CLIM 1.1
    ;;--- What does unviewable mean?
    (not (eq (tk::window-map-state (tk::widget-window mirror)) :unmapped))))

;;;---- Cursor stuff

(defvar *xt-cursor-type-alist*
	'((:default 132)
	  (:vertical-scroll 116)
	  (:scroll-up 114)
	  (:scroll-down 106)
	  (:horizontal-scroll 108)
	  (:scroll-left 110)
	  (:scroll-right 112)
	  (:busy 150)
	  (:upper-left 134)
	  (:upper-right 136)
	  (:lower-left 12)
	  (:lower-right 14)
	  (:vertical-thumb 112)
	  (:horizontal-thumb 114)
	  (:button 132)
	  (:prompt 132)
	  (:move 52)
	  (:position 34)))



(defmethod port-set-pointer-cursor ((port xt-port) pointer cursor)
  (unless (eq (pointer-cursor pointer) cursor)
    (let* ((cursor (and cursor (realize-cursor port cursor)))
	   (widget (sheet-direct-mirror 
		     (let ((frame (pane-frame (pointer-sheet pointer))))
		       (if frame
			   (frame-top-level-sheet frame)
			   (pointer-sheet pointer)))))
	   (window (and widget (tk::widget-window widget nil))))
      (when window
	(if cursor
	    (x11:xdefinecursor
	     (port-display port) window cursor)
	  (x11:xundefinecursor
	   (port-display port) window))
	(port-force-output port)))
    cursor))


(defmethod port-set-sheet-pointer-cursor ((port xt-port) sheet cursor)
  (unless (eq (sheet-pointer-cursor sheet)  cursor)
    (let* ((cursor (and cursor (realize-cursor port cursor)))
	   (widget (sheet-mirror sheet))
	   (window (tk::widget-window widget nil)))
      (when window
	(if cursor
	    (x11:xdefinecursor
	     (port-display port) window cursor)
	  (x11:xundefinecursor
	   (port-display port) window))
	(port-force-output port))))
  cursor)

(defmethod realize-cursor :around ((port xt-port) cursor)
  (with-slots (cursor-cache) port
    (or (getf cursor-cache cursor)
	(setf (getf cursor-cache cursor)
	      (call-next-method)))))

(defmethod realize-cursor ((port xt-port) (cursor symbol))
  (let ((cursor (or (second (assoc cursor *xt-cursor-type-alist*))
		    132)))
    (realize-cursor port cursor)))

(defmethod realize-cursor ((port xt-port) (cursor number))
  (x11:xcreatefontcursor
   (port-display port)
   cursor))


(defvar *pointer-grabbed* nil)


(defmethod port-invoke-with-pointer-grabbed ((port xt-port) (sheet basic-sheet) continuation 
								    &key
								    confine-to cursor
								    (ungrab-on-error t))
  (let ((widget (sheet-mirror sheet)))
    (unwind-protect
	(progn
	  (tk::xt_grab_pointer
	   widget 
	   ;;---- Are these parameters correct????
	   0				; owner-events
	   (xt-grabbed-event-mask)
					; Event-mask
	   x11:grabmodeasync		; pointer-grab-mode
	   x11:grabmodeasync		; keyboard
	   (if confine-to widget 0)	; confine to
	   (if cursor (realize-cursor port cursor) 0)
	   0				; current-time
	   )
	  (handler-bind ((error #'(lambda (condition)
				    (declare (ignore condition))
				    (when ungrab-on-error
				      (tk::xt_ungrab_pointer
				       widget 0)))))
	    (let ((*pointer-grabbed* t))
	      (funcall continuation))))
      (tk::xt_ungrab_pointer widget 0))))

(defun xt-grabbed-event-mask ()
  (tk::encode-event-mask '(:enter-window 
			   :leave-window
			   :pointer-motion-hint
			   :pointer-motion
			   :button-motion
			   :button-press
			   :button-release
			   )))

(defmethod port-set-sheet-grabbed-pointer-cursor ((port xt-port) sheet cursor)
  (declare (ignore sheet))
  (when *pointer-grabbed*
    (x11::xchangeactivepointergrab
     (port-display port)
     (xt-grabbed-event-mask)					; event-mask
     (if cursor (realize-cursor port cursor) 0)
     0					; time
     )))

(defmethod port-move-frame ((port xt-port) frame x y)
  (check-type x (signed-byte 16))
  (check-type y (signed-byte 16))
  (tk::set-values (frame-shell frame) :x x :y y))

(defmethod port-resize-frame ((port xt-port) frame width height)
  (check-type width (signed-byte 16))
  (check-type height (signed-byte 16))
  (tk::set-values (frame-shell frame) :width width :height height))

(defmethod destroy-port ((port xt-port))
  (when (port-process port)
    (clim-sys:destroy-process (port-process port)))
  (port-terminated port (make-condition 'error)))
