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
;; $fiHeader: xt-silica.lisp,v 1.14 92/03/10 15:40:01 cer Exp Locker: cer $

(in-package :xm-silica)

(defclass xt-port (port)
    ((application-shell :reader port-application-shell)
     (display :reader port-display)
     (context :reader port-context)     
     (copy-gc :initform nil)
     (type :allocation :class 
	   :initform :xt :reader port-type))
  (:default-initargs :allow-loose-text-style-size-mapping t)
  (:documentation "The port for X intrinsics based ports"))


(defmethod port-copy-gc ((port xt-port))
  (with-slots (copy-gc display) port
    (or copy-gc
	(setf copy-gc
	  (make-instance 'tk::gcontext
			 :display display
			 :foreign-address (x11:screen-default-gc (x11:xdefaultscreenofdisplay (port-display port))))))))


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
  (destructure-x-server-path (:display display) server-path
    (declare (ignore ignore))
    (multiple-value-bind (context display application-shell)
	(initialize-motif-toolkit display)
      (setf (slot-value port 'application-shell) application-shell
	    (slot-value port 'context) context
	    (slot-value port 'display) display)
      (initialize-xlib-port port display))))

(defvar *clx-font-families* '((:fix "*-*-courier-*-*-*-*-*-*-*-*-*-*-*-*")
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


(defvar *clx-fallback-font* "8x13"
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
    (dolist (family-stuff *clx-font-families*)
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
		   (dolist (family *clx-font-families*)
		     (when (text-style-mapping-exists-p port `(,(car family) :roman 10))
		       (return (make-text-style (car family) :roman 10)))))
	     (setf (text-style-mapping port (port-undefined-text-style port)) temp))
	    ;; Perhaps we should look for some other conveniently sized
	    ;; fonts.
	    (*clx-fallback-font*
	     (setf (text-style-mapping port (port-undefined-text-style port))
		   (make-instance 'tk::font 
				  :display display
				  :name *clx-fallback-font*)))
	    ;;; Perhaps we should just grab the first font we can find.
	    (t
	     (error "Unable to determine default font"))))))

(defparameter *clx-logical-size-alist*
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
    port style character-set *clx-logical-size-alist*))


(defmethod destroy-mirror ((port xt-port) sheet)
  ;;-- I dont think that we should do this.
  (tk::destroy-widget (sheet-direct-mirror sheet))
  )

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

(defmethod initialize-mirror (port sheet widget)
  (add-sheet-callbacks port sheet widget))

(defmethod add-sheet-callbacks ((port xt-port) sheet (widget t))
  (declare (ignore sheet)))

(defmethod sheet-mirror-event-handler (widget event sheet)
  (multiple-value-bind (same-p root child root-x root-y native-x native-y mask)
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
		    (character keysym)
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
		(multiple-value-bind
		    (character keysym)
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
				modifiers)
			       :sheet sheet))
	       (:enter-notify
		(make-instance 'pointer-enter-event
			       :native-x native-x
			       :native-y native-y
			       :button (x-button->silica-button button)
			       :modifiers
			       (state->modifiers
				modifiers)
			       :sheet sheet))
	       (:motion-notify
		(make-instance 'pointer-motion-event
			       :native-x native-x
			       :native-y native-y
			       :button (x-button->silica-button button)
			       :modifiers
			       (state->modifiers
				modifiers)
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
	(multiple-value-bind
		    (character keysym)
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
      (multiple-value-bind
	  (character keysym)
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
  (values 'xm-drawing-area (list :resize-policy :grow)))

(defmethod find-widget-class-and-initargs-for-sheet :around 
	   ((port xt-port) (parent t) (sheet sheet))
  (multiple-value-bind (class initargs)
      (call-next-method)
    (setq initargs (set-mirror-geometry parent sheet initargs))
    (values class initargs)))


(defmethod set-mirror-geometry (parent sheet initargs)
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
	;;--floor
	(setf (getf initargs :x) (floor left)
	      (getf initargs :y) (floor top)))
      (setf (getf initargs :width) (floor (- right left))
	    (getf initargs :height) (floor (- bottom top)))))
  initargs)

;; If we are creating a top level sheet then we have to create a shell for it
(defmethod find-widget-parent ((port xt-port) sheet)
  (let ((ma (sheet-mirrored-ancestor sheet)))
    (if (graftp ma)
	(multiple-value-bind (class initargs)
	    (find-shell-class-and-initargs port sheet)
	  (apply #'make-instance class
		 :parent (find-shell-parent port sheet)
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

(defmethod find-shell-class-and-initargs ((port xt-port) sheet)
  (values 'top-level-shell 
	  ;; Need this so that an interactive pane can have children
	  ;; but still accept the focus
	  '(:keyboard-focus-policy :pointer)))

(defmethod enable-mirror ((port xt-port) sheet)
  (declare (ignore port))
  (let ((mirror (sheet-mirror sheet)))
    (typecase (widget-parent mirror)
      (null)
      ((or top-level-shell tk::xm-dialog-shell)
	;; this is a nasty hack just to make sure that the child is managed.
	;; top-level-sheets are created unmanaged because they are
	;; disabled to we have to do not!
	(manage-child mirror)
	(popup (widget-parent mirror)))
      (t
	(manage-child mirror)))))

(defmethod disable-mirror ((port xt-port) sheet)
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
  ;;--- Mess with the region
  (warn "Do something about the graft")
  (setf (sheet-region graft)
	(ecase (graft-units graft)
	  ((:device :pixel)
	   (make-bounding-rectangle 0 0 1100 850))))
  ;; Mess with the native transformation
  )

(defmethod mirror-region* ((port xt-port) sheet)
  (when (sheet-mirror sheet)
    (multiple-value-bind (x y width height)
	(get-values (sheet-mirror sheet) :x :y :width :height)
      (values x y 
	      (+ x width)
	      (+ y height)))))

(defmethod mirror-region ((port xt-port) sheet)
  (multiple-value-call #'make-bounding-rectangle
    (mirror-region* port sheet)))

(defmethod mirror-inside-region* ((port xt-port) sheet)
  (multiple-value-bind (minx miny maxx maxy)
      (mirror-region* port sheet)
    (values 0 0 (- maxx minx) (- maxy miny))))

(defmethod mirror-native-edges* ((port xt-port) sheet)
  (let* ((mirror (sheet-direct-mirror sheet)))
    (multiple-value-bind (x y width height)
	(get-values mirror :x :y :width :height)
      (values x y (+ x width) (+ y height)))))

(defmethod mirror-inside-edges* ((port xt-port) sheet)
  (multiple-value-bind (a b c d)
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
	(let ((*error-output* excl:*initial-terminal-io*))
	  (warn "Geo set fail, ~S, ~S,~S"
		sheet
		(list  target-left  target-top w h)
		(list nx ny nw nh)))))))

(defmethod process-next-event (port &key wait-function timeout)
  (tk::process-one-event (port-context port)
			 :wait-function wait-function
			 :timeout timeout))

(defmethod port-force-output ((port xt-port))
  ;;--- move to tk
  (x11::xflush (port-display port)))

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
					
#+ignore
(ff::defforeign 'xtsetkeyboardfocus
    :entry-point "_XtSetKeyboardFocus")

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

(defvar *clx-keysym->clim-keysym-table*
    (make-hash-table))

(defvar *clim-keysym->clx-keysym-table* (make-hash-table))

(defmacro define-clx-keysym (clx-keysym clim-keysym)
  `(progn 
     (setf (gethash ,clx-keysym *clx-keysym->clim-keysym-table*) ',clim-keysym)
     (unless (gethash ,clim-keysym *clim-keysym->clx-keysym-table*)
       (setf (gethash ,clim-keysym *clim-keysym->clx-keysym-table*) ',clx-keysym))))

(defun-inline clx-keysym->keysym (clx-keysym)
  (gethash clx-keysym *clx-keysym->clim-keysym-table*))

(defun-inline keysym->clx-keysym (keysym)
  (gethash keysym *clim-keysym->clx-keysym-table*))

;; The standard characters
(define-clx-keysym 032 :space)
(define-clx-keysym 033 :\!)
(define-clx-keysym 034 :\")
(define-clx-keysym 035 :\#)
(define-clx-keysym 036 :\$)
(define-clx-keysym 037 :\%)
(define-clx-keysym 038 :\&)
(define-clx-keysym 039 :\')
(define-clx-keysym 040 :\()
(define-clx-keysym 041 :\))
(define-clx-keysym 042 :\*)
(define-clx-keysym 043 :\+)
(define-clx-keysym 044 :\,)
(define-clx-keysym 045 :\-)
(define-clx-keysym 046 :\.)
(define-clx-keysym 047 :\/)
(define-clx-keysym 048 :\0)
(define-clx-keysym 049 :\1)
(define-clx-keysym 050 :\2)
(define-clx-keysym 051 :\3)
(define-clx-keysym 052 :\4)
(define-clx-keysym 053 :\5)
(define-clx-keysym 054 :\6)
(define-clx-keysym 055 :\7)
(define-clx-keysym 056 :\8)
(define-clx-keysym 057 :\9)
(define-clx-keysym 058 :\:)
(define-clx-keysym 059 :\;)
(define-clx-keysym 060 :\<)
(define-clx-keysym 061 :\=)
(define-clx-keysym 062 :\>)
(define-clx-keysym 063 :\?)
(define-clx-keysym 064 :\@)
(define-clx-keysym 065 :\A)
(define-clx-keysym 097 :\A)
(define-clx-keysym 066 :\B)
(define-clx-keysym 098 :\B)
(define-clx-keysym 067 :\C)
(define-clx-keysym 099 :\C)
(define-clx-keysym 068 :\D)
(define-clx-keysym 100 :\D)
(define-clx-keysym 069 :\E)
(define-clx-keysym 101 :\E)
(define-clx-keysym 070 :\F)
(define-clx-keysym 102 :\F)
(define-clx-keysym 071 :\G)
(define-clx-keysym 103 :\G)
(define-clx-keysym 072 :\H)
(define-clx-keysym 104 :\H)
(define-clx-keysym 073 :\I)
(define-clx-keysym 105 :\I)
(define-clx-keysym 074 :\J)
(define-clx-keysym 106 :\J)
(define-clx-keysym 075 :\K)
(define-clx-keysym 107 :\K)
(define-clx-keysym 076 :\L)
(define-clx-keysym 108 :\L)
(define-clx-keysym 077 :\M)
(define-clx-keysym 109 :\M)
(define-clx-keysym 078 :\N)
(define-clx-keysym 110 :\N)
(define-clx-keysym 079 :\O)
(define-clx-keysym 111 :\O)
(define-clx-keysym 080 :\P)
(define-clx-keysym 112 :\P)
(define-clx-keysym 081 :\Q)
(define-clx-keysym 113 :\Q)
(define-clx-keysym 082 :\R)
(define-clx-keysym 114 :\R)
(define-clx-keysym 083 :\S)
(define-clx-keysym 115 :\S)
(define-clx-keysym 084 :\T)
(define-clx-keysym 116 :\T)
(define-clx-keysym 085 :\U)
(define-clx-keysym 117 :\U)
(define-clx-keysym 086 :\V)
(define-clx-keysym 118 :\V)
(define-clx-keysym 087 :\W)
(define-clx-keysym 119 :\W)
(define-clx-keysym 088 :\X)
(define-clx-keysym 120 :\X)
(define-clx-keysym 089 :\Y)
(define-clx-keysym 121 :\Y)
(define-clx-keysym 090 :\Z)
(define-clx-keysym 122 :\Z)
(define-clx-keysym 091 :\[)
(define-clx-keysym 092 :\\)
(define-clx-keysym 093 :\])
(define-clx-keysym 094 :\^)
(define-clx-keysym 095 :\_)
(define-clx-keysym 096 :\`)
(define-clx-keysym 123 :\{)
(define-clx-keysym 124 :\|)
(define-clx-keysym 125 :\})
(define-clx-keysym 126 :\~)

;; The semi-standard characters


(defmacro keysym (b &rest more)
  (dolist (n more)
    (setq b (+ (ash b 8) n)))
  b)

(define-clx-keysym (keysym 255 013) :return)
(define-clx-keysym (keysym 255 009) :tab)
(define-clx-keysym (keysym 255 255) :rubout)
(define-clx-keysym (keysym 255 008) :backspace)
(define-clx-keysym (keysym 009 227) :page)
(define-clx-keysym (keysym 255 010) :linefeed)

;; Other useful characters
(define-clx-keysym (keysym 255 087) :end)
(define-clx-keysym (keysym 255 105) :abort)
(define-clx-keysym (keysym 255 106) :help)
(define-clx-keysym (keysym 255 104) :complete)
(define-clx-keysym (keysym 255 086) :scroll)
(define-clx-keysym (keysym 255 097) :refresh)
(define-clx-keysym (keysym 255 011) :clear-input)

;; Finally, the shifts
;; snarfed from translate.cl

(defconstant left-shift-keysym (keysym 255 225))
(defconstant right-shift-keysym (keysym 255 226))
(defconstant left-control-keysym (keysym 255 227))
(defconstant right-control-keysym (keysym 255 228))
(defconstant caps-lock-keysym (keysym 255 229))
(defconstant shift-lock-keysym (keysym 255 230))
(defconstant left-meta-keysym (keysym 255 231))
(defconstant right-meta-keysym (keysym 255 232))
(defconstant left-alt-keysym (keysym 255 233))
(defconstant right-alt-keysym (keysym 255 234))
(defconstant left-super-keysym (keysym 255 235))
(defconstant right-super-keysym (keysym 255 236))
(defconstant left-hyper-keysym (keysym 255 237))
(defconstant right-hyper-keysym (keysym 255 238))

(define-clx-keysym left-shift-keysym :left-shift)
(define-clx-keysym right-shift-keysym :right-shift)
(define-clx-keysym left-control-keysym :left-control)
(define-clx-keysym right-control-keysym :right-control)
(define-clx-keysym caps-lock-keysym :caps-lock)
(define-clx-keysym shift-lock-keysym :shift-lock)
(define-clx-keysym left-meta-keysym :left-meta)
(define-clx-keysym right-meta-keysym :right-meta)
(define-clx-keysym left-super-keysym :left-super)
(define-clx-keysym right-super-keysym :right-super)
(define-clx-keysym left-hyper-keysym :left-hyper)
(define-clx-keysym right-hyper-keysym :right-hyper)

(defun lookup-character-and-keysym (sheet mirror event)
  (declare (ignore sheet mirror))
  (multiple-value-bind (ignore character keysym)
      (tk::lookup-string event)
    (declare (ignore ignore))
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
    (values character
	    (clx-keysym->keysym keysym))))


(defun state->modifiers (x &optional (shiftit t))
  (logior
   (if (and shiftit (logtest x x11:shiftmask)) +shift-key+ 0)
   (if (logtest x x11:controlmask) +control-key+ 0)
   (if (logtest x x11:mod1mask) +meta-key+ 0)
   (if (logtest x x11:mod2mask) +super-key+ 0)
   (if (logtest x x11:mod3mask) +hyper-key+ 0)))




