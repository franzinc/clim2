;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: menus.lisp,v 1.22 92/05/07 13:12:39 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

(defvar *abort-menus-when-buried* t)

(define-application-frame menu-frame ()
    (menu)
  (:pane
    (with-slots (menu) *application-frame*
      (outlining ()
	(scrolling ()
	  (setq menu (make-pane 'clim-stream-pane
				:initial-cursor-visibility nil))))))
  (:menu-bar nil))

(defmethod frame-calling-frame ((frame menu-frame))
  (and (boundp '*application-frame*)
       *application-frame*))

;; Returns a stream that corresponds to the pane holding the menu
(defun get-menu (&key port)
  (let ((frame (make-application-frame 'menu-frame
				       :parent port
				       :save-under t)))
    (values (slot-value frame 'menu) frame)))

(defresource menu (associated-window root)
  :constructor (let* ((port (if (null root) (find-port) (port root))))
		 (get-menu :port port))
  :deinitializer (window-clear menu)
  :initializer (initialize-menu (port menu) menu associated-window)
  ;; Horrible kludge in the case where no associated window is passed in.
  :matcher (eq (port menu) (port root)))

(defmethod initialize-menu ((port port) window associated-window)
  (declare (ignore window associated-window))
  )

(defun size-menu-appropriately (menu &key width height
					  (right-margin 10) (bottom-margin 10)
					  (size-setter #'window-set-inside-size))
  (with-slots (output-record) menu
    (with-bounding-rectangle* (left top right bottom) output-record
      (let* ((graft (graft menu))
	     (gw (bounding-rectangle-width (sheet-region graft)))
	     (gh (bounding-rectangle-height (sheet-region graft)))
	     (width (min gw (+ (or width (- right left)) right-margin)))
	     (height (min gh (+ (or height (- bottom top)) bottom-margin))))
	(funcall size-setter menu width height)
	(window-set-viewport-position menu left top)))))

(defun position-window-near-carefully (window x y)
  #-Silica
  (multiple-value-bind (width height) (bounding-rectangle-size window)
    (multiple-value-bind (parent-width parent-height)
	(window-inside-size (window-parent window))
      (let* ((left x)
	     (top y)
	     (right (+ left width))
	     (bottom (+ top height)))
	(when (> right parent-width)
	  (setq left (- parent-width width)))
	(when (> bottom parent-height)
	  (setq top (- parent-height height)))
	(bounding-rectangle-set-position window
					 (max 0 left) (max 0 top))))))

(defun position-window-near-pointer (window &optional x y)
  (unless (and x y)
    (multiple-value-setq (x y)
      #+++ignore (pointer-position (port-pointer (port window)))
      #---ignore (values 100 100)))
  (position-window-near-carefully window x y))

;; items := (item*)
;; item := object | (name :value object) | (object . value)
;; object := any lisp object.  It is written (using WRITE) into the menu using :ESCAPE NIL.

(defun menu-item-display (menu-item)
  (cond ((atom menu-item) menu-item)
	(t (first menu-item))))

(defun menu-item-value (menu-item)
  (let (rest)
    (cond ((atom menu-item) menu-item)
	  ((atom (setq rest (cdr menu-item))) rest)
	  (t (getf rest :value (first menu-item))))))

;; There is a semantic reason this is a macro: we don't want DEFAULT to get
;; evaluated before it is needed
(defmacro menu-item-getf (menu-item indicator &optional default)
  `(let (rest)
     (cond ((atom ,menu-item) nil)
	   ((atom (setq rest (cdr ,menu-item))) nil)
	   (t (getf rest ,indicator ,default)))))

(defun menu-item-style (menu-item)
  (menu-item-getf menu-item :style))

(defun menu-item-documentation (menu-item)
  (menu-item-getf menu-item :documentation))

(defun menu-item-type (menu-item)
  (or (menu-item-getf menu-item :type)
      :item))

(defun menu-item-active (menu-item)
  (or (atom menu-item)
      (menu-item-getf menu-item :active t)))

;; Item list can be a general sequence...
(defun menu-item-item-list (menu-item)
  (menu-item-getf menu-item :item-list))

;; Perhaps misguided attempt at allowing performance win if you use
;; simple unique-ids.
(defvar *menu-choose-output-history-caches* 
	(cons (make-hash-table :test #'eql)
	      (make-hash-table :test #'equal)))

(defun get-from-output-history-cache (unique-id id-test)
  (let ((table (cond ((or (eq id-test 'eql)
			  (eq id-test (load-time-value #'eql)))
		      (car *menu-choose-output-history-caches*))
		     ((or (eq id-test 'equal)
			  (eq id-test (load-time-value #'equal)))
		      (cdr *menu-choose-output-history-caches*)))))
  (gethash unique-id table)))

(defun set-output-history-cache (unique-id id-test new-value)
  (let ((table (cond ((or (eq id-test 'eql)
			  (eq id-test (load-time-value #'eql)))
		      (car *menu-choose-output-history-caches*))
		     ((or (eq id-test 'equal)
			  (eq id-test (load-time-value #'equal)))
		      (cdr *menu-choose-output-history-caches*)))))
    (setf (gethash unique-id table) new-value)))

(defsetf get-from-output-history-cache set-output-history-cache)

(define-presentation-type menu-item ())

;; This translator is here to compute menu item documentation
(define-presentation-translator menu-item-identity
    (menu-item menu-item global-command-table
     :priority 1		;prefer this to IDENTITY
     :tester-definitive t
     :documentation ((object stream)
		     (let ((documentation (or (menu-item-documentation object)
					      (menu-item-display object))))
		       (write documentation :stream stream :escape nil)))
     :gesture :select)
    (object presentation)
  (values object (presentation-type presentation)))

(defun draw-standard-menu (menu presentation-type items default-item
			   &key (item-printer #'print-menu-item)
				max-width max-height n-rows n-columns
				x-spacing y-spacing 
				(cell-align-x ':left) (cell-align-y ':top)
			   &aux default-presentation)
  (formatting-item-list (menu :max-width max-width :max-height max-height
			      :n-rows n-rows :n-columns n-columns
			      :x-spacing x-spacing :y-spacing y-spacing
			      :move-cursor nil)
    (flet ((format-item (item)
	     (let ((type (menu-item-type item)))
	       (unless (eq type :separator)
		 (flet ((print-item ()
			  (formatting-cell (menu :align-x cell-align-x 
						 :align-y cell-align-y)
			    (funcall item-printer item menu))))
		   (declare (dynamic-extent #'print-item))
		   (ecase type
		     (:item 
		       (if (menu-item-active item)
			   (let ((presentation
				   (with-output-as-presentation (menu item presentation-type
								 :single-box t)
				     (print-item))))
			     (when (and default-item
					(eq item default-item))
			       (setf default-presentation presentation)))
			 ;;--- Perhaps it should be grayed out in someway?
			 (print-item)))
		     (:label 
		       (print-item))
		     (:separator
		       ;; Ignore separators for the time being
		       )))))))
      (declare (dynamic-extent #'format-item))
      (map nil #'format-item items)))
  default-presentation)

(defun print-menu-item (menu-item &optional (stream *standard-output*))
  (let ((display (menu-item-display menu-item))
	(string nil)
	(style (menu-item-style menu-item)))
    (cond ((stringp display)
	   (setq string display))
	  ;; This is a [horrible] kludge to get around the fact
	  ;; that we haven't made WRITE properly generic yet.
	  (t
	   (setq string (with-output-to-string (stream)
			  (write display :stream stream :escape nil)))))
    (if style
	(with-text-style (stream style)
	  (write-string string stream))
	(write-string string stream))))

(defclass static-menu ()
    ((name :initarg :name)
     (menu-contents :initarg :menu-contents)
     (default-presentation :initarg :default-presentation)
     (root-window :initarg :root-window)
     ;; In case we need to "compile" the menu on the fly
     (items :initarg :items)
     (default-item :initarg :default-item)
     (default-style :initarg :default-style)
     (printer :initarg :printer)
     (presentation-type :initarg :presentation-type)
     (drawer-args :initarg :drawer-args)))

(defgeneric menu-choose (items &rest keys
			 &key associated-window default-item default-style
			      label printer presentation-type
			      cache unique-id id-test cache-value cache-test
			      max-width max-height n-rows n-columns
			      x-spacing y-spacing
			      cell-align-x cell-align-y
			      pointer-documentation))

;; Are these reasonable defaults for UNIQUE-ID, CACHE-VALUE, ID-TEST, and CACHE-TEST?
(defmethod menu-choose ((items t) &rest keys
			&key (associated-window (frame-top-level-sheet *application-frame*))
			     default-item default-style
			     label printer presentation-type
			     (cache nil) (unique-id items) (id-test #'equal)
			     (cache-value items) (cache-test #'equal)
			     max-width max-height n-rows n-columns
			     x-spacing y-spacing 
			     (cell-align-x ':left) (cell-align-y ':top)
			     pointer-documentation)
  (declare (values value chosen-item gesture))
  (declare (ignore associated-window
		   default-item default-style
		   label printer presentation-type
		   cache unique-id id-test cache-value cache-test
		   max-width max-height n-rows n-columns
		   x-spacing y-spacing cell-align-x cell-align-y
		   pointer-documentation))
  (declare (dynamic-extent keys))
  (apply #'frame-manager-menu-choose (frame-manager *application-frame*) items keys))

;; Specific ports can put :AROUND methods on this in order to use their own
;; kinds of menus.
(defmethod frame-manager-menu-choose
	   ((framem standard-frame-manager) items &rest keys
	    &key (associated-window
		   (frame-top-level-sheet *application-frame*))
		 default-item default-style
		 label printer presentation-type
		 (cache nil) (unique-id items) (id-test #'equal)
		 (cache-value items) (cache-test #'equal)
		 max-width max-height n-rows n-columns
		 x-spacing y-spacing 
		 (cell-align-x ':left) (cell-align-y ':top)
		 pointer-documentation)
  (declare (values value chosen-item gesture))
  (declare (ignore keys))
  (flet ((present-item (item stream)
	   (present item presentation-type :stream stream)))
    (declare (dynamic-extent #'present-item))
    (let ((item-printer (cond (presentation-type #'present-item)
			      (printer printer)
			      (t #'print-menu-item)))
	  ;; Lucid production compiler tries to use an undefined internal
	  ;; variable if this LET isn't done.
	  #+Lucid (items items))
      (with-menu (menu associated-window)
	(setf (window-label menu) label)
	(reset-frame (pane-frame menu) :title label)
	(with-text-style (menu default-style)
	  (with-end-of-line-action (menu :allow)
	    (loop
	      (multiple-value-bind (item gesture)
		  (flet ((menu-choose-body (stream presentation-type)
			   (draw-standard-menu stream presentation-type items default-item
					       :item-printer item-printer
					       :max-width max-width :max-height max-height
					       :n-rows n-rows :n-columns n-columns
					       :x-spacing x-spacing :y-spacing y-spacing 
					       :cell-align-x cell-align-x
					       :cell-align-y cell-align-y)))
		    (declare (dynamic-extent #'menu-choose-body))
		    (menu-choose-from-drawer 
		      menu 'menu-item #'menu-choose-body
		      :cache cache
		      :unique-id unique-id :id-test id-test
		      :cache-value cache-value :cache-test cache-test
		      :pointer-documentation pointer-documentation))
		(cond ((menu-item-item-list item)
		       ;; Set the new item list, then go back through the loop.
		       ;; Don't cache, because that will cause us to see the same
		       ;; menu items again and again.
		       (setq items (menu-item-item-list item)
			     default-item nil
			     cache nil)
		       (clear-output-history menu))
		      (t (return-from frame-manager-menu-choose
			   (values (menu-item-value item) item gesture))))))))))))


(defmacro with-mouse-grabbed-in-window ((window &rest options) &body body)
  `(invoke-with-mouse-grabbed-in-window
     ,window #'(lambda () ,@body)
     ,@options))

(defmethod invoke-with-mouse-grabbed-in-window ((window t) continuation &key)
  (funcall continuation))

(defmacro with-menu-as-popup ((menu) &body body)
  `(invoke-with-menu-as-popup
     ,menu #'(lambda () ,@body)))

(defmethod invoke-with-menu-as-popup ((window t) continuation)
  (funcall continuation))

;; The drawer gets called with (stream presentation-type &rest drawer-args).
;; It can use presentation-type for its own purposes.  The most common uses are:
;;  1) Using that type as the presentation-type of the presentations it makes for each item
;;  2) Using that type to find a printer for PRESENT purposes
;;  3) Presenting a different set of choices based on the type, or graying over things
;;     that are of different types.  See example below
(defun menu-choose-from-drawer (menu presentation-type drawer
				&key x-position y-position
				     cache unique-id (id-test #'equal) 
				     (cache-value t) (cache-test #'eql)
				     ;; for use by HIERARCHICHAL-MENU-CHOOSE
				     leave-menu-visible
				     (default-presentation nil)
				     pointer-documentation)
  (flet (#+Allegro
	 (abort-menu-handler () 
	   (return-from menu-choose-from-drawer nil)))
   #+Allegro (declare (dynamic-extent #'abort-menu-handler))
   ;;--- This should be done in a more modular way
   ;;--- If you change this, change RUN-FRAME-TOP-LEVEL :AROUND
   (let (#+Allegro (*click-outside-menu-handler* #'abort-menu-handler)
	 (*original-stream* nil)
	 (*input-wait-test* nil)
	 (*input-wait-handler* nil)
	 (*pointer-button-press-handler* nil)
	 (*numeric-argument* nil)
	 (*delimiter-gestures* nil)
	 (*activation-gestures* nil)
	 (*accelerator-gestures* nil)
	 (*input-context* nil)
	 (*accept-help* nil)
	 (*assume-all-commands-enabled* nil)
	 (*command-parser* 'command-line-command-parser)
	 (*command-unparser* 'command-line-command-unparser)
	 (*partial-command-parser*
	   'command-line-read-remaining-arguments-for-partial-command))
    ;; We could make the drawer a lexical closure, but that would then
    ;; partially defeat the purpose of the uid and cache-value because we'd cons the closure
    ;; whether or not we ran it.
    (let* ((cached-output-history-info
	     (when cache
	       (if (typep cache 'static-menu)
		   cache-value
		   (get-from-output-history-cache unique-id id-test))))
	   (cached-menu-contents
	     (when cached-output-history-info
	       (if (typep cache 'static-menu)
		   cached-output-history-info
		   (let* ((contents (pop cached-output-history-info))
			  (value cached-output-history-info))
		     (when (funcall cache-test value cache-value)
		       contents))))))
      (cond (cached-menu-contents
	     (stream-add-output-record menu cached-menu-contents))
	    (t
	     ;; "Draw" into deexposed menu for sizing only
	     (with-output-recording-options (menu :draw nil :record t)
	       (let ((menu-contents
		       (with-new-output-record (menu)
			 (setq default-presentation 
			       (funcall drawer menu presentation-type)))))
		 (when cache
		   (setf (get-from-output-history-cache unique-id id-test)
			 (cons menu-contents cache-value))))))))
    (size-menu-appropriately menu)
    (unwind-protect
	(with-menu-as-popup (menu)
	  (position-window-near-pointer menu x-position y-position)
	  (window-expose menu)
	  #+Cloe-Runtime (stream-set-input-focus menu)
	  (when default-presentation
	    (with-bounding-rectangle* (left top right bottom) default-presentation
	      (stream-set-pointer-position
		menu (floor (+ left right) 2) (floor (+ top bottom) 2))))
	  ;; Pointer documentation usually adds no information, and slows things
	  ;; down in a big way, which is why we defaultly disable it.
	  (let ((*pointer-documentation-output* pointer-documentation))
	    (with-input-context (presentation-type :override T)
				(object type gesture)
		 (labels ((input-wait-test (menu)
			    ;; Wake up if the menu becomes buried, or if highlighting is needed
			    (or (and *abort-menus-when-buried*
				     #+Cloe-Runtime
				     (not (stream-has-input-focus menu))
				     #-Cloe-Runtime
				     (not (window-visibility menu)))
				(pointer-motion-pending menu)))
			  (input-wait-handler (menu)
			    ;; Abort if the menu becomes buried
			    (when (and *abort-menus-when-buried*
				       #+Cloe-Runtime
				       (not (stream-has-input-focus menu))
				       #-Cloe-Runtime
				       (not (window-visibility menu)))
			      (return-from menu-choose-from-drawer nil))
			    ;; Take care of highlighting
			    (highlight-presentation-of-context-type menu)))
		   (declare (dynamic-extent #'input-wait-test #'input-wait-handler))
		   ;; Await exposure before going any further, since X can get
		   ;; to the call to READ-GESTURE before the menu is visible.
		   (when *abort-menus-when-buried*
		     #-Silica (wait-for-window-exposed menu))
		   (with-mouse-grabbed-in-window (menu)
		     (loop (read-gesture :stream menu
					 :input-wait-test #'input-wait-test
					 :input-wait-handler #'input-wait-handler)
			   (beep))))
	       (t (values object gesture)))))
      (unless leave-menu-visible
	(setf (window-visibility menu) nil))
      (force-output menu)))))

(defun hierarchical-menu-choose (items
				 &key (associated-window
					(frame-top-level-sheet *application-frame*))
				      default-item default-style
				      label printer presentation-type
				      x-position y-position
				      (cache nil)
				      (unique-id items) (id-test #'equal)
				      (cache-value items) (cache-test #'equal)
				      max-width max-height n-rows n-columns
				      x-spacing y-spacing 
				      (cell-align-x ':left) (cell-align-y ':top))
  (declare (values value chosen-item gesture))
  (flet ((present-item (item stream)
	   (present item presentation-type :stream stream)))
    (declare (dynamic-extent #'present-item))
    (let ((item-printer (cond (presentation-type #'present-item)
			      (printer printer)
			      (t #'print-menu-item))))
      (with-menu (menu associated-window)
	(setf (window-label menu) label)
	(with-text-style (menu default-style)
	  (multiple-value-bind (item gesture)
	      (flet ((menu-choose-body (stream presentation-type)
		       (draw-standard-menu stream presentation-type items default-item
					   :item-printer item-printer
					   :max-width max-width :max-height max-height
					   :n-rows n-rows :n-columns n-columns
					   :x-spacing x-spacing :y-spacing y-spacing 
					   :cell-align-x cell-align-x
					   :cell-align-y cell-align-y)))
		(declare (dynamic-extent #'menu-choose-body))
		(menu-choose-from-drawer
		  menu 'menu-item #'menu-choose-body
		  :x-position x-position :y-position y-position
		  :leave-menu-visible t
		  :cache cache
		  :unique-id unique-id :id-test id-test
		  :cache-value cache-value :cache-test cache-test))
	    (cond ((menu-item-item-list item)
		   (with-bounding-rectangle* (ml mt mr mb) menu
		     (declare (ignore ml mb))
		     ;;--- How to pass on LABEL, PRINTER, and PRESENTATION-TYPE?
		     (hierarchical-menu-choose
		       (menu-item-item-list item)
		       :associated-window associated-window
		       :default-style default-style
		       :x-position mr :y-position mt
		       :cache cache
		       :unique-id unique-id :id-test id-test
		       :cache-value cache-value :cache-test cache-test)))
		  (t (return-from hierarchical-menu-choose
		       (values (menu-item-value item) item gesture))))))))))


;;; Static menus

(defmethod print-object ((static-menu static-menu) stream)
  (print-unreadable-object (static-menu stream :type t :identity t)
    (write (slot-value static-menu 'name) :stream stream :escape nil)))

(defmacro define-static-menu (name root-window items
			      &rest keys
			      &key default-item default-style
				   printer presentation-type
				   max-width max-height n-rows n-columns
				   x-spacing y-spacing 
				   (cell-align-x ':left) (cell-align-y ':top))
  (declare (ignore max-width max-height n-rows n-columns
		   x-spacing y-spacing cell-align-x cell-align-y))
  (with-keywords-removed (drawer-keys keys
			  '(:default-item :default-style :printer :presentation-type))
    `(defvar ,name (define-static-menu-1 ',name ,root-window ',items
					 :default-item ',default-item
					 :default-style ',default-style
					 :presentation-type ',presentation-type
					 :printer ',printer
					 :drawer-args ,(copy-list drawer-keys)))))

(defun define-static-menu-1 (name root-window items
			     &key default-item default-style
				  printer presentation-type
				  drawer-args)
  (let ((menu-contents nil)
	(default-presentation nil))
    (when (typep root-window 'window-mixin)
      ;; Build the static menu if we can
      (flet ((present-item (item stream)
	       (present item presentation-type :stream stream)))
	(declare (dynamic-extent #'present-item))
	(let ((item-printer (cond (presentation-type #'present-item)
				  (printer printer)
				  (t #'print-menu-item))))
	  (with-menu (menu root-window)
	    (with-text-style (menu default-style)
	      (with-end-of-line-action (menu :allow)
		(with-output-recording-options (menu :draw nil :record t)
		  (setq menu-contents
			(with-new-output-record (menu)
			  (setq default-presentation
				(apply #'draw-standard-menu
				       menu 'menu-item items default-item
				       :item-printer item-printer
				       drawer-args)))))))))))
    (make-instance 'static-menu :name name
				:menu-contents menu-contents
				:default-presentation default-presentation
				:root-window root-window
				;; Save this in case we have to rebuild the menu
				:items items
				:default-item default-item
				:default-style default-style
				:printer printer
				:presentation-type presentation-type
				:drawer-args drawer-args)))

;;--- How to get FRAME-MANAGER-MENU-CHOOSE into the act here?
(defmethod menu-choose ((static-menu static-menu)
			&rest keys
			&key (associated-window (frame-top-level-sheet *application-frame*))
			     label default-style pointer-documentation
			&allow-other-keys)
  (declare (values value chosen-item gesture))
  (declare (dynamic-extent keys))
  (with-slots (name menu-contents items default-presentation root-window)
	      static-menu
    (let ((this-root (window-root associated-window)))
      (when (or (not (eq this-root root-window))
		(null menu-contents))
	;; If the root for the static menu is not the current root or the static
	;; menu has never been filled in, then we have to recompute its contents.
	(with-slots (default-item default-style printer presentation-type drawer-args)
		    static-menu
	  (let ((new-menu (define-static-menu-1 name this-root items
						:default-item default-item
						:default-style default-style
						:printer printer 
						:presentation-type presentation-type
						:drawer-args drawer-args)))
	    (setq menu-contents (slot-value new-menu 'menu-contents)
		  default-presentation (slot-value new-menu 'default-presentation)
		  root-window this-root)))))
    (with-menu (menu associated-window)
      (setf (window-label menu) label)
      (with-text-style (menu default-style)
	(multiple-value-bind (item gesture)
	    (menu-choose-from-drawer 
	      menu 'menu-item #'false		;the drawer never gets called
	      :cache static-menu
	      :unique-id name
	      :cache-value menu-contents
	      :default-presentation default-presentation
	      :pointer-documentation pointer-documentation)
	  (cond ((menu-item-item-list item)
		 (with-keywords-removed (keys keys '(:default-item))
		   (apply #'menu-choose (menu-item-item-list item) keys)))
		(t
		 (return-from menu-choose
		   (values (menu-item-value item) item gesture)))))))))

