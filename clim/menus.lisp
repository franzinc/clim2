;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca.  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, Ca.  All rights reserved.
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
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;

;; $fiHeader: menus.cl,v 1.1 91/11/21 17:06:39 cer Exp Locker: cer $

(in-package :clim-internals)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved.
Copyright (c) 1991, Franz Inc. All rights reserved
 Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

(defvar *abort-menus-when-buried* t)

;;--- This associated-window/root stuff is pretty irksome.
;;--- Menus should really be application frames.

(clim-utils::defresource menu (associated-window root)
  :constructor (open-window-stream :parent root	;"random" size
				   :left 100 :top 100 :width 300 :height 200
				   :scroll-bars ':vertical
				   :save-under t)
  ;; We used to have an initializer that set RECORD-P, cleared the window,
  ;; and set the size.  RECORD-P is unnecessary because we explicitly bind it
  ;; below when drawing the menu contents.  (Also, it should always be T
  ;; anyway.)  Clearing the window is unnecessary because it's done in the
  ;; deinitializer.  Setting the size is also unnecessary.  Any application
  ;; that's formatting into a menu is required to use WITH-END-OF-LINE-ACTION
  ;; and WITH-END-OF-PAGE-ACTION or set the size first itself.
  :initializer (initialize-menu menu associated-window)
  ;; Deexpose the menu, and clear it so that the GC can reclaim any
  ;; garbage output records.
  :deinitializer (progn (setf (window-visibility menu) nil)
			(window-clear menu)
			#-Silica (setf (window-label menu) nil))
  :matcher (eql (window-parent menu) root))

(defmacro with-menu ((menu associated-window) &body body)
  `(with-menu-1  ,associated-window #'(lambda (,menu) ,@body)))

(defun with-menu-1 (window continuation)
  (funcall continuation (get-menu)))


(define-application-frame menu-frame ()
  (menu)
  (:pane 
   (with-slots (menu) *application-frame*
     (setq menu (silica::realize-pane 'interactor-pane)))))

(defun get-menu (&key server-path)
  (let* ((framem (find-frame-manager))
	 (frame (make-application-frame 'menu-frame)))
    (adopt-frame framem frame)
    frame
    (slot-value frame 'menu)))
    
;; no window-mixin
#+ignore
(defmethod initialize-menu ((menu window-mixin) associated-window)
  (declare (ignore associated-window))
  )

#+ignore
(defmethod setup-popup-menu ((menu window-mixin) associated-window)
  (declare (ignore associated-window))
  )

(defun size-menu-appropriately (menu &key (right-margin 10) (bottom-margin 10))
  (with-slots (output-record parent) menu
    (with-bounding-rectangle* (left top right bottom) output-record
      (multiple-value-bind (label-width label-height)
	  (window-label-size menu)
	(multiple-value-bind (parent-width parent-height)
	    (window-inside-size parent)
	  (multiple-value-bind (lm tm rm bm)
	      (host-window-margins menu)
	    (multiple-value-bind (wlm wtm wrm wbm)
		(window-margins menu)
	      (bounding-rectangle-set-size 
		menu
		(min (+ (max label-width (- right left)) right-margin wlm wrm lm rm)
		     parent-width)
		(min (+ (max label-height (- bottom top)) bottom-margin wtm wbm tm bm)
		     parent-height))))))
      (window-set-viewport-position* menu left top)
      ;; blech
      (clear-input menu))))

(defun position-window-near-carefully (window x y)
  ;; unfortunately, the mouse-position is in window-parent coordinates
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
	(bounding-rectangle-set-position* window
					  (max 0 left) (max 0 top))))))

;; item-list := (item*)
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

(defun menu-item-item-list (menu-item)
  (menu-item-getf menu-item :item-list))

;; Perhaps misguided attempt at allowing performance win if you use
;; simple unique-ids.
(defvar *menu-choose-output-history-caches* 
	(cons (make-hash-table :test #'eql)
	      (make-hash-table :test #'equal)))

(defun get-from-output-history-cache (unique-id id-test)
  (let ((table (cond ((or (eql id-test 'eql)
			  (eql id-test '#,#'eql))
		      (car *menu-choose-output-history-caches*))
		     ((or (eql id-test 'equal)
			  (eql id-test '#,#'equal))
		      (cdr *menu-choose-output-history-caches*)))))
  (gethash unique-id table)))

(defun set-output-history-cache (unique-id id-test new-value)
  (let ((table (cond ((or (eql id-test 'eql)
			  (eql id-test '#,#'eql))
		      (car *menu-choose-output-history-caches*))
		     ((or (eql id-test 'equal)
			  (eql id-test '#,#'equal))
		      (cdr *menu-choose-output-history-caches*)))))
    (setf (gethash unique-id table) new-value)))

(defsetf get-from-output-history-cache set-output-history-cache)

(define-presentation-type menu-item ())

(defun draw-standard-menu (menu presentation-type item-list default-item
			   &key (item-printer #'print-menu-item)
				max-width max-height n-rows n-columns
				inter-column-spacing inter-row-spacing 
				(cell-align-x ':left) (cell-align-y ':top)
			   &aux default-presentation)
  (formatting-item-list (menu :max-width max-width :max-height max-height
			      :n-rows n-rows :n-columns n-columns
			      :inter-column-spacing inter-column-spacing
			      :inter-row-spacing inter-row-spacing
			      :move-cursor nil)
    (dolist (item item-list)
      (let ((presentation
	      (with-output-as-presentation (:stream menu
					    :object item
					    :type presentation-type
					    :single-box t)
		(formatting-cell (menu :align-x cell-align-x :align-y cell-align-y)
		  (funcall item-printer item menu)))))
	(when (and default-item (eql item default-item))
	  (setf default-presentation presentation)))))
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
	(with-text-style (style stream)
	  (write-string string stream))
	(write-string string stream))))

(defgeneric menu-choose (item-list &rest keys
			 &key associated-window default-item default-style
			      label printer presentation-type
			      cache unique-id id-test cache-value cache-test
			      max-width max-height n-rows n-columns
			      inter-column-spacing inter-row-spacing
			      cell-align-x cell-align-y
			      pointer-documentation))

;; Are these reasonable defaults for UNIQUE-ID, CACHE-VALUE, ID-TEST, and CACHE-TEST?
(defmethod menu-choose ((item-list t)
			&rest keys
			&key (associated-window (frame-top-level-window *application-frame*))
			     default-item default-style
			     label printer presentation-type
			     (cache nil) (unique-id item-list) (id-test #'equal)
			     (cache-value item-list) (cache-test #'equal)
			     max-width max-height n-rows n-columns
			     inter-column-spacing inter-row-spacing 
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
	  #+Lucid (item-list item-list))
      (with-menu (menu associated-window)
	#-Silica (setf (window-label menu) label)
	(with-text-style (default-style menu)
	  (with-end-of-line-action (:allow menu)
	    (loop
	      (multiple-value-bind (item gesture)
		  (flet ((menu-choose-body (stream presentation-type)
			   (draw-standard-menu stream presentation-type item-list default-item
					       :item-printer item-printer
					       :max-width max-width :max-height max-height
					       :n-rows n-rows :n-columns n-columns
					       :inter-column-spacing inter-column-spacing
					       :inter-row-spacing inter-row-spacing 
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
		       ;; Set the new item list, then go back through the loop
		       (setq item-list (menu-item-item-list item)
			     default-item nil)
		       (clear-output-history menu))
		      (t (return-from menu-choose
			   (values (menu-item-value item) item gesture))))))))))))

(defclass static-menu ()
    ((name :initarg :name)
     (menu-contents :initarg :menu-contents)
     (default-presentation :initarg :default-presentation)
     (root-window :initarg :root-window)
     ;; In case we need to "compile" the menu on the fly
     (item-list :initarg :item-list)
     (default-item :initarg :default-item)
     (default-style :initarg :default-style)
     (printer :initarg :printer)
     (presentation-type :initarg :presentation-type)
     (drawer-args :initarg :drawer-args)))

(defmethod print-object ((static-menu static-menu) stream)
  (print-unreadable-object (static-menu stream :type t :identity t)
    (write (slot-value static-menu 'name) :stream stream :escape nil)))

(defmacro define-static-menu (name root-window item-list
			      &rest keys
			      &key default-item default-style
				   printer presentation-type
				   max-width max-height n-rows n-columns
				   inter-column-spacing inter-row-spacing 
				   (cell-align-x ':left) (cell-align-y ':top))
  (declare (ignore max-width max-height n-rows n-columns
		   inter-column-spacing inter-row-spacing cell-align-x cell-align-y))
  (with-rem-keywords (drawer-keys keys
		      '(:default-item :default-style :printer :presentation-type))
    `(defvar ,name (define-static-menu-1 ',name ,root-window ',item-list
					 :default-item ',default-item
					 :default-style ',default-style
					 :presentation-type ',presentation-type
					 :printer ',printer
					 :drawer-args ,(copy-list drawer-keys)))))

(defun define-static-menu-1 (name root-window item-list
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
	    (with-text-style (default-style menu)
	      (with-end-of-line-action (:allow menu)
		(with-output-recording-options (menu :draw-p nil :record-p t)
		  (setq menu-contents
			(with-new-output-record (menu)
			  (setq default-presentation
				(apply #'draw-standard-menu
				       menu 'menu-item item-list default-item
				       :item-printer item-printer
				       drawer-args)))))))))))
    (make-instance 'static-menu :name name
				:menu-contents menu-contents
				:default-presentation default-presentation
				:root-window root-window
				;; Save this in case we have to rebuild the menu
				:item-list item-list
				:default-item default-item
				:default-style default-style
				:printer printer
				:presentation-type presentation-type
				:drawer-args drawer-args)))

(defmethod menu-choose ((static-menu static-menu)
			&rest keys
			&key (associated-window (frame-top-level-window *application-frame*))
			     label default-style pointer-documentation
			&allow-other-keys)
  (declare (values value chosen-item gesture))
  (declare (dynamic-extent keys))
  (with-slots (name menu-contents item-list default-presentation root-window)
	      static-menu
    (let ((this-root (window-root associated-window)))
      (when (or (not (eql this-root root-window))
		(null menu-contents))
	;; If the root for the static menu is not the current root or the static
	;; menu has never been filled in, then we have to recompute its contents.
	(with-slots (default-item default-style printer presentation-type drawer-args)
		    static-menu
	  (let ((new-menu (define-static-menu-1 name this-root item-list
						:default-item default-item
						:default-style default-style
						:printer printer 
						:presentation-type presentation-type
						:drawer-args drawer-args)))
	    (setq menu-contents (slot-value new-menu 'menu-contents)
		  default-presentation (slot-value new-menu 'default-presentation)
		  root-window this-root)))))
    (with-menu (menu associated-window)
      #-Silica (setf (window-label menu) label)
      (with-text-style (default-style menu)
	(multiple-value-bind (item gesture)
	    (menu-choose-from-drawer 
	      menu 'menu-item #'ignore		;the drawer never gets called
	      :cache static-menu
	      :unique-id name
	      :cache-value menu-contents
	      :default-presentation default-presentation
	      :pointer-documentation pointer-documentation)
	  (cond ((menu-item-item-list item)
		 (with-rem-keywords (keys keys '(:default-item))
		   (apply #'menu-choose (menu-item-item-list item) keys)))
		(t
		 (return-from menu-choose
		   (values (menu-item-value item) item gesture)))))))))

;; The drawer gets called with (stream presentation-type &rest drawer-args).
;; It can use presentation-type for its own purposes.  The most common uses are:
;;  1) Using that type as the presentation-type of the presentations it makes for each item
;;  2) Using that type to find a printer for PRESENT purposes
;;  3) Presenting a different set of choices based on the type, or graying over things
;;     that are of different types.  See example below

(defmacro with-mouse-grabbed-in-window ((window &rest options) &body body)
  `(with-mouse-grabbed-in-window-internal ,window #'(lambda () ,@body)
					  ,@options))

(defmethod with-mouse-grabbed-in-window-internal (window continuation &key)
  (declare (ignore window))
  (funcall continuation))

(defmacro with-menu-as-a-popup ((menu) &body body)
  `(with-menu-as-a-popup-internal ,menu #'(lambda () ,@body)))

(defmethod with-menu-as-a-popup-internal (window continuation)
  (declare (ignore window))
  (funcall continuation))


#+excl
(defvar *click-outside-menu-handler* nil)

(defun menu-choose-from-drawer (menu type drawer
				     &key x-position y-position
				     cache unique-id (id-test #'equal) 
				     (cache-value t) (cache-test #'eql)
					  ;; for use by HIERARCHICHAL-MENU-CHOOSE
				     leave-menu-visible
				     (default-presentation nil)
				     pointer-documentation)
  ;;--- This should be done in a more modular way
  ;;--- If you change this, change RUN-FRAME-TOP-LEVEL :AROUND
  (flet (#+excl
	 (abort-menu-handler () 
	   (return-from menu-choose-from-drawer nil)))
    (let (#+excl
	  (*click-outside-menu-handler* #'abort-menu-handler)
	(*original-stream* nil)
	(*input-wait-test* nil)
	(*input-wait-handler* nil)
	(*pointer-button-press-handler* nil)
	(*generate-button-release-events* nil)
	(*numeric-argument* nil)
	(*blip-characters* nil)
	(*activation-characters* nil)
	(*accelerator-characters* nil)
	(*input-context* nil)
	(*accept-help* nil)
	(*assume-all-commands-enabled* nil)
	(*sizing-application-frame* nil)
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
	     (add-output-record menu cached-menu-contents))
	    (t
	     ;; "Draw" into deexposed menu for sizing only
	     (with-output-recording-options (menu :draw-p nil :record-p t)
					    (let ((menu-contents
						   (with-new-output-record (menu)
									   (setq default-presentation (funcall drawer menu type)))))
					      (when cache
						(setf (get-from-output-history-cache unique-id id-test)
						      (cons menu-contents cache-value))))))))
    (size-menu-appropriately menu)
    (unless (and x-position y-position)
      (multiple-value-setq (x-position y-position)
	  (stream-pointer-position-in-window-coordinates (window-parent menu))))
    
    (unwind-protect
	(with-menu-as-a-popup (menu)
	  (position-window-near-carefully menu x-position y-position)
	  (window-expose menu)
	  #+Cloe-Runtime (stream-set-input-focus menu)
	  (when default-presentation
	    (with-bounding-rectangle* (left top right bottom) default-presentation
				      (stream-set-pointer-position*
				       menu (floor (+ left right) 2) (floor (+ top bottom) 2))))
	  ;; Pointer documentation usually adds no information, and slows things
	  ;; down in a big way, which is why we defaultly disable it.
	  (let ((*pointer-documentation-output* pointer-documentation))
	    (with-input-context (type :override t)
				(object presentation-type gesture)
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
		  (wait-for-window-exposed menu))
		(with-mouse-grabbed-in-window (menu)
					      (loop (read-gesture :stream menu
								  :input-wait-test #'input-wait-test
								  :input-wait-handler #'input-wait-handler)
						    (beep))))
				(t (values object gesture)))))
      (unless leave-menu-visible
	(setf (window-visibility menu) nil))
      (force-output menu)))))


(defun hierarchical-menu-choose (item-list
				 &key (associated-window
					(frame-top-level-window *application-frame*))
				      default-item default-style
				      label printer presentation-type
				      x-position y-position
				      (cache nil)
				      (unique-id item-list) (id-test #'equal)
				      (cache-value item-list) (cache-test #'equal)
				      max-width max-height n-rows n-columns
				      inter-column-spacing inter-row-spacing 
				      (cell-align-x ':left) (cell-align-y ':top))
  (declare (values value chosen-item gesture))
  (flet ((present-item (item stream)
	   (present item presentation-type :stream stream)))
    (declare (dynamic-extent #'present-item))
    (let ((item-printer (cond (presentation-type #'present-item)
			      (printer printer)
			      (t #'print-menu-item))))
      (with-menu (menu associated-window)
	#-Silica (setf (window-label menu) label)
	(with-text-style (default-style menu)
	  (multiple-value-bind (item gesture)
	      (flet ((menu-choose-body (stream presentation-type)
		       (draw-standard-menu stream presentation-type item-list default-item
					   :item-printer item-printer
					   :max-width max-width :max-height max-height
					   :n-rows n-rows :n-columns n-columns
					   :inter-column-spacing inter-column-spacing
					   :inter-row-spacing inter-row-spacing 
					   :cell-align-x cell-align-x :cell-align-y cell-align-y)))
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

