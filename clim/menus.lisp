;; -*- mode: common-lisp; package: clim-internals -*-
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
;; $fiHeader$



(in-package :clim-internals)

"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved."

(defparameter *default-menu-margin-components* '((margin-borders :thickness 2)
						 (margin-white-borders :thickness 2)))

(define-application-frame menu-frame ()
  (menu)
  (:pane
   (with-slots (menu) *application-frame*
     (silica::scrolling ()
			(setq menu (silica::realize-pane 'extended-stream-pane
							 :initial-cursor-visibility nil
							 ))))))

(defun get-menu (&key server-path)
  (let ((frame (make-application-frame 'menu-frame)))
    (slot-value frame 'menu)))

;;; Still requires some thought, but at least this works in an environment
;;; with multiple ports.

(clim-utils::defresource menu (associated-window root)
	     :constructor (let* ((port (if (null root) (find-port) ; Horrible kluge #2.
					 (port root)))
		      (server-path (silica::port-server-path port)))
		 (get-menu :server-path server-path))
  :deinitializer (window-clear menu)
  ;; We used to have an initializer that set record-p, cleared the window, and set
  ;; the size.  Record-p is unnecessary because we explicitly bind it below
  ;; when drawing the menu contents.  (Also, it should always be T anyway.)
  ;; Clearing the window is unnecessary because it's done in the deinitializer.
  ;; Setting the size is also unnecessary.  Any application that's formatting
  ;; into a menu is required to use with-end-of-line-action and with-end-of-page-action
  ;; or set the size first itself.
  :initializer (initialize-menu (port menu) menu associated-window)
  :matcher (or (null root)
	       ;; horrible kludge in the case where no associated window is passed in.
	       (eql (port menu) (port root)))
  )

;;; This needs to be graft-specific.  In X, the transient-for property
;;; needs to be set to an X window, either one associated with the associated window
;;; or to one associated with window.  In other implementations, this can do nothing.
;;; --- What class for this default method?

(defmethod initialize-menu ((port port) window associated-window)
  (declare (ignore window associated-window))
  )

;;; For now, MENU-CHOOSE requires that you pass in a parent.
(defmacro with-menu ((menu associated-window) &body body)
  (let ((ass-win (gensymbol 'associated-window)))
    `(let ((,ass-win ,associated-window))			;once-only
       (clim-utils::using-resource (,menu menu (window-top-level-window ,ass-win) (window-root ,ass-win))
	 ,@body))))




(defun size-menu-appropriately (menu &key (right-margin 10) (bottom-margin 10))
  (with-slots (output-record) menu
    (with-bounding-rectangle* (minx miny maxx maxy) output-record
      (let* ((graft (graft menu))
	     (gw (bounding-rectangle-width (sheet-region graft)))
	     (gh (bounding-rectangle-height (sheet-region graft)))
	     ;; (vw (pane-viewport menu))
	     (width (min gw (+ (- maxx minx) right-margin)))
	     (height (min gh (+ (- maxy miny) bottom-margin))))

	(silica::change-space-req menu :width width :height height)
	;; --- Damn.  How do we get this to propagate the size change up
	;; the tree so that the whole frame gets re-size to the pane?
	;; For now, kludge it by setting the frame size directly.
	;; Allow for scroll bar width
	;; (layout-frame (silica::pane-frame menu) (+ width 20) (+ 20 height))
	;; (when vw (change-space-req vw :hs width :vs height))
	;; (resize-sheet* menu width height)

	(silica::clear-space-req-caching-in-ancestors menu)
	
	;; --- RR needs to fix this at some point.
	;; --- It has to do with the viewport having a bad scrolling transform
	;; after a resize.
	#+ignore
	(setf (sheet-transformation (sheet-parent menu))
	      (make-translation-transformation 
	       0 0))))))

;;; item-list := (item*)
;;; item := object | (name :value object) | (object . value)
;;; object := any lisp object.  It is written (using WRITE) into the menu using :ESCAPE NIL.

(defun menu-item-value (menu-item &aux rest)
  (cond ((atom menu-item) menu-item)
	((atom (setq rest (cdr menu-item))) rest)
	(t (getf rest :value))))

(defun menu-item-display (menu-item)
  (cond ((atom menu-item) menu-item)
	(t (first menu-item))))

(defun menu-item-style (menu-item)
  (let (rest)
    (cond ((atom menu-item) nil)
	  ((atom (setq rest (cdr menu-item))) nil)
	  (t (getf rest :style)))))

(defun menu-item-item-list (menu-item)
  (let (rest)
    (cond ((atom menu-item) nil)
	  ((atom (setq rest (cdr menu-item))) nil)
	  (t (getf rest :item-list)))))

(defun print-menu-item (menu-item &optional (stream *standard-output*))
  (let ((display (menu-item-display menu-item))
	(string nil)
	(style (menu-item-style menu-item)))
    (cond ((stringp display) (setq string display))
	  ;; this is a [horrible] kludge to get around the fact
	  ;; that we haven't made WRITE properly generic yet.
	  (t (setq string (with-output-to-string (stream)
			    (write display :stream stream :escape nil)))))
    ;;(write-string string stream) ;; Old way to do output.
    (if style
	(with-text-style (style stream)
	  (write-string string stream))
	(write-string string stream))))

;;; Perhaps misguided attempt at allowing performance win if you use
;;; simple unique-ids.
(defvar *menu-choose-output-history-caches* 
	(cons (make-hash-table :test #'eql)
	      (make-hash-table :test #'equal)))

(defun get-from-output-history-cache (unique-id unique-id-test)
  (let ((table (cond ((or (eql unique-id-test 'eql)
			  (eql unique-id-test '#,#'eql))
		      (car *menu-choose-output-history-caches*))
		     ((or (eql unique-id-test 'equal)
			  (eql unique-id-test '#,#'equal))
		      (cdr *menu-choose-output-history-caches*)))))
  (gethash unique-id table)))

(defun set-output-history-cache (unique-id unique-id-test new-value)
  (let ((table (cond ((or (eql unique-id-test 'eql)
			  (eql unique-id-test '#,#'eql))
		      (car *menu-choose-output-history-caches*))
		     ((or (eql unique-id-test 'equal)
			  (eql unique-id-test '#,#'equal))
		      (cdr *menu-choose-output-history-caches*)))))

    (setf (gethash unique-id table) new-value)))

(defsetf get-from-output-history-cache set-output-history-cache)

;;; What are reasonable defaults for CACHE, UNIQUE-ID, CACHE-VALUE, UNIQUE-ID-TEST, and
;;; CACHE-VALUE-TEST?
(defun menu-choose (item-list
		    &key associated-window default-item default-style
			 (cache nil) (unique-id item-list) (cache-value item-list)
			 (unique-id-test #'equal) (cache-value-test #'equal)
			 label
			 printer
			 presentation-type)
  #+Genera (declare (values value chosen-item gesture))
  (labels ((menu-choose (stream presentation-type)
	     (draw-standard-menu stream presentation-type
				 item-list (or default-item
					       (first item-list))
				 #'item-printer))
	   (item-printer (item stream)
	     (cond (presentation-type
		    (present item presentation-type :stream stream))
		   (printer
		    (funcall printer item stream))
		   (t (print-menu-item item stream)))))
    (declare (dynamic-extent #'item-printer #'menu-choose))
    (with-menu (menu associated-window)
      (reset-frame (silica::pane-frame menu) :title label)
      (with-text-style (default-style menu)
	(loop
	  (multiple-value-bind (item gesture)
	      (menu-choose-from-drawer 
	       menu 'menu-item
	       #'menu-choose
	       :unique-id unique-id
	       :unique-id-test unique-id-test
	       :cache-value cache-value
	       :cache-value-test cache-value-test
	       :cache cache)
	    (cond ((menu-item-item-list item)
		   ;; set the new item list then go back through the loop
		   (setq item-list (menu-item-item-list item) default-item nil)
		   (clear-output-history menu))
		  (t (return-from menu-choose (values (menu-item-value item)
						      item
						      gesture))))))))))

;(defun draw-standard-menu (menu presentation-type item-list)
;  (dolist (item item-list)
;    ;; this will eventually call PRESENT itself.
;    (with-output-as-presentation (:stream menu
;				  :object item
;				  :type presentation-type)
;      (print-menu-item item menu))
;    (terpri menu)))

(defun draw-standard-menu (menu presentation-type item-list default-item
			   &optional (item-printer #'print-menu-item)
			   &aux default-pres)
  (formatting-item-list (menu :move-cursor nil)
    (dolist (item item-list)
      ;; this will eventually call PRESENT itself.
      (let ((pres (with-output-as-presentation (:stream menu
					    :object item
					    :type presentation-type
					    :single-box t)
		(formatting-cell (menu)
		  (funcall item-printer item menu)))))
	(when (eql item default-item) (setf default-pres pres)))))
  default-pres)

(defun position-window-at-pointer (window &optional x y)
  (unless (and x y)
    (multiple-value-setq (x y)

      (poll-pointer (graft window))))
  (position-window-near-carefully window x y))
  

;;; The drawer gets called with (stream presentation-type &rest drawer-args).
;;; It can use presentation-type for its own purposes.  The most common uses
;;; are
;;;  1) Using that type as the presentation-type of the presentations it makes for each item
;;;  2) Using that type to find a printer for PRESENT purposes
;;;  3) Presenting a different set of choices based on the type, or graying over things
;;;     that are of different types.  See example below
(defun menu-choose-from-drawer (menu type drawer &key
				     x-position y-position
				     ;; for use by hierarchichal-menu-choose
				     leave-menu-visible
				     unique-id (cache-value t) cache
				     (unique-id-test #'equal) (cache-value-test #'eql)
				     &aux default-presentation)
  ;; We could make the drawer a lexical closure, but that would then
  ;; partially defeat the purpose of the uid and cache-value because we'd cons the closure
  ;; whether or not we ran it.
  ;; yecch
  (stream-set-cursor-position* menu 5 5)
  (let ((old-output-history-info
	  (and cache (get-from-output-history-cache unique-id unique-id-test)))
	(old-menu-contents nil))
    (cond ((and old-output-history-info
		(setq old-menu-contents (pop old-output-history-info))
		(funcall cache-value-test old-output-history-info cache-value))
	   (stream-add-output-record menu old-menu-contents))
	  (t
	   ;; "Draw" into deexposed menu for sizing only
	   (with-output-recording-options (menu :draw-p nil :record-p t)
	     (let ((old-contents
		     (with-new-output-record (menu)
		       (setf default-presentation (funcall drawer menu type)))))
	       (when cache
		 (setf (get-from-output-history-cache unique-id unique-id-test)
		       (cons old-contents cache-value))))))))
  (size-menu-appropriately menu)
  (position-window-at-pointer menu x-position y-position)
  (unwind-protect
      (clim-utils::with-simple-abort-restart ("Exit the menu")
	;;; --- Figure out later why this is necessary...
	;;; it isn't now, 'cause we set the viewport in size-menu-appropriately
	;;(window-set-viewport-position menu 0 0)
	(window-expose menu)
	(when default-presentation
	  (multiple-value-bind (left top right bottom)
	      (bounding-rectangle* default-presentation)
	    (stream-set-pointer-position*
	      menu (floor (+ left right) 2) (floor (+ top bottom) 2))))
	;; Menus DON'T want a blinking cursor...
	(with-cursor-visibility (nil menu)
	  (with-input-context (type :override t)
			      (object presentation-type gesture)
	       (loop (read-gesture :stream menu)
		     (beep))
	     (t (values object gesture)))))
    (unless leave-menu-visible
      (window-set-visibility menu nil))
    (stream-force-output menu)))

#||
(defun choose-graphical-icon (icon-list &key parent cache)
  (labels ((draw-icon (icon stream)
	     (ecase icon
	       ((:rectangle :square) (draw-rectangle* 0 0 20 20 :stream stream))
	       (:triangle (draw-triangle* 0 0 20 0 10 20 :stream stream))
	       (:circle (draw-circle* 10 10 10 :stream stream))))
	   (draw-icon-menu (menu presentation-type)
	     #+Genera (declare (sys:downward-function))
	     (formatting-table (menu :inter-row-spacing 5)
	       (dolist (icon icon-list)
		 (with-output-as-presentation (:stream menu
					       :object icon
					       :type presentation-type)
		   (formatting-row (menu)
		     (formatting-cell (menu)
		       (with-user-coordinates (menu)
			 (draw-icon icon menu)))))))
	     nil))
    (with-menu (menu parent)
      (menu-choose-from-drawer menu 'menu-item #'draw-icon-menu :unique-id icon-list 
			       :cache cache
			       :cache-value T))))

;;; Why is this so much damn faster to pop up than CLIM?
(defun choose-graphical-icon-dw (icon-list &key parent)
  (labels ((draw-icon (icon stream)
	     (ecase icon
	       ((:rectangle :square) (graphics:draw-rectangle 0 0 20 20 :stream stream))
	       (:triangle (graphics:draw-triangle 0 0 20 0 10 20 :stream stream))
	       (:circle (graphics:draw-circle 10 10 10 :stream stream))))
	   (draw-icon-menu (menu &rest ignored)
	     #+Genera (declare (sys:downward-function))
	     (scl:formatting-table (menu :inter-row-spacing 5)
	       (dolist (icon icon-list)
		 (dw:with-output-as-presentation (:stream menu
					       :object icon
					       :type 'symbol)
		   (scl:formatting-row (menu)
		     (scl:formatting-cell (menu)
		       (multiple-value-bind (x y)
			   (scl:send menu :read-cursorpos)
			 (graphics:with-graphics-translation (menu x y)
			   (draw-icon icon menu))))))))
	     nil))
    (dw:menu-choose-from-drawer #'draw-icon-menu 'symbol)))

(defun draw-number-menu (menu presentation-type)
  (formatting-table (menu)
    (do ((i 1 (1+ i)))
	((= i 11))
      (formatting-row (menu)
	(formatting-cell (menu)
	  (with-user-coordinates (menu)
	    (let* ((type (if (oddp i) 'odd 'even))
		   (presentation (with-output-as-presentation
				   (:stream menu
				    :type type
				    :object i)
				   (draw-string* (format nil "~2,' D" i) 0 0 :stream menu)
				   ;; Write-string will try to write
				   ;; at, say, 300, 300.  Some thought is required
				   #+Ignore
				   (write-string (format nil "~D" i) menu))))
	      (unless (eql presentation-type type)
		(let ((extent presentation))
		  (multiple-value-bind (left top right bottom) (entity-edges extent)
		    (multiple-value-bind (x y) (entity-position presentation)
		      (draw-rectangle* (+ left x) (+ top y)
				       (+ right x) (+ bottom y)
				       :scale-x 1 :scale-y -1
				       :gray-level .1 :stream menu))))))))))))

(defun style-and-multiple-item-test (root)
  (let ((apple-item (list "Apples" :value :apples :style '(nil :bold nil)))
	(orange-item (list "Oranges" :value :oranges :style '(nil (:bold :italic) nil)))
	(grapes (list "Grapes, normal" :value :grapes :style '(nil :italic nil)))
	(grapefruit (list "Grapes, sour" :value :grapefruit :style '(nil nil :large))))
    (menu-choose (list apple-item orange-item grapes grapefruit) :default-item orange-item
		 :default-style '(nil nil :very-large) :parent root)))

(defun gray-number-test (&optional (presentation-type 'odd))
  (with-menu (menu root)			;aaugh!
    (menu-choose-from-drawer menu presentation-type #'draw-number-menu)))

||#

(defun hierarchical-menu-choose (item-list
				 &key associated-window default-item default-style
				      x-position y-position
				      (cache nil) (unique-id item-list)
				      (cache-value item-list)
				      (unique-id-test #'equal) (cache-value-test #'equal)
				      label printer presentation-type)
  #+Genera (declare (values value chosen-item gesture))
  (labels ((menu-choose (stream presentation-type)
	     (draw-standard-menu stream presentation-type
				 item-list (or default-item
					       (first item-list))
				 #'item-printer))
	   (item-printer (item stream)
	     #+Genera (declare (sys:downward-function))
	     (cond (presentation-type
		    (present item presentation-type :stream stream))
		   (printer
		    (funcall printer item stream))
		   (t (print-menu-item item stream)))))
    (declare (dynamic-extent #'menu-choose #'item-printer))
    (with-menu (menu associated-window)
      (reset-frame (silica::pane-frame menu) :title label)
      (with-text-style (default-style menu)
	(unwind-protect
	    (multiple-value-bind (item gesture)
		(menu-choose-from-drawer
		  menu 'menu-item #'menu-choose
		  :x-position x-position :y-position y-position
		  :leave-menu-visible t
		  :unique-id unique-id
		  :unique-id-test unique-id-test
		  :cache-value cache-value
		  :cache-value-test cache-value-test
		  :cache cache)
	      (cond ((menu-item-item-list item)
		     (multiple-value-bind (ml mt mr mb) (entity-edges menu)
		       (declare (ignore ml mb))
		       ;;--- How to pass on LABEL, PRINTER, and PRESENTATION-TYPE?
		       (hierarchical-menu-choose (menu-item-item-list item)
						 :associated-window associated-window
						 :default-style default-style
						 :x-position mr :y-position mt
						 :unique-id unique-id
						 :unique-id-test unique-id-test
						 :cache-value cache-value
						 :cache-value-test cache-value-test
						 :cache cache)))
		    (t (return-from hierarchical-menu-choose
			 (values (menu-item-value item)
				 item
				 gesture)))))
	  (window-set-visibility menu nil))))))

;;; Weird shit

(defun window-top-level-window (win) win)
(defun window-root (win) win)


(defun reset-frame (frame &rest ignore) nil)
(defun poll-pointer (x) (values 500 500))

(defmethod window-expose ((stream extended-stream-pane))
  (setf (window-visibility stream) t))

(defun window-set-visibility (window visibility)
  (setf (window-visibility window) visibility))


(defmethod (setf window-visibility) (nv (stream extended-stream-pane))
  (if nv 
      (enable-frame (silica::pane-frame stream))
      (disable-frame (silica::pane-frame stream))))

(defun entity-edges (x)
  (bounding-rectangle* x))




(defun position-window-near-carefully (menu x y)
  #+ignore
  (let* ((frame (silica::pane-frame menu))
	 (frame-manager (frame-manager frame))
	 (graft (graft frame-manager)))
    ;; Make sure the menu will fit inside the graft.
    (with-bounding-rectangle* (min-x min-y max-x max-y) (sheet-region graft)
      (multiple-value-bind (width height) (bounding-rectangle-size (sheet-region menu))
	(setf x (max min-x (min x (- max-x width))))
	(setf y (max min-y (min y (- max-y height))))))
    (move-frame frame x y)))

(warn "where should these be")

(defmethod clim-internals::window-refresh (window)
  (warn "What does this do???"))


(defmethod window-viewport-position* (window)
  (bounding-rectangle*
   (pane-viewport-region window)))


(defmethod window-set-viewport-position* (window x y)
  (scroll-extent window :x x :y y))

(defmethod window-inside-size (window)
  (bounding-rectangle-size (pane-viewport-region window)))

(defun windowp (x)
  (typep x 'silica::sheet))

(defmethod (setf window-label) (nv window)
  nil)
