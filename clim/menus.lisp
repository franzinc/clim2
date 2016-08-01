;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

(defvar *abort-menus-when-buried* t)

(defparameter *default-menu-text-style* (make-text-style :sans-serif :roman :small))
(defparameter *default-menu-label-text-style*
    (merge-text-styles (make-text-style nil :italic nil)
		       *default-menu-text-style*))

(define-application-frame menu-frame ()
  (menu
   (label :initarg :label :initform nil :accessor menu-frame-label)
   (scroll-bars :initarg :scroll-bars
                :initform t
                :reader menu-frame-scroll-bars))
  (:pane
   (with-slots (menu label scroll-bars) *application-frame*
     (outlining ()
       (let ((main
              (if scroll-bars
                  (scrolling (:scroll-bars scroll-bars)
                    (setq menu (make-pane 'clim-stream-pane
                                          :initial-cursor-visibility nil)))
                (setq menu (make-pane 'clim-stream-pane
                                      :initial-cursor-visibility nil)))))
         (if label
             (vertically ()
               (setq label (make-pane 'label-pane
                                      :label ""
                                      :text-style *default-menu-label-text-style*))
               main)
           main)))))
  (:menu-bar nil))

;(defmethod frame-calling-frame ((frame menu-frame))
;  ;; This is funny
;  ;; I suppose we are doing this because really the frame should have
;  ;; the correct parent and there is no otherway of doing this
;  (and (boundp '*application-frame*)
;       *application-frame*))

;; Returns a stream that corresponds to the pane holding the menu
(defmethod frame-manager-get-menu ((framem standard-frame-manager) &key scroll-bars label parent-frame)
  (declare (ignore parent-frame))
  (let ((frame (make-application-frame 'menu-frame
                                       :label label
                                       :scroll-bars scroll-bars
                                       :frame-manager framem
                                       :save-under t)))
    ;; This so that ports can do something interesting with popped-up
    ;; menu frames, such as implemented "click off menu to abort".
    (setf (getf (frame-properties frame) :menu-frame) t)
    (values (slot-value frame 'menu) frame)))

(defresource menu (associated-window root &key label (scroll-bars t))
             :constructor
             (let* ((framem (if (null root) (find-frame-manager) (frame-manager root))))
               (frame-manager-get-menu framem
                                       :scroll-bars scroll-bars
                                       :label label
                                       :parent-frame (pane-frame root)))
             :matcher (and (eq scroll-bars (menu-frame-scroll-bars (pane-frame menu)))
                           (eq (not label) (not (menu-frame-label (pane-frame menu))))
                           (eq (frame-manager menu) (frame-manager root))
			   
			   #+microsoft	; frames don't work across threads
			   (eq (current-process) (sheet-thread
						  (frame-top-level-sheet
						   (pane-frame menu))))			   
			   )

             :deinitializer (progn
                              (setf (window-visibility menu) nil)
                              (window-clear menu))
             :initializer (initialize-menu (port menu) menu :label label))

(defmethod initialize-menu ((port basic-port) menu &key label)
  ;;--- Should this flush the menu's event queue?
  (setf (stream-default-text-margin menu) nil)
  (when label
    (let ((text-style (if (listp label)
                          (getf (rest label) :text-style *default-menu-label-text-style*)
                        *default-menu-label-text-style*))
          (label (if (listp label) (first label) label))
          (label-pane (slot-value (pane-frame menu) 'label)))
      (setf (gadget-label label-pane) (or label "")
            (pane-text-style label-pane) (parse-text-style text-style)))))


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

(defun menu-item-getf (menu-item indicator &optional default)
  (let (rest)
    (cond ((atom menu-item) default)
          ((atom (setq rest (cdr menu-item))) default)
          (t (getf rest indicator default)))))

(defun menu-item-text-style (menu-item)
  (menu-item-getf menu-item :text-style))

(defun menu-item-documentation (menu-item)
  (menu-item-getf menu-item :documentation))

(defun menu-item-type (menu-item)
  (or (menu-item-getf menu-item :type)
      :item))

(defun menu-item-active (menu-item)
  (or (atom menu-item)
      (menu-item-getf menu-item :active t)))

;; A submenu, which can be a general sequence...
(defun menu-item-items (menu-item)
  (menu-item-getf menu-item :items))

;; Perhaps misguided attempt at allowing performance win if you use
;; simple unique-ids.
(defvar *menu-choose-output-history-caches*
        (cons (make-hash-table :test #'eql)
              (make-hash-table :test #'equal)))

(defvar *load-time-eql* #'eql)
(defvar *load-time-equal* #'equal)

(defun get-from-output-history-cache (unique-id id-test)
  (let ((table (cond ((or (eq id-test 'eql)
                          (eq id-test *load-time-eql*))
                      (car *menu-choose-output-history-caches*))
                     ((or (eq id-test 'equal)
                          (eq id-test *load-time-equal*))
                      (cdr *menu-choose-output-history-caches*)))))
  (gethash unique-id table)))

(defun set-output-history-cache (unique-id id-test new-value)
  (let ((table (cond ((or (eq id-test 'eql)
                          (eq id-test *load-time-eql*))
                      (car *menu-choose-output-history-caches*))
                     ((or (eq id-test 'equal)
                          (eq id-test *load-time-equal*))
                      (cdr *menu-choose-output-history-caches*)))))
    (setf (gethash unique-id table) new-value)))

(defsetf get-from-output-history-cache set-output-history-cache)

(define-presentation-type menu-item ())

;; This translator is here to compute menu item documentation
(define-presentation-translator menu-item-identity
    (menu-item menu-item global-command-table
     :priority 1                ;prefer this to IDENTITY
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
                                x-spacing y-spacing (row-wise nil)
                                (cell-align-x ':left) (cell-align-y ':top)
                           &aux default-presentation)
  (formatting-item-list (menu :max-width max-width :max-height max-height
			      :initial-spacing 1
                              :n-rows n-rows :n-columns n-columns
                              :x-spacing x-spacing :y-spacing y-spacing
                              :row-wise row-wise :move-cursor nil)
    (flet ((format-item (item)
             (let ((type (menu-item-type item)))
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
		      (with-drawing-options (menu :ink *command-table-menu-gray*
						  :text-face :bold)
			(print-item))))
                   (:label
		    (print-item))
                   (:divider
		    (let* ((width (menu-item-getf item :width 50))
			   (thickness (menu-item-getf item :thickness 2))
			   (ink (menu-item-getf item :ink *command-table-menu-gray*)))
		      (formatting-cell (menu :align-x cell-align-x
					     :align-y :center)
			(with-local-coordinates (menu)
			  (draw-line* menu 0 0 width 0
				      :line-thickness thickness :ink ink))))))))))
      (declare (dynamic-extent #'format-item))
      (map nil #'format-item items)))
  default-presentation)

(defun print-menu-item (menu-item &optional (stream *standard-output*))
  (let ((display (menu-item-display menu-item))
        (string nil)
        (style (menu-item-text-style menu-item)))
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
     (text-style :initarg :text-style)
     (printer :initarg :printer)
     (presentation-type :initarg :presentation-type)
     (drawer-args :initarg :drawer-args)))

(defgeneric menu-choose (items &rest keys
                         &key associated-window text-style
                              default-item foreground background
                              label scroll-bars printer presentation-type
                              cache unique-id id-test cache-value cache-test
                              max-width max-height n-rows n-columns
                              x-spacing y-spacing
                              cell-align-x cell-align-y
                              pointer-documentation menu-type
                              x-position y-position))

;; Are these reasonable defaults for UNIQUE-ID, CACHE-VALUE, ID-TEST, and CACHE-TEST?
(defmethod menu-choose ((items t) &rest keys
                        &key (associated-window (frame-top-level-sheet *application-frame*))
                             text-style default-item foreground background
                             label (scroll-bars t) printer presentation-type
                             (cache nil) (unique-id items) (id-test #'equal)
                             (cache-value items) (cache-test #'equal)
                             max-width max-height n-rows n-columns
                             x-spacing y-spacing (row-wise nil)
                             (cell-align-x ':left) (cell-align-y ':top)
                             pointer-documentation menu-type gesture
                             x-position y-position)
  (declare (values value chosen-item gesture))
  (declare (ignore text-style default-item foreground background
                   label scroll-bars printer presentation-type
                   cache unique-id id-test cache-value cache-test
                   max-width max-height n-rows n-columns
                   x-spacing y-spacing row-wise cell-align-x cell-align-y
                   pointer-documentation menu-type gesture
                   x-position y-position))
  (declare (dynamic-extent keys))
  (unless (zerop (length items))
    (apply #'frame-manager-menu-choose (frame-manager associated-window) items keys)))

;; Specific ports can put :AROUND methods on this in order to use their own
;; kinds of menus.
(defmethod frame-manager-menu-choose
           ((framem standard-frame-manager) items &rest keys
            &key (associated-window
                   (frame-top-level-sheet *application-frame*))
                 text-style default-item
                 label (scroll-bars t) printer presentation-type
                 (cache nil) (unique-id items) (id-test #'equal)
                 (cache-value items) (cache-test #'equal)
                 max-width max-height n-rows n-columns
                 x-spacing y-spacing (row-wise nil)
                 (cell-align-x ':left) (cell-align-y ':top)
                 pointer-documentation menu-type gesture
                 background foreground)
  (declare (values value chosen-item gesture))
  (declare (ignore #-aclpc keys gesture))
  (flet ((present-item (item stream)
           (present item presentation-type :stream stream)))
    (declare (dynamic-extent #'present-item))
    (let ((item-printer (cond (presentation-type #'present-item)
                              (printer printer)
                              (t #'print-menu-item)))
          ;; Lucid production compiler tries to use an undefined internal
          ;; variable if this LET isn't done.
          #+Lucid (items items))
      (with-menu (menu associated-window :label label :scroll-bars scroll-bars)
        (reset-frame (pane-frame menu) :title label)
        (setf (medium-background menu) (or background
                                           (medium-background associated-window))
              (medium-foreground menu) (or foreground
                                           (medium-foreground associated-window)))
        (with-text-style (menu (or text-style *default-menu-text-style*))
          (with-end-of-line-action (menu :allow)
            (loop
              (multiple-value-bind (item gesture)
                  (flet ((menu-choose-body (stream presentation-type)
                           (draw-standard-menu stream presentation-type items default-item
                                               :item-printer item-printer
                                               :max-width max-width :max-height max-height
                                               :n-rows n-rows :n-columns n-columns
                                               :x-spacing x-spacing :y-spacing y-spacing
                                               :row-wise row-wise
                                               :cell-align-x cell-align-x
                                               :cell-align-y cell-align-y)))
                    (declare (dynamic-extent #'menu-choose-body))
                    (menu-choose-from-drawer
                      menu 'menu-item #'menu-choose-body
                      :cache cache
                      :unique-id unique-id :id-test id-test
                      :cache-value cache-value :cache-test cache-test
                      :pointer-documentation pointer-documentation
                      :menu-type menu-type))
                (cond ((menu-item-items item)
                       ;; Set the new item list, then go back through the loop.
                       ;; Don't cache, because that will cause us to see the same
                       ;; menu items again and again.
                       (setq items (menu-item-items item)
                             default-item nil
                             cache nil)
                       (clear-output-history menu))
                      (t (return-from frame-manager-menu-choose
                           (values (menu-item-value item) item gesture))))))))))))


(defmacro with-mouse-grabbed-in-window ((window &rest options) &body body)
  (let ((w (gensym)))
    `(flet ((with-mouse-grabbed-in-window-body () ,@body))
       (declare (dynamic-extent #'with-mouse-grabbed-in-window-body))
       (let ((,w ,window))
         (invoke-with-mouse-grabbed-in-window
           (frame-manager ,w) ,w #'with-mouse-grabbed-in-window-body ,@options)))))

(defmethod invoke-with-mouse-grabbed-in-window
           ((framem standard-frame-manager) (window t) continuation &key)
  (funcall continuation))

(defmacro with-menu-as-popup ((menu) &body body)
  (let ((w (gensym)))
    `(flet ((with-menu-as-popup-body () ,@body))
       (declare (dynamic-extent #'with-menu-as-popup-body))
       (let ((,w ,menu))
         (invoke-with-menu-as-popup
           (frame-manager ,w) ,w #'with-menu-as-popup-body)))))

(defmethod invoke-with-menu-as-popup
           ((framem standard-frame-manager) (sheet basic-sheet) continuation)
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
                                     pointer-documentation menu-type)
  (declare (ignore menu-type))
  (flet (#+allegro
         (abort-menu-handler ()
           (return-from menu-choose-from-drawer nil)))
    #+allegro (declare (dynamic-extent #'abort-menu-handler))
    (with-clim-state-reset (:all t
                                 :additional-bindings
                                 #+allegro ((*click-outside-menu-handler* #'abort-menu-handler))
                                 #-allegro nil)
      ;; We could make the drawer a lexical closure, but that would then
      ;; partially defeat the purpose of the uid and cache-value
      ;; because we'd cons the closure whether or not we ran it.
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
      (size-frame-from-contents menu)
      (unwind-protect
          (with-menu-as-popup (menu)
            (let ((top-level-sheet (frame-top-level-sheet (pane-frame menu))))
              (position-sheet-near-pointer
               top-level-sheet x-position y-position)
              (setf (sheet-pointer-cursor top-level-sheet) :menu
                    (window-visibility menu) t))
            ;;--- If we have windows with backing store then we dont get
            ;;--- exposure event and so nothing appears
            #+allegro (replay (stream-output-history menu) menu)
            (stream-set-input-focus menu)
            (when default-presentation
              (with-bounding-rectangle* (left top right bottom) default-presentation
                                        (stream-set-pointer-position
                                         menu (floor (+ left right) 2) (floor (+ top bottom) 2))))
            ;; Pointer documentation usually adds no information, and slows things
            ;; down in a big way, which is why we defaultly disable it.
            (let ((*pointer-documentation-output* pointer-documentation))
              (with-input-context (presentation-type :override t)
                (object type gesture)
                (labels ((input-wait-test (menu)
                           ;; Wake up if the menu becomes buried, or if highlighting
                           ;; is needed.
                           ;;--- This screws up in Allegro because querying the server
                           ;;--- in the wait function screws event handling.

                           ;; on nt there is no server round trip so
                           ;; it's safe to check window-visibility
                           ;; (cim 9/16/96)
                           (or (and *abort-menus-when-buried*
                                    (not (window-visibility menu)))
                               (pointer-motion-pending menu)))
                         (input-wait-handler (menu)
                           ;; Abort if the menu becomes buried
                           (when (and *abort-menus-when-buried*
                                      (not (window-visibility menu)))
                             (return-from menu-choose-from-drawer nil))
                           ;; Take care of highlighting
                           (highlight-presentation-of-context-type menu)))
                  (declare (dynamic-extent #'input-wait-test #'input-wait-handler))
                  ;; Await exposure before going any further, since X can get
                  ;; to the call to READ-GESTURE before the menu is visible.
                  (when *abort-menus-when-buried*
                    #-silica (wait-for-window-exposed menu))
                  (with-mouse-grabbed-in-window (menu)
                    (loop
                      (read-gesture :stream menu
                                    :input-wait-test #'input-wait-test
                                    :input-wait-handler #'input-wait-handler)
		      ;; Silly user typed a character at a window.
		      ;; Admonish now.
		      #+ignore
                      (beep menu))))
                (t (values object gesture)))))
        (unless leave-menu-visible
          (setf (window-visibility menu) nil))
        (force-output menu)))))

(defun hierarchical-menu-choose (items
                                 &key (associated-window
                                        (frame-top-level-sheet *application-frame*))
                                      text-style default-item
                                      label (scroll-bars t) printer presentation-type
                                      x-position y-position
                                      (cache nil)
                                      (unique-id items) (id-test #'equal)
                                      (cache-value items) (cache-test #'equal)
                                      max-width max-height n-rows n-columns
                                      x-spacing y-spacing (row-wise nil)
                                      (cell-align-x ':left) (cell-align-y ':top)
                                      menu-type)
  (declare (values value chosen-item gesture))
  (flet ((present-item (item stream)
           (present item presentation-type :stream stream)))
    (declare (dynamic-extent #'present-item))
    (let ((item-printer (cond (presentation-type #'present-item)
                              (printer printer)
                              (t #'print-menu-item))))
      (with-menu (menu associated-window :label label :scroll-bars scroll-bars)
        (reset-frame (pane-frame menu) :title label)
        (with-text-style (menu text-style)
          (with-end-of-line-action (menu :allow)
            (multiple-value-bind (item gesture)
                (flet ((menu-choose-body (stream presentation-type)
                         (draw-standard-menu stream presentation-type items default-item
                                             :item-printer item-printer
                                             :max-width max-width :max-height max-height
                                             :n-rows n-rows :n-columns n-columns
                                             :x-spacing x-spacing :y-spacing y-spacing
                                             :row-wise row-wise
                                             :cell-align-x cell-align-x
                                             :cell-align-y cell-align-y)))
                  (declare (dynamic-extent #'menu-choose-body))
                  (menu-choose-from-drawer
                    menu 'menu-item #'menu-choose-body
                    :x-position x-position :y-position y-position
                    :leave-menu-visible t
                    :cache cache
                    :unique-id unique-id :id-test id-test
                    :cache-value cache-value :cache-test cache-test
                    :menu-type menu-type))
              (cond ((menu-item-items item)
                     (with-bounding-rectangle* (ml mt mr mb) menu
                       (declare (ignore ml mb))
                       ;;--- How to pass on LABEL, PRINTER, and PRESENTATION-TYPE?
                       (hierarchical-menu-choose
                         (menu-item-items item)
                         :associated-window associated-window
                         :text-style text-style
                         :x-position mr :y-position mt
                         :cache cache
                         :unique-id unique-id :id-test id-test
                         :cache-value cache-value :cache-test cache-test
                         :menu-type menu-type)))
                    (t (return-from hierarchical-menu-choose
                         (values (menu-item-value item) item gesture)))))))))))


;;; Static menus

(defmethod print-object ((static-menu static-menu) stream)
  (print-unreadable-object (static-menu stream :type t :identity t)
    (write (slot-value static-menu 'name) :stream stream :escape nil)))

(defmacro define-static-menu (name root-window items
                              &rest keys
                              &key text-style default-item
                                   printer presentation-type
                                   max-width max-height n-rows n-columns
                                   x-spacing y-spacing (row-wise nil)
                                   (cell-align-x ':left) (cell-align-y ':top))
  (declare (ignore max-width max-height n-rows n-columns
                   x-spacing y-spacing row-wise cell-align-x cell-align-y))
  (with-keywords-removed (drawer-keys keys
                          '(:text-style :default-item :printer :presentation-type))
    `(defvar ,name (define-static-menu-1 ',name ,root-window ',items
                                         :default-item ',default-item
                                         :text-style ',text-style
                                         :presentation-type ',presentation-type
                                         :printer ',printer
                                         :drawer-args ,(copy-list drawer-keys)))))

(defun define-static-menu-1 (name root-window items
                             &key text-style default-item
                                  printer presentation-type
                                  drawer-args)
  (let ((menu-contents nil)
        (default-presentation nil))
    (when (windowp root-window)
      ;; Build the static menu if we can
      (flet ((present-item (item stream)
               (present item presentation-type :stream stream)))
        (declare (dynamic-extent #'present-item))
        (let ((item-printer (cond (presentation-type #'present-item)
                                  (printer printer)
                                  (t #'print-menu-item))))
          (with-menu (menu root-window)
            (with-text-style (menu text-style)
              (with-end-of-line-action (menu :allow)
                (with-output-recording-options (menu :draw nil :record t)
                  (setq menu-contents
                        (with-new-output-record (menu)
                          (setq default-presentation
                                (apply #'draw-standard-menu
                                       menu 'menu-item items default-item
                                       :item-printer item-printer
                                       drawer-args)))))))))))
    (make-instance 'static-menu
      :name name
      :menu-contents menu-contents
      :default-presentation default-presentation
      :root-window root-window
      ;; Save this in case we have to rebuild the menu
      :items items
      :default-item default-item
      :text-style text-style
      :printer printer
      :presentation-type presentation-type
      :drawer-args drawer-args)))

;;--- How to get FRAME-MANAGER-MENU-CHOOSE into the act here?
(defmethod menu-choose ((static-menu static-menu)
                        &rest keys
                        &key (associated-window (frame-top-level-sheet *application-frame*))
                             label text-style (scroll-bars t)
                             pointer-documentation menu-type
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
        (with-slots (default-item text-style printer presentation-type drawer-args)
                    static-menu
          (let ((new-menu (define-static-menu-1 name this-root items
                                                :default-item default-item
                                                :text-style text-style
                                                :printer printer
                                                :presentation-type presentation-type
                                                :drawer-args drawer-args)))
            (setq menu-contents (slot-value new-menu 'menu-contents)
                  default-presentation (slot-value new-menu 'default-presentation)
                  root-window this-root)))))
    (with-menu (menu associated-window :label label :scroll-bars scroll-bars)
      (with-text-style (menu text-style)
        (multiple-value-bind (item gesture)
            (menu-choose-from-drawer
              menu 'menu-item #'false                ;the drawer never gets called
              :cache static-menu
              :unique-id name
              :cache-value menu-contents
              :default-presentation default-presentation
              :pointer-documentation pointer-documentation
              :menu-type menu-type)
          (cond ((menu-item-items item)
                 (with-keywords-removed (keys keys '(:default-item))
                   (apply #'menu-choose (menu-item-items item) keys)))
                (t
                 (return-from menu-choose
                   (values (menu-item-value item) item gesture)))))))))



;;; This is too useful to simply omit

(define-application-frame open-window-stream-frame ()
  ((type :initarg :type)
   (scroll-bars :initarg :scroll-bars)
   (borders :initarg :borders)
   (background :initarg :background)
   (foreground :initarg :foreground)
   (text-style :initarg :text-style)
   stream
   pane)
  (:command-definer nil)
  (:menu-bar nil)
  (:pane
   (with-slots (pane stream type scroll-bars borders background
                foreground text-style) *application-frame*
     (multiple-value-setq (pane stream)
       (make-clim-stream-pane
        :type type
        :scroll-bars scroll-bars
        :background background
        :foreground foreground
        :text-style text-style
        :borders borders))
     pane)))

(defun open-window-stream (&key left
				top
				right
				bottom
				width height
				foreground
				background
				text-style
				(vertical-spacing 2)
				(end-of-line-action :allow)
				(end-of-page-action :allow)
				output-record (draw t) (record t)
				(initial-cursor-visibility :off)
				text-margin default-text-margin
				save-under input-buffer
				(scroll-bars :vertical) borders label
				(type 'application-pane)
				parent)
  (declare (ignore save-under))
  (when (or width height)
    (assert (and (null right) (null bottom))))
  (when (null left) (setq left 0))
  (when (null top)  (setq top 0))
  (when (or (null width) (null height))
    (if (or (null right) (null bottom))
	(setq width 100
	      height 100)
	(setq width (- right left)
	      height (- bottom top))))
  (let* ((frame (make-application-frame 'open-window-stream-frame
					:resize-frame t
					:pretty-name label
					:type  type
					:scroll-bars scroll-bars
					:borders  borders
					:input-buffer input-buffer
					:background background
					:foreground foreground
					:text-style text-style
					:frame-manager
					(etypecase parent
					  ((or pane standard-application-frame)
					   (frame-manager parent))
					  (frame-manager parent)
					  (null (find-frame-manager)))
					:left left :top top))
	 (stream (slot-value frame 'stream)))
    (setf (stream-vertical-spacing stream) vertical-spacing
	  (stream-end-of-line-action stream) end-of-line-action
	  (stream-end-of-page-action stream) end-of-page-action
	  (stream-recording-p stream) record
	  (stream-drawing-p stream) draw
	  (cursor-visibility (stream-text-cursor stream)) initial-cursor-visibility
	  (stream-text-margin stream) text-margin
	  (stream-default-text-margin stream) default-text-margin)
    (when output-record
      (stream-output-history stream) (make-instance output-record))
    (window-set-inside-size stream width height)
    (window-set-viewport-position stream 0 0)

    ;; moved this into the :left, :top args to above make-application-frame.
    ;; The position-sheet-carefully doesn't seem to work because the frame
    ;;  window isn't realized at this point (cim 9/15/95)
    #+ignore
    (when (or leftp topp rightp bottomp)
      (position-sheet-carefully
       (frame-top-level-sheet (pane-frame stream)) left top))
    stream))
