;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2004 Franz Inc, Oakland, CA - All rights reserved.
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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: gadgets.lisp,v 2.8 2005/12/08 21:25:45 layer Exp $

;;;"Copyright (c) 1991, 1992 by Franz, Inc.  All rights reserved.
;;; Portions copyright (c) 1992 by Symbolics, Inc.	 All rights reserved."

(in-package :silica)


(define-protocol-class gadget (pane))

(defclass basic-gadget (sheet-with-resources-mixin gadget)
    ((id :initarg :id :reader gadget-id :initform nil)
     (client :initarg :client :initform nil :accessor gadget-client)
     (armed-callback :initarg :armed-callback :initform nil
		     :reader gadget-armed-callback)
     (disarmed-callback :initarg :disarmed-callback :initform nil
			:reader gadget-disarmed-callback)
     (active :initarg :active :accessor gadget-active-p)
     (help-callback :initform nil
		    :initarg :help-callback
		    :accessor gadget-help-callback))
  (:default-initargs :active t))

;;; Arming and disarming

(defun-inline invoke-callback-function (function &rest args)
  (declare (dynamic-extent args))
  (if (consp function)
      (apply (car function) (append args (cdr function)))
    (apply function args)))

(defmethod armed-callback ((gadget basic-gadget) (client t) (id t))
  nil)

(defmethod disarmed-callback ((gadget basic-gadget) (client t) (id t))
  nil)

(defmethod armed-callback :around ((gadget basic-gadget) (client t) (id t))
  (let ((callback (gadget-armed-callback gadget)))
    (if callback
	(invoke-callback-function callback gadget)
      (call-next-method))))

(defmethod disarmed-callback :around ((gadget basic-gadget) (client t) (id t))
  (let ((callback (gadget-disarmed-callback gadget)))
    (if callback
	(invoke-callback-function callback gadget)
      (call-next-method))))

;;; Activation and deactivation, not intended to be like callbacks

(defmethod activate-gadget ((gadget basic-gadget))
  (unless (gadget-active-p gadget)
    (setf (gadget-active-p gadget) t)
    (note-gadget-activated (gadget-client gadget) gadget)))

(defmethod note-gadget-activated ((client t) (gadget basic-gadget))
  nil)

(defmethod deactivate-gadget ((gadget basic-gadget))
  (when (gadget-active-p gadget)
    (setf (gadget-active-p gadget) nil)
    (note-gadget-deactivated (gadget-client gadget) gadget)))

(defmethod note-gadget-deactivated ((client t) (gadget basic-gadget))
  nil)


;;; Value Gadgets

(defclass value-gadget (basic-gadget)
  ((value
    :initarg :value :initform nil)	  ; no accessor defined - see below
   (value-changed-callback
    :initarg :value-changed-callback :initform nil
    :reader gadget-value-changed-callback)))

;; we explicitly define our own gadget-value accessor so that the
;; writer can include the invoke-callback keyword argument

(defgeneric gadget-value (gadget))

(defmethod gadget-value ((gadget value-gadget))
  (slot-value gadget 'value))

(defgeneric (setf gadget-value) (value gadget &key invoke-callback))

(defmethod (setf gadget-value)
    (value (gadget value-gadget) &key invoke-callback)
  (declare (ignore invoke-callback))
  (setf (slot-value gadget 'value) value))

(defmethod (setf gadget-value) :after
    (value (gadget value-gadget) &key invoke-callback)
  (when invoke-callback
    (value-changed-callback gadget
			    (gadget-client gadget) (gadget-id gadget) value)))

(defmethod value-changed-callback ((gadget value-gadget) (client t) (id t) value)
  (declare (ignore value))
  nil)

(defmethod value-changed-callback :around 
    ((gadget value-gadget) (client t) (id t) value)
  (setf (slot-value gadget 'value) value)
  (let ((callback (gadget-value-changed-callback gadget)))
    (when callback
      (invoke-callback-function callback gadget value))
    (call-next-method)))

;;; Action Gadgets

(defclass action-gadget (basic-gadget)
    ((activate-callback :initarg :activate-callback :initform nil
			:reader gadget-activate-callback)))

(defmethod activate-callback 
    ((gadget action-gadget) (client t) (id t))
  nil)

(defmethod activate-callback :around
    ((gadget action-gadget) (client t) (id t))
  (let ((callback (gadget-activate-callback gadget)))
    (when callback
      (invoke-callback-function callback gadget))
    (call-next-method)))

;;; Focus Gadgets

(defclass focus-gadget (basic-gadget)
    ((focus-out-callback :initarg :focus-out-callback :initform nil
			 :reader gadget-focus-out-callback)
     (focus-in-callback :initarg :focus-in-callback :initform nil
			:reader gadget-focus-in-callback)))

(defmethod focus-out-callback ((gadget focus-gadget) (client t) (id t))
  nil)

(defmethod focus-in-callback ((gadget focus-gadget) (client t) (id t))
  nil)

(defmethod focus-out-callback :around
    ((gadget focus-gadget) (client t) (id t)) 
  (let ((callback (gadget-focus-out-callback gadget)))
    (when callback
      (invoke-callback-function callback gadget))
    (call-next-method)))

(defmethod focus-in-callback :around
    ((gadget focus-gadget) (client t) (id t))
  (let ((callback (gadget-focus-in-callback gadget)))
    (when callback
      (invoke-callback-function callback gadget))
    (call-next-method)))

;;; Basic gadgets-mixins

(defclass oriented-gadget-mixin ()
    ((orientation :initarg :orientation
		  :type (member :horizontal :vertical)
		  :accessor gadget-orientation))
  (:default-initargs :orientation :horizontal))


(defclass labelled-gadget-mixin ()
    ((label :initarg :label
	    :accessor gadget-label)
     (alignment :initarg :align-x
		:accessor gadget-alignment))
  (:default-initargs :label "" :align-x :center))

(defmethod compute-gadget-label-size ((pane labelled-gadget-mixin))
  (let ((label (gadget-label pane)))
    (etypecase label
      (string
       (let ((text-style (slot-value pane 'text-style)))
	 ;; fixed in silica/text-style.lisp with text-style trampolines
	 ;;  31jan97 tjm w/colin
	 #+ignore(when (consp text-style)
		   (setq text-style (parse-text-style text-style)))
	 (with-sheet-medium (medium pane)
	   (multiple-value-bind (width height)
	       (text-size medium label :text-style text-style)
	     (values (+ width (text-style-width text-style medium))
		     (+ height (floor (text-style-height text-style medium) 2)))))))
      (null (values 0 0))
      (pattern
       (values (pattern-width label) (pattern-height label)))
      (pixmap
       (values (pixmap-width label) (pixmap-height label))))))

(defmethod print-object ((object labelled-gadget-mixin) stream)
  (if (and (slot-boundp object 'label)
	   (stringp (slot-value object 'label)))
      (print-unreadable-object (object stream :type t :identity t)
	(format stream "~A" (slot-value object 'label)))
      (call-next-method)))


#+(or aclpc acl86win32)
(defclass separator (oriented-gadget-mixin basic-pane) 
  () #||((sheet-parent :accessor sheet-parent :initarg :sheet-parent)
   (frame :initarg :frame)
   (frame-manager :initarg :frame-manager))||#)

#+(or aclpc acl86win32)
(defmethod compute-gadget-label-size ((pane separator))
  (values 0 0))

#+(or aclpc acl86win32)
(defmethod note-sheet-adopted ((sep separator)) nil)

#+(or aclpc acl86win32)
(defmethod compose-space ((pane separator) &key width height)
  (let ((width (or width 20))
	(height (or height 10)))
    (multiple-value-bind (w h)
	(compute-gadget-label-size pane)
      (make-space-requirement :width (max w width 20)  
			      :min-width (max w width 20)
			      :height (max h height 10) 
			      :min-height (max h height 10)))))

;; #+aclxx compose-box was here, merged back into silica/db-box.lisp
;;  31jan97 tjm w/colin

;;--- We might want a way of changing the range and the value together.
(defclass range-gadget-mixin ()
    ((min-value :initarg :min-value :accessor gadget-min-value)
     (max-value :initarg :max-value :accessor gadget-max-value))
  (:default-initargs :min-value 0.0 :max-value 1.0))

(defmethod gadget-range ((gadget range-gadget-mixin))
  (- (gadget-max-value gadget)
     (gadget-min-value gadget)))

(defmethod gadget-range* ((gadget range-gadget-mixin))
  (values (gadget-min-value gadget)
	  (gadget-max-value gadget)))


;;; The intent is that the real implementations inherit from these

(defparameter *default-slider-label-text-style*
	      (make-text-style :sans-serif :bold :small))
(defparameter *default-slider-range-label-text-style*
	      (make-text-style :sans-serif :bold :very-small))

;;; Slider
(defclass slider
	  (value-gadget oriented-gadget-mixin range-gadget-mixin labelled-gadget-mixin)
    ((drag-callback :initarg :drag-callback :initform nil
		    :reader slider-drag-callback)
     (decimal-places :initarg :decimal-places
		     :reader slider-decimal-places)
     (show-value-p :initarg :show-value-p
		   :accessor gadget-show-value-p)
     (min-label :initarg :min-label)
     (max-label :initarg :max-label)
     (range-label-text-style :initarg :range-label-text-style)
     (number-of-tick-marks :initarg :number-of-tick-marks)
     (number-of-quanta :initarg :number-of-quanta)
     (editable-p :initarg :editable-p :accessor gadget-editable-p))
    (:default-initargs :value 0.0
		       :decimal-places 0
		       :show-value-p nil
		       :min-label nil
		       :max-label nil
		       :range-label-text-style
		       #+allegro nil
		       #-allegro *default-slider-range-label-text-style*
		       :number-of-tick-marks 0
		       :number-of-quanta nil
		       :editable-p t))

(defmethod initialize-instance :after ((pane slider)
				       &key (decimal-places 0 places-p)
				       &allow-other-keys)
  (declare (ignore decimal-places))
  (unless places-p
    (let ((range (gadget-range pane)))
      (cond ((<= range 1)
	     (setf (slot-value pane 'decimal-places) 2))
	    ((<= range 10)
	     (setf (slot-value pane 'decimal-places) 1))))))

(defmethod drag-callback ((gadget slider) (client t) (id t) value)
  (declare (ignore value))
  nil)

(defmethod drag-callback :around ((gadget slider) (client t) (id t) value)
  (let ((callback (slider-drag-callback gadget)))
    (when callback
      (invoke-callback-function callback gadget value))
    (call-next-method)))

;;;
;;; Scroll bar
;;;

(defclass scroll-bar (value-gadget range-gadget-mixin oriented-gadget-mixin)
  ;; Removed initforms for SIZE and LINE-INCREMENT; these are now initialized
  ;; in an after method on INITIALIZE-INSTANCE (alemmens, 2004-12-10).
  ((size :initarg :size :accessor scroll-bar-size)
   (line-increment :initarg :line-increment :accessor scroll-bar-line-increment)
   (drag-callback :initarg :drag-callback :initform nil 
                  :reader scroll-bar-drag-callback)
   (scroll-to-bottom-callback :initform nil
                              :initarg :scroll-to-bottom-callback
                              :reader scroll-bar-scroll-to-bottom-callback)
   (scroll-to-top-callback :initform nil
                           :initarg :scroll-to-top-callback
                           :reader scroll-bar-scroll-to-top-callback)
   (scroll-down-line-callback :initform nil
                              :initarg :scroll-down-line-callback
                              :reader scroll-bar-scroll-down-line-callback)
   (scroll-up-line-callback :initform nil
                            :initarg :scroll-up-line-callback
                            :reader scroll-bar-scroll-up-line-callback)
   (scroll-down-page-callback :initform nil
                              :initarg :scroll-down-page-callback
                              :reader scroll-bar-scroll-down-page-callback)
   (scroll-up-page-callback :initform nil
                            :initarg :scroll-up-page-callback
                            :reader scroll-bar-scroll-up-page-callback)
   ))


(defmethod initialize-instance :after ((scroll-bar scroll-bar) &key value size line-increment &allow-other-keys)
  ;; The default value for SIZE should be (GADGET-RANGE scroll-bar), not 1.
  ;; The default value for VALUE should be (GADGET-MIN-VALUE scroll-bar), not NIL.
  ;; The default value for LINE-INCREMENT should be a fraction of SIZE, not 1.
  ;; (alemmens, 2004-12-10)
  (unless value
    (setf (slot-value scroll-bar 'value) (gadget-min-value scroll-bar)))
  (unless size
    (setf (slot-value scroll-bar 'size) (gadget-range scroll-bar)))
  (unless line-increment
    (setf (slot-value scroll-bar 'line-increment) (* 0.1 (slot-value scroll-bar 'size)))))

(defmethod print-object ((gadget scroll-bar) stream)
  ;; Added this method for easier debugging (alemmens, 2004-12-24).
  (print-unreadable-object (gadget stream :type t :identity t)
    (format stream "from ~A to ~A (size ~A, line ~A, value ~A)"
            (gadget-min-value gadget)
            (gadget-max-value gadget)
            (if (slot-boundp gadget 'size)
                (scroll-bar-size gadget)
                'unbound)
            (if (slot-boundp gadget 'line-increment)
                (scroll-bar-line-increment gadget)
                'unbound)
            (gadget-value gadget))))


(defmethod drag-callback :around ((gadget scroll-bar) (client t) (id t) value)
  (let ((callback (scroll-bar-drag-callback gadget)))
    (when callback
	(invoke-callback-function callback gadget value))
    (call-next-method)))

(defmethod drag-callback ((gadget scroll-bar) (client t) (id t) value)
  (declare (ignore value))
  nil)

(defmethod (setf scroll-bar-size) :around (size (gadget scroll-bar))
  (assert (and (<= (+ size (gadget-value gadget))
		   (gadget-max-value gadget))
	       (> size 0))
      (size)
    "Scroll bar size ~A out of range" size)
  (call-next-method size gadget))

(defmethod (setf gadget-value) :around
	   (value (gadget scroll-bar) &key invoke-callback)
  (declare (ignore invoke-callback))
  (assert (and (<= (+ value (scroll-bar-size gadget))
                 (gadget-max-value gadget))
            (>= value (gadget-min-value gadget)))
    (value)
    "Scroll bar value ~A out of range" value)
  (call-next-method value gadget))

(defmethod (setf line-increment) :around (value (object scroll-bar))
  (assert (and (> value (gadget-min-value object))
            (<= value (gadget-max-value object)))
    (value)
    "Scroll bar line-increment ~a out of range" value)
  (call-next-method value object))

(defmethod change-scroll-bar-values :before ((pane scroll-bar) 
                                             &key line-increment &allow-other-keys)
  ;; Record line-increment since clim doesn't.
  ;; (Only record it when it's actually given as an argument to
  ;;  change-scroll-bar-values; the win32 backend doesn't always do that.
  ;;  alemmens, 2004-12-24)
  (when line-increment
    (setf (scroll-bar-line-increment pane) line-increment)))

;;; Here are the callbacks that were once called out in the CLIM spec:

(defmethod scroll-to-bottom-callback ((pane scroll-bar) client id value)
  (value-changed-callback pane client id value)
  (let ((callback (scroll-bar-scroll-to-bottom-callback pane)))
    (when callback (funcall callback pane))))

(defmethod scroll-to-top-callback ((pane scroll-bar) client id value)
  (value-changed-callback pane client id value)
  (let ((callback (scroll-bar-scroll-to-top-callback pane)))
    (when callback (funcall callback pane))))

(defmethod scroll-up-line-callback ((pane scroll-bar) client id value)
  (value-changed-callback pane client id value)
  (let ((callback (scroll-bar-scroll-up-line-callback pane)))
    (when callback (funcall callback pane))))

(defmethod scroll-down-line-callback ((pane scroll-bar) client id value)
  (value-changed-callback pane client id value)
  (let ((callback (scroll-bar-scroll-down-line-callback pane)))
    (when callback (funcall callback pane))))

(defmethod scroll-up-page-callback ((pane scroll-bar) client id value)
  (value-changed-callback pane client id value)
  (let ((callback (scroll-bar-scroll-up-page-callback pane)))
    (when callback (funcall callback pane))))

(defmethod scroll-down-page-callback ((pane scroll-bar) client id value)
  (value-changed-callback pane client id value)
  (let ((callback (scroll-bar-scroll-down-page-callback pane)))
    (when callback (funcall callback pane))))

;;;
;;; Some scroll bar utilities
;;;

;; (These came from xt-gadgets.lisp. alemmens, 2004-12-24)

(defun convert-scroll-bar-value-out (scroll-bar value)
  "Convert a scroll bar value from CLIM to native."
  (multiple-value-bind
        (smin smax) (gadget-range* scroll-bar)
    (multiple-value-bind (native-min native-max)
        (native-gadget-range* scroll-bar)
      (max native-min
           (min native-max
                (floor
                 (compute-symmetric-value smin smax value native-min native-max)))))))

(defun convert-scroll-bar-value-in (scroll-bar value)
  "Convert a scroll bar value from native to CLIM."
  (multiple-value-bind
        (smin smax) (gadget-range* scroll-bar)
    (multiple-value-bind (native-min native-max)
        (native-gadget-range* scroll-bar)
      (max smin
           (min smax
                (compute-symmetric-value native-min native-max value smin smax))))))

;; Converting scroll-bar sizes (e.g. slug size or line increment) went
;; wrong when the scroll-bar's min-value was non-zero.  So I added the
;; following two little helper functions (alemmens, 2004-12-10).

(defun convert-scroll-bar-size-out (scroll-bar size)
  "Convert a scroll bar size from CLIM to native."
  (convert-scroll-bar-value-out scroll-bar
                                (+ (gadget-min-value scroll-bar) size)))

(defun convert-scroll-bar-size-in (scroll-bar native-size)
  "Convert a scroll bar size from native to CLIM."
  (- (convert-scroll-bar-value-in scroll-bar native-size)
     (gadget-min-value scroll-bar)))
    

;;;
;;; Push-button
;;;

(defclass push-button 
	  (action-gadget labelled-gadget-mixin) 
    ((show-as-default :initform nil :initarg :show-as-default
		      :accessor push-button-show-as-default)
     #+(or aclpc acl86win32)
     (pattern :initarg :pattern)
     #+(or aclpc acl86win32)
     (icon-pattern :initarg :icon-pattern)))


;;; Toggle button
(defvar +hbutton-pane-default-button-label-justify+ nil)
(defclass toggle-button 
	  (value-gadget labelled-gadget-mixin)
    ((indicator-type :initarg :indicator-type :initform :some-of
		     :type (member :some-of :one-of)
		     :reader gadget-indicator-type)
   ;; bug12221/SPR24998 -pnc
   ;; On Unix the default is:    <label> [X]
   ;; On Windows the default is: [X] <label>
   ;; If button-label-justify has a value of :left, the order is like on Unix.
   ;; If button-label-justify is nil, just do the platform-specific default.
   ;; NOTE:  This option is currently supported only on Windows.
   ;; (see hpbutton-pane in aclpc/acl-widget.lisp)
   (button-label-justify :initarg :button-label-justify 
			 :initform +hbutton-pane-default-button-label-justify+
			 :reader gadget-button-label-justify)
     
     ))


;;; Menu button
(defclass menu-button
	  (value-gadget labelled-gadget-mixin)
    ((indicator-type :initarg :indicator-type :initform :some-of
		     :reader gadget-indicator-type)))



;;; Menu bar
(defclass menu-bar (pane
		    row-column-gadget-mixin
		    sheet-with-resources-mixin)
    ((command-table :initarg :command-table :initform nil
		    :accessor menu-bar-command-table)))


;;; Cascade button
;;; Caption
;;; Option menu

(defclass row-column-gadget-mixin (oriented-gadget-mixin)
	  ((rows :initarg :rows :initform nil :accessor gadget-rows)
	   (columns :initarg :columns :initform nil :accessor gadget-columns)
	   (spacing :initarg :spacing :initform nil :accessor gadget-spacing)))

;;; Radio box [exclusive-choice] .. [inclusive-choice]
(defclass radio-box
    (value-gadget row-column-gadget-mixin
     #+(or aclpc acl86win32) labelled-gadget-mixin) 
    ((selections :initform nil
		 :reader radio-box-selections)
     ;;--- think about this...
     (value :initform nil
	    :initarg :current-selection
	    :accessor radio-box-current-selection)))

(defmethod (setf gadget-value) :after
    (new-value (gadget radio-box) &key invoke-callback)
  #-aclpc (declare (ignore invoke-callback))
  (unless (eq new-value (radio-box-current-selection gadget))
    (dolist (toggle (radio-box-selections gadget))
      (setf (gadget-value toggle)
	(eq new-value toggle)))))

(defmethod initialize-instance :after ((rb radio-box) &key choices)
  (let* ((frame (pane-frame rb))
	 (framem (frame-manager frame))
	 (selections nil)
	 (current-selection (radio-box-current-selection rb)))
    (assert (and frame framem) ()
      "There must be both a frame and frame manager active")
    (when current-selection
      (assert (member current-selection choices :test #'equal)
	  ()
	"Radio box current-selection: ~S must be one of choices: ~S"
	current-selection choices))
    (with-look-and-feel-realization (framem frame)
      (dolist (choice choices)
	(etypecase choice
	  (pane
	   ;; Adopt the button.	 Some framems will disown the button
	   ;; in order to do layout, but that will happen later.
	   (unless (sheet-parent choice)
	     (sheet-adopt-child rb choice))
	   (push choice selections)
	   (setf (gadget-value choice) (eq choice current-selection)))
	  (string
	   ;; Create a button if the user supplied an abbreviation
	   (let* ((value (equal current-selection choice))
		  (button (make-pane 'toggle-button
				     :value value
				     :label choice
				     :indicator-type :one-of
				     :client rb
				     :id choice
				     :parent rb)))
	     (push button selections)
	     (when value
	       (setf (radio-box-current-selection rb) button)))))))
    (setf (slot-value rb 'selections) (nreverse selections))))

(defmethod value-changed-callback :around 
	   ((selection basic-gadget) (client radio-box) gadget-id value)
  #-aclpc (declare (ignore gadget-id))
  ;;--- The following comment is wrong.	 Perhaps these should be :BEFORE
  ;;--- methods since the user could define a more specific around method only.
  ;; This and the one below have to be :AROUND because if the user has
  ;; specified a callback function only :AROUNDs ever get executed.
  (when (eq value t)
    (setf (radio-box-current-selection client) selection)
    (value-changed-callback
     client (gadget-client client) (gadget-id client) selection))
  (call-next-method))


;;; Check-box

(defclass check-box
    (value-gadget row-column-gadget-mixin)
    ((selections :initform nil
		 :reader check-box-selections)
     ;;--- think about this...
     (value :initform nil
	    :initarg :current-selection
	    :accessor check-box-current-selection)))

(defmethod (setf gadget-value) :after
    (new-value (gadget check-box) &key invoke-callback)
  #-aclpc (declare (ignore invoke-callback))
  (unless (equal new-value (check-box-current-selection gadget))
    (dolist (toggle (check-box-selections gadget))
      (setf (gadget-value toggle)
	(member toggle new-value)))))

(defmethod initialize-instance :after ((cb check-box) &key choices #+(or aclpc acl86win32) current-selection)
  (let* ((frame (pane-frame cb))
	 (framem (frame-manager frame))
	 (selections nil)
	 #-(or aclpc acl86win32)
	 (current-selection (check-box-current-selection cb)))
    (assert (and frame framem) ()
      "There must be both a frame and frame manager active")
    (assert (every #-(or aclpc acl86win32)
		   #'(lambda (x) (member x choices :test #'equal))
		   #+(or aclpc acl86win32)
		   #'(lambda (x)
		       (let ((y (if (typep x 'gadget) (gadget-id x) x)))
			 (or (member x choices :test #'equal)
			     (member y choices :test #'equal))
			 ))
		   current-selection)
	()
      "Each element of check box current-selection: ~S must be one of choices: ~S"
      current-selection choices)
    (with-look-and-feel-realization (framem frame)
      (dolist (choice choices)
	(etypecase choice
	  (toggle-button
	   ;; Adopt the button.	 Some framems will disown the button
	   ;; in order to do layout, but that will happen later.
	   (push choice selections)
	   (unless (sheet-parent choice)
	     (sheet-adopt-child cb choice))
	   (setf (gadget-value choice) (member choice current-selection)))
	  (string
	   ;; Create a button if the user supplied an abbreviation
	   (let* ((value (member choice current-selection :test #'equal))
		  (button (make-pane 'toggle-button
				     :value (and value t)
				     :label choice
				     :indicator-type :some-of
				     :client cb
				     :id choice
				     :parent cb)))
	     (push button selections)
	     (when value
	       (setf (car value) button)))))))
    (setf (slot-value cb 'selections) (nreverse selections))))

(defmethod value-changed-callback :around 
	   ((selection basic-gadget) (client check-box) gadget-id value)
  #-aclpc (declare (ignore gadget-id))
  (if (eq value t)
      (pushnew selection (check-box-current-selection client))
    (setf (check-box-current-selection client)
      (delete selection (check-box-current-selection client))))
  (value-changed-callback
   client (gadget-client client) (gadget-id client) (check-box-current-selection client))
  (call-next-method))


;; This macro is just an example of one possible syntax.  The obvious "core"
;; syntax is (MAKE-PANE 'RADIO-BOX :CHOICES (LIST ...) :SELECTION ...)
(defmacro with-radio-box ((&rest options &key (type ':one-of) &allow-other-keys)
			  &body body)
  (setq options (remove-keywords options '(:type)))
  (let ((current-selection (gensym))
	(choices (gensym)))
    (ecase type
      (:one-of
	`(let ((,current-selection nil))
	   (macrolet ((radio-box-current-selection (form)
			`(setq ,',current-selection ,form)))
	     (let ((,choices (list ,@body)))
	       (make-pane 'radio-box
		 :choices ,choices
		 :current-selection ,current-selection
		 ,@options)))))
      (:some-of
	`(let ((,current-selection nil))
	   (macrolet ((radio-box-current-selection (form)
			(let ((button (gensym)))
			  `(let ((,button ,form))
			     (setq ,',current-selection
			       (append ,',current-selection (list ,button)))
			     ,button)))
		      (check-box-current-selection (form)
			(let ((button (gensym)))
			  `(let ((,button ,form))
			     (setq ,',current-selection
			       (append ,',current-selection (list ,button)))
			     ,button))))
	     (let ((,choices (list ,@body)))
	       (make-pane 'check-box
		 :choices ,choices
		 :current-selection ,current-selection
		 ,@options))))))))


;;; Text edit
;;--- Do we want to specify a binding to commands?

(defclass text-field
	  (value-gadget focus-gadget action-gadget)
  ((editable-p :initarg :editable-p :accessor gadget-editable-p)
   (echo-character :initarg :echo-character :initform nil
		   :reader text-field-echo-character))
  (:default-initargs :editable-p t))

(defclass text-editor (text-field)
    ((ncolumns :initarg :ncolumns
	       :accessor gadget-columns)
     (nlines :initarg :nlines
	     :accessor gadget-lines)
     (word-wrap :initarg :word-wrap
		:accessor gadget-word-wrap))
  (:default-initargs :ncolumns 30 :nlines 1 :editable-p t :word-wrap nil))

(defmethod gadget-current-selection ((text-field text-field))
  nil)

(defgeneric text-field-cursor (text-field)
  ;; spr30683 (alemmens, 2005-11-30)
  (:documentation "Returns the current position of the text-field cursor."))

(defgeneric (setf text-field-cursor) (cursor text-field)
  ;; spr30683 (alemmens, 2005-11-30)
  (:documentation "Changes the position of the text-field cursor.
The text-field must have the focus before you can call this function."))

;;;
;;; Viewport
;;;

;;--- CLIM 0.9 has this VIEWPORT-LAYOUT-MIXIN -- do we need it?
(defclass viewport
	  (sheet-single-child-mixin
	   sheet-permanently-enabled-mixin
	   wrapping-space-mixin
	   sheet-with-resources-mixin
	   basic-pane)
    ;; This describes the region that we are displaying
    ((viewport-region :accessor viewport-viewport-region)
     (scroller-pane :initarg :scroller-pane :reader viewport-scroller-pane)))

(defmethod initialize-instance :after ((viewport viewport) &key)
  (multiple-value-bind (width height)
      (bounding-rectangle-size viewport)
    (setf (viewport-viewport-region viewport)
	  (make-bounding-rectangle 0 0 width height))))

(defmethod viewportp ((x t)) nil)
(defmethod viewportp ((x viewport)) t)

(defmethod compose-space ((viewport viewport) &key width height)
  (declare (ignore width height))
  (let ((sr (call-next-method)))
    (multiple-value-bind (w min-width max-width
			  h min-height max-height)
	(space-requirement-components sr)
      (declare (ignore min-width min-height))
      (make-space-requirement
	:width w :min-width 0 :max-width max-width
	:height h :min-height 0 :max-height max-height))))

;;;#+(or aclpc acl86win32)
;;;(eval-when (compile load eval)
;;;   ;;mm: 11Jan95 - this is defined later in clim\db-strea
;;;   (unless (ignore-errors (find-class 'clim-stream-pane))
;;;      (defclass clim-stream-pane () ())))

(defmethod allocate-space ((viewport viewport) width height)
  ;; Make sure the child is at least as big as the viewport
  ;; (VIEWPORT-REGION-CHANGED actually does this also)
  (let* ((child (sheet-child viewport)))
    (if (typep child 'clim-stream-pane)
	(multiple-value-bind (cwidth cheight)
	    (bounding-rectangle-size child)
	  (when (or (< cwidth width)
		    (< cheight height))
	    (resize-sheet
	     child (max width cwidth) (max height cheight))))
      (progn
	(let* ((sr (compose-space child :width width :height height))
	       (max-width (space-requirement-max-width sr))
	       (max-height (space-requirement-max-height sr))
	       (desired-width (space-requirement-width sr))
	       (desired-height (space-requirement-height sr)))
	  (resize-sheet child
			(if (>= max-width +fill+)
			    (max width desired-width)
			  (max desired-width max-width))
			(if (>= max-height +fill+)
			    (max height desired-height)
			  (max desired-height max-height))))))))

(defmethod allocate-space :after ((viewport viewport) width height)
  (let ((vr (viewport-viewport-region viewport)))
    (multiple-value-bind (owidth oheight) (bounding-rectangle-size vr)
      (bounding-rectangle-set-size vr width height)
      ;; If previously it was too small to display the entire contents
      ;; but now it is large enough, scroll the window
      (multiple-value-bind (ox oy) (bounding-rectangle-position vr)
	(with-bounding-rectangle* (cleft ctop cright cbottom)
	    (viewport-contents-extent viewport)
	  (let ((cw (- cright cleft))
		(ch (- cbottom ctop)))
	    (let ((x ox) (y oy))
	      (when (and (< owidth cw) (<= cw width))
		(setq x cleft))
	      (when (and (< oheight ch) (<= ch height))
		(setq y ctop))
	      (if (or (/= x ox) (/= y oy))
		  (scroll-extent (sheet-child viewport) x y)
		  (progn
		    (update-scroll-bars viewport)
		    (viewport-region-changed (sheet-child viewport) viewport))))))))))

;;-- This method seems applicable to a whole bunch of panes.
;;-- The problem we have is that a whole bunch of panes dont get the
;;-- right background so it looks crap.

;; reinstate this for the windows port - this is important because
;; unlike on the xt port, on the windows port viewports are *not*
;; mirrored. (cim 9/27/96)

;; simplified so that it repaints the viewport over the entire region
;; taking advantage of the fact that the repainting of overlapping
;; children happens after
#+ignore
(defmethod repaint-sheet ((sheet viewport) region)
  (let ((region (region-intersection (sheet-region sheet) region)))
    (dolist (child (sheet-children sheet))
      (setq region (region-difference region (transform-region (sheet-transformation child) (sheet-region child)))))
    (with-sheet-medium (medium sheet)
      (with-drawing-options (medium  :ink +background-ink+)
	(dolist (rect (region-set-regions region))
	  (unless (eq +nowhere+ rect)
	    (with-bounding-rectangle* (left top right bottom) rect
	      (draw-rectangle* medium left top right bottom))))))))

(defmethod note-space-requirements-changed ((pane viewport) inner)
  (declare (ignore inner))
  (allocate-space
    pane
    (bounding-rectangle-width pane) (bounding-rectangle-height pane)))

;;--- Work on this
#+++ignore
(defmethod note-sheet-region-changed :around ((viewport viewport) &key port-did-it)
  #-aclpc (declare (ignore port-did-it))
  (multiple-value-bind (changedp
			hscroll-bar hscroll-bar-enabled-p
			vscroll-bar vscroll-bar-enabled-p)
      (compute-dynamic-scroll-bar-values viewport)
    (if changedp
	 (update-dynamic-scroll-bars
	  viewport changedp
	  hscroll-bar hscroll-bar-enabled-p
	  vscroll-bar vscroll-bar-enabled-p)
	(call-next-method))))


;(defun update-dynamic-scroll-bars (scroller changedp
;				    hscroll-bar hscroll-bar-enabled-p
;				    vscroll-bar vscroll-bar-enabled-p
;				    &optional relayout)
;  (when changedp
;    (when hscroll-bar
;      (setf (sheet-enabled-p hscroll-bar) hscroll-bar-enabled-p))
;    (when vscroll-bar
;      (setf (sheet-enabled-p vscroll-bar) vscroll-bar-enabled-p))
;    (when (or (and hscroll-bar (not hscroll-bar-enabled-p))
;		(and vscroll-bar (not vscroll-bar-enabled-p)))
;      (let* ((contents (slot-value scroller 'contents))
;	       (c-extent (viewport-contents-extent
;			  (pane-viewport contents))))
;	  (multiple-value-bind (vx vy)
;	     (window-viewport-position contents)
;	    (window-set-viewport-position
;	     contents
;	     (if (and hscroll-bar (not hscroll-bar-enabled-p))
;		 (bounding-rectangle-min-x c-extent)
;		 vx)
;	     (if (and vscroll-bar (not vscroll-bar-enabled-p))
;		 (bounding-rectangle-min-y c-extent)
;		 vy)))))
;    (clear-space-requirement-caches-in-tree scroller)
;    (when relayout
;      ;;--- This is kinda bogus.  If this was a generic scroller then
;      ;;--- you want to layout the table.
;      (let ((table (slot-value scroller 'viewport)))
;	  (multiple-value-bind (width height)
;	      (bounding-rectangle-size table)
;	    (allocate-space table width height))))))

;(defun compute-dynamic-scroll-bar-values (scroller)
;  (let* ((hscroll-bar (scroller-pane-horizontal-scroll-bar scroller))
;	  (vscroll-bar (scroller-pane-vertical-scroll-bar scroller))
;	  (scroll-bar-policy (scroller-pane-scroll-bar-policy scroller)))
;    (if (eq scroll-bar-policy :dynamic)
;	 (multiple-value-bind (vwidth vheight)
;	     (bounding-rectangle-size scroller)
;	   (multiple-value-bind (cwidth cheight)
;	       (bounding-rectangle-size
;		 (viewport-contents-extent (slot-value scroller 'viewport)))
;	     (let ((ohenp (sheet-enabled-p hscroll-bar))
;		   (ovenp (sheet-enabled-p vscroll-bar))
;		   (nhenp (> cwidth vwidth))
;		   (nvenp (> cheight vheight)))
;	       (values
;		 (not (and (eq ohenp nhenp)
;			   (eq ovenp nvenp)))
;		 (and (not (eq ohenp nhenp)) hscroll-bar)
;		 nhenp
;		 (and (not (eq ovenp nvenp)) vscroll-bar)
;		 nvenp))))
;	 (values nil nil nil nil nil))))

(defun viewport-contents-extent (viewport)
  (let ((contents (sheet-child viewport)))
    (or (and (output-recording-stream-p contents)
	     (stream-output-history contents))
	contents)))


(defmacro scrolling (options &body contents)
  (assert (null (cdr contents)) (contents)
	  "The ~S layout macro can have only a single pane as its contents"
	  'scrolling)
  `(make-pane 'scroller-pane
     :contents ,@contents
     ,@options))


;;; List panes and option menus

(defclass set-gadget-mixin ()
    ((items :initarg :items :accessor set-gadget-items)
     (name-key :initarg :name-key :accessor set-gadget-name-key)
     (value-key :initarg :value-key :accessor set-gadget-value-key)
     (test :initarg :test :accessor set-gadget-test))
  (:default-initargs :items nil
		     :test #'eql
		     :value-key #'identity
		     :name-key #'princ-to-string))


(defclass list-pane (set-gadget-mixin value-gadget)
    ((mode :initarg :mode :type (member :exclusive :nonexclusive)
	   :accessor list-pane-mode)
     (visible-items :initarg :visible-items :reader gadget-visible-items)
     ;; spr 30639: top item position is necessary for redisplaying list-panes.
     ;; (alemmens, 2005-11-30)
     (top-item-position :initarg :top-item-position
                        :accessor list-pane-top-item-position))
  (:default-initargs
    :mode :exclusive
    :visible-items nil
    :top-item-position nil))

#-(or aclpc acl86win32)
(defun compute-list-pane-selected-items (sheet value)
  (with-accessors ((value-key set-gadget-value-key)
		   (test set-gadget-test)
		   (mode list-pane-mode)
		   (name-key set-gadget-name-key)) sheet
    (let* ((items (set-gadget-items sheet))
	   (total-items (length items))
	   (visible-items (or (gadget-visible-items sheet)
			      ;; we use +fill+ as the default here so
			      ;; that we don't try and do any
			      ;; scrolling if we don't know how many
			      ;; items are visible ie we assume the
			      ;; list-pane is infinite (cim 6/24/96)
			      +fill+)))
      (multiple-value-bind (selected-items m n)
	  (ecase mode
	    (:exclusive
	     (let ((i (position value items :test test :key value-key)))
	       (and i
		    (values (list (elt items i)) i i))))
	    (:nonexclusive
	     (let (selected-items m n)
	       (dotimes (i (length items))
		 (let ((item (elt items i)))
		   (when (member (funcall value-key item) value :test test)
		     (push item selected-items)
		     (setq m (or m i) n i))))
	       (when selected-items
		 (values (nreverse selected-items) m n)))))
	(when selected-items
	  (values (mapcar name-key selected-items)
		  (max 0 (- n (1- visible-items)))
		  (min m (max 0 (- total-items visible-items)))))))))

#+(or aclpc acl86win32)
(defun compute-list-pane-selected-items (sheet value)
  (with-accessors ((items set-gadget-items)
		   (value-key set-gadget-value-key)
		   (test set-gadget-test)
		   (mode list-pane-mode)
		   (name-key set-gadget-name-key)) sheet
    (ecase mode
      (:exclusive
	(let ((x (find value items :test test :key value-key)))
	  (and x (list (funcall name-key x)))))
      (:nonexclusive
	(mapcar name-key
		(remove-if-not #'(lambda (item)
				   (member (funcall value-key item) value :test test))
			       items))))))

(defun list-pane-selected-item-p (sheet item)
  (with-accessors ((items set-gadget-items)
		   (value gadget-value)
		   (value-key set-gadget-value-key)
		   (test set-gadget-test)
		   (mode list-pane-mode)
		   (name-key set-gadget-name-key)) sheet
    (ecase mode
      (:exclusive
	(funcall test (funcall value-key item) value))
      (:nonexclusive
	(member (funcall value-key item) value :test test)))))


(defclass option-pane (set-gadget-mixin
		       labelled-gadget-mixin
		       value-gadget)
    ;;--- Should this be :ONE-OF/:SOME-OF, as radio boxes are?
    ((mode :initarg :mode :type (member :exclusive :nonexclusive)
	   :accessor option-pane-mode)
     (printer :initarg :printer :reader option-pane-printer))
  (:default-initargs :mode :exclusive :printer nil))


;; Callbacks on widgets generate these events

(defclass gadget-event (event)
    ((gadget :initarg :gadget :reader event-sheet)))

(defclass value-changed-gadget-event (gadget-event)
    ((value :initarg :value :reader event-value)))

(defmethod handle-event ((gadget value-gadget) (event value-changed-gadget-event))
  (value-changed-callback
    gadget (gadget-client gadget) (gadget-id gadget) (slot-value event 'value)))


(defclass activate-gadget-event (gadget-event) ())

(defmethod handle-event ((gadget action-gadget) (event activate-gadget-event))
  (activate-callback gadget (gadget-client gadget) (gadget-id gadget)))


(defclass drag-gadget-event (gadget-event)
    ((value :initarg :value :reader event-value)))

(defmethod handle-event ((gadget value-gadget) (event drag-gadget-event))
  (drag-callback
   gadget (gadget-client gadget) (gadget-id gadget) (slot-value event 'value)))


(defclass focus-out-gadget-event (gadget-event) ())

(defmethod handle-event ((gadget focus-gadget) (event focus-out-gadget-event))
  (focus-out-callback gadget (gadget-client gadget) (gadget-id gadget)))

(defclass focus-in-gadget-event (gadget-event) ())

(defmethod handle-event ((gadget focus-gadget) (event focus-in-gadget-event))
  (focus-in-callback gadget (gadget-client gadget) (gadget-id gadget)))

;; armed/disarmed events

(defclass armed-gadget-event (gadget-event) ())

(defmethod handle-event ((gadget basic-gadget) (event armed-gadget-event))
  (armed-callback gadget (gadget-client gadget) (gadget-id gadget)))

(defclass disarmed-gadget-event (gadget-event) ())

(defmethod handle-event ((gadget basic-gadget) (event disarmed-gadget-event))
  (disarmed-callback gadget (gadget-client gadget) (gadget-id gadget)))


(defun move-focus-to-gadget (gadget)
  (port-move-focus-to-gadget (port gadget) gadget))

(defmethod port-move-focus-to-gadget ((port t) (gadget t))
  nil)
