(in-package :silica)
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

(defclass gadget ()
	  ((id :initarg :id :reader gadget-id :initform nil)
	   (client :initarg :client :initform nil :accessor gadget-client)))

(defclass value-gadget (gadget) ())

(defmethod value-changed-callback ((v value-gadget) (client t) (id t) (value t))
  ())

(defgeneric gadget-value (gadget))

(defgeneric (setf gadget-value) (gadget))

(defclass action-gadget (gadget) ())

(defmethod activate-callback ((v action-gadget) (client t) (id t))
  ())


;; slider
;; push-button
;; toggle-button
;; scroll-bar
;; Caption
;; option menu
;; label

;; radio-box [exclusive-choice]
;; .. [inclusive-choice]

;; menu-bar


;; [cascade]

;; As well as the callback mechanism
;; a we want to specify a binding to commands
;; text-field
;; text-edit

;;; Then there is the layout stuff and scrolling macros


(defmacro horizontally () ())
(defmacro tabling () ())
(defmacro form () ())

(defmacro scrolling (options contents)
  `(realize-pane 'scroller-pane
		 :contents ,contents
		 ,@options))

(defmethod realize-pane-internal ((framem frame-manager) (frame application-frame)
				  name &rest options)
  (apply #'make-instance name options))

(defmethod note-sheet-region-changed :after ((sheet gadget) &key port)
  (declare (ignore port))
  (allocate-space sheet
		  (sheet-width sheet) 
		  (sheet-height sheet)))

;;;;;;;; Whats the interface to different types of window: main, dialog,..

(defclass vertical-pane (sheet
			 sheet-multiple-child-mixin
			 sheet-transformation-mixin
			  
			 standard-repainting-medium
			 standard-sheet-input-mixin
			  
			 permanent-medium-sheet-output-mixin
			 mute-repainting-mixin)
	  ())

(defmethod initialize-instance :after ((x vertical-pane) &key contents)
  (dolist (y contents)
    (adopt-child x y)))


(defmethod compose-space ((sheet vertical-pane))
  (let ((x (make-instance 'space-req :width 0 :height 0)))
    (dolist (child (sheet-children sheet))
      (let ((s (compose-space child)))
	(macrolet ((foo (setf accessor)
			`(,setf (,accessor x) (,accessor s))))
		  (foo incf space-req-height)
		  (foo incf space-req-min-height)
		  (foo incf space-req-max-height)
		  
		  (foo maxf space-req-width)
		  (foo maxf space-req-min-width)
		  (foo maxf space-req-max-width))))
    x))

(defmethod allocate-space ((sheet vertical-pane) width height)
  (let ((av (truncate height (length (sheet-children sheet))))
	(al 0))
    (dolist (child (sheet-children sheet))
          (format t "al = ~D~%" al)
      (setf (sheet-transformation child)
	(make-translation-transformation 0 al))
      
      (setf (sheet-region child)
	    (make-bounding-rectangle 0 0 width av))
      
      (incf al av))))

;; Callbacks on widgets generate these events

(defclass gadget-event (event) 
	  ((gadget :initarg :gadget :reader event-sheet)))

(defclass value-changed-gadget-event (gadget-event) 
  ((value :initarg :value :reader event-value)))

(defmethod handle-event ((gadget value-gadget) (event value-changed-gadget-event))
  (value-changed-callback gadget (gadget-client gadget) (gadget-id gadget) (slot-value event 'value)))

(defclass activate-gadget-event (gadget-event) ())

(defmethod handle-event ((gadget  action-gadget) (event activate-gadget-event))
  (activate-callback gadget (gadget-client gadget) (gadget-id gadget)))

