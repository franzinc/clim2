;; -*- mode: common-lisp; package: tk -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :tk)

(defun has-callbacks-p (w name)
  (> (xt_has_callbacks w name) 1))

(defun-foreign-callable callback-handler ((widget :foreign-address)
					  (client-data :foreign-address)
					  (call-data :foreign-address))
  (callback-handler-1 widget client-data call-data))

(defun callback-handler-1 (address client-data call-data)
  (let* ((widget (find-object-from-address address))
	 (callback-info (or (assoc client-data (widget-callback-data widget))
			    (error "Cannot find callback info ~S,~S"
				   widget client-data))))
    (destructuring-bind
	(ignore (fn &rest args) &optional type)
	callback-info
      (declare (ignore ignore))
      (apply fn
	   widget
	   (append (multiple-value-list (spread-callback-data widget call-data type))
		   args)))
    0))

(defmethod spread-callback-data (widget call-data (type (eql nil)))
  (declare (ignore widget call-data))
  (values))

(defvar *callback-handler-address* nil)


(defun add-callback (widget callback-name function &rest args)
  (multiple-value-bind
      (name type)
      (convert-callback-name callback-name)
    (xt_add_callback
     widget
     name
     (or *callback-handler-address*
	 (setq *callback-handler-address* (register-foreign-callable 'callback-handler)))
     (caar (push
	    (list (new-callback-id) (cons function args) type)
	    (widget-callback-data widget))))))


(defun-foreign-callable create-popup-child-proc-function ((widget :foreign-address))
  (create-popup-child-proc-function-1 widget))

(defun create-popup-child-proc-function-1 (widget)
  (let* ((widget (find-object-from-address widget))
	 (function (or (cdr (assoc :create-popup-child-proc (widget-callback-data widget)))
		       (error "cannot find create-popup-childp-proc ~S" widget))))
    (funcall function widget)))


(defvar *create-popup-child-proc-function-address* nil)

(defun (setf widget-create-popup-child-proc) (function widget)
  (push
   (cons :create-popup-child-proc function)
   (widget-callback-data widget))
  (set-values widget :create-popup-child-proc
	      (or *create-popup-child-proc-function-address*
		  (setq *create-popup-child-proc-function-address*
		    (ff:register-foreign-callable 'create-popup-child-proc-function))))
  function)

(defun remove-all-callbacks (widget callback-name)
  (xt_remove_all_callbacks widget (convert-callback-name callback-name)))

(defvar *callback-ids* 0)
(defun new-callback-id ()
  (incf *callback-ids*))

(defun process-callback-alist-component (x)
  (destructuring-bind (name &optional type) x
    (list (lispify-tk-name name :package :keyword)
	  name
	  nil;; malloc cache
	  type)))

(defparameter *callback-name-alist*
    (mapcar #'process-callback-alist-component
	    '(
	    ("activateCallback" :activate)
	    ("armCallback")
	    ("disarmCallback")
	    ("popupCallback")
	    ("popdownCallback")
	    ("helpCallback")
	    ("decrementCallback")
	    ("dragCallback")
	    ("incrementCallback")
	    ("pageDecrementCallback")
	    ("pageIncrementCallback")
	    ("toBottomCallback")
	    ("toTopCallback")
	    ("focusCallback")
	    ("losingFocusCallback")
	    ("modifyVerifyCallback" :modify-verify)
	    ("valueChangedCallback")
	    ("noMatchCallback")
	    ("cancelCallback")
	    ("applyCallback")
	    ("okCallback")
	    ("browseSelectionCallback" :single-selection)
	    ("singleSelectionCallback" :single-selection)
	    ("defaultActionCallback")
	    ("extendedSelectionCallback")
	    ("multipleSelectionCallback" :multiple-selection)
	    ("entryCallback")
	    ("mapCallback")
	    ("unmapCallback")
	    ("cascadingCallback")
	    ("commandChangedCallback")
	    ("commandEnteredCallback")
	    ("exposeCallback" drawing-area)
	    ("inputCallback" drawing-area)
	    ("resizeCallback" drawing-area)
	    ("destroyCallback")
	    ("gainPrimaryCallback")
	    ("losePrimaryCallback")

	    ;; Motif Callbacks

	    ;; OpenLook Callbacks
	    ("sliderMoved" slider-moved)
	    ("select")
	    ("unselect")
	    ("postModifyNotification")
	    ("userMakeCurrent" ol-list-item-make-current)
	    )))

(defun convert-callback-name (x)
  (let ((z (assoc x *callback-name-alist*)))
    (unless z (error "No such Callback: ~S" x))
    (values (or (third z)
		(setf (third z)
		  (lisp-string-to-string8 (second z))))
	    (fourth z))))

(defmethod spread-callback-data (widget data (type (eql :activate)))
  (declare (ignore widget))
  (x-push-button-callback-struct-click-count data))

(defmethod spread-callback-data (widget data (type (eql :modify-verify)))
  (declare (ignore widget))
  data)

(defmethod spread-callback-data (widget call-data (type (eql 'drawing-area)))
  (declare (ignore widget))
  (values (x-drawing-area-callback-window call-data)
	  (x-drawing-area-callback-event call-data)))

