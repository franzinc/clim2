;; -*- mode: common-lisp; package: tk -*-
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
;; $fiHeader: callbacks.lisp,v 1.20 92/11/18 15:54:51 colin Exp $

(in-package :tk)

(defun has-callbacks-p (w name)
  (> (xt_has_callbacks w name) 1))

(defun-c-callable callback-handler ((widget :unsigned-long)
				    (client-data :unsigned-long)
				    (call-data :unsigned-long))
  (callback-handler-1 widget client-data call-data))

(defun callback-handler-1 (widget client-data call-data)
  (let* ((widget (find-object-from-address widget))
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

(defvar *callback-handler-address* (register-function
				    'callback-handler))


(defun add-callback (widget callback-name function &rest args)
  (multiple-value-bind
      (name type)
      (convert-callback-name callback-name)
    (xt_add_callback
     widget
     name
     *callback-handler-address*
     (caar (push
	    (list (new-callback-id) (cons function args) type)
	    (widget-callback-data widget))))))


(defun-c-callable create-popup-child-proc-function  ((widget :unsigned-long))
  (create-popup-child-proc-function-1 widget))

(defun create-popup-child-proc-function-1 (widget)
  (let* ((widget (find-object-from-address widget))
	 (function (or (cdr (assoc :create-popup-child-proc (widget-callback-data widget)))
		       (error "cannot find create-popup-childp-proc ~S" widget))))
    (funcall function widget)))


(defvar *create-popup-child-proc-function-address* 
    (ff:register-function 'create-popup-child-proc-function))

(defun (setf widget-create-popup-child-proc) (function widget)
  (push
   (cons :create-popup-child-proc function)
   (widget-callback-data widget))
  (set-values widget :create-popup-child-proc *create-popup-child-proc-function-address*)
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
	    ("motionVerifyCallback")  
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
		(setf (third z) (second z)))
	    (fourth z))))

(defmethod spread-callback-data (widget data (type (eql :activate)))
  (declare (ignore widget))
  (x-push-button-callback-struct-click-count data))

(defmethod spread-callback-data (widget data (type (eql :modify-verify)))
  (declare (ignore widget))
  (xm-text-field-callback-struct-doit data))

(defmethod spread-callback-data (widget call-data (type (eql 'drawing-area)))
  (declare (ignore widget))
  (values (x-drawing-area-callback-window call-data)
	  (x-drawing-area-callback-event call-data)))

