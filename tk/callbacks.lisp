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
;; $fiHeader: callbacks.cl,v 1.3 92/01/02 15:08:35 cer Exp Locker: cer $

(in-package :tk)

(defforeign 'add_callback
    :entry-point "_XtAddCallback")

(defforeign 'xt_has_callbacks
    :entry-point "_XtHasCallbacks")

(defun has-callbacks-p (w name)
  (not (zerop (xt_has_callbacks (object-handle w) name))))

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
      (apply fn
	   widget
	   (append (multiple-value-list (spread-callback-data widget call-data type))
		   args)))
    0))

(defmethod spread-callback-data (widget call-data (type (eql nil)))
  (values))

(defvar *callback-handler-address* (register-function
				    'callback-handler))


(defun add-callback (widget callback-name function &rest args)
  (multiple-value-bind
      (name type)
      (convert-callback-name callback-name)
    (add_callback
     (object-handle widget)
     name
     *callback-handler-address*
     (caar (push
	    (list (new-callback-id) (cons function args) type)
	    (widget-callback-data widget ))))))

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
	    ("modifyVerifyCallback")  
	    ("motionVerifyCallback")  
	    ("valueChangedCallback")  
	    ("noMatchCallback")       
	    ("cancelCallback")        
	    ("applyCallback")         
	    ("okCallback")            
	    ("browseSelectionCallback") 
	    ("singleSelectionCallback") 
	    ("defaultActionCallback")   
	    ("extendedSelectionCallback")
	    ("multipleSelectionCallback")
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

	    ;; OpenLook Callbacks
	    ("sliderMoved" slider-moved)
	    
	    )))

(defun convert-callback-name (x)
  (let ((z (assoc x *callback-name-alist*)))
    (unless z (error "No such Callback: ~S" x))
    (values (or (third z)
		(setf (third z) (string-to-char* (second z))))
	    (fourth z))))


(def-c-type (x-push-button-callback-struct :in-foreign-space) :struct
  (reason :int)
  (event * x11:xevent)
  (click-count :int))

(defmethod spread-callback-data (widget data (type (eql :activate)))
  (x-push-button-callback-struct-click-count data))

(def-c-type (x-drawing-area-callback :in-foreign-space) :struct
  (reason :int)
  (event * x11:xevent)
  (window x11:window))

(defmethod spread-callback-data (widget call-data (type (eql 'drawing-area)))
  (values (x-drawing-area-callback-window call-data)
	  (x-drawing-area-callback-event call-data)))
