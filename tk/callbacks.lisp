(in-package :tk)

(defforeign 'add_callback
    :entry-point "_XtAddCallback")

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

(defparameter *callback-name-alist* 
    (mapcar #'(lambda (x)
		(destructuring-bind (name &optional type) x
		  (list (lispify-tk-name name :package :keyword)
		    name
		    nil;; malloc cache
		    type
		    )))
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
