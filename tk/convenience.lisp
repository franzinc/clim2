(in-package :tk)

					;
;
;
;(defmacro define-convenience-function (name entry-point class)
;  (let ((c-function-name (intern (substitute #\_ #\- (symbol-name name)))))
;    `(progn
;       (defforeign ',c-function-name
;	 :entry-point ,entry-point)
;       (defun ,name (parent name &rest arglist)
;	 (let ((arglist (make-arglist-for-class
;			 (find-class ',class)
;			 parent
;			 arglist)))
;	   (register-handle
;	    (make-instance
;	     ',class
;	     :display display
;	     :handle
;	     (,c-function-name
;	      (object-handle parent)
;	      (string-to-char* name)
;	      arglist
;	      (truncate (length arglist) 2)))))))))
;	
;
;(define-convenience-function create-menu-bar 
;			     "_XmCreateMenuBar"
;			     xm-row-column)
;
;(define-convenience-function create-pulldown-menu 
;			     "_XmCreatePulldownMenu" 
;			     xm-row-column)
;
;(define-convenience-function create-row-column
;			     "_XmCreateRowColumn"
;			     xm-row-column)
;
;
;(define-convenience-function create-radio-box
;			     "_XmCreateRadioBox"
;			     xm-row-column)

(defmacro define-convenience-class (class superclasses entry-point)
  (let ((c-function-name
	 (intern (substitute #\_ #\- 
			     (lispify-tk-name entry-point :package nil)))))
    `(progn
       (defforeign ',c-function-name :entry-point ,entry-point)
       (defclass ,class ,superclasses 
		 ()
	 (:metaclass xt-class))
       (defmethod make-widget ((w ,class) &rest args &key (managed t)
			       (name "") 
			       parent &allow-other-keys)
	 (remf :name args)
	 (remf :parent args)
	 (remf :managed args)
	 (let* ((arglist (make-arglist-for-class
			  (find-class ',class)
			  parent
			  args))
		(o 
		 (,c-function-name
		  (object-handle parent)
		  (string-to-char* name)
		  arglist
		  (truncate (length arglist) 2))))
	   (when managed
	     (manage_child o))
	   o)))))

(define-convenience-class xm-menu-bar (xm-row-column) "_XmCreateMenuBar")
(define-convenience-class xm-pulldown-menu (xm-row-column) "_XmCreatePulldownMenu")
(define-convenience-class xm-radio-box (xm-row-column) "_XmCreateRadioBox")
