(in-package :tk)

(defgeneric make-widget (widget &key)
  ;;; returns the handle of a widget
  )

(defmethod make-widget ((w core) &rest args &key parent (managed t) (name "") &allow-other-keys)
  (remf :managed args)
  (remf :name args)
  (remf :parent args)
  (let ((class (class-of w)))
    (if managed
	(apply #'create-managed-widget name class parent args)
      (apply #'create-widget name class parent args))))

(defmethod make-widget ((w xm-gadget) 
			&rest args &key parent (managed t) (name "") &allow-other-keys)
  (remf :managed args)
  (remf :name args)
  (remf :parent args)
  (let ((class (class-of w)))
    (if managed
	(apply #'create-managed-widget name class parent args)
      (apply #'create-widget name class parent args))))


(defmethod make-widget ((w shell) &rest args &key parent (name "") &allow-other-keys)
  (error "shells not made this way"))

(defmethod make-widget ((w top-level-shell) &rest args &key parent (name "") &allow-other-keys)
  (remf :parent args)
  (remf :name args)
  (apply #'create-popup-shell name (class-of w) parent args))

(defmethod make-widget ((w xm-dialog-shell) &rest args &key parent (name "") &allow-other-keys)
  (remf :parent args)
  (remf :name args)
  (apply #'create-popup-shell name (class-of w) parent args))

(defmethod make-widget ((w xm-menu-shell) &rest args &key parent (name "") &allow-other-keys)
  (remf :parent args)
  (remf :name args)
  (apply #'create-popup-shell name (class-of w) parent args))

(defforeign 'xm_main_window_set_area :entry-point
  "_XmMainWindowSetAreas")

(defun add-resource-to-class (class resource)
  (clos::map-over-subclasses
   #'(lambda (c)
       (push
	resource
	(slot-value c 'resources)))
   class))

(tk::add-resource-to-class (find-class 'vendor-shell)
			   (make-instance 'resource
					  :name :delete-response
					  :type 'tk::delete-response
					  :original-name 
					  (string-to-char*
					   "deleteResponse")))


(tk::add-resource-to-class (find-class 'vendor-shell)
			   (make-instance 'resource
					  :name :keyboard-focus-policy
					  :type 'tk::keyboard-focus-policy
					  :original-name 
					  (string-to-char*
					   "keyboardFocusPolicy")))


