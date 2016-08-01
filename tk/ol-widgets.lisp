;; See the file LICENSE for the full license governing this code.
;;

(in-package :xt)


(defmethod make-widget ((w menu-shell) name parent &rest args)
  (apply #'create-popup-shell name (class-of w) parent args))


(defmethod make-widget ((w transient-shell) name parent &rest args)
  (apply #'create-popup-shell name (class-of w) parent args))


(tk::add-resource-to-class (find-class 'menu-shell)
			   (make-instance 'resource
					  :name :menu-pane
					  :type 'tk::widget
					  :original-name
					  (clim-utils:string-to-foreign
					   "menuPane")))


(tk::add-resource-to-class (find-class 'text-edit)
			   (make-instance 'resource
					  :name :source
					  :type 'string
					  :original-name
					  (clim-utils:string-to-foreign "source")))


(tk::add-resource-to-class (find-class 'ol-list)
			   (make-instance 'resource
					  :name :appl-add-item
					  :type 'function
					  :original-name
					  (clim-utils:string-to-foreign
					   "applAddItem")))

(tk::add-resource-to-class (find-class 'ol-list)
			   (make-instance 'resource
					  :name :appl-touch-item
					  :type 'function
					  :original-name
					  (clim-utils:string-to-foreign "applTouchItem")))

(tk::add-resource-to-class (find-class 'ol-list)
			   (make-instance 'resource
					  :name :appl-delete-item
					  :type 'function
					  :original-name
					  (clim-utils:string-to-foreign
					   "applDeleteItem")))

(tk::add-resource-to-class (find-class 'ol-list)
			   (make-instance 'resource
					  :name :view-height
					  :type 'dimension
					  :original-name
					  (clim-utils:string-to-foreign
					   "viewHeight")))

(tk::add-resource-to-class (find-class 'ol-list)
			   (make-instance 'resource
					  :name :recompute-width
					  :type 'boolean
					  :original-name
					  (clim-utils:string-to-foreign "recomputeWidth")))

(tk::add-resource-to-class (find-class 'vendor-shell)
			   (make-instance 'resource
					  :name :busy
					  :type 'boolean
					  :original-name
					  (clim-utils:string-to-foreign "busy")))

(tk::add-resource-to-class (find-class 'text-field)
			   (make-instance 'resource
					  :name :font-color
					  :type 'pixel
					  :original-name
					  (clim-utils:string-to-foreign "fontColor")))

(tk::add-resource-to-class (find-class 'notice-shell)
			   (make-instance 'resource
					  :name :focus-widget
					  :type 'widget
					  :original-name
					  (clim-utils:string-to-foreign "focusWidget")))
