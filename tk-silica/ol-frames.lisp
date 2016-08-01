;; See the file LICENSE for the full license governing this code.
;;

(in-package :xm-silica)

(defclass openlook-frame-manager (xt-frame-manager)
    ()
  (:default-initargs :dialog-view +gadget-dialog-view+))

(defmethod make-frame-manager ((port openlook-port) &key palette &allow-other-keys)
  (make-instance 'openlook-frame-manager :port port :palette palette))


(defmethod port-note-frame-adopted :after
	   ((port openlook-port) (frame standard-application-frame))
  (when (frame-panes frame)
    (let ((shell (frame-shell frame)))
      (tk::add-ol-callback
       shell
       (clim-utils:string-to-foreign "wmProtocol")
       :wm-protocol
       'ol-frame-wm-protocol-callback
       frame))))

(defun ol-frame-wm-protocol-callback (shell value frame)
  (when (eq value :wm-delete-window)
    (frame-wm-protocol-callback shell frame)))


;;;;

(defmethod frame-manager-construct-menu
	   ((framem openlook-frame-manager)
	    items
	    printer
	    presentation-type
	    associated-window
	    text-style
	    label
	    gesture
	    row-wise
	    n-columns
	    n-rows)
  (declare (ignore gesture))
  (let* (value-returned
	 return-value
	 (simplep (and (null printer)
		       (null presentation-type)))
	 (port (port framem))
	 (initargs (remove-keywords
		    (if (and associated-window
			     (typep associated-window 'sheet-with-resources-mixin))
			(find-widget-resource-initargs-for-sheet port
								 associated-window)
		      (find-application-resource-initargs port))
		    '(:font)))
	 (default-text-style
	     (or (and associated-window
		      (typep associated-window 'sheet-with-resources-mixin)
		      (pane-text-style associated-window))
		 (getf (get-application-resources port) :text-style)
		 *default-text-style*))
	 (menu-text-style (if text-style
			      (merge-text-styles text-style
						 default-text-style)
			    default-text-style))
	 (menu-shell (apply #'make-instance 'xt::menu-shell
			    :parent (or (and associated-window
					     (sheet-mirror associated-window))
					(port-application-shell port))
			    :menu-augment nil
			    :managed nil
			    initargs))
	 (menu (tk::get-values menu-shell :menu-pane))
	 (frame (pane-frame associated-window)))

    (when (or n-columns n-rows)
      (let ((n (length items)))
	(if row-wise
	    (tk::set-values menu
			    :layout-type :fixedcols
			    :measure (or n-columns
					 (ceiling n n-rows)))
	  (tk::set-values menu
			    :layout-type :fixedrows
			    :measure (or n-rows
					 (ceiling n n-columns))))))

    (let ((title (cond ((null label) "Choose")
		       ((atom label) label)
		       (t (car label)))))
      (check-type title string)
      (tk::set-values menu-shell :title title))

    (labels ((make-menu-button (item class parent &rest options)
	       (let* ((item-text-style (clim-internals::menu-item-text-style item))
		      (text-style (if item-text-style
				      (merge-text-styles item-text-style menu-text-style)
				    menu-text-style))
		      (button
		      (if simplep
			  (apply #'make-instance
				 class
				 :sensitive (clim-internals::menu-item-active item)
				 :parent parent
				 :managed nil
				 :label (princ-to-string (menu-item-display item))
				 (list* :font
					(text-style-mapping port text-style)
					options))
			(let* ((pixmap (pixmap-from-menu-item
					associated-window
					item
					printer
					presentation-type
					text-style))
			       (image (tk::get-image pixmap
						     :format x11:xypixmap))
			       (button
				(apply #'make-instance
				       class
				       :sensitive (clim-internals::menu-item-active item)
				       :parent parent
				       :label-type :image
				       :label-image image
				       options)))
			  ;;-- Fix this
			  #+ignore
			  (xt::add-widget-cleanup-function
			   button
			   #'tk::destroy-image image)
			  button))))
		 (add-documentation-callbacks
		  frame button
		  (clim-internals::menu-item-documentation item))
		 button))
	     (construct-menu-from-items (menu items)
	       (map nil #'(lambda (item)
			    (ecase (clim-internals::menu-item-type item)
			      (:divider
			       (apply #'make-instance 'xt::static-text
					      :parent menu
					      :managed nil
					      :string " "
					      initargs))
			      (:label
			       (let ((item-text-style
				      (clim-internals::menu-item-text-style item)))
				 (apply #'make-instance 'xt::static-text
					:parent menu
					:managed nil
					:string (string
						 (menu-item-display item))
					(list* :font
					       (text-style-mapping
						port
						(if item-text-style
						    (merge-text-styles item-text-style
								       menu-text-style)
						  menu-text-style))
					       initargs))))
			      (:item
			       (if (clim-internals::menu-item-items item)
				   (let* ((menu-button
					   (apply #'make-menu-button item
						  'xt::menu-button
						  menu
						  initargs))
					  (submenu (tk::get-values menu-button :menu-pane)))
				     (construct-menu-from-items
				      submenu
				      (clim-internals::menu-item-items item)))
				 (let ((menu-button
					(apply #'make-menu-button item
					       'xt::oblong-button
					       menu
					       initargs))
				       (value (menu-item-value item)))
				   (tk::add-callback
				    menu-button
				    :select
				    #'(lambda (&rest args)
					(declare (ignore args))
					(setq return-value (list value item)
					      value-returned t))))))))
		    items)
	       ;;
	       (tk::manage-children (tk::widget-children menu))))

      (construct-menu-from-items menu items))
    (tk::add-callback menu-shell
		      :popdown-callback
		      #'(lambda (&rest ignore)
			  (declare (ignore ignore))
			  (setq value-returned t)))
    (values menu-shell
	    #'(lambda (&optional init)
		(if init
		    (setf value-returned nil return-value nil)
		  (values value-returned return-value))))))



(defmethod framem-enable-menu ((framem openlook-frame-manager) menu)
  (tk::ol_menu_post menu))

(defmethod framem-destroy-menu ((framem openlook-frame-manager) menu)
  ;;--- We need to fix this
  #+ignore(tk::unmanage-child menu)
  #-ignore
  (tk::destroy-widget menu))

(defmethod framem-popdown-menu ((framem openlook-frame-manager) menu)
  (tk::ol_menu_popdown menu 1))

(defmethod framem-menu-active-p ((framem openlook-frame-manager) menu)
  (declare (ignore menu))
  t)


(defmethod clim-internals::frame-manager-invoke-with-noting-progress ((framem openlook-frame-manager)
								      note
								      continuation)
  (let ((shell (let ((stream (slot-value note 'stream)))
		 (if stream
		     (frame-shell (pane-frame stream))
		   (frame-shell *application-frame*)))))
    (if (typep shell 'xt::vendor-shell)
	(let ((old-busy (tk::get-values shell :busy)))
	  (unwind-protect
	      (progn
		(tk::set-values shell :busy t)
		(call-next-method))
	    (tk::set-values shell :busy old-busy)))
      (call-next-method))))
