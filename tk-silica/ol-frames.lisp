;; -*- mode: common-lisp; package: xm-silica -*-
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
;; $fiHeader: ol-frames.lisp,v 1.7 92/07/20 16:01:47 cer Exp Locker: cer $


(in-package :xm-silica)

(defclass openlook-frame-manager (xt-frame-manager) 
    ()
  (:default-initargs :dialog-view +gadget-dialog-view+))

(defmethod make-frame-manager ((port openlook-port))
  (make-instance 'openlook-frame-manager :port port))


(defmethod adopt-frame :after ((framem openlook-frame-manager) 
			       (frame standard-application-frame))
  (when (frame-panes frame)
    (establish-wm-protocol-callbacks framem frame)))

(defmethod establish-wm-protocol-callbacks ((framem openlook-frame-manager) frame)
  (let ((shell (frame-shell frame)))
    (tk::add-ol-callback
      shell 
      (ff::string-to-char* "wmProtocol")
      :wm-protocol
      'ol-frame-wm-protocol-callback
      frame)))

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
	    default-style
	    label)
  (declare (ignore default-style))
  (let* (value-returned 
	 return-value
	 (simplep (and (null printer)
		       (null presentation-type)))
	 (port (port framem))
	 (menu-shell (make-instance 'xt::menu-shell
			      :parent (or (and associated-window
					       (sheet-mirror associated-window))
					  (port-application-shell port))
			      :menu-augment nil
			      :managed nil))
	 (menu (tk::get-values menu-shell :menu-pane)))

    (tk::set-values menu-shell :title (or label "Choose"))
    
    (labels ((make-menu-button (item class parent &rest options)
	       (let ((button
		      (if simplep
			  (apply #'make-instance
				 class 
				 :sensitive (clim-internals::menu-item-active item)
				 :parent parent 
				 :managed nil
				 :label (string (menu-item-display item))
				 options) 
			(let* ((pixmap (pixmap-from-menu-item
					associated-window 
					item
					printer
					presentation-type))
			       (image (tk::image-from-pixmap pixmap))
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
		 #+dunno
		 (when (clim-internals::menu-item-documentation item)
		   (tk::add-callback button 
				 :help-callback 
				 'display-motif-help
				 port
				 (clim-internals::menu-item-documentation item)))
		 button))
	     (construct-menu-from-items (menu items)
	       (map nil #'(lambda (item)
			    (ecase (clim-internals::menu-item-type item)
			      (:separator
			       (make-instance 'xt::static-text
					      :parent menu
					      :managed nil
					      :string " "))
			      (:label
			       (make-instance 'xt::static-text
						 :parent menu
						 :managed nil
						 :string (string
								(menu-item-display item))))
			      (:item
			       (if (clim-internals::menu-item-items item)
				   (let* ((menu-button
					   (make-menu-button item 
							     'xt::menu-button
							     menu))
					  (submenu (tk::get-values menu-button :menu-pane)))
				     (construct-menu-from-items 
				      submenu 
				      (clim-internals::menu-item-items item)))
				 (let ((menu-button
					(make-menu-button item 'xt::oblong-button menu))
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
  (declare (ignore t))
  t)

