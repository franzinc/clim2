;; -*- mode: common-lisp; package: xm-silica -*-
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
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: xm-frames.lisp,v 1.30 92/09/22 19:38:08 cer Exp Locker: cer $

(in-package :xm-silica)

;; Motif stuff

(defclass motif-frame-manager (xt-frame-manager) 
    ()
  (:default-initargs :dialog-view +gadget-dialog-view+))

(defmethod make-frame-manager ((port motif-port) &key)
  (make-instance 'motif-frame-manager :port port))

(defmethod adopt-frame :after ((framem motif-frame-manager) 
			       (frame standard-application-frame))
  (when (frame-panes frame)
    (establish-wm-protocol-callbacks framem frame)))

(defmethod establish-wm-protocol-callbacks ((framem motif-frame-manager) frame)
  (let ((shell (frame-shell frame)))
    (tk::set-values shell :delete-response :do-nothing)
    (tk::add-wm-protocol-callback
      shell 
      :wm-delete-window
      'frame-wm-protocol-callback
      frame)))

;;; Definitions of the individual classes

(defclass motif-menu-bar (xt-leaf-pane menu-bar) ())

(defmethod find-widget-class-and-initargs-for-sheet 
    ((port motif-port) (parent t) (sheet motif-menu-bar))
  (if (flat-command-table-menu-p (menu-bar-command-table sheet))
      (values 'xt::xm-row-column 
	      ;;--- It makes sense to be able to specify the orientation
	      ;;--- of the command menu
	      (list :orientation :horizontal))
    (values 
     'tk::xm-menu-bar 
     ;;---- This seems important but why
     ;;---- At a guess I would say its because the query-geometry gets
     ;;---- all stupid if these resources are NIL
     (list :resize-height t
	   :resize-width t))))

(defun flat-command-table-menu-p (ct)
  (map-over-command-table-menu-items
   #'(lambda (menu keystroke item)
       (declare (ignore keystroke menu))
       (let ((type (command-menu-item-type item)))
	 (when (eq type :menu) (return-from flat-command-table-menu-p nil))))
   ct)
  t)
		      

;;; If would be nice if we could abstract this and use it for the OLIT
;;; port

(defmethod realize-mirror :around ((port motif-port) (sheet motif-menu-bar))

  ;; This code fills the menu-bar. If top level items do not have
  ;; submenus then it creates one with a menu of its own
  
  (let ((mirror (call-next-method)))
    (labels ((update-menu-item-sensitivity (widget frame commands)
	       (declare (ignore widget))
	       (dolist (cbs commands)
		 (tk::set-sensitive (second cbs)
				    (command-enabled
				     (car (second (car cbs)))
				     frame))))
	     (make-menu-for-command-table (command-table parent top)
	       (if top
		   (make-menu-for-command-table-1 command-table parent
						  top)
		 (let ((commands-and-buttons
			(make-menu-for-command-table-1 command-table parent top)))
		   ;; We might have add-callbacks for a different set
		   ;; of children which are now destroyed
		   (tk::remove-all-callbacks parent :map-callback)
		   (tk::add-callback parent :map-callback
				     #'update-menu-item-sensitivity 
				     (pane-frame sheet)
				     commands-and-buttons))))
	     (make-submenu (parent menu item)
	       (let* ((shell (make-instance 'xt::xm-menu-shell 
					    :width 5 ; This widget is
						     ; realized when
						     ; it is created
						     ; so we have to
						     ; specify a size!
					    :height 5
					    :allow-shell-resize t
					    :override-redirect t
					    :parent parent))

		      (submenu (make-instance 'xt::xm-row-column
					      :managed nil
					      :parent shell
					      :row-column-type :menu-pulldown))
		      (cb (make-instance 'xt::xm-cascade-button-gadget
					 :parent parent
					 :label-string menu
					 :sub-menu-id submenu)))
		 
		 (set-button-mnemonic sheet
				      cb (getf (command-menu-item-options item) :mnemonic))
		 
		 (let* ((ct (find-command-table (second item)))
			(tick (slot-value ct 'clim-internals::menu-tick)))
		   (make-menu-for-command-table ct submenu nil)
		   ;;-- This is what we wanted to do but looses
		   ;;-- because of the shared shell
		  (setf (tk::widget-create-popup-child-proc shell)
		    #'(lambda (shell)
			(declare (ignore shell))
			(let ((children
			       (tk::widget-children submenu)))
			  (when (or (null children)
				    (/= tick
					(setq tick
					  (slot-value ct 'clim-internals::menu-tick))))
			    (mapc #'tk::destroy-widget children)
			    (make-menu-for-command-table
			     ct
			     submenu
			     nil))))))))
	     (make-menu-for-command-table-1 (command-table parent top)
	       ;; Unless we are at the top level we want to have a
	       ;; map-before callback that sets the sensitivity of
	       ;; each item. Also we might want to regenerate the menu
	       ;; if it has got out of date wrt to the command table.
	       (let ((commands-and-buttons nil))
		 (map-over-command-table-menu-items
		  #'(lambda (menu keystroke item)
		      (let ((type (command-menu-item-type item)))
			(case type
			  (:divider
			   (make-instance 'tk::xm-separator
					      :managed nil
					      :parent menu))
			  (:function
			   ;;--- Do this sometime
			   )
			  (:menu
			   (make-submenu parent menu item))
			  (t
			   (let ((parent parent))
			     (when top
			       (let* ((submenu (make-instance
						'tk::xm-pulldown-menu
						:managed nil
						:parent parent))
				      (cb (make-instance 'xt::xm-cascade-button-gadget
							 :parent parent
							 :label-string menu
							 :sub-menu-id
							 submenu)))
				 (declare (ignore cb))
				 (setq parent submenu)))
			     (let ((button 
				    (make-instance 'xt::xm-push-button-gadget
						   :label-string menu
						   :managed t
						   :parent parent)))
			       (push (list item button)
				     commands-and-buttons)
			       
			       (set-button-accelerator-from-keystroke 
				sheet
				button keystroke)

			       (set-button-mnemonic
				sheet
				button (getf (command-menu-item-options item) :mnemonic))

			       (when (getf (command-menu-item-options item) :documentation)
				 (tk::add-callback
				  button
				  :help-callback
				  'display-motif-help
				  port
				  (getf (command-menu-item-options
					 item) :documentation)))
			       
			       (tk::add-callback
				button
				:activate-callback
				'command-button-callback
				(slot-value sheet 'silica::frame)
				command-table
				item)))))))
			  
		  command-table)
		 commands-and-buttons)))
      (let ((ct (menu-bar-command-table sheet)))
	(make-menu-for-command-table
	 ct mirror (not (flat-command-table-menu-p ct)))))
    mirror))

(defun display-motif-help (widget port documentation)
  (frame-manager-notify-user 
   (find-frame-manager :port port)		;--- frame manager should be passed in
   documentation
   :associated-window widget))
	     

(defmethod update-frame-settings ((framem motif-frame-manager) (frame t))
  ;;--- Lets see how this works out
  (let ((shell (sheet-shell (frame-top-level-sheet frame))))
    (let ((sr (compose-space (frame-top-level-sheet frame))))
      (tk::set-values shell
		      :min-width (fix-coordinate (space-requirement-min-width sr))
		      :min-height (fix-coordinate (space-requirement-min-height sr))))

    (let ((geo (clim-internals::frame-geometry frame)))
      (destructuring-bind
	  (&key left top width height &allow-other-keys) geo
	;;-- what about width and height
	(when (and width height)
	  (tk::set-values shell :width (fix-coordinate width) 
			  :height (fix-coordinate height)))
	(when (and left top)
	  (tk::set-values shell 
			  :x (fix-coordinate left)
			  :y (fix-coordinate top)))))
    
    (tk::set-values shell :title (frame-pretty-name frame))
    (let ((icon (clim-internals::frame-icon frame)))
      (flet ((decode-pixmap (x)
	       (etypecase x
		 (string x)
		 (pattern 
		  (let ((sheet (frame-top-level-sheet frame)))
		    (with-sheet-medium (medium sheet)
		      (second 
		       (decode-gadget-background medium sheet x))))))))
	(destructuring-bind
	    (&key name pixmap clipping-mask) icon
	  ;;-- Dialog shells do not have :icon-name resource
	  (when (and name (typep shell 'tk::top-level-shell))
	    (tk::set-values shell :icon-name name))
	  (when pixmap
	    (tk::set-values shell :icon-pixmap (decode-pixmap pixmap)))
	  (when clipping-mask
	    (tk::set-values shell :clip-mask (decode-pixmap clipping-mask))))))))


;;;

(defmethod frame-manager-construct-menu 
	   ((framem motif-frame-manager) 
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
	 (menu (make-instance 'tk::xm-popup-menu 
			      :parent (or (and associated-window
					       (sheet-mirror associated-window))
					  (port-application-shell port))
			      :managed nil)))
    (when label
      (make-instance 'xt::xm-label
		     :parent menu
		     :managed nil
		     :label-string label)
      (make-instance 'xt::xm-separator
		     :managed nil
		     :separator-type :double-line
		     :parent menu))
    (labels ((make-menu-button (item class parent &rest options)
	       (let ((button
		      (if simplep
			  (apply #'make-instance
				 class 
				 :sensitive (clim-internals::menu-item-active item)
				 :parent parent 
				 :managed nil
				 :label-string (string (menu-item-display item))
				 options) 
			(let* ((pixmap (pixmap-from-menu-item
					associated-window 
					item
					printer
					presentation-type))
			       (button
				(apply #'make-instance
				       class 
				       :sensitive (clim-internals::menu-item-active item)
				       :parent parent 
				       :label-type :pixmap
				       :label-pixmap pixmap
				       options)))
			  (xt::add-widget-cleanup-function
			   button
			   #'tk::destroy-pixmap pixmap)
			  button))))
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
			      (:divider
			       (make-instance 'xt::xm-separator
					      :managed nil
					      :parent menu))
			      (:label
				  (make-instance 'xt::xm-label
						 :parent menu
						 :managed nil
						 :label-string (string
								(menu-item-display item))))
			      (:item
			       (if (clim-internals::menu-item-items item)
				   (let* ((submenu (make-instance
						    'tk::xm-pulldown-menu
						    :managed nil
						    :parent menu))
					  (menu-button
					   (make-menu-button item 
							     'xt::xm-cascade-button
							     menu
							     :sub-menu-id submenu)))
				     (declare (ignore menu-button))
				     (construct-menu-from-items 
				      submenu 
				      (clim-internals::menu-item-items item)))
				 (let ((menu-button
					(make-menu-button item 
							  'xt::xm-push-button
							  menu))
				       (value (menu-item-value item)))
				   (tk::add-callback
				    menu-button
				    :activate-callback
				    #'(lambda (&rest args)
					(declare (ignore args))
					(setq return-value (list value item)
					      value-returned t))))))))
		    items)
	       ;;
	       (tk::manage-children (tk::widget-children menu))))
	
      (construct-menu-from-items menu items))
    (tk::add-callback (widget-parent menu)
		      :popdown-callback
		      #'(lambda (&rest ignore) 
			  (declare (ignore ignore))
			  (setq value-returned t)))
    (values menu
	    #'(lambda (&optional init)
		(if init
		    (setf value-returned nil return-value nil)
		  (values value-returned return-value))))))

(defmethod framem-enable-menu ((framem motif-frame-manager) menu)
  (tk::manage-child menu))

(defmethod framem-destroy-menu ((framem motif-frame-manager) menu)
  (tk::destroy-widget (tk::widget-parent menu)))

(defmethod framem-popdown-menu ((framem motif-frame-manager) menu)
  ;; Should be already popped down
  nil)


(defmethod framem-menu-active-p ((framem motif-frame-manager) menu)
  (tk::is-managed-p menu))


;;;; 
