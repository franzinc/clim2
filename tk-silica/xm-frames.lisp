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
;; $fiHeader: xm-frames.lisp,v 1.54 93/04/16 09:45:56 cer Exp $

(in-package :xm-silica)

;; Motif stuff

(defclass motif-frame-manager (xt-frame-manager) 
    ()
  (:default-initargs :dialog-view +gadget-dialog-view+))

(defmethod make-frame-manager ((port motif-port) &key palette &allow-other-keys)
  (make-instance 'motif-frame-manager :port port :palette palette))

(defmethod port-note-frame-adopted :after
	   ((port motif-port) (frame standard-application-frame))
  (when (frame-panes frame)
    (let ((shell (frame-shell frame)))
      (tk::set-values shell :delete-response :do-nothing)
      (tk::add-wm-protocol-callback
       shell 
       :wm-delete-window
       'frame-wm-protocol-callback
       frame))))

(defmethod note-frame-enabled :after ((framem motif-frame-manager) frame)
  ;;-- Doing this gets around the problem with motif-menu bars coming
  ;;-- up the wrong size.
  ;;-- This is because the :resize-width/height are true 
  ;;-- ManagedSetChanged allows the widget to be resized.
  ;;-- Conversely querygeoetry will return bogus results otherwise.
  (frobulate-menubars frame))

(defmethod clim-internals::note-frame-current-layout-changed ((frame-manager motif-frame-manager) (frame t))
  (frobulate-menubars frame))

(defun frobulate-menubars (frame)
  (flet ((fix-sheet (sheet)
	   (when (typep sheet 'motif-menu-bar)
	     (update-mirror-region (port sheet) sheet))))
    (declare (dynamic-extent #'fix-sheet))
    (map-over-sheets #'fix-sheet (frame-top-level-sheet frame))))

;;; Definitions of the individual classes

(defclass motif-menu-bar (xt-leaf-pane xt-menu-bar menu-bar) 
  ())

(defmethod find-widget-class-and-initargs-for-sheet 
    ((port motif-port) (parent t) (sheet motif-menu-bar))
  (if (flat-command-table-menu-p (menu-bar-command-table sheet))
      (values 'xt::xm-row-column 
	      ;;--- It makes sense to be able to specify the orientation
	      ;;--- of the command menu
	      (list :orientation (gadget-orientation sheet)))
    (values 
     'tk::xm-menu-bar 
     ;;---- This seems important but why
     ;;---- At a guess I would say its because the query-geometry gets
     ;;---- all stupid if these resources are NIL
     ;;---- These are actually the default values
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

(defmethod compose-space ((mb motif-menu-bar) &key width height)
  (let ((sr (call-next-method)))
    (when (or width height)
      (return-from compose-space sr))
    (let ((max-height 0)
	  (max-width  0)
	  (m (sheet-direct-mirror mb)))
      (multiple-value-bind (margin-width margin-height)
	  (tk::get-values m :margin-width :margin-height)
	(dolist (child (tk::widget-children m))
	  (multiple-value-bind (x y width height border-width)
	      (tk::widget-best-geometry child)
	    (declare (ignore x y))
	    (maxf max-width (+ width (* 2 margin-width)  (* 2 border-width)))
	    (maxf max-height (+ height (* 2 margin-height) (* 2 border-width)))))
	(make-space-requirement
	 :min-width max-width
	 :width (space-requirement-width sr)
	 :max-width (space-requirement-max-width sr)
	 :min-height max-height
	 :height (space-requirement-height sr)
	 :max-height (space-requirement-max-height sr))))))

;;; If would be nice if we could abstract this and use it for the OLIT
;;; port

(defmethod realize-mirror :around ((port motif-port) (sheet motif-menu-bar))

  ;; This code fills the menu-bar. If top level items do not have
  ;; submenus then it creates one with a menu of its own
  
  (let* ((mirror (call-next-method))
	 (text-style (pane-text-style sheet))
	 (font-list (list :font-list (text-style-mapping port text-style)))
	 (options font-list)
	 (ct (menu-bar-command-table sheet))
	 (flatp (flat-command-table-menu-p ct)))
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
		      (cb (apply #'make-instance 'xt::xm-cascade-button-gadget
					 :parent parent
					 :label-string menu
					 :sub-menu-id submenu
					 options)))
		 (unless flatp
		   (set-button-mnemonic sheet
					cb (getf (command-menu-item-options item) :mnemonic)))
		 
		 (let* ((ct (find-command-table (second item)))
			(tick (slot-value ct 'clim-internals::menu-tick)))
		   (make-menu-for-command-table ct submenu nil)
		   (xt::add-callback shell :popup-callback
		    #'(lambda (shell)
			(declare (ignore shell))
			(let ((children
			       (tk::widget-children submenu)))
			  (when (or (null children)
				    (/= tick
					(setq tick
					  (slot-value ct
						      'clim-internals::menu-tick))))
			    (tk::unmanage-children children)
			    (make-menu-for-command-table
			     ct submenu nil)
			    (mapc #'tk::destroy-widget children))))))))
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
			   (make-instance 'tk::xm-separator :parent parent))
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
				      (cb (apply #'make-instance 'xt::xm-cascade-button
						 :parent parent
						 :label-string menu
						 :sub-menu-id
						 submenu
						 options)))
				 (declare (ignore cb))
				 (setq parent submenu)))
			     (let ((button 
				    (apply #'make-instance 'xt::xm-push-button
					   :label-string menu
					   :managed t
					   :parent parent
					   :sensitive (command-enabled
						       (car (command-menu-item-value item))
						       (slot-value sheet 'silica::frame))
					   options)))
			       (push (list item button)
				     commands-and-buttons)
			       
			       (when flatp 
				 (push (cons (car (command-menu-item-value item))
					     button)
				       (menu-bar-command-name-to-button-table sheet)))
			       
			       (unless flatp
				 (set-button-accelerator-from-keystroke 
				  sheet
				  button keystroke)
			
				 (set-button-mnemonic
				  sheet
				  button (getf (command-menu-item-options item) :mnemonic)))

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
      (make-menu-for-command-table
	 ct mirror (not flatp)))
    mirror))

(defun display-motif-help (widget port documentation)
  (frame-manager-notify-user 
   (find-frame-manager :port port)		;--- frame manager should be passed in
   documentation
   :associated-window widget))
	     

;;--- Perhaps port-update-frame-settings ?

;;;

(defmethod frame-manager-construct-menu 
	   ((framem motif-frame-manager) 
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
  (when row-wise
    (rotatef n-rows n-columns))
  (let* (value-returned 
	 return-value
	 (simplep (and (null printer)
		       (null presentation-type)))
	 (port (port framem))
	 (menu (make-instance 'tk::xm-popup-menu 
			      :parent (or (and associated-window
					       (sheet-mirror associated-window))
					  (port-application-shell
					   port))
			      :menu-post (case (and gesture
						    (gesture-name-button-and-modifiers gesture))
					   (#.+pointer-left-button+ "<Btn1Down>")
					   (#.+pointer-middle-button+ "<Btn2Down>")
					   (#.+pointer-right-button+ "<Btn3Down>")
					   (t "<Btn3Down>"))
			      :orientation (if row-wise 
					       :horizontal
					     :vertical)
			      :packing :column
			      :num-columns 
			      (or n-columns
				  (and n-rows
				       (ceiling (length items) n-rows))
				  (if row-wise 
				      (length items)
				    1))
			      :managed nil))
	 (font (and text-style (text-style-mapping port text-style)))
	 (font-list (and (or label simplep) font (list :font-list (list font)))))
    (when label
      (let ((title (if (atom label) label (car label))))
	(check-type title string)
	(destructuring-bind (&key text-style) (and (listp label) (cdr label))
	  (apply #'make-instance 'xt::xm-label
		 :parent menu
		 :managed nil
		 :label-string title
		 (if text-style
		     (list :font-list (list (text-style-mapping port text-style)))
		   font-list))))
      (make-instance 'xt::xm-separator
		     :managed nil
		     :separator-type :double-line
		     :parent menu))
    (labels ((make-menu-button (item class parent &rest options)
	       (let* ((style (clim-internals::menu-item-text-style item))
		      (button
		      (if simplep
			  (apply #'make-instance
				 class 
				 :sensitive (clim-internals::menu-item-active item)
				 :parent parent 
				 :managed nil
				 :label-string (string (menu-item-display item))
				 (if style
				     (list* :font-list (list (text-style-mapping port style))
					    options)
				 (append font-list options)))
			(let* ((pixmap (pixmap-from-menu-item
					associated-window 
					item
					printer
					presentation-type
					text-style))
			       (button
				(apply #'make-instance
				       class 
				       :sensitive (clim-internals::menu-item-active item)
				       :parent parent 
				       :label-type :pixmap
				       :label-pixmap pixmap
				       :label-insensitive-pixmap pixmap
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

(defmethod frame-manager-allows-menu-caching ((framem motif-frame-manager))
  ;; Deal with the problem of motif installing passive grabs
  nil)

(defmethod framem-enable-menu ((framem motif-frame-manager) menu)
  (tk::manage-child menu))

(defmethod framem-destroy-menu ((framem motif-frame-manager) menu)
  (tk::destroy-widget (tk::widget-parent menu)))

(defmethod framem-popdown-menu ((framem motif-frame-manager) menu)
  (declare (ignore menu))
  ;; Should be already popped down
  nil)


(defmethod framem-menu-active-p ((framem motif-frame-manager) menu)
  (tk::is-managed-p menu))


;;; Progress notification code.

(defvar *working-dialog* nil)

(defmethod clim-internals::frame-manager-invoke-with-noting-progress ((framem motif-frame-manager)
						      note
						      continuation)
  (if *working-dialog*
      (let ((old-string (tk::get-values *working-dialog* :message-string)))
	(unwind-protect
	    (progn
	      (tk::set-values *working-dialog* :message-string (string (progress-note-name note)))
	      (funcall continuation note))
	  (tk::set-values *working-dialog* :message-string old-string)))
    (let ((dialog (make-instance 'xt::xm-working-dialog
					   :dialog-style :primary-application-modal
					   :managed nil
					   :parent (let ((stream (slot-value note 'stream)))
						     (if stream
							 (frame-shell (pane-frame stream))
						       (frame-shell *application-frame*)))
					   :name "Working"
					   :dialog-title "Progress Note"
					   :resize-policy :grow
					   :message-string 
					   (format nil "~A      "(progress-note-name note)))))
      (multiple-value-bind
	  (ok-button cancel-button help-button separator)
	  (get-message-box-child dialog :ok :cancel :help :separator)
	(tk::unmanage-child help-button)
	(xt::unmanage-child cancel-button)
	(tk::unmanage-child ok-button)
	(tk::unmanage-child separator))
      (unwind-protect
	  (progn
	    (tk::manage-child dialog)
	    (let ((*working-dialog* dialog))
	    (funcall continuation note)))
	(tk::destroy-widget dialog)))))

(defmethod clim-internals::frame-manager-display-progress-note
    ((framem motif-frame-manager) note)
  (with-slots ((name clim-internals::name)
	       (stream clim-internals::stream)
	       (numerator clim-internals::numerator)
	       (denominator clim-internals::denominator)) note
    (tk::set-values *working-dialog* 
		    :message-string (format nil "~A: ~3D%" name (round (* 100 numerator) denominator)))))

;;

(define-application-frame motif-menu-frame (clim-internals::menu-frame)
  ()
  (:pane
   (with-slots ((menu clim-internals::menu)
		(label clim-internals::label)
		(scroll-bars clim-internals::scroll-bars)) *application-frame*
     (make-pane 'motif-frame-pane
		:shadow-type :out
		:contents
		(let ((main
		       (if scroll-bars
			   (scrolling (:scroll-bars scroll-bars)
			     (setq menu (make-pane 'clim-stream-pane
						   :initial-cursor-visibility nil)))
			 (setq menu (make-pane 'clim-stream-pane
					       :initial-cursor-visibility nil)))))
		  (if label
		      (vertically ()
			(setq label (make-pane 'label-pane 
					       :label ""
					       :text-style clim-internals::*default-menu-label-text-style*))
			main)
		    main)))))

  (:menu-bar nil))

(defmethod clim-internals::frame-manager-get-menu ((framem
						    motif-frame-manager) &key scroll-bars
						    label)
  (let ((frame (make-application-frame 'motif-menu-frame
				       :scroll-bars scroll-bars
				       :label label
				       :frame-manager framem
				       :save-under t)))
    ;; This so that ports can do something interesting with popped-up
    ;; menu frames, such as implemented "click off menu to abort".
    (setf (getf (frame-properties frame) :menu-frame) t)
    (values (slot-value frame 'clim-internals::menu) frame)))
