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
;; $fiHeader: xm-frames.lisp,v 1.66 1993/12/07 05:34:21 colin Exp $

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

(defmethod note-frame-layout-changed ((frame-manager motif-frame-manager) (frame t))
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

(defun update-menu-item-sensitivity (widget frame commands)
  (declare (ignore widget))
  (dolist (cbs commands)
    (tk::set-sensitive (second cbs)
		       (command-enabled
			(car (second (car cbs)))
			frame))))

(defmethod realize-mirror :around ((port motif-port) (sheet motif-menu-bar))

  ;; This code fills the menu-bar. If top level items do not have
  ;; submenus then it creates one with a menu of its own
  
  (let* ((mirror (call-next-method))
	 (initargs (remove-keywords
		    (find-widget-resource-initargs-for-sheet port sheet)
		    '(:font-list)))
	 (menu-text-style (pane-text-style sheet))
	 (ct (menu-bar-command-table sheet))
	 (flatp (flat-command-table-menu-p ct))
	 (frame (pane-frame sheet)))
    (labels 
	((compute-options (item)
	   (let ((item-text-style (getf (command-menu-item-options item)
					:text-style)))
	     (list* :font-list
		    (text-style-mapping
		     port
		     (if item-text-style
			 (merge-text-styles item-text-style menu-text-style)
		       menu-text-style))
		    initargs)))

	 (make-command-button (parent menu item keystroke command-table)
	   (let ((options (compute-options item))
		 (command-name (car (command-menu-item-value item))))
	     (let ((button 
		    (apply #'make-instance 'xt::xm-push-button
			   :label-string menu
			   :managed t
			   :parent parent
			   :sensitive (command-enabled command-name frame)
			   options)))
	       
	       (when (or (equalp menu "exit")
			 (eq (getf (command-menu-item-options item) :button-type)
			     :help))
		 (tk::set-values mirror :menu-help-widget button))

	       (add-documentation-callbacks 
		frame button (getf (command-menu-item-options item) :documentation))

	       (push (cons command-name button)
		     (menu-bar-command-name-to-button-table sheet))
			       
	       (unless flatp
		 (set-button-accelerator-from-keystroke 
		  sheet
		  button keystroke)
			
		 (set-button-mnemonic
		  sheet
		  button (getf
			  (command-menu-item-options
			   item) :mnemonic)))
	       
	       (tk::add-callback button
				 :activate-callback
				 'command-button-callback
				 frame
				 command-table
				 item))))
	 
	 (make-submenu (parent menu item)
	   (let* ((options (compute-options item))
		  (submenu (apply #'make-instance
				  'tk::xm-pulldown-menu
				  :managed nil
				  :parent parent
				  options))
		  (cb (apply #'make-instance 'xt::xm-cascade-button
			     :parent parent
			     :label-string menu
			     :sub-menu-id submenu
			     options)))

	     (set-button-mnemonic sheet
				  cb
				  (getf (command-menu-item-options item)
					:mnemonic))
	     #+ignore
	     (when (equalp menu "help")
	       (tk::set-values mirror :menu-help-widget cb))

	     (add-documentation-callbacks 
	      frame cb (getf (command-menu-item-options item) :documentation))
		 
	     (make-menu-for-command-table (find-command-table
					   (command-menu-item-value item))
					  submenu nil)))

	 (make-menu-for-command-table (command-table parent top)
	   (map-over-command-table-menu-items
	    #'(lambda (menu keystroke item)
		(ecase (command-menu-item-type item)
		  (:divider
		   (ecase (command-menu-item-value item)
		     (:label 
		      (apply #'make-instance 'tk::xm-label
			     :label-string menu
			     :parent parent
			     (compute-options item)))
		     ((nil :line)
		      (apply #'make-instance 'tk::xm-separator
			     :parent parent
			     (compute-options item)))))
		  (:function
		   ;;--- Do this sometime
		   )
		  (:menu
		   (make-submenu parent menu item))
		  (:command
		   (let* ((button-parent
			   (if top
			       (let* ((submenu (apply #'make-instance
						      'tk::xm-pulldown-menu
						      :managed nil
						      :parent parent
						      (compute-options item)))
				      (cb (apply #'make-instance 'xt::xm-cascade-button
						 :parent parent
						 :label-string menu
						 :sub-menu-id
						 submenu
						 (compute-options item))))
				 (declare (ignore cb))
				 submenu) 
			     parent))
			     (button (make-command-button
				      button-parent menu item
				      keystroke command-table)))))))
	    command-table)))
      (make-menu-for-command-table ct mirror (not flatp)))
    mirror))

(defmethod set-button-accelerator-from-keystroke ((menubar motif-menu-bar) button keystroke)
  (when keystroke 
    (let ((modifiers (cdr keystroke)))
      (when modifiers
	(record-accelerator menubar keystroke))
      (multiple-value-bind (accel accel-text)
	  (get-accelerator-text keystroke)
	(dolist (modifier modifiers)
	  (setq accel-text
	    (concatenate 'string 
	      (ecase modifier 
		(:control "Ctrl+") 
		(:meta "Alt+")
		(:super "Super+")
		(:hyper "Hyper+"))
	      accel-text))
	  (setq accel
	    (concatenate 'string 
	      (ecase modifier 
		(:control "Ctrl")
		(:meta "Mod1")
		(:super "Mod2")
		(:hyper "Mod3"))
	      accel)))
	(if modifiers
	    (tk::set-values button 
			    :accelerator accel
			    :accelerator-text accel-text)
	  (tk::set-values button 
			  :accelerator-text accel-text))))))

(defun display-motif-help (widget framem documentation)
  (frame-manager-notify-user 
   framem
   documentation
   ;;-- Gross
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
	    foreground
	    background
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
	 ;; we treat text-style specially as individual menu items can
	 ;; have their own text-style which must be merged (cim 10/3/94)
	 (initargs (remove-keywords 
		    (find-widget-resource-initargs-for-sheet 
		     port associated-window
		     :foreground foreground :background background)
		    '(:font-list)))
	 (default-text-style (sheet-text-style port associated-window))
	 (menu-text-style (if text-style
			      (merge-text-styles text-style
						 default-text-style)
			    default-text-style))
	 (frame (pane-frame associated-window))
	 (menu (apply #'make-instance  'tk::xm-popup-menu 
		      :parent (or (and associated-window
				       (sheet-mirror associated-window))
				  (port-application-shell port))
		      :menu-post (case (and gesture
					    (gesture-name-button-and-modifiers gesture))
				   (#.+pointer-left-button+ "<Btn1Down>")
				   (#.+pointer-middle-button+ "<Btn2Down>")
				   (#.+pointer-right-button+ "<Btn3Down>")
				   (t "<Btn3Down>"))
		      :orientation (if row-wise 
				       :horizontal
				     :vertical)
		      :packing (if (or n-columns n-rows) :column :tight)
		      :num-columns 
		      (or n-columns
			  (and n-rows
			       (ceiling (length items) n-rows))
			  (if row-wise 
			      (length items)
			    1))
		      :managed nil
		      initargs)))
    (when label
      (let ((title (if (atom label) label (car label))))
	(check-type title string)
	(destructuring-bind (&key text-style) (and (listp label) (cdr label))
	  (apply #'make-instance 'xt::xm-label
		 :parent menu
		 :managed nil
		 :label-string title
		 (list* :font-list
			(text-style-mapping
			 port
			 (if text-style
			     (merge-text-styles text-style menu-text-style)
			   menu-text-style))
			initargs))))
      (apply #'make-instance 'xt::xm-separator
	     :managed nil
	     :separator-type :double-line
	     :parent menu
	     initargs))
    (labels ((make-menu-button (item class parent &rest options)
	       (let* ((item-text-style (clim-internals::menu-item-text-style item))
		      (text-style (if item-text-style
				      (merge-text-styles item-text-style menu-text-style)
				    menu-text-style))
		      (button
		       (if simplep
			   (apply #'make-instance class 
				  :sensitive (clim-internals::menu-item-active item)
				  :parent parent 
				  :managed nil
				  :label-string (princ-to-string (menu-item-display item))
				  (list* :font-list
					 (text-style-mapping port text-style)
					 options))
			 (let* ((pixmap (pixmap-from-menu-item
					 associated-window 
					 item
					 printer
					 presentation-type
					 text-style))
				(button
				 (apply #'make-instance class 
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
		 (add-documentation-callbacks
		  frame button
		  (clim-internals::menu-item-documentation item))
		 button))
	     (construct-menu-from-items (menu items)
	       (map nil #'(lambda (item)
			    (ecase (clim-internals::menu-item-type item)
			      (:divider
			       (apply #'make-instance 'xt::xm-separator
				      :parent menu
				      initargs))
			      (:label
			       (let ((item-text-style
				      (clim-internals::menu-item-text-style item)))
				 (apply #'make-instance 'xt::xm-label
					:parent menu
					:managed nil
					:label-string (string
						       (menu-item-display item))
					(list* :font-list
					       (text-style-mapping
						port
						(if item-text-style
						    (merge-text-styles item-text-style
								       menu-text-style)
						  menu-text-style))
					       initargs))))
			      (:item
			       (if (clim-internals::menu-item-items item)
				   (let* ((submenu (apply #'make-instance
							  'tk::xm-pulldown-menu
							  :managed nil
							  :parent menu
							  initargs))
					  (menu-button
					   (apply #'make-menu-button item 
						  'xt::xm-cascade-button
						  menu
						  :sub-menu-id submenu
						  initargs)))
				     (declare (ignore menu-button))
				     (construct-menu-from-items 
				      submenu 
				      (clim-internals::menu-item-items item)))
				 (let ((menu-button
					(apply #'make-menu-button item 
					       'xt::xm-push-button
					       menu
					       initargs))
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
      (let ((old-string (tk::get-values (car *working-dialog*) :message-string))
	    (old-value (tk::get-values (cdr *working-dialog*) :value)))
	(unwind-protect
	    (progn
	      (tk::set-values (car *working-dialog*) :message-string (string (progress-note-name note)))
	      (tk::set-values (cdr *working-dialog*) :value 0)
	      (funcall continuation note))
	  (tk::set-values (car *working-dialog*) :message-string old-string)
	  (tk::set-values (cdr *working-dialog*) :value old-value)))
    (let ((dialog (make-instance 'xt::xm-working-dialog
				 :dialog-style :modeless
				 :managed nil
				 :parent (let ((stream (slot-value note 'stream)))
					   (if stream
					       (frame-shell (pane-frame stream))
					     (frame-shell *application-frame*)))
				 :dialog-title "Progress Note"
				 :name :progress-note
				 :resize-policy :grow
				 :message-string
				 (format nil "~A" (progress-note-name note)))))
      (multiple-value-bind
	  (ok-button cancel-button help-button separator)
	  (get-message-box-child dialog :ok :cancel :help :separator)
	(tk::unmanage-child help-button)
	;; 
	(if (find-restart 'abort nil)
	    (tk::add-callback cancel-button :activate-callback #'(lambda (widget count process)
								   (declare (ignore widget count))
								   (mp:process-interrupt process 'abort))
			      mp:*current-process*)
	  (xt::unmanage-child cancel-button))
	(tk::unmanage-child ok-button)
	(tk::unmanage-child separator))
      (let ((slider
	     (make-instance 'xt::xm-scale 
			    :show-value t
			    :managed t
			    :sensitive nil
			    :parent dialog
			    :orientation :horizontal)))
	(unwind-protect
	    (progn
	      (tk::manage-child dialog)
	      (let ((*working-dialog* (cons dialog slider)))
		(funcall continuation note)))
	  (tk::destroy-widget dialog))))))

(defmethod clim-internals::frame-manager-display-progress-note
    ((framem motif-frame-manager) note)
  (with-slots ((name clim-internals::name)
	       (stream clim-internals::stream)
	       (numerator clim-internals::numerator)
	       (denominator clim-internals::denominator)) note
    (let ((slider (cdr *working-dialog*)))
      (unless (xt::is-managed-p slider)
	(tk::manage-child slider))
      (tk::set-values slider
		      :value (round (* 100 numerator) denominator)))))

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
									      label
									      parent-frame)
  (let ((frame (make-application-frame 'motif-menu-frame
				       :scroll-bars scroll-bars
				       :label label
				       :frame-manager framem
				       :save-under t
				       :calling-frame parent-frame)))
    ;; This so that ports can do something interesting with popped-up
    ;; menu frames, such as implemented "click off menu to abort".
    (setf (getf (frame-properties frame) :menu-frame) t)
    (values (slot-value frame 'clim-internals::menu) frame)))

(defmethod add-documentation-callbacks (frame (button tk::xm-push-button) documentation)
  (when documentation
    (tk::add-callback
     button :arm-callback 
     'pointer-documentation-callback-function
     frame documentation t)
    (tk::add-callback
     button :disarm-callback 
     'pointer-documentation-callback-function
     frame documentation nil)))

(defmethod add-documentation-callbacks (frame (button tk::xm-cascade-button) documentation)
  (when documentation
    (tk::add-callback
     button :cascading-callback 
     'pointer-documentation-callback-function
     frame documentation t)))

(defmethod add-documentation-callbacks :after (frame (button t) documentation)
  (when documentation
    (tk::add-callback button 
		      :help-callback 
		      'display-motif-help
		      (frame-manager frame)
		      documentation)))
