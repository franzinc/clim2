;; See the file LICENSE for the full license governing this code.
;;

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
       frame) 
      (let* ((palette (frame-palette frame))
	     (colormap (palette-colormap palette)))
	;; Tell motif to use this color palette.
	(tk::set-values shell :colormap colormap)
	))))

(defmethod note-frame-enabled :after ((framem motif-frame-manager) frame)
  ;;-- Doing this gets around the problem with motif-menu bars coming
  ;;-- up the wrong size.
  ;;-- This is because the :resize-width/height are true
  ;;-- ManagedSetChanged allows the widget to be resized.
  ;;-- Conversely querygeoetry will return bogus results otherwise.
  (frobulate-mirrors frame))

(defmethod note-frame-layout-changed ((frame-manager motif-frame-manager) (frame t))
  (frobulate-mirrors frame))

;; This gets around the problem with gadgets in different layouts
;; coming up the wrong size when the layout changes (cim 9/28/95)

(defun frobulate-mirrors (frame)
  (flet ((fix-sheet (sheet)
	   (when (or (typep sheet 'motif-menu-bar)
		     (typep sheet 'clim-stream-pane))
	     (invalidate-cached-transformations sheet))))
    (map-over-sheets #'fix-sheet (frame-top-level-sheet frame))))

;;; Definitions of the individual classes

(defclass motif-menu-bar (motif-row-column-gadget-mixin
			  xt-leaf-pane
			  xt-menu-bar
			  menu-bar)
  ())

(defmethod find-widget-class-and-name-for-sheet
    ((port motif-port) (parent t) (sheet motif-menu-bar))
  (if (flat-command-table-menu-p (or (menu-bar-command-table sheet)
				     (frame-command-table (pane-frame sheet))))
      'xt::xm-row-column
    'tk::xm-menu-bar))

(defmethod find-widget-initargs-for-sheet
    ((port motif-port) (parent t) (sheet motif-menu-bar))
  (unless (flat-command-table-menu-p (or (menu-bar-command-table sheet)
					 (frame-command-table (pane-frame sheet))))
    ;; for xm-menu-bar
    ;;---- This seems important but why
    ;;---- At a guess I would say its because the query-geometry gets
    ;;---- all stupid if these resources are NIL
    ;;---- These are actually the default values
    (list :resize-height t
	  :resize-width t)))

(defun flat-command-table-menu-p (ct)
  (map-over-command-table-menu-items
   #'(lambda (menu keystroke item)
       (declare (ignore keystroke menu))
       (let ((type (command-menu-item-type item)))
	 (when (eq type :menu) (return-from flat-command-table-menu-p nil))))
   ct)
  t)

(defmethod compose-space ((mb motif-menu-bar) &key width height)
  (let ((mirror (sheet-direct-mirror mb)))
    (multiple-value-bind (x y best-width best-height)
	(tk::widget-best-geometry mirror :width width)
      (declare (ignore x y))
      (let ((min-height (if width
			    best-height
			  0))
	    (min-width  (if height
			    best-width
			  0)))
	(multiple-value-bind (margin-width margin-height)
	    (tk::get-values mirror :margin-width :margin-height)
	  (dolist (child (tk::widget-children mirror))
	    (multiple-value-bind (x y width height border-width)
		(tk::widget-best-geometry child)
	      (declare (ignore x y))
	      (maxf min-width (+ width (* 2 margin-width) (* 2 border-width)))
	      (maxf min-height (+ height (* 2 margin-height) (* 2 border-width))))))
	(make-space-requirement
	 :min-width min-width
	 :width best-width
	 :max-width best-width
	 :min-height min-height
	 :height best-height
	 :max-height best-height)))))

;;; If would be nice if we could abstract this and use it for the OLIT
;;; port

;;; ************************************************************
;;;  For spr24205
;;;
;;;  In brief:  Changing an item on a command-table menu on an
;;;  existing frame will cause a seg-fault the next time an
;;;  item on the frame's menu is selected (on motif platforms;
;;;  no such problem appears on the Windows machines).
;;;  
;;;  The basic problem is that a callback-closure is defined in a
;;;  labels-form and cached away.  A seg-fault occurs when closure calls
;;;  other "sibling" labels-functions.
;;;  
;;;  (Note also that the callback-closure is called during a
;;;  foreign-function callback by a motif-gadget, in the Motif process.)
;;;  
;;;  Some details:
;;;  
;;;  1] The following method is called to create a new item for a
;;;  (motif-specific) menubar.
;;;  
;;;  2] One of the things it does is to define a "popup-callback".
;;;  (See the call to xt::add-callback in make-submenu .)
;;;  
;;;  This callback is called when the when the menu is popped-up
;;;  (as a result of being clicked on).  For the present case, the
;;;  important thing that it does is to see if the command-table
;;;  has changed, and the menu needs to be updated.
;;;  
;;;  (See the "t" clause in the cond statement in the closure
;;;  defined by the xt::add-callback in make-submenu .)
;;;  
;;;  To do this, it calls the labels-fuction make-menu-for-command-table
;;;  
;;;  3] make-menu-for-command-table in turn uses
;;;  map-over-command-table-menu to call a closure which contains a call to
;;;  the labels-function item-initargs.
;;;  
;;;  The seg-fault occurs when item-initargs is called.
;;;  
;;;  (That is, the arguments to the function are correct, etc.  Likewise,
;;;  the body of the function is not entered.  Simply calling the function
;;;  causes the seg-fault.)
;;;  
;;;  4] Note: If, for example the code is changed to skip this call to
;;;  item-initarg, the system will crash at the next call to a sibling
;;;  labels-function.  (For example at the subsequent call to
;;;  make-command-button.)
;;;  
;;;  
;;;  The fix:
;;;  The labels-functions in the method have been re-written
;;;  as a collection of "regular" functions.
;;;  (Unfortunately, this entails passing around a lot more
;;;  args to compensate for the lexical variables.)

;;;(defmethod realize-mirror :around ((port motif-port) (sheet motif-menu-bar))
;;;
;;;  ;; This code fills the menu-bar. If top level items do not have
;;;  ;; submenus then it creates one with a menu of its own
;;;
;;;  (let* ((mirror (call-next-method))
;;;	 (initargs
;;;	  #-ignore `(:background ,(tk::get-values mirror :background)
;;;		     :foreground ,(tk::get-values mirror :foreground))
;;;	  ;; I'd prefer to use the code below but unfortunately menu-bars
;;;	  ;; sometimes choose to ignore the background resource given
;;;	  ;; to them so it's safer to get the actual value from the
;;;	  ;; widget (cim 5/10/95)
;;;	  #+ignore (remove-keywords
;;;		    (find-widget-resource-initargs-for-sheet port sheet)
;;;		    '(:font-list)))
;;;	 ;; menu-text-style used to be (pane-text-style sheet)
;;;	 ;; changed 28May97 -tjm
;;;	 ;; changed back 14Sep98 JPM 
;;;	 ;; (X resource database should provide default text styles - spr18064)
;;;	 (menu-text-style (pane-text-style sheet) 
;;;			  #+ignore clim-internals::*default-menu-text-style*)
;;;	 (frame (pane-frame sheet))
;;;	 (top-command-table (or (menu-bar-command-table sheet)
;;;				(frame-command-table frame)))
;;;	 (flatp (flat-command-table-menu-p top-command-table)))
;;;    (labels ((item-initargs (item)
;;;	       (let ((item-text-style (getf (command-menu-item-options item)
;;;					    :text-style)))
;;;		 (list* :font-list
;;;			(text-style-mapping
;;;			 port
;;;			 (if item-text-style
;;;			     (merge-text-styles item-text-style menu-text-style)
;;;			   menu-text-style)
;;;			 *all-character-sets*)
;;;			initargs)))
;;;
;;;	     (set-button-attributes (button options &optional keystroke)
;;;	       (add-documentation-callbacks
;;;		frame button (getf options :documentation))
;;;	       (when (eq (getf options :button-type) :help)
;;;		 (tk::set-values mirror :menu-help-widget button))
;;;	       (unless flatp
;;;		 (set-button-mnemonic sheet button (getf options :mnemonic))
;;;		 (let ((accelerator-text (getf options :accelerator-text)))
;;;		   (when (or keystroke accelerator-text)
;;;		     (set-button-accelerator-from-keystroke
;;;		      sheet button keystroke accelerator-text)))))
;;;
;;;	     (make-command-button (parent menu keystroke item command-table)
;;;	       (let* ((command-name (car (command-menu-item-value item)))
;;;		      (options (command-menu-item-options item))
;;;		      (button (apply #'make-instance 'xt::xm-push-button
;;;				     :label-string menu
;;;				     :managed t
;;;				     :parent parent
;;;				     :sensitive (command-enabled command-name frame)
;;;				     (item-initargs item))))
;;;
;;;		 (push (cons command-name button)
;;;		       (menu-bar-command-name-to-button-table sheet))
;;;
;;;		 (set-button-attributes button options keystroke)
;;;
;;;		 (tk::add-callback button
;;;				   :activate-callback
;;;				   'command-button-callback
;;;				   frame
;;;				   command-table
;;;				   item)
;;;		 button))
;;;
;;;	     (update-menu-item-sensitivity (commands-and-buttons)
;;;	       (dolist (cb commands-and-buttons)
;;;		 (destructuring-bind (item button) cb
;;;		   (tk::set-sensitive button
;;;				      (command-enabled
;;;				       (car (command-menu-item-value item))
;;;				       frame)))))
;;;
;;;	     (make-submenu (parent menu item)
;;;	       (let* ((options (command-menu-item-options item))
;;;		      (initargs (item-initargs item))
;;;		      (submenu (apply #'make-instance
;;;				      'tk::xm-pulldown-menu
;;;				      :managed nil
;;;				      :parent parent
;;;				      initargs))
;;;		      (cb (apply #'make-instance 'xt::xm-cascade-button
;;;				 :parent parent
;;;				 :label-string menu
;;;				 :sub-menu-id submenu
;;;				 initargs)))
;;;
;;;		 (set-button-attributes cb options)
;;;
;;;		 (let* ((sub-command-table (find-command-table
;;;					    (command-menu-item-value item)))
;;;			(tick (slot-value sub-command-table 'clim-internals::menu-tick))
;;;			(commands-and-buttons
;;;			 (make-menu-for-command-table sub-command-table submenu)))
;;;		   (xt::add-callback (tk::widget-parent submenu) :popup-callback
;;;		    #'(lambda (shell)
;;;			(declare (ignore shell))
;;;			(cond
;;;			 ((= tick
;;;			     (setq tick
;;;			       (slot-value sub-command-table
;;;					   'clim-internals::menu-tick)))
;;;			  ;; nothing changed except possibly the sensitivity
;;;			  (update-menu-item-sensitivity commands-and-buttons))
;;;			 (t
;;;			  ;; no need to update the sensitivity here because
;;;			  ;; the menu is freshly remade.
;;;			  (tk::unmanage-children (tk::widget-children submenu))
;;;			  (setq commands-and-buttons
;;;			    (make-menu-for-command-table sub-command-table submenu))
;;;			  ;; not doing this gives a memory leak - the reason
;;;			  ;; we don't is that it seems to break accelerators
;;;			  ;; on the new children (cim 5/12/95)
;;;			  #+ignore (mapc #'tk::destroy-widget children))))))))
;;;
;;;	     (make-menu-for-command-table (command-table parent)
;;;	       (let ((commands-and-buttons nil))
;;;		 (map-over-command-table-menu-items
;;;		  #'(lambda (menu keystroke item)
;;;		      (let ((initargs (item-initargs item)))
;;;			(ecase (command-menu-item-type item)
;;;			  (:divider
;;;			   (ecase (command-menu-item-value item)
;;;			     (:label
;;;			      (apply #'make-instance 'tk::xm-label
;;;				     :label-string menu
;;;				     :parent parent
;;;				     initargs))
;;;			     ((nil :line)
;;;			      (apply #'make-instance 'tk::xm-separator
;;;				     :parent parent
;;;				     initargs))))
;;;			  (:function
;;;			   ;;--- Not yet implemented
;;;			   )
;;;			  (:menu
;;;			   (make-submenu parent menu item))
;;;			  (:command
;;;			   (multiple-value-bind (button-parent cb)
;;;			       (if (and (not flatp)
;;;					(eq command-table top-command-table))
;;;				   (let* ((submenu (apply #'make-instance
;;;							  'tk::xm-pulldown-menu
;;;							  :managed nil
;;;							  :parent parent
;;;							  initargs))
;;;					  (cb (apply #'make-instance 'xt::xm-cascade-button
;;;						     :parent parent
;;;						     :label-string menu
;;;						     :sub-menu-id submenu
;;;						     initargs)))
;;;				     (values submenu cb))
;;;				 parent)
;;;			     (push (list item
;;;					 (make-command-button button-parent
;;;							      menu keystroke item
;;;							      command-table))
;;;				   commands-and-buttons)
;;;			     ;; this is done after creating the command-button so
;;;			     ;; that the help widget is correctly set to be the
;;;			     ;; cascade-button rather than the command button
;;;			     ;; (cim 5/10/95)
;;;			     (when cb
;;;			       (set-button-attributes
;;;				cb (command-menu-item-options item))))))))
;;;		  command-table)
;;;		 commands-and-buttons)))
;;;      (make-menu-for-command-table top-command-table mirror))
;;;    mirror))

(defmethod realize-mirror :around ((port motif-port) (sheet motif-menu-bar))
  ;; This code fills the menu-bar. If top level items do not have
  ;; submenus then it creates one with a menu of its own

  (let* ((mirror (call-next-method))
	 (initargs
	  #-ignore `(:background ,(tk::get-values mirror :background)
				 :foreground ,(tk::get-values mirror :foreground))
	  ;; I'd prefer to use the code below but unfortunately menu-bars
	  ;; sometimes choose to ignore the background resource given
	  ;; to them so it's safer to get the actual value from the
	  ;; widget (cim 5/10/95)
	  #+ignore (remove-keywords
		    (find-widget-resource-initargs-for-sheet port sheet)
		    '(:font-list)))
	 ;; menu-text-style used to be (pane-text-style sheet)
	 ;; changed 28May97 -tjm
	 ;; changed back 14Sep98 JPM 
	 ;; (X resource database should provide default text styles - spr18064)
	 (menu-text-style (pane-text-style sheet) 
			  #+ignore clim-internals::*default-menu-text-style*)
	 (frame (pane-frame sheet))
	 (top-command-table (or (menu-bar-command-table sheet)
				(frame-command-table frame)))
	 (flatp (flat-command-table-menu-p top-command-table)))
    (rmfp-make-menu-for-command-table top-command-table mirror
				     ;; new args
				     flatp top-command-table
				     port menu-text-style initargs
				     frame mirror sheet
				     )
    mirror))

(defun rmfp-item-initargs (item
			   ;; New args
			   port menu-text-style initargs)
  (let ((item-text-style (getf (command-menu-item-options item)
			       :text-style)))
    (list* :font-list
	   (text-style-mapping
	    port
	    (if item-text-style
		(merge-text-styles item-text-style menu-text-style)
                menu-text-style)
	    *all-character-sets*)
	   initargs)))

(defun rmfp-set-button-attributes (button options 
				   ;; keystroke no long optional
				   keystroke
				   ;; new args
				   frame mirror flatp sheet
				   )
  (add-documentation-callbacks
   frame button (getf options :documentation))
  (when (eq (getf options :button-type) :help)
    (tk::set-values mirror :menu-help-widget button))
  (unless flatp
    (set-button-mnemonic sheet button (getf options :mnemonic))
    (let ((accelerator-text (getf options :accelerator-text)))
      (when (or keystroke accelerator-text)
	(set-button-accelerator-from-keystroke
	 sheet button keystroke accelerator-text)))))

(defun rmfp-update-menu-item-sensitivity (commands-and-buttons
					  ;; new args
					  frame)
  (dolist (cb commands-and-buttons)
    (destructuring-bind (item button) cb
      (tk::set-sensitive button
			 (command-enabled
			  (car (command-menu-item-value item))
			  frame)))))

(defun rmfp-make-command-button (parent menu keystroke item command-table
				 ;; new args
				 frame sheet
				 ;; new args for item-initargs
				 port menu-text-style initargs 
				 ;; new args to support set-button-attributes
				 mirror flatp
				 ;; new args to support make-command-button 
				 ;; already supplied
				 )
  (let* ((command-name (car (command-menu-item-value item)))
	 (options (command-menu-item-options item))
	 (button (apply #'make-instance 'xt::xm-push-button
			:label-string menu
			:managed t
			:parent parent
			:sensitive (command-enabled command-name frame)
			(rmfp-item-initargs item
					    ;; new args
					    port menu-text-style initargs))))

    (push (cons command-name button)
	  (menu-bar-command-name-to-button-table sheet))

    (rmfp-set-button-attributes button options keystroke
				;; new args 
				frame mirror flatp sheet)

    (tk::add-callback button
		      :activate-callback
		      'command-button-callback
		      frame
		      command-table
		      item)
    button))

(defun rmfp-make-submenu (parent menu item
			  ;; new args to support item-initargs 
			  port menu-text-style initargs
			  ;; new arg to support update-menu-item-sensitivity
			  frame
			  ;; new args to support set-button-attributes 
			  mirror flatp sheet
			  ;; new args to support make-menu-for-commandtable
			  top-command-table)
  (let* ((options (command-menu-item-options item))
	 (initargs (rmfp-item-initargs item
				       ;; new args 
				       port menu-text-style initargs))
	 (submenu (apply #'make-instance
			 'tk::xm-pulldown-menu
			 :managed nil
			 :parent parent
			 initargs))
	 (cb (apply #'make-instance 'xt::xm-cascade-button
		    :parent parent
		    :label-string menu
		    :sub-menu-id submenu
		    initargs)))

    (rmfp-set-button-attributes cb options
				;; arg (keystroke) no longer optional
				nil
				;; new args 
				frame mirror flatp sheet
				)

    (let* ((sub-command-table (find-command-table
			       (command-menu-item-value item)))
	   (tick (slot-value sub-command-table 'clim-internals::menu-tick))
	   (commands-and-buttons
	    (rmfp-make-menu-for-command-table sub-command-table submenu
					      ;; new args
					      flatp top-command-table
					      port menu-text-style initargs
					      frame mirror sheet)))
      (xt::add-callback (tk::widget-parent submenu) :popup-callback
			#'(lambda (shell)
			    (declare (ignore shell))
			    (cond
			     ((= tick
				 (setq tick
				   (slot-value sub-command-table
					       'clim-internals::menu-tick)))
			      ;; nothing changed except possibly the sensitivity
			      (rmfp-update-menu-item-sensitivity commands-and-buttons
								 ;; new arg
								 frame))
			     (t
			      ;; no need to update the sensitivity here because
			      ;; the menu is freshly remade.
			      (tk::unmanage-children (tk::widget-children submenu))
			      (setq commands-and-buttons
				(rmfp-make-menu-for-command-table sub-command-table submenu
								  ;; new args
								  flatp top-command-table
								  port menu-text-style initargs 
								  frame mirror sheet 
								  ))
			      ;; not doing this gives a memory leak - the reason
			      ;; we don't is that it seems to break accelerators
			      ;; on the new children (cim 5/12/95)
			      #+ignore (mapc #'tk::destroy-widget children))))))))

(defun rmfp-make-menu-for-command-table (command-table parent
					 ;; new args
					 flatp top-command-table
					 ;; additional args to support item-initargs 
					 port menu-text-style initargs
					 ;; args to support set-button-attributes
					 frame mirror sheet
					 ;; args to support make-submenu already supplied
					 )
  (let ((commands-and-buttons nil))
    (map-over-command-table-menu-items
     #'(lambda (menu keystroke item)
	 (let ((initargs (rmfp-item-initargs item
					     ;; new args
					     port menu-text-style initargs
					     )))
	   (ecase (command-menu-item-type item)
	     (:divider
	      (ecase (command-menu-item-value item)
		(:label
		 (apply #'make-instance 'tk::xm-label
			:label-string menu
			:parent parent
			initargs))
		((nil :line)
		 (apply #'make-instance 'tk::xm-separator
			:parent parent
			initargs))))
	     (:function
	      ;;--- Not yet implemented
	      )
	     (:menu
	      (rmfp-make-submenu parent menu item
				 ;; new args 
				 port menu-text-style initargs
				 frame
				 mirror flatp sheet 
				 top-command-table))
	     (:command
	      (multiple-value-bind (button-parent cb)
		  (if (and (not flatp)
			   (eq command-table top-command-table))
		      (let* ((submenu (apply #'make-instance
					     'tk::xm-pulldown-menu
					     :managed nil
					     :parent parent
					     initargs))
			     (cb (apply #'make-instance 'xt::xm-cascade-button
					:parent parent
					:label-string menu
					:sub-menu-id submenu
					initargs)))
			(values submenu cb))
		    parent)
		(push (list item
			    (rmfp-make-command-button button-parent
						      menu keystroke item
						      command-table
						      ;; new args
						      frame sheet
						      port menu-text-style initargs 
						      mirror flatp
						      ))
		      commands-and-buttons)
		;; this is done after creating the command-button so
		;; that the help widget is correctly set to be the
		;; cascade-button rather than the command button
		;; (cim 5/10/95)
		(when cb
		  (rmfp-set-button-attributes
		   cb (command-menu-item-options item)
		   ;; arg (keystroke) no longer optional
		   nil
		   ;; new args
		   frame mirror flatp sheet
		   )))))))
     command-table)
    commands-and-buttons))
;;;
;;;  For spr24205
;;; ************************************************************


;; get rid of get-accelerator-text and clean this function up!

(defmethod set-button-accelerator-from-keystroke
    ((menubar motif-menu-bar) button keystroke accelerator-text)
  (when keystroke
    (let ((modifiers (cdr keystroke)))
      (record-accelerator menubar keystroke)
      (multiple-value-bind (accel accel-text)
	  (get-accelerator-text keystroke)
	(dolist (modifier modifiers)
	  (setq accel-text
	    (concatenate 'string
	      (case modifier
		(:shift "Shift+")
		(:control "Ctrl+")
		(:meta "Alt+")
		(:super "Super+")
		(:hyper "Hyper+")
		(t ""))
	      accel-text))
	  (setq accel
	    (concatenate 'string
	      (case modifier
		(:shift "Shift ")
		(:control "Ctrl ")
		(:meta "Mod1 ")
		(:super "Mod2 ")
		(:hyper "Mod3 ")
		(t ""))
	      accel)))
	(tk::set-values button :accelerator accel)
	(setq accelerator-text (or accelerator-text
				   accel-text)))))
  (when accelerator-text
    (tk::set-values button :accelerator-text accelerator-text)))

(defmethod clim-internals::initialize-for-command-reader 
    ((frame-manager motif-frame-manager) (frame t))
  (flet ((fix-sheet (sheet)
	   (when (typep sheet 'motif-menu-bar)
	     (update-menubar-sensitivity sheet frame))))
    (declare (dynamic-extent #'fix-sheet))
    (map-over-sheets #'fix-sheet (frame-top-level-sheet frame))))

(defmethod update-menubar-sensitivity ((sheet motif-menu-bar) frame)
  ;; There is a common convention in CLIM applications to control
  ;; the enabling/disabling of commands by defining EQL-specialized
  ;; methods on the COMMAND-ENABLED generic function.  That is a
  ;; passive approach that won't have any effect until the function
  ;; is called.  Here is where we can call the COMMAND-ENABLED function.
  (let ((command-table (or (menu-bar-command-table sheet)
			   (frame-command-table (pane-frame sheet))))
	(framem (frame-manager frame)))
    (labels ((visit-table (ct)
	       (map-over-command-table-menu-items
		#'(lambda (menu keystroke item)
		    (declare (ignore keystroke menu))
		    (let ((type (command-menu-item-type item))
			  (value (command-menu-item-value item)))
		      (cond ((eq type :menu)
			     (visit-table value))
			    ((eq type :command)
			     (let* ((name (car value))
				    (enabled (command-enabled name frame)))
			       (if enabled
				   (note-command-enabled framem frame name)
				 (note-command-disabled framem frame name)))))))
		ct)))
      (declare (dynamic-extent #'visit-table))
      (visit-table command-table))))

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
	 ;; default-text-style used to be (sheet-text-style port associated-window)
	 ;; changed 28May97 -tjm
	 (default-text-style clim-internals::*default-menu-text-style*)
	 (menu-text-style (if text-style
			      (merge-text-styles text-style
						 default-text-style)
			    default-text-style))
	 (frame (pane-frame associated-window))
	 (parent (if associated-window
		     (sheet-mirror associated-window)
		   (port-application-shell port)))
	 (menu (apply #'make-instance  'tk::xm-popup-menu
		      :parent parent
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
      (let ((title (if (atom label) label (car label)))
	    (menu-text-style clim-internals::*default-menu-label-text-style*))
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
			   menu-text-style)
			 *all-character-sets*)
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
		      (sensitive (clim-internals::menu-item-active item))
		      (button
		       (if simplep
			   (apply #'make-instance class
				  :sensitive sensitive
				  :parent parent
				  :managed nil
				  :label-string (princ-to-string (menu-item-display item))
				  (list* :font-list
					 (text-style-mapping port
							     text-style
							     *all-character-sets*)
					 options))
			 (let* ((pixmap (pixmap-from-menu-item
					 associated-window
					 item
					 printer
					 presentation-type
					 :text-style text-style
					 :background background
					 :foreground foreground
					 :gray-out (not sensitive)))
				(button
				 (apply #'make-instance class
					:sensitive sensitive
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
						  menu-text-style)
						*all-character-sets*)
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

;;; spr25133 --pnc
;;; Running the following (at least since Motif2.1) causes an
;;; "XtGrabPointer failed" warning to be reported.  (This seems
;;; particularly true on Linux platforms.)
;;; What appears to be happening is a race-condition.
;;; 1] (port-pointer-grabbed-p xt-port) works by first trying to
;;; grab the pointer (using XGrabPointer)  If the grab succeeds
;;; (meaning it is not already grabed) it ungrabs the pointer 
;;; and returns nil.
;;; 2] It apears that XtManageChild also tries to grab the pointer
;;; (by using XtGrabPointer).  If the ungrab (in port-pointer-grabbed-p)
;;; hasn't completed yet, the second grab (in tk::manage-child)
;;; prints out a warning.
;;; The (present) solution is to sleep briefly (and, consequently
;;; delay the menu pop-up by a small amount).
(defmethod framem-enable-menu ((framem motif-frame-manager) menu)
  ;; don't try to popup a menu if the mouse is already grabbed
  ;; (cim 1/3/96)
  (unless (port-pointer-grabbed-p (port framem)) 
    (sleep 0.1)
    (tk::manage-child menu)))

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
