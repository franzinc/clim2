;; See the file LICENSE for the full license governing this code.
;;

(in-package :xm-silica)

;; Basic intrinsics frame-manager

(defclass xt-frame-manager (standard-frame-manager)
    ((menu-cache
       :initform nil
       :accessor frame-manager-menu-cache)
     ))

(defmethod frame-wrapper ((framem xt-frame-manager)
			  (frame standard-application-frame) pane)
  (with-look-and-feel-realization (framem frame)
    (let* ((menu-bar (slot-value frame 'menu-bar))
	   (menu-bar-pane
	    (and menu-bar
		 (apply #'make-pane 'menu-bar
			:name :menu-bar
			:command-table (cond ((eq menu-bar t)
					      (frame-command-table frame))
					     ((listp menu-bar)
					      (find-command-table (car menu-bar)))
					     (t (find-command-table menu-bar)))
			(and (listp menu-bar) (cdr menu-bar)))))
	   (pointer-doc-pane
	    ;;--- Don't like these forward references
	    (let ((options (clim-internals::frame-pointer-documentation-p frame)))
	      (and options
		   (apply #'make-pane
			  'clim-internals::pointer-documentation-pane
			  :name :pointer-documentation
			  (append (and (listp options) options)
				  `(
				    :max-width ,+fill+
				    :min-height (1 :line)
				    :max-height (1 :line)
				    :height (1 :line))))))))
      (cond ((and menu-bar-pane pointer-doc-pane)

	     (vertically () menu-bar-pane pane pointer-doc-pane))
	    (menu-bar-pane
	     (vertically () menu-bar-pane pane))
	    (pointer-doc-pane
	     (vertically () pane pointer-doc-pane))
	    (t pane)))))


;;;

(defun command-button-callback (button dunno frame command-table item)
  (declare (ignore dunno button))
  (execute-command-in-frame
   frame (substitute frame clim-internals::*application-frame-marker*
		     (command-menu-item-value item))
   :presentation-type `(command :command-table ,command-table)))


(defun frame-wm-protocol-callback (widget frame)
  (declare (ignore widget))
  ;; Invoked when the Wm close function has been selected
  ;; We want to queue an "event" somewhere so that we can
  ;; synchronously quit from the frame
  (distribute-event
   (port frame)
   (allocate-event 'window-manager-delete-event
     :sheet (frame-top-level-sheet frame))))

(defmethod handle-event (sheet (event window-manager-delete-event))
  (let ((frame (pane-frame sheet)))
    (and frame
         (if (typep frame 'clim-internals::accept-values-own-window)
             ;; [spr30282] A window-manager-delete-event (i.e. a window close
             ;; event) is supposed to equate to a "cancel" gesture.
             ;; (alemmens, 2005-07-04)
             (abort)
             (frame-exit frame)))))

;;; Menu code

(defmethod frame-manager-menu-choose
    ((framem xt-frame-manager) items &rest keys
     &key printer
	  presentation-type
	  (associated-window (frame-top-level-sheet *application-frame*))
	  text-style label
	  foreground background
	  cache
	  (unique-id items)
	  (id-test 'equal)
	  (cache-value items)
	  (cache-test #'equal)
	  (gesture :select)
	  row-wise
	  n-columns
	  n-rows
	  x-position
	  y-position
	  scroll-bars
	  
	  default-item  ;; bug12221/spr25238
	  
	  )
  (declare (values value chosen-item gesture))
  ;; let's degrade to CLIM 1 style menus if we need scroll-bars
  (if scroll-bars
      (call-next-method)
    (let ((port (port framem))
	  menu closure)
      (setq cache (and cache (frame-manager-allows-menu-caching framem)))
      (when cache
	(let ((x (assoc unique-id
			(frame-manager-menu-cache framem)
			:test id-test)))
	  (when x
	    (destructuring-bind
		(menu-cache-value amenu aclosure) (cdr x)
	      (if (funcall cache-test cache-value menu-cache-value)
		  (setq menu amenu closure aclosure)
		(progn
		  (setf (frame-manager-menu-cache framem)
		    (delete x (frame-manager-menu-cache framem)))
		  (framem-destroy-menu framem amenu)))))))

      (unless menu
	(multiple-value-setq
	    (menu closure)
	  (frame-manager-construct-menu framem
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
					n-rows))
	(when cache
	  (push (list unique-id cache-value menu closure)
		(frame-manager-menu-cache framem))))

      ;; initialize the closure
      (funcall closure t)
      ;;

      (unless (and x-position y-position)
	(multiple-value-bind
	    (ignore win1 win2 x y root-x root-y)
	    (tk::query-pointer (tk::display-root-window
				(port-display port)))
	  (declare (ignore ignore win1 win2 x y))
	  (setq x-position root-x
		y-position root-y)
	  
	  ;; bug12221/spr25238
	  (multiple-value-setq (x-position y-position)
	    (calculate-xm-menu-pos framem
				   x-position y-position
				   items
				   default-item
				   label
				   ))
	  
	  ))

      (tk::set-values menu :x x-position :y y-position)
      (let ((aborted t))
	(catch 'menu-choose
	  (unwind-protect
	      (progn
		(loop
		  (when (funcall closure)
		    (setq aborted nil)
		    (return nil))
		  (framem-enable-menu framem menu)
		  ;; Now we have to wait
		  (port-force-output port)
		  (with-toolkit-dialog-component (menu-choose items)
		    (wait-for-callback-invocation
		     port
		     #'(lambda ()
			 ;;-- This is to deal
			 ;;-- with the race
			 ;;-- condition where
			 ;;-- the menu go down
			 ;;-- to quick
			 (or (funcall closure)
			     (not (framem-menu-active-p framem menu))))
		     "Returned value")))
		(framem-popdown-menu framem menu))
	    (if cache
		(when aborted (framem-popdown-menu framem menu))
	      (framem-destroy-menu framem menu)))
	  (port-force-output port)
	  (values-list (nth-value 1 (funcall closure))))))))


(defun calculate-xm-menu-pos (framem 
			      init-cursor-x init-cursor-y
			      items
			      default-item
			      label)
  framem items default-item
  (let ((menu-left init-cursor-x)
	(menu-top init-cursor-y))

    (let ((default-item-pos (and default-item
				 (position default-item items))))
      (when default-item-pos
	(when label
	  (setq default-item-pos (+ default-item-pos 1)))
	(let ((line-hei 20) ;; A good guess at the height of a menu-item
	      (bottom-buffer 10) ;; Put a buffer at the bottom, in case the line-hei is not perfect.
	      (cursor-x nil)
	      (cursor-y nil))
	  (multiple-value-bind (graft-width graft-height)
	      (bounding-rectangle-size (graft framem))
	    graft-width
	    (let ((item-offset-x 6)
		  (item-offset-y (+ (* default-item-pos line-hei) 10))
		  (menu-hei (* (length items) line-hei)))
	      (cond  
	       ((< (- graft-height bottom-buffer)
		   menu-hei)
		;; Now, place the menu against the top of the screen...
		(setq menu-left init-cursor-x
		      menu-top 0)
		;; ... and try to place the pointer on the default-item.
		(setq cursor-x (+ init-cursor-x item-offset-x)
		      cursor-y (min (+ menu-top item-offset-y)
				    (- graft-height bottom-buffer))))
	       ((< (- graft-height bottom-buffer) 
		   (+ (- init-cursor-y item-offset-y) menu-hei))
		;; We are bumping against the bottom.
		;; So place the menu at the bottom of the screen...
		(setq menu-left init-cursor-x
		      menu-top (- (- graft-height bottom-buffer) menu-hei))
		;; ... and place the pointer on the default-item.
		(setq cursor-x (+ init-cursor-x item-offset-x)
		      cursor-y (+ menu-top item-offset-y))
		)
	       ((< init-cursor-y item-offset-y)
		;; We are bumping against the top of the screen.
		;; So place the menu at the top...
		(setq menu-left init-cursor-x
		      menu-top 0)
		;; ... and place the pointer on the default-item.
		(setq cursor-x (+ init-cursor-x item-offset-x)
		      cursor-y (+ menu-top item-offset-y))) 
	       (t
		;; The normal case-- everything fits on the screen
		;; So position the menu so the the default item
		;; comes up under the pointer.
		(setq menu-left (- init-cursor-x item-offset-x)
		      menu-top  (- init-cursor-y item-offset-y))
		;; Warp the pointer slightly, 
		;; so that the item highlights properly 
		(setq cursor-x (+ init-cursor-x 1)
		      cursor-y (+ init-cursor-y 1))))

	      ;; Finally, warp the pointer
	      (when (and cursor-x cursor-y)
		(let ((port (port framem)))
		  (let ((pntr (port-pointer port)))
		    (setf (clim-internals::pointer-native-position pntr)
		      (values cursor-x cursor-y))))))))))
    (values  menu-left
	     menu-top)))




(defmethod frame-manager-allows-menu-caching ((framem xt-frame-manager))
  t)


;;;

(defmethod frame-manager-exit-box-labels ((framem xt-frame-manager) frame view)
  (declare (ignore frame view))
  '((:exit  "OK" :documentation "Exit from dialog" :show-as-default t)
    (:abort  "Cancel" :documentation "Cancel dialog")))

(defmethod frame-manager-default-exit-boxes ((framem xt-frame-manager))
  '((:exit) (:abort)))

(defvar *suppress-xevents* nil)

(defmethod note-frame-iconified ((framem xt-frame-manager) frame)
  (unless *suppress-xevents*
    (let ((display (port-display (port framem))))
      (x11:xiconifywindow display (tk::widget-window (frame-shell frame))
			  (xt::display-screen-number display)))))

(defmethod note-frame-deiconified ((framem xt-frame-manager) frame)
  (unless *suppress-xevents*
    (x11:xmapwindow (port-display (port framem))
		    (tk::widget-window (frame-shell frame)))))

;;;

(defmethod invoke-with-mouse-grabbed-in-window ((framem xt-frame-manager) (window t) continuation &key)
  (invoke-with-pointer-grabbed window continuation))



(defclass xt-menu-bar ()
  ((command-name-to-button-table
    :accessor menu-bar-command-name-to-button-table
    :initform nil)))

(defmethod note-sheet-degrafted :after ((sheet xt-menu-bar))
  (setf (menu-bar-command-name-to-button-table sheet) nil))

(defmethod note-command-enabled :after ((framem xt-frame-manager) frame command)
  (update-command-button-status frame command t))

(defmethod note-command-disabled :after ((framem xt-frame-manager) frame command)
  (update-command-button-status frame command nil))

(defun update-command-button-status (frame command enabled)
  (let ((sheet (frame-top-level-sheet frame)))
    (when sheet
      (flet ((update-sheet (sheet)
	       (when (typep sheet 'xt-menu-bar)
		 (let ((button (cdr (assoc command (menu-bar-command-name-to-button-table sheet)))))
		   (when button
		     (tk::set-sensitive button enabled))))))
	(map-over-sheets #'update-sheet sheet)))))

(defmethod update-frame-settings ((framem xt-frame-manager) (frame t))
  ;;--- Lets see how this works out
  (let ((graft (graft framem))
	(shell (sheet-shell (frame-top-level-sheet frame))))
    (let* ((sr (compose-space (frame-top-level-sheet frame)))
	   (width (space-requirement-min-width sr))
	   (height (space-requirement-min-height sr)))
      (clim-internals::limit-size-to-graft width height graft)
      (tk::set-values shell
		      :min-width (fix-coordinate width)
		      :min-height (fix-coordinate height)))
    (let ((geo (clim-internals::frame-geometry frame)))
      (destructuring-bind
	  (&key left top width height &allow-other-keys) geo
	;;-- what about width and height
	(when (and width height)
	  (clim-internals::limit-size-to-graft width height graft)
	  (tk::set-values shell :width (fix-coordinate width)
			  :height (fix-coordinate height)))
	(when (and left top)
	  (tk::set-values shell
			  :x (fix-coordinate left)
			  :y (fix-coordinate top)))))

    (tk::set-values shell :title (frame-pretty-name frame))
    (let ((icon (clim-internals::frame-icon frame)))
      (flet ((decode-bitmap (x &optional format)
	       (etypecase x
		 ((or string tk::pixmap) x)
		 (pattern
		  (let ((sheet (frame-top-level-sheet frame)))
		    (with-sheet-medium (medium sheet)
		      (pixmap-from-pattern x medium format)))))))
	(destructuring-bind
	    (&key (name (frame-pretty-name frame)) pixmap clipping-mask) icon
	  ;;-- Dialog shells do not have :icon-name resource
	  (when (typep shell 'tk::top-level-shell)
	    (tk::set-values shell :icon-name name))
	  (when pixmap
	    (tk::set-values shell :icon-pixmap (decode-bitmap pixmap)))
	  (when clipping-mask
	    (tk::set-values shell :icon-mask (decode-bitmap clipping-mask :bitmap))))))))

(defmethod frame-manager-note-pretty-name-changed ((framem xt-frame-manager)
						   (frame standard-application-frame))
  (let ((shell (frame-shell frame)))
    (tk::set-values shell :title (frame-pretty-name frame))
    (when (typep shell 'tk::top-level-shell)
      (destructuring-bind (&key name &allow-other-keys) (clim-internals::frame-icon frame)
	;;-- Dialog shells do not have :icon-name resource
	(tk::set-values shell :icon-name (or name (frame-pretty-name frame)))))))

(defmethod add-documentation-callbacks (frame button documentation)
  (declare (ignore frame button documentation)))

(defun pointer-documentation-callback-function (widget frame documentation showp)
  (declare (ignore widget))
  (let ((pointer-documentation-pane
	 (clim-internals::frame-actual-pointer-documentation-pane frame)))
    (when pointer-documentation-pane
      (frame-manager-display-pointer-documentation-string
       (frame-manager frame)
       frame
       pointer-documentation-pane
       (and showp documentation)))))

(defmethod clim-internals::frame-manager-position-dialog ((framem xt-frame-manager)
							  frame
							  own-window-x-position own-window-y-position)
  (when (and own-window-x-position own-window-y-position)
    (position-sheet-carefully
     (frame-top-level-sheet frame) own-window-x-position own-window-y-position)))
