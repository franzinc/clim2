;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-1998 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: xt-frames.lisp,v 1.47.22.3 1998/07/06 23:10:21 layer Exp $

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
	 (frame-exit frame))))

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
	  scroll-bars)
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
		y-position root-y)))

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
