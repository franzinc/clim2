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
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: xm-frames.lisp,v 1.15 92/04/28 09:26:24 cer Exp Locker: cer $

(in-package :xm-silica)

;; Motif stuff

(defclass motif-frame-manager (xt-frame-manager)
    ())

(defmethod make-frame-manager ((port motif-port))
  (make-instance 'motif-frame-manager :port port))

(defmethod adopt-frame :after ((framem motif-frame-manager) 
			       (frame standard-application-frame))
  (when (frame-panes frame)
    (establish-wm-protocol-callbacks framem frame)))

(defmethod establish-wm-protocol-callbacks ((framem motif-frame-manager) frame)
  (let ((shell (frame-shell frame)))
    (tk::set-values shell 
		    :icon-name (frame-pretty-name frame)
		    :title (frame-pretty-name frame)
		    :delete-response :do-nothing)
    (tk::add-wm-protocol-callback
      shell 
      :wm-delete-window
      'frame-wm-protocol-callback
      frame)))

;;; Definitions of the individual classes

;; This looks like garbage??

;(defclass motif-main-window (motif-composite-pane)
;  (
;   (contents :accessor main-window-contents :initform nil)
;   (command-window :initform nil :accessor main-window-command-window)
;   command-window-location
;   menu-bar
;   message-window
;   ))
;
;(defmethod initialize-instance :after ((mw motif-main-window)
;				       &key
;				       command-window
;				       command-window-location
;				       menu-bar
;				       message-window
;				       contents)
;  (when contents
;    (setf (main-window-contents mw) contents))
;  (when command-window
;    (setf (main-window-command-window mw) command-window))
;  (when message-window
;    (setf (main-window-message-window mw) message-window)))
;
;(defmethod (setf main-window-contents) :after (nv (mw motif-main-window))
;  (unless (sheet-parent nv)
;    (sheet-adopt-child mw nv)))
;
;(defmethod (setf main-window-command-window) :after (nv (mw motif-main-window))
;  (unless (sheet-parent nv)
;    (sheet-adopt-child mw nv)))
;
;#+ignore
;(defmethod realize-mirror :around (port (motif-main-window))
;  (let ((m (call-next-method)))
;    (tk::xm_set_main_window_areas
;     m
;     .. 
;     (and command-window (realize-mirror port command-window))
;     ..
;     (and message-window (realize-mirror port message-window))
;     ...)))

;;;; Drawing area
;
;#+ignore
;(defclass motif-drawing-area (motif-composite-pane) ())
;
;;;; Row colum
;
;(defclass motif-row-colum (motif-composite-pane) ())
;
;;;; Push button

;#+ignore
;(defclass motif-push-button (motif-leaf-pane) ())

;;; Menu bar

;(defclass motif-menu-bar (motif-composite-pane) ())
;(defclass motif-pulldown-menu (motif-composite-pane) ())
;
;(defclass motif-cascade-button (motif-composite-pane)
;  ((submenu :accessor cascade-button-submenu :initform nil))
;  )
;
;(defmethod realize-mirror :around ((port motif-port) (sheet motif-cascade-button))
;  (let ((m (call-next-method)))
;    (when (cascade-button-submenu sheet)
;      (tk::set-values m :sub-menu-id 
;		      (realize-mirror port (cascade-button-submenu sheet))))
;    m))

(defmethod handle-event (sheet (event window-manager-delete-event))
  (frame-exit *application-frame*))

(defun frame-wm-protocol-callback (widget frame)
  ;; Invoked when the Wm close function has been selected
  ;; We want to queue an "event" somewhere so that we can
  ;; synchronously quit from the frame
  (distribute-event
   (port frame)
   (make-instance 'window-manager-delete-event
		  :sheet (frame-top-level-sheet frame))))


(defclass motif-menu-bar (xt-leaf-pane) 
	  ((command-table :initarg :command-table)))

(defmethod find-widget-class-and-initargs-for-sheet 
    ((port motif-port) (parent t) (sheet motif-menu-bar))
  (if (flat-command-table-menu-p (slot-value sheet 'command-table))
      (values 'xt::xm-row-column 
	      ;;--- It makes sense to be able to specify the orientation
	      ;;--- of the command menu
	      (list :orientation :horizontal))
    (values 
     'tk::xm-menu-bar 
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
		   ;; We should call
		   (tk::add-callback parent :map-callback
				     #'update-menu-item-sensitivity 
				     (pane-frame sheet)
				     commands-and-buttons))))
	     (make-submenu (parent menu item)
	       (let* ((submenu (make-instance
				'tk::xm-pulldown-menu
				:managed nil
				:parent parent))
		      (cb (make-instance 'tk::xm-cascade-button
					 :parent parent
					 :label-string menu
					 :sub-menu-id submenu)))
		 (declare (ignore cb))
		 ;;-- At this point should write the code to lazily
		 ;;-- create menus and update them if the table has
		 ;;-- changed.
		 (let* ((ct (find-command-table (second item)))
			(tick (slot-value ct 'clim-internals::menu-tick)))
		   (tk::add-callback (tk::widget-parent submenu)
				     :popup-callback
				     #'(lambda (ignore)
					 (declare (ignore ignore))
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
		      (declare (ignore keystroke))
		      (let ((type (command-menu-item-type item)))
			(case type
			  (:divider)
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
				      (cb (make-instance 'tk::xm-cascade-button
							 :parent parent
							 :label-string menu
							 :sub-menu-id
							 submenu)))
				 (declare (ignore cb))
				 (setq parent submenu)))
			     (let ((button 
				    (make-instance 'tk::xm-push-button
						   :label-string menu
						   :managed t
						   :parent parent)))
			       (push (list item button)
				     commands-and-buttons)
			       
			       (when (getf (command-menu-item-options
					    item) :documentation)
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
				item)))))))
			  
		  command-table)
		 commands-and-buttons)))
      (let ((ct (slot-value sheet 'command-table)))
	(make-menu-for-command-table
	 ct mirror (not (flat-command-table-menu-p ct)))))
    mirror))

(defun display-motif-help (widget port documentation)
  (port-notify-user 
   port
   documentation
   :associated-window widget))
	     
(defmethod port-dialog-view ((port motif-port))
  +gadget-dialog-view+)
  
;;--- Should "ungray" the command button, if there is one
;;--- If the command button is visible, right now since everything is
;;--- in a menu we do not have a problem with this.

(defmethod note-command-enabled ((framem motif-frame-manager) frame command)
  (declare (ignore frame command)))

;;--- Should "gray" the command button, if there is one
(defmethod note-command-disabled ((framem motif-frame-manager) frame command)
  (declare (ignore frame command)))

(defmethod silica::update-frame-settings ((framem motif-frame-manager) (frame t))
  ;;--- Lets see how this works out
  (let ((shell (sheet-shell (frame-top-level-sheet frame))))
    (let ((sr (compose-space (frame-top-level-sheet frame))))
      (tk::set-values shell
		      :min-width (fix-coordinate (space-requirement-min-width sr))
		      :min-height
		      (fix-coordinate (space-requirement-min-height sr))))
    (let ((geo (clim-internals::frame-geometry frame)))
      (destructuring-bind
	  (&key x y width height &allow-other-keys) geo
	;;-- what about width and height
	(when (and width height)
	  (tk::set-values shell :width width :height height))
	(when (and x y)
	  (tk::set-values shell 
			  :x (fix-coordinate x)
			  :y (fix-coordinate y)))))
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
	  (when name
	    (tk::set-values shell :icon-name name))
	  (when pixmap
	    (tk::set-values shell :icon-pixmap (decode-pixmap pixmap)))
	  (when clipping-mask
	    (tk::set-values shell :clip-mask (decode-pixmap clipping-mask))))))))
