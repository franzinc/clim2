;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLX-CLIM; Base: 10; Lowercase: Yes -*-

(in-package :clx-clim)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."
;;; $fiHeader$


(defclass clx-frame-manager (standard-frame-manager) 
    ())

(defmethod make-frame-manager ((port clx-port))
  (make-instance 'clx-frame-manager :port port))

(defmethod adopt-frame :after ((framem clx-frame-manager)
			       (frame standard-application-frame))
  (when (frame-panes frame)
    (establish-wm-protocol-callbacks framem frame)))

(defmethod establish-wm-protocol-callbacks ((framem clx-frame-manager) frame)
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
;     (object-handle m)
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
;(defmethod realize-mirror :around ((port clx-port) (sheet motif-cascade-button))
;  (let ((m (call-next-method)))
;    (when (cascade-button-submenu sheet)
;      (tk::set-values m :sub-menu-id 
;		      (realize-mirror port (cascade-button-submenu sheet))))
;    m))

(defun frame-wm-protocol-callback (widget frame)
  ;; Invoked when the Wm close function has been selected
  ;; We want to queue an "event" somewhere so that we can
  ;; synchronously quit from the frame
  (distribute-event
   (sheet-port frame)
   (make-instance 'window-manager-delete-event
		  :sheet (frame-top-level-sheet frame))))


(defmethod frame-wrapper ((framem clx-frame-manager) 
			  (frame standard-application-frame) pane)
  (with-look-and-feel-realization (framem frame)
    (vertically ()
      (realize-pane 'menu-bar
		    :command-table (frame-command-table frame))
      pane)))

(defclass motif-menu-bar (xt-leaf-pane) 
	  ((command-table :initarg :command-table)))

(defmethod find-widget-class-and-initargs-for-sheet (port (sheet motif-menu-bar))
  (values 'tk::xm-menu-bar nil))

(defmethod realize-mirror :around ((port clx-port) (sheet motif-menu-bar))

  ;; This code fills the menu-bar. If top level items do not have
  ;; submenus then it creates one with a menu of its own
  
  (let ((mirror (call-next-method)))
    (labels ((make-menu-for-command-table (command-table parent top)
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
		       (let* ((submenu (make-instance
					'tk::xm-pulldown-menu
					:managed nil
					:parent parent))
			      (cb (make-instance 'tk::xm-cascade-button
						 :parent parent
						 :label-string menu
						 :sub-menu-id submenu)))
			 (declare (ignore cb))
			 (make-menu-for-command-table
			  (find-command-table (second item))
			  submenu
			  nil)))
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
			 (tk::add-callback
			  button
			  :activate-callback
			  'command-button-callback
			  (slot-value sheet 'silica::frame)
			  item)))))))
			  
	      command-table)))
      (make-menu-for-command-table
       (slot-value sheet 'command-table)
       mirror
       t))
    mirror))

(defclass presentation-event (event)
	  ((value :initarg :value :reader presentation-event-value)
	   (sheet :initarg :sheet :reader event-sheet)))

(defmethod handle-event (sheet (event presentation-event))
  (throw-highlighted-presentation
   (make-instance 'standard-presentation
		  :object (presentation-event-value event)
		  :type 'command)
   *input-context*
   (make-instance 'pointer-button-press-event
		  :sheet sheet
		  :x 0
		  :y 0
		  :modifiers 0
		  :button 256)))

(defun command-button-callback (button dunno frame item)
  (distribute-event
   (sheet-port frame)
   (make-instance 'presentation-event
		  :sheet (frame-top-level-sheet frame)
		  :value (second item))))

(defmethod port-dialog-view ((port clx-port))
  +gadget-dialog-view+)
  
;;--- Should "ungray" the command button, if there is one
(defmethod note-command-enabled ((framem clx-frame-manager) frame command)
  (declare (ignore frame command)))

;;--- Should "gray" the command button, if there is one
(defmethod note-command-disabled ((framem clx-frame-manager) frame command)
  (declare (ignore frame command)))
