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
;; $fiHeader: xm-frames.lisp,v 1.7 92/02/05 21:45:25 cer Exp Locker: cer $

(in-package :xm-silica)

;; Motif stuff

(defclass motif-frame-manager (xt-frame-manager)
	  ())

(defmethod make-frame-manager ((port motif-port))
  (make-instance ' motif-frame-manager :port port))

(defmethod adopt-frame :after ((framem motif-frame-manager) (frame standard-application-frame))
  (when (frame-panes frame)
    (establish-wm-protocol-callbacks framem frame)))

(defmethod establish-wm-protocol-callbacks ((framem motif-frame-manager) frame)
  (let ((shell (frame-shell frame)))
    (tk::set-values shell 
		    :icon-name (frame-name frame)
		    :title (frame-name frame)
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
;(defmethod realize-mirror :around ((port motif-port) (sheet motif-cascade-button))
;  (let ((m (call-next-method)))
;    (when (cascade-button-submenu sheet)
;      (tk::set-values m :sub-menu-id 
;		      (realize-mirror port (cascade-button-submenu sheet))))
;    m))

(defclass window-manager-event (event)
	  ())

(defclass wm-delete-window-event (window-manager-event)
	  ((sheet :initarg :sheet :reader event-sheet))
  )

(defmethod handle-event (sheet (event wm-delete-window-event))
  (frame-exit *application-frame*))

(defun frame-wm-protocol-callback (widget frame)
  ;; Invoked when the Wm close function has been selected
  ;; We want to queue an "event" somewhere so that we can
  ;; synchronously quit from the frame
  (distribute-event
   (sheet-port frame)
   (make-instance 'wm-delete-window-event
		  :sheet (frame-top-level-sheet frame))))





(defclass motif-menu-bar (xt-leaf-pane) 
	  ((command-table :initarg :command-table)))

(defmethod find-widget-class-and-initargs-for-sheet (port
						     (parent t)
						     (sheet motif-menu-bar))
  (values 'tk::xm-menu-bar 
	  (list :resize-height t
		:resize-width t)))

(defmethod realize-mirror :around ((port motif-port) (sheet motif-menu-bar))

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



(defmethod port-dialog-view ((port motif-port))
  +gadget-dialog-view+)
  
;;--- Should "ungray" the command button, if there is one
(defmethod note-command-enabled ((framem motif-frame-manager) frame command)
  (declare (ignore frame command)))

;;--- Should "gray" the command button, if there is one
(defmethod note-command-disabled ((framem motif-frame-manager) frame command)
  (declare (ignore frame command)))
