(in-package :xm-silica)
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

(defclass motif-frame-manager (frame-manager) 
	  ())

(defmethod make-frame-manager ((port motif-port))
  (make-instance ' motif-frame-manager :port port))

(defmethod adopt-frame ((framem motif-frame-manager) (frame application-frame))
  (call-next-method)
  (when (frame-panes frame)
    (let* ((top-pane (frame-panes frame))
	   (sheet (make-instance 'motif-top-level-sheet 
				 :region (make-bounding-rectangle
					  0 0 (sheet-width
					       top-pane) 
					  (sheet-height top-pane))
				 :parent (find-graft))))
      (setf (frame-top-level-sheet frame) sheet
	    (frame-shell frame) (sheet-shell sheet))
      (adopt-child sheet (frame-panes frame))
      (establish-wm-protocol-callbacks frame))))

(defmethod note-frame-enabled :after ((framem motif-frame-manager) frame)
  ;; Perhaps we want to resize the top leve sheet if there is one
  (when (frame-top-level-sheet frame)
    (setf (sheet-enabled-p (frame-top-level-sheet frame)) t)))

(defmethod note-frame-disabled :after ((framem motif-frame-manager) frame)
  (setf (sheet-enabled-p (frame-top-level-sheet frame)) nil))

;;; Definitions of the individual classes

;;; Main window

(defclass motif-main-window (motif-composite-pane)
  (
   (contents :accessor main-window-contents :initform nil)
   (command-window :initform nil :accessor main-window-command-window)
   command-window-location
   menu-bar
   message-window
   ))

(defmethod initialize-instance :after ((mw motif-main-window)
				       &key
				       command-window
				       command-window-location
				       menu-bar
				       message-window
				       contents)
  (when contents
    (setf (main-window-contents mw) contents))
  (when command-window
    (setf (main-window-command-window mw) command-window))
  (when message-window
    (setf (main-window-message-window mw) message-window)))

(defmethod (setf main-window-contents) :after (nv (mw motif-main-window))
  (unless (sheet-parent nv)
    (adopt-child mw nv)))

(defmethod (setf main-window-command-window) :after (nv (mw motif-main-window))
  (unless (sheet-parent nv)
    (adopt-child mw nv)))

#+ignore
(defmethod realize-mirror :around (port (motif-main-window))
  (let ((m (call-next-method)))
    (tk::xm_set_main_window_areas
     (object-handle m)
     .. 
     (and command-window (realize-mirror port command-window))
     ..
     (and message-window (realize-mirror port message-window))
     ...)))

;;; Drawing area

#+ignore
(defclass motif-drawing-area (motif-composite-pane) ())

;;; Row colum

(defclass motif-row-colum (motif-composite-pane) ())

;;; Push button

#+ignore
(defclass motif-push-button (motif-leaf-pane) ())

;;; Menu bar

(defclass motif-menu-bar (motif-composite-pane) ())
(defclass motif-pulldown-menu (motif-composite-pane) ())

(defclass motif-cascade-button (motif-composite-pane)
  ((submenu :accessor cascade-button-submenu :initform nil))
  )

(defmethod realize-mirror :around ((port motif-port) (sheet motif-cascade-button))
  (let ((m (call-next-method)))
    (when (cascade-button-submenu sheet)
      (tk::set-values m :sub-menu-id 
		      (realize-mirror port (cascade-button-submenu sheet))))
    m))

  

#+ignore
(define-application-frame motif-example-frame ()
  :pane (make-instance 'motif-main-window
		       ))

#|

(defmethod compose-space ((sheet motif-pane))
  (multiple-value-bind
      (width height)
      (tk::get-values (sheet-mirror sheet) :width :height)
    (make-instance 'space-req
		   :width width
		   :height height)))

(make-instance 'motif-main-window
	       :parent ..
	       :contents ...)

;; What about all the readers/writers that are applicable?
;; We have to trampoline those?

(setf (sheet-resource sheet :x) value)

Can the basic motif stuff support lazy realization?

|#

#+ignore
(define-motif-sheet motif-menu-bar tk::xm-menu-bar)


;; Leaf panes



;;; definition of the composite pane stuff

;;; So once we have done all this we will be able to
;;; define-application-frames using motif widgets
;;; Still need a stream pane though

;;; These frames use the Motif layout technology but perhaps we can
;;; plug in CLIM style stuff too.

;;; Need a pane we can draw on.

;;; Perhaps a :resources initarg to specify resources

;;; Do scrollbars come from the use of the scrolling macro.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; How do we interface to motif UIL.
;;; Perhaps use the name of the widget??
;;; We can query the UIL file to obtain the widget hiearchy.

;;; Gadget callbacks are not interfaced to the CP.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Final step is mapping abstract gadgets onto motif gadgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmethod establish-wm-protocol-callbacks (frame)
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

(defun frame-wm-protocol-callback (widget frame)
  ;; Invoked when the Wm close function has been selected
  ;; We want to queue an "event" somewhere so that we can
  ;; synchronously quit from the frame
  )



(defmethod clim::frame-wrapper ((frame t) (framem motif-frame-manager) pane)
  (clim-internals::with-look-and-feel-realization
      (framem frame)
    (silica::vertically ()
			(silica::realize-pane 'silica::menubar
					      :command-table
					      (clim-internals::frame-command-table frame))
			pane)))

(defclass motif-menubar (motif-leaf-pane) 
	  ((command-table :initarg :command-table)))

(defmethod find-widget-class-and-initargs-for-sheet (port (sheet motif-menubar))
  (values 'tk::xm-menu-bar nil))

(defmethod realize-mirror :around ((port motif-port) (sheet
						      motif-menubar))

  ;; This code fills the menubar. If top level items do not have
  ;; submenus then it creates one with a menu of its own
  
  (let ((mirror (call-next-method)))
    (labels ((make-menu-for-command-table (command-table parent top)
	     (clim::map-over-command-table-menu-items
	      #'(lambda (menu keystroke item)
		  (declare (ignore keystroke))
		  (let ((type (clim::command-menu-item-type item)))
		    (case type
		      (:divider)
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
			  (clim-internals::find-command-table
			   (second item))
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
	   (sheet :initarg :sheet :reader silica::event-sheet)))

(defmethod silica::handle-event (sheet (event presentation-event))
  (clim-internals::throw-highlighted-presentation
   (make-instance 'clim-internals::standard-presentation
		  :object (presentation-event-value event)
		  :type 'clim::command)
   clim::*input-context*
   (make-instance 'silica::pointer-event
		  :sheet sheet
		  :x 0
		  :y 0
		  :modifiers 0
		  :button 256)))

(defun command-button-callback (button dunno frame item)
  (distribute-event
   (port frame)
   (make-instance 'presentation-event
		  :sheet (frame-top-level-sheet frame)
		  :value (second item))))
