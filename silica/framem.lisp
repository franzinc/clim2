;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: framem.lisp,v 1.26 92/12/03 10:29:21 cer Exp $

(in-package :silica)

"Copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
 Portions copyright (c) 1992 Symbolics, Inc.  All rights reserved."


(define-protocol-class frame-manager ())

(defclass standard-frame-manager (frame-manager) 
    ((port :reader port :initarg :port)
     (frames :accessor frame-manager-frames :initform nil)
     (palette :accessor frame-manager-palette :initarg :palette)
     (dialog-view :accessor frame-manager-dialog-view :initarg :dialog-view)))

(defmethod initialize-instance :after ((framem standard-frame-manager) &key)
  (with-slots (port palette) framem
    (unless palette
      (setf palette (port-default-palette port)))))

(defmethod find-named-color (name (framem standard-frame-manager) &key (errorp t))
  (find-named-color name (frame-manager-palette framem) :errorp errorp))

;;--- This is most likely wrong, since it forces a 1-to-1 mapping
;;--- between ports and grafts
(defmethod graft ((framem standard-frame-manager))
  (find-graft :port (port framem)))

(defvar *default-frame-manager* nil)

(defmacro with-frame-manager ((framem) &body body)
  `(let ((*default-frame-manager* ,framem))
     ,@body))

(defun find-frame-manager (&rest options 
			   &key port palette (server-path *default-server-path*)
			   &allow-other-keys)
  (declare (dynamic-extent options))
  (with-keywords-removed (options options '(:port :palette :server-path))
    (unless port 
      (setq port (find-port :server-path server-path)))
    (unless palette
      (setq palette (port-default-palette port)))
    (cond 
      ;; (find-frame-manager) -> default one
      ((and (null options) *default-frame-manager*))
      ;; We specified a port, so make sure the default framem matches it
      ((and *default-frame-manager*
	    (apply #'frame-manager-matches-options-p
		   *default-frame-manager* port options))
       *default-frame-manager*)
      ;; No default, look for one in the port, or create a new one
      (t
       (dolist (framem (port-frame-managers port))
	 (when (apply #'frame-manager-matches-options-p 
		      framem port :palette palette options)
	   (return-from find-frame-manager framem)))
       (let ((framem (apply #'make-frame-manager 
			    port :palette palette options)))
	 (setf (port-frame-managers port)
	       (nconc (port-frame-managers port) (list framem)))
	 framem)))))

#+Genera
(scl:add-initialization "Reset frame managers"
  '(progn
     (setq *default-frame-manager* nil))
  '(before-cold))

(defmethod make-frame-manager ((port basic-port) &key palette &allow-other-keys)
  (make-instance 'standard-frame-manager :port port :palette palette))

(defmethod frame-manager-matches-options-p
	   ((framem standard-frame-manager) port &key palette &allow-other-keys)
  (declare (ignore palette))
  (eq (port framem) port))


(defun map-over-frames (function &key port frame-manager)
  (cond (frame-manager
	 ;; Frame manager specified, so map over all the frames for
	 ;; just this frame manager.
	 (mapc function (frame-manager-frames frame-manager)))
	(port
	 ;; Port specified, map over all of the frames for all of the
	 ;; frame managers on the port.
	 (dolist (frame-manager (port-frame-managers port))
	   (mapc function (frame-manager-frames frame-manager))))
	(t
	 ;; Map over all frames for every frame manager on every port.
	 (dolist (port *ports*)
	   (dolist (frame-manager (port-frame-managers port))
	     (mapc function (frame-manager-frames frame-manager)))))))


(defmethod adopt-frame :before ((framem standard-frame-manager) frame)
  (assert (null (frame-manager frame)))
  (setf (frame-manager frame) framem))

(defmethod adopt-frame ((framem standard-frame-manager) frame)
  (generate-panes framem frame)
  (when (frame-panes frame)
    (let* ((top-pane (frame-panes frame))
	   (sheet (with-look-and-feel-realization (framem frame)
		    (make-pane 'top-level-sheet
		      :event-queue (frame-input-buffer frame)
		      :user-specified-position-p (frame-user-specified-position-p frame)
		      :user-specified-size-p (frame-user-specified-size-p frame)
		      :region (multiple-value-bind (width height)
				  (bounding-rectangle-size top-pane)
				(make-bounding-rectangle 0 0 width height))
		      :parent (find-graft :port (port frame))))))
      (setf (frame-top-level-sheet frame) sheet
	    (frame-shell frame) (sheet-shell sheet))
      (sheet-adopt-child sheet (frame-panes frame)))))

(defmethod adopt-frame :after ((framem standard-frame-manager) frame)
  (setf (frame-manager-frames framem)
	(nconc (frame-manager-frames framem) (list frame)))
  (port-note-frame-adopted (port frame) frame))

(defmethod disown-frame ((framem standard-frame-manager) frame)
  (assert (eq (frame-manager frame) framem))
  (case (frame-state frame)
    (:enabled (disable-frame frame)))
  (let ((top (frame-top-level-sheet frame)))
    (when top
      (sheet-disown-child (sheet-parent top) top)
      (setf (frame-top-level-sheet frame) nil
	    (slot-value frame 'clim-internals::all-panes) nil
	    (frame-state frame) :disowned))))

(defmethod disown-frame :after ((framem standard-frame-manager) frame)
  (setf (frame-manager-frames framem) (delete frame (frame-manager-frames framem))
	(slot-value frame 'frame-manager) nil
	(frame-shell frame) nil
	(slot-value frame 'clim-internals::initialized-panes) nil
	(frame-panes frame) nil))

(defmethod note-frame-enabled :after ((framem standard-frame-manager) frame)
  (update-frame-settings framem frame)
  ;;--- Perhaps we want to resize the top level sheet if there is one
  (when (frame-top-level-sheet frame)
    (setf (sheet-enabled-p (frame-top-level-sheet frame)) t)))

(defmethod note-frame-disabled :after ((framem standard-frame-manager) frame)
  (setf (sheet-enabled-p (frame-top-level-sheet frame)) nil))

(defmethod note-frame-iconified :after ((framem standard-frame-manager) frame)
  (setf (frame-state frame) :shrunk))

(defmethod note-frame-deiconified :after ((framem standard-frame-manager) frame)
  (setf (frame-state frame) :enabled))

(defmethod update-frame-settings ((framem standard-frame-manager) frame)
  (declare (ignore frame))
  nil)

;;--- Should "ungray" the command button, if there is one
(defmethod note-command-enabled ((framem standard-frame-manager) frame command)
  (declare (ignore frame command)))

;;--- Should "gray" the command button, if there is one
(defmethod note-command-disabled ((framem standard-frame-manager) frame command)
  (declare (ignore frame command)))


(defgeneric frame-manager-notify-user
	    (framem message-string &rest options
	     &key frame associated-window title documentation
		  exit-boxes name style text-style))

(defgeneric frame-manager-select-file 
	    (framem &rest options 
	     &key frame associated-window title documentation exit-boxes name
		 file-search-proc directory-list-label file-list-label))


(defmethod make-pane-class ((framem standard-frame-manager) class &rest options)
  (declare (ignore options))
  (second (assoc class '((scroll-bar scroll-bar-pane)
			 (scroller-pane generic-scroller-pane)
			 (viewport viewport)
			 (menu-bar menu-bar-pane)
			 (push-button push-button-pane)
			 (toggle-button toggle-button-pane)
			 (radio-box radio-box-pane)
			 (check-box check-box-pane)
			 (slider slider-pane)
			 (top-level-sheet top-level-sheet)
			 (frame-pane frame-pane)
			 (label-pane generic-label-pane)
			 (text-field text-field-pane)
			 (text-editor text-editor-pane)
			 (list-pane generic-list-pane)
			 (option-pane generic-option-pane)
			 ;;--- Need to do these
			 (horizontal-divider-pane)
			 (vertical-divider-pane)
			 ))))

(defmethod make-pane-1 ((framem standard-frame-manager)
			frame abstract-type &rest options)
  (declare (dynamic-extent options))
  (let ((type (apply #'make-pane-class framem abstract-type options)))
    ;; If there's a mapping from the abstract type to a pane class, use it.
    ;; Otherwise just try to create a class named by the abstract pane type.
    (if type
	(apply #'make-instance type
	       :frame frame :frame-manager framem
	       (apply #'make-pane-arglist framem abstract-type options))
	(apply #'make-instance abstract-type
	       :frame frame :frame-manager framem
	       options))))

(defmethod make-pane-arglist ((framem standard-frame-manager) type &rest options)
  (declare (ignore realizer type))
  (declare (non-dynamic-extent options))
  options)
