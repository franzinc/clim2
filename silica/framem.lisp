;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: framem.lisp,v 1.12 92/07/27 19:29:24 cer Exp $

(in-package :silica)

"Copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
 Portions copyright (c) 1992 Symbolics, Inc.  All rights reserved."


(define-protocol-class frame-manager ())

(defclass standard-frame-manager (frame-manager) 
    ((port :reader port :initarg :port)
     (frames :accessor frame-manager-frames :initform nil)
     (dialog-view :accessor frame-manager-dialog-view :initarg :dialog-view)))

;;--- This is most likely wrong
(defmethod graft ((framem standard-frame-manager))
  (car (port-grafts (port framem))))

(defvar *default-frame-manager* nil)

(defmacro with-frame-manager ((framem) &body body)
  `(let ((*default-frame-manager* ,framem))
     ,@body))

(defun find-frame-manager (&rest options 
			   &key port (server-path *default-server-path*)
			   &allow-other-keys)
  (declare (dynamic-extent options))
  (with-keywords-removed (options options '(:port :server-path))
    (unless port 
      (setq port (find-port :server-path server-path)))
    (cond 
      ;; (find-frame-manager) -> default one
      ((and (null options) *default-frame-manager*))
      ;; We specified a port we have to make sure the default on
      ;; matches it
      ((and *default-frame-manager*
	    (apply #'frame-manager-matches-options-p
		   *default-frame-manager* port options))
       *default-frame-manager*)
      ;; Failing that we make one
      (t
       (let ((framem (port-frame-manager port)))
	 (if (and framem
		  (apply #'frame-manager-matches-options-p framem port options))
	     framem
	     (setf (port-frame-manager port)
		   (apply #'make-frame-manager port options))))))))

#+Genera
(scl:add-initialization "Reset frame managers"
  '(progn
     (setq *default-frame-manager* nil))
  '(before-cold))

(defmethod make-frame-manager ((port basic-port) &key)
  (make-instance 'standard-frame-manager :port port))

(defmethod frame-manager-matches-options-p
	   ((framem standard-frame-manager) port &key)
  (eq (port framem) port))


;; Things like the Genera and CLX frame managers create a CLIM stream pane
;; that simply use DISPLAY-COMMAND-MENU.
(defmethod frame-wrapper ((framem standard-frame-manager) frame pane)
  (declare (ignore frame))
  pane)

(defmethod adopt-frame ((framem standard-frame-manager) frame)
  (generate-panes framem frame)
  (when (frame-panes frame)
    (let* ((top-pane (frame-panes frame))
	   (sheet (with-look-and-feel-realization (framem frame)
		    (make-pane 'top-level-sheet
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
	(nconc (frame-manager-frames framem) (list frame))))

(defmethod disown-frame ((framem standard-frame-manager) frame)
  (let ((top (frame-top-level-sheet frame)))
    (when top
      (sheet-disown-child (sheet-parent top) top)
      (setf (frame-top-level-sheet frame) nil
	    (frame-state frame) :disowned))))

(defmethod disown-frame :after ((framem standard-frame-manager) frame)
  (setf (frame-manager-frames framem) (delete frame (frame-manager-frames framem))
	(slot-value frame 'frame-manager) nil))

(defmethod note-frame-enabled :after ((framem standard-frame-manager) frame)
  (update-frame-settings framem frame)
  ;;--- Perhaps we want to resize the top level sheet if there is one
  (when (frame-top-level-sheet frame)
    (setf (sheet-enabled-p (frame-top-level-sheet frame)) t)))

(defmethod note-frame-disabled :after ((framem standard-frame-manager) frame)
  (setf (sheet-enabled-p (frame-top-level-sheet frame)) nil))

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
	     &key frame associated-window title documentation exit-boxes name style))

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
			 ;;--- Some day...
			 (list-pane)
			 (cascade-button)
			 (text-field text-field-pane)
			 (text-editor text-editor-pane)
			 (horizontal-divider-pane)
			 (vertical-divider-pane)
			 (label-button-pane)
			 (radio-button-pane)
			 (caption-pane)
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
