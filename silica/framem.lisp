;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :silica)

;;;"Copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
;;; Portions copyright (c) 1992 Symbolics, Inc.  All rights reserved."


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
  (or (and (null options) *default-frame-manager*) ; (find-frame-manager)
      (with-keywords-removed (new-options options '(:port :palette :server-path))
        (unless port 
          (setq port (find-port :server-path server-path)))
        (unless palette
          (setq palette (port-default-palette port)))
        (cond 
         ;; We specified a port and perhaps other options so make sure
         ;; the default framem matches it 
         ((and *default-frame-manager*
               (apply #'frame-manager-matches-options-p
                      *default-frame-manager* port :palette palette new-options))
          *default-frame-manager*)
         ;; No default, look for one in the port, or create a new one
         (t
          (dolist (framem (port-frame-managers port))
            (when (apply #'frame-manager-matches-options-p 
                         framem port :palette palette new-options)
              (return-from find-frame-manager framem)))
          (let ((framem (apply #'make-frame-manager 
                               port :palette palette new-options)))
            (setf (port-frame-managers port)
              (nconc (port-frame-managers port) (list framem)))
            framem))))))

#+Genera
(scl:add-initialization "Reset frame managers"
  '(progn
     (setq *default-frame-manager* nil))
  '(before-cold))

(defmethod make-frame-manager ((port basic-port) &key palette &allow-other-keys)
  (make-instance 'standard-frame-manager :port port :palette palette))

(defmethod frame-manager-matches-options-p
           ((framem standard-frame-manager) port &key palette &allow-other-keys)
  (and (eq (port framem) port)
       (eq (frame-manager-palette framem) palette)))

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
#-(or aclpc acl86win32) :background #-(or aclpc acl86win32) (frame-background frame)
#-(or aclpc acl86win32) :foreground #-(or aclpc acl86win32) (frame-foreground frame)
                               :text-style (frame-text-style frame)))))
      (sheet-adopt-child (find-graft :port (port frame)) sheet)
      (setf (frame-top-level-sheet frame) sheet
            (frame-shell frame) (sheet-shell sheet))
      (sheet-adopt-child sheet top-pane))))

(defmethod adopt-frame :before ((framem standard-frame-manager) frame)
  (assert (null (frame-manager frame)))
  (setf (frame-manager frame) framem))

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
                  exit-boxes name style text-style background
                  foreground x-position y-position))

(defgeneric frame-manager-select-file 
            (framem &rest options 
             &key frame associated-window title documentation exit-boxes name
                  file-search-proc directory-list-label file-list-label
                  text-style foreground background x-position y-position))


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
  (declare (ignore type) (non-dynamic-extent options))
  options)
