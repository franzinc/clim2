;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

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
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: framem.lisp,v 1.8 92/05/22 19:26:51 cer Exp $

(in-package :silica)

(define-protocol-class frame-manager ())

(defclass standard-frame-manager (frame-manager) 
    ((port :reader port :initarg :port)
     (frames :accessor frame-manager-frames :initform nil)))

;;--- This is most likely wrong
(defmethod graft ((framem standard-frame-manager))
  (car (port-grafts (port framem))))

(defvar *default-frame-manager* nil)

(defmacro with-frame-manager ((framem) &body body)
  `(let ((*default-frame-manager* ,framem))
     ,@body))

(defun find-frame-manager (&rest options 
			   &key port &allow-other-keys)
  (declare (dynamic-extent options))
  (unless port 
    (with-keywords-removed (options options '(:port))
      (setq port (apply #'find-port options))))
  (cond 
    ;; (find-frame-manager) -> default one
    ((and (null options) *default-frame-manager*))
    ;; We specified a port we have to make sure the default on
    ;; matches it
    ((and *default-frame-manager*
	  (frame-manager-matches-options-p
	    *default-frame-manager*
	    port options))
     *default-frame-manager*)
    ;; Failing that we make one
    (t
     (or (port-frame-manager port)
	 (setf (port-frame-manager port)
	       (make-frame-manager port))))))

(defmethod frame-manager-matches-options-p
	   ((framem standard-frame-manager) port options)
  (declare (ignore options))
  (eq (port framem) port))

(defmethod make-frame-manager ((port basic-port))
  (make-instance 'standard-frame-manager :port port))


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
			       :region (multiple-value-bind (width height)
					   (bounding-rectangle-size top-pane)
					 (make-bounding-rectangle 0 0 width height))
			       :parent (find-graft :port (port frame))))))
      (setf (frame-top-level-sheet frame) sheet
	    (frame-shell frame) (sheet-shell sheet))
      (sheet-adopt-child sheet (frame-panes frame)))))

(defmethod adopt-frame :after ((framem standard-frame-manager) frame)
  (pushnew frame (frame-manager-frames framem)))

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


(defmethod make-pane-class ((framem standard-frame-manager) class &rest options)
  (declare (ignore options))
  (second (assoc class '((scroll-bar scroll-bar-pane)
			 (slider slider-pane)
			 (push-button push-button-pane)
			 (text-field text-field-pane)
			 (toggle-button toggle-button-pane)
			 (menu-bar menu-bar-pane)
			 (viewport viewport)
			 (radio-box radio-box-pane)
			 (check-box check-box-pane)
			 (frame-pane frame-pane)
			 (top-level-sheet top-level-sheet)
			 ;; One day
			 (line-editor-pane)
			 (label-button-pane)
			 (radio-button-pane)
			 (horizontal-divider-pane)
			 (vertical-divider-pane)
			 (label-pane)
			 ;;
			 (list-pane)
			 (caption-pane)
			 (cascade-button)
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
	(apply #'make-instance 
	       abstract-type
	       :frame frame :frame-manager framem
	       options))))

(defmethod make-pane-arglist ((framem standard-frame-manager) type &rest options)
  (declare (ignore realizer type))
  (declare (non-dynamic-extent options))
  options)
