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
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: classes.lisp,v 1.3 92/01/31 14:55:26 cer Exp $

(in-package :silica)

(define-protocol-class frame-manager ())

(defclass standard-frame-manager (frame-manager) 
    ((port :reader sheet-port :initarg :port)
     (frames :accessor frame-manager-frames :initform nil)))

(defvar *frame-managers* nil)

(defun find-frame-manager (&rest options)
  (declare (dynamic-extent options))
  (let ((port (apply #'find-port options)))
    (second (or (assoc port *frame-managers*)
		(car (push (list port (make-frame-manager port))
			   *frame-managers*))))))

(defmethod make-frame-manager (port)
  (cerror "Make a default frame manager"
	  "Couldn't find a frame manager for the port ~S" port)
  (make-instance 'standard-frame-manager :port port))


;;--- We need a "null" frame manager that creates a menu-bar that ends
;;--- up calling DISPLAY-COMMAND-MENU.  This can't happen for on
;;--- STANDARD-FRAME-MANAGER, because that will break those port that
;;--- have real menu bars (and do the displaying in REALIZE-MIRROR).
(defmethod frame-wrapper ((framem standard-frame-manager) frame pane)
  (declare (ignore frame))
  pane)

(defmethod adopt-frame ((framem standard-frame-manager) frame)
  (generate-panes framem frame)
  (when (frame-panes frame)
    (let* ((top-pane (frame-panes frame))
	   (sheet (with-look-and-feel-realization (framem frame)
		    (realize-pane 'top-level-sheet 
				  :region (multiple-value-bind (width height)
					      (bounding-rectangle-size top-pane)
					    (make-bounding-rectangle 
					      0 0 width height))
				  :parent (find-graft)))))
      (setf (frame-top-level-sheet frame) sheet
	    (frame-shell frame) (sheet-shell sheet))
      (sheet-adopt-child sheet (frame-panes frame)))))

(defmethod adopt-frame :after ((framem standard-frame-manager) frame)
  (pushnew frame (frame-manager-frames framem)))

(defmethod disown-frame :after ((framem standard-frame-manager) frame)
  (setf (frame-manager-frames framem)
	(delete frame (frame-manager-frames framem))))

(defmethod note-frame-enabled :after ((framem standard-frame-manager) frame)
  ;; Perhaps we want to resize the top level sheet if there is one
  (when (frame-top-level-sheet frame)
    (setf (sheet-enabled-p (frame-top-level-sheet frame)) t)))

(defmethod note-frame-disabled :after ((framem standard-frame-manager) frame)
  (setf (sheet-enabled-p (frame-top-level-sheet frame)) nil))

