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
;; $fiHeader: port.lisp,v 1.10 92/04/15 11:45:18 cer Exp Locker: cer $

(in-package :silica)


;; Ports and grafts

(defvar *default-server-path* #+Allegro '(:motif)
			      #+Lucid '(:openlook)
			      #+Genera `(:genera :host ,net:*local-host*
						 :screen ,tv:main-screen)
			      #-(or Allegro Lucid Genera) nil)

(defvar *ports* nil)
(defvar *port-type-mapping* nil)

(defun find-port (&key (server-path *default-server-path*))
  (map-over-ports #'(lambda (port)
		      (when (port-match port server-path) 
			(return-from find-port port))))
  (make-port :server-path server-path))

(defun port-match (port server-path)
  (equal (port-server-path port) server-path))

(defun make-port (&rest keys &key server-path &allow-other-keys)
  (declare (dynamic-extent keys))
  (apply #'make-instance (find-port-type (car server-path)) keys))

(defgeneric find-port-type (type)
  (:method (x)
   (error "Cannot find port type: ~S" x)))


(defmethod initialize-instance :around ((port port) &key server-path)
  (setf (slot-value port 'server-path) (copy-list server-path))
  (call-next-method)
  (push port *ports*)
  (restart-port port))


(defgeneric port (x)
  ;;;-- Do we need this???
  #+ignore
  (:method ((port port)) port)
  #+ignore
  (:method ((x t)) nil))

(defgeneric port-properties (port))

(defgeneric (setf port-properties) (properties port))

(defun map-over-ports (function)
  (declare (dynamic-extent function))
  (mapc function *ports*))

(defgeneric restart-port (port))

(defmethod restart-port ((port port))
  (when (port-process port)
    (destroy-process (port-process port)))
  (setf (port-process port)
	(make-process #'(lambda () (port-event-loop port))
		      :name (format nil "CLIM Event Dispatcher for ~A"
			      (port-server-path port)))))

(defgeneric port-event-loop (port)
  (:method ((port port))
   (loop
     (process-next-event port))))


(defgeneric destroy-port (port))

(defgeneric add-watcher (port watcher))
(defgeneric remove-watcher (port watcher))
(defgeneric reset-watcher (watcher how))



;;;;;;;;;;;;;;;;

(defclass graft (sheet 
		 mirrored-sheet-mixin
		 sheet-multiple-child-mixin
		 sheet-transformation-mixin)
    ((port :initarg :port :reader port)
     (lock :initform (make-lock "a graft lock") :reader graft-lock)
     (orientation :reader graft-orientation :initarg :orientation)
     (units :reader graft-units :initarg :units)
     (pixel-width :reader graft-pixel-width)
     (pixel-height :reader graft-pixel-height)
     (mm-width :reader graft-mm-width)
     (mm-height :reader graft-mm-height)
     (pixels-per-point :reader graft-pixels-per-point)))

(defgeneric graftp (x)
  (:method ((x graft)) t)
  (:method ((x t)) nil))

(defun find-graft (&key (server-path *default-server-path*)
			(port (find-port :server-path server-path))
			(orientation :default)
			(units :device))
  (unless port
    (setq port (find-port :server-path server-path)))
  (map-over-grafts #'(lambda (graft)
		       (when (graft-matches-spec graft orientation units)
			 (return-from find-graft graft)))
		   port)
  (make-instance (port-graft-class port)
		 :port port
		 :orientation orientation 
		 :units units))

(defun graft-matches-spec (graft orientation units)
  t)

(defmethod initialize-instance :after ((graft graft) &key port)
  (pushnew graft (port-grafts port))
  (realize-graft port graft))

(defmethod update-mirror-region ((port port) (sheet graft))
  ;;--- I don't think we ever change the region of a graft...
  )

(defmethod update-mirror-transformation ((port port) (sheet graft))
  ;;--- I don't think we ever change the transformation of a graft...
  )

(defun fit-region*-in-region* (left1 top1 right1 bottom1
			       left2 top2 right2 bottom2)
  #+Genera (declare (values left1 top1 right1 bottom1 adjusted-p))
  (let* ((adjusted-p nil)
	 (w (- right1 left1))
	 (h (- bottom1 top1))
	 (ww (- right2 left2))
	 (hh (- bottom2 top2)))
    (when (> w ww)
      (let ((too-much (- w ww)))
	(decf w too-much)
	(decf right1 too-much)
	(setq adjusted-p t)))
    (when (> h hh)
      (let ((too-much (- h hh)))
	(decf h too-much)
	(decf bottom1 too-much)
	(setq adjusted-p t)))
    (when (< left1 left2)
      (let ((too-much (- left2 left1)))
	(incf left1 too-much)
	(incf right1 too-much)
	(setq adjusted-p t)))
    (when (< top1 top2)
      (let ((too-much (- top2 top1)))
	(incf top1 too-much)
	(incf bottom1 too-much)
	(setq adjusted-p t)))
    (when (> right1 right2)
      (let ((too-much (- right1 right2)))
	(decf left1 too-much)
	(decf right1 too-much)
	(setq adjusted-p t)))
    (when (> bottom1 bottom2)
      (let ((too-much (- bottom1 bottom2)))
	(decf top1 too-much)
	(decf bottom1 too-much)
	(setq adjusted-p t)))
    (values left1 top1 right1 bottom1 adjusted-p)))

(defgeneric port-graft-class (port)
  (:method ((port port)) 'graft))

(defgeneric graft (object)
  (:method ((x graft)) x))

(defun map-over-grafts (function port)
  (declare (dynamic-extent function))
  (mapc function (port-grafts port)))

(defgeneric graft-orientation (graft))
(defgeneric graft-units (graft))
(defgeneric graft-width (graft))
(defgeneric graft-height (graft))
(defgeneric graft-pixels-per-millimeter (graft))
(defgeneric graft-pixels-per-inch (graft))

