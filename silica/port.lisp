;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: port.lisp,v 1.22 92/10/28 11:30:53 cer Exp $

(in-package :silica)

"Copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
 Portions copyright (c) 1992 Symbolics, Inc.  All rights reserved."


;; Ports and grafts

(defvar *default-server-path* #+Allegro '(:motif)
			      #+Lucid '(:clx)
			      #+Genera `(:genera)
			      #+Cloe-Runtime `(:cloe)
			      #-(or Allegro Lucid Genera Cloe-Runtime) nil)

(defvar *ports* nil)

(defun map-over-ports (function)
  (declare (dynamic-extent function))
  (mapc function *ports*))

(defun map-over-grafts (function port)
  (declare (dynamic-extent function))
  (mapc function (port-grafts port)))

(defun find-port (&key (server-path *default-server-path*))
  (map-over-ports #'(lambda (port)
		      (when (port-match port server-path) 
			(return-from find-port port))))
  (make-port :server-path server-path))

(defmethod find-named-color (name (port port))
  (find-named-color name (port-default-palette port)))

#+Genera
(scl:add-initialization "Reset ports"
  '(progn
     (dolist (port *ports*)
       (destroy-port port)))
  '(before-cold))

#+Genera
(scl:add-initialization "Restart ports"
  '(progn
     (dolist (port *ports*)
       (restart-port port)))
  '(warm))

#+Allegro
(progn
  (defun reset-ports ()
    ;;--- Should this do more?
    (setq *ports* nil))
  (push `(:eval reset-ports) excl::*restart-actions*))

(defun port-match (port server-path)
  (equal (port-server-path port) server-path))

(defun make-port (&rest keys &key server-path &allow-other-keys)
  (declare (dynamic-extent keys))
  (apply #'make-instance (find-port-type (car server-path)) keys))

(defgeneric find-port-type (type))
(defmethod find-port-type (x)
  (error "Cannot find port type: ~S" x))


(defmethod initialize-instance :around ((port basic-port) &key server-path)
  (setf (slot-value port 'server-path) (copy-list server-path))
  (call-next-method)
  (setq *ports* (nconc *ports* (list port)))
  (restart-port port))


(defmethod port-pointer ((port basic-port))
  (with-slots (pointer grafts) port
    (or pointer
	(setq pointer (make-instance 'standard-pointer 
			:graft (find-graft :port port)
			:port port)))))

(defmethod (setf port-pointer) (pointer (port basic-port))
  (setf (slot-value port 'pointer) pointer))

(defgeneric port-set-pointer-position (port pointer x y))
(defgeneric port-set-pointer-cursor (port pointer cursor))

(defgeneric port (x))

(defmethod port ((port basic-port)) port)

(defmethod port ((object t)) nil)


(defgeneric port-properties (port))

(defgeneric (setf port-properties) (properties port))

(defgeneric restart-port (port))

(defmethod restart-port ((port basic-port))
  (when *multiprocessing-p*
    (when (port-process port)
      (destroy-process (port-process port)))
    (setf (port-process port)
	  (make-process #'(lambda () (port-event-loop port))
			:name (format nil "CLIM Event Dispatcher for ~A"
				(port-server-path port))))))

(defgeneric port-event-loop (port))
(defmethod port-event-loop ((port basic-port))
  (with-simple-restart (nil "Exit event loop for ~A" port)
    (loop
      (with-simple-restart (nil "Restart event loop for ~A" port)
	(loop
	  (process-next-event port))))))


(defgeneric destroy-port (port))

(defmethod destroy-port (port)
  (when (port-process port)
    (destroy-process (port-process port)))
  (dolist (framem (port-frame-managers port))
    (dolist (frame (frame-manager-frames framem))
      (disown-frame framem frame)))
  (setq *ports* (delete port *ports*)))


(defgeneric add-watcher (port watcher))
(defgeneric remove-watcher (port watcher))
(defgeneric reset-watcher (watcher how))



;;;;;;;;;;;;;;;;

(define-protocol-class graft (sheet))

(defclass standard-graft 
	  (mirrored-sheet-mixin
	   sheet-multiple-child-mixin
	   sheet-transformation-mixin
	   basic-sheet
	   graft)
    ((port :initarg :port :reader port)
     (lock :initform (make-lock "a graft lock") :reader graft-lock)
     (orientation :reader graft-orientation :initarg :orientation)
     (units :reader graft-units :initarg :units)
     (pixel-width :reader graft-pixel-width)
     (pixel-height :reader graft-pixel-height)
     (mm-width :reader graft-mm-width)
     (mm-height :reader graft-mm-height)
     (pixels-per-point :reader graft-pixels-per-point)))

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

(defgeneric realize-graft (port graft))

(defmethod graft-matches-spec ((graft standard-graft) orientation units)
  t)

(defmethod initialize-instance :after ((graft standard-graft) &key port)
  (setf (slot-value graft 'graft) graft)
  (setf (port-grafts port)
	(nconc (port-grafts port) (list graft)))
  (realize-graft port graft))

(defmethod update-mirror-region ((port basic-port) (sheet standard-graft))
  ;;--- I don't think we ever change the region of a graft...
  )

(defmethod update-mirror-transformation ((port basic-port) (sheet standard-graft))
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

(defgeneric port-graft-class (port))
(defmethod port-graft-class ((port basic-port)) 'standard-graft)


(defgeneric graft (x))

;;--- Not strictly necessary any more, since the graft points to itself
(defmethod graft ((graft standard-graft)) graft)

(defmethod graft ((object t)) nil)


(defgeneric graft-orientation (graft))
(defgeneric graft-units (graft))
(defgeneric graft-width (graft))
(defgeneric graft-height (graft))
(defgeneric graft-pixels-per-millimeter (graft))
(defgeneric graft-pixels-per-inch (graft))
