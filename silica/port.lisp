;; -*- mode: common-lisp; package: silica -*-
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
;; $fiHeader$

(in-package :silica)


;; Ports and grafts

(defvar *default-server-path* '(:motif :display "localhost:0"))
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
  (:method ((port port)) port)
  (:method ((x t)) nil))

(defgeneric port-properties (port)
  )

(defgeneric (setf port-properties) (properties port)
  )

(defun map-over-ports (fn)
  (mapc fn *ports*))

(defgeneric restart-port (port)
  (:method ((port port))
	   (when (port-process port)
	     (mp::process-kill (port-process port)))
	   (setf (port-process port)
		 (mp::process-run-function "Event Dispatch"
					   'port-event-loop
					   port))))

(defgeneric port-event-loop (port)
  (:method ((port port))
	   (loop
	       (process-next-event port))))


(defgeneric destroy-port (port)
  )

(defgeneric add-watcher (port watcher)
  )

(defgeneric remove-watcher (port watcher)
  )

(defgeneric reset-watcher (watcher how)
  )

;;;;;;;;;;;;;;;;

(defclass graft (sheet 
		 mirrored-sheet-mixin
		 sheet-multiple-child-mixin
		 sheet-transformation-mixin)
  ((port :initarg :port :reader port)
   (lock :initform (mp::make-process-lock) :reader graft-lock)
   
   (orientation :reader graft-orientation :initarg :orientation)
   (units :reader graft-device :initarg :units)
   ))

(defgeneric graftp (x)
  (:method ((x graft)) t)
  (:method ((x t)) nil))

(defun find-graft (&key (port (find-port))
			(server-path *default-server-path*)
			(orientation :default)
			(units :device))
  (unless port (setq port (find-port :server-paths server-path)))
  (map-over-grafts #'(lambda (graft)
		       (when (graft-matches-spec graft
						 orientation
						 units)
			 (return-from find-graft graft)))
		   port)
  (make-instance (port-graft-class port)
		 :port port
		 :orientation  orientation 
		 :units units))

(defun graft-matches-spec (graft orientation units) t)

(defmethod initialize-instance :after ((graft graft) &key port)
  (push graft (port-grafts port))
  (realize-graft port graft))

(defgeneric port-graft-class (port)
  (:method ((port port)) 'graft))

(defgeneric graft (object)
  (:method ((x graft)) x)
  )

(defun map-over-grafts (fn port)
  (mapc fn (port-grafts port)))

(defgeneric graft-orrientation (graft)
  )

(defgeneric graft-units (graft)
  )

(defgeneric graft-widget (graft)
  )

(defgeneric graft-height (graft)
  )

(defgeneric graft-pixels-per-millimeter (graft)
  )

(defgeneric graft-pixels-per-inch (graft)
  )

