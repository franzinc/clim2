;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-1998 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: port.lisp,v 1.42 2000/05/01 21:43:32 layer Exp $

(in-package :silica)

;;;"Copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
;;; Portions copyright (c) 1992 Symbolics, Inc.  All rights reserved."


;; Ports and grafts

(defvar *default-server-path* #+(and allegro (not microsoft-32)) '(:motif)
                              #+Lucid '(:clx)
                              #+Genera `(:genera)
                              #+Cloe-Runtime `(:cloe)
                              #+(and allegro microsoft-32) '(:aclpc)
                              #-(or allegro Lucid Genera Cloe-Runtime) nil)


(defvar *ports* nil)

(defun map-over-ports (function)
  (declare (dynamic-extent function))
  (mapc function *ports*))

(defun map-over-grafts (function port)
  (declare (dynamic-extent function))
  (mapc function (port-grafts port)))

(defun find-port (&rest initargs &key (server-path *default-server-path*) &allow-other-keys)
  (declare (dynamic-extent initargs))
  (map-over-ports #'(lambda (port)
                      (when (port-match port server-path)
                        (return-from find-port port))))
  (with-keywords-removed (initargs initargs '(:server-path))
    (apply #'make-port :server-path server-path initargs)))

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

#+allegro
(progn
  (defun reset-ports ()
    ;;--- Should this do more?
    (setq *ports* nil))
  (push `reset-ports excl::*restart-actions*))

(defun port-match (port server-path)
  (equal (port-server-path port) server-path))

(defun make-port (&rest keys &key server-path &allow-other-keys)
  (declare (dynamic-extent keys))
  (apply #'make-instance (find-port-type (car server-path)) keys))

(defgeneric find-port-type (type))
(defmethod find-port-type (x)
  (error "Cannot find port type: ~S" x))

(defgeneric port-type (port))
(defgeneric port-name (port))

(defmethod initialize-instance :around ((port basic-port) &key server-path)
  (setf (slot-value port 'server-path) (copy-list server-path))
  (call-next-method)
  (restart-port port)
  (setq *ports* (nconc *ports* (list port))))


(defmethod find-named-color (name (port basic-port) &key (errorp t))
  (find-named-color name (port-default-palette port) :errorp errorp))

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
      (make-process 
       #'(lambda () (port-event-loop port))
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
  (setf (port-process port) nil)
  (dolist (framem (port-frame-managers port))
    (dolist (frame (frame-manager-frames framem))
      (disown-frame framem frame)))
  (setq *ports* (delete port *ports*)))


(define-event-class port-terminated (window-manager-event) 
  ((condition :initarg :condition :reader port-terminated-condition)))

(defmethod port-terminated ((port basic-port) condition)
  ;;--- Should mark it as dead 
  (setq *ports* (delete port *ports*))
  (dolist (graft (port-grafts port))
    (dolist (sheet (sheet-children graft))
      (queue-event sheet (make-instance 'port-terminated 
                           :condition condition
                           :sheet sheet)))))

(defmethod port-alive-p ((port basic-port))
  (or (not *multiprocessing-p*)
      (port-process port)))

;;;;;;;;;;;;;;;;

(define-protocol-class graft (sheet))

(defgeneric graft-orientation (graft))
(defgeneric graft-units (graft))

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

(defgeneric graft-pixels-per-millimeter (graft))

(defmethod graft-pixels-per-millimeter ((graft standard-graft))
  (with-slots (mm-width pixel-width) graft
    (/ pixel-width mm-width)))

(defgeneric graft-pixels-per-inch (graft))

(defmethod graft-pixels-per-inch ((graft standard-graft))
  (with-slots (mm-width pixel-width) graft
    (* 25.4 (/ pixel-width mm-width))))

(defun find-graft (&key (server-path *default-server-path*)
                        (port (find-port :server-path server-path))
                        (orientation :default)
                        (units :device))
  ;; Simultaneous calls to this function must not result in
  ;; multiple grafts.
  (without-scheduling			
    (unless port
      (setq port (find-port :server-path server-path)))
    (map-over-grafts #'(lambda (graft)
			 (when (graft-matches-spec graft orientation units)
			   (return-from find-graft graft)))
		     port)
    (make-instance (port-graft-class port)
      :port port
      :orientation orientation
      :units units)))

(defgeneric realize-graft (port graft))

(defmethod graft-matches-spec ((graft standard-graft) orientation units)
  (declare (ignore orientation units))
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

#-(or aclpc acl86win32)
(progn
;;; ics kanji server support - we put this here for convenience
;;;--- consider putting in separate file (cim 2/26/96)

(excl:ics-target-case
(:+ics

(define-protocol-class kanji-server ())

(defclass basic-kanji-server (kanji-server)
  ((server-path :reader kanji-server-path)))

(defvar *default-kanji-server-path* '(:jserver))

(defvar *kanji-servers* nil)

(defun map-over-kanji-servers (function)
  (declare (dynamic-extent function))
  (mapc function *kanji-servers*))

(defun find-kanji-server
    (&rest initargs &key (server-path *default-kanji-server-path*)
     &allow-other-keys)
  (declare (dynamic-extent initargs))
  (map-over-kanji-servers #'(lambda (kanji-server)
                              (when (kanji-server-match kanji-server
                                                        server-path)
                                (return-from find-kanji-server kanji-server))))
  (with-keywords-removed (initargs initargs '(:server-path))
    (apply #'make-kanji-server :server-path server-path initargs)))

(defun kanji-server-match (kanji-server server-path)
  (equal (kanji-server-path kanji-server) server-path))

(defun make-kanji-server (&rest keys &key server-path &allow-other-keys)
  (declare (dynamic-extent keys))
  (apply #'make-instance (find-kanji-server-type (car server-path)) keys))

(defgeneric find-kanji-server-type (type))
(defmethod find-kanji-server-type (x)
  (error "Cannot find kanji-server type: ~S" x))

(defgeneric kanji-server-type (kanji-server))

(defmethod initialize-instance :around
           ((kanji-server basic-kanji-server) &key server-path)
  (setf (slot-value kanji-server 'server-path) (copy-list server-path))
  (call-next-method)
  (setq *kanji-servers* (nconc *kanji-servers* (list kanji-server))))

))) ;; ics-target-case
