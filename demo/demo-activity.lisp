;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;; $fiHeader: demo-activity.lisp,v 1.2 92/10/02 15:20:36 cer Exp Locker: cer $

(in-package :clim-demo)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1992 Franz, Inc.  All rights reserved."


(defclass demo-activity (activity) ())

(define-application-frame demo-app (activity-frame)
    ()
  (:panes
    (interactor :interactor)
    (display :application))
  (:pointer-documentation t)
  (:layouts
    (default 
      (vertically () (1/2 interactor) (:fill display)))))

(define-demo-app-command (com-activity-exit :menu t :name "Exit")
    ()
  (activity-frame-window-close *application-frame*))

(defvar *untitled-count* 1
  "Counter for naming new, untitled document windows")

;;; This method must be implemented by subclasses of activity
(defmethod start-initial-application-frame ((activity demo-activity))
  (start-application-frame activity
                           'demo-app 
                           :width 300 :height 250
                           :pretty-name "Initial"))

(define-demo-app-command (com-new :menu t :name t)
    ((name 'string 
	   :prompt "Name of Window"
	   :default (format nil "Untitled-~A" *untitled-count*)
	   :display-default t
	   :documentation "A Title String"))
  (start-application-frame (frame-activity *application-frame*)
                           'demo-app
                           :width 300 :height 250
                           :pretty-name name)
  (incf *untitled-count*))

(define-demo-app-command (com-select-frame :name t :menu t) 
    ((frame `((member ,@(frame-manager-frames *activity*)) 
	      ;;-- After a frame is deleted it ends not being a member
	      ;;-- of the presentation type so you get bogus
	      ;;-- presentations and  the find in the present method
	      ;;-- returns nil
	      :name-key ,#'(lambda (x) (and x (frame-pretty-name x))))))
  (select-activity-active-frame *activity* frame))

(define-demo-app-command (com-funky-add :name t :menu t) 
    ((n 'number))
  (flet ((print-random-sum (stream n)
           (let ((sum (+ n (random 20) -10)))
             (with-output-as-presentation (stream sum 'number)
               (format stream "~%~A plus ~A gives ~A" n (- sum n) sum)))))
    (print-random-sum (get-frame-pane *application-frame* 'display) n)))

;; This one gives a nicer pointer doc than the default one by  the
;; :gesture option of define-command
(define-presentation-to-command-translator add-number
    (number com-funky-add demo-app
     :pointer-documentation ((stream object)
			     (format stream "Add something to ~A" object)))
    (object)
  `(,object))


(defvar *activity-demos* nil)

(defun do-activity-demo (&key (port (find-port)) (force nil))
  (let* ((framem (find-frame-manager :port port))
	 (frame 
	   (let* ((entry (assoc port *activity-demos*))
		  (frame (cdr entry)))
	     (when (or force (null frame))
	       (setq frame (make-instance 'demo-activity
			     :frame-manager framem)))
	     (if entry 
		 (setf (cdr entry) frame)
		 (push (cons port frame) *activity-demos*))
	     frame)))
    (run-frame-top-level frame)))

(define-demo "Activity Demo" do-activity-demo)
