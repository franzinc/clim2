;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: activities.lisp,v 1.1 92/09/22 19:42:14 cer Exp Locker: cer $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1991, 1992 Franz, Inc.  All rights reserved."



(defclass activity () 
	  ((frames :initform nil :accessor activity-frames)
	   (pretty-name :accessor activity-pretty-name)
	   (top-level-process :initform nil)
	   (active-frame :initform nil :initarg :initial-frame :accessor activity-active-frame)
	   (input-buffer :initform (make-locking-queue) :accessor activity-input-buffer)
	   (auto-select :initform nil :initarg :auto-select :accessor activity-auto-select)
	   ))

(defmethod initialize-instance :after ((activity activity) &key pretty-name)
  (setf (activity-pretty-name activity)
    (or pretty-name (title-capitalize (string (type-of activity))))))

(defmethod run-activity-top-level :around ((activity activity) &key &allow-other-keys)
  (with-simple-restart (activity-exit "Exit activity ~A" (activity-pretty-name activity))
    (loop
      (with-simple-restart (nil "~A top level" (activity-pretty-name activity))
	(loop
	  #-CCL-2
	  (return-from run-activity-top-level (call-next-method))
	  #+CCL-2
	  (let ((results (multiple-value-list (call-next-method))))
	    (return-from run-activity-top-level (values-list results))))))))

(defmethod activity-exit ((activity activity))
  (invoke-restart 'activity-exit))

(defmethod run-activity-top-level ((activity activity) &key &allow-other-keys)
  (with-slots (top-level-process) activity
    (when top-level-process
      (cerror "Bludgeon ahead, assuming the risk"
	      "The process ~S is already running the top-level function for frame ~S"
	      top-level-process activity))
    (unwind-protect
	(progn
	  (setq top-level-process (current-process))
	  (default-activity-top-level activity))
      (setq top-level-process nil))))

(defvar *activity* nil)

(defmethod default-activity-top-level ((activity activity))
  (unless (activity-active-frame activity)
    (setf (activity-active-frame activity)
          (or (car (active-frames activity))
	      (start-initial-application-frame activity))))
  (loop
    (let ((*activity* activity)
	  (*original-stream* nil)
	  (*input-wait-test* nil)
	  (*input-wait-handler* nil)
	  (*pointer-button-press-handler* nil)
	  (*numeric-argument* nil)
	  (*delimiter-gestures* nil)
	  (*activation-gestures* nil)
	  (*accelerator-gestures* nil)
	  (*accelerator-numeric-argument* nil)
	  (*input-context* nil)
	  (*accept-help* nil)
	  (*assume-all-commands-enabled* nil)
	  (*command-parser* 'command-line-command-parser)
	  (*command-unparser* 'command-line-command-unparser)
	  (*partial-command-parser*
	   'command-line-read-remaining-arguments-for-partial-command) 
	  ;; Start these out nowhere
	  ;;-- Surely these are bound by the frame top level
	  ;; (*standard-output* nil)
	  ;; (*standard-input* nil)
	  ;; (*query-io* nil)
	  (*pointer-documentation-output* nil))
      (loop
	(with-simple-restart (nil "~A top level" (activity-pretty-name activity))
	  (catch 'window-resynchronize
	    (unless (activity-active-frame activity)
	      (setf (activity-active-frame activity)
		(car (activity-frames activity))))
	    (unless (activity-active-frame activity)
	      (activity-exit activity))
	    (let* ((frame (activity-active-frame activity))
		   (*application-frame* frame)
		   (top-level (frame-top-level frame)))
	      (unwind-protect
		  (loop
		    (catch 'layout-changed
		      (with-frame-manager ((frame-manager frame))
			(setf (slot-value frame 'top-level-process)
			  (slot-value activity 'top-level-process))
			(if (atom top-level)
			    (funcall top-level frame)
			  (apply (first top-level) frame (rest top-level))))
		      (break "do something")))
		(setf (slot-value frame 'top-level-process) nil)))))))))


;;;--- 

(defclass activity-frame (standard-application-frame)
	  ((activity :initform nil :accessor frame-activity :initarg :activity)))



(defgeneric activity-quit (activity)
  (:documentation "Closes all frames and exits the top-level-loop")
  (:method ((activity activity))
   (dolist (frame (frame-manager-frames activity))
     (close-application-frame activity frame))))

(defgeneric start-initial-application-frame (activity)
  (:documentation 
   "Starts the initial application, when the activity is startet up. ~
    There is no default method, so subclasses MUST implement this."))

(defgeneric start-application-frame (activity &rest frame-options)
  (:documentation 
   "Starts an application frame and registers it in the activity. ~
    The frame is exposed, so appropriate handlers will probably make it ~
    the active frame. This function must be run inside a command to ~
    synchronize the command loop."))

(defgeneric close-application-frame (activity frame)
  (:documentation 
   "Closes the application-frame frame and de-registers it in the activity. ~
    The frame is de-exposed, so appropriate handlers will probably make another ~
    frame the active frame. This function must be run inside a command to ~
    synchronize the command loop."))

(defmethod start-application-frame ((activity activity) &rest frame-options)
  (let* ((frame (apply #'make-application-frame 
                       (append frame-options
                               (list :input-buffer (activity-input-buffer activity)
				     :activity activity)))))
    (enable-frame frame)
    (push frame (activity-frames activity))
    frame))


(defmethod close-application-frame  ((activity activity) (frame activity-frame))
  ;; do we have to take care of the input-buffer that it will not be reused by
  ;; some magic resource mechanism?
  (disable-frame frame)
  (setf (activity-frames activity) (remove frame (activity-frames activity)))
  (setf (frame-activity frame) nil)
  (when (eq frame (activity-active-frame activity))
    (if (activity-frames activity)
	(setf (activity-active-frame activity) nil)
      (activity-exit activity))))


;;;
;;; callbacks from the window manager
;;;


(defgeneric activity-frame-window-select (activity-frame)
  (:documentation 
   "This function is called when the window manager of the host's display server ~
    selects the (a?) window of the frame activity-frame. Methods can specialize ~
    on this callback to provide customized behavior -- e.g. dependent from the ~
    state of the CP throw out of the command loop or not. (see example code). ~
    The default method sets activity-frame as the active frame and throws to ~
    the tag 'window-resynchronize to restart the command loop.")
  (:method  ((frame activity-frame))
	    ;;--- We should think about exporting the cp-state and so
	    ;;--- give the the application more of a chance.
	    (let ((activity (frame-activity frame)))
	      (when (and (frame-activity frame) 
			 (activity-auto-select activity)
			 *input-buffer-empty* *reading-frame-command*)
		(select-activity-active-frame activity frame)))))

(defun select-activity-active-frame (activity frame)
  ;;-- Mark current frame in need of throwing
  (setf (activity-active-frame activity) frame)
  (throw 'window-resynchronize :window-resynchronize))

(defmethod activity-frame-window-select :around ((activity-frame activity-frame))
  (unless (eq activity-frame (activity-active-frame (frame-activity activity-frame)))
    (call-next-method)))

(defmethod receive-gesture :after ((stream top-level-sheet)
				   (gesture pointer-enter-event))
  (unless (eq (pointer-boundary-event-kind gesture) :inferior)
    (let ((frame (pane-frame stream)))
      (when (typep frame 'activity-frame)
	(activity-frame-window-select frame)))))

(defgeneric activity-frame-window-close (activity-frame)
  (:documentation
   "This function is called when the window manager of the host's display server ~
    closes the (a?) window of the frame activity-frame. Methods can specialize ~
    on this callback to provide customized behavior. ~
    The default method calls close-application-frame on the frame's activity and ~
    the frame as arguments.")
  (:method  ((frame activity-frame))
            (when (frame-activity frame)
              (close-application-frame (frame-activity frame) frame)
	      ;; we dont want throw out from here, dont we?
	      ;;-- Mark current frame in need of throwing
              ;(throw 'window-resynchronize :window-resynchronize)
              )))

(defmethod frame-exit ((frame activity-frame))
  (activity-frame-window-close frame))


(defmethod redisplay-frame-panes ((frame activity-frame) &rest options)
  ;; First display all the :accept-values panes, then display the rest.
  ;; We do this to ensure that all side-effects from :accept-values panes
  ;; have taken place.
  (apply #'redisplay-activity-panes (frame-activity frame) options))

(defmethod redisplay-activity-panes ((activity activity) &key force-p)
  (dolist (frame (activity-frames activity))
    (map-over-sheets #'(lambda (sheet)
			 (when (typep sheet 'accept-values-pane)
			   (redisplay-frame-pane frame sheet :force-p force-p)))
		     (frame-top-level-sheet frame)))
  (dolist (frame (activity-frames activity))
    (map-over-sheets #'(lambda (sheet)
			 (when (and (typep sheet 'clim-stream-pane)
				    (not (typep sheet 'accept-values-pane)))
			   (redisplay-frame-pane frame sheet :force-p force-p)))
		     (frame-top-level-sheet frame))))

(defmethod disable-frame :after ((frame activity-frame))
  (let ((activity (frame-activity frame)))
    (when (and activity (eq (activity-active-frame frame) frame))
      (setf (activity-active-frame frame) nil)
      ;;--- Need to resynchronize at this point
      ;;--- We should set flag in frame so that the default top level
      ;;--- notices this and exits or throws
      )))
