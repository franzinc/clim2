;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: activities.lisp,v 1.11 93/03/19 09:43:12 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1992 Franz, Inc.  All rights reserved."


;;; Activities

;; An activity acts like both an application frame and a frame manager
;;--- Implement other relevant frame and frame-manager methods
;;--- What about methods to choose the default values for the standard streams?
(defclass activity (application-frame)
    ((frames :initform nil :accessor frame-manager-frames)
     (pretty-name :accessor frame-pretty-name)
     (top-level-process :initform nil)
     (active-frame :initform nil :initarg :initial-frame :accessor activity-active-frame)
     (input-buffer :initform (make-locking-queue) :accessor frame-input-buffer)
     (auto-select :initform nil :initarg :auto-select :accessor activity-auto-select)
     (frame-manager :initform nil :initarg :frame-manager
		    :accessor frame-manager)))

(defmethod initialize-instance :after ((activity activity) &key pretty-name)
  (setf (frame-pretty-name activity)
	(or pretty-name (title-capitalize (string (frame-name activity)))))
  (unless (frame-manager activity)
    (setf (frame-manager activity) (find-frame-manager))))

(defmethod frame-name ((activity activity))
  (type-of activity))

(defmethod run-frame-top-level :around ((activity activity) &key)
  (handler-bind ((frame-exit
		   #'(lambda (condition)
		       (let ((exit-frame (frame-exit-frame condition)))
			 (when (eq activity exit-frame)
			   (return-from run-frame-top-level nil))))))
    (loop
      (with-simple-restart (nil "~A top level" (frame-pretty-name activity))
	(loop
	  #-CCL-2
	  (return-from run-frame-top-level (call-next-method))
	  #+CCL-2
	  (let ((results (multiple-value-list (call-next-method))))
	    (return-from run-frame-top-level (values-list results))))))))

(defmethod run-frame-top-level ((activity activity) &key)
  (with-slots (top-level-process) activity
    (when top-level-process
      (cerror "Bludgeon ahead, assuming the risk"
	      "The process ~S is already running the top-level function for frame ~S"
	      top-level-process activity))
    (unwind-protect
	(progn
	  (setq top-level-process (current-process))
	  (default-frame-top-level activity))
      (setq top-level-process nil))))

(defmethod frame-exit ((activity activity))
  (signal 'frame-exit :frame activity))

(defmethod default-frame-top-level ((activity activity) &key &allow-other-keys)
  (if (frame-manager-frames activity)
      (progn
	(setf (activity-active-frame activity)
	      (select-activity-initial-frame activity))
	(enable-activity-frames activity))
      (setf (activity-active-frame activity)
	    (prog1 (start-initial-application-frame activity)
		   (start-other-application-frames activity))))
  (unwind-protect
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
	      ;;--- Surely these are bound by the frame top level
	      ;; (*standard-output* nil)
	      ;; (*standard-input* nil)
	      ;; (*query-io* nil)
	      (*pointer-documentation-output* nil))
	  (loop
	    (with-simple-restart (nil "~A top level" (frame-pretty-name activity))
	      (catch 'window-resynchronize
		(unless (activity-active-frame activity)
		  (setf (activity-active-frame activity)
			(select-next-active-frame activity)))
		(unless (activity-active-frame activity)
		  (frame-exit activity))
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
			  ;;--- Well, what *are* we supposed to do here?
			  (break "do something")))
		    (setf (slot-value frame 'top-level-process) nil))))))))
    (disable-activity-frames activity)))

(defmethod select-activity-initial-frame ((activity activity))
  (first (frame-manager-frames activity)))

(defmethod select-next-active-frame ((activity activity))
  (and (activity-auto-select activity)
       (first (frame-manager-frames activity))))
		
(defmethod disable-activity-frames ((activity activity))
  (mapc #'disable-frame (frame-manager-frames activity)))

(defmethod enable-activity-frames ((activity activity))
  (mapc #'enable-frame (frame-manager-frames activity)))

;; Closes all of the frames in the activity and exits the activity's
;; top level loop
(defmethod activity-quit ((activity activity))
  (dolist (frame (frame-manager-frames activity))
    (stop-application-frame activity frame)))


;;; Application frames within an activity

;; An application frame that participates in an activity
(defclass activity-frame (standard-application-frame)
    ((activity :initform nil :accessor frame-activity :initarg :activity)))

(defmethod frame-top-level-process ((frame activity-frame))
  (let ((act (frame-activity frame)))
    (and act (slot-value act 'top-level-process))))

(defmethod initialize-instance :after ((frame activity-frame) &key activity)
  (assert activity () "The activity frame ~S requires an activity" frame))

(defclass secondary-activity-frame (activity-frame)
    ())

(defmethod initialize-instance :after ((frame secondary-activity-frame) &key activity)
  (assert (frame-manager-frames activity) ()
	  "Other frames must be created before secondary activity frames"))


;; Starts an application frame and registers it in the activity
(defmethod start-application-frame ((activity activity) frame-name &rest frame-options)
  (declare (dynamic-extent frame-options))
  (let* ((frame (apply #'make-application-frame frame-name
		       :input-buffer (frame-input-buffer activity)
		       :activity activity
		       :frame-manager (frame-manager activity)
                       frame-options)))
    (enable-frame frame)
    (push frame (frame-manager-frames activity))
    frame))

;; Closes the application-frame frame and un-registers it from the activity.
(defmethod stop-application-frame  ((activity activity) (frame activity-frame))
  ;; Do we have to take care of the input buffer that it will not be
  ;; reused by some magic resource mechanism?
  (disable-frame frame)
  (setf (frame-manager-frames activity) 
	(delete frame (frame-manager-frames activity)))
  (setf (frame-activity frame) nil)
  (when (eq frame (activity-active-frame activity))
    (if (frame-manager-frames activity)
	(setf (activity-active-frame activity) nil)
	(frame-exit activity)))
  (throw 'window-resynchronize :window-resynchronize))

;; Starts the initial application, when the activity is started up.
;; There is no default method, so subclasses must implement this.
(defgeneric start-initial-application-frame (activity))

(defmethod start-other-application-frames ((activity activity))
  nil)



;;; Callbacks from the window manager

;; This function is called when the window manager of the host's display
;; server selects the top-level window of the frame activity-frame.
;; Methods can specialize on this callback to provide customized behavior
;; -- e.g.  dependent from the state of the CP throw out of the command
;; loop or not (see example code).  The default method sets activity-frame
;; as the active frame and throws to the tag 'window-resynchronize to
;; restart the command loop.
(defmethod activity-frame-window-select ((frame activity-frame))
  ;;--- We should think about exporting the cp-state and so
  ;;--- give the the application more of a chance.
  (let ((activity (frame-activity frame)))
    (when (and (frame-activity frame) 
	       (activity-auto-select activity)
	       *input-buffer-empty*
	       *reading-frame-command*)
      (select-activity-active-frame activity frame))))

(defmethod activity-frame-window-select :around ((activity-frame activity-frame))
  (unless (let ((activity (frame-activity activity-frame)))
	    (and activity
		 (eq activity-frame (activity-active-frame activity))))
    (call-next-method)))

(defun select-activity-active-frame (activity frame)
  ;;--- Mark current frame in need of throwing
  (setf (activity-active-frame activity) frame)
  (throw 'window-resynchronize :window-resynchronize))

(defmethod receive-gesture :after 
	   ((stream top-level-sheet) (gesture pointer-enter-event))
  (unless (eq (pointer-boundary-event-kind gesture) :inferior)
    (let ((frame (pane-frame stream)))
      (when (typep frame 'activity-frame)
	(activity-frame-window-select frame)))))

;; This function is called when the window manager of the host's display
;; server closes the top-level window of the frame activity-frame.  Methods
;; can specialize on this callback to provide customized behavior.  The
;; default method calls STOP-APPLICATION-FRAME on the frame's activity and
;; the frame as arguments.
(defmethod activity-frame-window-close ((frame activity-frame))
  (when (frame-activity frame)
    (stop-application-frame (frame-activity frame) frame)
    ;; we dont want throw out from here, dont we?
    ;;--- Mark current frame in need of throwing
    #+++ignore (throw 'window-resynchronize :window-resynchronize)))

;; Exit from just this frame, not the whole activity
(defmethod frame-exit ((frame activity-frame))
  (activity-frame-window-close frame))

(defmethod redisplay-frame-panes ((frame activity-frame) &key force-p)
  ;; First display all the :accept-values panes, then display the rest.
  ;; We do this to ensure that all side-effects from :accept-values panes
  ;; have taken place.
  (let ((activity (frame-activity frame)))
    (dolist (frame (frame-manager-frames activity))
      (map-over-sheets #'(lambda (sheet)
			     (when (typep sheet 'accept-values-pane)
			       (redisplay-frame-pane frame sheet :force-p force-p)))
			 (frame-top-level-sheet frame)))
    (dolist (frame (frame-manager-frames activity))
      (map-over-sheets #'(lambda (sheet)
			     (when (and (typep sheet 'clim-stream-pane)
					(not (typep sheet 'accept-values-pane)))
			       (redisplay-frame-pane frame sheet :force-p force-p)))
			 (frame-top-level-sheet frame))))
  ;; Once we've redisplayed everything, the layout is done changing
  (setq *frame-layout-changing-p* nil))


(defmethod disable-frame :after ((frame activity-frame))
  (let ((activity (frame-activity frame)))
    (when (and activity (eq (activity-active-frame activity) frame))
      (setf (activity-active-frame activity) nil)
      ;;--- Need to resynchronize at this point
      ;;--- We should set flag in frame so that the default top level
      ;;--- notices this and exits or throws
      )))


(defmethod destroy-activity ((activity activity))
  ;;-- Need it do anything else?
  (mapc #'destroy-frame (frame-manager-frames activity)))
