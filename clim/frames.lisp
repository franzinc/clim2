(in-package :clim)
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

;; Frame protocol:
;;   adopt-frame
;;   enable-frame
;;   generate-panes
;;   layout-frame
;;   make-application-frame

(defclass frame-manager () 
  ((port :reader port :initarg :port)))
	   


(defclass application-frame () 
  ((port :reader port :initarg :port)
   (graft :reader graft :initarg :graft)
   (frame-manager :reader frame-manager :initarg :frame-manager)
   (panes :initarg :panes :accessor frame-panes)
   (top-level-sheet :accessor frame-top-level-sheet :initform nil)
   (shell :accessor frame-shell)
   (state :initform :disowned :accessor frame-state 
	  :type (member :disowned :disabled :enabled :shrunk))
   (command-table :initarg :command-table 
		  :initform (find-command-table 'user-command-table)
		  :accessor frame-command-table)
   (top-level :initarg :top-level  :accessor frame-top-level)
   )
  (:default-initargs
   :top-level 'default-frame-top-level
   :frame-manager (find-frame-manager)
   :port (find-port)
   :graft (find-graft)))

(defmethod frame-name ((frame application-frame))
  (format nil "~A" (type-of frame)))

(defmethod frame-pretty-name ((frame application-frame))
  (frame-name frame))

(defvar *frame-managers* nil)

(defun find-frame-manager (&rest options)
  (let ((port (apply #'find-port options)))
    (second (or (assoc port *frame-managers*)
		(car
		 (push (list port (make-frame-manager port))
		       *frame-managers*))))))

(defmethod make-frame-manager (port)
  (make-instance 'frame-manager :port port))
    

(defmethod initialize-instance :after ((frame application-frame) &rest
				       x &key frame-manager)
  (adopt-frame frame-manager frame))

(defmethod adopt-frame ((framem frame-manager) (frame application-frame))
  (generate-panes frame framem))
  
(defmethod generate-panes ((frame application-frame) (framem
						      frame-manager))
  (setf (frame-panes frame) nil))



(defmacro define-application-frame (name superclasses slots &rest options)
  (unless superclasses (setq superclasses '(application-frame))) 
  (let ((pane (second (assoc :pane options)))
	(command-definer (second (or (assoc :command-definer options)
				     '(t t))))
	(command-table (second (or (assoc :command-table options)
				   '(t t))))
	(top-level (assoc :top-level options)))
    (cond ((null command-table))
	  ((symbolp command-table)
	   (setq command-table (list command-table))))
    
    (when (eq (car command-table) t)
      (setq command-table (list* name (rest command-table))))
	   
    `(progn
       (defclass ,name 
	   ,superclasses 
	   ,slots
	 (:default-initargs
	  ,@(and top-level `(:top-level ',(second top-level)))
	  ,@(and command-table
		 `(:command-table (clim-internals::find-command-table ',(car command-table))))))
       ,@(when command-table
	   `((define-command-table ,(first command-table)
					 ,@(cdr command-table))))
       ,@(when command-definer
	   (compute-command-definer-code name command-table))
       ,@(compute-generate-panes-code name pane))))

(defmacro define-frame-command (command-table-name name-and-options arguments &body body)
  (multiple-value-bind (command-name command-options)
      (decode-name-and-options name-and-options command-table-name)
    `(define-command (,command-name :command-table ,command-table-name ,@command-options)
			   ,arguments
			   ,@body)))

(defun compute-command-definer-code (name command-table)
  (let ((command-definer (fintern "~A~A~A" `define- name '-command)))
    `((defmacro ,command-definer (command-name arguments &body body)
	`(define-frame-command ,',(first command-table)
				     ,command-name ,arguments ,@body)))))



(defun compute-generate-panes-code (name code)
  (and code
       (let ((f (gensym))
	     (fm (gensym)))
	 `((defmethod generate-panes ((,f ,name) (,fm frame-manager))
	     (let ((*application-frame* ,f))
	       (setf (frame-panes ,f)
		 (frame-wrapper
		  ,f ,fm
		  (with-look-and-feel-realization 
		      (,fm ,f)
		    ,code)))))))))

(defmethod frame-wrapper ((frame t) (framem t) pane)
  pane)

(defmacro with-look-and-feel-realization ((realizer frame) &rest forms)
  `(macrolet ((silica::realize-pane (&rest foo)
			    `(realize-pane-internal ,',realizer ,',frame ,@foo)))
     ,@forms))


(defmethod enable-frame ((frame application-frame))
  (ecase (frame-state frame)
    (:enabled)
    ((:disabled :disowned)
     (let ((old (frame-state frame)))
       (setf (frame-state frame) :enabled)
       ;; IF this is a new frame then if the user specified a width
       ;; then we should be using that
       ;; IF the frame already exists then we probably should be using
       ;; the top level sheet size
       (multiple-value-call
	   #'layout-frame 
	 frame
	 (ecase old
	   (:disowned (values))
	   (:disabled
	    (bounding-rectangle-size
	     (frame-top-level-sheet frame)))))
       (note-frame-enabled (frame-manager frame) frame)))))

(defmethod disable-frame ((frame application-frame))
  (ecase (frame-state frame)
    (:disabled)
    ((:enabled :shrunk)
     (setf (frame-state frame) :disabled)
     (note-frame-disabled (frame-manager frame) frame))))

(defmethod layout-frame ((frame application-frame) &optional width height)
  (when (frame-panes frame)
    (unless (and width height)
      (let ((sr (compose-space (frame-panes frame))))
	(setq width (silica::space-req-width sr)
	      height (silica::space-req-height sr))))
    (allocate-space (frame-panes frame) width height)
    ;; This is quite likely not going to work
    (when (frame-top-level-sheet frame)
      (silica::resize-sheet* (frame-top-level-sheet frame)
			     width height))))

(defun make-application-frame (class &rest options &key enable &allow-other-keys)
  (with-rem-keywords (options options '(:enable))
		     (let ((frame (apply #'make-instance class options)))
		       (when enable (enable-frame frame))
		       frame)))

(defmethod note-frame-enabled ((framem frame-manager) frame)
  (declare (ignore frame)))

(defmethod note-frame-disabled (framem frame)
  (declare (ignore frame)))

(defmethod run-frame-top-level :around ((frame application-frame))
  (with-simple-restart (frame-exit "Exit ~A" (frame-pretty-name frame))
    (let ((*application-frame* frame))
      (call-next-method))))

(defmethod run-frame-top-level ((frame application-frame))
  (unwind-protect
      (progn
	(let ((tl (frame-top-level frame)))
	  (if (atom tl)
	      (funcall tl frame)
	    (apply (car tl) frame (cdr tl)))))
    (disable-frame frame)))

(defun command-enabled-p (command frame) t)
				
(defmethod default-frame-top-level (frame
				    &key command-parser command-unparser partial-command-parser
					 (prompt "Command: "))
  
  (unless (eq (frame-state frame) :enabled)
    (enable-frame frame))
  (let* ((*standard-output* (or (frame-standard-output frame) *standard-output*))
	 (*query-io* (or (frame-query-io frame) *query-io*))
	 (interactor (find-frame-pane-of-type frame 'interactor-pane))
	 (*standard-input* (or interactor *standard-output*))
	 (*command-parser*
	  (or command-parser
	      (if interactor
		  #'command-line-command-parser
		#'menu-only-command-parser)))
	 (*command-unparser*
	  (or command-unparser
	      #'command-line-command-unparser))
	 (*partial-command-parser*
	  (or partial-command-parser
	      (if interactor
		  #'command-line-read-remaining-arguments-for-partial-command
		#'menu-only-read-remaining-arguments-for-partial-command))))
    (loop
      (clim-utils::with-simple-abort-restart ("Abort Command")
	(redisplay-frame-panes frame)
	(when interactor
	  (fresh-line *standard-input*)
	  (if (stringp prompt)
	      (write-string prompt *standard-input*)
	    (funcall prompt *standard-input* frame)))
	(let ((command (read-frame-command frame :stream *standard-input*)))
	  (when interactor
	    (terpri *standard-input*))
	  ;; Need this check in case the user aborted out of a command menu
	  (when command
	    (execute-frame-command frame command)))))))

(defmethod redisplay-frame-panes (frame &key force-p)
  (map-over-sheets #'(lambda (sheet)
		       (when (typep sheet 'basic-clim-interactor)
			 (redisplay-frame-pane frame sheet :force-p
					       force-p)))
		   (frame-top-level-sheet frame)))

(defun redisplay-frame-pane (frame pane &key force-p)
  (when (pane-display-function pane)
    (let* ((ird (slot-value pane 'incremental-redisplay-p))
	   (history 
	    (and ird 
		 (output-recording-stream-output-record pane)))
	   (record (and history
			(> (output-record-count history) 0)
			(output-record-element history 0))))
      (cond ((and ird (or force-p (null record)))
	     (when force-p
	       (window-clear pane))
	     (updating-output (pane)
			      (invoke-pane-redisplay-function frame pane)))
	    (ird
	     (redisplay record pane))
	    (t
	     (invoke-pane-redisplay-function frame pane))))))

(defun invoke-pane-redisplay-function (frame pane)
  (let ((fn (pane-display-function pane)))
    (if (atom fn)
	(funcall fn frame pane)
      (apply (car fn) frame pane (cdr fn)))))
			 
(defun execute-frame-command (frame command)
  (declare (ignore frame))
  (apply (command-name command) (command-arguments command)))


(defun frame-find-innermost-applicable-presentation (frame
							   input-context 
							   history-window px py)
  (find-innermost-applicable-presentation 
   input-context history-window px py))

(defmethod frame-maintain-presentation-histories (frame) nil)

(defvar *click-outside-menu-handler* nil)

(defmethod frame-input-context-button-press-handler
	   (frame stream button-press-event)
  (declare (ignore stream))
  (let* ((window (event-sheet button-press-event))
	 (x (pointer-event-x button-press-event))
	 (y (pointer-event-y button-press-event))
	 (highlighted-presentation (highlighted-presentation window nil))
	 (input-context *input-context*))
    #+excl
    (when (and *click-outside-menu-handler*
		(output-recording-stream-p window)
		(not 
		 (region-contains-point*-p 
		 (output-recording-stream-output-record window) x y)))
      (funcall *click-outside-menu-handler*))
    
    (when highlighted-presentation
      ;; Unhighlight on the way out.
      ;; But only unhighlight the window that the click is from. 
      (unhighlight-highlighted-presentation window nil))
    (throw-highlighted-presentation 
      (or (and (output-recording-stream-p window)
	       (frame-find-innermost-applicable-presentation
		frame input-context window x y))
	  *null-presentation*)
      input-context
      button-press-event)))

(defun find-frame-pane-of-type (frame type)
  (map-over-sheets 
   #'(lambda (sheet)
       (when (typep  sheet type)
	 (return-from find-frame-pane-of-type sheet)))
   (frame-top-level-sheet frame)))

(defun map-over-sheets (fn sheet)
  (funcall fn sheet)
  (when (typep sheet 'silica::sheet-parent-mixin)
    (dolist (child (sheet-children sheet))
      (map-over-sheets fn child))))

(defmethod frame-standard-output (frame)
  (find-frame-pane-of-type frame 'application-pane))

(defmethod frame-standard-input (frame)
  (find-frame-pane-of-type frame 'interactor-pane))

(defmethod frame-query-io (frame)
  (or (frame-standard-input frame)
      (frame-standard-output frame)))

;; frame-query-io
;; frame-pointer-documentation


  
(defmethod read-frame-command ((frame application-frame) 
			       &key (stream *query-io*)		;frame-query-io?
			       ;; should the rest of the *command-parser*
			       ;; etc. variables be passed as keywords or bound?
			       )
  (read-command (frame-command-table frame) :stream stream))

(defmethod frame-exit ((frame application-frame))
  (invoke-restart 'frame-exit))
