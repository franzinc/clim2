;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: frames.lisp,v 1.6 92/01/31 14:57:58 cer Exp Locker: cer $

(in-package :clim-internals)

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
;; $fiHeader: frames.lisp,v 1.6 92/01/31 14:57:58 cer Exp Locker: cer $

;; Frame protocol:
;;   adopt-frame
;;   enable-frame
;;   generate-panes
;;   layout-frame
;;   make-application-frame

(define-protocol-class application-frame ())

(defclass standard-application-frame (application-frame)
	  ;;--- Is it right for these to be SHEET accessors??
	  ((name :initarg :name :accessor frame-name)
	   (pretty-name :initarg :pretty-name :accessor frame-pretty-name)
	   (command-table :initarg :command-table 
			  :initform (find-command-table 'user-command-table)
			  :accessor frame-command-table)
	   (disabled-commands :initarg :disabled-commands :initform nil)
	   ;;--- One of  T, NIL, or a command-table; used by the menu-bar
	   (menu-bar :initarg :menu-bar :initform t)
	   (histories :initform nil)
	   (port :reader sheet-port :initarg :port)
	   (graft :reader sheet-graft :initarg :graft)
	   (frame-manager :reader frame-manager :initarg :frame-manager)
	   (panes :initarg :panes :accessor frame-panes)
	   (top-level-sheet :accessor frame-top-level-sheet :initform nil)
	   (shell :accessor frame-shell)
	   (state :initform :disowned :accessor frame-state 
		  :type (member :disowned :disabled :enabled :shrunk))
	   (top-level :initarg :top-level  :accessor frame-top-level)
	   (current-layout :initarg :default-layout :reader frame-current-layout)
	   (all-panes :initform nil)
	   (pane-constructors :initarg :pane-constructors)
	   (default-size :initform nil :initarg :default-size)
	   (menu-bar :initarg :menu-bar))
  (:default-initargs
      :menu-bar t
    :top-level 'default-frame-top-level
    :frame-manager (find-frame-manager)
    :port (find-port)
    :graft (find-graft)))

(defvar *frame-managers* nil)

(defun find-frame-manager (&rest options)
  (let ((port (apply #'find-port options)))
    (second (or (assoc port *frame-managers*)
		(car
		 (push (list port (make-frame-manager port))
		       *frame-managers*))))))

(defmethod make-frame-manager (port)
  (cerror "Do it" "Making default frame manager for ~S" port)
  (make-instance 'frame-manager :port port))
    

(defmethod initialize-instance :after ((frame standard-application-frame) 
				       &rest args &key frame-manager)
  (declare (ignore args))
  (adopt-frame frame-manager frame))

(defmethod adopt-frame ((framem frame-manager) (frame standard-application-frame))
  (generate-panes frame framem))

;; what is this???

(defmethod generate-panes ((frame standard-application-frame) (framem frame-manager))
  (setf (frame-panes frame) nil))

(defmacro define-application-frame (name superclasses slots &rest options)
  (unless superclasses (setq superclasses '(standard-application-frame))) 
  (let ((pane (second (assoc :pane options)))
	(panes (cdr (assoc :panes options)))
	(layout (cdr (assoc :layout  options)))
	(menu-bar (assoc :menu-bar options))
	(command-definer (second (or (assoc :command-definer options)
				     '(t t))))
	(command-table (second (or (assoc :command-table options)
				   '(t t))))
	(top-level (assoc :top-level options)))
    
    (when (and pane panes)
      (error "Cannot use :pane and :panes together"))
    
    (when (or (and panes (null layout))
	      (and layout (null panes)))
      (error ":layout and :panes must be used together"))
    
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
	     ,@(and layout `(:default-layout ',(car (car layout))))
	   ,@(and menu-bar `(:menu-bar ',(second menu-bar)))
	   ,@(and top-level `(:top-level ',(second top-level)))
	   ,@(and panes `(:pane-constructors
			  ,(compute-pane-constructor-code panes)))
	   ,@(and command-table
		  `(:command-table (find-command-table ',(car command-table))))))
       ,@(when command-table
	   `((define-command-table ,(first command-table)
		 ,@(cdr command-table))))
       ,@(when command-definer
	   (compute-command-definer-code name command-table))
       ,@(compute-generate-panes-code name pane panes layout))))

#+Genera
(scl:defprop define-application-frame "CLIM Application Frame" si:definition-type-name)

;; For now each application frame has its own command table named after the application
(defmacro define-frame-command (command-table-name name-and-options arguments &body body)
  #+Genera (declare (zwei:indentation 2 3 3 1))
  (multiple-value-bind (command-name command-options)
      (decode-name-and-options name-and-options command-table-name)
    `(define-command (,command-name :command-table ,command-table-name ,@command-options)
	 ,arguments
       ,@body)))

#+Genera
(scl::defprop define-frame-command define-command zwei:definition-function-spec-type)
#+Genera
(scl:defun (:property define-frame-command zwei:definition-function-spec-finder) (bp)
  (zwei:defselect-function-spec-finder (zwei:forward-sexp bp 1 t)))

(defun compute-command-definer-code (name command-table)
  (let ((command-definer (fintern "~A~A~A" `define- name '-command)))
    `((defmacro ,command-definer (command-name arguments &body body)
	`(define-frame-command ,',(first command-table)
				     ,command-name ,arguments ,@body)))))



(defun compute-generate-panes-code (name code panes layouts)
  (if panes
      (compute-complex-generate-panes-code name panes layouts)
    (compute-simple-generate-panes-code name code)))

(defun compute-simple-generate-panes-code (name code)
  (and code
       (let ((f (gensym))
	     (fm (gensym)))
	 `((defmethod generate-panes ((,f ,name) (,fm frame-manager))
	     (let ((*application-frame* ,f))
	       (setf (frame-panes ,f)
		 (frame-wrapper
		  ,f ,fm
		  (with-look-and-feel-realization (,fm ,f)
		    ,code)))))))))

(defun compute-complex-generate-panes-code (name panes layouts)
  (let ((f (gensym))
	(fm (gensym)))
    `((defmethod generate-panes ((,f ,name) (,fm frame-manager))
	(symbol-macrolet
	    ,(mapcar #'(lambda (pane-spec)
			 (destructuring-bind
			     (name code) pane-spec
			   `(,name (find-or-make-pane-named ,f
							    ',name))))
	      panes)

	  (let ((*application-frame* ,f))
	    (setf (frame-panes ,f)
	      (frame-wrapper
	       ,f ,fm
	       (with-look-and-feel-realization (,fm ,f)
		 (ecase (frame-current-layout ,f)
		   ,@(mapcar #'(lambda (layout-spec)
				 (destructuring-bind
				     (name panes) layout-spec
				   `(,name ,panes)))
			     layouts)))))))))))

(defun find-or-make-pane-named (frame name)
  (second (or (assoc name (slot-value frame 'all-panes))
	      (car (push
		    (list name 
			  (funcall (second (assoc name (slot-value frame 'pane-constructors)))
				   frame (frame-manager frame)))
		    (slot-value frame 'all-panes))))))

;; The contract of GET-FRAME-PANE is to get a pane upon which we can do normal
;; I/O operations, that is, a CLIM stream pane
(defun get-frame-pane (frame pane-name &key (errorp t))
  (let ((pane (assoc pane-name (slot-value frame 'all-panes))))
    (when pane
      (map-over-sheets #'(lambda (sheet)
			   (when (typep sheet 'clim-pane-stream-mixin)
			     (return-from get-frame-pane sheet)))
		       (second pane)))
    (when errorp
      (error "There is no pane named ~S in frame ~S" pane-name frame))))

(defun compute-pane-constructor-code (panes)
  `(list ,@(mapcar #'(lambda (pane-spec)
		      (destructuring-bind
			  (name code) pane-spec
			`(list ',name
			       #'(lambda (frame framem)
				   (with-look-and-feel-realization (framem frame)
				     ,code)))))
		  panes)))
   
(defmethod frame-wrapper ((frame t) (framem t) pane)
  pane)

(defmethod enable-frame ((frame standard-application-frame))
  (with-slots (default-size) frame
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
	     (:disowned 
	      (if default-size 
		  (values (first default-size) (second default-size))
		  (values)))
	     (:disabled
	      (bounding-rectangle-size
	       (frame-top-level-sheet frame)))))
	 (note-frame-enabled (frame-manager frame) frame))))))

(defmethod disable-frame ((frame standard-application-frame))
  (ecase (frame-state frame)
    ((:disowned :disabled))
    ((:enabled :shrunk)
     (setf (frame-state frame) :disabled)
     (note-frame-disabled (frame-manager frame) frame))))

(defmethod note-frame-enabled ((framem frame-manager) (frame standard-application-frame))
  )

(defmethod note-frame-disabled ((framem frame-manager) (frame standard-application-frame))
  )

(defmethod layout-frame ((frame standard-application-frame) &optional width height)
  (when (frame-panes frame)
    (unless (and width height)
      (let ((sr (compose-space (frame-panes frame))))
	(setq width (space-requirement-width sr)
	      height (space-requirement-height sr))))
    (allocate-space (frame-panes frame) width height)
    ;; This is quite likely not going to work
    (when (frame-top-level-sheet frame)
      (silica::resize-sheet* (frame-top-level-sheet frame)
			     width height))))

(defun title-capitalize (string)
  (let ((new-string (substitute #\Space #\- string)))
    (when (eq new-string string)
      (setq new-string (copy-seq new-string)))
    (nstring-capitalize new-string)))

(defun make-application-frame (class &rest options 
			       &key enable name pretty-name
			            width height
			       &allow-other-keys)
  (declare (dynamic-extent options))
  (let ((name (or name (format nil "~A" class))))
    (with-keywords-removed (options options '(:enable :width :height))
      (let ((frame (apply #'make-instance
			  class
			  :name name
			  :pretty-name (or pretty-name (title-capitalize name))
			  :default-size (when (or width height)
					  (list width height))
			  options)))
	(when enable 
	  (enable-frame frame))
	frame))))

(defmethod run-frame-top-level :around ((frame standard-application-frame))
  (with-simple-restart (frame-exit "Exit ~A" (frame-pretty-name frame))
    (let ((*application-frame* frame))
      (call-next-method))))

(defmethod run-frame-top-level ((frame standard-application-frame))
  (unwind-protect
      (progn
	(let ((tl (frame-top-level frame)))
	  (if (atom tl)
	      (funcall tl frame)
	    (apply (car tl) frame (cdr tl)))))
    (disable-frame frame)))

(defmethod default-frame-top-level (frame
				    &key command-parser command-unparser partial-command-parser
					 (prompt "Command: "))
  
  (unless (eq (frame-state frame) :enabled)
    (enable-frame frame))
  (loop
    (catch 'layout-changed
      (let* ((*standard-output* (or (frame-standard-output frame) *standard-output*))
	     (*query-io* (or (frame-query-io frame) *query-io*))
	     (interactor (find-frame-pane-of-type frame 'interactor-pane))
	     (*standard-input* (or interactor *standard-output*))
	     (*command-parser*
	      (or command-parser
		  (if interactor
		      #'command-line-command-parser
		      #'menu-command-parser)))
	     (*command-unparser*
	      (or command-unparser
		  #'command-line-command-unparser))
	     (*partial-command-parser*
	      (or partial-command-parser
		  (if interactor
		      #'command-line-read-remaining-arguments-for-partial-command
		      #'menu-read-remaining-arguments-for-partial-command))))
	(unless (typep *standard-input* 'excl::bidirectional-terminal-stream)
	  (assert (sheet-port *standard-input*)))
	(unless (typep *standard-output* 'excl::bidirectional-terminal-stream)
	  (assert (sheet-port *standard-output*)))
	(unless (typep *query-io* 'excl::bidirectional-terminal-stream)
	    (assert (sheet-port *query-io*)))
	(loop
	  (catch-abort-gestures ("Return to ~A command level" (frame-pretty-name frame))
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
		(execute-frame-command frame command)))))))))

(defmethod redisplay-frame-panes (frame &key force-p)
  (map-over-sheets #'(lambda (sheet)
		       (when (typep sheet 'basic-clim-interactor)
			 (redisplay-frame-pane frame sheet :force-p
					       force-p)))
		   (frame-top-level-sheet frame)))

(defun redisplay-frame-pane (frame pane &key force-p)
  (when (symbolp pane)
    (setq pane (get-frame-pane frame pane)))
  (when (pane-display-function pane)
    (let* ((ird (slot-value pane 'incremental-redisplay-p))
	   (history 
	    (and ird 
		 (stream-output-history pane)))
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
			 
(defmethod read-frame-command ((frame standard-application-frame) 
			       &key (stream *query-io*)		;frame-query-io?
			       ;; should the rest of the *command-parser*
			       ;; etc. variables be passed as keywords or bound?
			       )
  (read-command (frame-command-table frame) :stream stream))

(defmethod execute-frame-command ((frame standard-application-frame) command)
  (apply (command-name command) (command-arguments command)))

;; Generic because someone might want :BEFORE or :AFTER
(defmethod frame-exit ((frame standard-application-frame))
  (invoke-restart 'frame-exit))

;;--- This causes direct-manipulation and menu-driven applications not to
;;--- maintain histories.  Is there a better heuristic?
(defmethod frame-maintain-presentation-histories ((frame standard-application-frame))
  (not (null (find-frame-pane-of-type frame 'interactor-pane))))

(defvar *click-outside-menu-handler* nil)

(defmethod command-enabled (command-name (frame standard-application-frame))
  (with-slots (disabled-commands) frame
    (or *assume-all-commands-enabled*
	(and (not (member command-name disabled-commands))
	     (command-accessible-in-command-table-p
	       command-name (frame-command-table frame))))))

(defmethod (setf command-enabled) (enabled command-name (frame standard-application-frame))
  (with-slots (disabled-commands) frame
    (cond (enabled
	   (setf disabled-commands (delete command-name disabled-commands))
	   (note-command-enabled (frame-manager frame) frame command-name))
	  (t
	   (push command-name disabled-commands)
	   (note-command-enabled (frame-manager frame) frame command-name)))))

(defmethod note-command-enabled 
           ((framem frame-manager) (frame standard-application-frame) command)
  (declare (ignore command)))

(defmethod note-command-disabled
           ((framem frame-manager) (frame standard-application-frame) command)
  (declare (ignore command)))

;;; The contract of this is to replay the contents of STREAM within the region.
(defmethod frame-replay ((frame standard-application-frame) stream &optional region)
  (stream-replay stream region)
  (force-output stream))

;;; The contract of this is to find an "appropriate" presentation; i.e., one
;;; satisfying the input context specified by TYPE.  Everything that looks for a
;;; presentation goes through this so that applications can specialize it.
(defmethod frame-find-innermost-applicable-presentation
	   ((frame standard-application-frame) input-context stream x y)
  (find-innermost-applicable-presentation input-context stream x y))

(defmethod frame-input-context-button-press-handler
	   ((frame standard-application-frame) stream button-press-event)
  (declare (ignore stream))
  (let* ((window (event-sheet button-press-event))
	 (x (pointer-event-x button-press-event))
	 (y (pointer-event-y button-press-event))
	 (highlighted-presentation (highlighted-presentation window nil))
	 (input-context *input-context*))
    #+Allegro
    (when (and *click-outside-menu-handler*
		(output-recording-stream-p window)
		(not (region-contains-point*-p (stream-output-history window) x y)))
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

(defmethod frame-standard-output ((frame standard-application-frame))
  (or (find-frame-pane-of-type frame 'application-pane)
      (find-frame-pane-of-type frame 'interactor-pane)))

(defmethod frame-standard-input ((frame standard-application-frame))
  (or (find-frame-pane-of-type frame 'interactor-pane)
      (frame-standard-output frame)))

(defmethod frame-query-io ((frame standard-application-frame))
  (or (frame-standard-input frame)
      (frame-standard-output frame)))

(defmethod frame-error-output ((frame standard-application-frame))
  (frame-standard-output frame))

;; frame-pointer-documentation

(defmethod (setf frame-current-layout) (nv (frame standard-application-frame))
  (unless (eq (frame-current-layout frame) nv)
    (setf (slot-value frame 'current-layout) nv)
    ;; Top level sheet should loose all its child annd then we should 

    (dolist (name-and-pane (slot-value frame 'all-panes))
      (let ((sheet (second name-and-pane)))
	(when (sheet-parent sheet)
	  (sheet-disown-child (sheet-parent sheet) sheet))))
    
    (dolist (child (sheet-children (frame-top-level-sheet frame)))
      (sheet-disown-child (frame-top-level-sheet frame) child))
    
    ;; Now we want to give it some new ones
    (generate-panes frame (frame-manager frame))
    (sheet-adopt-child (frame-top-level-sheet frame) (frame-panes frame))
    (silica::clear-space-requirement-caches-in-tree (frame-panes frame))
    (multiple-value-call #'layout-frame
      frame
      (bounding-rectangle-size
       (frame-top-level-sheet frame)))
    (print 'throwing excl:*initial-terminal-io*)
    (throw 'layout-changed nil)))
		 

(defmethod reset-frame ((frame standard-application-frame) &rest ignore)
  (declare (ignore ignore))
  nil)

