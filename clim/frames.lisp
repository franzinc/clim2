;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: frames.lisp,v 1.18 92/05/06 15:37:38 cer Exp Locker: cer $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 International Lisp Associates.
 Portions copyright (c) 1991, 1992 Franz, Inc.  All rights reserved."

(define-protocol-class application-frame ())

;;--- We should add an input event-queue to frames, so that other
;;--- processes can queue up requests.  This queue should be managed
;;--- like other event queues.  It can contains "command" events, too.
(defclass standard-application-frame (application-frame)
    ;;--- Is it right for these to be SHEET accessors??
    ((name :initarg :name :accessor frame-name)
     (pretty-name :initarg :pretty-name :accessor frame-pretty-name)
     (command-table :initarg :command-table 
		    :initform (find-command-table 'user-command-table)
		    :accessor frame-command-table)
     (disabled-commands :initarg :disabled-commands :initform nil)
     ;; One of  T, NIL, or a command-table; used by the menu-bar
     (menu-bar :initarg :menu-bar :initform nil)
     (histories :initform nil)
     (frame-manager :reader frame-manager)
     (calling-frame :reader frame-calling-frame :initarg :calling-frame)
     ;; PANES is the description of all of the named panes,
     ;; ALL-PANES is an alist that stores all of the named panes that
     ;; have actually been realized so far
     (panes :initarg :panes :accessor frame-panes)
     (all-panes :initform nil)
     (current-panes :initform nil :accessor frame-current-panes)
     (initialized-panes :initform nil)
     (pane-constructors :initarg :pane-constructors)
     (top-level-sheet :accessor frame-top-level-sheet :initform nil)
     (state :initform :disowned :accessor frame-state 
	    :type (member :disowned :disabled :enabled :shrunk))
     (top-level :initarg :top-level  :accessor frame-top-level)
     (current-layout :initarg :default-layout :initform nil
		     :reader frame-current-layout)
     (geometry :initform nil :initarg :geometry :reader frame-geometry)
     (icon :initform nil :initarg :icon :reader frame-icon)
     (shell :accessor frame-shell)
     (pointer-documentation-p :initarg :pointer-documentation
			      :reader frame-pointer-documentation-p)
     (pointer-documentation-pane :initform nil)
     (properties :initform nil :initarg :properties
		 :accessor frame-properties)
     (resizable :initarg :resize-frame
		:reader frame-resizable))
     (:default-initargs :pointer-documentation nil
       :resize-frame nil
       :top-level 'default-frame-top-level))

(defmethod port ((frame standard-application-frame))
  (port (frame-manager frame)))

(defmethod graft ((frame standard-application-frame))
  (graft (frame-manager frame)))

#+CLIM-1-compatibility
(define-compatibility-function (frame-top-level-window frame-top-level-sheet)
			       (frame)
  (frame-top-level-sheet frame))

(defmethod initialize-instance :after ((frame standard-application-frame) 
				       &rest args
				       &key frame-manager
					    geometry
					    icon
					    (parent frame-manager))
  (declare (ignore args))
  
  (destructuring-bind (&key x y width height) geometry
    (declare (ignore x y width height)))
  (destructuring-bind (&key name pixmap clipping-mask) icon
    (declare (ignore name pixmap clipping-mask)))
  
  (let ((frame-manager
	  (etypecase parent
	    (null (find-frame-manager))
	    (list (apply #'find-frame-manager parent))
	    (frame-manager parent)
	    (application-frame (frame-manager parent))
	    (port (find-frame-manager :port parent))
	    (sheet (frame-manager (pane-frame parent))))))
    (setf (slot-value frame 'frame-manager) frame-manager)
    (adopt-frame frame-manager frame)))

;; Default method does nothing
(defmethod generate-panes ((framem standard-frame-manager)
			   (frame standard-application-frame))
  (setf (frame-panes frame) nil))

(eval-when (eval compile load)
(defun define-application-frame-1 (name state-variables pane-descriptions
				   &key top-level layout
					command-table disabled-commands)
  (let* ((command-table-name (first command-table)))
    ;; If we're going to be defining commands for this application frame,
    ;; make sure there's an command table lying around so that all other
    ;; code doesn't have to be defensive against its absence.
    (when command-table
      (apply #'define-command-table-1 command-table))))
)	;eval-when

(defmacro define-application-frame (name superclasses slots &rest options)
  #+Genera (declare (zwei:indentation 1 25 2 3 3 1))
  (with-warnings-for-definition name define-application-frame
    (let (pane panes layout top-level menu-bar pointer-documentation
	  command-definer command-table disabled-commands
	  icon geometry default-initargs)
      (macrolet ((extract (name keyword default &optional (pair t))
		   `(let ((entry (assoc ',keyword options)))
		      (cond ((null entry)
			     (setq ,name ,default))
			    (t
			     ,@(if pair
				   `((assert (= (length entry) 2) (entry)
					     "The length of the option ~S must be 2" entry)
				     (setq ,name (second entry)
					   options (delete entry options)))
				   `((assert (listp (rest entry)) (entry)
					     "The remainder of ~S must be a list" entry)
				     (setq ,name (rest entry)
					   options (delete entry options)))))))))
	(extract pane :pane nil)
	(extract panes :panes nil nil)
	(extract layout :layout nil nil)
	(extract top-level :top-level '(default-frame-top-level))
	(extract menu-bar :menu-bar t)
	(extract pointer-documentation :pointer-documentation nil)
	(extract command-definer :command-definer t)
	(extract command-table :command-table t)
	(extract disabled-commands :disabled-commands nil)
	(extract default-initargs :default-initargs nil nil)
	(extract icon :icon nil nil)
	(extract geometry :geometry nil nil))
      (check-type name symbol)
      (check-type superclasses list)
      (check-type slots list)
      (check-type pane list)
      (check-type panes list)
      (check-type layout list)
      (check-type top-level list)
      (check-type disabled-commands list)
      (when (and pane panes)
	(error "The ~S and ~S options cannot be used together" :pane :panes))
      (when (and pane layout)
	(error "The ~S and ~S options cannot be used together" :pane :layout))
      (when (or (and panes (null layout))
		(and layout (null panes)))
	(error "The ~S and ~S options must be used together" :panes :layout))
      (when (null superclasses)
	(setq superclasses '(standard-application-frame)))
      (when (eq command-definer 't)
	(setq command-definer (fintern "~A-~A-~A" 'define name 'command)))
      (check-type command-definer symbol)
      (cond ((null command-table))
	    ((symbolp command-table)
	     (setq command-table (list command-table)))
	    (t (warn-if-command-table-invalid name command-table)))
      (when (eq (first command-table) 't)
	(setq command-table (list* name (rest command-table))))
    #-Silica (warn-if-pane-descriptions-invalid name pane-descriptions)
    #-Silica (warn-if-layout-invalid name layout pane-descriptions)
    (let ((pane-constructors 
	    (cond (panes
		   (compute-pane-constructor-code panes))
		  (pane
		   (compute-pane-constructor-code `((,name ,pane)))))))
      `(progn
	 (eval-when (compile)
	   (when ',command-table
	     (setf (compile-time-property ',(first command-table) 'command-table-name) t))
	   (define-application-frame-1 ',name ',slots ,pane-constructors
				       :layout ',layout
				       :top-level ',top-level
				       :command-table ',command-table))
	 (define-group ,name define-application-frame
	   (defclass ,name ,superclasses ,slots
	     ,@options
	     (:default-initargs
	       :menu-bar ',menu-bar
	       :pointer-documentation ',pointer-documentation
	       ,@(and command-table `(:command-table ',(car command-table)))
	       ,@(and top-level `(:top-level ',top-level))
	       ,@(and panes `(:pane-constructors ,pane-constructors))
	       ,@(and layout `(:default-layout ',(caar layout)))
	       ,@(and icon `(:icon (list ,@icon)))
	       ,@(and geometry `(:geometry (list ,@geometry)))
	       ,@default-initargs))
	   ,@(when command-definer
	       `((defmacro ,command-definer (command-name arguments &body body)
		   #+Genera (declare (zwei:indentation 1 3 2 1))
		   `(define-frame-command ,',(first command-table)
					  ,command-name ,arguments ,@body))
		 #+Genera (scl:defprop ,command-definer
				       define-command
				       zwei:definition-function-spec-type)
		 #+Genera (scl:defprop ,command-definer
				       remove-command
				       zwei:kill-definition)
		 #+Genera (scl:defprop ,command-definer
				       zwei:defselect-function-spec-finder
				       zwei:definition-function-spec-finder)))
	   ;;--- Need to handle DISABLED-COMMANDS properly, 
	   ;;--- which entails doing a COPY-LIST
	   (define-application-frame-1 ',name ',slots ,pane-constructors
				       :layout ',layout
				       :top-level ',top-level
				       :command-table ',command-table
				       :disabled-commands ',disabled-commands)
	   #+Cloe-Runtime
	   (cloe:define-program ,name ()
	     :main ,name
	     :debugger-hook cloe-debugger-hook)
	   ,@(compute-generate-panes-code name pane panes layout)))))))

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

#+CLIM-1-compatibility
(defmacro with-frame-state-variables
	  ((frame-name &optional (frame '*application-frame*)) &body body)
  ;; frame descriptor must be findable at compile-time
  (let* ((descriptor (find-frame-descriptor frame-name))
	 (state-variables (frame-descriptor-state-variable-names descriptor)))
    `(with-slots ,state-variables ,frame ,@body)))

(defun warn-if-command-table-invalid (frame-name command-table)
  (cond ((not (and (listp command-table)
		   (symbolp (first command-table))
		   (oddp (length command-table))))
	 (warn "The ~S option for frame ~S, ~S, is invalid.~@
		It is supposed to be a list of a command table name followed by ~
		keyword/value pairs."
	       :command-table frame-name command-table))
	(t
	 (do* ((options (cdr command-table) (cddr options))
	       (keyword (first options) (first options))
	       (value (second options) (second options)))
	      ((null options))
	   (case keyword
	     (:inherit-from
	       (if (listp value)
		   (dolist (item value)
		     (unless (typep item '(or symbol command-table))
		       (warn "The ~S keyword in the ~S option for frame ~S~@
			      is followed by a list containing ~S, which is not a ~
			      command table nor a command table name."
			     :inherit-from :command-table frame-name item)))
		   (warn "The ~S keyword in the ~S option for frame ~S~@
			  is followed by ~S, which is not a list of command tables ~
			  or command table names."
			 :inherit-from :command-table frame-name value)))
	     (:menu
	       (if (listp value)
		   (dolist (clause value)
		     (unless (and (listp clause)
				  (>= (length clause) 3)
				  (oddp (length clause))
				  (stringp (first clause))
				  (member (second clause)
					  '(:command :function :menu :divider)))
		       (warn "The ~S keyword in the ~S option for frame ~S~@
			      is followed by a list of menu clauses containing~@
			      an invalid clause ~S."
			     :menu :command-table frame-name clause)))
		   (warn "The ~S keyword in the ~S option for frame ~S~@
			  is followed by ~S, which is not a list of menu clauses."
			 :menu :command-table frame-name value)))
	     (otherwise
	       (warn "The keyword ~S in the ~S option for frame ~S is invalid.~@
		      The valid keywords are ~S and ~S."
		     keyword :command-table frame-name :inherit-from :menu))))))) 

(defun compute-generate-panes-code (name code panes layouts)
  (if panes
      (compute-complex-generate-panes-code name panes layouts)
      (compute-simple-generate-panes-code name code)))

(defun compute-simple-generate-panes-code (name code)
  (and code
       (let ((frame '#:frame)
	     (framem '#:framem))
	 `((defmethod generate-panes ((,framem standard-frame-manager) (,frame ,name))
	     (let ((*application-frame* ,frame))
	       (setf (frame-panes ,frame)
		     (frame-wrapper ,framem ,frame
		       (with-look-and-feel-realization (,framem ,frame)
			 ,code)))))))))

(defun compute-complex-generate-panes-code (name panes layouts)
  (let ((frame '#:frame)
	(framem '#:framem))
    `((defmethod generate-panes ((,framem standard-frame-manager) (,frame ,name))
	(symbol-macrolet
	  ,(mapcar #'(lambda (pane-spec)
		       (destructuring-bind (name code &rest ignore) pane-spec
			 (declare (ignore code ignore))
			 `(,name (find-or-make-pane-named ,frame ',name))))
		   panes)
	  (let ((*application-frame* ,frame))
	    (setf (frame-panes ,frame)
		  (frame-wrapper ,framem ,frame
		    (with-look-and-feel-realization (,framem ,frame)
		      (ecase (frame-current-layout ,frame)
			,@(mapcar #'(lambda (layout-spec)
				      (destructuring-bind (name panes) layout-spec
					`(,name ,panes)))
				  layouts)))))))))))

(defmethod find-or-make-pane-named ((frame standard-application-frame) name)
  (with-slots (all-panes pane-constructors) frame
    (second (or (assoc name all-panes)
		(car (push (list name 
				 (funcall (second (assoc name pane-constructors))
					  frame (frame-manager frame)))
			   all-panes))))))

(defun compute-pane-constructor-code (panes)
  `(list ,@(mapcar #'(lambda (pane-spec)
		       (destructuring-bind (name code &rest options) pane-spec
			 (setq code (canonicalize-pane-spec name code options))
			 `(list ',name
				#'(lambda (frame framem)
				    (with-look-and-feel-realization (framem frame)
				      ,code)))))
		   panes)))
   
(defun canonicalize-pane-spec (name code rest)
  (cond ((symbolp code)
	 (unless (getf rest :name)
	   (setf (getf rest :name) `',name))
	 (apply #'find-pane-class-constructor code rest))
	((null rest) code)
	(t
	 (error "Invalid pane specification: ~S"
		(list* name code rest)))))

(defmethod find-pane-class-constructor ((typep t) &rest options)
  (error "Unknown pane type ~S with options ~S" type options))

(defmacro define-pane-type (type lambda-list &body body)
  `(defmethod find-pane-class-constructor ((type (eql ',type)) ,@lambda-list)
     ,@body))

(define-pane-type :title (&rest options)
  `(make-pane 'title-pane ,@options))

(define-pane-type :interactor (&rest options)
  `(make-clim-interactor-pane ,@options))

(define-pane-type :application (&rest options)
  `(make-clim-application-pane ,@options))

(define-pane-type :pointer-documentation (&rest options)
  `(make-pane 'pointer-documentation-pane ,@options))

(define-pane-type :command-menu (&rest options)
  `(make-pane 'command-menu-pane ,@options))

(define-pane-type scroll-bar (&rest options)
  `(make-pane 'scroll-bar ,@options))

(define-pane-type slider (&rest options)
  `(make-pane 'slider ,@options))

(define-pane-type push-button (&rest options)
  `(make-pane 'push-button ,@options))

(define-pane-type label-pane (&rest options)
  `(make-pane 'label-pane ,@options))

(define-pane-type text-field (&rest options)
  `(make-pane 'text-field ,@options))

(define-pane-type text-editor (&rest options)
  `(make-pane 'silicatext-editor ,@options))

(define-pane-type toggle-button (&rest options)
  `(make-pane 'toggle-button ,@options))

(define-pane-type radio-box (&rest options)
  `(make-pane 'radio-box ,@options))

(define-pane-type menu-bar (&rest options)
  `(make-pane 'menu-bar ,@options))

;;; 

(defmethod find-or-make-pane-named ((frame standard-application-frame) name)
  (with-slots (all-panes pane-constructors) frame
    (second (or (assoc name all-panes)
		(car (push (list name 
				 (funcall (second (assoc name pane-constructors))
					  frame (frame-manager frame)))
			   all-panes))))))

>>>>>>> 1.18
(defmethod layout-frame ((frame standard-application-frame) &optional width height)
  (let ((panes (frame-panes frame)))
    (when panes
      (clear-space-requirement-caches-in-tree panes)
      (unless (and width height)
	(let ((sr (compose-space panes)))
	  (setq width  (space-requirement-width sr)
		height (space-requirement-height sr))
	  (multiple-value-bind
	      (gw gh) (bounding-rectangle-size (graft frame))
	    (minf width (* 0.9 gw))
	    (minf height (* 0.9 gh)))))
      ;;--- Don't bother with this if the size didn't change?
      (let ((top (or (frame-top-level-sheet frame) panes)))
	(if (and (sheet-enabled-p top)
		 (not (frame-resizable frame)))
	    (multiple-value-call 
		#'allocate-space 
	      top
	      (bounding-rectangle-size top))
	  (resize-sheet* 
	   top
	   width height
	   ;; We need to do this because if the WM has changed the sheet
	   ;; then this will not change its size but we need it to start
	   ;; the layout stuff
	   :force t))))))

(defmethod (setf frame-current-layout) (nv (frame standard-application-frame))
  (unless (eq (frame-current-layout frame) nv)
    (setf (slot-value frame 'current-layout) nv)
    ;; First disown all the children
    (dolist (name-and-pane (slot-value frame 'all-panes))
      (let ((sheet (second name-and-pane)))
	(when (sheet-parent sheet)
	  (sheet-disown-child (sheet-parent sheet) sheet))))
    (dolist (child (sheet-children (frame-top-level-sheet frame)))
      (sheet-disown-child (frame-top-level-sheet frame) child))
    ;; Now we want to give it some new ones
    (generate-panes (frame-manager frame) frame)
    (sheet-adopt-child (frame-top-level-sheet frame) (frame-panes frame))
    (multiple-value-call #'layout-frame
      frame (bounding-rectangle-size (frame-top-level-sheet frame)))
    (throw 'layout-changed nil)))
		 
#+CLIM-1-compatibility
(define-compatibility-function (set-frame-layout (setf frame-current-layout))
			       (frame new-layout)
  (setf (frame-current-layout frame) new-layout))

(defun make-application-frame (frame-name &rest options 
			       &key frame-class
				    enable pretty-name
			            x y width height
				    save-under
			       &allow-other-keys)
  (declare (dynamic-extent options))
  (check-type pretty-name (or null string))
  (when (null frame-class)
    (setq frame-class frame-name))
  (when (or x y width height)
    (when (getf options :geometry)
      (error "Cannot specify :geometry and :x,:y,:width or :height at ~
the same time " options))
    (setf (getf options :geometry)
      (append (and x `(:x ,x))
	      (and y `(:y ,y))
	      (and width `(:width ,width))
	      (and height `(:height ,height)))))
  (with-keywords-removed (options options 
			  '(:frame-class :pretty-name
			    :enable :x :y :width :height :save-under))
      (let ((frame (apply #'make-instance frame-class
			  :name frame-name
			  ;;-- Perhaps this should be a default-initarg?
			  :pretty-name (or pretty-name
					   (title-capitalize (string frame-name)))
			  :properties `(:save-under ,save-under)
			  options)))
	(when enable 
	  (enable-frame frame))
	frame)))

(defun title-capitalize (string)
  (let ((new-string (substitute #\Space #\- string)))
    (when (eq new-string string)
      (setq new-string (copy-seq new-string)))
    (nstring-capitalize new-string)))

(defmethod enable-frame ((frame standard-application-frame))
  (unless (frame-manager frame)
    (error "Cannot enabled :disowned frame ~S" frame))
  (destructuring-bind
      (&key width height &allow-other-keys) (frame-geometry frame)
    (ecase (frame-state frame)
      (:enabled)
      ((:disabled :disowned)
       (let ((old (frame-state frame)))
	 (setf (frame-state frame) :enabled)
	 ;; IF this is a new frame then if the user specified a width
	 ;; then we should be using that
	 ;; IF the frame already exists then we probably should be using
	 ;; the top level sheet size
	 (multiple-value-call #'layout-frame 
	   frame
	   (ecase old
	     (:disowned 
	       (if (and width height)
		   (values width height)
		   (values)))
	     (:disabled
	       (bounding-rectangle-size
		 (frame-top-level-sheet frame)))))
	 (note-frame-enabled (frame-manager frame) frame))))))

(defmethod destroy-frame ((frame standard-application-frame))
  (when (eq (frame-state frame) :enabled)
    (disable-frame frame))
  (disown-frame (frame-manager frame) frame))

(defmethod disable-frame ((frame standard-application-frame))
  (ecase (frame-state frame)
    ((:disowned :disabled))
    ((:enabled :shrunk)
     (setf (frame-state frame) :disabled)
     (note-frame-disabled (frame-manager frame) frame))))

(defmethod reset-frame ((frame standard-application-frame) &rest ignore)
  (declare (ignore ignore))
  nil)

(defmethod note-frame-enabled ((framem standard-frame-manager)
			       (frame standard-application-frame))
  )

(defmethod note-frame-disabled ((framem standard-frame-manager)
				(frame standard-application-frame))
  )

(defmethod run-frame-top-level :around ((frame standard-application-frame))
  (with-simple-restart (frame-exit "Exit ~A" (frame-pretty-name frame))
    (unwind-protect
	(let (#-Silica (*frame-layout-changing-p* *frame-layout-changing-p*)
	      ;; Reset the state of the input editor and the presentation
	      ;; type system, etc., in case there is an entry into another
	      ;; application from inside the input editor, such as a Debugger
	      ;; written using CLIM.
	      ;;--- This should be done in a more modular way
	      ;;--- If you change this, change MENU-CHOOSE-FROM-DRAWER
	      (*original-stream* nil)
	      (*input-wait-test* nil)
	      (*input-wait-handler* nil)
	      (*pointer-button-press-handler* nil)
	      (*numeric-argument* nil)
	      (*delimiter-gestures* nil)
	      (*activation-gestures* nil)
	      (*accelerator-gestures* nil)
	      (*input-context* nil)
	      (*accept-help* nil)
	      (*assume-all-commands-enabled* nil)
	      #-Silica (*sizing-application-frame* nil)
	      (*command-parser* 'command-line-command-parser)
	      (*command-unparser* 'command-line-command-unparser)
	      (*partial-command-parser*
		'command-line-read-remaining-arguments-for-partial-command) 
	      (*application-frame* frame))
	  (loop
	    (with-simple-restart (nil "~A top level" (frame-pretty-name frame))
	      (loop
		(catch 'layout-changed
		  (let ((*application-frame* frame)
			(*pointer-documentation-output*
			  (frame-pointer-documentation-output frame)))
		    ;; We must return the values from CALL-NEXT-METHOD,
		    ;; or else ACCEPTING-VALUES will return NIL
		    #-CCL-2
		    (return-from run-frame-top-level (call-next-method))
		    ;; The (RETURN-FROM FOO (CALL-NEXT-METHOD)) form above
		    ;; doesn't work in Coral.  If the "top level" restart
		    ;; above is taken, the CALL-NEXT-METHOD form blows out
		    ;; the second time through this code, claiming that it
		    ;; can't find the next method.  Hoisting the
		    ;; CALL-NEXT-METHOD out of the RETURN-FROM form seems
		    ;; to fix it...  So it conses, big deal.
		    #+CCL-2
		    (let ((results (multiple-value-list (call-next-method))))
		      (return-from run-frame-top-level (values-list results)))))))))
      (disable-frame frame))))

(defmethod run-frame-top-level ((frame standard-application-frame))
  (let ((top-level (frame-top-level frame)))
    (if (atom top-level)
	(funcall top-level frame)
        (apply (first top-level) frame (rest top-level)))))

(defmethod default-frame-top-level ((frame standard-application-frame)
				    &key command-parser command-unparser
					 partial-command-parser
					 (prompt "Command: "))
  (unless (eq (frame-state frame) :enabled)
    (enable-frame frame))
  (loop
    (catch 'layout-changed
      (let* ((*standard-output*
	       (or (frame-standard-output frame) *standard-output*))
	     (*standard-input* 
	       (or (frame-standard-input frame) *standard-output*))
	     (*query-io* 
	       (or (frame-query-io frame) *standard-input*))
	     (*error-output* 
	       (or (frame-error-output frame) *standard-output*))
	     (interactor
	       (not (null (find-frame-pane-of-type frame 'interactor-pane))))
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
		       #'menu-read-remaining-arguments-for-partial-command)))
	     (command-stream
	      ;;-- We have to ask the frame since we do not want to
	      ;; just pick up a stream from the dynamic environment
	      (let ((si (or (frame-standard-input frame)
			    (frame-standard-output frame))))
	      ;;--- I'm not really convinced that this is right
	      ;; Who is not convinced
	       (typecase si
		 (output-protocol-mixin si)
		 (t (frame-top-level-sheet frame))))))
	#+Allegro
	(unless (typep *standard-input* 'excl::bidirectional-terminal-stream)
	  (assert (port *standard-input*)))
	#+Allegro
	(unless (typep *standard-output* 'excl::bidirectional-terminal-stream)
	  (assert (port *standard-output*)))
	#+Allegro
	(unless (typep *query-io* 'excl::bidirectional-terminal-stream)
	  (assert (port *query-io*)))
	;; The read-eval-print loop for applications...
	(loop
	  ;; Redisplay all the panes
	  (catch-abort-gestures ("Return to ~A command level" (frame-pretty-name frame))
	    (redisplay-frame-panes frame)
	    (when interactor
	      (fresh-line *standard-input*)
	      (if (stringp prompt)
		  (write-string prompt *standard-input*)
		  (funcall prompt *standard-input* frame)))
	    (let ((command (read-frame-command frame :stream command-stream)))
	      (when interactor
		(terpri *standard-input*))
	      ;; Need this check in case the user aborted out of a command menu
	      (when command
		(execute-frame-command frame command)))))))))

;; Generic because someone might want :BEFORE or :AFTER
(defmethod frame-exit ((frame standard-application-frame))
  (invoke-restart 'frame-exit))

#-Silica
(defun display-title (frame stream)
  (multiple-value-bind (pane desc) (get-frame-pane frame stream)
    (when (and pane (eq pane stream))
      (let ((title (getf (pane-descriptor-options desc) :display-string)))
	(when (and (stringp title)
		   (not (string-equal title (frame-pretty-name frame))))
	  ;; On some hosts this will update the title bar 
	  (setf (frame-pretty-name frame) title))
	(when (and (eq stream pane) (not (dummy-pane-p pane)))
	  (multiple-value-bind (width height)
	      (window-inside-size stream)
	    (draw-string* stream (or title (frame-pretty-name frame))
			  (/ width 2) (/ height 2)
			  :align-x :center :align-y :center))
	  (force-output stream))
	(values)))))

;;--- Handle incremental redisplay...
(defun display-command-menu (frame stream &rest keys
			     &key command-table &allow-other-keys)
  (declare (dynamic-extent keys))
  (when (or (null command-table)
	    (eq command-table t))
    (setq command-table (frame-command-table frame)))
  (with-keywords-removed (keys keys '(:command-table))
    (apply #'display-command-table-menu command-table stream keys)))

;; The contract of GET-FRAME-PANE is to get a pane upon which we can do normal
;; I/O operations, that is, a CLIM stream pane
(defmethod get-frame-pane ((frame standard-application-frame) pane-name &key (errorp t))
  (with-slots (all-panes) frame
    (let ((pane (assoc pane-name all-panes)))
      (when pane
	(map-over-sheets #'(lambda (sheet)
			     (when (typep sheet 'clim-stream-pane)
			       (return-from get-frame-pane sheet)))
			 (second pane)))
      (when errorp
	(error "There is no pane named ~S in frame ~S" pane-name frame)))))

(defmethod redisplay-frame-panes (frame &key force-p)
  (map-over-sheets #'(lambda (sheet)
		       (when (typep sheet 'clim-stream-pane)
			 (redisplay-frame-pane frame sheet :force-p
					       force-p)))
		   (frame-top-level-sheet frame)))

;;--- What about CLIM 0.9's PANE-NEEDS-REDISPLAY, etc?
;;--- What about CLIM 1.0's :DISPLAY-AFTER-COMMANDS :NO-CLEAR?

(defun redisplay-frame-pane (frame pane &key force-p)
  (when (symbolp pane)
    (setq pane (get-frame-pane frame pane)))
  (cond ((pane-display-function pane)
	 (let* ((ir (slot-value pane 'incremental-redisplay-p))
		(redisplay-p (if (listp ir) (first ir) ir))
		(check-overlapping (or (atom ir)	;default is T
				       (getf (rest ir) :check-overlapping t))))
	   (with-simple-restart (nil "Skip redisplaying pane ~S" pane)
	     (loop
	       (with-simple-restart (nil "Retry displaying pane ~S" pane)
		 (return
		   (let ((redisplay-record
			   (and redisplay-p
				(let ((history (stream-output-history pane)))
				  (when history
				    #+compulsive-redisplay
				    (when (> (output-record-count history) 1)
				      (cerror "Clear the output history and proceed"
					      "There is more than one element in this redisplay pane")
				      (window-clear pane))
				    (unless (zerop (output-record-count history))
				      (output-record-element history 0)))))))
		     (cond ((and redisplay-p
				 (or force-p (null redisplay-record)))
			    (when force-p
			      (window-clear pane))
			    (invoke-pane-redisplay-function frame pane))
			   (redisplay-p
			    (redisplay redisplay-record pane 
				       :check-overlapping check-overlapping))
			   ((pane-needs-redisplay pane)
			    (invoke-pane-display-function frame pane))))))))))
	(force-p
	 ;;-- Is there anything else we need to do?
	 (stream-replay pane))))

(defun invoke-pane-redisplay-function (frame pane &rest args)
  (declare (dynamic-extent args))
  (updating-output (pane)
    (apply #'invoke-pane-display-function frame pane args)))

(defun invoke-pane-display-function (frame pane &rest args)
  (declare (dynamic-extent args))
  (let* ((df (pane-display-function pane))
	 (display-args (if (listp df) (rest df) nil))
	 (display-function (if (listp df) (first df) df)))
    ;; Cons as little as possible...
    (cond ((and (null args) (null display-args))
	   (funcall display-function frame pane))
	  ((null display-args)
	   (apply display-function frame pane args))
	  ((null args)
	   (apply display-function frame pane display-args))
	  (t
	   (apply display-function frame pane (append args display-args))))))
			 
(defmethod read-frame-command ((frame standard-application-frame) 
			       &key (stream *query-io*)		;frame-query-io?
			       ;; should the rest of the *command-parser*
			       ;; etc. variables be passed as keywords or bound?
			       )
  (read-command (frame-command-table frame) :stream stream))

(defmethod execute-frame-command ((frame standard-application-frame) command)
  (apply (command-name command) (command-arguments command)))

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

#+CLIM-1-compatibility
(progn
(define-compatibility-function (command-enabled-p command-enabled)
			       (command-name frame)
  (command-enabled command-name frame))

(define-compatibility-function (enable-command (setf command-enabled))
			       (command-name frame)
  (setf (command-enabled command-name frame) t))

(define-compatibility-function (disable-command (setf command-enabled))
  (setf (command-enabled command-name frame) nil))
)	;#+CLIM-1-compatibility

(defmethod note-command-enabled ((framem standard-frame-manager)
				 (frame standard-application-frame) command)
  (declare (ignore command)))

(defmethod note-command-disabled ((framem standard-frame-manager)
				  (frame standard-application-frame) command)
  (declare (ignore command)))

;;; The contract of this is to replay the contents of STREAM within the region.
(defmethod frame-replay ((frame standard-application-frame) stream &optional region)
  (stream-replay stream region)
  (force-output stream))

;;; The contract of this is to find an "appropriate" presentation; i.e., one
;;; satisfying the input context specified by TYPE.  Everything that looks for a
;;; presentation goes through this so that applications can specialize it.
(defmethod frame-find-innermost-applicable-presentation
	   ((frame standard-application-frame) input-context stream x y &key event)
  (find-innermost-applicable-presentation
    input-context stream x y
    :frame frame
    :modifier-state (window-modifier-state stream) :event event))

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
		(not (region-contains-position-p (stream-output-history window) x y)))
      (funcall *click-outside-menu-handler*))
    (when highlighted-presentation
      ;; Unhighlight on the way out.
      ;; But only unhighlight the window that the click is from. 
      (unhighlight-highlighted-presentation window nil))
    (throw-highlighted-presentation 
      (or (and (output-recording-stream-p window)
	       (frame-find-innermost-applicable-presentation
		 frame input-context window x y
		 :event button-press-event))
	  *null-presentation*)
      input-context
      button-press-event)))

(defun find-frame-pane-of-type (frame type)
  (map-over-sheets #'(lambda (sheet)
		       (when (typep sheet type)
			 (return-from find-frame-pane-of-type sheet)))
		   (frame-top-level-sheet frame)))

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

(defmethod frame-pointer-documentation-output ((frame standard-application-frame))
  (with-slots (pointer-documentation-pane) frame
    pointer-documentation-pane))

;;--- This causes direct-manipulation and menu-driven applications not to
;;--- maintain histories.  Is there a better heuristic?
(defmethod frame-maintain-presentation-histories ((frame standard-application-frame))
  (not (null (find-frame-pane-of-type frame 'interactor-pane))))

(defmethod notify-user (frame message &rest options) 
  (when frame (setf (getf options :frame) frame))
  (apply #'port-notify-user 
	 (if frame (port frame) (find-port))
	 message
	 options))

(defmethod select-file (frame &rest options) 
  (when frame (setf (getf options :frame) frame))
  (apply #'port-select-file 
	 (if frame (port frame) (find-port))
	 options))


;;; Pointer documentation

(defvar *pointer-documentation-interval*
	(max (floor (* 1/10 internal-time-units-per-second)) 1))
(defvar *last-pointer-documentation-time* 0)

#+Genera
(defvar *pointer-documentation-buffer*
	(make-array 80 :element-type 'string-char :fill-pointer 0 :adjustable t))

;;; Produce pointer documentation
(defmethod frame-document-highlighted-presentation
	   ((frame standard-application-frame) presentation input-context window x y stream)
  (let (#+Genera (documentation-window (mouse-documentation-window window)))
    ;; The documentation should never say anything if we're not over a presentation
    (when (null presentation) 
      #+Genera (if documentation-window
		   (scl:send documentation-window :clear-window)
		   (window-clear stream))
      #-Genera (window-clear stream))
    ;; Cheap test to not do this work too often
    (let ((old-modifier-state *last-pointer-documentation-modifier-state*)
	  (modifier-state (window-modifier-state window))
	  (last-time *last-pointer-documentation-time*)
	  (time (get-internal-real-time)))
      (setq *last-pointer-documentation-modifier-state* modifier-state)
      (when (and (< time (+ last-time *pointer-documentation-interval*))
		 (= modifier-state old-modifier-state))
	(return-from frame-document-highlighted-presentation nil))
      (setq *last-pointer-documentation-time* time))
    (when presentation
      #+Genera
      (when documentation-window
	(setf (fill-pointer *pointer-documentation-buffer*) 0)
	(with-output-to-string (stream *pointer-documentation-buffer*)
	  (scl:send documentation-window :clear-window)
	  (when (null (frame-document-highlighted-presentation-1
			frame presentation input-context window x y stream))
	    (setq *last-pointer-documentation-time* 0))
	  (scl:send documentation-window :string-out *pointer-documentation-buffer*))
	(return-from frame-document-highlighted-presentation nil))
      (with-output-recording-options (stream :record nil)
	(with-end-of-line-action (stream :allow)
	  (with-end-of-page-action (stream :allow)
	    (window-clear stream)
	    (when (null (frame-document-highlighted-presentation-1
			  frame presentation input-context window x y stream))
	      (setq *last-pointer-documentation-time* 0))
	    (force-output stream)))))))

(defun frame-document-highlighted-presentation-1
       (frame presentation input-context window x y stream)
  (let ((modifier-state (window-modifier-state window)))
    (declare (type fixnum modifier-state))
    (multiple-value-bind (left   left-presentation   left-context
			  middle middle-presentation middle-context
			  right  right-presentation  right-context)
	(find-applicable-translators-for-documentation presentation input-context
						       frame window x y modifier-state)
      (let* ((*print-length* 3)
	     (*print-level* 2)
	     (*print-circle* nil)
	     (*print-array* nil)
	     (*print-readably* nil)
	     (*print-pretty* nil))
	(flet ((document-translator (translator presentation context-type
				     button-name separator)
		 ;; Assumes 5 modifier keys and the reverse ordering of *MODIFIER-KEYS*
		 (let ((bit #o20)
		       (shift-name '("h-" "s-" "m-" "c-" "sh-")))
		   (declare (type fixnum bit))
		   (repeat 5			;length of shift-name
		     (unless (zerop (logand bit modifier-state))
		       (write-string (car shift-name) stream))
		     (pop shift-name)
		     (setq bit (the fixnum (ash bit -1)))))
		 (write-string button-name stream)
		 (document-presentation-translator translator presentation context-type
						   frame nil window x y
						   :stream stream
						   :documentation-type :pointer)
		 (write-string separator stream)))
	  (declare (dynamic-extent #'document-translator))
	  ;;--- The button names should be hard-wired in.  Consider 1-button
	  ;;--- Macs and 2-button PCs...
	  (when left
	    (let ((button-name (cond ((and (eq left middle)
					   (eq left right))
				      (setq middle nil
					    right nil)
				      "L,M,R: ")
				     ((eq left middle) 
				      (setq middle nil)
				      "L,M: ")
				     (t "L: "))))
	      (document-translator left left-presentation left-context
				   button-name (if (or middle right) "; " "."))))
	  (when middle
	    (let ((button-name (cond ((eq middle right)
				      (setq right nil)
				      "M,R: ")
				     (t "M: "))))
	      (document-translator middle middle-presentation middle-context
				   button-name (if right "; " "."))))
	  (when right
	    (document-translator right right-presentation right-context
				 "R: " "."))
	  ;; Return non-NIL if any pointer documentation was produced
	  (or left middle right))))))

;; This is derived directly from FIND-APPLICABLE-TRANSLATORS
(defun find-applicable-translators-for-documentation
       (presentation input-context frame window x y modifier-state)
  ;; Assume a maximum of three pointer buttons
  (let ((left nil)   (left-presentation nil)   (left-context nil)
	(middle nil) (middle-presentation nil) (middle-context nil)
	(right nil)  (right-presentation nil)  (right-context nil))
    (macrolet ((match (translator presentation context-type button)
		 (let* ((symbol (symbol-name button))
			(button-translator (intern symbol))
			(button-presentation (fintern "~A-~A" symbol 'presentation))
			(button-context (fintern "~A-~A" symbol 'context)))
		   `(when (and (or (null ,button-translator)
				   (> (presentation-translator-priority ,translator)
				      (presentation-translator-priority ,button-translator)))
			       (button-and-modifier-state-matches-gesture-name-p
				 (button-index ,button) modifier-state
				 (presentation-translator-gesture-name ,translator)))
		      (setq ,button-translator ,translator
			    ,button-presentation ,presentation
			    ,button-context ,context-type)))))
      (do ((presentation presentation
			 (parent-presentation-with-shared-box presentation window)))
	  ((null presentation))
	(let ((from-type (presentation-type presentation)))
	  (dolist (context input-context)
	    (let ((context-type (pop context)))	;input-context-type = first
	      (let ((translators (find-presentation-translators 
				   from-type context-type (frame-command-table frame))))
		(when translators
		  (dolist (translator translators)
		    (when (test-presentation-translator translator
							presentation context-type
							frame window x y
							:modifier-state modifier-state)
		      (match translator presentation context-type :left)
		      (match translator presentation context-type :middle)
		      (match translator presentation context-type :right)))))
	      (when (and (or left middle right)
			 *presentation-menu-translator*
			 (test-presentation-translator *presentation-menu-translator*
						       presentation context-type
						       frame window x y
						       :modifier-state modifier-state))
		(match *presentation-menu-translator* presentation context-type :left)
		(match *presentation-menu-translator* presentation context-type :middle)
		(match *presentation-menu-translator* presentation context-type :right)))))))
    (values left   left-presentation   left-context
	    middle middle-presentation middle-context
	    right  right-presentation  right-context)))

#+Genera
(defmethod mouse-documentation-window ((window window-stream)) nil)
