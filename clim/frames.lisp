;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: frames.lisp,v 1.46 92/10/04 14:16:23 cer Exp Locker: cer $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 International Lisp Associates."

(define-protocol-class application-frame ())

(defclass standard-application-frame (application-frame)
    ((name :initarg :name :accessor frame-name)
     (pretty-name :initarg :pretty-name :accessor frame-pretty-name)
     (command-table :initarg :command-table 
		    :initform (find-command-table 'user-command-table)
		    :accessor frame-command-table)
     (disabled-commands :initarg :disabled-commands :initform nil)
     ;; One of  T, NIL, or a command-table; used by the menu-bar
     (menu-bar :initarg :menu-bar :initform nil)
     (histories :initform nil)
     (frame-manager :initform nil :accessor frame-manager)
     (calling-frame :reader frame-calling-frame :initarg :calling-frame)
     ;; PANES is the description of all of the named panes,
     ;; ALL-PANES is an alist that stores all of the named panes that
     ;; have actually been realized so far
     (panes :initarg :panes :accessor frame-panes)
     (all-panes :initform nil)
     (current-panes :initform nil :accessor frame-current-panes)
     (initialized-panes :initform nil)
     (pane-constructors :initarg :pane-constructors)
     (top-level-sheet :accessor frame-top-level-sheet
		      :initarg :top-level-sheet :initform nil)
     (state :initform :disowned :accessor frame-state 
	    :type (member :disowned :disabled :enabled :shrunk))
     (top-level :initarg :top-level  :accessor frame-top-level)
     (current-layout :initarg :default-layout :initform nil
		     :reader frame-current-layout)
     (geometry :initform nil :initarg :geometry :reader frame-geometry)
     (icon :initform nil :initarg :icon :reader frame-icon)
     (user-specified-position-p :initform :unspecified
				:initarg :user-specified-position-p
				:reader frame-user-specified-position-p)
     (user-specified-size-p :initform :unspecified
			    :initarg :user-specified-size-p
			    :reader frame-user-specified-size-p)
     (shell :accessor frame-shell)
     (pointer-documentation-p :initarg :pointer-documentation
			      :reader frame-pointer-documentation-p)
     (properties :initform nil :initarg :properties
		 :accessor frame-properties)
     (resizable :initarg :resize-frame
		:reader frame-resizable)
     (layout :initarg :layouts :reader frame-layouts)
     (top-level-process :initform nil)
     (command-queue :initform (make-locking-queue) :reader frame-command-queue)
     (input-buffer :initform nil :initarg :input-buffer :reader frame-input-buffer))
  (:default-initargs :pointer-documentation nil
		     :layouts nil
		     :resize-frame nil
		     :top-level 'default-frame-top-level))

(defmethod port ((frame standard-application-frame))
  (port (frame-manager frame)))

(defmethod graft ((frame standard-application-frame))
  (graft (frame-manager frame)))

(defmethod frame-palette ((frame standard-application-frame))
  (frame-manager-palette (frame-manager frame)))

;;--- These should really be somewhere else
(defmethod frame-manager ((stream standard-encapsulating-stream))
  (frame-manager (encapsulating-stream-stream stream)))

(defmethod frame-manager ((stream t))
  (cond (*application-frame* (or (frame-manager *application-frame*)
				 (find-frame-manager)))
	(t (find-frame-manager))))

(defmethod initialize-instance :after ((frame standard-application-frame) 
				       &rest args
				       &key frame-manager
					    geometry icon
					    (parent frame-manager parent-p))
  (declare (ignore args))
  (destructuring-bind (&key left top width height) geometry
    (declare (ignore left top width height)))
  (destructuring-bind (&key name pixmap clipping-mask) icon
    (declare (ignore name pixmap clipping-mask)))
  (let ((frame-manager
	  (etypecase parent
	    (null (unless parent-p (find-frame-manager)))
	    (list (apply #'find-frame-manager parent))
	    (frame-manager parent)
	    (application-frame (frame-manager parent))
	    (port (find-frame-manager :port parent))
 	    (graft (find-frame-manager :port (port parent)))
	    (sheet (frame-manager (pane-frame parent))))))
    (when frame-manager
      (adopt-frame frame-manager frame))))

;; Default method does nothing
(defmethod generate-panes ((framem standard-frame-manager)
			   (frame standard-application-frame))
  (setf (frame-panes frame) nil))

(eval-when (compile load eval)
(defun define-application-frame-1 (name state-variables pane-descriptions
				   &key top-level layouts
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
    (let (pane panes layouts top-level menu-bar pointer-documentation
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
	(extract layouts :layouts nil nil)
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
      (check-type layouts list)
      (check-type top-level list)
      (check-type disabled-commands list)
      (when (and pane panes)
	(error "The ~S and ~S options cannot be used together" :pane :panes))
      (when (and layouts
		 (null pane)
		 (null panes))
	(error "You must use either ~S or ~S with ~S" :pane :panes :layouts))
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
    #-Silica (warn-if-layouts-invalid name layouts pane-descriptions)
    (let ((pane-constructors 
	    (cond (panes
		   (compute-pane-constructor-code panes))
		  (pane
 		   (compute-pane-constructor-code `((,name ,pane))))))
 	  (layout-value
	    `(list ,@(mapcar
		       #'(lambda (layout)
			   (destructuring-bind (name panes &rest sizes) layout
			     (if sizes
				 `(list ',name ',panes 
					,@(mapcar #'(lambda (pane-and-size)
						      `(list ',(car pane-and-size)
							     ,@(cdr pane-and-size)))
						  sizes))
				 `',layout)))
		       layouts))))
      `(progn
	 (eval-when (compile)
	   (when ',command-table
	     (setf (compile-time-property ',(first command-table) 'command-table-name) t))
	   (define-application-frame-1 ',name ',slots ,pane-constructors
				       :layouts ,layout-value
				       :top-level ',top-level
				       :command-table ',command-table))
	 (define-group ,name define-application-frame
	   (defclass ,name ,superclasses ,slots
	     ,@options
	     (:default-initargs
	       :layouts ,layout-value
	       :menu-bar ',menu-bar
	       :pointer-documentation ',pointer-documentation
	       ,@(and command-table `(:command-table ',(car command-table)))
	       ,@(and top-level `(:top-level ',top-level))
	       ,@(and panes `(:pane-constructors ,pane-constructors))
	       ,@(and layouts `(:default-layout ',(caar layouts)))
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
				       :layouts ,layout-value
				       :top-level ',top-level
				       :command-table ',command-table
				       :disabled-commands ',disabled-commands)
	   #+Cloe-Runtime
	   (cloe:define-program ,name ()
	     :main ,name
	     :debugger-hook cloe-debugger-hook)
	   ,@(compute-generate-panes-code name pane panes layouts)))))))

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
	     (:inherit-menu)
	     (otherwise
	       (warn "The keyword ~S in the ~S option for frame ~S is invalid.~@
		      The valid keywords are ~S and ~S."
		     keyword :command-table frame-name :inherit-from :menu))))))) 

(defun compute-generate-panes-code (name code panes layouts)
  (if layouts
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
			,@(mapcar 
			    #'(lambda (layout-spec)
				(destructuring-bind (name panes &rest ignore) layout-spec
				  (declare (ignore ignore))
				  `(,name ,panes)))
			    layouts)))))))))))

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

(defmethod find-pane-class-constructor ((type t) &rest options)
  (declare (dynamic-extent options))
  (error "Unknown pane type ~S with options ~S" type options))


;;--- Extend this to default the options from the lambda list
(defmacro define-pane-type (type lambda-list &body body)
  `(defmethod find-pane-class-constructor ((type (eql ',type)) ,@lambda-list)
     ,@body))

(define-pane-type :title (&rest options)
  (declare (non-dynamic-extent options))
  `(make-clim-stream-pane
     :type 'title-pane
     ,@options
     :display-function 'display-title
     :display-after-commands nil
     :text-style (parse-text-style '(:sans-serif :bold :large))
     :scroll-bars nil
     :width :compute :height :compute
     :end-of-page-action :allow 
     :end-of-line-action :allow))

(define-pane-type :command-menu (&rest options)
  (declare (non-dynamic-extent options))
  `(make-clim-stream-pane
     :type 'command-menu-pane
     ,@options
     :display-function `(display-command-menu :command-table ,(frame-command-table frame))
     :incremental-redisplay t
     :display-after-commands t
     :text-style *command-table-menu-text-style*
     :scroll-bars nil
     :width :compute :height :compute
     :end-of-page-action :allow 
     :end-of-line-action :allow))

(define-pane-type :interactor (&rest options &key (scroll-bars :vertical))
  (declare (non-dynamic-extent options))
  `(make-clim-interactor-pane
     ,@options
     :scroll-bars ,scroll-bars))

(define-pane-type :application (&rest options &key (scroll-bars :both))
  (declare (non-dynamic-extent options))
  `(make-clim-application-pane 
     ,@options
     :scroll-bars ,scroll-bars))

(define-pane-type :accept-values (&rest options &key (scroll-bars :vertical))
  (declare (non-dynamic-extent options))
  `(make-clim-stream-pane 
     :type 'accept-values-pane
     ,@options
     :display-after-commands :no-clear
     :scroll-bars ,scroll-bars
     :end-of-page-action :allow
     :end-of-line-action :allow))

(define-pane-type :pointer-documentation (&rest options)
  (declare (non-dynamic-extent options))
  `(make-clim-stream-pane
     :type 'pointer-documentation-pane
     ,@options
     :display-after-commands nil
     :text-style (parse-text-style '(:sans-serif :bold :normal))
     :scroll-bars nil
     :end-of-page-action :allow
     :end-of-line-action :allow))

(define-pane-type scroll-bar (&rest options)
  (declare (non-dynamic-extent options))
  `(make-pane 'scroll-bar ,@options))

(define-pane-type slider (&rest options)
  (declare (non-dynamic-extent options))
  `(make-pane 'slider ,@options))

(define-pane-type push-button (&rest options)
  (declare (non-dynamic-extent options))
  `(make-pane 'push-button ,@options))

(define-pane-type label-pane (&rest options)
  (declare (non-dynamic-extent options))
  `(make-pane 'label-pane ,@options))

(define-pane-type text-field (&rest options)
  (declare (non-dynamic-extent options))
  `(make-pane 'text-field ,@options))

(define-pane-type text-editor (&rest options)
  (declare (non-dynamic-extent options))
  `(make-pane 'text-editor ,@options))

(define-pane-type toggle-button (&rest options)
  (declare (non-dynamic-extent options))
  `(make-pane 'toggle-button ,@options))

(define-pane-type radio-box (&rest options)
  (declare (non-dynamic-extent options))
  `(make-pane 'radio-box ,@options))

(define-pane-type check-box (&rest options)
  (declare (non-dynamic-extent options))
  `(make-pane 'check-box ,@options))

(define-pane-type list-pane (&rest options)
  (declare (non-dynamic-extent options))
  `(make-pane 'list-pane ,@options))

(define-pane-type option-pane (&rest options)
  (declare (non-dynamic-extent options))
  `(make-pane 'option-pane ,@options))

(define-pane-type menu-bar (&rest options)
  (declare (non-dynamic-extent options))
  `(make-pane 'menu-bar ,@options))


(defmethod find-or-make-pane-named ((frame standard-application-frame) name)
  (with-slots (all-panes pane-constructors) frame
    (second (or (assoc name all-panes)
		(let ((new-pane
			(list name 
			      (funcall (second (assoc name pane-constructors))
				       frame (frame-manager frame)))))
		  ;; Maintain ALL-PANES in the order the panes are created
		  (setq all-panes (nconc all-panes (list new-pane)))
		  new-pane)))))
 
(defmethod layout-frame ((frame standard-application-frame) &optional width height)
  (let ((panes (frame-panes frame))
	(*application-frame* frame))
    (when panes
      (clear-space-requirement-caches-in-tree panes)
      (multiple-value-bind (graft-width graft-height) 
	  (bounding-rectangle-size (graft frame))
	(cond ((and width height)
	       (minf width graft-width) 
	       (minf height graft-height))
	      (t
	       (let ((sr (compose-space panes)))
		 (setq width  (or width (space-requirement-width sr))
		       height (or height (space-requirement-height sr)))
		 ;;--- This fudge factor stuff looks dubious  --SWM
		 (let ((fudge-factor #+Allegro 0.9 #-Allegro 1))
		   (minf-or width (* graft-width fudge-factor))
		   (minf-or height (* graft-height fudge-factor)))))))
      ;;--- Don't bother with this if the size didn't change?
      (let ((top-sheet (or (frame-top-level-sheet frame) panes)))
	(if (and (sheet-enabled-p top-sheet)
		 (not (frame-resizable frame)))
	    (multiple-value-call #'allocate-space 
	      top-sheet (bounding-rectangle-size top-sheet)) 
	    (resize-sheet top-sheet width height))))))

(defvar *frame-layout-changing-p* nil)

(defmethod (setf frame-current-layout) (layout (frame standard-application-frame))
  (unless (or (eq (frame-current-layout frame) layout)
	      (null (frame-top-level-sheet frame)))
    (setq *frame-layout-changing-p* t)
    (setf (slot-value frame 'current-layout) layout)
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
    (let ((layout-space-requirements 
	    (cddr (assoc layout (frame-layouts frame)))))
      (changing-space-requirements (:layout nil)
	(flet ((adjust-layout (sheet)
		 (change-space-requirements-to-default sheet)
		 (let ((x (and (panep sheet)
			       (assoc (pane-name sheet) layout-space-requirements))))
		   (when x (apply #'change-space-requirements sheet (cdr x))))))
	  (declare (dynamic-extent #'adjust-layout))
	  (map-over-sheets #'adjust-layout (frame-top-level-sheet frame)))))
    ;;--- Don't throw, just recompute stream bindings in a principled way
    (handler-case
        (throw 'layout-changed nil)
      (error () nil))))

(defmethod frame-all-layouts ((frame standard-application-frame))
  (mapcar #'first (frame-layouts frame)))
		 
#+Genera (zwei:defindentation (make-application-frame 1 1))
(defun make-application-frame (frame-name &rest options 
			       &key frame-class
				    enable pretty-name
			            left top right bottom width height
				    (user-specified-position-p :unspecified)
 				    (user-specified-size-p :unspecified)
				    save-under
			       &allow-other-keys)
  (declare (dynamic-extent options))
  (check-type pretty-name (or null string))
  (when (null frame-class)
    (setq frame-class frame-name))
  (when (or left top right bottom width height)
    (when (getf options :geometry)
      (error "Cannot specify ~S and ~S, S, ~S, ~S, ~S, or ~S at the same time"
	     :geometry :left :top :right :bottom :width :height))
    (macrolet ((check-conflict (edge1 edge2 size)
		 `(cond 
		    ((and ,edge1 ,size)
		     (if ,edge2
			 (error "Cannot specify ~S, ~S, and ~S together" ,edge1 ,size ,edge2)
			 (setq ,edge2 (+ ,edge1 ,size))))
		    ((and ,edge2 ,size)
		     (if ,edge1
			 (error "Cannot specify ~S, ~S, and ~S together" ,edge2 ,size ,edge1)
			 (setq ,edge1 (+ ,edge2 ,size))))
		    ((and ,edge2 ,edge1)
		     (if ,size
			 (error "Cannot specify ~S, ~S, and ~S together" ,edge2 ,edge1 ,size)
			 (setq ,size (- ,edge2 ,edge1)))))))
      (check-conflict left right width)
      (check-conflict top bottom height))
    (setf (getf options :geometry)
	  (append (and left `(:left ,left))
		  (and top `(:top ,top))
		  (and width `(:width ,width))
		  (and height `(:height ,height)))))
  (let ((geometry (getf options :geometry)))
    (when (and (eq user-specified-position-p :unspecified)
	       (or (and (getf geometry :left)
			(getf geometry :top))
		   (and left top)))
      (setf user-specified-position-p t))
    (when (and (eq user-specified-size-p :unspecified)
	       (or (and (getf geometry :width)
			(getf geometry :height))
		   (and width height)))
      (setf user-specified-size-p t)))
  (with-keywords-removed (options options 
			  '(:frame-class :pretty-name :enable :save-under
			    :left :top :right :bottom :width :height
			    :user-specified-position-p :user-specified-size-p))
    (let ((frame (apply #'make-instance
			frame-class
			:name frame-name
			;;--- Perhaps this should be a default-initarg?
			:pretty-name (or pretty-name
					 (title-capitalize (string frame-name)))
			:properties `(:save-under ,save-under)
			:user-specified-size-p user-specified-size-p
			:user-specified-position-p user-specified-position-p
			options)))
      (when enable 
	(enable-frame frame))
      frame)))

(defmethod enable-frame ((frame standard-application-frame))
  (unless (frame-manager frame)
    (error "Cannot enable a disowned frame ~S" frame))
  (destructuring-bind (&key left top width height &allow-other-keys)
      (frame-geometry frame)
    (ecase (frame-state frame)
      (:enabled)
      ((:disabled :disowned)
       (let ((old-state (frame-state frame)))
	 (setf (frame-state frame) :enabled)
	 ;; If this is a new frame then if the user specified a width
	 ;; then we should be using that
	 ;; If the frame already exists then we probably should be using
	 ;; the top level sheet size
	 (multiple-value-bind (width height)
	     (ecase old-state
	       (:disowned 
		 (if (and width height)
		     (values width height)
		     (values)))
	       (:disabled
		 (bounding-rectangle-size
		   (frame-top-level-sheet frame))))  
	   (layout-frame frame width height)
	   (when (and left top)
	     (move-sheet (frame-top-level-sheet frame) left top))
	   (note-frame-enabled (frame-manager frame) frame))))
      (:shrunk 
        (note-frame-deiconified (frame-manager frame) frame)))))

(defmethod iconify-frame ((frame standard-application-frame))
  (note-frame-iconified (frame-manager frame) frame))

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

(defmethod raise-frame ((frame standard-application-frame))
  (raise-sheet (frame-top-level-sheet frame)))

(defmethod bury-frame ((frame standard-application-frame))
  (bury-sheet (frame-top-level-sheet frame)))

(defmethod note-frame-enabled 
	   ((framem standard-frame-manager) (frame standard-application-frame))
  )

(defmethod note-frame-disabled 
	   ((framem standard-frame-manager) (frame standard-application-frame))
  )

(defmethod note-frame-iconified 
	   ((framem standard-frame-manager) (frame standard-application-frame))
  )

(defmethod note-frame-deiconified
	   ((framem standard-frame-manager) (frame standard-application-frame))
  )


(defgeneric run-frame-top-level (frame &key &allow-other-keys))

;;--- It would be nice to have the CLIM 0.9 START-FRAME and STOP-FRAME functions
(defmethod run-frame-top-level :around ((frame standard-application-frame) &key)
  (with-simple-restart (frame-exit "Exit ~A" (frame-pretty-name frame))
    (unwind-protect
	(let (;; Reset the state of the input editor and the presentation
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
	      (*accelerator-numeric-argument* nil)
	      (*input-context* nil)
	      (*accept-help* nil)
	      (*assume-all-commands-enabled* nil)
	      (*sizing-application-frame* nil)
	      (*frame-layout-changing-p* *frame-layout-changing-p*)
	      (*command-parser* 'command-line-command-parser)
	      (*command-unparser* 'command-line-command-unparser)
	      (*partial-command-parser*
		'command-line-read-remaining-arguments-for-partial-command) 
	      (*application-frame* frame))
	  (with-frame-manager ((frame-manager frame))
	    (loop
	      (with-simple-restart (nil "~A top level" (frame-pretty-name frame))
		(loop
		  (catch 'layout-changed
		    (let ((*application-frame* frame))
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
			(return-from run-frame-top-level (values-list results))))))))))
      ;; We disable the frame here, but it is the responsibility of the
      ;; top-level function to enable the frame.  For example, if we
      ;; called ENABLE-FRAME here, ACCEPTING-VALUES would disable the
      ;; wrong frame.  Sigh.
      (disable-frame frame))))

(defmethod run-frame-top-level ((frame standard-application-frame) &rest args)
  (declare (dynamic-extent args))
  (with-slots (top-level-process) frame
    (when top-level-process
      (cerror "Bludgeon ahead, assuming the risk"
	      "The process ~S is already running the top-level function for frame ~S"
	      top-level-process frame))
    (unwind-protect
	(let* ((top-level (frame-top-level frame))
	       (tl-function (if (listp top-level) (first top-level) top-level))
	       (tl-args (if (listp top-level) (rest top-level) nil)))
	  (setq top-level-process (current-process))
	  ;; Cons as little as possible
	  (cond ((and (null args) (null tl-args))
		 (funcall tl-function frame))
		((null tl-args)
		 (apply tl-function frame args))
		((null args)
		 (apply tl-function frame tl-args))
		(t
		 (apply tl-function frame (append args tl-args)))))
      (setq top-level-process nil))))

(defmethod default-frame-top-level ((frame standard-application-frame)
				    &key command-parser command-unparser
					 partial-command-parser
					 (prompt "Command: "))
  ;; Enable the frame now
  (unless (eq (frame-state frame) :enabled)
    (enable-frame frame))
  (loop
    (let* ((*standard-output*
	     (or (frame-standard-output frame) *standard-output*))
	   (*standard-input* 
	     (or (frame-standard-input frame) *standard-output*))
	   (*query-io* 
	     (or (frame-query-io frame) *standard-input*))
	   (*error-output* 
	     (or (frame-error-output frame) *standard-output*))
	   (*pointer-documentation-output*
	     (frame-pointer-documentation-output frame))
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
	     ;;--- We have to ask the frame since we do not want to
	     ;;--- just pick up a stream from the dynamic environment
	     (let ((si (or (frame-standard-input frame)
			   (frame-standard-output frame))))
	       ;;--- I'm not really convinced that this is right  --SWM
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
	      (execute-frame-command frame command))))))))

;; Generic because someone might want :BEFORE or :AFTER
(defmethod frame-exit ((frame standard-application-frame))
  (invoke-restart 'frame-exit))


;;; Sizing and moving of frames

;; Sizes an application frame based on the size of the contents of the 
;; output recording stream STREAM.
(defun size-frame-from-contents (stream 
				  &key width height
				       (right-margin 10) (bottom-margin 10)
				       (size-setter #'window-set-inside-size))
  (with-slots (output-record) stream
    (with-bounding-rectangle* (left top right bottom) output-record
      (let* ((graft (or (graft stream)
			(find-graft)))		;--- is this right?
	     (gw (bounding-rectangle-width (sheet-region graft)))
	     (gh (bounding-rectangle-height (sheet-region graft)))
	     ;;--- Does this need to account for the size of window decorations?
	     (width (min gw (+ (or width (- right left)) right-margin)))
	     (height (min gh (+ (or height (- bottom top)) bottom-margin))))
	;; The size-setter will typically resize the entire frame
	(funcall size-setter stream width height)
	(window-set-viewport-position stream left top)))))

;; Moves the sheet to the specified position, taking care not to move
;; it outside of the graft.  It's safest to use this on a top-level sheet.
(defun position-sheet-carefully (sheet x y)
  (multiple-value-bind (width height) (bounding-rectangle-size sheet)
    (multiple-value-bind (graft-width graft-height)
	(bounding-rectangle-size (or (graft sheet) (find-graft)))
      (let* ((left x)
	     (top y)
	     (right (+ left width))
	     (bottom (+ top height)))
	(when (> right graft-width)
	  (setq left (- graft-width width)))
	(when (> bottom graft-height)
	  (setq top (- graft-height height)))
	(move-sheet sheet (max 0 left) (max 0 top))))))

;; Moves the sheet to be near where the pointer is.  It's safest to use this
;; on a top-level sheet.
(defun position-sheet-near-pointer (sheet &optional x y)
  (unless (and x y)
    (multiple-value-setq (x y)
      (pointer-native-position (port-pointer (port sheet)))))
  (position-sheet-carefully sheet x y))


(defun display-title (frame stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (with-slots (display-string) stream
    (let ((title display-string))
      (when (and (stringp title)
		 (not (string-equal title (frame-pretty-name frame))))
	;; On some hosts this will update the title bar 
	(setf (frame-pretty-name frame) title))
      (multiple-value-bind (width height)
	  (window-inside-size stream)
	(draw-text* stream (or title (frame-pretty-name frame))
		    (/ width 2) (/ height 2)
		    :align-x :center :align-y :center)))))

(defun title-capitalize (string)
  (let ((new-string (substitute #\Space #\- string)))
    (when (eq new-string string)
      (setq new-string (copy-seq new-string)))
    (nstring-capitalize new-string)))

(defun display-command-menu (frame stream &rest keys
			     &key command-table max-width max-height &allow-other-keys)
  (declare (dynamic-extent keys)
	   (ignore max-width max-height))
  (when (or (null command-table)
	    (eq command-table t))
    (setq command-table (frame-command-table frame)))
  (setq command-table (find-command-table command-table))
  (with-keywords-removed (keys keys '(:command-table))
    (if (slot-value stream 'incremental-redisplay-p)
	(apply #'display-command-menu-1 stream command-table keys)
	(apply #'display-command-table-menu command-table stream keys))))

;; Split out to avoid consing unnecessary closure environments.
(defun display-command-menu-1 (stream command-table &rest keys)
  (declare (dynamic-extent keys))
  (updating-output (stream :unique-id stream
			   :cache-value (slot-value command-table 'menu-tick))
    (apply #'display-command-table-menu command-table stream keys)))

(defmethod find-pane-named ((frame standard-application-frame) pane-name &optional (errorp t))
  (with-slots (all-panes) frame
    (cond ((second (assoc pane-name all-panes)))
	  (errorp (error "There is no pane named ~S in frame ~S" pane-name frame)))))

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
	(error "There is no CLIM stream pane named ~S in frame ~S" pane-name frame)))))

(defmethod redisplay-frame-panes ((frame standard-application-frame) &key force-p)
  ;; First display all the :accept-values panes, then display the rest.
  ;; We do this to ensure that all side-effects from :accept-values panes
  ;; have taken place.
  (map-over-sheets #'(lambda (sheet)
		       (when (typep sheet 'accept-values-pane)
			 (redisplay-frame-pane frame sheet :force-p force-p)))
		   (frame-top-level-sheet frame))
  (map-over-sheets #'(lambda (sheet)
		       (when (and (typep sheet 'clim-stream-pane)
				  (not (typep sheet 'accept-values-pane)))
			 (redisplay-frame-pane frame sheet :force-p force-p)))
		   (frame-top-level-sheet frame))
  ;; Once we've redisplayed everything, the layout is done changing
  (setq *frame-layout-changing-p* nil))

(defmethod redisplay-frame-pane ((frame standard-application-frame) pane &key force-p)
  (when (symbolp pane)
    (setq pane (get-frame-pane frame pane)))
  (let* ((display-function (pane-display-function pane))
	 (ir (slot-value pane 'incremental-redisplay-p))
	 (redisplay-p (if (listp ir) (first ir) ir))
	 (check-overlapping (or (atom ir)	;default is T
				(getf (rest ir) :check-overlapping t))))
    (with-simple-restart (nil "Skip redisplaying pane ~S" pane)
      (loop
	(with-simple-restart (nil "Retry displaying pane ~S" pane)
	  (when *frame-layout-changing-p*
	    (setq force-p t))
	  (unless *sizing-application-frame*
	    (unless (member pane (slot-value frame 'initialized-panes))
	      (setq force-p t)
	      (push pane (slot-value frame 'initialized-panes))))
	  (return
	    (cond (display-function
		   (cond (redisplay-p
			  (let ((redisplay-record
				  (let ((history (stream-output-history pane)))
				    (when history
				      #+compulsive-redisplay
				      (when (> (output-record-count history :fastp t) 1)
					(cerror "Clear the output history and proceed"
						"There is more than one element in this redisplay pane")
					(window-clear pane))
				      (unless (zerop (output-record-count history :fastp t))
					(output-record-element history 0))))))
			    (cond ((or (null redisplay-record) force-p)
				   (when force-p
				     (window-clear pane))
				   (invoke-pane-redisplay-function frame pane))
				  (t 
				   (redisplay redisplay-record pane 
					      :check-overlapping check-overlapping)))))
			 ((or force-p (pane-needs-redisplay pane))
			  (multiple-value-bind (needs-display clear)
			      (pane-needs-redisplay pane)
			    (declare (ignore needs-display))
			    (when (or force-p clear)
			      (window-clear pane)))
			  (invoke-pane-display-function frame pane)))
		   (force-output pane))
		  (force-p
		   ;; If refilling from scratch, give the application a chance
		   ;; to draw stuff
		   (frame-replay frame pane))
		  ;; Otherwise do nothing, the bits will still be on display
		  ;; and will be refreshed properly if there's a damage event.
		  )))))))

;; Factored out of the above to avoid consing closures
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
			 
;;; The contract of this is to replay the contents of STREAM within the region.
(defmethod frame-replay ((frame standard-application-frame) stream &optional region)
  (stream-replay stream region)
  (force-output stream))


(defmethod read-frame-command ((frame standard-application-frame) 
			       &key (stream *standard-input*)	;--- FRAME-STANDARD-INPUT?
			       ;; should the rest of the *command-parser*
			       ;; etc. variables be passed as keywords or bound?
			       )
  (read-command (frame-command-table frame) :stream stream))

#+++ignore	;--- install this?
(defmethod execute-frame-command :around ((frame standard-application-frame) command)
  (let ((top-level-process (slot-value frame 'top-level-process)))
    (if (and top-level-process
	     (eq top-level-process (current-process)))
	;; If we're in the process for the frame, just execute the command
	(call-next-method)
	;; Otherwise arrange to run the command in the frame's process
	(execute-command-in-frame frame command))))

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


;;--- There is the compiler bug with (eval-when (compile load eval) ...)
;;--- DEFMETHOD with a CALL-NEXT-METHOD.  See to incorporate this patch.
(eval-when (#-Allegro compile load eval)
(define-condition synchronous-command-event ()
  ((command :initarg :command :reader synchronous-command-event-command))
  (:report (lambda (condition stream)
	     (format stream "Command event condition signalled for ~S"
	       (synchronous-command-event-command condition)))))
)	;eval-when

(defvar *reading-frame-command* nil)

(defmethod read-frame-command :around ((frame standard-application-frame) &key)
  (let* ((command (queue-pop (frame-command-queue frame))))
    (or command 
	(handler-bind ((synchronous-command-event
			 #'(lambda (c)
			     (return-from read-frame-command
			       (synchronous-command-event-command c)))))
	  (let ((*reading-frame-command* t))
	    (call-next-method))))))
	
;;--- Actually this should be named COMMAND-EVENT
(defclass presentation-event (event)
    ((value :initarg :value :reader presentation-event-value)
     (sheet :initarg :sheet :reader event-sheet)
     (frame :initarg :frame :reader event-frame)
     (queuep :initarg :queuep :initform nil)
     (presentation-type :initarg :presentation-type :reader event-presentation-type))
  (:default-initargs :presentation-type 'command))

(defmethod handle-event (sheet (event presentation-event))
  (process-command-event sheet event))

(defun process-command-event (sheet event)
  ;;--- This code is as bad as I feel.
  (let ((command (presentation-event-value event)))
    (if (partial-command-p command)
	(throw-highlighted-presentation
	 (make-instance 'standard-presentation
			:object command
			:type (event-presentation-type event))
	 *input-context*
	 (allocate-event 'pointer-button-press-event
			 :sheet sheet
			 :x 0 :y 0
			 :modifier-state 0
			 :button +pointer-left-button+))
      (let ((frame (event-frame event))
	    activity)
	(when (and *input-buffer-empty*
		   (or (eq *application-frame* frame)
		       (and (typep frame 'activity-frame)
			    (eq (frame-activity frame) *activity*)
			    (setq activity *activity*))))
	  (signal 'synchronous-command-event
		  :command command))
	;; Perhaps if this results directly from a user action then either
	;; we should do it right away, ie. lose the input buffer or beep if
	;; it has to be deferred,
	(if (slot-value event 'queuep)
	    (queue-frame-command (if activity (activity-active-frame activity) frame)
				 (presentation-event-value event))
	  (beep sheet))))))

(defun queue-frame-command (frame command)
  (queue-push (frame-command-queue frame) command))

(defmethod execute-command-in-frame 
	   ((frame standard-application-frame) command &rest initargs)
  (declare (dynamic-extent initargs))
  (distribute-event
    (port frame)
    (apply #'allocate-event 'presentation-event 
 	   :frame frame
 	   :sheet (frame-top-level-sheet frame) 
 	   :value command 
 	   initargs)))

(defun make-command-timer (frame command 
			   &key (delay 0) interval
				(command-table (frame-command-table frame)))
  (flet ((queue-command-event (timer)
	   (declare (ignore timer))
	   (execute-command-in-frame 
	     frame command
	     :queuep t
	     :presentation-type `(command :command-table ,command-table))))
    (let ((timer (make-timer
		   :function #'queue-command-event
		   :delay delay :interval interval)))
      (add-timer timer)
      timer)))


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
  (find-frame-pane-of-type frame 'pointer-documentation-pane))

;;--- This causes direct-manipulation and menu-driven applications not to
;;--- maintain histories.  Is there a better heuristic?
(defmethod frame-maintain-presentation-histories ((frame standard-application-frame))
  (not (null (find-frame-pane-of-type frame 'interactor-pane))))

(defmethod notify-user (frame message &rest options) 
  (declare (dynamic-extent options))
  (when frame
    (setf (getf options :frame) frame))
  (apply #'frame-manager-notify-user 
	 (if frame (frame-manager frame) (find-frame-manager)) message options))

(defmethod select-file (frame &rest options) 
  (declare (dynamic-extent options))
  (when frame
    (setf (getf options :frame) frame))
  (apply #'frame-manager-select-file
	 (if frame (frame-manager frame) (find-frame-manager)) options))


;;; Pointer documentation

(defvar *pointer-documentation-interval*
	(max (floor (* 1/10 internal-time-units-per-second)) 1))
(defvar *last-pointer-documentation-time* 0)

;;; Produce pointer documentation
(defmethod frame-document-highlighted-presentation
	   ((frame standard-application-frame) presentation input-context window x y stream)
  (frame-manager-display-pointer-documentation
    (frame-manager frame) frame presentation input-context window x y stream))

(defmethod frame-manager-display-pointer-documentation
	   ((framem standard-frame-manager)
	    frame presentation input-context window x y stream)
  (when stream
    ;; The documentation should never say anything if we're not over a presentation
    (when (null presentation) 
      (window-clear stream))
    ;; Cheap test to not do this work too often
    (let ((old-modifier-state *last-pointer-documentation-modifier-state*)
	  (modifier-state (window-modifier-state window))
	  (last-time *last-pointer-documentation-time*)
	  (time (get-internal-real-time)))
      (setq *last-pointer-documentation-modifier-state* modifier-state)
      (when (and (< time (+ last-time *pointer-documentation-interval*))
		 (= modifier-state old-modifier-state))
	(return-from frame-manager-display-pointer-documentation nil))
      (setq *last-pointer-documentation-time* time))
    (when presentation
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

(defmethod port-note-frame-adopted ((port port) (frame standard-application-frame))
  nil)

