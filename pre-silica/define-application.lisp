;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: define-application.lisp,v 1.6 92/07/01 15:48:31 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 International Lisp Associates."

(defvar *window-stream-to-frame-table* (make-hash-table))

(defun-inline window-stream-to-frame (window-stream)
  (gethash window-stream *window-stream-to-frame-table*))

(defun create-application-frame (frame &rest args)
  (declare (dynamic-extent args))
  (let* ((window-stream
	   (apply #'open-window-stream args)))
    (setf (gethash window-stream *window-stream-to-frame-table*) frame)
    window-stream))

(define-protocol-class application-frame ())

(defclass standard-application-frame
	  (application-frame)
     ((name :initarg :name :accessor frame-name)
      (pretty-name :initarg :pretty-name :accessor frame-pretty-name)
      (current-layout :initarg :current-layout
		      :reader frame-current-layout)
      (top-level-sheet :initarg :top-level-sheet :initform nil
		       :accessor frame-top-level-sheet)
      (panes :initarg panes :initform nil :accessor frame-panes)
      ;; This is a copy of what's in the frame-descriptor, so that local
      ;; state can be cached here.  It's length and order matches PANES.
      (pane-descriptions :initarg :pane-descriptions :initform nil)
      ;; The real panes in the current layout
      (current-panes :initform nil :accessor frame-current-panes)
      (initialized-panes :initform nil)
      (command-table :initarg :command-table :initform nil
		     :accessor frame-command-table)
      (disabled-commands :initarg :disabled-commands :initform nil)
      ;; A slot for per-application histories
      (histories :initform nil)))

#+CLIM-1-compatibility
(define-compatibility-function (frame-top-level-window frame-top-level-sheet)
			       (frame)
  (frame-top-level-sheet frame))

(defmethod (setf frame-pretty-name) :after (name (frame standard-application-frame))
  (with-slots (top-level-sheet) frame
    (when #+Genera (not (typep top-level-sheet 'sheet-implementation-mixin))
	  #-Genera t
      (setf (window-label top-level-sheet) name))))

(eval-when (eval compile load)

(defstruct (frame-descriptor (:print-function print-frame-descriptor))
  name
  state-variables
  state-variable-names
  pane-descriptions
  layout
  top-level
  command-table
  disabled-commands)

(defun print-frame-descriptor (descriptor stream depth)
  (declare (ignore depth))
  (print-unreadable-object (descriptor stream :type t :identity t)
    (write (frame-descriptor-name descriptor) :stream stream :escape nil)))

;; Where is the LIST* DEFSTRUCT type when we need it?
(defun-inline pane-descriptor-name    (pane-descriptor) (first pane-descriptor))
(defun-inline pane-descriptor-type    (pane-descriptor) (second pane-descriptor))
(defun-inline pane-descriptor-options (pane-descriptor) (cddr pane-descriptor))
(defsetf pane-descriptor-options      (pane-descriptor) (new-value)
  `(setf (cddr ,pane-descriptor) ,new-value))

(defvar *frame-descriptors* nil)

(defun collect-state-variable-names (state-variables)
  (let ((names nil))
    (dolist (var state-variables)
      (cond ((atom var) (push var names))
	    (t (push (first var) names))))
    (nreverse names)))

(defun construct-default-layout (pane-descriptions)
  (let ((the-layout nil))
    (dolist (entry pane-descriptions)
      (let ((window-name (pane-descriptor-name entry))
	    (default-size (getf (pane-descriptor-options entry) :default-size ':rest)))
	(push `(,window-name ,default-size) the-layout)))
    `((main (:column :rest ,@(nreverse the-layout))))))

(defun define-application-frame-1 (name state-variables pane-descriptions
				   &key top-level layout
					command-table disabled-commands)
  (unless layout
    (setq layout (construct-default-layout pane-descriptions)))
  (let* ((state-variable-names (collect-state-variable-names state-variables))
	 (command-table-name (first command-table))
	 (frame-descriptor
	   (make-frame-descriptor :name name
				  :state-variables state-variables
				  :state-variable-names state-variable-names
				  :pane-descriptions pane-descriptions
				  :layout layout
				  :top-level top-level
				  :command-table command-table-name
				  :disabled-commands disabled-commands)))
    (push-unique frame-descriptor *frame-descriptors*
		 :key #'frame-descriptor-name)
    ;; If we're going to be defining commands for this application frame,
    ;; make sure there's an command table lying around so that all other
    ;; code doesn't have to be defensive against its absence.
    (when command-table
      (apply #'define-command-table-1 command-table))
    frame-descriptor))

(defun find-frame-descriptor (frame)
  (when (application-frame-p frame)
    (setq frame (frame-name frame)))
  (find frame *frame-descriptors* :key #'frame-descriptor-name))

)

(defmacro define-application-frame (name superclasses slots &rest options)
  #+Genera (declare (zwei:indentation 1 25 2 3 3 1))
  (with-warnings-for-definition name define-application-frame
    (let (panes layout top-level command-definer command-table disabled-commands)
      (macrolet ((extract (name keyword default)
		   `(let ((entry (assoc ',keyword options)))
		      (cond ((null entry)
			     (setq ,name ,default))
			    (t
			     (assert (= (length entry) 2) (entry)
				     "The length of the option ~S must be 2" entry)
			     (setq ,name (second entry)
				   options (delete entry options)))))))
	(extract panes :panes nil)
	(extract layout :layout nil)
	(extract top-level :top-level '(default-frame-top-level))
	(extract command-definer :command-definer t)
	(extract command-table :command-table t)
	(extract disabled-commands :disabled-commands nil))
      (check-type name symbol)
      (check-type superclasses list)
      (check-type slots list)
      (check-type panes list)
      (check-type layout list)
      (check-type top-level list)
      (check-type disabled-commands list)
      (let* ((pane-descriptions (produce-pane-descriptions panes)))
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
	(warn-if-pane-descriptions-invalid name pane-descriptions)
	(warn-if-layout-invalid name layout pane-descriptions)
	;; The pane descriptions is really a form that evaluates to be the
	;; pane decriptions, but it's easier to deal with in the "raw" form.
	;; A little extra consing at compile-time won't kill us.
	(setq pane-descriptions
	      `(list ,@(mapcar #'(lambda (desc)
				   (let ((name (first desc))
					 (type (second desc))
					 (options (cddr desc)))
				     `(list ',name ',type ,@options)))
			       pane-descriptions)))
	`(progn
	   (eval-when (compile)
	     (when ',command-table
	       (setf (compile-time-property ',(first command-table) 'command-table-name) t))
	     (define-application-frame-1 ',name ',slots ,pane-descriptions
					 :layout ',layout
					 :top-level ',top-level
					 :command-table ',command-table))
	   (define-group ,name define-application-frame
	     (defclass ,name ,superclasses ,slots ,@options)
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
	     (define-application-frame-1 ',name ',slots ,pane-descriptions
					 :layout ',layout
					 :top-level ',top-level
					 :command-table ',command-table
					 :disabled-commands ',disabled-commands)
	     #+Cloe-Runtime
	     (cloe:define-program ,name ()
	       :main ,name
	       :debugger-hook cloe-debugger-hook)))))))

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
	 (warn "The :COMMAND-TABLE option for frame ~S, ~S, is invalid.~@
		It is supposed to be a list of a command table name followed by ~
		keyword/value pairs."
	       frame-name command-table))
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
		       (warn "The :INHERIT-FROM keyword in the :COMMAND-TABLE option for ~
			      frame ~S~@
			      is followed by a list containing ~S, which is not a ~
			      command table nor a command table name."
			     frame-name item)))
		   (warn "The :INHERIT-FROM keyword in the :COMMAND-TABLE option for ~
			  frame ~S~@
			  is followed by ~S, which is not a list of command tables ~
			  or command table names."
			 frame-name value)))
	     (:menu
	       (if (listp value)
		   (dolist (clause value)
		     (unless (and (listp clause)
				  (>= (length clause) 3)
				  (oddp (length clause))
				  (stringp (first clause))
				  (member (second clause)
					  '(:command :function :menu :divider)))
		       (warn "The :MENU keyword in the :COMMAND-TABLE option for frame ~S~@
			      is followed by a list of menu clauses containing~@
			      an invalid clause ~S."
			     frame-name clause)))
		   (warn "The :MENU keyword in the :COMMAND-TABLE option for frame ~S~@
			  is followed by ~S, which is not a list of menu clauses."
			 frame-name value)))
	     (otherwise
	       (warn "The keyword ~S in the :COMMAND-TABLE option for frame ~S is invalid.~@
		      The valid keywords are :INHERIT-FROM and :MENU."
		     keyword frame-name)))))))

;;; The args just get passed through to the specific application.
(defun make-application-frame (frame-name &rest args
			       &key frame-class pretty-name &allow-other-keys)
  (declare (arglist frame-name
		    &key frame-class pretty-name
		    ;; initargs to application-frame that are
		    ;; not supplied in the make-instance below
		    parent left top right bottom height width
		    ;panes		;i don't think we want to mention this
		    ;top-level-sheet 	;..
		    ;; this is here because application frame classes
		    ;; can define additional initialization arguments,
		    ;; so we can't predict the full set of keyword
		    ;; arguments allowed in a particular call.
		    &allow-other-keys))
  (declare (dynamic-extent args))
  (check-type pretty-name (or null string))
  (let* ((descriptor (find-frame-descriptor frame-name))
	 pane-descriptions current-layout)
    (assert descriptor nil "unknown application frame: ~s" frame-name)
    (setq pane-descriptions (frame-descriptor-pane-descriptions descriptor)
	  current-layout (caar (frame-descriptor-layout descriptor)))
    (when (null frame-class) (setq frame-class frame-name))
    (with-keywords-removed (new-args args '(:frame-class :pretty-name))
      (apply #'make-instance frame-class
	     :name frame-name
	     :pretty-name (or pretty-name
			      (getf (pane-descriptor-options
				      (find :title pane-descriptions
					    :key #'pane-descriptor-type))
				    :display-string)
			      (title-capitalize (string frame-name)))
	     :pane-descriptions (copy-tree pane-descriptions)
	     :current-layout current-layout
	     :command-table (frame-descriptor-command-table descriptor)
	     :disabled-commands (copy-list (frame-descriptor-disabled-commands descriptor))
	     new-args))))

;;; PANE-TYPE-OPTIONS is how you define a new pane type.
(defmethod pane-type-options ((type (eql :command-menu)))
  `(:default-text-style *command-table-menu-text-style*
    :incremental-redisplay t
    :display-function 'display-command-menu
    :display-after-commands t
    :default-size :compute
    :scroll-bars nil))

;;--- Should this use incremental redisplay?
(defmethod pane-type-options ((type (eql :title)))
  `(:default-text-style (parse-text-style '(:sans-serif :bold :very-large))
    :display-function 'display-title
    :display-after-commands nil
    :default-size :compute
    :scroll-bars nil))

(defmethod pane-type-options ((type (eql :interactor)))
  `(:scroll-bars :vertical
    :initial-cursor-visibility :off))

(defmethod pane-type-options ((type (eql :application)))
  `(:default-size :rest))

(defmethod pane-type-options ((type (eql :pointer-documentation)))
  `(:default-text-style (parse-text-style '(:sans-serif :bold :normal))
    :display-after-commands nil
    :default-size :compute
    :scroll-bars nil))

(defun produce-pane-descriptions (panes)
  (let ((descriptions nil))
    (dolist (window-info panes (nreverse descriptions))
      ;; We really need destructuring-bind, but until there are options we can wait
      (let ((name (first window-info))
	    (type (second window-info))
	    (options (cddr window-info)))
	(push `(,name ,type ,@options ,@(pane-type-options type)) descriptions)))))

(defun-inline dummy-pane-p (pane)
  (symbolp pane))

(defvar *non-window-pane-keywords* 
	'(:width :height :default-size :height-in-lines
	  :display-after-commands :incremental-redisplay
	  :display-function :display-string :save-under))

;;; This function simply creates a bunch of windows with dummy size.
;;; LAYOUT-FRAME-PANES actually does the laying out of the windows.
(defun create-windows-from-description (frame descriptions root
					&key left top right bottom width height
					     save-under)
  (when descriptions		;suppose no windows...
    (let ((p-left 0) (p-top 0) (p-right nil) (p-bottom nil))
      (multiple-value-setq (p-right p-bottom)
	(window-inside-size root))
      ;; This is the same definition as is macroleted in open-window-stream.  You
      ;; could argue that it should be pulled out and published.
      (macrolet ((rationalize-dimensions (low high delta direction)
		   (flet ((parent-name (name) (fintern "~A-~A" 'p name)))
		     (let ((p-low (parent-name low))
			   (p-high (parent-name high)))
		       `(progn 
			  (assert (not (and ,low ,high ,delta
					    (/= ,delta (- ,high ,low))))
				  (,low ,high ,delta)
				  ,(format
				     nil
				       "The ~A dimensions of this window are overconstrained."
				     direction))
			  (when ,low (setf ,p-low ,low))
			  (when ,high (setf ,p-high ,high))
			  (when ,delta (if ,high
					   (setf ,p-low (- ,p-high ,delta))
					   (setf ,p-high (+ ,p-low ,delta))))
			  (setf ,low ,p-low ,high ,p-high ,delta (- ,p-high ,p-low)))))))
      (rationalize-dimensions left right width "horizontal")
      (rationalize-dimensions top bottom height "vertical")))
    (let* ((genera-p #+Genera (typep root 'sheet-window-stream)
		     #-Genera nil)
	   ;; On non-Genera window systems, use the title bar that those window
	   ;; systems provide instead of a title pane.
	   (title-pane-p (and genera-p
			      (find :title descriptions :key #'pane-descriptor-type)))
	   ;; On Genera, only label the frame if there is no title pane.  On other
	   ;; window systems, the label will appear in the title bar.
	   (label (if genera-p
		      (if title-pane-p nil (frame-pretty-name frame))
		      (frame-pretty-name frame)))
	   ;; Only give the frame borders when there is no title pane.
	   (borders (not title-pane-p))
	   (main (create-application-frame frame
					   :parent root :left left :top top
					   :width (- right left) :height (- bottom top)
					   :label label :borders borders
					   :scroll-bars nil :save-under save-under))
	   (input-buffer (stream-input-buffer main))
	   (panes nil))
      (dolist (desc descriptions)
	(let ((type (pane-descriptor-type desc))
	      (options (pane-descriptor-options desc)))
	  (with-keywords-removed (new-options options *non-window-pane-keywords*)
	    (cond ((and genera-p (eq type ':pointer-documentation))
		   ;; On Genera, use the normal mouse-doc line
		   (push 'dummy-pointer-documentation-pane panes))
		  ((and (not genera-p) (eq type ':title))
		   ;; Most window systems put the title in the window decorations
		   (push 'dummy-title-pane panes))
		  (t
		   (push (apply #'open-window-stream :parent main :left 0 :top 0
				:width 100 :height 100
				:input-buffer input-buffer
				new-options)
			 panes))))))
      (values main (nreverse panes)))))

(defun warn-if-pane-descriptions-invalid (frame-name descriptions)
  (let ((valid-keywords
	  ;; We will instantiate a port-dependent subclass of this class, but we only care
	  ;; about initialization arguments that are accepted by all subclasses.
	  (let ((class (find-class 'window-stream)))
	    ;; It's probably a bug that class-make-instance-keywords doesn't do this itself
	    #+(or Genera Cloe-Runtime) (unless (clos-internals::class-finalized-p class)
					 (clos-internals::finalize-inheritance class))
	    ;; In Symbolics CLOS this function returns the initialization arguments accepted
	    #+(or Genera Cloe-Runtime) (clos-internals::class-make-instance-keywords class)
	    ;; There is no standard for this, so in systems where we don't know how to do it,
	    ;; just punt.
	    #-(or Genera Cloe-Runtime) (return-from warn-if-pane-descriptions-invalid nil)))
	(non-window-keywords *non-window-pane-keywords*))
    (dolist (desc descriptions)
      (let (;; Surprisingly, the valid options don't depend on the pane type
	    ;(type (pane-descriptor-type desc))
	    (options (pane-descriptor-options desc))
	    (invalid-keywords nil))
	(do* ((options options (cddr options))
	      (keyword (car options) (car options)))
	     ((null options))
	  (unless (or (eq keyword ':window-class)
		      (member keyword non-window-keywords)
		      (member keyword valid-keywords))
	    (push keyword invalid-keywords)))
	(when invalid-keywords
	  (setq invalid-keywords (nreverse invalid-keywords))
	  (warn "The option~P ~{~S~^, ~}~@
		 specified for pane ~S of frame ~S ~:[is~;are~] not valid.~@
		 Known options are ~S."
		(length invalid-keywords) invalid-keywords
		(pane-descriptor-name desc) frame-name (cdr invalid-keywords)
		(sort (append valid-keywords non-window-keywords '(:window-class) nil)
		      #'string<)))))))

(defvar *frame-layout-changing-p* nil)

;;; This used to be an after method, unconditionally setting the layout.
;;; Now, make it an around method that only recomputes the layout if the size changes,
;;; not if only the position changes.  (Eliminates some unnecessary redisplays.)
#-Silica
(defmethod window-note-size-or-position-change :around ((window window-mixin)
							new-left new-top new-right new-bottom)
  (declare (type coordinate new-left new-top new-right new-bottom))
  (multiple-value-bind (width height) (bounding-rectangle-size window)
    (declare (type coordinate width height))
    (call-next-method window new-left new-top new-right new-bottom)
    (unless (and (= width (- new-right new-left))
		 (= height (- new-bottom new-top)))
      (let ((frame (window-stream-to-frame window)))
	(when frame
	  (let ((*application-frame* frame))
	    (catch 'resynchronize
	      (let ((*frame-layout-changing-p* t))
		;; Forcibly layout the frame again
		(layout-frame-panes frame (frame-top-level-sheet frame))))))))))

(defmethod (setf frame-current-layout) (new-layout (frame standard-application-frame))
  (setq *frame-layout-changing-p* t)
  (with-slots (current-panes initialized-panes current-layout) frame
    (setf current-layout new-layout)
    (layout-frame-panes frame (frame-top-level-sheet frame))
    ;; Forcibly display any panes that have not been displayed yet
    (dolist (pane current-panes)
      (unless (member pane initialized-panes)
	;; Display the pane and mark it initialized
	(redisplay-frame-pane frame pane :force-p t))))
  (unless (eq *application-frame* *default-application*)
    ;; must resynchronize even if the layout phase "didn't do anything"
    ;; because we might need to get streams rebound, etc.
    (throw 'resynchronize T)))

#+CLIM-1-compatibility
(define-compatibility-function (set-frame-layout (setf frame-current-layout))
			       (frame new-layout)
  (setf (frame-current-layout frame) new-layout))

;; This works in 2 phases.
;; The first phase figures out which windows need to be visible and where
;; the second phase does the work, doing as little as possible.  Quite often,
;; the size and position of some windows will not change, and there's no reason
;; to set their visibility to NIL and back to T in that case.
(defmethod layout-frame-panes ((frame standard-application-frame) window-frame)
  ;; Some applications have no top-level window.
  #-silica
  (when window-frame
    (let* ((*sizing-application-frame* t)
	   (descriptor (find-frame-descriptor frame))
	   (layout (frame-descriptor-layout descriptor))
	   (current-layout (frame-current-layout frame))
	   (this-layout (second (assoc current-layout layout))))
      (multiple-value-bind (available-x available-y)
	  (window-inside-size window-frame)
	(let ((windows-and-sizes nil))
	  (flet ((size-setter (name left top right bottom)
		   (setq left (floor left)
			 top (floor top)
			 right (floor right)
			 bottom (floor bottom))
		   (let ((pane (get-frame-pane frame name)))
		     (push (list pane left top right bottom) windows-and-sizes)))
		 (size-helper (name cluster-type available-width available-height)
		   (multiple-value-bind (pane description)
		       (get-frame-pane frame name)
		     (let ((amount
			     (if (dummy-pane-p pane)
				 0
			         (size-frame-pane pane frame 
						  (pane-descriptor-type description)
						  description
						  cluster-type
						  available-width available-height))))
		       (cond ((or (keywordp amount)
				  (dummy-pane-p pane)
				  (< 0 amount 1))	;for upward compatibility
			      amount)
			     (t
			      ;; the amount returned by SIZE-FRAME-PANE is inside size
			      ;; to save SIZE-FRAME-PANE method writers from having to
			      ;; understand margins.
			      (multiple-value-bind (lm tm rm bm)
				  (host-window-margins pane)
				(case cluster-type
				  (:row (+ amount lm rm))
				  (:column (+ amount tm bm))))))))))
	    (layout-frame-panes-1 this-layout 0 0 available-x available-y
					 #'size-setter #'size-helper))
	  ;; Now windows-and-sizes contains the size and position of each pane
	  (setq windows-and-sizes (nreverse windows-and-sizes))
	  ;; First set the sizes properly
	  (dolist (element windows-and-sizes)
	    (let ((pane (pop element)))
	      (unless (dummy-pane-p pane)
		(multiple-value-bind (left top right bottom)
		    (values-list element)
		  (with-bounding-rectangle* (pl pt pr pb) pane
		    (unless (ltrb-equals-ltrb-p pl pt pr pb left top right bottom)
		      (bounding-rectangle-set-edges pane left top right bottom)
		    ;; If the size changed, the pane isn't initialized anymore
		    (setf (slot-value frame 'initialized-panes)
			  (delete pane (slot-value frame 'initialized-panes)))))))))
	  ;; Then get the visibilities right
	  (dolist (inf (window-children window-frame))
	    (if (assoc inf windows-and-sizes)
		(unless (window-visibility inf) (setf (window-visibility inf) T))
		(when (window-visibility inf) (setf (window-visibility inf) nil))))
	  ;; Remember the panes in this layout for later
	  (setf (frame-current-panes frame)
		(delete nil (mapcar #'first windows-and-sizes))))))))

(defun layout-frame-panes-1 (layout-spec start-x start-y available-x available-y
			     size-setter size-helper)
  (declare (dynamic-extent size-setter size-helper))
  (setq layout-spec (copy-list layout-spec))	; so we can modify it
  (let ((parent-type (first layout-spec))
	(inferiors (cddr layout-spec))
	(calculated-inferiors nil))
    (dolist (inf inferiors)
      (let ((type-or-name (first inf))
	    (size-spec (second inf))
	    size)
	(cond ((eq size-spec :rest)
	       (setq size ':rest))
	      ((eq size-spec :compute)
	       (setq size (funcall size-helper type-or-name parent-type 
				   available-x available-y)))
	      ((and (numberp size-spec)
		    (<= 0 size-spec 1))
	       ;; A pane with a relative sizing parameter
	       (case parent-type
		 (:row
		   (setq size (* available-x size-spec)))
		 (:column
		   (setq size (* available-y size-spec)))))
	      (t
	       (cerror "Compute the size of the pane"
		       "The size specification ~S is unrecognized" size-spec)
	       (setq size (funcall size-helper type-or-name parent-type 
				   available-x available-y))))
	;; Save a copy of this inferior's spec, with the abstract size
	;; spec (e.g. :COMPUTE) replaced by a number.
	(let ((entry (copy-list inf)))
	  (setf (second entry) size)
	  (push entry calculated-inferiors))))
    (setq calculated-inferiors (nreverse calculated-inferiors))
    (let ((available (case parent-type
		       (:row available-x)
		       (:column available-y)))
	  (n-rests 0))
      ;; Compute left-over space after explicitly-sized panes are allocated
      (dolist (inf calculated-inferiors)
	(let ((size (second inf)))
	  (if (eq size ':rest)
	      (incf n-rests)
	      (decf available size))))
      (let ((rest-allocation (and (plusp n-rests) (floor available n-rests)))
	    constant-start adjusted-start constant-size adjusted-size)
	;; Attempt to write code that deals with allocating space along one
	;; dimension (either X or Y) while holding the other dimension constant.
	(case parent-type
	  (:row (setq constant-start start-y
		      adjusted-start start-x
		      constant-size  available-y))
	  (:column (setq constant-start start-x
			 adjusted-start start-y
			 constant-size  available-x)))
	;; Decode the "adjusted" and "constant" dimensions into actual X and Y
	;; depending on whether we are laying out a row or column
	(macrolet ((with-real-coords ((xvar yvar widthvar heightvar) &body body)
		     `(let (,xvar ,yvar ,widthvar ,heightvar)
			(case parent-type
			  (:row
			    (setq ,xvar adjusted-start
				  ,yvar constant-start
				  ,widthvar  adjusted-size
				  ,heightvar constant-size))
			  (:column
			    (setq ,yvar adjusted-start
				  ,xvar constant-start
				  ,heightvar adjusted-size
				  ,widthvar  constant-size)))
			,@body)))
	  (dolist (inf calculated-inferiors)
	    (let* ((name-or-type (first inf))
		   (size-info (second inf)))
	      ;; Bite off a chunk for this pane
	      (setq adjusted-size (if (eq size-info ':rest)
				      rest-allocation
				      size-info))
	      (with-real-coords (x y width height)
	        ;; If a sub-row or -column, lay it out
	        (cond ((keywordp name-or-type)
		       (layout-frame-panes-1
			 inf x y width height size-setter size-helper))
		      ;; must be a pane name
		      (t (funcall size-setter name-or-type x y
				  (+ x width) (+ y height))))
		;; Set where the new pane will start.
		(incf adjusted-start adjusted-size)))))))))

(defun warn-if-layout-invalid (frame-name layouts pane-descriptions)
  (dolist (layout layouts)
    (if (and (listp layout)
	     (= (length layout) 2)
	     (symbolp (first layout))
	     (listp (second layout))
	     (>= (length (second layout)) 3))
	(let ((layout-name (first layout)))
	  (labels ((check-layout-spec (layout-spec max-remaining)
		     (unless (and (listp layout-spec)
				  (>= (length layout-spec) 2))
		       (warn "~S appears where either a list of a pane name and size~@
			      or a list of :ROW or :COLUMN, size, and inferiors is expected~@
			      in layout ~S of frame ~S."
			     layout-spec layout-name frame-name)
		       (return-from check-layout-spec max-remaining))
		     (let* ((name-or-type (first layout-spec))
			    (aggregate (keywordp name-or-type))
			    (size-spec (second layout-spec))
			    (inferiors (cddr layout-spec)))
		       ;; Check name-or-type
		       (if aggregate
			   (unless (member name-or-type '(:row :column))
			     (warn "The keyword ~S appears where :ROW, :COLUMN, or a~@
				    pane name is expected, in layout ~S of frame ~S."
				   name-or-type layout-name frame-name))
			   (unless (assoc name-or-type pane-descriptions)
			     (warn "The undefined pane name ~S appears ~
				    in layout ~S of frame ~S."
				   name-or-type layout-name frame-name)))
		       ;; Check size-spec
		       (cond ((eq size-spec :rest))
			     ((and (not aggregate) (eq size-spec :compute)))
			     ((typep size-spec #-Cloe-Runtime '(real 0 1)
				               #+Cloe-Runtime '(or (rational 0 1)
								   (float 0 1)))
			      (decf max-remaining size-spec))
			     (aggregate
			      (warn "The invalid size specification ~S appears in~@
				     a ~S entry in layout ~S of frame ~S.~@
				     A valid size specification for a row or column is :REST~@
				     or a real number between 0 and 1 inclusive."
				    size-spec name-or-type layout-name frame-name))
			     (t
			      (warn "The invalid size specification ~S appears in~@
				     the entry for pane ~S in layout ~S of frame ~S.~@
				     A valid size specification for a pane is :REST,~@
				     :COMPUTE, or a real number between 0 and 1 inclusive."
				    size-spec name-or-type layout-name frame-name)))
		       ;; Check inferiors and check overall plausibility of size-specs
		       (cond ((not aggregate)
			      (unless (null inferiors)
				(warn "Extra garbage ~S appears after the size specification~@
				       in the entry for pane ~S in layout ~S of frame ~S."
				      inferiors name-or-type layout-name frame-name)))
			     ((not (consp inferiors))
			      (warn "~S appears where a list of inferiors is expected,~@
				     after the size specification~@
				     in a ~S entry in layout ~S of frame ~S."
				    inferiors name-or-type layout-name frame-name))
			     (t (check-layout-specs name-or-type inferiors)))
		       max-remaining))
		   (check-layout-specs (name-or-type inferiors)
		     (let ((max-remaining 1) (rest nil))
		       (dolist (layout-spec inferiors)
			 (when (and (listp layout-spec)
				    (symbolp (second layout-spec)))
			   (setq rest t))
			 (setq max-remaining (check-layout-spec layout-spec max-remaining)))
		       (cond ((< max-remaining 0)
			      (warn "~:[The top~;~:*A ~S~] entry in layout ~S of frame ~S~@
				     overcommitted the available space by ~S."
				    name-or-type layout-name frame-name
				    (- max-remaining)))
			     ((and rest (= max-remaining 0))
			      (warn "~:[The top~;~:*A ~S~] entry in layout ~S of frame ~S~@
				     overcommitted the available space.  ~
				     It didn't leave anything for :REST/:COMPUTE."
				    name-or-type layout-name frame-name))
			     ((and (not rest) (> max-remaining 0))
			      (warn "~:[The top~;~:*A ~S~] entry in layout ~S of frame ~S~@
				     failed to consume all available space.  ~
				     The fraction ~S was left over."
				    name-or-type layout-name frame-name
				    max-remaining))))))
	    (check-layout-specs nil (cdr layout))))
        (warn "~S appears where a list (layout-name (direction 1.0 spec...))~@
	       is expected, in the :LAYOUT option of frame ~S."
	      layout frame-name))))

;;; Returns one (1) value, the space taken up along the "available space" dimension.
#-silica
(defmethod size-frame-pane ((window window-mixin)
			    (frame standard-application-frame)
			    (type T)		;pane type
			    pane-description	;description of this window
			    cluster-type	;horizontal or vertical
			    available-width available-height)
  ;; default method (on T)
  pane-description cluster-type available-width available-height
  ':rest)

(defmethod size-frame-pane :around ((window window-mixin)
				    (frame standard-application-frame)
				    (type T)
				    pane-description 
				    cluster-type
				    available-width available-height)
  available-width available-height
  (let* ((options (pane-descriptor-options pane-description)) 
	 (nlines (getf options :height-in-lines))
	 (vsp (getf options :vertical-spacing (stream-vertical-spacing window)))
	 (size (call-next-method)))
    (case cluster-type
      (:row size)
      (:column
	(if nlines
	    (if (eq size ':rest)
		(+ 5 (* nlines (+ (stream-line-height window) vsp)))
		(max size (+ 5 (* nlines (+ (stream-line-height window) vsp)))))
	    size)))))

#-silica
(defmethod size-frame-pane ((window window-mixin)
			    (frame standard-application-frame)
			    (type (eql :title))
			    pane-description
			    cluster-type
			    available-width available-height)
  available-width available-height
  (let* ((options (pane-descriptor-options pane-description))
	 (vsp (getf options :vertical-spacing (stream-vertical-spacing window))))
    (case cluster-type
      (:row
	;; as wide as the title
	':rest)
      (:column 
	;; one line in current text style
	(+ 5 (+ vsp (stream-line-height window)))))))

#-silica
(defmethod size-frame-pane ((window window-mixin)
			    (frame standard-application-frame)
			    (type (eql :command-menu))
			    pane-description
			    cluster-type
			    available-width available-height)
  ;; Unconstrain the direction along which we are computing the size, so that
  ;; the menu will use the constraint in the orthogonal direction to compute
  ;; how much space it needs to occupy in this direction
  (case cluster-type
    (:row (setq available-width nil))
    (:column (setq available-height nil)))
  (let* ((options (pane-descriptor-options pane-description))
	 (vsp (getf options :vertical-spacing (stream-vertical-spacing window)))
	 (display-function (getf options :display-function #'display-command-menu))
	 (incremental-redisplay (getf options :incremental-redisplay))
	 ;; The menu cell for a disabled command is (sometimes) smaller than the
	 ;; menu cell for an enabled command, so make sure we allocate enough space
	 (*assume-all-commands-enabled* t)
	 (command-table (getf options :command-table))
	 (displayer (if incremental-redisplay
			#'call-redisplay-function
			#'call-display-function))
	 (menu-contents
	   (with-output-to-output-record (window)
	     (if command-table
		 (funcall displayer display-function frame window
			  :command-table command-table
			  :max-width available-width
			  :max-height available-height)
		 (funcall displayer display-function frame window
			  :max-width available-width
			  :max-height available-height)))))
    (multiple-value-bind (width height) (bounding-rectangle-size menu-contents)
      (case cluster-type
	(:row width)
	(:column (+ height vsp))))))

#-silica
(defmethod size-frame-pane ((window window-mixin)
			    (frame standard-application-frame)
			    (type (eql :application))
			    pane-description
			    cluster-type
			    available-width available-height)
  available-width available-height
  ;;--- This might want to see if the window already has stuff in it, no?
  ;;--- The display-function needs to produce a meaningful result when
  ;;--- the application frame is created.
  (let* ((options (pane-descriptor-options pane-description))
	 (display-function (getf options :display-function))
	 (incremental-redisplay (getf options :incremental-redisplay)))
    (if display-function
	(let ((window-contents
		(with-output-to-output-record (window)
		  (funcall (if incremental-redisplay
			       #'call-redisplay-function
			       #'call-display-function)
			   display-function frame window))))
	  (multiple-value-bind (width height)
	      (bounding-rectangle-size window-contents)
	    (case cluster-type
	      (:row width)
	      (:column height))))
        ':rest)))

#-silica
(defmethod size-frame-pane ((window window-mixin)
			    (frame standard-application-frame)
			    (type (eql :pointer-documentation))
			    pane-description
			    cluster-type
			    available-width available-height)
  available-width available-height
  (let* ((options (pane-descriptor-options pane-description))
	 (vsp (getf options :vertical-spacing (stream-vertical-spacing window))))
    (case cluster-type
      (:row
	;; as wide as the title
	':rest)
      (:column 
	;; one line in current text style
	(+ 2 (+ vsp (stream-line-height window)))))))

(defmethod initialize-instance :after ((frame standard-application-frame)
				       &key pane-descriptions parent
					    left top right bottom width height)
  ;; If parent is NIL, assume that the caller of MAKE-APPLICATION-FRAME is going
  ;; to take care of window initialization.
  (when parent
    (multiple-value-bind (main-window panes)
	(create-windows-from-description frame pane-descriptions parent
					 :left left :top top
					 :right right :bottom bottom
					 :width width :height height)
      (setf (slot-value frame 'top-level-sheet) main-window)
      (setf (slot-value frame 'panes) panes)
      (let ((*application-frame* frame))
	(layout-frame-panes frame main-window)))))

(defun get-frame-pane (frame pane-name &key (errorp t))
  (let* ((descriptor (find-frame-descriptor frame))
	 (position (if (typep pane-name 'window-mixin)
		       (position pane-name (frame-panes frame))
		       (position pane-name (frame-descriptor-pane-descriptions descriptor)
				 :key #'pane-descriptor-name))))
    (when (and (null position)
	       (dummy-pane-p pane-name))
      (setq position (position pane-name (frame-panes frame))))
    (if (null position)
	(when errorp
	  (error "There is no pane named ~S in frame ~S" pane-name frame))
        (let ((pane (elt (frame-panes frame) position))
	      (descr (elt (frame-descriptor-pane-descriptions descriptor) position)))
	  ;; Note that this can return dummy panes!
	  (values pane descr)))))

(defun title-capitalize (string)
  (let ((new-string (substitute #\Space #\- string)))
    (when (eq new-string string)
      (setq new-string (copy-seq new-string)))
    (nstring-capitalize new-string)))

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

(defun display-command-menu (frame stream &rest keys
			     &key command-table &allow-other-keys)
  (declare (dynamic-extent keys))
  (declare (arglist frame stream &rest keys
		    &key command-table
			 ;; Defaults for these provided by DISPLAY-COMMAND-TABLE-MENU
			 max-width max-height n-rows n-columns
			 (cell-align-x ':left) (cell-align-y ':top) 
			 (initial-spacing t)))
  ;;--- This really wants to get the information from some other place
  ;;--- than having to find the descriptor each time, which is kludgy at best.
  (multiple-value-bind (descriptor pane)
      (do ((panes (frame-panes frame) (cdr panes))
	   (descriptions (frame-descriptor-pane-descriptions
			   (find-frame-descriptor frame))
			 (cdr descriptions)))
	  ((null panes) nil)
	(when (eq (first panes) stream)
	  (return (values (first descriptions) (first panes)))))
    (when pane
      (let ((command-table
	      (find-command-table (or command-table (frame-command-table frame))))
	    (incremental-redisplay
	      (getf (pane-descriptor-options descriptor) :incremental-redisplay))
	    (*application-frame* frame))
	(with-keywords-removed (keys keys '(:command-table))
	  (if incremental-redisplay
	      (apply #'display-command-menu-1 stream command-table keys)
	      (apply #'display-command-table-menu command-table stream keys)))))))

;; Split out to avoid consing unnecessary closure environments.
(defun display-command-menu-1 (stream command-table &rest keys)
  (declare (dynamic-extent keys))
  (updating-output (stream :unique-id stream
			   :cache-value (slot-value command-table 'menu-tick))
    (apply #'display-command-table-menu command-table stream keys)))

(defmethod run-frame-top-level :around ((frame standard-application-frame))
  (with-simple-restart (frame-exit "Exit ~A" (frame-pretty-name frame))
    (with-slots (top-level-sheet) frame
      (unwind-protect
	  (let ((*frame-layout-changing-p* *frame-layout-changing-p*)
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
		(*generate-button-release-events* nil)
		(*numeric-argument* nil)
		(*delimiter-gestures* nil)
		(*activation-gestures* nil)
		(*accelerator-gestures* nil)
		(*input-context* nil)
		(*accept-help* nil)
		(*assume-all-commands-enabled* nil)
		(*sizing-application-frame* nil)
		(*command-parser* 'command-line-command-parser)
		(*command-unparser* 'command-line-command-unparser)
		(*partial-command-parser*
		  'command-line-read-remaining-arguments-for-partial-command))
	    (loop
	      (with-simple-restart (nil "~A top level" (frame-pretty-name frame))
		(when top-level-sheet
		  (window-expose top-level-sheet))
		(loop
		  (catch 'resynchronize
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
	(when top-level-sheet
	  (setf (window-visibility top-level-sheet) nil))))))

(defmethod run-frame-top-level ((frame standard-application-frame))
  (let* ((descriptor (find-frame-descriptor frame))
	 (top-level (frame-descriptor-top-level descriptor)))
    (apply (first top-level) frame (rest top-level))))

(defun find-pane-of-type (frame type &key (if-current t))
  (let ((current-panes (frame-current-panes frame)))
    (do ((panes (frame-panes frame) (cdr panes))
	 (descriptions (frame-descriptor-pane-descriptions (find-frame-descriptor frame))
		       (cdr descriptions)))
	((null panes) nil)
      (let* ((pane (first panes))
	     (descriptor (first descriptions)))
	(when (and (eq (pane-descriptor-type descriptor) type)
		   (or (null if-current)
		       (member pane current-panes)))
	  (return (values pane descriptor)))))))

(defmethod frame-standard-output ((frame standard-application-frame))
  (or (find-pane-of-type frame ':application)
      (find-pane-of-type frame ':interactor)))

(defmethod frame-standard-input ((frame standard-application-frame))
  (or (find-pane-of-type frame ':interactor)
      (frame-standard-output frame)))

(defmethod frame-query-io ((frame standard-application-frame))
  (or (frame-standard-input frame)
      (frame-standard-output frame)))

(defmethod frame-error-output ((frame standard-application-frame))
  (frame-standard-output frame))

(defmethod frame-pointer-documentation-output ((frame standard-application-frame))
  ;; If it's a Genera frame, use the mouse-doc line, otherwise find
  ;; the pointer documentation pane.
  (or #+Genera (let ((stream (frame-top-level-sheet frame)))
		 (and (typep stream 'sheet-window-stream)
		      (not (null (tv:screen-who-line-screen
				   (tv:sheet-screen (slot-value stream 'window)))))))
      (find-pane-of-type frame ':pointer-documentation)))

(defmethod read-frame-command ((frame standard-application-frame) 
			       &key (stream *query-io*)		;frame-query-io?
			       ;; should the rest of the *command-parser*
			       ;; etc. variables be passed as keywords or bound?
			       )
  (read-command (frame-command-table frame) :stream stream))

(defmethod execute-frame-command ((frame standard-application-frame) command)
  (apply (command-name command) (command-arguments command)))

;;--- This causes direct-manipulation and menu-driven applications not to
;;--- maintain histories.  Is there a better heuristic?
(defmethod frame-maintain-presentation-histories ((frame standard-application-frame))
  (not (null (find-pane-of-type frame ':interactor :if-current nil))))

;; Generic because someone might want :BEFORE or :AFTER
(defmethod frame-exit ((frame standard-application-frame))
  (invoke-restart 'frame-exit))

(defmethod notify-user ((frame standard-application-frame) format-string &rest format-args)
  (declare (dynamic-extent format-args))
  (apply #'notify-user-1 (frame-top-level-sheet frame) frame format-string format-args))

(defun default-frame-top-level (frame
				&key command-parser command-unparser partial-command-parser
				     (prompt "Command: "))
  (let* ((*standard-output*
	   (or (frame-standard-output frame) *standard-output*))
	 (*standard-input* 
	   (or (frame-standard-input frame) *standard-output*))
	 (*query-io* 
	   (or (frame-query-io frame) *standard-input*))
	 (*error-output* 
	   (or (frame-error-output frame) *standard-output*))
	 (interactor (not (null (find-pane-of-type frame ':interactor))))
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

    ;; The read-eval-print loop for applications...
    (loop
      ;; Redisplay all the panes
      (redisplay-frame-panes frame)
      (catch-abort-gestures ("Return to ~A command level" (frame-pretty-name frame))
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

(defmethod redisplay-frame-panes ((frame standard-application-frame) &key force-p)
  (with-slots (panes pane-descriptions current-panes) frame
    (do ((windows panes (cdr windows))
	 (descriptions pane-descriptions (cdr descriptions)))
	((null windows) nil)
      (let* ((window (first windows))
	     (description (first descriptions)))
	(when (member window current-panes)
	  (redisplay-frame-pane-1 frame window description force-p))))
    (setq *frame-layout-changing-p* nil)))

(defmethod redisplay-frame-pane ((frame standard-application-frame) pane-name &key force-p)
  (multiple-value-bind (pane description)
      (get-frame-pane frame pane-name)
    (redisplay-frame-pane-1 frame pane description force-p)))

;; FORCE-P can be NIL (meaning just do the redisplay normally) or T (meaning
;; forcibly redisplay the pane).
(defun redisplay-frame-pane-1 (frame pane description &optional force-p)
  (let* ((options (pane-descriptor-options description))
	 (display-function (getf options :display-function))
	 (ir (getf options :incremental-redisplay))
	 (redisplay-p (if (listp ir) (first ir) ir))
	 (check-overlapping (or (atom ir)	;default is T
				(getf (rest ir) :check-overlapping t)))
	 (redisplay-record
	   (and redisplay-p
		(let ((history (stream-output-history pane)))
		  (when history
		    #+compulsive-redisplay
		    (when (> (output-record-count history) 1)
		      (cerror "Clear the output history and proceed"
			      "There more than one output record in this redisplay pane")
		      (window-clear pane))
		    (unless (zerop (output-record-count history))
		      (output-record-element history 0)))))))
    ;; We're trying to get bits on the screen.
    (cond ((dummy-pane-p pane)
	   ;; Give everyone a chance to update the title bar
	   (when (eq (pane-descriptor-type description) :title)
	     (display-title frame pane)))
	  (t
	   (when *frame-layout-changing-p*
	     (setq force-p t))
	   (unless *sizing-application-frame*
	     (unless (member pane (slot-value frame 'initialized-panes))
	       (setq force-p t)
	       (push pane (slot-value frame 'initialized-panes))))
	   (with-simple-restart (nil "Skip redisplaying pane ~S" pane)
	     (loop
	       (with-simple-restart (nil "Retry displaying pane ~S" pane)
		 (return
		   (cond (display-function
			  ;; If there's a display function, call it to generate new contents.
			  (cond (redisplay-p
				 (cond ((or (null redisplay-record) force-p)
					(when force-p
					  (window-clear pane))
					(call-redisplay-function
					  display-function frame pane))
				       (t (redisplay redisplay-record pane
						     :check-overlapping check-overlapping))))
				((or force-p
				     (getf options :display-after-commands t))
				 (unless (and (not force-p)
					      (eq (getf options :display-after-commands)
						  ':no-clear))
				   (window-clear pane))
				 (call-display-function display-function frame pane)))
			  (force-output pane))
			 (force-p
			  ;; If refilling from scratch, give the application a chance
			  ;; to draw stuff
			  (frame-replay frame pane))
			 ;; Otherwise do nothing, the bits will still be on display
			 ;; and will be refreshed properly if there's a damage event.
			 )))))))))

;; Factored out of the above to avoid consing closures
(defun call-redisplay-function (display-function frame pane &rest args)
  (declare (dynamic-extent args))
  (updating-output (pane)
    (apply #'call-display-function display-function frame pane args)))

(defun call-display-function (display-function frame pane &rest args)
  (declare (dynamic-extent args))
  (let* ((display-args
	   (if (listp display-function) (rest display-function) nil))
	 (display-function
	   (if (listp display-function) (first display-function) display-function)))
    ;; Cons as little as possible...
    (cond ((and (null args) (null display-args))
	   (funcall display-function frame pane))
	  ((null display-args)
	   (apply display-function frame pane args))
	  ((null args)
	   (apply display-function frame pane display-args))
	  (t
	   (apply display-function frame pane (nconc (copy-list args) display-args))))))


;;; Enabling and disabling of commands

(defmethod command-enabled (command-name (frame standard-application-frame))
  (with-slots (disabled-commands) frame
    (or *assume-all-commands-enabled*
	(and (not (member command-name disabled-commands))
	     (command-accessible-in-command-table-p
	       command-name (frame-command-table frame))))))

(defmethod (setf command-enabled) (enabled command-name (frame standard-application-frame))
  (with-slots (disabled-commands) frame
    (if enabled
	(setf disabled-commands (delete command-name disabled-commands))
        (push command-name disabled-commands))))

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
