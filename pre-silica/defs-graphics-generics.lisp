;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-


(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 International Lisp Associates."

(eval-when (compile load eval)

;; NOTE: if you change this list of keywords, you also have to change the keyword arguments
;; accepted by (CLOS:METHOD INVOKE-WITH-DRAWING-OPTIONS (DRAWING-STATE-MIXIN T))
(defparameter *all-drawing-options*
	      '(:ink :clipping-region :transformation
		:line-style :line-unit :line-thickness :line-dashes
		:line-joint-shape :line-cap-shape
		:text-style :text-family :text-face :text-size))

(defparameter *always-meaningful-drawing-options* '(:ink :clipping-region :transformation))

(defparameter *drawing-option-subsets*
	      '((:point          :line-style :line-thickness :line-unit)
		(:line-cap       :line-style :line-thickness :line-unit
				 :line-dashes :line-cap-shape)
		(:line-joint     :line-style :line-thickness :line-unit
				 :line-dashes :line-joint-shape)
		(:line-joint-cap :line-style :line-thickness :line-unit
				 :line-dashes :line-joint-shape :line-cap-shape)
		(:text           :text-style :text-family :text-face :text-size)))

(defun non-drawing-option-keywords (arglist)
  (do ((l (cdr (member '&key arglist)) (cdr l))
       (non-drawing-option-keywords nil)
       k)
      ((null l) non-drawing-option-keywords)
    (setq k (cond ((atom (car l)) (intern (symbol-name (car l)) :keyword))
		  ((atom (caar l)) (intern (symbol-name (caar l)) :keyword))
		  (t (caaar l))))
    (unless (member k *all-drawing-options*)
      (push k non-drawing-option-keywords))))

;;; Caller must stick &key in front
;;; If drawing-options isn't nil, it's a list of the option keywords accepted.
(defun all-drawing-options-lambda-list (drawing-options)
  (mapcar #'(lambda (keyword) (intern (symbol-name keyword)))
	  (cond ((null drawing-options) *all-drawing-options*)
		((atom drawing-options)
		 (append (or (cdr (assoc drawing-options *drawing-option-subsets*))
			     (warn "~S was specified in :drawing-options but is not ~
				    a known drawing-option subset."
				   drawing-options))
			 *always-meaningful-drawing-options*))
		(t
		 (dolist (option drawing-options)
		   (unless (member option *all-drawing-options*)
		     (warn "~S was specified in :drawing-options but ~
			    is not a known drawing option."
			   option)))
		 (append drawing-options *always-meaningful-drawing-options*)))))

)	;eval-when

(defclass graphics-mixin (drawing-state-mixin) ())

;;; Register this class.
(define-stream-protocol graphics-mixin)

(define-protocol-p-method graphics-stream-p graphics-mixin)

(defmacro define-graphics-operation (name arglist &key arguments method-body drawing-options)
  (let ((method-name (fintern "~A-~A" name 'method)))
    (with-warnings-for-definition name define-graphics-operation
      `(define-group ,name define-graphics-operation
	 (defoperation ,method-name graphics-mixin 
	   ((graphics-mixin graphics-mixin)
	    ,@(remove '&key (flatten-arglist arglist))))
	 ,(define-graphics-method name method-name arglist arguments method-body)
	 ,(define-standard-graphics-operation-1 name method-name arglist
						arguments drawing-options)
	 ,(define-point-graphics-operation-1 name method-name arglist
					     arguments drawing-options)
	 ,(define-point-sequence-operation-1 name method-name arglist
					     arguments drawing-options)))))

(defmacro fix-points (&rest numbers)
  `(progn ,@(do* ((numbers numbers (cdr numbers))
		  (number (first numbers) (first numbers))
		  (forms nil))
		 ((null numbers) (nreverse forms))
	      (push `(unless (integerp ,number)
		       (setf ,number (floor ,number)))
		    forms))
	  (values)))

(defmacro transform-positions ((transform) &rest positions)
  (when positions
    (assert (evenp (length positions)) ()
	    "Positions must be x/y pairs.  There are an odd number of elements in ~S"
	    positions)
    (let ((xform '#:transform))
      `(let ((,xform ,transform))
	 ,@(do* ((positions positions (cddr positions))
		 (x (first positions) (first positions))
		 (y (second positions) (second positions))
		 (forms nil))
		((null positions) (nreverse forms))
	     (push `(multiple-value-setq (,x ,y)
		      (transform-position ,xform ,x ,y))
		   forms))
	 (values)))))

;;; Update the positions list in the reference.
(defmacro transform-position-sequence ((transform) positions-reference)
  (let ((pts-sequence '#:position-sequence)
	(pts-temp '#:positions)
	(original-pts-temp '#:original-positions)
	(xform '#:transform))
    ;;--- is a run-time error check worth it?
    `(let ((,pts-sequence ,positions-reference)
	   (,xform ,transform))
       (assert (evenp (length ,pts-sequence)) ()
	       "Positions must be x/y pairs.  There are an odd number of elements in ~S"
	       ,pts-sequence)
       (setf ,positions-reference
	     (let* ((,pts-temp (copy-list ,pts-sequence))
		    (,original-pts-temp ,pts-temp))
	       (dorest (pt ,pts-sequence cddr)
		 (let ((x (first pt))
		       (y (second pt)))
		   (multiple-value-bind (nx ny)
		       (transform-position ,xform x y)
		     (setf (car ,pts-temp) nx
			   ,pts-temp (cdr ,pts-temp)
			   (car ,pts-temp) ny
			   ,pts-temp (cdr ,pts-temp)))))
	       ,original-pts-temp))
       (values))))

(defmacro transform-distances ((transform) &rest distances)
  (when distances
    (assert (evenp (length distances)) ()
	    "Distances must be dx/dy pairs.  There are an odd number of elements in ~S"
	    distances)
    (let ((xform '#:transform))
      `(let ((,xform ,transform))
	 ,@(do* ((distances distances (cddr distances))
		 (dx (first distances) (first distances))
		 (dy (second distances) (second distances))
		 (forms nil))
		((null distances) (nreverse forms))
	     (push `(multiple-value-setq (,dx ,dy)
		      (transform-distance ,xform ,dx ,dy))
		   forms))
	 (values)))))


;;; Macro to write all the version of a drawing function
;;; It also will write the drawing method in the simplest case.

(defun define-graphics-method (drawing-function-name method-name arglist argument-specs method-body)
  (declare (ignore drawing-function-name))
  (let ((method-arglist `((stream graphics-mixin) ,@(remove '&key (flatten-arglist arglist)))))
    ;; This writes the draw-foo-method.  By default, it will write a
    ;; simple method body that transforms the args and calls the
    ;; DRAW-FOO-INTERNAL method.  The caller can supply a method body,
    ;; and that body had better wrap WITH-TRANSFORMED-ARGUMENTS around
    ;; its call to DRAW-FOO-INTERNAL methods, if any.
    `(defmethod ,method-name ,method-arglist
       ;; Write a "customized" macro that knows how to transform the
       ;; args for this drawing function.  The :method-body writer has
       ;; to wrap this around any call to a DRAW-FOO-INTERNAL method.
       (macrolet ((with-transformed-arguments (&body body)
		    ;; Postpone drawing-state update until last minute
		    `(progn
		       ;; Transform the positions, ltrbs, position
		       ;; sequences, and distances.
		       ,@(write-transform-clauses ',argument-specs
						  '(medium-transformation stream))
		       ,@body)))
	 (progn
	   ,(or method-body
		(error ":METHOD-BODY must be supplied.")))))))

;;; Interprets the argument specs provided via the :arguments keyword to
;;; define-graphics-operation and writes the appropriate coordinate
;;; transform form.
(defun write-transform-clauses (argument-specs transform)
  (let ((clauses nil))
    (dolist (spec argument-specs)
      (case (first spec)
	(point
	  ;; The rest of the spec is a list of x/y pairs
	  (assert (evenp (length (rest spec))) ()
		  "Points must be listed in x/y pairs. ~S has an odd number of elements."
		  (rest spec))
	  (push `(transform-positions (,transform) ,@(rest spec))
		clauses))
	(rectangle
	  ;; The rest of the spec if a list of left/top/right/bottom 4-tuples.
	  (assert (zerop (mod (length (rest spec)) 4)) ()
		  "Rectangles must have four values, left top right bottom.  ~S"
		  (rest spec))
	  (push `(transform-positions (,transform) ,@(rest spec))
		clauses))
	(point-sequence
	  ;; Elements of REST are references containing lists of points.
	  (dolist (ps (rest spec))
	    ;; No compile-time ASSERTion.
	    (push `(transform-position-sequence (,transform) ,ps)
		  clauses)))
	(distance
	  ;; The rest of the spec is a list of dx/dy pairs indicating a distance
	  (assert (evenp (length (rest spec))) ()
		  "Distances must be listed in dx/dy pairs. ~S has an odd number of elements."
		  (rest spec))
	  (push `(transform-distances (,transform) ,@(rest spec))
		clauses))))
    (nreverse clauses)))

;;; Macro to write the DRAW-FOO-INTERNAL methods

(defmacro adjust-for-viewport-and-margins (stream &rest points)
  (assert (evenp (length points)) ()
	  "Points must be x/y pairs.  There are an odd number of elements in ~S"
	  points)
  (let ((dx '#:dx)
	(dy '#:dy))
    `(multiple-value-bind (,dx ,dy)
	 (drawing-surface-to-viewport-coordinates ,stream 0 0)
       (translate-positions ,dx ,dy ,@points))))

(defmacro with-stipple-offsets ((sx sy array stream) &body body)
  (let ((vx '#:viewport-x)
	(vy '#:viewport-y)
	(sw '#:stipple-width)
	(sh '#:stipple-height))
    `(let ((,sx 0) 
	   (,sy 0))
       (when ,array
	 (multiple-value-bind (,vx ,vy)
	     (window-viewport-position ,stream)
	   (multiple-value-bind (,sw ,sh)
	       (values-list (array-dimensions ,array))
	     (translate-positions (mod ,vx ,sw) (mod ,vy ,sh) ,sx ,sy))))
       ,@body)))

(defmacro define-graphics-internal (name arglist
				    &key points-to-convert point-sequence-to-convert
					 bounding-rectangle
					 highlighting-test highlighting-function
					 output-recording-hook)
  (when (and point-sequence-to-convert
	     (null output-recording-hook))
    ;; Since we're using NTRANSLATE below, we must avoid bashing user lists
    (setq output-recording-hook
	  `(setq ,point-sequence-to-convert (copy-list ,point-sequence-to-convert))))
  (let* ((pass-on-arglist (make-pass-on-arglist arglist))
	 (flat-arglist (flatten-arglist arglist))
	 (class-name (fintern "~A-~A"
			      (remove-word-from-string
				"-internal" (remove-word-from-string "draw-" name))
			      'displayed-output-record))
	 (constructor-name (fintern "~A-~A" 'make class-name)))
    `(;; define-group ,name define-graphics-internal ;; POS Genera
      progn
      (defgeneric ,name (stream x-offset y-offset ,@flat-arglist))
      (defclass ,class-name
		(output-record-element-mixin graphics-displayed-output-record)
	  ,(mapcar #'(lambda (arg) `(,arg :initarg ,arg)) flat-arglist))       
      (define-constructor-using-prototype-instance
	,constructor-name ,class-name ,flat-arglist
	,@(mapcar #'(lambda (arg) `(,arg ',arg ,arg)) flat-arglist))
      ;; I'm too damned lazy to invent another protocol to hang this off of...
      (defmethod ,name ((stream standard-encapsulating-stream) xoff yoff ,@arglist)
	(,name (slot-value stream 'stream) xoff yoff ,@pass-on-arglist))
      (defmethod ,name :around ((stream output-and-window-protocol-intermediary) 
				x-offset y-offset ,@arglist)
	(declare (type coordinate x-offset y-offset))
	(adjust-for-viewport-and-margins stream x-offset y-offset)
	(call-next-method stream x-offset y-offset ,@pass-on-arglist))
      (defmethod replay-output-record ((record ,class-name) stream
				       &optional region 
						 (x-offset (coordinate 0))
						 (y-offset (coordinate 0)))
	;; Ignore the region and let MAP-OVER-OUTPUT-RECORDS-OVERLAPPING-REGION
	;; take care of not calling REPLAY-OUTPUT-RECORD unnecessarily.  Too expensive here.
	(declare (ignore region))
	(declare (type coordinate x-offset y-offset))
	(with-slots (,@flat-arglist) record
	  (,name stream x-offset y-offset ,@flat-arglist)))
      (defmethod ,name :around ((stream graphics-output-recording) x-offset y-offset
				,@arglist)
	(declare (ignore x-offset y-offset))
	(when (stream-recording-p stream)
	  ,output-recording-hook
	  (multiple-value-bind (abs-x abs-y)
	      (point-position
		(stream-output-history-position stream))
	    (declare (type coordinate abs-x abs-y))
	    ;; Adjust the coordinates by the position of the current record
	    (let ((rec (,constructor-name ,@flat-arglist)))
	      (multiple-value-bind (lf tp rt bt)
		  (progn ,bounding-rectangle)
		(declare (type coordinate lf tp rt bt))
		(bounding-rectangle-set-edges
		  rec
		  (- lf abs-x) (- tp abs-y)
		  (- rt abs-x) (- bt abs-y)))
	      (multiple-value-bind (cx cy) (stream-cursor-position stream)
		(declare (type coordinate cx cy))
		;; Doing this directly beats calling OUTPUT-RECORD-SET-START-CURSOR-POSITION
		(with-slots (start-x start-y) rec
		  (setq start-x (- cx abs-x)
			start-y (- cy abs-y)))
		;; Adjust the stored coordinates by the current cursor position
		,@(when points-to-convert
		    `((with-slots (,@points-to-convert) rec
			(translate-positions (- abs-x) (- abs-y) ,@points-to-convert))))
		,@(when point-sequence-to-convert
		    `((with-slots (,point-sequence-to-convert) rec
			(setq ,point-sequence-to-convert
			      (ntranslate-position-sequence (- abs-x) (- abs-y)
							    ,point-sequence-to-convert))))))
	      (stream-add-output-record stream rec))))
	(when (stream-drawing-p stream)
	  (call-next-method)))

      ,@(when highlighting-test
	  (let ((args (first highlighting-test))
		(body (rest highlighting-test)))
	    `((defmethod output-record-refined-sensitivity-test ((record ,class-name) ,@args)
		,@body))))
      ,@(when highlighting-function
	  (let ((args (first highlighting-function))
		(body (rest highlighting-function)))
	    `((defmethod highlight-output-record-1 ((record ,class-name) ,@args)
		,@body)))))))


;;; Macro helpers to write the user-called drawing functions.

;;; First some helper functions.

;;; Extract the positional and &rest arguments from an arglist.
(defun compute-pass-on (arglist)
  (let ((pass-on nil))
    (dolist (arg arglist)
      (cond ((eq arg '&key)
	     (return))
	    (t (push arg pass-on))))
    (nreverse pass-on)))

;;; Splice in an "&rest .args." before the &key in an arglist.
(defun add-&rest (arglist &optional (rest-arg-var '.args.))
  (let ((pos-&key (position '&key arglist)))
    (append (subseq arglist 0 pos-&key)
	    `(&rest ,rest-arg-var)
	    (when pos-&key
	      (subseq arglist pos-&key)))))

(defun apply-method-code (method-name stream pass-on args non-drawing-option-keywords)
  `(if ,args
       (flet ((trampoline ()
		(,method-name ,stream ,@pass-on)))
	 (declare (dynamic-extent #'trampoline))
	 ,(if non-drawing-option-keywords
	      `(with-keywords-removed (drawing-options ,args ',non-drawing-option-keywords)
		 (apply #'invoke-with-drawing-options ,stream #'trampoline drawing-options))
	      `(apply #'invoke-with-drawing-options ,stream #'trampoline ,args)))
       (,method-name ,stream ,@pass-on)))

;;; Writes the "standard" draw-foo* function, which takes separate X and Y arguments.
(defun define-standard-graphics-operation-1 (name method-name arglist
					     arguments drawing-options)
  (declare (ignore arguments))
  (let ((pass-on (remove '&key (flatten-arglist arglist)))
	(fcn-args (add-&rest (append arglist
				     (if (member '&key arglist)
					 `(&allow-other-keys)
					 `(&key &allow-other-keys)))
			     '.args.))
	;; Spread form is called DRAW-FOO*
	(function-name (fintern "~A*" name)))
    `(progn
       (defun ,function-name (stream ,@fcn-args)
	 (declare (dynamic-extent .args.))
	 (declare (arglist stream
			   ,@arglist
			   ,@(unless (member '&key arglist) `(&key))
			   ,@(all-drawing-options-lambda-list drawing-options)))
	 ,(apply-method-code method-name 'stream pass-on '.args.
			     (non-drawing-option-keywords arglist)))
       ,(when (null (intersection arglist lambda-list-keywords))
	  `(define-compiler-macro ,function-name (&whole form &rest args)
	     (when (= (length args) (1+ ,(length arglist)))
	       (return-from ,function-name
		 `(,',method-name ,@args)))
	     form)))))

;;; Writes the draw-foo version, which spreads out the points into positions.
(defun define-point-graphics-operation-1 (name method-name arglist arguments drawing-options)
  ;; Turn adjacent args of type point into "point" arguments
  (let ((point-arglist nil)
	(point-associations nil)
	(count 1)
	(point-args (cdr (assoc 'point arguments))))
    (when point-args
      (do* ((args arglist (cdr args))
	    (arg (first args) (first args)))
	   ((null args) nil)
	(let ((stuff (member arg point-args)))
	  (cond ((and stuff
		      (eq (second args) (second stuff)))
		 (let ((point-varname
			 (fintern "~A-~D" 'point count)))
		   (incf count)
		   (push `(,(first args) ,(second args) ,point-varname) point-associations)
		   (push point-varname point-arglist))
		 (setq args (cdr args)))
		(t (push arg point-arglist)))))
      (setq point-arglist
	    (append (reverse point-arglist)
		    (if (member '&key point-arglist)
			`(&allow-other-keys)
			`(&key &allow-other-keys))))
      (setq point-arglist (add-&rest point-arglist '.args.))
      ;; Structure-oriented version is called DRAW-FOO
      (let ((function-name name))
	(flet ((let-args ()
		 (let ((args nil))
		   (dolist (spec point-associations)
		     (push (first spec) args)
		     (push (second spec) args))
		   (nreverse args)))
	       (set-clauses ()
		 (let ((clauses nil))
		   (dolist (spec point-associations)
		     (push `(setf ,(first spec) (point-x ,(third spec))) clauses)
		     (push `(setf ,(second spec) (point-y ,(third spec))) clauses))
		   (nreverse clauses))))
	  `(defun ,function-name (stream ,@point-arglist)
	     (declare (dynamic-extent .args.))
	     (declare (arglist stream
			       ,@(ldiff point-arglist (member '&rest point-arglist))
			       &key ,@(cdr (member '&key arglist))
				    ,@(all-drawing-options-lambda-list drawing-options)))
	     (let (,@(let-args))
	       ,@(set-clauses)
	       ,(apply-method-code method-name 'stream (remove '&key (flatten-arglist arglist))
				   '.args. (non-drawing-option-keywords arglist)))))))))

;;; Writes the DRAW-FOO version for things that take an arg that is a
;;; sequence of points/positions.
(defun define-point-sequence-operation-1 (name method-name arglist arguments drawing-options)
  ;; turn point-sequence (list of x&ys) args into sequences of points
  ;; (inefficiently).
  (let ((point-sequence-arglist nil)
	(point-sequence-associations nil)
	(count 1)
	(point-sequence-args (cdr (assoc 'point-sequence arguments))))
    (when point-sequence-args
      (do* ((args arglist (cdr args))
	    (arg (first args) (first args)))
	   ((null args) nil)
	(let ((stuff (member arg point-sequence-args)))
	  (cond (stuff
		 (let ((point-sequence-varname
			 (fintern "~A-~D" 'point-sequence count)))
		   (incf count)
		   (push `(,(first args) ,point-sequence-varname) point-sequence-associations)
		   (push point-sequence-varname point-sequence-arglist)))
		(t (push arg point-sequence-arglist)))))
      (setq point-sequence-arglist
	    (append (reverse point-sequence-arglist)
		    (if (member '&key point-sequence-arglist)
			`(&allow-other-keys)
			`(&key &allow-other-keys))))
      (setq point-sequence-arglist (add-&rest point-sequence-arglist '.args.))
      ;; Structure-oriented version is called DRAW-FOO
      (let ((function-name name))
	(flet ((let-args ()
		 (let ((args nil))
		   (dolist (spec point-sequence-associations)
		     (push (first spec) args))
		   (nreverse args)))
	       (set-clauses ()
		 (let ((clauses nil))
		   (dolist (spec point-sequence-associations)
		     (push `(do* ((points ,(second spec) (cdr points))
				  (point (first points) (first points)))
				 ((null points) (setq ,(first spec) (nreverse ,(first spec))))
			      (push (point-x point) ,(first spec))
			      (push (point-y point) ,(first spec)))
			   clauses))
		   (nreverse clauses))))
	  `(defun ,function-name (stream ,@point-sequence-arglist)
	     (declare (dynamic-extent .args.))
	     (declare (arglist stream
			       ,@(ldiff point-sequence-arglist
					(member '&rest point-sequence-arglist))
			       &key ,@(cdr (member '&key arglist))
				    ,@(all-drawing-options-lambda-list drawing-options)))
	     ,(ignore-arglist point-sequence-arglist)
	     (let (,@(let-args))
	       ,@(set-clauses)
	       ,(apply-method-code method-name 'stream (remove '&key (flatten-arglist arglist))
				   '.args. (non-drawing-option-keywords arglist)))))))))
