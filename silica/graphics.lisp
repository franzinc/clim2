;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: graphics.lisp,v 1.11 92/05/07 13:11:21 cer Exp $

(in-package :silica)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1991, 1992 Franz, Inc.  All rights reserved."

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

;; One would imagine that WITH-DRAWING-OPTIONS affects this rather
;; than the medium
;;--- I don't think so --SWM
(defclass sheet-with-graphics-state-mixin ()
    ((foreground :initform +black+ :accessor sheet-foreground)
     (background :initform +white+ :accessor sheet-background)
     (ink :initform +white+ :accessor sheet-ink)))

(defmethod (setf sheet-foreground) :after (ink (sheet sheet-with-graphics-state-mixin))
  (when (sheet-medium sheet)
    (setf (medium-foreground (sheet-medium sheet)) ink)))


(eval-when (compile load eval)

(defun write-graphics-function-transformer (name 
					    medium-graphics-function-name
					    unspread-argument-names
					    spread-arguments
					    spread-name
					    spread-argument-names
					    drawing-options
					    unspread-other-keyword-arguments
					    other-keyword-arguments
					    arguments
					    keyword-arguments-to-spread)
  (declare (ignore spread-arguments))
  (list
    `(define-compiler-macro ,spread-name 
			    (&whole form medium-or-stream ,@spread-argument-names
			     &rest drawing-options-and-keyword-arguments)
       (or (transform-graphics-function-call 
	     medium-or-stream
	     ',medium-graphics-function-name
	     ',drawing-options
	     ',other-keyword-arguments
	     (list ,@spread-argument-names)
	     drawing-options-and-keyword-arguments)
	   form))
    `(define-compiler-macro ,name 
			    (&whole form medium-or-stream ,@unspread-argument-names 
			     &rest drawing-options-and-keyword-arguments)
       (or (transform-graphics-function-call 
	     medium-or-stream
	     ',medium-graphics-function-name
	     ',drawing-options
	     ',unspread-other-keyword-arguments
	     (list ,@unspread-argument-names)
	     drawing-options-and-keyword-arguments
	     ',arguments
	     ',keyword-arguments-to-spread)
	   form))))

(defun generate-argument-spreading-code (x)
  (if (consp x)
      (destructuring-bind (argname type . names) x
	(ecase type
	  (point-sequence
	    (destructuring-bind (new-name) names
	      (values argname
		      (list `(unspread-point-sequence ,argname))
		      (list new-name))))
	  (point
	    (destructuring-bind (x y) names
	      (values argname
		      (list `(point-x ,argname)
			    `(point-y ,argname))
		      (list x y))))))
      (values x (list x) (list x))))

(defun decode-graphics-function-arguments (arguments keyword-arguments-to-spread)
  (let* ((keyn (position '&key arguments))
	 (no-keyword (subseq arguments 0 keyn))
	 (keyword (and keyn (subseq arguments (1+ keyn))))
	 unspread-argument-names
	 spread-arguments 
	 spread-argument-names)
    (dolist (x no-keyword)
      (multiple-value-bind (argname spread-args spread-values)
	  (generate-argument-spreading-code x)
	(push argname unspread-argument-names)
	  (dolist (x spread-args) (push x spread-arguments))
 	  (dolist (x spread-values) (push x spread-argument-names))))
    (let ((original-keywords keyword)
	  (new-keywords 
	    (mapcan #'(lambda (x)
			(let ((y (assoc (if (consp x) (car x) x)
					keyword-arguments-to-spread)))
			  (if y (copy-list (cddr y)) (list x))))
		    keyword)))
      (values (nreverse unspread-argument-names)
	      (nreverse spread-arguments)
	      (nreverse spread-argument-names)
	      (mapcar #'(lambda (x) (if (consp x) (car x) x)) new-keywords)
	      original-keywords
	      new-keywords
	      (mapcar #'(lambda (x)
			  (intern (symbol-name (if (consp x) (car x) x)) :keyword))
		      new-keywords)))))

(defun transform-graphics-function-call (medium-or-stream
					 medium-graphics-function-name
					 drawing-options
					 other-keyword-arguments
					 required-arguments
					 rest-argument
					 &optional arguments keyword-arguments-to-spread)
  (let ((drawing-options
	  (mapcar #'(lambda (x)
		      (intern (symbol-name x) :keyword))
		  drawing-options)))
    (flet ((kw-arg-keyword (x)
	     (intern (symbol-name (if (consp x) (car x) x)) :keyword))
	   (kw-arg-default-value (x)
	     (and (consp x) (second x))))
      (when (do ((args rest-argument (cddr args)))
		(nil)
	      (cond ((null args) (return t))
		    ((null (cdr args)) (return nil))
		    ((not (or (member (car args) drawing-options)
			      (dolist (arg other-keyword-arguments)
				(when (eq (kw-arg-keyword arg) (car args))
				  (return t)))))
		     (return nil))))
	(let ((bindings nil))
	  (when arguments
	    (setq required-arguments
		  (mapcan #'(lambda (arg req-arg)
			      (let ((g (gensym)))
				(push (list g req-arg) bindings)
				(if (consp arg)
				    (multiple-value-bind (name spread)
					(generate-argument-spreading-code
					  (cons g (cdr arg)))
				      (declare (ignore name))
				      spread)
				    (list g))))
			  arguments
			  required-arguments))
	    (setq bindings (nreverse bindings)))
	  (let* ((stuff
		   (do ((args rest-argument (cddr args))
			(result nil))
		       ((null args)
			(nreverse result))
		     (let ((kw (car args))
			   (value (cadr args)))
		       (push (list kw (gensymbol kw) value) result))))
		 (medium-or-stream-name (gensymbol 'medium))
		 (call
		   `(,medium-graphics-function-name
		     ,medium-or-stream-name
		     ,@required-arguments
		     ,@(mapcan #'(lambda (kw-arg)
				   (let ((v (or (second (assoc (kw-arg-keyword kw-arg) stuff))
						(kw-arg-default-value kw-arg)))
					 (ks (assoc kw-arg keyword-arguments-to-spread)))
				     (if ks
					 (ecase (second ks)
					   (point (list `(and ,v (point-x ,v))
							`(and ,v (point-y ,v)))))
					 (list v))))
			       other-keyword-arguments)))
		 (supplied-drawing-options
		   (mapcan #'(lambda (do)
			       (let ((x (assoc do stuff)))
				 (and x (list do (second x)))))
			   drawing-options)))
	    
	    `(let ((,medium-or-stream-name ,medium-or-stream))
	       (let ,bindings
		 (let ,(mapcar #'(lambda (x)
				   (list (second x) (third x)))
			       stuff)
		   ,(if supplied-drawing-options
			`(with-drawing-options 
			   (,medium-or-stream-name ,@supplied-drawing-options)
			   ,call)
		        call))))))))))

)	;eval-when


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


(defmacro define-graphics-function (name arguments 
				    &rest args
				    &key keywords-to-spread
					 drawing-options
					 optional-points-to-transform
					 points-to-transform
					 distances-to-transform
					 point-sequences-to-transform)
  (let* ((spread-name (intern (format nil "~A*" name)))
	 (drawing-options
	   (all-drawing-options-lambda-list drawing-options))
	 (medium-graphics-function-name
	   (intern (format nil "~A~A*" 'medium- name)))
	 (port-function-name
	   (intern (format nil "~A~A*" 'port- name))))
    (multiple-value-bind (unspread-argument-names spread-arguments
			  spread-argument-names keyword-argument-names
			  unspread-other-keyword-arguments			  
			  other-keyword-arguments keywords)
	(decode-graphics-function-arguments arguments keywords-to-spread)
      `(progn
	 (defun ,name (medium ,@unspread-argument-names &rest args
		       &key ,@drawing-options ,@unspread-other-keyword-arguments) 
	   (declare (ignore ,@drawing-options ,@keyword-argument-names)
		    (dynamic-extent args))
	   ,(if keywords-to-spread
		`(with-keywords-removed 
		     (args args ',(mapcar #'(lambda (x)
					      (intern (symbol-name (car x)) :keyword))
					  keywords-to-spread))
		   (apply #',spread-name 
			  medium
			  ,@spread-arguments
			  ,@(mapcan 
			      #'(lambda (x) 
				  (destructuring-bind (name type . rest) x
				    (ecase type
				      (point 
					(list (intern (symbol-name (first rest)) :keyword)
					      `(and ,name (point-x ,name))
					      (intern (symbol-name (second rest)) :keyword)
					      `(and ,name (point-y ,name)))))))
			      keywords-to-spread)
			  args))
 		`(apply #',spread-name 
			medium
			,@spread-arguments
			args)))
	 (defun ,spread-name (medium ,@spread-argument-names &rest args 
			      &key ,@drawing-options ,@other-keyword-arguments)
	   (declare (ignore ,@drawing-options)
		    (dynamic-extent args))
	   ,(if keywords
		`(with-keywords-removed (args args ',keywords)
		   (apply #'invoke-with-drawing-options
			  medium
			  #'(lambda ()
			      (,medium-graphics-function-name 
				 medium
				 ,@spread-argument-names
				 ,@keyword-argument-names))
			  args))
		`(apply #'invoke-with-drawing-options
			medium
			#'(lambda ()
			    (,medium-graphics-function-name 
			       medium
			       ,@spread-argument-names
			       ,@keyword-argument-names))
			args)))
	 (setf (get ',name 'args)
	       '((,@spread-argument-names ,@keyword-argument-names)
		 ,@args)) 
	 (defmethod ,medium-graphics-function-name ((sheet sheet) 
						    ,@spread-argument-names
						    ,@keyword-argument-names)
	   (with-sheet-medium (medium sheet)
	     (,medium-graphics-function-name medium 
					     ,@spread-argument-names
					     ,@keyword-argument-names)))
	 (defmethod ,medium-graphics-function-name ((medium medium) 
						    ,@spread-argument-names
						    ,@keyword-argument-names)
	   (let* ((sheet (medium-sheet medium)))
	     ;; want to tranform stuff, set up clipping region etc etc
	     ,(and points-to-transform
		   (do ((pts points-to-transform (cddr pts))
			(tf '#:transform)
			(r nil))
		       ((null pts) 
			`(let ((,tf (medium-transformation medium)))
			   ,@(nreverse r)))
		     (let ((b `(transform-positions (,tf)
				,(first pts) ,(second pts))))
		       (if (member (car pts) optional-points-to-transform)
			   (push `(when ,(car pts) ,b) r)
			   (push b r)))))
	     ,@(and distances-to-transform
		    `((transform-distances 
			((medium-transformation medium))
			,@distances-to-transform)))
	     ,@(mapcar #'(lambda (seq)
			   `(transform-position-sequence
			      ((medium-transformation medium)) 
			      ,seq))
		       point-sequences-to-transform)
	     (,port-function-name (port medium)
				  sheet medium
				  ,@spread-argument-names
				  ,@keyword-argument-names)))
	 ,@(write-graphics-function-transformer
	     name 
	     medium-graphics-function-name
	     unspread-argument-names
	     spread-arguments
	     spread-name
	     spread-argument-names
	     drawing-options
	     unspread-other-keyword-arguments
	     other-keyword-arguments
	     arguments
	     keywords-to-spread)))))

(defun get-drawing-function-description (name)
  (or (get name 'args)
      (error "Cannot find description for: ~S" name)))


(define-graphics-function draw-point ((point point x y))
  :drawing-options :point
  :points-to-transform (x y))

;;--- Not really satisfactory
(defun draw-points (medium point-seq &rest args)
  (declare (dynamic-extent args))
  (declare (arglist medium point-seq &key . #.(all-drawing-options-lambda-list :point)))
  (flet ((trampoline ()
	   (if (listp point-seq)
	       (loop
		 (when (null point-seq) (return))
		 (let ((point (pop point-seq)))
		   (draw-point-method medium (point-x point) (point-y point))))
	       (do ((i 0 (+ i 1)))
		   ((= i (length point-seq)))
		 (let ((point (aref point-seq i)))
		   (draw-point-method medium (point-x point) (point-y point)))))))
    (declare (dynamic-extent #'trampoline))
    (if args
	(apply #'invoke-with-drawing-options medium #'trampoline args)
	(trampoline))))

;;--- Not really satisfactory
(defun draw-points* (medium coord-seq &rest args)
  (declare (dynamic-extent args))
  (declare (arglist medium coord-seq &key . #.(all-drawing-options-lambda-list :point)))
  (flet ((trampoline ()
	   (if (listp coord-seq)
	       (loop
		 (when (null coord-seq) (return))
		 (draw-point-method medium (pop coord-seq) (pop coord-seq)))
	     (do ((i 0 (+ i 2)))
		 ((= i (length coord-seq)))
	       (draw-point-method medium (aref coord-seq i) (aref coord-seq (1+ i)))))))
    (declare (dynamic-extent #'trampoline))
    (if args
	(apply #'invoke-with-drawing-options medium #'trampoline args)
	(trampoline))))


(define-graphics-function draw-line ((point1 point x1 y1)
				     (point2 point x2 y2))
  :drawing-options :line-cap
  :points-to-transform (x1 y1 x2 y2))

;;--- Not really satisfactory
(defun draw-lines (medium point-seq &rest args)
  (declare (dynamic-extent args))
  (declare (arglist medium point-seq &key . #.(all-drawing-options-lambda-list :line-cap)))
  (flet ((trampoline ()
	   (if (listp point-seq)
	       (loop
		 (when (null point-seq) (return))
		 (let* ((point1 (pop point-seq))
			(point2 (pop point-seq)))
		   (draw-line-method medium
				     (point-x point1) (point-y point1)
				     (point-x point2) (point-y point2))))
	     (do ((i 0 (+ i 2)))
		 ((= i (length point-seq)))
	       (let* ((point1 (aref point-seq i))
		      (point2 (aref point-seq (1+ i))))
		 (draw-line-method medium
				   (point-x point1) (point-y point1)
				   (point-x point2) (point-y point2)))))))
    (declare (dynamic-extent #'trampoline))
    (if args
	(apply #'invoke-with-drawing-options medium #'trampoline args)
	(trampoline))))

;;--- Not really satisfactory
(defun draw-lines* (medium coord-seq &rest args)
  (declare (dynamic-extent args))
  (declare (arglist medium coord-seq &key . #.(all-drawing-options-lambda-list :line-cap)))
  (flet ((trampoline ()
	   (if (listp coord-seq)
	       (loop
		 (when (null coord-seq) (return))
		 (draw-line-method medium
				   (pop coord-seq) (pop coord-seq)
				   (pop coord-seq) (pop coord-seq)))
	     (do ((i 0 (+ i 4)))
		 ((= i (length coord-seq)))
	       (draw-line-method medium
				 (aref coord-seq i) (aref coord-seq (1+ i))
				 (aref coord-seq (+ i 2)) (aref coord-seq (+ i 3)))))))
    (declare (dynamic-extent #'trampoline))
    (if args
	(apply #'invoke-with-drawing-options medium #'trampoline args)
	(trampoline))))


;;--- What about DRAW-ARROW[*]?


(define-graphics-function draw-rectangle ((point1 point x1 y1)
					  (point2 point x2 y2)
					  &key (filled t))
  :drawing-options :line-joint
  :points-to-transform (x1 y1 x2 y2))

;;--- What about DRAW-RECTANGLES[*]?

;; DRAW-PATTERN* is a special case of DRAW-RECTANGLE*, believe it or not...
(defun draw-pattern* (medium pattern x y &key clipping-region transformation)
  (check-type pattern pattern)
  (let ((width (pattern-width pattern))
	(height (pattern-height pattern)))
    (if (or clipping-region transformation)
	(with-drawing-options (medium :clipping-region clipping-region
				      :transformation transformation
				      :ink pattern)
	  (draw-rectangle* medium x y (+ x width) (+ y height)
			   :filled t))
	(with-drawing-options (medium :ink pattern)
	  (draw-rectangle* medium x y (+ x width) (+ y height)
			   :filled t)))))


(define-graphics-function draw-polygon ((points point-sequence list-of-x-and-ys)
					&key (closed t) (filled t))
  :drawing-options :line-joint-cap
  :point-sequences-to-transform (list-of-x-and-ys))

(defun draw-regular-polygon* (medium x1 y1 x2 y2 nsides
			      &rest args &key (handedness :left) (closed t) &allow-other-keys)
  (declare (dynamic-extent args))
  (declare (arglist medium x1 y1 x2 y2 nsides
		    &key (filled t) (handedness :left) (closed t)
			 . #.(all-drawing-options-lambda-list :line-joint-cap)))
  (let* ((theta (* (float (* pi (/ 2.0 nsides)) 0.0)
		   (ecase handedness
		     (:left +1)
		     (:right -1))))
	 (transform (make-rotation-transformation theta))
	 (coordinates (list x1 y1 x2 y2))
	 (dx (- x2 x1))
	 (dy (- y2 y1))
	 (next-x x2)
	 (next-y y2))
    (repeat (- nsides 2)
      (multiple-value-setq (dx dy)
	(transform-distance transform dx dy))
      (incf next-x dx)
      (incf next-y dy)
      (setq coordinates (append coordinates (list next-x next-y))))
    (when closed
      (setq coordinates (append coordinates (list x1 y1))))
    (with-keywords-removed (args args '(:handedness))
      (apply #'draw-polygon* medium coordinates args))))

(defun draw-regular-polygon (medium point1 point2 nsides &rest args)
  (declare (dynamic-extent args))
  (declare (arglist medium point1 point2 nsides
		    &key (handedness :left) (closed t) (filled t)
			 . #.(all-drawing-options-lambda-list :line-joint-cap)))
  (apply #'draw-regular-polygon* medium
				 (point-x point1) (point-y point1)
				 (point-x point2) (point-y point2)
				 nsides args))

(defun draw-triangle (medium p1 p2 p3 &rest args)
  (declare (dynamic-extent args))
  (declare (arglist medium p1 p2 p3
		    &key (filled t) . #.(all-drawing-options-lambda-list :line-joint)))
  (with-stack-list (points p1 p2 p3)
    (apply #'draw-polygon medium points :closed t args)))

(defun draw-triangle* (medium x1 y1 x2 y2 x3 y3 &rest args)
  (declare (dynamic-extent args))
  (declare (arglist medium x1 y1 x2 y2 x3 y3
		    &key (filled t) . #.(all-drawing-options-lambda-list :line-joint)))
  (with-stack-list (points x1 y1 x2 y2 x3 y3)
    (apply #'draw-polygon* medium points :closed t args)))


(define-graphics-function draw-ellipse ((center point center-x center-y)
					radius-1-dx radius-1-dy
					radius-2-dx radius-2-dy
					&key (start-angle 0) (end-angle 2pi)
					     (filled t))
  :drawing-options :line-cap
  :points-to-transform (center-x center-y)
  :distances-to-transform (radius-1-dx radius-1-dy radius-2-dx radius-2-dy))

(defun draw-circle (medium center radius &rest args)
  (declare (dynamic-extent args))
  (declare (arglist medium center radius
		    &key start-angle end-angle (filled t)
			 . #.(all-drawing-options-lambda-list :line-cap)))
  (apply #'draw-ellipse medium center radius 0 0 radius args))

(define-compiler-macro draw-circle (medium center radius &rest args)
  (let ((gm (gensymbol 'medium))
	(gc (gensymbol 'center))
	(gr (gensymbol 'radius)))
    `(let ((,gm ,medium)
	   (,gc ,center)
	   (,gr ,radius))
       (draw-ellipse ,gm ,gc ,gr 0 0 ,gr ,@args))))

(defun draw-circle* (medium center-x center-y radius &rest args)
  (declare (dynamic-extent args))
  (declare (arglist medium center-x center-y radius
		    &key start-angle end-angle (filled t)
		         . #.(all-drawing-options-lambda-list :line-cap)))
  (apply #'draw-ellipse* medium center-x center-y radius 0 0 radius args))

(define-compiler-macro draw-circle* (medium center-x center-y radius &rest args)
  (let ((gm (gensymbol 'medium))
	(gx (gensymbol 'x))
	(gy (gensymbol 'y))
	(gr (gensymbol 'radius)))
    `(let ((,gm ,medium)
	   (,gx ,center-x)
	   (,gy ,center-y)
	   (,gr ,radius))
       (draw-ellipse* ,gm ,gx ,gy ,gr 0 0 ,gr ,@args))))


;;--- What about DRAW-OVAL[*]?


(define-graphics-function draw-text (string-or-char (point point x y)
						    &key (start 0) (end nil)
							 (align-x :left)
							 (align-y :baseline)
							 towards-point
							 transform-glyphs)
  :points-to-transform (x y towards-x towards-y)
  :optional-points-to-transform (towards-x towards-y)
  :keywords-to-spread ((towards-point point towards-x towards-y))
  :drawing-options :text)

(defmethod sheet-beep ((x t))
  x)

(defmethod sheet-beep ((x sheet))
  (port-beep (port x) x))


(defmethod copy-area (sheet 
		      from-left from-top from-right from-bottom
		      to-left to-top)
  (port-copy-area (port sheet) 
		  sheet sheet
		  from-left from-top from-right from-bottom to-left to-top))
