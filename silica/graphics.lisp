;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $fiHeader: graphics.lisp,v 1.14 92/07/01 15:45:04 cer Exp $

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
		      (list `(spread-point-sequence ,argname))
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


;; Modifies the positions
(defmacro transform-positions (transform &body positions)
  (when positions
    (assert (evenp (length positions)) ()
	    "Positions must be x/y pairs, but there are an odd number of elements in ~S"
	    positions)
    (let ((xform '#:transform))
      `(let ((,xform ,transform))
	 (unless (eq ,xform +identity-transformation+)
	   ,@(do* ((positions positions (cddr positions))
		   (x (first positions) (first positions))
		   (y (second positions) (second positions))
		   (forms nil))
		  ((null positions) (nreverse forms))
	       (push `(multiple-value-setq (,x ,y)
			(transform-position ,xform ,x ,y))
		     forms)))))))

;; Modifies the distances
(defmacro transform-distances (transform &body distances)
  (when distances
    (assert (evenp (length distances)) ()
	    "Distances must be dx/dy pairs, but there are an odd number of elements in ~S"
	    distances)
    (let ((xform '#:transform))
      `(let ((,xform ,transform))
	 (unless (eq ,xform +identity-transformation+)
	   ,@(do* ((distances distances (cddr distances))
		   (dx (first distances) (first distances))
		   (dy (second distances) (second distances))
		   (forms nil))
		  ((null distances) (nreverse forms))
	       (push `(multiple-value-setq (,dx ,dy)
			(transform-distance ,xform ,dx ,dy))
		     forms)))))))

(defun map-position-sequence (function positions)
  (declare (dynamic-extent function))
  (if (listp positions)
      (loop
	(when (null positions) (return))
	(let* ((x (pop positions))
	       (y (pop positions)))
	  (funcall function x y)))
      (let ((length (length positions))
	    #+Genera (positions positions))
	(declare (type vector positions))
	(do ((i 0 (+ i 2)))
	    ((>= i length))
	  (funcall function (aref positions i) (aref positions (1+ i)))))))

(defun map-endpoint-sequence (function positions)
  (declare (dynamic-extent function))
  (if (listp positions)
      (loop
	(when (null positions) (return))
	(let* ((x1 (pop positions))
	       (y1 (pop positions))
	       (x2 (pop positions))
	       (y2 (pop positions)))
	  (funcall function x1 y1 x2 y2)))
      (let ((length (length positions))
	    #+Genera (positions positions))
	(declare (type vector positions))
	(do ((i 0 (+ i 4)))
	    ((>= i length))
	  (funcall function (aref positions i) (aref positions (1+ i))
			    (aref positions (+ i 2)) (aref positions (+ i 3)))))))

;; Transforms all of the positions in the sequence.  This returns the
;; original sequence if the transformation is the identity and COPY-P
;; is false, otherwise it returns a new vector containing the result.
(defun transform-position-sequence (transform positions &optional copy-p)
  (if (eq transform +identity-transformation+)
      (if copy-p 
	  (make-array (length positions) :initial-contents positions)
	  positions)
      (let* ((length (length positions))
	     (result (make-array length)))
	(declare (simple-vector result)
		 (optimize (speed 3) (safety 0)))
	(assert (evenp length) ()
		"Positions sequences must be x/y pairs, but there are an odd number of elements in ~S"
		positions)
	;; Inline MAP-POSITION-SEQUENCE for speed...
	(if (listp positions)
	    (let ((i -1))
	      (loop
		(when (null positions) (return))
		(let* ((x (pop positions))
		       (y (pop positions)))
		  (multiple-value-setq (x y)
		    (transform-position transform x y))
		  (setf (svref result (incf i)) x
			(svref result (incf i)) y))))
	    (let (#+Genera (positions positions))
	      (declare (type vector positions))
	      (do ((i 0 (+ 2 i)))
		  ((= i length))
		(multiple-value-bind (x y)
		    (transform-position transform 
					(aref positions i) (aref positions (1+ i)))
		  (setf (svref result i) x
			(svref result (1+ i)) y)))))
	result)))
    
(defun spread-point-sequence (sequence)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((length (length sequence))
	 (result (make-array (* 2 length)))
	 (i -1))
    (declare (type simple-vector new))
    (doseq (point sequence)
      (setf (svref result (incf i)) (point-x point)) 
      (setf (svref result (incf i)) (point-y point)))
    result))


(defmacro define-graphics-generic (name arguments 
				   &rest args
				   &key keywords-to-spread
					drawing-options
					optional-positions-to-transform
					positions-to-transform
					distances-to-transform
					position-sequences-to-transform)
  (let* ((spread-name (intern (format nil "~A*" name)))
	 (drawing-options
	   (all-drawing-options-lambda-list drawing-options))
	 (medium-graphics-function-name
	   (intern (format nil "~A~A*" 'medium- name))))
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
	 (defmethod ,medium-graphics-function-name
		    ((sheet sheet) ,@spread-argument-names ,@keyword-argument-names)
	   (with-sheet-medium (medium sheet)
	     (,medium-graphics-function-name medium 
					     ,@spread-argument-names
					     ,@keyword-argument-names)))
	 (defmethod ,medium-graphics-function-name :around
		    ((medium basic-medium) ,@spread-argument-names ,@keyword-argument-names)
	   ;; Want to tranform stuff, set up clipping region etc etc
	   ,(and positions-to-transform
		 (do ((pts positions-to-transform (cddr pts))
		      (tf '#:transform)
		      (r nil))
		     ((null pts) 
		      `(let ((,tf (medium-transformation medium)))
			 ,@(nreverse r)))
		   (let ((b `(transform-positions ,tf
			       ,(first pts) ,(second pts))))
		     (if (member (car pts) optional-positions-to-transform)
			 (push `(when ,(car pts) ,b) r)
			 (push b r)))))
	   ,@(and distances-to-transform
		  `((transform-distances 
		      (medium-transformation medium)
		      ,@distances-to-transform)))
	   ,@(mapcar #'(lambda (seq)
			 `(setq ,seq (transform-position-sequence
				       (medium-transformation medium) ,seq)))
		     position-sequences-to-transform)
	   (call-next-method medium ,@spread-argument-names ,@keyword-argument-names))
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


(define-graphics-generic draw-point ((point point x y))
  :drawing-options :point
  :positions-to-transform (x y))

(define-graphics-generic draw-points ((points point-sequence position-seq))
  :drawing-options :point
  :position-sequences-to-transform (position-seq))

(define-graphics-generic draw-line ((point1 point x1 y1)
				    (point2 point x2 y2))
  :drawing-options :line-cap
  :positions-to-transform (x1 y1 x2 y2))

(define-graphics-generic draw-lines ((points point-sequence position-seq))
  :drawing-options :line-cap
  :position-sequences-to-transform (position-seq))

(defun draw-arrow* (medium x1 y1 x2 y2 
		    &rest args
		    &key (from-head nil) (to-head t) (head-length 10) (head-width 5)
		    &allow-other-keys)
  (declare (dynamic-extent args))
  (declare (arglist medium x1 y1 x2 y2
		    (from-head nil) (to-head t) (head-length 10) (head-width 5)
		    . #.(all-drawing-options-lambda-list :line-cap)))
  (flet ((draw-arrow ()
	   (let* ((dx (- x2 x1))
		  (dy (- y2 y1))
		  (norm (if (zerop dx)
			    (if (zerop dy)
				nil
				(/ 1.0 (abs dy)))
			    (if (zerop dy)
				(/ 1.0 (abs dx))
				(/ (sqrt (+ (* dx dx) (* dy dy))))))))
	     (when norm
	       (let* ((length-norm (* head-length norm))
		      (ldx (* dx length-norm))
		      (ldy (* dy length-norm))
		      (base-norm (* head-width norm 0.5))
		      (bdx (* dy base-norm))
		      (bdy (* dx base-norm)))
		 (draw-line* medium x1 y1 x2 y2)
		 (when from-head
		   (let ((xa (+ x1 ldx)) (ya (+ y1 ldy)))
		     (with-stack-list (points x1 y1
					      (+ xa bdx) (- ya bdy)
					      (- xa bdx) (+ ya bdy))
		       (draw-polygon* medium points :filled t))
		     (setq x1 xa y1 ya)))
		 (when to-head
		   (let ((xa (- x2 ldx)) (ya (- y2 ldy)))
		     (with-stack-list (points x2 y2
					      (+ xa bdx) (- ya bdy)
					      (- xa bdx) (+ ya bdy))
		       (draw-polygon* medium points :filled t)
		       (setq x2 xa y2 ya)))))))))
    (declare (dynamic-extent #'draw-arrow))
    (with-keywords-removed (options args 
			    '(:from-head :to-head :head-length :head-width))
      (apply #'invoke-with-drawing-options medium #'draw-arrow options))))

(defun draw-arrow (medium point1 point2 &rest args)
  (declare (dynamic-extent args))
  (declare (arglist medium x1 y1 x2 y2
		    (from-head nil) (to-head t) (head-length 10) (head-width 5)
		    . #.(all-drawing-options-lambda-list :line-cap)))
  (apply #'draw-arrow* 
	 medium (point-x point1) (point-y point1) (point-x point2) (point-y point2) args))

(define-graphics-generic draw-rectangle ((point1 point x1 y1)
					 (point2 point x2 y2)
					 &key (filled t))
  :drawing-options :line-joint
  :positions-to-transform (x1 y1 x2 y2))

(define-graphics-generic draw-rectangles ((points point-sequence position-seq)
					  &key (filled t))
  :drawing-options :line-joint
  :position-sequences-to-transform (position-seq))

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

(define-graphics-generic draw-polygon ((points point-sequence position-seq)
				       &key (closed t) (filled t))
  :drawing-options :line-joint-cap
  :position-sequences-to-transform (position-seq))

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

(define-graphics-generic draw-ellipse ((center point center-x center-y)
				       radius-1-dx radius-1-dy
				       radius-2-dx radius-2-dy
				       &key (start-angle 0) (end-angle 2pi)
					    (filled t))
  :drawing-options :line-cap
  :positions-to-transform (center-x center-y)
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

(defun draw-oval* (medium center-x center-y x-radius y-radius 
		   &rest args &key (filled t) &allow-other-keys)
  (declare (dynamic-extent args))
  (declare (arglist medium center-x center-y x-radius y-radius
		    . #.(all-drawing-options-lambda-list :line-cap)))
  (flet ((draw-oval ()
	   (multiple-value-bind (x-radius y-radius)
	       (transform-distance (medium-transformation medium) x-radius y-radius)
	     (let ((left (- center-x x-radius))
		   (right (+ center-x x-radius))
		   (top (- center-y y-radius))
		   (bottom (+ center-y y-radius)))
	       (cond ((or (= x-radius y-radius) 
			  (zerop x-radius))
		      (draw-ellipse* medium center-x center-y y-radius 0 0 y-radius
				     :filled filled))
		     ((zerop y-radius)
		      (draw-ellipse* medium center-x center-y x-radius 0 0 x-radius
				     :filled filled))
		     ((> x-radius y-radius)
		      (let ((rect-left (+ left y-radius))
			    (rect-right (- right y-radius)))
			(cond (filled
			       (draw-rectangle* medium rect-left top rect-right bottom))
			      (t
			       (draw-line* medium rect-left top rect-right top)
			       (draw-line* medium rect-left bottom rect-right bottom)))
			(let ((north (float (* pi 1/2) 0.0))
			      (south (float (* pi 3/2) 0.0)))
			  (draw-ellipse* medium rect-left center-y y-radius 0 0 y-radius
					 :start-angle north :end-angle south
					 :filled filled)
			  (draw-ellipse* medium rect-right center-y y-radius 0 0 y-radius
					 :start-angle south :end-angle north
					 :filled filled))))
		     (t
		      (let ((rect-top (+ top x-radius))
			    (rect-bottom (- bottom x-radius)))
			(cond (filled
			       (draw-rectangle* medium left rect-top right rect-bottom))
			      (t
			       (draw-line* medium left rect-top left rect-bottom)
			       (draw-line* medium right rect-top right rect-bottom)))
			(let ((east 0.0)
			      (west (float pi 0.0)))
			  (draw-ellipse* medium center-x rect-top x-radius 0 0 x-radius
					 :start-angle west :end-angle east
					 :filled filled)
			  (draw-ellipse* medium center-x rect-bottom x-radius 0 0 x-radius
					 :start-angle east :end-angle west
					 :filled filled)))))))))
    (declare (dynamic-extent #'draw-oval))
    (apply #'invoke-with-drawing-options medium #'draw-oval args)))

(defun draw-oval (medium center x-radius y-radius &rest args)
  (declare (dynamic-extent args))
  (declare (arglist medium point x-radius y-radius
		    . #.(all-drawing-options-lambda-list :line-cap)))
  (apply #'draw-oval* 
	 medium (point-x center) (point-y center) x-radius y-radius args))

(define-graphics-generic draw-text (string-or-char (point point x y)
				    &key (start 0) (end nil)
					 (align-x :left) (align-y :baseline)
					 towards-point transform-glyphs)
  :positions-to-transform (x y towards-x towards-y)
  :optional-positions-to-transform (towards-x towards-y)
  :keywords-to-spread ((towards-point point towards-x towards-y))
  :drawing-options :text)
