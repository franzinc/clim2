;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: graph-formatting.lisp,v 1.4 92/01/31 15:07:38 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics Inc.  All rights reserved."

(defvar *graph-type-record-type-alist* nil)

(defmacro define-graph-type (graph-type class)
  `(let ((old (assoc ',graph-type *graph-type-record-type-alist*)))
     (if old
	 (setf (second old) ',class)
	 (setq *graph-type-record-type-alist*
	       (nconc *graph-type-record-type-alist* (list (list ',graph-type ',class)))))))


(defparameter *default-generation-separation* 20)
(defparameter *default-within-generation-separation* 10)


;; Some graph layout algorithms will want this...
(defresource graph-node-hash-table (&key test)
  :constructor (make-hash-table :test test)
  :deinitializer (clrhash graph-node-hash-table))


(define-protocol-class graph-output-record (output-record))

;; The basic graph output record class
(defclass basic-graph-output-record 
	  (standard-sequence-output-record graph-output-record)
    ((orientation :initarg :orientation)
     (center-nodes :initarg :center-nodes)
     (cutoff-depth :initarg :cutoff-depth)
     (merge-duplicates :initarg :merge-duplicates)
     (generation-separation :initarg :generation-separation)
     (within-generation-separation :initarg :within-generation-separation)
     ;; The output records corresponding to the root nodes
     (root-nodes :initform nil :accessor graph-root-nodes)
     (hash-table :initform nil :initarg :hash-table))
  (:default-initargs :size 30))

;;--- Not correct, since edges can overlap nodes. 
;;--- This really needs its own AUGMENT-DRAW-SET method.
(defmethod inferiors-never-overlap-p ((record basic-graph-output-record)) t)

;; Recover the hash table during incremental redisplay
(defmethod match-output-records :after
	   ((record basic-graph-output-record) &key hash-table &allow-other-keys)
  (when hash-table
    (setf (slot-value record 'hash-table) hash-table)))


(define-protocol-class graph-node-output-record (output-record))

;; The basic graph node output record class
(defclass standard-graph-node-output-record 
	  (standard-sequence-output-record graph-node-output-record)
    ((generation :accessor node-generation :initform 0)
     ;; The output records corresponding to this node's parents and children
     (node-children :accessor graph-node-children :initform nil)
     (node-parents :accessor graph-node-parents :initform nil))
  (:default-initargs :size 5))

(define-output-record-constructor graph-node-output-record
				  (&key x-position y-position (size 25))
  :x-position x-position :y-position y-position :size size)

(defmethod tree-recompute-extent-1 ((record standard-graph-node-output-record))
  (bounding-rectangle* record))

(defmethod graph-node-x ((node standard-graph-node-output-record)) 
  (bounding-rectangle-left node))

(defmethod (setf graph-node-x) (new-value (node standard-graph-node-output-record))
  (output-record-set-position* node new-value (bounding-rectangle-top node)))

(defmethod graph-node-y ((node standard-graph-node-output-record))
  (bounding-rectangle-top node))

(defmethod (setf graph-node-y) (new-value (node standard-graph-node-output-record))
  (output-record-set-position* node (bounding-rectangle-left node) new-value))


;; For compatibility...
(defun format-graph-from-root (root-object object-printer inferior-producer
			       &rest keys
			       &key (stream *standard-output*)
				    (key #'identity)
				    (orientation ':horizontal)
				    (cutoff-depth nil)
				    (generation-separation
				      *default-generation-separation*)
				    (within-generation-separation
				      *default-within-generation-separation*)
				    (move-cursor t))
  (declare (dynamic-extent keys object-printer inferior-producer)
	   (ignore stream key orientation cutoff-depth 
		   generation-separation within-generation-separation move-cursor))
  (with-keywords-removed (keys keys '(:key))
    (apply #'format-graph-from-roots
	   (list root-object) object-printer inferior-producer keys)))

(defun format-graph-from-roots (root-objects object-printer inferior-producer
				&key (stream *standard-output*)
				     (orientation ':horizontal) (center-nodes nil)
				     (cutoff-depth nil)
				     (merge-duplicates nil)
				     (duplicate-key #'identity key-supplied-p)
				     (duplicate-test #'eql)
				     (arc-drawer #'draw-line*)
				     (arc-drawing-options nil)
				     (graph-type (if merge-duplicates :digraph :tree))
				     (generation-separation
				       *default-generation-separation*)
				     (within-generation-separation
				       *default-within-generation-separation*)
				     (move-cursor t))
  (declare (dynamic-extent object-printer inferior-producer))
  (check-type cutoff-depth (or null integer))
  (check-type generation-separation real)
  (check-type within-generation-separation real)
  (unless merge-duplicates
    (assert (null key-supplied-p) ()
	    "You must not supply :DUPLICATE-KEY when you use :MERGE-DUPLICATES NIL")
    ;; Guarantees that each object in the graph is unique...
    (setq duplicate-key #'list
	  duplicate-test #'eql))
  (assert (not (null (assoc graph-type *graph-type-record-type-alist*))) ()
	  "The graph type ~S is not a known graph type" graph-type)
  (using-resource (hash-table graph-node-hash-table :test duplicate-test)
    (let* ((record-type (second (assoc graph-type *graph-type-record-type-alist*)))
	   (graph-record 
	     (with-output-recording-options (stream :draw nil :record t)
	       (with-end-of-page-action (stream :allow)
		 (with-end-of-line-action (stream :allow)
		   (with-new-output-record 
		       (stream record-type graph-record
			:orientation orientation
			:center-nodes center-nodes
			:cutoff-depth cutoff-depth
			:merge-duplicates merge-duplicates
			:generation-separation generation-separation
			:within-generation-separation within-generation-separation
			:hash-table hash-table)
		     (generate-graph-nodes graph-record stream
					   root-objects object-printer inferior-producer
					   :duplicate-key duplicate-key 
					   :duplicate-test duplicate-test)
		     graph-record))))))
      (unwind-protect
	  (progn
	    (layout-graph-nodes graph-record stream)
	    (layout-graph-edges graph-record stream arc-drawer arc-drawing-options))
	;; We're going to free the hash table as we exit, so make sure
	;; there are no pointers to it
	(setf (slot-value graph-record 'hash-table) nil))
      (tree-recompute-extent graph-record)
      (replay graph-record stream)
      (when move-cursor
	(move-cursor-beyond-output-record stream graph-record))
      graph-record)))


;;; Tree graphs

(defclass tree-graph-output-record (basic-graph-output-record) ())

(define-graph-type :tree tree-graph-output-record)

(defmethod generate-graph-nodes ((graph tree-graph-output-record) stream
				 root-objects object-printer inferior-producer
				 &key duplicate-key duplicate-test)
  (declare (ignore duplicate-key duplicate-test))
  (let ((cutoff-depth (slot-value graph 'cutoff-depth))
	(depth 0))
    (labels ((format-node (object)
	       (when cutoff-depth
		 (incf depth)
		 (when (> depth cutoff-depth)
		   (return-from format-node nil)))
	       (let ((children nil))
		 (dolist (inferior (funcall inferior-producer object))
		   (let ((node (format-node inferior)))
		     (when (and node (not (member node children)))
		       ;; Don't add the same node to the children more than
		       ;; once, which can come up during redisplay
		       (setq children (nconc children (list node))))))
		 (let ((this-node (with-new-output-record 
				      (stream 'standard-graph-node-output-record)
				    (funcall object-printer object stream))))
		   (setf (graph-node-children this-node) children)
		   (dolist (child (graph-node-children this-node))
		     (setf (graph-node-parents child) (list this-node)))
		   this-node))))
      (setf (graph-root-nodes graph) 
	    (map 'list #'format-node root-objects))))
  graph)

(defmethod layout-graph-nodes ((graph tree-graph-output-record) stream)
  (declare (ignore stream))
  (with-slots (root-nodes orientation center-nodes
	       generation-separation within-generation-separation) graph
    (let ((start-x 0)
	  (start-y 0)
	  ;;--- This needs to handle multiple root nodes
	  (root-node (first root-nodes)))
      (macrolet
	((layout-body (driver-function-name 
		       node-var depth-var breadth-var tallest-sibling-var
		       generation-separation-var within-generation-separation-var)
	   `(let ((children (graph-node-children ,node-var))
		  (start-breadth ,breadth-var)
		  (node-breadth (node-breadth ,node-var))
		  (breadth-margin ,within-generation-separation-var)
		  (depth-margin ,generation-separation-var)
		  (tallest-child 0))
	      (declare (type coordinate start-breadth node-breadth
			     		depth-margin breadth-margin tallest-child))
	      (dolist (child children)
		(maxf tallest-child (node-depth child)))
	      (dolist (child children)
		(incf start-breadth (round breadth-margin 2))
		(let ((child-breadth 
			(,driver-function-name
			 child
			 (+ ,depth-var ,tallest-sibling-var depth-margin)
			 start-breadth
			 tallest-child)))
		  (incf start-breadth child-breadth))
		(incf start-breadth (round breadth-margin 2)))
	      (let* ((total-child-breadth (- start-breadth ,breadth-var))
		     (my-breadth
		       (+ ,breadth-var
			  (round (max 0 (- total-child-breadth node-breadth)) 2))))
		(setf (node-depth-start ,node-var) ,depth-var
		      (node-breadth-start ,node-var) my-breadth)
		(max total-child-breadth (node-breadth ,node-var))))))
	(ecase orientation
	  ((:vertical :down)
	   (macrolet ((node-breadth (node) `(bounding-rectangle-width ,node))
		      (node-depth (node) `(bounding-rectangle-height ,node))
		      (node-breadth-start (node) `(graph-node-x ,node))
		      (node-depth-start (node) `(graph-node-y ,node)))
	     (labels ((layout-graph (root start-x start-y tallest-sibling)
			(layout-body layout-graph root
				     start-x start-y tallest-sibling
				     generation-separation
				     within-generation-separation)))
	       ;; Yes, this really is "start-y" followed by "start-x"
	       (layout-graph root-node start-y start-x (node-depth root-node)))))
	  ((:horizontal :right)
	   (macrolet ((node-breadth (node) `(bounding-rectangle-height ,node))
		      (node-depth (node) `(bounding-rectangle-width ,node))
		      (node-breadth-start (node) `(graph-node-y ,node))
		      (node-depth-start (node) `(graph-node-x ,node)))
	     (labels ((layout-graph (root start-x start-y tallest-sibling)
			(layout-body layout-graph root
				     start-x start-y tallest-sibling
				     generation-separation
				     within-generation-separation)))
	       (layout-graph root-node start-x start-y (node-depth root-node))))))))))

(defmethod layout-graph-edges ((graph tree-graph-output-record) 
			       stream arc-drawer arc-drawing-options)
  (with-slots (orientation root-nodes) graph
    ;;--- This needs to handle multiple root nodes 
    (let ((root-node (first root-nodes)))
      (flet ((parent-attachment-position (node)
	       (with-bounding-rectangle* (left top right bottom) node
		 (case orientation
		   ((:horizontal :right)
		    (values (1+ right) (+ top (floor (- bottom top) 2))))
		   ((:vertical :down)
		    (values (+ left (floor (- right left) 2)) (1+ bottom))))))
	     (child-attachment-position (node)
	       (with-bounding-rectangle* (left top right bottom) node
		 (case orientation
		   ((:horizontal :right)
		    (values (1- left) (+ top (floor (- bottom top) 2))))
		   ((:vertical :down)
		    (values (+ left (floor (- right left) 2)) (1- top)))))))
	(declare (dynamic-extent #'parent-attachment-position #'child-attachment-position))
	(multiple-value-bind (xoff yoff)
	    (convert-from-relative-to-absolute-coordinates
	      stream (output-record-parent graph))
	  (with-identity-transformation (stream)
	    (with-output-recording-options (stream :draw nil :record t)
	      (with-new-output-record (stream 'standard-sequence-output-record nil
				       :parent graph)
		(labels ((draw-edges (parent)
			   (dolist (child (graph-node-children parent))
			     (when (graph-node-children child)
			       (draw-edges child))
			     (multiple-value-bind (parent-x parent-y)
				 (parent-attachment-position parent)
			       (multiple-value-bind (child-x child-y)
				   (child-attachment-position child)
				 (translate-fixnum-positions xoff yoff
				   parent-x parent-y child-x child-y)
				 ;;--- This really needs to pass the objects, too
				 (apply arc-drawer stream
					parent-x parent-y child-x child-y
					arc-drawing-options))))))
		  (declare (dynamic-extent #'draw-edges))
		  (draw-edges root-node))))))))))


;;; Directed graphs, both acyclic and cyclic

(defclass directed-graph-output-record (basic-graph-output-record)
    ((n-generations :initform 0)))

;; For now, treat digraphs and DAGs the same way...
(define-graph-type :directed-graph directed-graph-output-record)
(define-graph-type :digraph directed-graph-output-record)
(define-graph-type :directed-acyclic-graph directed-graph-output-record)
(define-graph-type :dag directed-graph-output-record)

(defmethod generate-graph-nodes ((graph directed-graph-output-record) stream
				 root-objects object-printer inferior-producer
				 &key duplicate-key duplicate-test)
  (declare (dynamic-extent object-printer inferior-producer))
  (declare (ignore duplicate-test))
  (let ((hash-table (slot-value graph 'hash-table))
	(cutoff-depth (slot-value graph 'cutoff-depth))
	(n-generations 0))
    (labels ((inferior-mapper (function node)
	       (map nil function (funcall inferior-producer node)))
	     (new-node-function (parent-object parent-record child-object)
	       (let ((child-record
		       (with-new-output-record
			   (stream 'standard-graph-node-output-record)
			 (funcall object-printer child-object stream))))
		 (old-node-function parent-object parent-record child-object child-record)))
	     (old-node-function (parent-object parent-record child-object child-record)
	       (declare (ignore parent-object child-object))
	       (maxf n-generations
		     (maxf (node-generation child-record)
			   (if parent-record
			       (1+ (node-generation parent-record))
			       0)))
	       ;; Preserve the ordering of the nodes
	       (when parent-record
		 (unless (member parent-record (graph-node-parents child-record))
		   (setf (graph-node-parents child-record)
			 (nconc (graph-node-parents child-record)
				(list parent-record))))
		 (unless (member child-record (graph-node-children parent-record))
		   (setf (graph-node-children parent-record)
			 (nconc (graph-node-children parent-record)
				(list child-record)))))
	       child-record))
      (declare (dynamic-extent #'inferior-mapper #'new-node-function #'old-node-function))
      (traverse-graph root-objects #'inferior-mapper
		      hash-table duplicate-key
		      #'new-node-function #'old-node-function
		      cutoff-depth)
      (setf (slot-value graph 'n-generations) n-generations))
    (let ((root-nodes nil))
      (maphash #'(lambda (key node)
		   (declare (ignore key))
		   (when (and (graph-node-output-record-p node)
			      (null (graph-node-parents node)))
		     (push node root-nodes)))
	       hash-table)
      (setf (graph-root-nodes graph) (nreverse root-nodes))))
  graph)

(defstruct (generation-descriptor (:conc-name generation-) (:type list))
  generation				;generation number
  (breadth 0)				;sum of breadth of all nodes in this generation
  (depth 0)				;maximum depth of any node in this generation
  (start-depth nil)			;starting depth position for this generation
  (size 0)				;number of nodes in this generation
  ;; "Temporaries" used during placement
  breadth-so-far			;running placement on the breadth axis
  (inner-breadth-separation nil)	;separation between nodes
  (edge-breadth-separation nil)		;margin whitespace
  (touched nil))			;if T, use inner breadth separation

(defmethod layout-graph-nodes ((graph directed-graph-output-record) stream)
  (declare (ignore stream))
  (with-slots (orientation center-nodes
	       generation-separation within-generation-separation n-generations
	       root-nodes hash-table) graph
    (let ((start-x 0)
	  (start-y 0))
      (flet ((inferior-mapper (function node)
	       (map nil function (graph-node-children node)))
	     (yx-output-record-set-position* (record y x)
	       (output-record-set-position* record x y)))
	(declare (dynamic-extent #'inferior-mapper #'yx-output-record-set-position*))
	(multiple-value-bind (breadthfun depthfun set-positionfun start-breadth start-depth)
	    (ecase orientation
	      ((:vertical :down :up)
	       (values #'bounding-rectangle-width #'bounding-rectangle-height
		       #'output-record-set-position* start-x start-y))
	      ((:horizontal :right :left)
	       (values #'bounding-rectangle-height #'bounding-rectangle-width
		       #'yx-output-record-set-position* start-y start-x)))
	  (macrolet ((traverse (new-node-function &optional (old-node-function '#'false))
		       `(traverse-graph root-nodes #'inferior-mapper 
					hash-table #'identity
					,new-node-function ,old-node-function))
		     ;; "Breadth" is the width in vertical orientation, otherwise 
		     ;; it's the height.  "Depth" is vice-versa.
		     (breadth (node) `(funcall breadthfun ,node))
		     (depth (node) `(funcall depthfun ,node)))
	    (let ((generation-descriptors
		    (loop for generation to n-generations
			  collect (make-generation-descriptor
				    :generation generation
				    :breadth-so-far start-breadth)))
		  (max-gen-breadth 0) 
		  broadest-gen-descr)
	      (when (member orientation '(:up :left))
		(setq generation-descriptors (nreverse generation-descriptors)))
	      ;; Determine the breadth and depth of each generation
	      (flet ((collect-node-size (p ph child-node)
		       (declare (ignore p ph))
		       (let ((descr (assoc (node-generation child-node)
					   generation-descriptors)))
			 (incf (generation-size descr))
			 (incf (generation-breadth descr) (breadth child-node))
			 (maxf (generation-depth descr) (depth child-node)))))
		(declare (dynamic-extent #'collect-node-size))
		(traverse #'collect-node-size))
	      ;; Determine max-breadth and starting-depth
	      (loop with depth-so-far = start-depth
		    for descr in generation-descriptors do
		(let ((gen-breadth (generation-breadth descr)))
		  (when (> gen-breadth max-gen-breadth)
		    (setf max-gen-breadth gen-breadth
			  broadest-gen-descr descr)))
		(setf (generation-start-depth descr) depth-so-far)
		(incf depth-so-far (+ generation-separation (generation-depth descr))))
	      ;; Determine breadth-spacing
	      (incf max-gen-breadth
		    (* within-generation-separation (generation-size broadest-gen-descr)))
	      (loop for descr in generation-descriptors do
		(let ((excess (floor (- max-gen-breadth (generation-breadth descr))
				     (max (generation-size descr) 1))))
		  (setf (generation-inner-breadth-separation descr) excess)
		  (setf (generation-edge-breadth-separation descr) (floor excess 2))))
	      ;; Place nodes
	      (flet ((place-node (p ph child-node)
		       (declare (ignore p ph))
		       (let ((descr (assoc (node-generation child-node)
					   generation-descriptors)))
			 (incf (generation-breadth-so-far descr)
			       (if (generation-touched descr)
				   (generation-inner-breadth-separation descr)
				   (progn (setf (generation-touched descr) t)
					  (generation-edge-breadth-separation descr))))
			 (funcall set-positionfun
				  child-node
				  (generation-breadth-so-far descr)
				  (if center-nodes
				      (+ (generation-start-depth descr)
					 (floor (- (generation-depth descr) (depth child-node)) 2))
				      (generation-start-depth descr)))
			 (incf (generation-breadth-so-far descr) (breadth child-node)))))
		(declare (dynamic-extent #'place-node))
		(traverse #'place-node)))))))))

(defmethod layout-graph-edges ((graph directed-graph-output-record) 
			       stream arc-drawer arc-drawing-options)
  (with-slots (orientation root-nodes hash-table) graph
    (flet ((inferior-mapper (function node)
	     (map nil function (graph-node-children node)))
	   (north (node)
	     (with-bounding-rectangle* (left top right bottom) node
	       (declare (ignore bottom))
	       (values (floor (+ right left) 2) (1- top))))
	   (south (node)
	     (with-bounding-rectangle* (left top right bottom) node
	       (declare (ignore top))
	       (values (floor (+ right left) 2) (1+ bottom))))
	   (west (node)
	     (with-bounding-rectangle* (left top right bottom) node
	       (declare (ignore right))
	       (values (1- left) (floor (+ top bottom) 2))))
	   (east (node)
	     (with-bounding-rectangle* (left top right bottom) node
	       (declare (ignore left))
	       (values (1+ right) (floor (+ top bottom) 2)))))
      (declare (dynamic-extent #'inferior-mapper #'north #'south #'west #'east))
      (multiple-value-bind (parent-attach child-attach)
	  (ecase orientation
	    ((:vertical :down) (values #'south #'north))
	    ((:up) (values #'north #'south))
	    ((:horizontal :right) (values #'east #'west))
	    ((:left) (values #'west #'east)))
	(multiple-value-bind (xoff yoff)
	    (convert-from-relative-to-absolute-coordinates
	      stream (output-record-parent graph))
	  (with-identity-transformation (stream)
	    (with-output-recording-options (stream :draw nil :record t)
	      (with-new-output-record (stream 'standard-sequence-output-record nil
				       :parent graph)
		(labels ((draw-edge (parent ph child &optional ch)
			   (declare (ignore ph ch))
			   (when parent
			     (multiple-value-bind (parent-x parent-y)
				 (funcall parent-attach parent)
			       (multiple-value-bind (child-x child-y)
				   (funcall child-attach child)
				 (translate-fixnum-positions xoff yoff
				   parent-x parent-y child-x child-y)
				 ;;--- This really needs to pass the objects, too
				 (apply arc-drawer stream 
					parent-x parent-y child-x child-y
					arc-drawing-options))))))
		  (declare (dynamic-extent #'draw-edge)) 
		  (traverse-graph root-nodes #'inferior-mapper
				  hash-table #'identity
				  #'draw-edge #'draw-edge))))))))))

;; ROOT-OBJECTS is a sequence of the roots of the graph.  
;; INFERIOR-MAPPER is a function of two arguments, a function and an object
;; over whose inferiors the function should be applied.
;; HASH-TABLE is a table that is used to record and detect when an object has
;; already been included in the graph.
;; KEY is a function of one argument used to produce the hash table key.
;; There is no TEST function, since it is already captured in the hash table.
;; NEW-NODE-FUNCTION is a function of three arguments, the parent object, the
;; parent object's hash value, and the child object.  Its returned value will
;; be stored as the hash value of the child object.
;; OLD-NODE-FUNCTION is a function of four arguments, the parent object, the
;; parent object's hash value, the child object, and the child object's hash 
;; value.  Its returned value is ignored.
;; MAX-DEPTH is the cutoff depth of the tree, or NIL for no cutoff.
;;--- Potential bug: the cutoff (MAX-DEPTH) may fall short in that you may
;;--- reach a certain node at the maximum depth, mark that node as seen and
;;--- decline to descend into its inferiors, then find that node again
;;--- through a shorter path.  If you really want to fix this, write a
;;--- breadth-first descent of the graph.
(defun traverse-graph (root-objects inferior-mapper hash-table key
		       new-node-function old-node-function &optional max-depth)
  (declare (dynamic-extent inferior-mapper key new-node-function old-node-function))
  (check-type max-depth (or null integer))
  (clrhash hash-table)
  (labels
    ((traverse (parent-object parent-hashval object max-depth)
       (let ((object-hashval
	       (funcall new-node-function parent-object parent-hashval object)))
	 (setf (gethash (funcall key object) hash-table) object-hashval)
	 (when max-depth (decf max-depth))
	 (unless (eql max-depth 0)
	   (flet ((traverse1 (child-object)
		    (let ((child-key (funcall key child-object)))
		      (multiple-value-bind (child-hashval found)
			  (gethash child-key hash-table)
			(if found
			    (funcall old-node-function 
				     object object-hashval child-object child-hashval)
			    (traverse object object-hashval child-object max-depth))))))
	     (declare (dynamic-extent #'traverse1))
	     (funcall inferior-mapper #'traverse1 object))))))
    (declare (dynamic-extent #'traverse))
    (map nil #'(lambda (root)
		 (traverse nil nil root max-depth))
	 root-objects)))
