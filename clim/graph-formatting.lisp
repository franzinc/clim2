;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM; Base: 10; Lowercase: Yes -*-

;; $fiHeader: graph-formatting.lisp,v 1.7 91/08/05 14:29:31 cer Exp $

(in-package :clim)

"Copyright (c) 1989, 1990 International Lisp Associates.  All rights reserved."
"Copyright (c) 1991, Franz Inc. All rights reserved"

(defclass graph-output-record (linear-output-record)
     ((orientation :initarg :orientation)
      (generation-separation :initarg :generation-separation)
      (within-generation-separation :initarg :within-generation-separation)
      (root-node :accessor graph-root-node))
  (:default-initargs :size 50))

;;--- Not correct, since edges can overlap nodes. 
;;--- This really needs its own AUGMENT-DRAW-SET method.
(defmethod elements-never-overlap-p ((record graph-output-record)) t)

(defclass graph-node-output-record (linear-output-record)
     ((node-children :accessor graph-node-children)
      (node-parent :accessor graph-node-parent))
  (:default-initargs :size 5))

(define-output-record-constructor graph-node-output-record
				  (&key x-position y-position (size 25))
  :x-position x-position :y-position y-position :size size)

;;; (Copy of the comment in table-formatting)
;;; Cells have been positioned manually, probably.
;;; Also, this should probably be an :around method so that we can 
;;; drag the circle inside a cell and have the cell get updated automatically.
;;; Some extra bit will be needed.
;;; Until we do this, only table formatting really works (and arbitrary dragging
;;; of things above cells.
(defmethod tree-recompute-extent-1 ((record graph-node-output-record))
  (bounding-rectangle* record))

(defparameter *default-generation-separation* 20)
(defparameter *default-within-generation-separation* 10)

(defun format-graph-from-root (root-object object-printer inferior-producer
			       &key (stream *standard-output*)
				    (key #'identity)
				    (orientation ':horizontal)
				    (cutoff-depth nil)
				    (generation-separation
				      *default-generation-separation*)
				    (within-generation-separation
				      *default-within-generation-separation*)
				    (move-cursor t))
  (declare (dynamic-extent object-printer inferior-producer))
  (let ((graph nil)
	(root-node nil))
    (setq graph
	  (with-output-recording-options (stream :draw-p nil :record-p t)
	    (with-end-of-page-action (:allow stream)
	      (with-end-of-line-action (:allow stream)
		(with-new-output-record
		  (stream 'graph-output-record nil
		   :orientation orientation
		   :generation-separation generation-separation
		   :within-generation-separation within-generation-separation)
		  (setq root-node
			(format-graph-node root-object object-printer inferior-producer
					   :stream stream
					   :key key
					   :orientation orientation
					   :cutoff-depth cutoff-depth)))))))
    (setf (graph-root-node graph) root-node)
    (setf (graph-node-parent root-node) nil)
    (layout-graph-nodes-and-edges graph stream)
    (replay graph stream)
    (when move-cursor
      (move-cursor-beyond-output-record stream graph))
    graph))

(defun format-graph-node (element object-printer inferior-producer
			  &key stream key orientation cutoff-depth (depth 0))
  (declare (dynamic-extent object-printer inferior-producer))
  (when cutoff-depth
    (incf depth)
    (when (> depth cutoff-depth)
      (return-from format-graph-node nil)))
  (let ((inferiors nil))
    (dolist (inferior (funcall inferior-producer element))
      (let ((node (format-graph-node inferior object-printer inferior-producer
				     :stream stream
				     :key key
				     :orientation orientation
				     :cutoff-depth cutoff-depth
				     :depth depth)))
	(when node
	  (push node inferiors))))
    (let ((this-node (with-new-output-record (stream 'graph-node-output-record)
		       (funcall object-printer element stream))))
      (setf (graph-node-children this-node) (nreverse inferiors))
      (dolist (child (graph-node-children this-node))
	(setf (graph-node-parent child) this-node))
      this-node)))

(defun layout-graph-nodes-and-edges (graph stream)
  ;; 0,0 are passed in here because we want the nodes to be positioned
  ;; relative to the graph!
  (layout-graph-nodes (graph-root-node graph) 0 0 (slot-value graph 'orientation)
		      (slot-value graph 'generation-separation)
		      (slot-value graph 'within-generation-separation))
  (layout-graph-edges graph stream)
  (tree-recompute-extent graph))

(defun graph-node-width (node)  (bounding-rectangle-width node))

(defun graph-node-height (node) (bounding-rectangle-height node))

(defmethod graph-node-x ((node graph-node-output-record)) (bounding-rectangle-left node))

(defmethod (setf graph-node-x) (new-value (node graph-node-output-record))
  (output-record-set-position* node new-value (bounding-rectangle-top node)))

(defmethod graph-node-y ((node graph-node-output-record)) (bounding-rectangle-top node))

(defmethod (setf graph-node-y) (new-value (node graph-node-output-record))
  (output-record-set-position* node (bounding-rectangle-left node) new-value))

(defmacro layout-graph-nodes-body (driver-function-name node-var height-var width-var
				   tallest-sibling-var
				   generation-separation-var within-generation-separation-var)
  `(let ((inferiors (graph-node-children ,node-var))
	 (start-width ,width-var)
	 (height-margin ,generation-separation-var)
	 (width-margin ,within-generation-separation-var)
	 (node-width (node-width ,node-var))
	 (tallest-inferior 0))
     (declare (fixnum start-width height-margin width-margin node-width tallest-inferior))
     (dolist (inferior inferiors)
       (maxf tallest-inferior (node-height inferior)))
     (dolist (inferior inferiors)
       (incf start-width (the fixnum (round width-margin 2)))
       (let ((inferior-width (,driver-function-name
			        inferior
				(the fixnum (+ ,height-var ,tallest-sibling-var height-margin))
				start-width
				tallest-inferior)))
	 (incf start-width inferior-width))
       (incf start-width (the fixnum (round width-margin 2))))
     (let* ((total-inferior-width (the fixnum (- start-width ,width-var)))
	    (my-width
	      (the fixnum (+ ,width-var
			     (round (max 0 (the fixnum (- total-inferior-width node-width)))
				    2)))))
       (setf (node-height-start ,node-var) ,height-var
	     (node-width-start ,node-var) my-width)
       (max total-inferior-width (node-width ,node-var)))))

;;--- For now, rely on the fact that we know that nodes are built on rectangles.
(defun layout-graph-nodes (root-node start-x start-y orientation 
			   generation-separation within-generation-separation)
  (ecase orientation
    (:vertical
      (macrolet ((node-width (node) `(the fixnum (graph-node-width ,node)))
		 (node-height (node) `(the fixnum (graph-node-height ,node)))
		 (node-width-start (node) `(the fixnum (graph-node-x ,node)))
		 (node-height-start (node) `(the fixnum (graph-node-y ,node))))
	(labels ((layout-graph (root start-x start-y tallest-sibling)
		   (layout-graph-nodes-body layout-graph root
					    start-x start-y tallest-sibling
					    generation-separation
					    within-generation-separation)))
	  ;; Yes, this really is "start-y" followed by "start-x"
	  (layout-graph root-node start-y start-x (node-height root-node)))))
    (:horizontal
      (macrolet ((node-width (node) `(the fixnum (graph-node-height ,node)))
		 (node-height (node) `(the fixnum (graph-node-width ,node)))
		 (node-width-start (node) `(the fixnum (graph-node-y ,node)))
		 (node-height-start (node) `(the fixnum (graph-node-x ,node))))
	(labels ((layout-graph (root start-x start-y tallest-sibling)
		   (layout-graph-nodes-body layout-graph root
					    start-x start-y tallest-sibling
					    generation-separation
					    within-generation-separation)))
	  (layout-graph root-node start-x start-y (node-height root-node)))))))

(defun layout-graph-edges (graph stream)
  (let ((orientation (slot-value graph 'orientation)))
    (with-output-recording-options (stream :draw-p nil :record-p t)
      (with-new-output-record (stream 'linear-output-record nil
			       :parent graph)
	(multiple-value-bind (xoff yoff)
	    (convert-from-relative-to-absolute-coordinates
	      stream (output-record-parent graph))
	  (labels ((draw-edges (parent)
		     (multiple-value-bind (parent-x parent-y)
			 (parent-attachment-position* parent orientation)
		       (dolist (child (graph-node-children parent))
			 (when (graph-node-children child)
			   (draw-edges child))
			 (multiple-value-bind (child-x child-y)
			     (child-attachment-position* child orientation)
			   (translate-fixnum-positions xoff yoff
			     parent-x parent-y child-x child-y)
			   (draw-line* stream parent-x parent-y child-x child-y))))))
	    (declare (dynamic-extent #'draw-edges))
	    (draw-edges (graph-root-node graph))))))))

(defmethod parent-attachment-position* ((node graph-node-output-record) orientation)
  (with-bounding-rectangle* (left top right bottom) node
    (case orientation
      (:horizontal
	(values (1+ right) (the fixnum (+ top (floor (- bottom top) 2)))))
      (:vertical
	(values (the fixnum (+ left (floor (- right left) 2))) (1+ bottom))))))

(defmethod child-attachment-position* ((node graph-node-output-record) orientation)
  (with-bounding-rectangle* (left top right bottom) node
    (case orientation
      (:horizontal
	(values (1- left) (the fixnum (+ top (floor (- bottom top) 2)))))
      (:vertical
	(values (the fixnum (+ left (floor (- right left) 2))) (1- top))))))
