;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics Inc.  All rights reserved.
;;; Portions copyright (c) 1992 Franz, Inc.  All rights reserved."

(defvar *graph-type-record-type-alist* nil)

(defmacro define-graph-type (graph-type class)
  `(let ((old (assoc ',graph-type *graph-type-record-type-alist*)))
     (if old
         (setf (second old) ',class)
         (setq *graph-type-record-type-alist*
               (nconc *graph-type-record-type-alist* (list (list ',graph-type ',class)))))))


(defparameter *default-generation-separation* (coordinate 20))
(defparameter *default-within-generation-separation* (coordinate 10))


;;; Graph node tables

;; Like hash tables, only a bit more general.  It's reasonable for someone
;; to want to use a "custom" duplicate test that is not supported by X3J13
;; hash tables, for example, =.
(defclass graph-node-table ()
    ((test :initarg :test)
     (table :initarg :table)))

(defun make-graph-node-table (&key (test 'eql) (size 50))
  (let ((table
          (and (or (member test '(eq eql equal 
                                  #-Cloe-Runtime equalp
                                  #+Genera string-equal #+Genera string=
                                  #+Genera char-equal #+Genera char=))
                   (eq test (load-time-value #'eq))
                   (eq test (load-time-value #'eql))
                   (eq test (load-time-value #'equal))
                   #-Cloe-Runtime (eq test (load-time-value #'equalp)))
               (make-hash-table :test test :size size))))
    (make-instance 'graph-node-table
      :test test :table table)))

(defmethod clear-node-table ((node-table graph-node-table))
  (with-slots (table) node-table
    (if (listp table)
        (setq table nil)
        (clrhash table))))

(defmethod get-node-table (key (node-table graph-node-table) &optional default)
  (with-slots (table test) node-table
    (if (listp table)
        (let ((entry (assoc key table :test test)))
          (if entry
              (values (cdr entry) t)
              (values default nil)))
        (gethash key table default))))

(defmethod (setf get-node-table) (value key (node-table graph-node-table) &optional default)
  #-aclpc (declare (ignore default))
  (with-slots (table test) node-table
    (if (listp table)
        (let ((entry (assoc key table :test test)))
          (if entry
              (setf (cdr entry) value)
              (push (cons key value) table)))
        (setf (gethash key table) value))
    value))

(defmethod map-node-table (function (node-table graph-node-table))
  (declare (dynamic-extent function))
  (with-slots (table) node-table
    (if (listp table)
        (dolist (entry table)
          (funcall function (car entry) (cdr entry)))
        (maphash function table))))

(defresource graph-node-table (&key (test 'eql))
  :constructor (make-graph-node-table :test test)
  :deinitializer (clear-node-table graph-node-table))


;;; The grapher

(define-protocol-class graph-output-record (output-record))

;; The basic graph output record class
(defclass basic-graph-output-record 
          (standard-sequence-output-record graph-output-record)
    ((graph-type :initarg :graph-type)
     ;; The output records corresponding to the root nodes
     (root-nodes :initform nil :accessor graph-root-nodes)
     (hash-table :initform nil :initarg :hash-table)
     ;; Stores things like :ORIENTATION, :MERGE-DUPLICATES, etc.
     (properties :initarg :properties))
  (:default-initargs :size 30))

;;--- Not correct, since edges can overlap nodes. 
;;--- This really needs its own AUGMENT-DRAW-SET method.
(defmethod children-never-overlap-p ((record basic-graph-output-record)) t)

;; Recover the hash table during incremental redisplay
(defmethod match-output-records :after
           ((record basic-graph-output-record) &key hash-table &allow-other-keys)
  (when hash-table
    (setf (slot-value record 'hash-table) hash-table)))


(define-protocol-class graph-node-output-record (output-record))

;; The basic graph node output record class
(defclass standard-graph-node-output-record 
          (standard-sequence-output-record graph-node-output-record)
    ((generation :accessor graph-node-generation :initform 0)
     ;; The output records corresponding to this node's parents and children
     (node-children :accessor graph-node-children :initform nil)
     (node-parents :accessor graph-node-parents :initform nil)
     (object :accessor graph-node-object :initarg :object))
  (:default-initargs :size 5))

;; Take, but ignore, :DUPLICATE-KEY and :DUPLICATE-TEST
(defmethod initialize-instance :after ((node standard-graph-node-output-record)
                                       &key duplicate-key duplicate-test &allow-other-keys)
  (declare (ignore duplicate-key duplicate-test))
  nil)

(define-output-record-constructor standard-graph-node-output-record
                                  (&key object x-position y-position (size 25)
                                        duplicate-key duplicate-test)
  :object object :x-position x-position :y-position y-position :size size)

(defmethod tree-recompute-extent-1 ((record standard-graph-node-output-record))
  (bounding-rectangle* record))

(defmethod graph-node-x ((node standard-graph-node-output-record)) 
  (bounding-rectangle-left node))

(defmethod (setf graph-node-x) (new-value (node standard-graph-node-output-record))
  (output-record-set-position node new-value (bounding-rectangle-top node)))

(defmethod graph-node-y ((node standard-graph-node-output-record))
  (bounding-rectangle-top node))

(defmethod (setf graph-node-y) (new-value (node standard-graph-node-output-record))
  (output-record-set-position node (bounding-rectangle-left node) new-value))

;; Graph node output records match if their objects match
(defmethod match-output-records ((record standard-graph-node-output-record) 
                                 &key object duplicate-key duplicate-test &allow-other-keys)
  (when (or (and (null object)
                 (null (graph-node-object record)))
            (and duplicate-key 
                 duplicate-test
                 (funcall duplicate-test (funcall duplicate-key object)
                          (funcall duplicate-key (graph-node-object record)))))
    (setf (graph-node-parents record) nil
          (graph-node-children record) nil)
    t))

;; [bug18430]: format-graph-from-root moved down to avoid dynamic-extent
;; warning without defun-proto form

(defun format-graph-from-roots (root-objects object-printer inferior-producer
                                &key (stream *standard-output*)
                                     (orientation ':horizontal) (center-nodes nil)
                                     (cutoff-depth nil)
                                     (merge-duplicates nil)
                                     (graph-type (if merge-duplicates :digraph :tree))
                                     (duplicate-key #'identity key-supplied-p)
                                     (duplicate-test #'eql)
                                     (arc-drawer #'draw-linear-arc)
                                     (arc-drawing-options nil)
                                     (generation-separation
                                       *default-generation-separation*)
                                     (within-generation-separation
                                       *default-within-generation-separation*)
                                     (maximize-generations #+allegro t #-allegro nil)
                                     ;; These `offpage' connector print functions
                                     ;; should receive more useful id in addition
                                     ;; to an identification number, such as the
                                     ;; from and to nodes.
                                     (offpage-connector-out-printer
                                       #'(lambda (s n) (format s ">~D" n)))
                                     (offpage-connector-in-printer
                                       #'(lambda (s n) (format s "~D>" n)))
                                     (store-objects t) (move-cursor t))
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
  (using-resource (hash-table graph-node-table :test duplicate-test)
    (let* ((record-type (second (assoc graph-type *graph-type-record-type-alist*)))
           (graph-record 
             (with-output-recording-options (stream :draw nil :record t)
               (with-end-of-page-action (stream :allow)
                 (with-end-of-line-action (stream :allow)
                   (with-new-output-record 
                       (stream record-type graph-record
                        :graph-type graph-type
                        :properties `(:orientation ,orientation
                                      :center-nodes ,center-nodes
                                      :cutoff-depth ,cutoff-depth
                                      :merge-duplicates ,merge-duplicates
                                      :maximize-generations ,maximize-generations
                                      :generation-separation
                                        ,(coordinate generation-separation)
                                      :within-generation-separation 
                                        ,(coordinate within-generation-separation))
                        :hash-table hash-table)
                     (generate-graph-nodes
                       graph-record stream
                       root-objects object-printer inferior-producer
                       :duplicate-key  duplicate-key 
                       :duplicate-test duplicate-test
                       :offpage-connector-out-printer offpage-connector-out-printer
                       :offpage-connector-in-printer  offpage-connector-in-printer)
                     graph-record))))))
      (unwind-protect
          (progn
            (layout-graph-nodes graph-record stream arc-drawer arc-drawing-options)
            (layout-graph-edges graph-record stream arc-drawer arc-drawing-options))
        ;; Flush any references to the user's objects if he doesn't want
        ;; them stored
        (unless store-objects
          (map-over-output-records
            #'(lambda (r)
                (when (graph-node-output-record-p r)
                  (setf (graph-node-object r) nil)))
            graph-record))
        ;; We're going to free the hash table as we exit, so make sure
        ;; there are no pointers to it
        (setf (slot-value graph-record 'hash-table) nil))
      (tree-recompute-extent graph-record)
      (replay graph-record stream)
      (when move-cursor
        (move-cursor-beyond-output-record stream graph-record))
      graph-record)))

;; For compatibility...
(defun format-graph-from-root (root-object object-printer inferior-producer
                               &rest keys
                               &key (stream *standard-output*)
                                    (orientation ':horizontal) (center-nodes nil)
                                    (cutoff-depth nil)
                                    (merge-duplicates nil)
                                    (graph-type (if merge-duplicates :digraph :tree))
                                    (key #'identity) (test #'eql)
                                    (arc-drawer #'draw-linear-arc) 
                                    (arc-drawing-options nil)
                                    (generation-separation
                                      *default-generation-separation*)
                                    (within-generation-separation
                                      *default-within-generation-separation*)
                                    (maximize-generations #+allegro t #-allegro nil)
                                    (store-objects t) (move-cursor t))
  (declare (dynamic-extent keys object-printer inferior-producer)
           (ignore stream orientation center-nodes cutoff-depth
                   arc-drawer arc-drawing-options graph-type move-cursor store-objects
                   generation-separation within-generation-separation maximize-generations))
  (with-keywords-removed (keys keys '(:merge-duplicates :key :test))
    (apply #'format-graph-from-roots
           (list root-object) object-printer inferior-producer
           (if merge-duplicates
               (append `(:merge-duplicates ,merge-duplicates
                         :duplicate-key ,key :duplicate-test ,test) keys)
               keys))))



(defun draw-linear-arc (stream from-object to-object x1 y1 x2 y2 &rest drawing-options
                        &key path &allow-other-keys)
  (declare (dynamic-extent drawing-options))
  (declare (ignore from-object to-object))
  (loop
      (unless path (return nil))
      (apply #'draw-line* stream x1 y1 (setq x1 (pop path)) (setq y1 (pop path)) drawing-options))
  (apply #'draw-line* stream x1 y1 x2 y2 drawing-options))


;; Split out to avoid consing a closure environment
(defun call-arc-drawer-1 (stream arc-drawer parent-object child-object
                          parent parent-x parent-y child child-x child-y
                          arc-drawing-options)
  (declare (dynamic-extent arc-drawer))
  (declare (ignore child parent))
  #---ignore        ;--- this is the wrong thing...
  (apply arc-drawer stream parent-object child-object
         parent-x parent-y child-x child-y
         arc-drawing-options)
  #+++ignore        ;--- ...but the right thing doesn't work either
  (updating-output (stream :unique-id (list parent child)
                           :id-test #'equal
                           :cache-value (list parent-x parent-y
                                              child-x child-y)
                           :cache-test #'equal)
    (apply arc-drawer stream parent-object child-object
           parent-x parent-y child-x child-y
           arc-drawing-options)))

(defun-inline call-arc-drawer (stream arc-drawer parent-object child-object
                               parent parent-x parent-y child child-x child-y
                               arc-drawing-options)
  (declare (dynamic-extent arc-drawer))
  (if (or (stream-redisplaying-p stream)
          (stream-current-redisplay-record stream))
      (call-arc-drawer-1 stream arc-drawer parent-object child-object
                         parent parent-x parent-y child child-x child-y
                         arc-drawing-options)
      (apply arc-drawer stream parent-object child-object
             parent-x parent-y child-x child-y
             arc-drawing-options)))


;;; Tree graphs

(defclass tree-graph-output-record (basic-graph-output-record) ())

(define-graph-type :tree tree-graph-output-record)

(defmethod generate-graph-nodes ((graph tree-graph-output-record) stream
                                 root-objects object-printer inferior-producer
                                 &key duplicate-key duplicate-test
                                      offpage-connector-out-printer
                                      offpage-connector-in-printer)
  #-aclpc (declare (ignore offpage-connector-out-printer offpage-connector-in-printer))
  (let* ((properties (slot-value graph 'properties))
         (cutoff-depth (getf properties :cutoff-depth)))
    (labels ((format-node (object &optional (depth 1))
               (when (and cutoff-depth (> depth cutoff-depth))
                 (return-from format-node nil))
               (let ((children nil))
                 (dolist (inferior (funcall inferior-producer object))
                   (let ((node (format-node inferior (1+ depth))))
                     (when (and node (not (member node children)))
                       ;; Don't add the same node to the children more than
                       ;; once, which can come up during redisplay
                       (setq children (nconc children (list node))))))
                 (let ((this-node 
                         (with-stream-cursor-position-saved (stream)
                           (with-new-output-record
                               (stream 'standard-graph-node-output-record nil
                                :object object
                                :duplicate-key duplicate-key
                                :duplicate-test duplicate-test)
                             (funcall object-printer object stream)))))
                   (setf (graph-node-children this-node) children)
                   (dolist (child (graph-node-children this-node))
                     (setf (graph-node-parents child) (list this-node)))
                   this-node))))
      (setf (graph-root-nodes graph)
            (map 'list #'format-node root-objects))))
  graph)

(defmethod layout-graph-nodes ((graph tree-graph-output-record) stream
                               arc-drawer arc-drawing-options)
  (declare (ignore stream arc-drawer arc-drawing-options))
  (with-slots (root-nodes) graph
    (let* ((properties (slot-value graph 'properties))
           (orientation (getf properties :orientation))
           (generation-separation (getf properties :generation-separation))
           (within-generation-separation (getf properties :within-generation-separation))
           (center-nodes (getf properties :center-nodes))
           (start-x (coordinate 0))
           (start-y (coordinate 0)))
      (dolist (root-node root-nodes)
        (macrolet
          ((layout-body (driver-function-name 
                         node-var depth-var breadth-var tallest-sibling-var
                         generation-separation-var within-generation-separation-var)
             `(let ((children (graph-node-children ,node-var))
                    (start-breadth ,breadth-var)
                    (node-breadth (node-breadth ,node-var))
                    (breadth-margin ,within-generation-separation-var)
                    (depth-margin ,generation-separation-var)
                    (tallest-child (coordinate 0)))
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
                  (setf (node-depth-start ,node-var) (if center-nodes 
                                                         (+ ,depth-var
                                                            (round (- ,tallest-sibling-var (node-depth ,node-var))
                                                                   2))
                                                       ,depth-var)
                        (node-breadth-start ,node-var) my-breadth)
                  ;; Returns the breadth of the graph as a result
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
                 (incf start-x
                       (layout-graph root-node start-y start-x
                                     (node-depth root-node))))))
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
                 (incf start-y
                       (layout-graph root-node start-x start-y
                                     (node-depth root-node))))))))))))

(defmethod layout-graph-edges ((graph tree-graph-output-record) stream
                               arc-drawer arc-drawing-options)
  (with-slots (root-nodes properties) graph
    (let ((orientation (getf properties :orientation)))
      (dolist (root-node root-nodes)
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
                                   (translate-coordinates xoff yoff
                                     parent-x parent-y child-x child-y)
                                   (call-arc-drawer stream arc-drawer
                                                    (graph-node-object parent) 
                                                    (graph-node-object child)
                                                    parent parent-x parent-y
                                                    child child-x child-y
                                                    arc-drawing-options))))))
                    (declare (dynamic-extent #'draw-edges))
                    (draw-edges root-node)))))))))))


;;; Directed graphs, both acyclic and cyclic

#|

The basic strategy is that each node appearing more than once in the
graph is reliably assigned the maximum generation (ply) to which it
belongs, instead of depending on which occurence was encountered
first.  Then `filler' output records are inserted to fill in missing
generations in any link.  These fillers then participate in regular
node layout.  This results in some unnecessarily squiggley lines, but
generally the behavior is reasonable.

In graphs where a node with large "depth" is adjacent to a small depth
node with large fanout connections there is still the possibly that a
connector will overwrite a node.  This could be fixed with one of
several strategies: Pad the short node's depth somehow, which moves a
lot of complexity onto the arc-drawing functions; or add a whole
generation of additional filler nodes, which would be laid out to
constrain the set of children of each parent to lie within that
parents breadth.  I'd draw a picture of it weren't so tedious...

The unnecessary squiggles could be fixed without too much work by
postprocessing the links, successively moving filler nodes when there
is room to line them up with one of it's connections in the adjacent
generation.

Useful rfe: Enforce some limit on the maximum absolute value slope on
a connection.  Extra inter-generation spacing would be added to limit
the slope, since high-slope lines are the most visually confusing,
especially when many occur together.

As an additional hack, the digraph grapher handles circularities by
creating a pair of `offpage' connectors.  This isn't as elegant as
plotting circular paths backwards, but the current hack results in a
clear visual diagram with little effort.  The offpage-connector-out-printer
and offpage-connector-in-printer args to format-graph-from-roots are
something of a crock -- perhaps we can avoid documenting them until
better treatment of cyclic graphs is devised.  BTW, before this hack,
a circular graph with no roots (i.e. all nodes have at least one parent)
would blow the stack, so don't undertake a more ambitious fix for
circular graphs without accounting for this case.

|#

(defclass directed-graph-output-record (basic-graph-output-record)
    ((n-generations :initform 0)))

;; For now, treat digraphs and DAGs the same way...
(define-graph-type :directed-graph directed-graph-output-record)
(define-graph-type :digraph directed-graph-output-record)
(define-graph-type :directed-acyclic-graph directed-graph-output-record)
(define-graph-type :dag directed-graph-output-record)

;; These implement the dummy nodes inserted when an arc must span more
;; than one generation.
(defclass graph-node-filler-output-record   (standard-graph-node-output-record) ())
(defclass graph-node-filler-output-record-1 (standard-graph-node-output-record) ())

(defmethod filler-node-p ((node standard-graph-node-output-record)) nil)
(defmethod filler-node-p ((node graph-node-filler-output-record)) t)
(defmethod filler-node-p ((node graph-node-filler-output-record-1)) t)

;; These implement the `offpage' connectors for cyclic graphs.
(defclass graph-node-connector-output-record
          (standard-graph-node-output-record)
    ((id :accessor connector-id :initarg :connector-id)))

(defclass graph-node-connector-in-output-record (graph-node-connector-output-record) 
    () 
  (:default-initargs :object nil))

(defclass graph-node-connector-out-output-record (graph-node-connector-output-record)
    ()
  (:default-initargs :object nil))

(defclass grapher-fake-object ()
  ((data :initarg :data :reader grapher-fake-object-data)))

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
  (clear-node-table hash-table)
  (labels
    ((traverse (parent-object parent-hashval object max-depth)
       (let ((object-hashval
               (funcall new-node-function parent-object parent-hashval object)))
         (setf (get-node-table (funcall key object) hash-table) object-hashval)
         (when max-depth (decf max-depth))
         (unless (eq max-depth 0)
           (flet ((traverse1 (child-object)
                    (let ((child-key (funcall key child-object)))
                      (multiple-value-bind (child-hashval found)
                          (get-node-table child-key hash-table)
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

(defmethod generate-graph-nodes ((graph directed-graph-output-record) stream
                                 root-objects object-printer inferior-producer
                                 &key duplicate-key duplicate-test
                                      offpage-connector-out-printer
                                      offpage-connector-in-printer)
  (declare (dynamic-extent object-printer inferior-producer))
  (with-slots (n-generations) graph
    (let* ((hash-table (slot-value graph 'hash-table))
           (graph-type (slot-value graph 'graph-type))
           (properties (slot-value graph 'properties))
           (cutoff-depth (getf properties :cutoff-depth))
           (maximize-generations (getf properties :maximize-generations))
           (root-nodes nil)
           (connectors-in nil))
      (labels ((inferior-mapper (function node)
                 (map nil function (funcall inferior-producer node)))
               (new-node-function (parent-object parent-record child-object)
                 (let ((child-record
                         (with-stream-cursor-position-saved (stream)
                           (with-new-output-record
                               (stream 'standard-graph-node-output-record nil
                                :object child-object
                                :duplicate-key duplicate-key
                                :duplicate-test duplicate-test)
                             (funcall object-printer child-object stream)))))
                   ;; This guarantees that the next phase will have at least one
                   ;; node from which to start.  Otherwise the entire graph gets
                   ;; lost.  If the first node isn't really a root, it will be
                   ;; deleted from the set of roots when the cycle is
                   ;; detected.
                   (when (null root-nodes)
                     (push child-record root-nodes))
                   (old-node-function parent-object parent-record child-object child-record)))
               (old-node-function (parent-object parent-record child-object child-record)
                 (declare (ignore parent-object child-object))
                 (unless maximize-generations
                   (let ((old-generation (graph-node-generation child-record)))
                     ;; Set the generation of this node to 1 greater than the parent,
                     ;; and keep track of the highest generation encountered.
                     (maxf n-generations
                           (maxf (graph-node-generation child-record)
                                 (if parent-record
                                     (1+ (graph-node-generation parent-record))
                                     0)))
                     ;; If the child-record got its generation adjusted, then we must
                     ;; adjust the generation-number of already-processed children,
                     ;; and their children, etc.
                     (unless (eql (graph-node-generation child-record) old-generation)
                       (increment-generation child-record))))
                 ;; Preserve the ordering of the nodes.  Generation numbers are
                 ;; computed later in the case when MAXIMIZE-GENERATIONS is T.
                 (when parent-record
                   (unless (member parent-record (graph-node-parents child-record))
                     (setf (graph-node-parents child-record)
                           (nconc (graph-node-parents child-record)
                                  (list parent-record))))
                   (unless (member child-record (graph-node-children parent-record))
                     (setf (graph-node-children parent-record)
                           (nconc (graph-node-children parent-record)
                                  (list child-record)))))
                 child-record)
               (increment-generation (record)
                 (let ((new-generation (1+ (graph-node-generation record))))
                   (dolist (child (graph-node-children record))
                     ;; Remember which generation the child belonged to.
                     (let ((old-generation (graph-node-generation child)))
                       (maxf n-generations
                             (maxf (graph-node-generation child) new-generation))
                       ;; If it has changed, fix up the next generation recursively.
                       (unless (eql (graph-node-generation child) old-generation)
                         (increment-generation child)))))))
        (declare (dynamic-extent #'inferior-mapper #'increment-generation
                                 #'new-node-function #'old-node-function))
        (traverse-graph root-objects #'inferior-mapper
                        hash-table duplicate-key
                        #'new-node-function #'old-node-function
                        cutoff-depth))
      (map-node-table #'(lambda (key node)
                          (declare (ignore key))
                          (when (and (typep node 'graph-node-output-record)
                                     (null (graph-node-parents node)))
                            (pushnew node root-nodes)))
                      hash-table)
      (when (and (member graph-type '(:directed-graph :digraph))
                 offpage-connector-in-printer offpage-connector-out-printer)
        (labels 
          ((break-cycles (node &optional path)
             (do ((c (graph-node-children node) (cdr c)))
                 ((null c))
               (let ((child (car c)))
                 (if (member child path)
                     (let* ((id (connector-id
                                  (or (find child connectors-in
                                            :key #'(lambda (x)
                                                     (car (graph-node-children x))))
                                      (let* ((id (1+ (length connectors-in)))
                                             (rec (with-new-output-record
                                                      (stream 'graph-node-connector-in-output-record nil
                                                              :object
                                                              (make-instance 'grapher-fake-object
                                                                             :data `(:in-connector ,node ,child))
                                                       :connector-id id)
                                                    (funcall offpage-connector-in-printer
                                                             stream id))))
                                        (push rec connectors-in)
                                        (setf (graph-node-children rec) (list child))
                                        ;;--- What about ordering?
                                        (push rec (graph-node-parents child))
                                        (setf root-nodes (delete child root-nodes))
                                        rec)))))
                       (setf (car c)
                             (with-new-output-record
                                 (stream 'graph-node-connector-out-output-record nil
                                         :object (make-instance 'grapher-fake-object
                                                  :data `(:out-connector ,node ,child))
                                  :connector-id id)
                               (funcall offpage-connector-out-printer stream id))))
                     (break-cycles child (cons node path)))))))
          (declare (dynamic-extent #'break-cycles))
          (map nil #'break-cycles (copy-list root-nodes))))
      (setf (slot-value graph 'root-nodes) 
            (nconc (nreverse connectors-in) (nreverse root-nodes)))
      (when maximize-generations
        (compute-graph-effective-generations graph)
        (add-graph-filler-output-records graph stream))))
  graph)

(defmethod compute-graph-effective-generations ((graph directed-graph-output-record))
  (with-slots (root-nodes n-generations) graph
    (labels ((traverse (node ply)
               (with-slots (generation) node
                 (maxf generation ply)
                 (let ((children (graph-node-children node))
                       (gen1 (1+ generation)))
                   (if children                        ;save time -- do one or the other
                       (dolist (child children)
                         (traverse child gen1))
                       (maxf n-generations generation))))))
      (declare (dynamic-extent #'traverse))
      (dolist (root root-nodes)
        (traverse root 0)))))

(defmethod add-graph-filler-output-records ((graph directed-graph-output-record) stream)
  (with-slots (root-nodes) graph
    (labels ((traverse (node)
               (let* ((gen (graph-node-generation node))
                      (gen1 (1+ gen))
                      (children (graph-node-children node)))
                 (do ((c children (cdr c)))
                     ((null c))
                   (let ((child (car c)))
                     (if (> (graph-node-generation child) gen1)
                         (let ((filler (with-new-output-record
                                           (stream 'graph-node-filler-output-record)
                                         nil)))
                           (setf (graph-node-children filler) (list child)
                                 (graph-node-parents filler)  (list node)
                                 (graph-node-object filler) 
                                 (make-instance 'grapher-fake-object
                                                :data `(:filler ,filler ,(graph-node-object node)))
                                 (car c) filler
                                 (graph-node-generation filler) gen1)
                           (traverse filler))
                         (traverse child)))))))
      (declare (dynamic-extent #'traverse))
      (dolist (root root-nodes)
        (traverse root)))))

;;

;;

(defstruct (generation-descriptor (:conc-name generation-) (:type list))
  generation                                ;generation number
  (breadth 0)                                ;sum of breadth of all nodes in this generation
  (depth 0)                                ;maximum depth of any node in this generation
  (start-depth nil)                        ;starting depth position for this generation
  (size 0)                                ;number of nodes in this generation
  ;; "Temporaries" used during placement
  breadth-so-far                        ;running placement on the breadth axis
  (inner-breadth-separation nil)        ;separation between nodes
  (edge-breadth-separation nil)                ;margin whitespace
  (touched nil))                        ;if T, use inner breadth separation

(defmethod layout-graph-nodes ((graph directed-graph-output-record) stream
                               arc-drawer arc-drawing-options)
  (with-slots (root-nodes hash-table n-generations) graph
    (when root-nodes
      (let* ((properties (slot-value graph 'properties))
             (orientation (getf properties :orientation))
             (center-nodes (getf properties :center-nodes))
             (generation-separation (getf properties :generation-separation))
             (within-generation-separation (getf properties :within-generation-separation))
             (maximize-generations (getf properties :maximize-generations))
             (start-x (coordinate 0))
             (start-y (coordinate 0)))
        (flet ((inferior-mapper (function node)
                 (map nil function (graph-node-children node)))
               (yx-output-record-set-position (record y x)
                 (output-record-set-position record x y)))
          (declare (dynamic-extent #'inferior-mapper #'yx-output-record-set-position))
          (multiple-value-bind (breadthfun depthfun set-positionfun start-breadth start-depth)
              (ecase orientation
                ((:vertical :down :up)
                 (values #'bounding-rectangle-width #'bounding-rectangle-height
                         #'output-record-set-position start-x start-y))
                ((:horizontal :right :left)
                 (values #'bounding-rectangle-height #'bounding-rectangle-width
                         #'yx-output-record-set-position start-y start-x)))
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
                    (max-gen-breadth (coordinate 0)) 
                    broadest-gen-descr)
                (when (member orientation '(:up :left))
                  (setq generation-descriptors (nreverse generation-descriptors)))
                ;; Determine the breadth and depth of each generation
                (flet ((collect-node-size (p ph child-node)
                         (declare (ignore p ph))
                         (let ((descr (assoc (graph-node-generation child-node)
                                             generation-descriptors)))
                           (incf (generation-size descr))
                           (incf (generation-breadth descr) (breadth child-node))
                           (maxf (generation-depth descr) (depth child-node)))))
                  (declare (dynamic-extent #'collect-node-size))
                  (traverse #'collect-node-size))
                ;; Replace all filler nodes, giving them the max depth in their generation.
                (when maximize-generations
                  (labels
                    ((replace-filler-nodes (parent)
                       (let ((children (graph-node-children parent)))
                         (do ((c children (cdr c)))
                             ((null c))
                           (let ((node (car c)))
                             (when (typep node 'graph-node-filler-output-record)
                               ;;-- Perhaps all we really need to do is
                               ;;-- change the size of it?
                               (delete-output-record node graph)
                               (let* ((depth (generation-depth
                                               (assoc (graph-node-generation node)
                                                      generation-descriptors)))
                                      (new (with-new-output-record
                                               (stream 'graph-node-filler-output-record-1 nil
                                                       :parent graph)
                                             nil))
                                      (child (car (graph-node-children
                                                   node))))
                                 (multiple-value-call
                                     #'bounding-rectangle-set-size new
                                     (bounding-rectangle-size 
                                      ;;-- Goddamn! This is not
                                      ;;-- drawing an arc its
                                      ;;-- drawing a node
                                      (with-output-to-output-record (stream)
                                        (call-arc-drawer stream arc-drawer
                                                         (graph-node-object parent)
                                                         (graph-node-object node)
                                                         parent 0 0
                                                         node depth 0
                                                         (list*
                                                          :draw-node t
                                                          :allow-other-keys t
                                                          arc-drawing-options)))))
                                 (setf (graph-node-generation new) (graph-node-generation node)
                                       (graph-node-children new) (graph-node-children node)
                                       (graph-node-parents new) (graph-node-parents node)
                                       (graph-node-parents child) (list new)
                                       (graph-node-object new) 
                                       (make-instance
                                        'grapher-fake-object
                                        :data `(:filler ,new ,(graph-node-object node)))
                                       (car c) new)))))
                         (map nil #'replace-filler-nodes children))))
                    (declare (dynamic-extent #'replace-filler-nodes))
                    (map nil #'replace-filler-nodes root-nodes)))
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
                         (let ((descr (assoc (graph-node-generation child-node)
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
                                           (floor (- (generation-depth descr)
                                                     (depth child-node)) 2))
                                        (generation-start-depth descr)))
                           (incf (generation-breadth-so-far descr) (breadth child-node)))))
                  (declare (dynamic-extent #'place-node))
                  (traverse #'place-node))))))))))

(defmethod layout-graph-edges ((graph directed-graph-output-record) stream
                               arc-drawer arc-drawing-options)
  (with-slots (root-nodes hash-table properties) graph
    (let ((orientation (getf properties :orientation)))
      (when root-nodes
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
                    (flet ((draw-edge (parent ph child &optional ch)
                             (declare (ignore ph ch))
                             (when parent

                               (when (filler-node-p parent)
                                 (return-from draw-edge))

                               (let ((intermediate-children nil)
                                     (i-coords nil)
                                     children
                                     (current-child child))
                                 (when (filler-node-p child)
                                   (loop
                                     (setq children (graph-node-children current-child))
                                     (assert (= (length children) 1))
                                     (push current-child intermediate-children)
                                     (setq current-child (car children))
                                     (unless (filler-node-p current-child)
                                       (return))))
                                 
                                 (setq intermediate-children (nreverse intermediate-children))
                                 (dolist (child intermediate-children)
                                   (multiple-value-bind (parent-x parent-y)
                                       (funcall parent-attach child)
                                     (multiple-value-bind (child-x child-y)
                                         (funcall child-attach child)
                                       (translate-coordinates xoff yoff parent-x parent-y child-x child-y)
                                       (push child-x i-coords)
                                       (push child-y i-coords)
                                       (push parent-x i-coords)
                                       (push parent-y i-coords))))
                                   
                                 (setq child current-child) 
                                   
                                 (setq i-coords (nreverse i-coords))
                                 
                                 (multiple-value-bind (parent-x parent-y)
                                     (funcall parent-attach parent)
                                   (multiple-value-bind (child-x child-y)
                                       (funcall child-attach child)
                                     (translate-coordinates xoff yoff
                                                            parent-x parent-y child-x child-y)
                                     (call-arc-drawer stream arc-drawer
                                                      (graph-node-object parent) 
                                                      (graph-node-object child)
                                                      parent parent-x parent-y
                                                      child child-x child-y
                                                      (list* :path i-coords
                                                             :allow-other-keys t
                                                             arc-drawing-options))))))))
                      (declare (dynamic-extent #'draw-edge)) 
                      (traverse-graph root-nodes #'inferior-mapper
                                      hash-table #'identity
                                      #'draw-edge #'draw-edge))))))))))))

;;-- This is a version that searches all of the old children.
;;-- Graph-output records can loose reorder children quite easily and
;;-- it seems worthwhile searching.

(defmethod find-child-output-record
    ((record basic-graph-output-record) use-old-children record-type
                                        &rest initargs &key unique-id id-test &allow-other-keys)
  (declare (dynamic-extent initargs))
  (flet ((do-match (candidate)
           ;; (class-name (class-of ...)) should just be type-of, but not in PCL.
           (and (eq record-type (class-name (class-of candidate)))
                (apply #'match-output-records candidate initargs))))
    (let ((elts-to-find (when use-old-children (output-record-old-children record))))
      (if use-old-children
          (let ((found-record
                 (if unique-id
                     (find-with-test unique-id elts-to-find
                                     #'output-record-unique-id id-test)
                   ;; UNIQUE-ID can be NIL when we are coming through
                   ;; INVOKE-WITH-NEW-OUTPUT-RECORD to create new records
                   (or (and (do-match (first elts-to-find))
                            (first elts-to-find))
                       (dolist (child (output-record-old-children record))
                           (when (do-match child)
                             (return child)))))))
            (when found-record
              (setf (output-record-old-children record)
                (delete found-record (output-record-old-children record)))
              found-record))
        (call-next-method)))))
