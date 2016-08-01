;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."


;;; R tree output records

;; R trees are about somewhat slower than coordinate sorted sets for the
;; highly optimized cases TTY-style output or:
;;   0 1 2
;;   3 4 5
;;   6 7
;;   8 9
;; 
;; Conversely, they are faster for the worst case:
;;   9 8 7
;;   6 5 4
;;   3 2
;;   1 0
;; 
;; Because of the nature of the optimizations, they are still about 5
;; times faster for a random ordering that includes a lot of overlapping
;; records.  There is basically no performance difference between
;; insertion orderings for R trees: random is the worst because there is
;; initial area wastage jumping around.  Naturally, the sorted sets are
;; identical no matter how they were produced.  Since R trees do not
;; give a strict ordering, and it is impractical to do an exhaustive
;; determination of best split, the results do vary depending on how
;; they got built.
;; 
;; The best case order for coordinate sorted sets is very common, of
;; course.  It corresponds to TTY style output, and even much program
;; generated graphics.
;; 
;; The storage overhead of R trees is greater than coordinate sorted sets
;; because of the intermediate nodes and their bounding box structures.
;; 
;; Searching is 2-3 times slower in R trees.  Because inferiors are not
;; sorted, and the tree is not order balanced, you have to do about
;; M log(M) n probes instead of just log(2) n.  This means M / log(2) M
;; times as many, which is about 4 times with the default settings.
;;
;; What all this means is that you should use R trees when there are lots
;; of overlapping output records and inserting performance is important.
;; R trees are also much more "accurate" than coordinate sorted sets, in
;; that they correctly maintain stacking (temporal) order.


;;; More region arithmetic

;; Assumes REGION and OTHER-REGION are in the same coordinate space
(defun bounding-rectangle-copy-edges (region other-region)
  (with-bounding-rectangle* (left top right bottom) other-region
    (bounding-rectangle-set-edges region left top right bottom)))

;; Assumes REGION and OTHER-REGION are in the same coordinate space
(defun bounding-rectangle-extend-edges (region other-region)
  (with-bounding-rectangle* (left top right bottom) region
    (with-bounding-rectangle* (oleft otop oright obottom) other-region
      (bounding-rectangle-set-edges 
        region
        (min left oleft) (min top otop)
        (max right oright) (max bottom obottom)))))

(defun bounding-rectangle-area (region)
  (multiple-value-bind (width height)
      (bounding-rectangle-size region)
    (* width height)))

;; Assumes REGION and OTHER-REGION are in the same coordinate space
(defun bounding-rectangle-area-extension (region other-region)
  (with-bounding-rectangle* (left top right bottom) region
    (with-bounding-rectangle* (oleft otop oright obottom) other-region
      (let ((area (* (- right left) (- bottom top))))
        (values (- (* (- (max right oright) (min left oleft))
                      (- (max bottom obottom) (min top otop)))
                   area)
                area)))))


;;; Now the R tree data structures

(defparameter *r-tree-minimum-fullness* 2)
(defparameter *r-tree-maximum-fullness* 20)


;; R tree nodes contain R tree leaf nodes, not output records
(defclass r-tree-node (standard-bounding-rectangle)
    ((inferiors :accessor r-tree-node-inferiors
                :initform (make-array *r-tree-maximum-fullness*
                                      :fill-pointer 0 :adjustable nil))
     (superior :accessor r-tree-node-superior 
               :initform nil :initarg :superior)))

(define-constructor make-r-tree-node r-tree-node (superior)
  :superior superior)


;; R tree leaf nodes contain output records
(defclass r-tree-leaf (r-tree-node) ())

(define-constructor make-r-tree-leaf r-tree-leaf (superior)
  :superior superior)


(defmethod r-tree-leaf-node-p ((node r-tree-node)) nil)

(defmethod r-tree-leaf-node-p ((node r-tree-leaf)) t)


;; Maps over the R tree, applying the function to all of the output
;; records in the tree that overlap the region
(defun map-over-r-tree-records-overlapping-region
       (function node record region
        &optional (x-offset (coordinate 0)) (y-offset (coordinate 0)) 
        &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (multiple-value-bind (left top right bottom)
      (if (or (null region) (eq region +everywhere+))
          (values nil nil nil nil)
          (bounding-rectangle* region))
    (when left
      (translate-coordinates (coordinate x-offset) (coordinate y-offset)
        left top right bottom)
      (multiple-value-bind (xoff yoff)
          (output-record-position record)
        (translate-coordinates (- xoff) (- yoff) left top right bottom)))
    (apply #'map-over-r-tree-records-overlapping-region-1
           function node left top right bottom continuation-args)))

(defmethod map-over-r-tree-records-overlapping-region-1
           (function (node r-tree-node) left top right bottom
            &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (let ((inferiors (r-tree-node-inferiors node)))
    (declare (type vector inferiors))
    (if (null left)
        (dovector (node inferiors)
          (apply #'map-over-r-tree-records-overlapping-region-1
                 function node left top right bottom continuation-args))
        (dovector (node inferiors)
          (with-bounding-rectangle* (nleft ntop nright nbottom) node
            (when (ltrb-overlaps-ltrb-p left top right bottom
                                        nleft ntop nright nbottom)
              (apply #'map-over-r-tree-records-overlapping-region-1
                     function node left top right bottom continuation-args)))))))

(defmethod map-over-r-tree-records-overlapping-region-1
           (function (node r-tree-leaf) left top right bottom
            &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (let ((inferiors (r-tree-node-inferiors node)))
    (declare (type vector inferiors))
    (if (null left)
        (dovector (record inferiors)
          (apply function record continuation-args))
        (dovector (record inferiors)
          (with-bounding-rectangle* (nleft ntop nright nbottom) record
            (when (ltrb-overlaps-ltrb-p left top right bottom
                                        nleft ntop nright nbottom)
              (apply function record continuation-args)))))))


;; Maps over the R tree, applying the function to all of the output
;; records in the tree that contain the position
(defun map-over-r-tree-records-containing-position
       (function node record x y 
        &optional (x-offset (coordinate 0)) (y-offset (coordinate 0)) 
        &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (translate-coordinates (coordinate x-offset) (coordinate y-offset) x y)
  (multiple-value-bind (xoff yoff)
      (output-record-position record)
    (translate-coordinates (- xoff) (- yoff) x y))
  (apply #'map-over-r-tree-records-containing-position-1
         function node x y continuation-args))

(defmethod map-over-r-tree-records-containing-position-1
           (function (node r-tree-node) x y 
            &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (let ((inferiors (r-tree-node-inferiors node)))
    (declare (type vector inferiors))
    (dovector (node inferiors :from-end t)
      (with-bounding-rectangle* (nleft ntop nright nbottom) node
        (when (ltrb-contains-position-p nleft ntop nright nbottom x y)
          (apply #'map-over-r-tree-records-containing-position-1
                 function node x y continuation-args))))))

(defmethod map-over-r-tree-records-containing-position-1
           (function (node r-tree-leaf) x y 
            &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (let ((inferiors (r-tree-node-inferiors node)))
    (declare (type vector inferiors))
    (dovector (record inferiors :from-end t)
      (with-bounding-rectangle* (nleft ntop nright nbottom) record
        (when (ltrb-contains-position-p nleft ntop nright nbottom x y)
          (apply function record continuation-args))))))


(defmethod copy-r-tree-node ((node r-tree-node))
  (make-r-tree-node (r-tree-node-superior node)))

(defmethod copy-r-tree-node ((node r-tree-leaf))
  (make-r-tree-leaf (r-tree-node-superior node)))


(defmethod r-tree-compute-bounding-rectangle ((node r-tree-node))
  (let ((inferiors (r-tree-node-inferiors node)))
    (declare (type vector inferiors))
    (with-bounding-rectangle* (mleft mtop mright mbottom) (aref inferiors 0)
      (dovector (node inferiors :start 1)
        (with-bounding-rectangle* (left top right bottom) node
          (minf mleft left)
          (minf mtop top)
          (maxf mright right)
          (maxf mbottom bottom)))
      (bounding-rectangle-set-edges node mleft mtop mright mbottom))))


;; Add a record to an R tree, and return the (possibly new) root
(defun r-tree-insert (root record)
  (declare (values new-root))
  (let ((leaf root))
    (loop 
      (when (r-tree-leaf-node-p leaf)
        (return))
      (setq leaf (r-tree-choose-leaf leaf record)))
    (r-tree-insert-1 leaf record)))

(defun r-tree-insert-1 (node record)
  (multiple-value-bind (leaf split-leaf)
      (r-tree-push-or-split node record)
    (multiple-value-bind (root split-root)
        (r-tree-adjust-tree leaf split-leaf)
      (if (null split-root)
          root
          (let* ((new-root (make-r-tree-node nil))
                 (inferiors (r-tree-node-inferiors new-root)))
            (declare (type vector inferiors))
            (setf (r-tree-node-superior root) new-root)
            (setf (r-tree-node-superior split-root) new-root)
            (setf (fill-pointer inferiors) 2)
            (setf (aref inferiors 0) root)
            (setf (aref inferiors 1) split-root)
            (r-tree-compute-bounding-rectangle new-root)
            #+R-tree-debugging
            (r-tree-node-check-bounding-rectangle new-root)
            new-root)))))

;; Pick the inferior node requiring the least extension to accomodate the
;; new record
(defmethod r-tree-choose-leaf ((node r-tree-node) record)
  (let ((inferiors (r-tree-node-inferiors node))
        (best-node nil)
        best-area best-diff)
    (declare (type vector inferiors))
    (dovector (node inferiors)
      (multiple-value-bind (diff area)
          (bounding-rectangle-area-extension node record)
        ;; If this leaf needs less extension, use it
        (when (cond ((null best-node) t)
                    ((< diff best-diff) t)
                    ((> diff best-diff) nil)
                    ((< area best-area) t))
          (setq best-node node
                best-area area
                best-diff diff))))
    best-node))


;; Add object into node, splitting into two if no more room.  Return node
;; and optionally new sibling.
(defmethod r-tree-push-or-split ((node r-tree-node) record)
  (let* ((inferiors (r-tree-node-inferiors node))
         (fp (fill-pointer inferiors)))
    (declare (type vector inferiors))
    (cond ((< fp (array-dimension inferiors 0))        ;*R-TREE-MAXIMUM-FULLNESS*
           (setf (aref inferiors fp) record)
           (setf (fill-pointer inferiors) (1+ fp))
           (if (zerop fp)
               ;; Can only happen when first inserting into the root leaf.
               (bounding-rectangle-copy-edges node record)
               (bounding-rectangle-extend-edges node record))
           #+R-tree-debugging
           (progn
             (unless (r-tree-leaf-node-p node)
               (assert (eql (r-tree-node-superior record) node)))
             (r-tree-node-check-bounding-rectangle node))
           node)
          (t
           ;; Split the node.  Splitter will return node and new sibling,
           ;; with the correct bounding boxes.
           (r-tree-split-node node record)))))

;; Propagate new siblings and box additions up the tree
(defun r-tree-adjust-tree (node split-node)
  (loop
    (let ((superior (r-tree-node-superior node)))
      (when (null superior)
        (return (values node split-node)))
      ;; We might have gotten bigger, adjust superior
      (bounding-rectangle-extend-edges superior node)
      (if (null split-node)
          (setq node superior)
          (multiple-value-setq (node split-node)
            (r-tree-push-or-split superior split-node)))
      #+R-tree-debugging
      (progn
        (r-tree-node-check-bounding-rectangle node)
        (when split-node
          (r-tree-node-check-bounding-rectangle split-node))))))

;; Linear cost version node splitting.  There is a quadratic cost algorithm
;; that yields slightly better results, but the performance does not turn out
;; to be enough of an improvement to warrant the complexity.
(defun r-tree-split-node (node1 additional-object)
  (let ((leaf-p (r-tree-leaf-node-p node1))
        (node2 (copy-r-tree-node node1)))
    (let ((objects1 (r-tree-node-inferiors node1))
          (objects2 (r-tree-node-inferiors node2))
          fp1 fp2)
      (declare (type vector objects1 objects2))
      (setq fp1 (fill-pointer objects1))
      (multiple-value-bind (object1 object2)
          ;; Pick the pair of objects whose normalized separation is the greatest
          (let (min-left min-top max-right max-bottom
                max-left max-left-object max-top max-top-object
                min-right min-right-object min-bottom min-bottom-object)
            (multiple-value-setq (min-left min-top max-right max-bottom)
              (bounding-rectangle* additional-object))
            (setq max-left min-left
                  max-left-object additional-object
                  max-top min-top
                  max-top-object additional-object
                  min-right max-right
                  min-right-object additional-object
                  min-bottom max-bottom
                  min-bottom-object additional-object)
            (dotimes (index fp1)
              (let ((object (aref objects1 index)))
                (with-bounding-rectangle* (left top right bottom) object
                  (minf min-left left) 
                  (minf min-top top)
                  (maxf max-right right)
                  (maxf max-bottom bottom)
                  (when (> left max-left)
                    (setq max-left left
                          max-left-object object))
                  (when (> top max-top)
                    (setq max-top top
                          max-top-object object))
                  (when (< right min-right)
                    (setq min-right right
                          min-right-object object))
                  (when (< bottom min-bottom)
                    (setq min-bottom bottom
                          min-bottom-object object)))))
            (if (> (round (- max-left min-right) (- max-right min-left))
                   (round (- max-top min-bottom) (- max-bottom min-top)))
                (values max-left-object min-right-object)
                (values max-top-object min-bottom-object)))
        ;; This can happen if all objects enclose a small one.
        ;; I guess just pick anything else.
        (when (eq object1 object2)
          (setq object2 (if (eq object2 additional-object)
                            (aref objects1 0)
                            additional-object)))
        (let ((index 0)
              (nobjects (1- fp1)))
          (with-stack-array (objects nobjects)
            (declare (type vector objects))
            ;; Collect all remaining objects
            (macrolet ((maybe-add (object &environment env)
                         (once-only (object &environment env)
                           `(unless (or (eql ,object object1)
                                        (eql ,object object2))
                              (setf (aref objects index) ,object)
                              (incf index)))))
              (maybe-add additional-object)
              (dotimes (jndex fp1)
                (maybe-add (aref objects1 jndex))))
            #+R-tree-debugging
            (assert (= index nobjects))
            (macrolet
              ((add-object (object group &optional init-p &environment env)
                 (once-only (object &environment env)
                   (multiple-value-bind (set fp superior)
                       (ecase group
                         (1 (values 'objects1 'fp1 'node1))
                         (2 (values 'objects2 'fp2 'node2)))
                     `(progn
                        ,(ecase group
                           (1 #+R-tree-debugging
                              `(unless leaf-p
                                 (assert (eq (r-tree-node-superior ,object) ,superior))))
                           (2 `(unless leaf-p
                                 (setf (r-tree-node-superior ,object) ,superior))))
                        ,(when init-p
                           `(setf ,fp 0))
                        (setf (aref ,set ,fp) ,object)
                        (incf ,fp)
                        ,(if init-p
                             `(bounding-rectangle-copy-edges ,superior ,object)
                             `(bounding-rectangle-extend-edges ,superior ,object)))))))
              ;; Initialize two groups
              (add-object object1 1 t)
              (add-object object2 2 t)
              (dotimes (index nobjects)
                (let ((to-go (- nobjects index)))
                  (when (< to-go *r-tree-minimum-fullness*)
                    ;; If we need all of them to reach the threshold in one
                    ;; node, do so at once.
                    (when (<= (+ fp1 to-go) *r-tree-minimum-fullness*)
                      (do ((j index (1+ j)))
                          ((>= j nobjects))
                        (add-object (aref objects j) 1))
                      (return))
                    (when (<= (+ fp2 to-go) *r-tree-minimum-fullness*)
                      (do ((j index (1+ j)))
                          ((>= j nobjects))
                        (add-object (aref objects j) 2))
                      (return))))
                (let ((object (aref objects index)))
                  ;; Add to group needing least extension, then smallest in
                  ;; area, then smallest in fullness
                  (if (multiple-value-bind (darea1 area1)
                          (bounding-rectangle-area-extension node1 object)
                        (multiple-value-bind (darea2 area2)
                            (bounding-rectangle-area-extension node2 object)
                          (cond ((< darea1 darea2) t)
                                ((> darea1 darea2) nil)
                                ((< area1 area2) t)
                                ((> area1 area2) nil)
                                ((< fp1 fp2) t))))
                      (add-object object 1)
                      (add-object object 2)))))))
        (setf (fill-pointer objects1) fp1
              (fill-pointer objects2) fp2)))
    #+R-tree-debugging
    (progn
      (r-tree-node-check-bounding-rectangle node1)
      (r-tree-node-check-bounding-rectangle node2))
    (values node1 node2)))


;; Remove an object from the R tree and trim it appropriately
(defun r-tree-delete (root record)
  (declare (values new-root found-p))
  (with-stack-array (orphans 20 :fill-pointer 0)
    (multiple-value-bind (found-p delete-p)
        (r-tree-delete-1 root record orphans)
      (when delete-p
        (setq root (make-r-tree-leaf (r-tree-node-superior root))))
      (r-tree-delete-repatriate-orphans root orphans)
      (values root found-p))))

(defun r-tree-delete-repatriate-orphans (root orphans)
  (labels ((repatriate (from-node)
             (let ((inferiors (r-tree-node-inferiors from-node)))
               (declare (type vector inferiors))
               (if (r-tree-leaf-node-p from-node)
                   ;; Deleted from leaf, easy to put back
                   (dovector (object inferiors)
                     (setq root (r-tree-insert root object)))
                   (let* ((orphan-depth (distance-to-leaf from-node))
                          (delta (- (distance-to-leaf root) orphan-depth)))
                     (if (minusp delta)
                         ;; These came from further up the tree than exists now,
                         ;; must step down
                         (map nil #'repatriate inferiors)
                         ;; Find a node down the tree for insertion
                         (loop
			   (when (= (length inferiors) 0) (return)) ; KRA 16OCT96:
                           (let ((orphan-node (vector-pop inferiors)))
                             (when (null orphan-node) (return))
                             (let ((sub-node root))
                               (repeat delta
                                 (setq sub-node (r-tree-choose-leaf sub-node orphan-node)))
                               (r-tree-insert-1 sub-node orphan-node)))
                           (setq delta (- (distance-to-leaf root) orphan-depth))))))))
           (distance-to-leaf (node)
             (loop until (r-tree-leaf-node-p node)
                   do (setq node (aref (r-tree-node-inferiors node) 0))
                   count t)))
    (declare (dynamic-extent #'repatriate #'distance-to-leaf))
    (map nil #'repatriate orphans))
  root)

(defmethod r-tree-delete-1 ((root r-tree-node) record orphans)
  (let ((inferiors (r-tree-node-inferiors root))
        (index 0))
    (declare (type vector inferiors))
    (dovector (node inferiors)
      (when (region-intersects-region-p node record)
        (multiple-value-bind (found-p delete-p)
            (r-tree-delete-1 node record orphans)
          (when found-p
            (when delete-p
              (shift-buffer-portion inferiors (1+ index) index)
              (when (setq delete-p (< (fill-pointer inferiors) *r-tree-minimum-fullness*))
                (vector-push-extend root orphans)))
            (unless delete-p
              (r-tree-compute-bounding-rectangle root))
            (return (values found-p delete-p)))))
      (incf index))))

(defmethod r-tree-delete-1 ((root r-tree-leaf) record orphans)
  (let* ((inferiors (r-tree-node-inferiors root))
         (index (position record (r-tree-node-inferiors root))))
    (declare (type vector inferiors))
    (when index
      (shift-buffer-portion inferiors (1+ index) index)
      (cond ((< (fill-pointer inferiors) *r-tree-minimum-fullness*)
             (vector-push-extend root orphans)
             (values t t))
            (t
             (r-tree-compute-bounding-rectangle root)
             (values t nil))))))


;; Remove all objects which are touched by given region.
(defun r-tree-delete-region (root left top right bottom)
  (with-stack-array (orphans 20 :fill-pointer 0)
    (multiple-value-bind (changed-p delete-p)
        (r-tree-delete-region-1 root left top right bottom orphans)
      (when delete-p
        (setq root (make-r-tree-leaf (r-tree-node-superior root))))
      (r-tree-delete-repatriate-orphans root orphans)
      (values root changed-p))))

(defmethod r-tree-delete-region-1 ((root r-tree-node) left top right bottom orphans)
  (let ((wp 0)
        (any-changed-p nil)
        (inferiors (r-tree-node-inferiors root)))
    (declare (type vector inferiors))
    (dotimes (rp (fill-pointer inferiors))
      (let ((node (aref inferiors rp)))
        (with-bounding-rectangle* (bleft btop bright bbottom) node
          (multiple-value-bind (changed-p delete-p)
              (when (ltrb-overlaps-ltrb-p left top right bottom
                                          bleft btop bright bbottom)
                (if (ltrb-contains-ltrb-p left top right bottom
                                          bleft btop bright bbottom)
                    ;; Everything inside here must go, no orphans
                    (values t t)
                    (r-tree-delete-region-1 node left top right bottom orphans)))
            (when changed-p
              (setq any-changed-p t))
            (unless delete-p
              (unless (= rp wp)
                (setf (aref inferiors wp) node))
              (incf wp))))))
    (when any-changed-p
      (setf (fill-pointer inferiors) wp)
      (cond ((< wp *r-tree-minimum-fullness*)
             (when (plusp wp)
               (vector-push-extend root orphans))
             (values t t))
            (t
             (r-tree-compute-bounding-rectangle root)
             (values t nil))))))

(defmethod r-tree-delete-region-1 ((root r-tree-leaf) left top right bottom orphans)
  (let ((wp 0)
        (any-changed-p nil)
        (inferiors (r-tree-node-inferiors root)))
    (declare (type vector inferiors))
    (dotimes (rp (fill-pointer inferiors))
      (let ((object (aref inferiors rp)))
        (with-bounding-rectangle* (bleft btop bright bbottom) object
          (cond ((ltrb-overlaps-ltrb-p left top right bottom
                                       bleft btop bright bbottom)
                 (setq any-changed-p t))
                (t
                 (unless (= rp wp)
                   (setf (aref inferiors wp) object))
                 (incf wp))))))
    (when any-changed-p
      (setf (fill-pointer inferiors) wp)
      (cond ((< wp *r-tree-minimum-fullness*)
             (when (plusp wp)
               (vector-push-extend root orphans))
             (values t t))
            (t
             (r-tree-compute-bounding-rectangle root)
             (values t nil))))))


;;; The R tree output record classes

(defclass r-tree-output-record
          (output-record-mixin 
           output-record-element-mixin
           output-record)
    ;; Start out with a leaf that will contain output records
    ((root :initform (make-r-tree-leaf nil))))

(defmethod initialize-instance :after ((record r-tree-output-record) &key size)
  ;; Accept :SIZE as an initarg
  (declare (ignore size)))

(define-output-record-constructor r-tree-output-record
                                  (&key x-position y-position (size 5))
  :x-position x-position :y-position y-position :size size)

(defmethod output-record-children ((record r-tree-output-record))
  (with-slots (root) record
    (let ((children nil))
      (map-over-r-tree-records-overlapping-region
        #'(lambda (record)
            (push record children))
        root record nil)
      (nreverse children))))

(defmethod output-record-element ((record r-tree-output-record) index)
  (with-slots (root) record
    (let ((count 0))
      (map-over-r-tree-records-overlapping-region
        #'(lambda (record)
            (when (= count index)
              (return-from output-record-element record))
            (incf count))
        root record nil))))

(defmethod output-record-count ((record r-tree-output-record) &key fastp)
  (let ((root (slot-value record 'root)))
    (if fastp
        ;; This will return some number between 0 and *R-TREE-MAXIMUM-FULLNESS*,
        ;; which is good enough for the fast case where only 0, 1, and >1 matter.
        ;; For a few hundred records, this can be a hundred times faster...
        (typecase root
          (r-tree-node *r-tree-maximum-fullness*)
          (t (fill-pointer (r-tree-node-inferiors root))))
        (let ((count 0))
          (map-over-r-tree-records-overlapping-region
            #'(lambda (record)
                (declare (ignore record))
                (incf count))
            root record nil)
          count))))

(defmethod clear-output-record ((record r-tree-output-record))
  (with-slots (root) record 
    (setq root (make-r-tree-leaf nil))))

(defmethod add-output-record (child (record r-tree-output-record))
  (with-slots (root) record
    (let ((new-root (r-tree-insert root child)))
      (setf root new-root))))

(defmethod delete-output-record (child (record r-tree-output-record)
                                 &optional (errorp t))
  (with-slots (root) record
    (multiple-value-bind (new-root foundp) 
        (r-tree-delete root child)
      (cond (foundp 
             (setf root new-root))
            (errorp
             (error "The output record ~S was not found in ~S"
                    child record))))))

(defmethod map-over-output-records-overlapping-region
           (function (record r-tree-output-record) region
            &optional (x-offset (coordinate 0)) (y-offset (coordinate 0))
            &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (with-slots (root) record
    (apply #'map-over-r-tree-records-overlapping-region
           function root record region x-offset y-offset continuation-args)))

(defmethod map-over-output-records-containing-position
           (function (record r-tree-output-record) x y
            &optional (x-offset (coordinate 0)) (y-offset (coordinate 0))
            &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (with-slots (root) record
    (apply #'map-over-r-tree-records-containing-position
           function root record x y x-offset y-offset continuation-args)))


(defclass r-tree-output-history
          (stream-output-history-mixin r-tree-output-record)
    ())


;;; Debugging code

#+R-tree-debugging
(progn

(defmethod r-tree-node-check-bounding-rectangle ((node r-tree-node))
  (let ((inferiors (r-tree-node-inferiors node)))
    (declare (type vector inferiors))
    (with-bounding-rectangle* (mleft mtop mright mbottom)
        (aref inferiors 0)
      (dovector (object inferiors :start 1)
        (with-bounding-rectangle* (left top right bottom) object
          (minf mleft left) 
          (minf mtop top)
          (maxf mright right)
          (maxf mbottom bottom)))
      (with-bounding-rectangle* (left top right bottom) node
        (assert (and (= left mleft) (= top mtop) (= right mright) (= bottom mbottom))
                nil "Bounding box of ~S malformed" node)))))

(defun check-r-tree (root)
  (labels ((check-1 (node superior)
             (assert (eq (r-tree-node-superior node) superior) nil
                     "Superior of ~S should be ~S, not ~S" node superior
                     (r-tree-node-superior node))
             (r-tree-node-check-bounding-rectangle node)
             (unless (r-tree-leaf-node-p node)
               (let ((inferiors (r-tree-node-inferiors node)))
                 (declare (type vector inferiors))
                 (dovector (inf inferiors)
                   (check-1 inf node))))))
    (declare (dynamic-extent #'check-1))
    (check-1 root nil)))

(defun r-tree-equal (root1 root2)
  (and (region-equal root1 root2)
       (let ((inf1 (r-tree-node-inferiors root1))
             (inf2 (r-tree-node-inferiors root2)))
         (declare (type vector inf1 inf2))
         (if (r-tree-leaf-node-p root1)
             (and (r-tree-leaf-node-p root2) 
                  (equalp inf1 inf2))
             (and (= (length inf1) (length inf2))
                  (loop for i below (length inf1)
                        always (r-tree-equal (aref inf1 i) (aref inf2 i))))))))

(defun show-r-tree (root &optional (stream *standard-output*))
  (fresh-line stream)
  (labels ((show-1 (node depth)
             (with-bounding-rectangle* (left top right bottom) node
               (format stream "~@vTx ~D:~D y ~D:~D~%" (* depth 2) left right top bottom))
             (when (typep node 'r-tree-node)
               (let ((inferiors (r-tree-node-inferiors node)))
                 (declare (type vector inferiors))
                 (dovector (inf inferiors)
                   (show-1 inf (1+ depth)))))))
    (declare (dynamic-extent #'show-1))
    (show-1 root 0)))

(defun draw-r-tree (root &optional (stream *standard-output*) use-color)
  (let ((max-depth 0))
    (labels ((add-max (node depth)
               (maxf max-depth depth)
               (unless (r-tree-leaf-node-p node)
                 (let ((inferiors (r-tree-node-inferiors node)))
                   (declare (type vector inferiors))
                   (dovector (inf inferiors)
                     (add-max inf (1+ depth)))))))
      (declare (dynamic-extent #'add-max))
      (add-max root 2))
    (with-room-for-graphics (stream :first-quadrant nil)
      (dotimes-noting-progress (at-depth max-depth "Tree depth")
        (labels ((show-1 (node depth i n)
                   (if (= depth at-depth)
                       (with-bounding-rectangle* (left top right bottom) node
                         (when (and use-color (> n 1))
                           (draw-rectangle* stream
                                            left top right bottom
                                            :ink (make-ihs-color 
                                                   1 (/ i n) (if (oddp i) 1/2 1))))
                         (draw-rectangle* stream
                                          left top right bottom
                                          :filled nil))
                       (let* ((inferiors (r-tree-node-inferiors node))
                              (length (length inferiors)))
                         (declare (type vector inferiors))
                         (dotimes (i length)
                           (show-1 (aref inferiors i) (1+ depth) i length))))))
          (declare (dynamic-extent #'show-1))
          (show-1 root 0 0 1))))))

)        ;#+R-tree-debugging
