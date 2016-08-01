;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :silica)

;;;"Copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
;;; Portions copyright (c) 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1989, 1990 Xerox Corp.  All rights reserved."


;;;
;;; Layout Protocol
;;;

(defgeneric compose-space (pane &key width height)
  (:documentation "<Pane> should calculate how much space it wants."))

(defgeneric allocate-space (pane width height)
  (:documentation "<Pane> should allocate given space amongst itself and children"))

(defgeneric note-space-requirements-changed (composite child)
  (:documentation "Tells the composite that the child's shape has changed"))

;;;
;;; Default Methods for Layout Protocol
;;;

;;--- This seems dubious...
(defmethod allocate-space ((pane basic-sheet) width height)
  (declare (ignore width height)))

(defmethod note-space-requirements-changed ((composite composite-pane) child)
  (declare (ignore child))
  (note-space-requirements-changed (sheet-parent composite)
                                   composite))

(defmethod note-space-requirements-changed ((composite top-level-sheet) child)
  (declare (ignore child))
  (layout-frame (pane-frame composite)))

(defmethod note-space-requirements-changed ((composite null) child)
  ;; ??? Why is this method necessary -- RR
  ;; I'm sure it is, I just don't like the looks of it, which probably means
  ;; that there's a deeper problem somewhere.
  (declare (ignore child)))


;;;
;;; Layout Mixin
;;;

(defclass layout-mixin () ())

(defmethod change-space-requirements ((pane layout-mixin) &key &allow-other-keys)
  nil)

(defmethod note-sheet-region-changed :after ((pane layout-mixin) &key port-did-it)
  (note-layout-mixin-region-changed pane :port port-did-it))

(defmethod note-layout-mixin-region-changed ((pane layout-mixin) &key port)
  (declare (ignore port))
  (multiple-value-bind (width height) (bounding-rectangle-size pane)
    (allocate-space pane width height)))


(defmacro changing-space-requirements ((&rest options &key resize-frame layout)
                                       &body body)
  (declare (ignore resize-frame layout))
  `(flet ((changing-space-requirements-body () ,@body))
     (declare (dynamic-extent #'changing-space-requirements-body))
     (invoke-with-changing-space-requirements
       #'changing-space-requirements-body ,@options)))

(defmethod change-space-requirements-to ((pane layout-mixin) space-requirement)
  (multiple-value-bind (width min-width max-width
                        height min-height max-height)
      (space-requirement-components space-requirement)
    (change-space-requirements pane
      :width width :min-width min-width :max-width max-width
      :height height :min-height min-height :max-height max-height)))


;;--- Do we need this?
(defmethod change-space-requirements-to-default ((pane basic-pane))
  nil)

(defmethod change-space-requirements-to-default ((pane layout-mixin))
  nil)

(defvar *inside-changing-space-requirements* nil)

(defun invoke-with-changing-space-requirements (continuation &key resize-frame (layout t))
  (declare (ignore resize-frame))   ;-- Why?
  (let ((old-inside-changing-space-requirements
          *inside-changing-space-requirements*)
        (*inside-changing-space-requirements*
          (cons nil *inside-changing-space-requirements*)))
    (multiple-value-prog1
      (funcall continuation)
      (when (and layout (not old-inside-changing-space-requirements))
        ;; Layout that frames that need to be laid out and
        ;; re-layout the minimal subtrees that need to be laid out
        (let* ((frames (remove-duplicates
                         (remove-if-not
                           #'(lambda (x) (application-frame-p x))
                           *inside-changing-space-requirements*)))
               (panes (remove-if #'(lambda (x)
                                     (or (not (panep x))
                                         (member (pane-frame x) frames)))
                                 *inside-changing-space-requirements*)))
          (mapc #'layout-frame frames)
          (dolist (pane panes)
            (unless (some #'(lambda (p)
                              (and (not (eq p pane))
                                   (sheet-ancestor-p pane p)))
                          panes)
              ;; do this because the change has occurred somewhere in
              ;; the tree
              (clear-space-requirement-caches-in-tree pane)
              (multiple-value-call #'allocate-space
                pane (bounding-rectangle-size pane)))))))))

(defmethod change-space-requirements :around ((pane layout-mixin)
                                              &key resize-frame &allow-other-keys)
  (call-next-method)
  (let ((frame (pane-frame pane)))
    (if *inside-changing-space-requirements*
        (push (if (and frame resize-frame) frame (sheet-parent pane))
              (cdr *inside-changing-space-requirements*))
        (if (and frame resize-frame)
            (layout-frame frame)
            (note-space-requirements-changed (sheet-parent pane) pane)))))


(defun space-requirement-combine (function sr1 sr2)
  (multiple-value-bind (width1 min-width1 max-width1 height1 min-height1 max-height1)
      (space-requirement-components sr1)
    (multiple-value-bind (width2 min-width2 max-width2 height2 min-height2 max-height2)
        (space-requirement-components sr2)
      (make-space-requirement
        :width (funcall function width1 width2)
        :min-width (funcall function min-width1 min-width2)
        :max-width (funcall function max-width1 max-width2)
        :height (funcall function height1 height2)
        :min-height (funcall function min-height1 min-height2)
        :max-height (funcall function max-height1 max-height2)))))

;; Add two space requirements
(defun-inline space-requirement+ (sr1 sr2)
  (space-requirement-combine #'+ sr1 sr2))

;; The "spread" version of the above...
(defun space-requirement+* (sr &key (width 0) (max-width width) (min-width width)
                                    (height 0) (max-height height) (min-height height))
  (multiple-value-bind (srwidth  srmin-width  srmax-width
                        srheight srmin-height srmax-height)
      (space-requirement-components sr)
    (make-space-requirement
      :width  (+ srwidth width)
      :min-width (+ srmin-width min-width)
      :max-width (+ srmax-width max-width)
      :height (+ srheight height)
      :min-height (+ srmin-height min-height)
      :max-height (+ srmax-height max-height))))

(define-compiler-macro space-requirement+ (&whole form sr1 sr2)
  (cond ((and (listp sr1)
              (eq (first sr1) 'make-space-requirement))
         (destructuring-bind (f &key (width 0) (max-width width) (min-width width)
                                     (height 0) (max-height height) (min-height height))
             sr1
           (declare (ignore f))
           `(space-requirement+* ,sr2
              :width ,width :max-width ,max-width :min-width ,min-width
              :height ,height :max-height ,max-height :min-height ,min-height)))
        ((and (listp sr2)
              (eq (first sr2) 'make-space-requirement))
         (destructuring-bind (f &key (width 0) (max-width width) (min-width width)
                                     (height 0) (max-height height) (min-height height))
             sr2
           (declare (ignore f))
           `(space-requirement+* ,sr1
              :width ,width :max-width ,max-width :min-width ,min-width
              :height ,height :max-height ,max-height :min-height ,min-height)))
        (t form)))


;;;
;;; Space Req Mixin
;;;

(defclass basic-space-requirement-mixin (layout-mixin)
    ((initial-space-requirement :reader pane-initial-space-requirements)
     (space-requirement :reader pane-space-requirements)))

(defmethod initialize-instance :after ((pane basic-space-requirement-mixin)
                                       &rest args
                                       &key width max-width min-width
                                            height max-height min-height)
  (declare (dynamic-extent args))
  (declare (ignore width min-width max-width
                   height min-height max-height))
  (with-slots (space-requirement initial-space-requirement) pane
    (multiple-value-bind (width min-width max-width
                          height min-height max-height)
        (apply #'default-space-requirements pane :allow-other-keys t args)
      (let ((sr (make-space-requirement
                  :width width :min-width min-width :max-width max-width
                  :height height :min-height min-height :max-height max-height)))
        ;; Space requirements are immutable...
        (setf space-requirement sr
              initial-space-requirement sr)))))

(defmethod change-space-requirements-to-default ((pane basic-space-requirement-mixin))
  (when (pane-initial-space-requirements pane)
    (change-space-requirements-to pane (pane-initial-space-requirements pane))))

(defmethod default-space-requirements ((pane basic-space-requirement-mixin)
                                       &key (width 0)
                                            (min-width width)
                                            (max-width width)
                                            (height 0)
                                            (min-height height)
                                            (max-height height))
  (values width  min-width  max-width
          height min-height max-height))

(defmethod change-space-requirements ((pane basic-space-requirement-mixin)
                                      &key width min-width max-width
                                           height min-height max-height
                                           resize-frame)
  (declare (ignore resize-frame))
  (with-slots (space-requirement) pane
    (multiple-value-bind (srwidth  srmin-width  srmax-width
                          srheight srmin-height srmax-height)
        (space-requirement-components space-requirement)
      (setq space-requirement (make-space-requirement
                                :width (or width srwidth)
                                :min-width (or min-width srmin-width)
                                :max-width (or max-width srmax-width)
                                :height (or height srheight)
                                :min-height (or min-height srmin-height)
                                :max-height (or max-height srmax-height))))))


;;;
(defclass space-requirement-mixin (basic-space-requirement-mixin) ())


(defmethod compose-space ((pane space-requirement-mixin) &key width height)
  (declare (ignore width height))
  (or (slot-value pane 'space-requirement)
      (slot-value pane 'initial-space-requirement)))


;;;
;;; Client Overridability
;;;

(defclass client-overridability-mixin (basic-space-requirement-mixin)
    ())

(defmethod default-space-requirements ((pane client-overridability-mixin)
                                       &key width min-width max-width height min-height max-height)
  (values width  min-width  max-width
          height min-height max-height))

(defmethod normalize-space-requirement ((pane client-overridability-mixin) sr)
  sr)

(defmethod compose-space :around ((pane client-overridability-mixin) &key width height)
  (declare (ignore width height))
  (with-slots (space-requirement) pane
    (multiple-value-bind (width1 min-width1 max-width1 height1 min-height1 max-height1) 
        (space-requirement-components (normalize-space-requirement pane space-requirement))
      ;; If width1 has been explicitly specified, ensure that max-width1 has also.
      ;; Otherwise, it will always loose out to the max-width2.  spr20225 7/99.
      (when width1
	(setq max-width1 (or max-width1 width1))
        (setq min-width1 (or min-width1 width1)))
      (multiple-value-bind (width2 min-width2 max-width2 height2 min-height2 max-height2)
          (space-requirement-components (call-next-method))
        (flet ((mmin (x y z)
                 (or x (and y z (min y z)) y z))
               (mmax (x y z)
                 (or x (and y z (max y z)) y z)))
          (let ((height (or height1 height2))
                (width (or width1 width2)))
	    ;; Ensure space requirements remain normalized.  JPM spr18629 11/98.
	    (when width
	      (when (and min-width1 (< width min-width1)) (setq width min-width1))
	      (when (and max-width1 (> width max-width1)) (setq width max-width1)))
	    (when height
	      (when (and min-height1 (< height min-height1)) (setq height min-height1))
	      (when (and max-height1 (> height max-height1)) (setq height max-height1)))
	    (make-space-requirement
	     :width width
	     :min-width (or (mmin min-width1 width1 min-width2) width)
	     :max-width (or (mmax max-width1 width1 max-width2) width)
	     :height height
	     :min-height (or (mmin min-height1 height1 min-height2) height)
	     :max-height (or (mmax max-height1 height1 max-height2) height))))))))

;; A more correct version. This was implemented to make :max-height
;; override the :visible-items in list-panes. This is "more correct"
;; than the above definition in that it ensures that as long as the
;; original space requirement is well formed (ie min<=desired<=max)
;; then the result space requirement is well formed. Unfortunately
;; this breaks other code which relies in certain circumstances that
;; desired>max. In other words the whole thing is a complete mess and
;; I give up (cim 8/31/95)

#+ignore
(defmethod compose-space :around ((pane client-overridability-mixin) &key width height)
  (declare (ignore width height))
  (with-slots (space-requirement) pane
    (multiple-value-bind (width1 min-width1 max-width1 height1 min-height1 max-height1)
        (space-requirement-components (normalize-space-requirement pane space-requirement))
      (multiple-value-bind (width2 min-width2 max-width2 height2 min-height2 max-height2)
          (space-requirement-components (call-next-method))

        (macrolet ((nmin (arg &rest args)
                     (if args
                         (with-gensyms (a b)
                           `(let ((,a ,arg)
                                  (,b (nmin ,@args)))
                              (if ,a (if ,b (min ,a ,b) ,a) ,b)))
                       arg))
                   (nmax (arg &rest args)
                     (if args
                         (with-gensyms (a b)
                           `(let ((,a ,arg)
                                  (,b (nmax ,@args)))
                              (if ,a (if ,b (max ,a ,b) ,a) ,b)))
                       arg)))
          (let* ((min-width (nmin (nmax min-width1 min-width2)
                                  width1
                                  max-width1))
                 (max-width (nmax (nmin max-width1 max-width2)
                                  width1
                                  min-width1))
                 (width (or width1
                            (and width2
                                 (nmin max-width
                                       (nmax min-width width2)))))
                 (min-height (nmin (nmax min-height1 min-height2)
                                   height1
                                   max-height1))
                 (max-height (nmax (nmin max-height1 max-height2)
                                   height1
                                   min-height1))
                 (height (or height1
                             (and height2
                                  (nmin max-height
                                        (nmax min-height height2))))))
            (make-space-requirement
             :width width :min-width min-width :max-width max-width
             :height height :min-height min-height :max-height max-height)))))))

;;; Space Req Cache
;;;

(defclass space-requirement-cache-mixin (layout-mixin)
    ((space-requirement-cache :initform nil)))

(defmethod compose-space :around ((pane space-requirement-cache-mixin) &key width height)
  (declare (ignore width height))
  (with-slots (space-requirement-cache) pane
    (or space-requirement-cache
        (setf space-requirement-cache (call-next-method)))))

(defmethod clear-space-requirement-cache ((pane layout-mixin))
  nil)

(defmethod clear-space-requirement-cache ((pane t))
  nil)

(defmethod clear-space-requirement-cache ((pane space-requirement-cache-mixin))
  (with-slots (space-requirement-cache) pane
    (setf space-requirement-cache nil)))

(defmethod change-space-requirements :before ((pane space-requirement-cache-mixin) &key)
  (clear-space-requirement-cache pane))

(defmethod note-space-requirements-changed :before
           (composite (pane space-requirement-cache-mixin))
  (declare (ignore composite))
  (clear-space-requirement-cache pane))

(defun clear-space-requirement-caches-in-tree (sheet)
  (map-over-sheets #'(lambda (sheet)
                       (clear-space-requirement-cache sheet))
                   sheet))

(defun clear-space-requirement-caching-in-ancestors (menu)
  (do ((parent (sheet-parent menu) (sheet-parent parent)))
      ((null parent))
    (clear-space-requirement-cache parent)))

;;;
;;; Wrapping Space Mixin
;;;

(defclass wrapping-space-mixin (layout-mixin)
    ())

(defmethod allocate-space ((pane wrapping-space-mixin) width height)
  (resize-sheet (sheet-child pane) width height))

(defmethod compose-space ((pane wrapping-space-mixin) &key width height)
  (let ((child (sheet-child pane)))
    (compose-space child :width width :height height)))


;; This new supa dupa version saves up all the fills to the end and
;; then divides up any leftover space equally amongst them and it
;; seems to work!

(defun allocate-space-to-items (given wanted items min-size
                                desired-size max-size item-size)
  (macrolet ((desired-size (x) `(funcall desired-size ,x))
             (min-size (x) `(funcall min-size ,x))
             (max-size (x) `(funcall max-size ,x))
             (item-size (x) `(funcall item-size ,x)))
    (let ((stretch-p (>= given (desired-size wanted)))
          (allocated 0)
          extra give used sizes)
      (if stretch-p
          (setq give (- (max-size wanted) (desired-size wanted))
                extra (min (- given (desired-size wanted)) give))
          (setq give (- (desired-size wanted) (min-size wanted))
                extra (min (- (desired-size wanted) given) give)))
      (dolist (item items)
        (let (alloc)
          (typecase item
            ((member :fill)
             (setq alloc :fill))
            (t
              (let ((item-size (item-size item)))
                (cond ((eq item-size :fill)
                       (setq alloc :fill))
                      ((numberp item-size)
                       (setq alloc (if (> item-size 1.0)
                                       item-size
                                     (fix-coordinate (* item-size given)))))
                      (t
                       (setq alloc (desired-size item-size))
                       (when (> give 0)
                         (if stretch-p
                             (progn
                               (setq used (/ (* (- (max-size item-size)
                                                   (desired-size item-size))
                                                extra)
                                             give))
                               (incf alloc used)
                               (fix-coordinates alloc)
                               (decf give (- (max-size item-size)
                                             (desired-size item-size))))
                           (progn
                             (setq used (/ (* (- (desired-size item-size)
                                                 (min-size item-size))
                                              extra)
                                           give))
                             (decf alloc used)
                             (fix-coordinates alloc)
                             (decf give (- (desired-size item-size)
                                           (min-size item-size)))))
                         (decf extra used)))))))
          (when (numberp alloc)
            (incf allocated alloc))
          (push alloc sizes)))
      (let ((fills (count :fill sizes))
            (leftover (- given allocated)))
        (when (> fills 0)
          (let ((x (if (> leftover 0)
                       (fix-coordinate (/ leftover fills))
                       0)))
            (setf sizes (nsubstitute x :fill sizes)))))
      (nreverse sizes))))


;; Most of the layout panes should inherit from this
;;--- It would be nice if this and COMPOSITE-PANE had single- and multiple-child
;;--- versions so that users could get better error diagnostics

(defclass layout-pane (sheet-mute-input-mixin
                       sheet-with-resources-mixin
                       space-requirement-cache-mixin
                       client-overridability-mixin
                       sheet-permanently-enabled-mixin
                       composite-pane)
    ())

