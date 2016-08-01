;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;;; Basic histories

;; NIL means that the default history has unbounded length.
;;--- Should the default be bigger?  unbounded?
(defparameter *default-history-length* 40)

(defclass basic-history
          ()
    ((name :initform "Unnamed" :initarg :name
           :reader history-name)
     (elements :initform nil                        ;the contents of the history
               :reader history-elements)
     (current-length :initform 0)                ;current length of the history
     (maximum-length :initform *default-history-length*
                     :initarg :maximum-length        ;NIL means no bound on length
                     :reader history-maximum-length)
     (rotation :initform 0)                        ;state for m-Y
     (yank-position :initform 0)                ;c-Y/c-m-Y to m-Y communication
     (temporary-element :initform nil)))        ;can be bound to temporary front of list

(defmethod print-object ((history basic-history) stream)
  (print-unreadable-object (history stream :type t :identity t)
    (write-string (slot-value history 'name) stream)))

;; On strings, EQUAL does case-sensitive comparisons.
(defmethod history-elements-equal ((history basic-history) element1 element2)
  (equal element1 element2))

;; Default method for matching an element.  Assumes a string representation.
(defmethod history-match-element ((history basic-history) element string)
  (search string element))

;; Is this element visible in the history?  The default answer is yes.
(defmethod history-element-visible-p ((history basic-history) element)
  (declare (ignore element))
  t)

;; Default method for printing an element.  Assumes a string representation.
(defmethod print-history-element ((history basic-history) element stream)
  (write-string element stream))

(defmethod history-and-element ((history basic-history) element)
  (cons history element))

(define-presentation-type basic-history-element ())

(define-presentation-method presentation-typep (object (type basic-history-element))
  (and (consp object)
       (typep (car object) 'basic-history)))

(defmethod history-element-type ((history basic-history))
  'basic-history-element)

(defmethod display-history-menu-element ((history basic-history) stream number index
                                         &optional element-type)
  (when (null element-type)
    (setq element-type (history-element-type history)))
  (with-slots (rotation) history
    (let ((element (history-element history index)))
      (with-output-as-presentation
          (stream (history-and-element history element) element-type)
        (cond ((zerop rotation)
               (format stream "~3D: " number))
              ((= index rotation)
               (format stream "*~3D: " number))
              (t
               (format stream " ~3D: " number)))
        (print-history-element history element stream))
      (terpri stream))))

;; Number of elements in the history, including TEMPORARY-ELEMENT.
(defmethod history-length ((history basic-history))
  (with-slots (current-length temporary-element) history
    (if temporary-element (1+ current-length) current-length)))

(defmethod (setf history-maximum-length) (length (history basic-history))
  (with-slots (current-length elements maximum-length) history
    (when (and (not (null length))
               (< length current-length))
      (let ((new-elements
              (let ((new nil)
                    (elts elements))
                (repeat length
                  (push (pop elts) new))
                (nreverse new))))
         (setq elements new-elements
               current-length length)))
    (setq maximum-length length)))

;; Get the element from HISTORY indexed by INDEX.
(defmethod history-element ((history basic-history) index &optional fixup-p)
  (declare (values element index))
  (let ((delta 0))
    (with-slots (elements current-length temporary-element) history
      (without-scheduling
        (when temporary-element
          (when (or (zerop index) (and fixup-p (minusp index)))
            (return-from history-element (values temporary-element 0)))
          (decf index)
          (incf delta))
        (cond ((minusp index)
               (values (and fixup-p (first elements)) delta))
              ((< index current-length)
               (values (nth index elements) (+ index delta)))
              (fixup-p
               (values (car (last elements)) (+ current-length -1 delta)))
              (t nil))))))

;; Iterate over the elements in the history, starting at INDEX offset by OFFSET,
;; continuing CUTOFF-LENGTH times.  If TEST is supplied, it should be a function
;; of one argument, the element.
(defmacro do-history-elements ((history element-var index-var &rest options)
                               &body body)
  (declare (arglist (history element-var index-var
                     &key index offset cutoff-length test) &body body))
  `(flet ((do-history-elements-body (,element-var ,index-var) ,@body))
     (declare (dynamic-extent #'do-history-elements-body))
     (do-history-elements-1 ,history #'do-history-elements-body ,@options)))

(defmethod do-history-elements-1 ((history basic-history) continuation
                                  &key index offset cutoff-length test)
  (declare (dynamic-extent continuation))
  (setq index (cond ((null index) 0)
                    ((not (minusp index)) (1- index))
                    (t index)))
  (with-slots (rotation) history
    (let ((index (+ (or index rotation) (or offset 0)))
          (length (history-length history)))
      (repeat (if cutoff-length (min length cutoff-length) length)
        (unless (< -1 index length)
          (setq index (mod index length)))
        (let ((element (history-element history index)))
          (when (and (history-element-visible-p history element)
                     (or (null test) (funcall test element)))
            (funcall continuation element index)))
        (incf index)))))

;; This is the guts of c-Y/c-m-Y, or c-sh-Y/c-m-sh-Y.
;; Returns an element, or NIL if it can't find one (that matches).
;; INDEX is the index at which to start looking for a matching element.
;; If TEST is supplied, it should be a function of one argument, an element.
(defmethod yank-from-history ((history basic-history) &key index test)
  (with-slots (rotation yank-position) history
    (let ((idx (cond ((null index) rotation)
                     ((plusp index) (1- index))
                     ((minusp index) index)
                     (t (error "Zero should have been handled at a higher level"))))
          (fixup-p t))
      (loop
        (multiple-value-bind (element position)
            (history-element history idx fixup-p)
          (cond ((null element)
                 (return-from yank-from-history nil))
                ((and (or index (history-element-visible-p history element))
                      (or (null test) (funcall test element)))
                 (setq yank-position position)
                 (return-from yank-from-history element))))
        (incf idx)
        (setq fixup-p nil)))))

;; This is the guts of m-Y or m-sh-Y (having already figured out that the last
;; command was a yanking command and which history it used).
;; Returns NIL if it fails to find anything different.
;; INDEX and TEST are as for YANK-FROM-HISTORY.
(defmethod yank-next-from-history ((history basic-history) &key (index 1) test)
  (with-slots (yank-position rotation) history
    (when (zerop index)
      (error "Zero should have been handled at a higher level"))
    (when yank-position
      (let ((old-element (history-element history yank-position)))
        (do-history-elements (history element idx
                              :index (1+ yank-position) :offset index :test test)
          (unless (history-elements-equal history element old-element)
            (setq rotation (setq yank-position idx))
            (return-from yank-next-from-history element)))))))

;; This allows m-Y to work after doing a "random access" yank from the mouse.
(defmethod history-note-element-yanked ((history basic-history) element)
  (with-slots (yank-position) history
    (let ((index 0))
      (loop
        (let ((other (history-element history index)))
          (when (null other)
            (return-from history-note-element-yanked nil))
          (when (eq element other)
            (return-from history-note-element-yanked (setq yank-position index))))
        (incf index)))))

(defmethod history-note-element-index-yanked ((history basic-history) index)
  (with-slots (yank-position) history
    (setq yank-position index)))

;; Push an element onto the front of the history and extend its length.
(defmethod push-history-element ((history basic-history) element)
  (with-slots (current-length maximum-length elements rotation temporary-element) history
    (without-scheduling
      ;; Save space: don't push the new element on top if it is equal
      ;; to the element currently on top
      (unless (history-elements-equal history element (history-top-element history))
        (cond ((eq current-length maximum-length)
               (let ((cons (last elements)))
                 (setf (car cons) element)
                 (setf (cdr cons) (nbutlast elements))
                 (setq elements cons)))
              (t
               (push element elements)
               (incf current-length))))
      (setq rotation 0
            temporary-element nil))))

;; Pop the top element from the history, and reduce its length.
(defmethod pop-history-element ((history basic-history))
  (with-slots (current-length elements) history
    (when (and current-length (plusp current-length))
      (without-scheduling
        (decf current-length)
        (prog1 (car elements)
               (setq elements (cdr elements)))))))

(defmethod history-top-element ((history basic-history))
  (with-slots (elements) history
    (car elements)))

;; Replace the top element of the history with a new element.
(defmethod (setf history-top-element) (element (history basic-history))
  (with-slots (elements) history
    (setf (car elements) element)))

(defmethod canonicalize-default ((history basic-history) element)
  element)

(defmacro with-default-bound-in-history (history default-element &body body)
  `(flet ((with-default-bound-in-history-body () ,@body))
     (declare (dynamic-extent #'with-default-bound-in-history-body))
     (invoke-with-default-bound-in-history ,history ,default-element
                                           #'with-default-bound-in-history-body)))

(defmethod invoke-with-default-bound-in-history
           ((history basic-history) default-element continuation)
  (with-slots (yank-position rotation temporary-element) history
    ;; Reset both pieces of state so that multiple calls to ACCEPT start fresh
    (setq yank-position 0
          rotation 0)
    (letf-globally ((temporary-element default-element))
      (funcall continuation))))

;; Hook it up to the input editor
(defmethod history-replace-input
           ((history basic-history) (istream input-editing-stream-mixin) element
            &key replace-previous)
  (declare (ignore replace-previous))
  (with-slots (scan-pointer insertion-pointer) istream
    (setf scan-pointer insertion-pointer)        ;Don't delete anything
    (replace-input istream element :buffer-start insertion-pointer)))

(defmethod history-replace-input :around
           ((history basic-history) (istream input-editing-stream-mixin) element
            &key replace-previous)
  #-aclpc (declare (ignore element))
  (with-slots (insertion-pointer previous-insertion-pointer
               scan-pointer input-buffer mark) istream
    (unless replace-previous
      (setq mark insertion-pointer))
    ;; If we are m-Y'ing, remove the previously yanked quantity from the buffer.
    ;; PREVIOUS-INSERTION-POINTER is guaranteed to be valid when REPLACE-PREVIOUS
    ;; is T, because of how the input editor calls c-Y/c-m-Y and m-Y.
    (when (and replace-previous previous-insertion-pointer)
      (erase-input-buffer istream previous-insertion-pointer)
      (shift-buffer-portion input-buffer insertion-pointer previous-insertion-pointer)
      (setq insertion-pointer previous-insertion-pointer
            scan-pointer previous-insertion-pointer))
    (let ((old-ip insertion-pointer))
      (call-next-method)
      (unless replace-previous
        (setq previous-insertion-pointer old-ip)))))

(defmethod reset-history ((history basic-history))
  (with-slots (elements current-length rotation yank-position temporary-element) history
    (setq elements nil
          current-length 0
          rotation 0
          yank-position 0
          temporary-element nil)))


;;; Kill ring histories

(defclass kill-ring-history (basic-history) ())

(defparameter *kill-ring-abbreviation-length* 60)

;; Do a little abbreviation
(defmethod print-history-element ((history kill-ring-history) element stream)
  (let (length)
    (cond ((> (setq length (length element)) *kill-ring-abbreviation-length*)
           (let ((newline (position #\Newline element))
                 (half-length (floor *kill-ring-abbreviation-length* 2)))
             (cond (newline
                    (let ((other-newline (position #\Newline element :from-end t)))
                      (write-string element stream :end (min half-length newline))
                      (write-string "..." stream)
                      (write-string element stream :start (max (- length half-length)
                                                               other-newline))))
                   (t
                    (write-string element stream :end half-length)
                    (write-string "..." stream)
                    (write-string element stream :start (- length half-length))))))
          (t (write-string element stream)))))

(define-presentation-type kill-ring-element ()
  :inherit-from 'basic-history-element)

(define-presentation-method presentation-typep (object (type kill-ring-element))
  (and (consp object)
       (typep (car object) 'kill-ring-history)))

(defmethod history-element-type ((history kill-ring-history))
  'kill-ring-element)

(setq *kill-ring* (make-instance 'kill-ring-history :name "Kill ring"))

#+Genera
(defun genera-kill-ring-save (string merge)
  (let ((interval (zwei:strings-into-kill-ring-interval string)))
    (if merge
        (scl:send zwei:*kill-history* :replace-top interval nil)
        (scl:send zwei:*kill-history* :push interval nil))))

#+Genera
(defun clim-kill-ring-save (interval merge)
  (let ((string (si:string-thin
                  (typecase interval
                    (string interval)
                    (zwei:kill-ring-interval
                      (let ((zwei:*interval* interval))
                        (zwei:string-interval interval)))))))
    (if merge
        (setf (history-top-element *kill-ring*) string)
        (push-history-element *kill-ring* string))))

#+Genera
(setq zwei:*clim-kill-ring-save* 'clim-kill-ring-save)


;;; Presentation histories

(defclass presentation-history (basic-history)
    ((type :initarg :type :reader presentation-history-type)))

(defstruct (presentation-history-element (:type list))
  object
  type
  ;; Cache the printed representation for a particular view
  (string nil)
  (view nil))

#+(and allegro aclpc)
(eval-when (compile load)
  (setf (find-class 'presentation-history-element) nil))

#+(and allegro aclpc)
(eval-when (compile)
  (warn "~S structure hacked for bug2419" 'presentation-history-element))

(defun make-presentation-type-history (type &key (maximum-length *default-history-length*)
                                                 history-name)
  (let* ((type-name (presentation-type-name type))
         (history-name (or history-name
                           ;; check that type-name itself is a valid presentation type
                           ;; before calling describe-presentation-type (cim 11/17/95)
                           (and (presentation-type-specifier-p type-name)
                                (describe-presentation-type type-name nil nil)))))
    (make-instance 'presentation-history
      :name history-name :type type-name
      :maximum-length maximum-length)))

(defmethod history-elements-equal ((history presentation-history) element1 element2)
  ;; Match by the object and its type, the view doesn't matter
  (and (equal (presentation-history-element-object element1)
              (presentation-history-element-object element2))
       (equal (presentation-history-element-type element1)
              (presentation-history-element-type element2))))

;; The element is visible only if it matches the desired presentation type.
(defmethod history-element-visible-p ((history presentation-history) element)
  (presentation-typep (presentation-history-element-object element)
                      (presentation-type-for-yanking history)))

;; The string matcher for presentation histories.
(defmethod history-match-element ((history presentation-history) element string)
  (search string (unparse-presentation-history-element history element)))

(defmethod unparse-presentation-history-element ((history presentation-history) element
                                                 &key (view +textual-view+))
  (or (and (eq view (presentation-history-element-view element))
           (presentation-history-element-string element))
      (let ((string
              (present-to-string (presentation-history-element-object element)
                                 (presentation-history-element-type element)
                                 :view view :acceptably t
                                 :for-context-type (presentation-type-for-yanking history))))
        (setf (presentation-history-element-view element) view)
        (setf (presentation-history-element-string element) string)
        string)))

(defmethod print-history-element ((history presentation-history) element stream)
  (handler-case
      (let ((string (unparse-presentation-history-element
                      history element :view (stream-default-view stream))))
        (with-output-as-presentation (stream
                                      (presentation-history-element-object element)
                                      (presentation-history-element-type element))
          (write-string string stream)))
    (error ()
      (with-text-style (stream *accept-result-style*)
        (present (presentation-history-element-object element)
                 (presentation-history-element-type element)
                 :view (stream-default-view stream) :acceptably nil
                 :for-context-type (presentation-type-for-yanking history))))))

(define-presentation-type presentation-history-element ()
  :inherit-from 'basic-history-element)

(define-presentation-method presentation-typep (object (type presentation-history-element))
  (and (consp object)
       (typep (car object) 'presentation-history)))

(defmethod history-element-type ((history presentation-history))
  'presentation-history-element)

(defmethod history-and-element ((history presentation-history) element)
  (cons history (unparse-presentation-history-element history element)))

(defmethod presentation-type-for-yanking ((history presentation-history))
  (with-slots (type) history
    (or (and (presentation-subtypep-1
               *presentation-type-for-yanking*
               (presentation-history-type history))
             *presentation-type-for-yanking*)
        (dolist (context *input-context*)
          ;; If there is an input context that is more specific than the type
          ;; provided by the history, use it.
          (when (presentation-subtypep-1
                  (presentation-type-name (input-context-type context)) type)
            (return-from presentation-type-for-yanking (input-context-type context))))
        type)))

(defmethod canonicalize-default ((history presentation-history) element)
  (unparse-presentation-history-element history element))

#+compulsive-type-checking
(defmethod push-history-element :around ((history presentation-history) element)
  (when (and (presentation-typep
               (presentation-history-element-object element)
               (presentation-history-element-type element))
             (presentation-subtypep-1
               (presentation-history-element-type element)
               (presentation-history-type history)))
    (call-next-method)))

(defmethod history-replace-input
           ((history presentation-history) (istream input-editing-stream-mixin) element
            &key replace-previous)
  (declare (ignore replace-previous))
  (presentation-replace-input istream
                              (presentation-history-element-object element)
                              (presentation-history-element-type element)
                              (stream-default-view istream)
                              :buffer-start (stream-insertion-pointer istream)
                              :for-context-type *presentation-type-for-yanking*))

(defun reset-all-presentation-histories ()
  (maphash #'(lambda (type-name class)
               (declare (ignore class))
               (when (presentation-type-specifier-p type-name)
                 (let ((history (presentation-type-history type-name)))
                   (when history
                     (reset-history history)))))
           *presentation-type-class-table*))

#+Genera
(si:define-gc-optimization reset-all-presentation-histories :cleanup
  (:documentation "Reset presentation histories")
  (:before-flip (incremental)
   (declare (ignore incremental))
   (reset-history *kill-ring*)
   (reset-all-presentation-histories)))

#+Genera
(scl:add-initialization "Reset presentation histories"
  '(progn
     (reset-history *kill-ring*)
     (reset-all-presentation-histories))
  '(before-cold))


;;; Display the contents of a history

(defparameter *history-menu-length* 20.)

(define-presentation-type input-editor (&key stream))

(define-presentation-method presentation-typep (object (type input-editor))
  (declare (ignore object))
  nil)

(define-presentation-method presentation-subtypep ((subtype input-editor) supertype)
  (declare (ignore supertype))
  t)

(define-presentation-type display-rest-of-history ())

(defmethod display-history-contents ((history basic-history) stream
                                     &key maximum-length (start-index 0) string)
  (with-input-editor-help stream
    (with-slots (name) history
      (let ((current-length (history-length history))
            (scanned-length 0))
        (format stream "~&~A history:~%" name)
        (when (null maximum-length)
          (setq maximum-length (min current-length *history-menu-length*)))
        (let* ((height (bounding-rectangle-height stream))
               (line-height (stream-line-height stream))
               (lines (floor height line-height)))
          ;; 3 allows for the heading line, the parenthesized footing line,
          ;; and the blank line that the cursor gets left on.
          (when (< (- lines 3) maximum-length)
            (setq maximum-length (- lines 3))))
        (let ((elts-displayed 0)
              (max-displayed-index 0)
              (element-type (history-element-type history)))
          ;; Print elements with negative rotated positions.
          (block display-them
            (let ((previous nil))
              (do-history-elements (history element index
                                    :test (when string
                                            #'(lambda (elt)
                                                (history-match-element history elt string))))
                (incf scanned-length)
                (when (and (>= index start-index)
                           (or (null previous)
                               (not (history-elements-equal history element previous))))
                  (incf elts-displayed)
                  (maxf max-displayed-index index)
                  (display-history-menu-element history stream (1+ index) index
                                                element-type))
                (when (>= elts-displayed maximum-length)
                  (return-from display-them (values)))
                (setq previous element))))
          (if (zerop current-length)
              (write-string "(History is empty.)" stream)
            (if string
                (cond ((zerop elts-displayed)
                       (write-string "(No items of history match.)" stream))
                      ((= scanned-length current-length)
                       (write-string "(End of matching items in history.)" stream))
                      (t
                       (with-output-as-presentation (stream
                                                     (list history max-displayed-index string)
                                                     'display-rest-of-history)
                         (write-string "Display rest of history." stream))))
                (cond ((zerop elts-displayed)
                       (write-string "(No items of history displayed.)" stream))
                      ((= scanned-length current-length)
                       (write-string "(End of history.)" stream))
                      (t
                       (with-output-as-presentation (stream
                                                     (list history max-displayed-index nil stream)
                                                     'display-rest-of-history)
                         (format stream "(~D more item~:P in history.)"
                           (- current-length max-displayed-index)))))))
          (terpri stream))))))
