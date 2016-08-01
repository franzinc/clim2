;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved."

(defstruct (resource-descriptor (:conc-name rd-)
                                #+allegro (:print-function print-resource)
                                (:constructor make-resource-descriptor (name)))
  name
  (objects nil)
  constructor
  initializer
  deinitializer
  matcher
  (lock (make-lock "a resource lock")))

#+allegro
(defun print-resource (o s d)
  (declare (ignore d))
  (format s "#<~S ~S>" (type-of o) (rd-name o)))

(defstruct (object-storage (:conc-name os-))
  object
  use-cons
  parameters)

(defmacro with-resource-rd ((resource rd) &body body)
  (let ((resource-var (gensymbol 'resource)))
    `(let ((,resource-var ,resource)                ;once-only...
           (,rd nil))
       (cond ((symbolp ,resource-var)
              (setq ,rd (lookup-resource-descriptor ,resource-var)))
             ((typep ,resource-var 'resource-descriptor)
              (setq ,rd ,resource-var)))
       (unless ,rd (error "Can't find resource ~S" ,resource))
       ,@body)))

(defun describe-resource (resource)
  (with-resource-rd (resource rd)
    (format t "~&Resource ~S:" (rd-name rd))
    (when (rd-constructor rd)
      (format t "~%Constructor: ~S" (rd-constructor rd)))
    (when (rd-initializer rd)
      (format t "~%Initializer: ~S" (rd-initializer rd)))
    (when (rd-matcher rd)
      (format t "~%Matcher: ~S" (rd-matcher rd)))
    (let ((objects (rd-objects rd)))
      (when (> (fill-pointer objects) 0)
        (format t "~%~D Object~:P:" (fill-pointer objects)))
      (doseq (object-storage objects)
        (format t "~%  ~S, ~:[not in~;in~] use"
                (os-object object-storage)
                (cdr (os-use-cons object-storage)))))))

(defun map-resource (function resource)
  (declare (dynamic-extent function))
  (with-resource-rd (resource rd)
    (let ((objects (rd-objects rd)))
      (doseq (object-storage objects)
        (funcall function
                 (os-object object-storage) 
                 (not (null (cdr (os-use-cons object-storage))))
                 resource)))))

(defun clear-resource (resource)
  (with-resource-rd (resource rd)
    (let ((objects (rd-objects rd)))
      (dotimes (i (array-total-size objects))
        (setf (aref objects i) nil))
      (setf (fill-pointer objects) 0)))
  nil)

(defun lookup-resource-descriptor (name)
  (get name 'defresource))

(defun set-resource-descriptor (name value)
  (setf (get name 'defresource) value))

(defsetf lookup-resource-descriptor set-resource-descriptor)

(defun default-resource-matcher (object object-storage &rest parameters)
  (declare (ignore object))
  (declare (dynamic-extent parameters))
  (let ((object-parameters (os-parameters object-storage)))
    (equal object-parameters parameters)))

(defmacro defresource (name parameters &key constructor initializer deinitializer
                       matcher initial-copies)
  (unless constructor
    (error "Can't make a resource without a constructor: ~S" name))
  (let ((result ()))
    (labels ((output (form) (push form result))
             (cleanup-lambda-list (lambda-list)
               (mapcar #'(lambda (x) (if (listp x) (first x) x))
                       (set-difference lambda-list lambda-list-keywords)))
             (output-function (function type &rest extra-parameters)
               (declare (dynamic-extent extra-parameters))
               (cond ((null function) nil)
                     ((symbolp function) `#',function)
                     (t (let ((function-name
                                (make-symbol (concatenate 'string
                                               (string name) "-" (string type)))))
                          (output `(defun ,function-name (,@extra-parameters ,@parameters)
                                     ,@extra-parameters ,@(cleanup-lambda-list parameters)
                                     ,function))
                          `#',function-name)))))
      (setf constructor (output-function constructor 'constructor 
                                         (make-symbol (symbol-name 'rd)))
            initializer (output-function initializer 'initializer name)
            matcher (or (output-function matcher 'matcher name
                                         (make-symbol (symbol-name 'object-storage)))
                        (output-function 'default-resource-matcher 'matcher))
            deinitializer (letf-globally ((parameters nil))
                            (output-function deinitializer 'deinitializer name)))
      (output `(defresource-load-time ',name
                 ,constructor
                 ,initializer
                 ,deinitializer
                 ,matcher
                 ',initial-copies)))
    `(define-group ,name defresource ,@(nreverse result))))

(defun defresource-load-time (name constructor initializer deinitializer matcher
                              initial-copies)
  #+++ignore
  (when (lookup-resource-descriptor name)
    (cerror "Continue to overwrite old definition of resource ~S"
            "Attempt to redefine resource ~S" name))
  (let ((rd (or (lookup-resource-descriptor name) (make-resource-descriptor name))))
    (with-lock-held ((rd-lock rd) "Resource lock")
      (unless (rd-objects rd)
        (setf (rd-objects rd) (make-array (* 2 (max (or initial-copies 10) 10))
                                          :fill-pointer 0))
        (when initial-copies
          (repeat initial-copies
            (vector-push (cons nil (funcall constructor rd)) (rd-objects rd)))))
      (setf (rd-constructor rd) constructor)
      (setf (rd-initializer rd) initializer)
      (setf (rd-deinitializer rd) deinitializer)
      (setf (rd-matcher rd) matcher)
      (setf (lookup-resource-descriptor name) rd))))

(defun allocate-resource (name &rest parameters &aux rd object-storage)
  (declare (dynamic-extent parameters))
  (block allocate
    (setf rd (lookup-resource-descriptor name))
    (unless rd (error "Can't allocate nonexistent resource ~S" name))
    (with-lock-held ((rd-lock rd) "Resource lock")
      (let* ((array (rd-objects rd))
             (matcher (rd-matcher rd))
             (fill-pointer (fill-pointer array)))
        (declare (type vector array))
        (dotimes (i fill-pointer)
          (setf object-storage (aref array i))
          (when (and (null (cdr (os-use-cons object-storage)))
                     (if (null matcher)
                         (equal (os-parameters object-storage) parameters)
                         (apply matcher (os-object object-storage) object-storage parameters)))
            (if (os-use-cons object-storage)
                (setf (car (os-use-cons object-storage)) rd
                      (cdr (os-use-cons object-storage)) i)
                (setf (os-use-cons object-storage) (cons rd i)))
            (return-from allocate)))
        (let* ((new-object (apply (rd-constructor rd) rd parameters))
               (array-size (array-dimension array 0)))
          (setf object-storage (make-object-storage
                                 :object new-object
                                 :use-cons (cons rd fill-pointer)
                                 :parameters (copy-list parameters)))
          (when (<= array-size fill-pointer)
            (let ((new-array (make-array (* 2 array-size)
                                         :fill-pointer fill-pointer)))
              (replace new-array array)
              (setf array (setf (rd-objects rd) new-array))))
          (vector-push object-storage array)))))
  (when (rd-initializer rd) (apply (rd-initializer rd) (os-object object-storage) parameters))
  (values (os-object object-storage) object-storage))

(defun deallocate-resource (name object &optional allocation-key
                            &aux rd object-array object-index object-storage)
    (if allocation-key
        (setf rd (car (os-use-cons allocation-key)) object-array (rd-objects rd)
              object-index (cdr (os-use-cons allocation-key)))
        (setf rd (or (lookup-resource-descriptor name)
                     (error "Can't deallocate nonexistent resource ~S" name))
              object-array (rd-objects rd)
              object-index (position object object-array
                                     :key #'os-object)))
    (unless object-index
      (error "Can't deallocate object ~S: not present in resource ~S" object (rd-name rd)))
    (with-lock-held ((rd-lock rd) "Resource lock")
      (setf object-storage (aref object-array object-index))
      (unless (cdr (os-use-cons object-storage))
        (error "Can't deallocate object ~S: already deallocated from resource ~S"
               object (rd-name rd)))
      (unless (eq object (os-object object-storage))
        (error "Can't deallocate object ~S: not present in resource ~S" object (rd-name rd)))
      (when (rd-deinitializer rd)
        (funcall (rd-deinitializer rd) (os-object object-storage)))
      (setf (cdr (os-use-cons object-storage)) nil)))

(defmacro using-resource ((variable resource &rest parameters) &body body)
  (let ((allocation-key (make-symbol (symbol-name 'allocation-key))))
    `(let ((,variable nil)
           (,allocation-key nil))
       (unwind-protect
           (progn (multiple-value-setq (,variable ,allocation-key)
                    (allocate-resource ',resource ,@parameters))
                  ,@body)
         (when ,allocation-key
           (deallocate-resource ',resource ,variable ,allocation-key))))))

;;; A combination of LETF-GLOBALLY and USING-RESOURCE -- written because
;;; we only want to have a single unwind-protect in a stack frame if
;;; possible.  Who knows how slow they are?
(defmacro letf-using-resource ((place resource &rest parameters) &body body)
  (let ((old-place (gensymbol 'letf-globally-temp))
        (resource-variable (gensymbol resource 'temp))
        (allocation-key (gensymbol 'allocation-key)))
    `(let ((,resource-variable nil)
           (,allocation-key nil)
           (,old-place ,place))
       (unwind-protect
           (progn (multiple-value-setq (,resource-variable ,allocation-key)
                    (allocate-resource ',resource ,@parameters))
                  (setf ,place ,resource-variable)
                  ,@body)
         (setf ,place ,old-place)
         (when ,allocation-key (deallocate-resource ',resource ,resource-variable
                                                    ,allocation-key))))))
