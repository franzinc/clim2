;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2004 Franz Inc, Oakland, CA - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: protocols.lisp,v 2.6 2005/12/08 21:25:47 layer Exp $

;;;
;;; Copyright (c) 1989, 1990 by Xerox Corporation.  All rights reserved. 
;;;

(in-package :clim-utils)

;;;
;;; Protocol Stuff (should be moved to a utils package)
;;;

(defclass protocol ()
    ((name :initarg :name :accessor protocol-name)
     (roles :initarg :roles :accessor protocol-roles)
     (operations :initform () :initarg :operations
                 :accessor protocol-operations)))

(defmethod print-object ((protocol protocol) stream)
  (print-unreadable-object (protocol stream :type t :identity t)
    (write (protocol-name protocol) :stream stream :escape nil)))

(defvar *protocols* nil)
(defun-inline find-protocol (name) (getf *protocols* name))
(defsetf find-protocol (name) (value) `(setf (getf *protocols* ,name) ,value))

(defmacro defprotocol (name supers &rest options)
 (declare (ignore supers options))
  `(eval-when (compile eval load)
     (setf (find-protocol ',name)
           (make-instance 'protocol :name ',name))))


(defclass role ()
    ((name :initarg :name :accessor role-name)
     (slots :initarg :slots :accessor role-slots)))

(defmethod print-object ((role role) stream)
  (print-unreadable-object (role stream :type t :identity t)
    (write (role-name role) :stream stream :escape nil)))

(defvar *roles* nil)
(defun-inline find-role (name) (getf *roles* name))
(defsetf find-role (name) (value) `(setf (getf *roles* ,name) ,value))
  
(defmacro defrole (class supers slots &rest options)
  (declare (ignore supers options))
  `(eval-when (compile eval load)
     #+PCL
     (pcl::do-standard-defsetf
       ,@(mapcan #'(lambda (slot)
                     (with-collection
                       (do* ((tail (cdr slot) (cddr tail))
                             (key  (car tail) (car tail))
                             (val  (cadr tail) (cadr tail)))
                            ((null tail))
                         (when (or (eq key :writer)
                                   (eq key :accessor))
                           (collect val)))))
                 slots))
     (setf (find-role ',class)
           (make-instance 'role :name ',class :slots ',slots))))


(defclass operation ()
    ((name :initarg :name :accessor operation-name)
     (required-args :initarg :required-args :accessor operation-required-args)
     (specs :initarg :specs :accessor operation-specs)
     (extra-args :initarg :extra-args :accessor operation-extra-args)))

(defmethod print-object ((operation operation) stream)
  (print-unreadable-object (operation stream :type t :identity t)
    (write (operation-name operation) :stream stream :escape nil)))
(defmacro defoperation (name protocol arg-specs &body options)
  #+Genera (declare (zwei:indentation 2 1))
  (let* ((pos (position-if #'(lambda (x) (member x '(&key &optional &rest)))
                           arg-specs))
         (required-arg-specs (if pos (subseq arg-specs 0 pos) 
                                 arg-specs))
         (required-args (mapcar #'(lambda (arg-spec) 
                                    (if (listp arg-spec)
                                        (first arg-spec)
                                        arg-spec))
                                required-arg-specs))
         (specs (mapcar #'(lambda (arg-spec) 
                            (if (listp arg-spec)
                                (second arg-spec)
                                t))
                        required-arg-specs))
         (extra-args (and pos (subseq arg-specs pos)))
         (trampoline-extra-args extra-args)
         (keyword-args (member '&key extra-args))
         (defgeneric
           ;; Kludge to inhibit doing the DEFGENERIC...
           (unless (second (assoc :no-defgeneric options))
             `((defgeneric ,name (,@required-args
                                  ,@(mapcar #'(lambda (arg)
                                                (cond ((atom arg) arg)
                                                      ((atom (first arg)) (first arg))
                                                      (t (first (first arg)))))
                                            extra-args))
                 ,@options)))))
    (when keyword-args
      (setq trampoline-extra-args (append (ldiff extra-args keyword-args)
                                          (unless (member '&rest extra-args)
                                            `(&rest keyword-arguments)))))
    `(eval-when (compile eval load)
       #-VDPCL ,@defgeneric                        ;PCL's defgeneric fails.
       (let* ((protocol (find-protocol ',protocol))
              (operation
                (make-instance 'operation
                  :name ',name
                  :required-args ',required-args
                  :specs ',specs
                  :extra-args ',trampoline-extra-args)))
         ;; Just simple now.
         (push-unique operation (protocol-operations protocol) 
                      :key #'operation-name)))))

;; This gets bound to the outermost player
(defvar *outer-self* nil)

(defmacro define-trampoline-template
          (protocol-name role-name role-player (player-var body-var) outer-self
           &body body)
  (let ((protocol (find-protocol protocol-name))
        (macro-name (gensymbol protocol-name 'trampoline-generator)))
    (unless protocol
      (warn "~S: can't find protocol named ~S" 'define-trampoline-template protocol-name))
    (when (null outer-self) (setq outer-self '*outer-self*))
    `(progn
       (defmacro ,macro-name (,player-var &body ,body-var) ,@body)
       ;; Don't blow up when protocol is undefined.
       ,@(when protocol
           (mapcar
             #'(lambda (operation)
                 (with-slots (name required-args specs extra-args) operation
                   (let* ((subst-extra-args 
                            (subst role-player role-name extra-args))
                          (role-pos (position role-name specs))
                          (arg-specs (copy-list required-args))
                          (call-specs (copy-list required-args))
                          (rest-p (member '&rest subst-extra-args))
                          (extras (make-pass-on-arglist subst-extra-args)))
                     (setf (nth role-pos arg-specs) `(,player-var ,role-player))
                     (setf (nth role-pos call-specs) player-var)
                     `(defmethod ,name (,@arg-specs ,@subst-extra-args)
                        ,@(when rest-p
                            `((declare (dynamic-extent ,(second rest-p)))))
                        (let ((,outer-self (or ,outer-self ,player-var)))
                          (,macro-name ,player-var
                           ,(if rest-p
                                `(apply #',name ,@call-specs ,@extras)
                                `(,name ,@call-specs ,@extras))))))))
             (protocol-operations protocol))))))

(defmacro define-slot-trampoline-template
          (protocol-name role-name role-player (player-var body-var) outer-self
           &body body)
  (let ((role (find-role role-name))
        (macro-name (gensymbol protocol-name 'trampoline-generator)))
    (unless role (warn "~S: can't find role ~S" 'define-slot-trampoline-template role-name))
    (when (null outer-self) (setq outer-self '*outer-self*))
    `(progn
       (defmacro ,macro-name (,player-var &body ,body-var) ,@body)
       ;; Don't blow up when role is undefined.
       ,@(when role
           (mapcan
             #'(lambda (slot)
                 (let ((writer (or (getf (cdr slot) :writer)
                                   (getf (cdr slot) :accessor)))
                       (reader (or (getf (cdr slot) :reader)
                                   (getf (cdr slot) :accessor)))
                       (nvalues (or (getf (cdr slot) :nvalues) 1)))
                   `(,@(when reader
                         `((defmethod ,reader ((,role-player ,role-player))
                             (let ((,outer-self (or ,outer-self ,role-player)))
                               (,macro-name ,player-var 
                                (,reader ,player-var))))))
                     ,@(when writer
                         (let ((values-vars
                                 (with-collection 
                                   (dotimes (i nvalues)
                                     (collect 
                                       (make-symbol 
                                         (format nil "~A-~D" 'new-value i)))))))
                           `((,(if (= nvalues 1) 'defmethod 'defmethod*)
                              (setf ,writer) (,@values-vars (,role-player ,role-player))
                              (let ((,outer-self (or ,outer-self ,role-player)))
                                (,macro-name ,player-var 
                                 (setf (,writer ,player-var) (values ,@values-vars)))))))))))
             (role-slots (find-role role-name)))))))

;; If you change this to maintain a more complex mapping than a simple
;; binding of *ORIGINAL-STREAM*, change ENCAPSULATED-STREAM too.
(defmacro generate-trampolines (protocol-name role-name role-player delegate-form
                                &optional (outer-self '*outer-self*))
  `(progn
     (define-trampoline-template ,protocol-name ,role-name ,role-player
                                 (,role-player body) ,outer-self
       `(let ((,',role-player ,,delegate-form))
          ,@body))
     (define-slot-trampoline-template ,protocol-name ,role-name ,role-player
                                      (,role-player body) ,outer-self
       `(let ((,',role-player ,,delegate-form))
          ,@body))))

(defmacro define-protocol-class (class-name superclass-names)
  (let ((predicate-name (if (find #\- (string class-name))
                            (fintern "~A-~A" class-name 'p)
                            (fintern "~A~A" class-name 'p))))
    `(progn
       (defclass ,class-name ,superclass-names ())
       (defgeneric ,predicate-name (object)
         #+Genera (declare (sys:function-parent ,class-name define-protocol-class)))
       (defmethod ,predicate-name ((object t))
         #+Genera (declare (sys:function-parent ,class-name define-protocol-class))
         nil)
       (defmethod ,predicate-name ((object ,class-name))
         #+Genera (declare (sys:function-parent ,class-name define-protocol-class))
         t))))
