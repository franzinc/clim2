;; -*- mode: common-lisp; package: tk -*-
;;
;;				-[]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, CA  All rights reserved.
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
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: make-classes.lisp,v 1.7 92/02/16 20:55:03 cer Exp $

(in-package :tk)

#+obsolete
(ff:defforeign 'insert_classes :return-type :fixnum)


(defun get-entry-point-value (x)  
  (let ((xx (make-array 1 :element-type '(unsigned-byte 32))))
    (unless (zerop (get-entry-points (vector x) xx))
      (error "Cannot find the entry-point for: ~S" x))
    (aref xx 0)))

(def-c-type (class-array :in-foreign-space) 1 :unsigned-long)

(defun get-foreign-variable-value (x)
  (class-array (get-entry-point-value x) 0))

(defun get-resource-list-internal (class fn resource-class)
  (let ((x (make-array 1 :element-type '(unsigned-byte 32)))
	(y (make-array 1 :element-type '(unsigned-byte 32))))
    (funcall fn class x y)
    (let ((resources (aref x 0))
	  (n (aref y 0))
	  (r nil))
      (dotimes (i n)
	(push (make-instance
	       resource-class
	       :original-name (x-resource-name (x-resource-list resources i))
	       :name (lispify-resource-name (char*-to-string (x-resource-name (x-resource-list resources i))))
	       :class (lispify-resource-class 
		       (char*-to-string (x-resource-class (x-resource-list resources i))))
	       :type (lispify-resource-type 
		      (char*-to-string (x-resource-type (x-resource-list resources i)))))
	      r))
      (xt_free resources)
      r)))
      
(defun get-resource-list (class) 
  (get-resource-list-internal class #'get-resource-list-1 'resource))

(defun get-constraint-resource-list (class) 
  (get-resource-list-internal class #'get-constraint-resource-list-1
			      'constraint-resource))



(defclass display-object ()
	  ((display :initarg :display :reader object-display)))

(defclass xt-root-class (handle-class display-object)
	  ((display :initarg :display :reader widget-display)
	   (events :initform nil :accessor widget-event-handler-data)
	   (callbacks :initform nil :accessor widget-callback-data)
	   (window-cache :initform nil))
  (:metaclass xt-class))

(defclass rect (xt-root-class) ())
(defclass un-named-obj (rect) ())

(defclass basic-resource ()
  (
   (name :initarg :name :reader resource-name)
   (original-name :initarg :original-name :reader resource-original-name)
   (type :initarg :type :reader resource-type)
   (class :initarg :class :reader resource-class)))

(defmethod print-object ((r basic-resource) s)
  (print-unreadable-object (r s :type t :identity nil)
			   (format s "~A,~A " 
				   (resource-name r)
				   (resource-type r))))

(defun lispify-resource-name (x) (lispify-tk-name x :package :keyword))
(defun lispify-resource-class (x) (lispify-tk-name x))
(defun lispify-resource-type (x) (lispify-tk-name x))

(defclass resource (basic-resource)
	  ())


			 
(defclass constraint-resource (basic-resource)
	  ())


(defvar *handle-class-mapping* (make-hash-table :test #'equalp))
  
(defun make-classes (classes)
  (let ((direct-resources nil)
	(all-resources nil))


    (dolist (class-ep classes)
      (let ((h (get-foreign-variable-value class-ep)))
	(push (list h 
		    class-ep
		    (get-resource-list h)
		    (get-constraint-resource-list h))
	      direct-resources)))
      
    (setq direct-resources (nreverse direct-resources))
    
    (dolist (class-ep classes)
      (format excl:*initial-terminal-io* "Initializing class ~s~%" class-ep)
      (let ((h (get-foreign-variable-value class-ep)))
	(initialize-widget-class h)
	(push (list h
		    (get-resource-list h)
		    (get-constraint-resource-list h))
	      all-resources)))

    (do ((directs direct-resources (cdr directs))
	 r)
	((null directs)
	 (nreverse r))
      (destructuring-bind
       ((handle class-ep direct-resources direct-constraints) . ignore)
       directs
       (declare (ignore ignore))
       (destructuring-bind
	(handle all-resources all-constraints) 
	(assoc handle all-resources)
	(let ((class
	       (clos::ensure-class
		(lispify-class-name (widget-class-name handle))
		:direct-superclasses (list (if (zerop (xtk-class-superclass handle))
					    'xt-root-class
					  (lispify-class-name (widget-class-name
							       (xtk-class-superclass handle)))))
		:direct-slots nil
		:metaclass 'xt-class
		:direct-resources  direct-resources
		:direct-constraints  direct-constraints
		:resources all-resources
		:constraints  all-constraints
		:entry-point class-ep
		:handle  (get-foreign-variable-value class-ep))))
	  (register-address class)
	  (add-accessors-for-toolkit-class class)
	  (push class r)))))))


(defun add-accessors-for-toolkit-class (class)
  (dolist (r (class-direct-resources class))
    (let* ((rname (resource-name r))
	   (name (intern (format nil "~A-~A" 'widget rname))))
      (setq name (case name
		   (widget-window 'widget-stub-window)
		   (t name)))
      (add-method
       (ensure-generic-function name)
       (make-instance 'clos::standard-method
		      :lambda-list '(x)
		      :qualifiers nil
		      :specializers (list class)
		      :function #'(lambda (x)
				    (get-values x rname))))
      (add-method
       (ensure-generic-function `(setf ,name))
       (make-instance 'clos::
		      standard-method
		      :lambda-list '(nv x)
		      :qualifiers nil
		      :specializers (list (find-class t) class)
		      :function #'(lambda (nv x)
				    (set-values x rname nv)
				    nv))))))

(defun define-toolkit-classes (classes)
  (make-classes classes))

	
(defun widget-class-name (h)
  (char*-to-string (xtk-class-name h)))

(defun lispify-class-name (x) 
  (let ((name (lispify-tk-name x)))
    (case name
      ;; Openlook
      (list 'ol-list)
      (t name))))
  
(defun lispify-tk-name (string &key 
			       (start 0)
			       (package *package*)
			       prefix)
  (let ((string
	 (let ((n start) 
	       (frags (and prefix (list prefix)))
	       old-n)
	   (loop
	    (setq old-n n
		  n (position-if #'upper-case-p string 
				 :start (or n 0)))
	    (if n
		(progn 
		  (push (subseq string (or old-n 0) n) frags)
		  (when (> n 0) (push "-" frags))
		  (push (string-downcase (subseq string n (1+ n))) frags)
		  (incf n))
	      (return (apply #'concatenate 
			     'simple-string
			     (nreverse (cons (subseq string old-n)
					     frags)))))))))
    (if package
	(intern string package)
      string)))

(defun collect-resource-types (c)
  (let (ts)
    (clos::map-over-subclasses
     #'(lambda (class)
	 (dolist (r (append (class-constraint-resources class) (class-resources class)))
	   (pushnew (resource-type r) ts)))
     c)
    ts))



(defun-c-callable toolkit-error-handler ((message :unsigned-long))
  (error "toolkit errror: ~a" (char*-to-string message)))

(defun-c-callable toolkit-warning-handler ((message :unsigned-long))
  (warn "toolkit warning: ~a" (char*-to-string message)))


(defun add-resource-to-class (class resource)
  (clos::map-over-subclasses
   #'(lambda (c)
       (push
	resource
	(slot-value c 'resources)))
   class))

#|
(make-widget class parent . resources)
|#
