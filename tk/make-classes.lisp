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
;; $fiHeader: make-classes.lisp,v 1.24 92/08/18 17:23:20 cer Exp Locker: cer $

(in-package :tk)

(defun get-entry-point-value (x)  
  (let ((xx (make-array 1 :element-type '(unsigned-byte 32))))
    (unless (zerop (get-entry-points (vector x) xx))
      (error "Cannot find the entry-point for: ~S" x))
    (aref xx 0)))

(defun get-foreign-variable-value (x)
  (class-array (get-entry-point-value x) 0))

;; This is only called to fill in the cache, so it can be (real) slow.
(defun get-resource-internal (class fn resource-class resource-name)
  (let ((x (make-array 1 :element-type '(unsigned-byte 32)))
	(y (make-array 1 :element-type '(unsigned-byte 32)))
	result)
    (funcall fn (class-handle class) x y)
    (let ((resources (aref x 0))
	  (n (aref y 0)))
      (setq result
	(dotimes (i n)
	  (let* ((res (x-resource-list resources i))
		 (original-name (x-resource-name res))
		 (name (lispify-resource-name (char*-to-string original-name))))
	    (if (equal name resource-name)
		(let ((*package* (find-package :tk)))
		  (return (make-instance resource-class
			    :original-name original-name
			    :name name
			    :class (lispify-resource-class 
				    (char*-to-string (x-resource-class res)))
			    :type (lispify-resource-type 
				   (char*-to-string (x-resource-type res))))))))))
      (xt_free resources))
    #+ignore
    (unless result
      ;; Error is actually caught in calling functions.
      (error "No resource named ~s found for class ~s" resource-name class))
    result))

;; These are so we don't need the foreign functions at run time.
(defun xt-get-resource-list (class res-return num-res-return)
  (xt_get_resource_list class res-return num-res-return))
(defun xt-get-constraint-resource-list (class res-return num-res-return)
  (xt_get_constraint_resource_list class res-return num-res-return))

(defmethod find-class-resource ((class xt-class) resource-name)
  (declare (optimize (speed 3) (safety 0)))
  (with-slots (cached-resources specially-hacked-resources) class
    (multiple-value-bind
	(value foundp)
	(gethash resource-name cached-resources)
      (if foundp
	  value				; Never had it, never will
	(setf (gethash resource-name cached-resources)
	  (if* (find resource-name specially-hacked-resources
		     :key #'resource-name)
	     thenret
	     else (get-resource-internal class
					 #'xt-get-resource-list
					 'resource
					 resource-name)))))))

(defmethod find-class-constraint-resource ((class xt-class) resource-name)
  (declare (optimize (speed 3) (safety 0)))
  (with-slots (cached-constraint-resources) class
    (multiple-value-bind
	(value foundp)
	(gethash resource-name cached-constraint-resources)
      (if foundp
	  value				; Never had it, never will
	(setf (gethash resource-name cached-constraint-resources)
	  (get-resource-internal class
				 #'xt-get-constraint-resource-list
				 'constraint-resource
				 resource-name))))))



;; These next five aren't used except maybe by describe-widget.
(defmethod class-resources ((class xt-class))
  (append 
   (slot-value class 'specially-hacked-resources)
   (get-resource-list class)))

(defmethod class-constraint-resources ((class xt-class))
  (get-constraint-resource-list class))

(defun get-resource-list-internal (class fn resource-class)
  (let ((x (make-array 1 :element-type '(unsigned-byte 32)))
	(y (make-array 1 :element-type '(unsigned-byte 32))))
    (funcall fn (class-handle class) x y)
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
  (get-resource-list-internal class #'xt-get-resource-list 'resource))

(defun get-constraint-resource-list (class) 
  (get-resource-list-internal class #'xt-get-constraint-resource-list
			      'constraint-resource))


(defclass xt-root-class (display-object)
  ((events :initform nil :accessor widget-event-handler-data)
   (callbacks :initform nil :accessor widget-callback-data)
   (window-cache :initform nil)
   (cleanups :initform nil :accessor widget-cleanup-functions))
  (:metaclass xt-class))

(defmethod add-widget-cleanup-function ((widget xt-root-class) fn &rest args)
  (push (list* fn args)
	(widget-cleanup-functions widget)))

(defmethod widget-display ((object xt-root-class))
  (object-display object))


(defclass rect (xt-root-class) ())
(defclass un-named-obj (rect) ())

(defclass basic-resource ()
  ((name :initarg :name :reader resource-name)
   (original-name :initarg :original-name :reader resource-original-name)
   (type :initarg :type :reader resource-type)
   (class :initarg :class :reader resource-class)))

(defmethod print-object ((r basic-resource) s)
  (print-unreadable-object (r s :type t :identity nil)
    (format s "~A,~A "
	    (if (slot-boundp r 'name)
		(resource-name r)
	      "..UNBOUND..")
	    (if (slot-boundp r 'type)
		(resource-type r)
	      "..UNBOUND.."))))

(defun lispify-resource-name (x) (lispify-tk-name x :package :keyword))
(defun lispify-resource-class (x) (lispify-tk-name x))
(defun lispify-resource-type (x) (lispify-tk-name x))


(defclass resource (basic-resource)
  ())
			 
(defclass constraint-resource (basic-resource)
  ())


(defun make-classes (classes)
  (let ((clist nil))
    
    (dolist (class-ep classes)
      #+ignore
      (format excl:*initial-terminal-io* ";; Initializing class ~s~%" class-ep)
      (let ((h (get-foreign-variable-value class-ep)))
	(push (list h class-ep) clist)
	(xt_initialize_widget_class h)))
    (setq clist (nreverse clist))

    (do ((cs clist (cdr cs))
	 r)
	((null cs)
	 (nreverse r))
      (destructuring-bind (handle class-ep) (car cs)
	(let ((class
	       (clos::ensure-class
		(lispify-class-name (widget-class-name handle))
		:direct-superclasses (list (if (zerop (xt-class-superclass handle))
					       'xt-root-class
					     (lispify-class-name
					      (widget-class-name
					       (xt-class-superclass handle)))))
		:direct-slots nil
		:metaclass 'xt-class
		:entry-point class-ep
		:foreign-address (get-foreign-variable-value class-ep))))
	  (register-address class)
	  (push class r))))))


(defun define-toolkit-classes (&rest classes)
  (make-classes (remove-duplicates 
		 (apply #'append classes)
		 :test #'string=)))
	
(defun widget-class-name (h)
  (char*-to-string (xt-class-name h)))

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
	(intern 
	 (ecase excl:*current-case-mode*
	   ((:case-sensitive-lower :case-insensitive-lower) string)
	   ((:case-sensitive-upper :case-insensitive-upper) (string-upcase string)))
	 package)
      string)))


(defun-c-callable toolkit-error-handler ((message :unsigned-long))
  (let ((*error-output* excl:*initial-terminal-io*))
    (error "Xt: ~a" (char*-to-string message))))

(defun-c-callable toolkit-warning-handler ((message :unsigned-long))
  (let ((*error-output* excl:*initial-terminal-io*))
    (warn "Xt: ~a" (char*-to-string message))))


;; This is a terrible hack used to compensate for bugs/inconsistencies
;; in the XM and OLIT toolkits.
(defun add-resource-to-class (class resource)
  (clos::map-over-subclasses
   #'(lambda (c)
       (pushnew resource (slot-value c 'specially-hacked-resources))
       (with-slots (cached-resources get-values-cache set-values-cache
				     cached-constraint-resources) c
	 (clrhash cached-resources)
	 (clrhash get-values-cache)
	 (clrhash set-values-cache)
	 (clrhash cached-constraint-resources)))
   class))
