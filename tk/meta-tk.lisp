(in-package :tk)


(defclass xt-class (handle-class standard-class)
  (
   (handle :initform nil)
   (entry-point :initarg :entry-point :reader class-entry-point)
   (resources :initform nil :initarg :resources :reader class-resources)
   (constraint-resources :initform nil
			 :initarg :constraints
			 :reader class-constraint-resources)
   
   (direct-resources :initform nil :initarg :direct-resources :reader class-direct-resources)
   (constraint-resources :initform nil
			 :initarg :direct-constraints
			 :reader class-direct-constraint-resources)))


(defmethod class-handle ((class xt-class))
  (unless (clos::class-finalized-p class)
    (clos::finalize-inheritance class))
  (dolist (c (clos::class-precedence-list class) 
	     (error "Cannot get handle for class" class))
    (when (and (typep c 'xt-class)
	       (object-handle c))
      (return (object-handle c)))))

(defmethod clos::finalize-inheritance :after ((class xt-class))
  (let ((super (car (clos::class-direct-superclasses class))))
    (when (typep super 'xt-class)
      (unless (slot-value class 'resources)
	(setf (slot-value class 'resources)
	      (slot-value super 'resources)))
      (unless (slot-value class 'constraint-resources)
	(setf (slot-value class 'constraint-resources)
	  (slot-value super 'constraint-resources))))))
