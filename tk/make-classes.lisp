;; -*- mode: common-lisp; package: tk -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :tk)

(defun get-foreign-variable-value (x)
  (let ((ep #-dlfcn (ff:get-extern-data-address x)
	    #+dlfcn (ff:get-entry-point
		     x
		     #-(version>= 5 0) :note-shared-library-references
		     #-(version>= 5 0) nil)))
    (unless ep (error "Entry point ~S not found" x))
    (class-array ep 0)))

;; This is only called to fill in the cache, so it can be (real) slow.
(defun get-resource-internal (class fn resource-class resource-name)
  (with-ref-par ((resources 0 *)
		 (n 0 :unsigned-int))
    (let ((tk-resource-name (tkify-lisp-name resource-name))
	  result)
      ;;--- Perhaps we can just do this when we want to grab the resources.
      ;;--- In this way we would only have to do one the display is
      ;;--- opened and the toolkit initialized etc etc.
      (xt_initialize_widget_class (class-handle class))
      (funcall fn (class-handle class) &resources &n)
      (let ((resources resources))
	(setq result
	  (dotimes (i n)
	    (let* ((res (xt-resource-list resources i))
		   (original-name (xt-resource-name res)))
	      (when (string-equal (excl:native-to-string original-name)
				  tk-resource-name)
		(let ((*package* (find-package :tk)))
		  (return
		    (make-instance resource-class
		      :original-name original-name
		      :name resource-name
		      :class (lispify-resource-class
			      (excl:native-to-string (xt-resource-class res)))
		      :type (lispify-resource-type
			     (excl:native-to-string (xt-resource-type res))))))))))
	(xt_free resources))
      result)))

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
  (with-ref-par ((resources 0 *)
		 (n 0 :unsigned-int))
    (funcall fn (class-handle class) &resources &n)
    (let ((resources resources)
	  (r nil))
      (dotimes (i n)
	(push (make-instance
		  resource-class
		:original-name (xt-resource-name (xt-resource-list resources i))
		:name (lispify-resource-name (excl:native-to-string (xt-resource-name (xt-resource-list resources i))))
		:class (lispify-resource-class
			(excl:native-to-string (xt-resource-class (xt-resource-list resources i))))
		:type (lispify-resource-type
		       (excl:native-to-string (xt-resource-type (xt-resource-list resources i)))))
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


(defclass rect (xt-root-class) () (:metaclass xt-class))
(defclass un-named-obj (rect) () (:metaclass xt-class))

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
	(push (list h class-ep) clist)))
    (setq clist (nreverse clist))

    (do ((cs clist (cdr cs))
	 r)
	((null cs)
	 (nreverse r))
      (destructuring-bind (handle class-ep) (car cs)
	#+ignore
	(format excl:*initial-terminal-io* ";; Initializing class ~s~%" class-ep)
	(let ((class
	       (clos:ensure-class
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
  (make-classes
   (mapcar #-rs6000 #'ff:convert-foreign-name
	   #+rs6000 #'identity
	   (remove-duplicates
	    (apply #'append classes)
	    :test #'string=))))

(defun widget-class-name (h)
  (values (excl:native-to-string (xt-class-name h))))

(defvar *widget-name-to-class-name-mapping*
    '(((list scrolling-list) ol-list)
      ((event-obj) event)
      ((control-area) control)))

(defun lispify-class-name (x)
  (let ((name (lispify-tk-name x)))
    (or (cadr (assoc name *widget-name-to-class-name-mapping*
		     :test #'member))
	name)))

(defun lispify-tk-name (string &key (package *package*))
  (let* ((len (length string))
	 (nbreaks 0))
    (declare (fixnum nbreaks len))

    ;; find the case breaks
    (dotimes (i len)
      (declare (fixnum i))
      (when (and (<= #.(char-code #\A)
		     (char-code (schar string i))
		     #.(char-code #\Z))
		 (not (zerop i)))
	(incf nbreaks)))

    ;; make a new string
    (let ((new (make-string (+ len nbreaks)))
	  (j 0)
	  ;; [bug21429]: fix bogus type declarations
	  (l 0)
	  (m 0)
	  (n 0))
      (declare (fixnum j l m n))

      (ecase excl:*current-case-mode*
	((:case-sensitive-lower :case-insensitive-lower)
	 (setq l #.(char-code #\A) m #.(char-code #\Z)
	       n  #.(- (char-code #\a) (char-code #\A))))
	((:case-sensitive-upper :case-insensitive-upper)
	 (setq l #.(char-code #\a) m #.(char-code #\z)
	       n  #.(- (char-code #\A) (char-code #\a)))))

      (macrolet ((push-char (c)
		   `(progn
		      (setf (schar new j) ,c)
		      (incf j))))
	(dotimes (i len)
	  (declare (fixnum i))
	  (let* ((c (schar string i))
		 (cc (char-code c)))
	    (declare (fixnum cc))
	    (when (and (<= #.(char-code #\A) cc #.(char-code #\Z))
		       (not (zerop i)))
	      (push-char #\-))
	    (push-char (if (<= l cc m)
			   (code-char (+ cc n))
			 c)))))
      (if package
	  (values
	   (intern new package))
	new))))

(defun tkify-lisp-name (name &key class)
  (setq name (string name))
  (let ((len (length name))
	(nbreaks 0))
    (declare (fixnum nbreaks len))

    ;; find the breaks
    (dotimes (i len)
      (declare (fixnum i))
      (when (eql (schar name i) #\-)
	(incf nbreaks)))

    ;; make a new string
    (let ((new (make-string (- len nbreaks)))
	  (j 0)
	  (break class))
      (declare (fixnum j))

      (macrolet ((push-char (c)
		   `(progn
		      (setf (schar new j) ,c)
		      (incf j))))
	(dotimes (i len)
	  (declare (fixnum i))
	  (let* ((c (schar name i))
		 (cc (char-code c)))
	    (declare (fixnum cc))
	    (if (eql c #\-)
		(setq break t)
	      (progn
		(push-char
		 (if break
		     (if (<= #.(char-code #\a) cc #.(char-code #\z))
			 (code-char (+ cc
				       #.(- (char-code #\A)
					    (char-code #\a))))
		       c)
		   (if (<= #.(char-code #\A) cc #.(char-code #\Z))
		       (code-char (+ cc
				     #.(- (char-code #\a)
					  (char-code #\A))))
		     c)))
		(setq break nil))))))
      new)))


(defun-foreign-callable toolkit-error-handler ((message :foreign-address))
  (let ((*error-output* excl:*initial-terminal-io*))
    (error "Xt: ~a" (excl:native-to-string message))))

(defun-foreign-callable toolkit-warning-handler ((message :foreign-address))
  (let ((*error-output* excl:*initial-terminal-io*))
    (warn "Xt: ~a" (excl:native-to-string message))))


;; This is a terrible hack used to compensate for bugs/inconsistencies
;; in the XM and OLIT toolkits.
(defun add-resource-to-class (class resource)
  (excl::map-over-subclasses
   #'(lambda (c)
       (pushnew resource (slot-value c 'specially-hacked-resources))
       (with-slots (cached-resources get-values-cache set-values-cache
				     cached-constraint-resources) c
	 (clrhash cached-resources)
	 (clrhash get-values-cache)
	 (clrhash set-values-cache)
	 (clrhash cached-constraint-resources)))
   class))

#+(version>= 5 0)
(defun fixup-class-entry-points ()
  (let ((root (find-class 'xt-root-class)))
    (excl::map-over-subclasses #'tk::unregister-address root)
    (excl::map-over-subclasses
     #'(lambda (class)
	 (let* ((old-addr (and (typep class 'xt-class)
			       (ff::foreign-pointer-address class)))
		(entry-point (and (typep class 'xt-class)
				  (slot-boundp class 'entry-point)
				  (slot-value class 'entry-point)))
		(new-addr (and entry-point
			       (get-foreign-variable-value entry-point))))
	   (when entry-point
	       (unless (equal old-addr new-addr)
		 (setf (ff::foreign-pointer-address class) new-addr))
	       (register-address class ))))
     root)))


#+(and (not (version>= 5 0)) dlfcn)
(defun fixup-class-entry-points ()
  (let ((root (find-class 'xt-root-class)))
    (excl::map-over-subclasses #'tk::unregister-address root)
    (excl::map-over-subclasses
     #'(lambda (class)
	 (let* ((old-addr (and (typep class 'xt-class)
			       (ff::foreign-pointer-address class)))
		(entry-point (and (typep class 'xt-class)
				  (slot-boundp class 'entry-point)
				  (slot-value class 'entry-point)))
		(new-addr (and entry-point
			       (get-foreign-variable-value entry-point))))
	   (when entry-point
	       (unless (equal old-addr new-addr)
		 (setf (ff::foreign-pointer-address class) new-addr))
	       (register-address class ))))
     root)))

