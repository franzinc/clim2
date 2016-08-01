;; See the file LICENSE for the full license governing this code.
;;

(in-package :tk)

(defmethod make-widget ((w xm-gadget) name parent &rest args &key (managed t)
			&allow-other-keys)
  (remf args :managed)
  (let ((class (class-of w)))
    (if managed
	(apply #'create-managed-widget name class parent args)
      (apply #'create-widget name class parent args))))

(defmethod make-widget ((w xm-dialog-shell) name parent &rest args)
  (apply #'create-popup-shell name (class-of w) parent args))

#+ignore
(defmethod make-widget ((w override-shell) name parent &rest args)
  (apply #'create-popup-shell name (class-of w) parent args))

(defmethod make-widget ((w xm-menu-shell) name parent &rest args)
  (apply #'create-popup-shell name (class-of w) parent args))

(tk::add-resource-to-class (find-class 'vendor-shell)
			   (make-instance 'resource
					  :name :delete-response
					  :type 'tk::delete-response
					  :original-name
					  (excl:string-to-native
					   "deleteResponse")))

(tk::add-resource-to-class (find-class 'xm-text)
			   (make-instance 'resource
					  :name :font-list
					  :type 'font-list
					  :original-name
					  (excl:string-to-native
					   "fontList")))

(tk::add-resource-to-class (find-class 'vendor-shell)
			   (make-instance 'resource
					  :name :keyboard-focus-policy
					  :type 'tk::keyboard-focus-policy
					  :original-name
					  (excl:string-to-native
					   "keyboardFocusPolicy")))

(tk::add-resource-to-class (find-class 'vendor-shell)
                           (make-instance 'resource
                              :name :preedit-type
                              :type 'string
                              :original-name (excl:string-to-native
                                              "preeditType")))

(tk::add-resource-to-class (find-class 'xm-cascade-button-gadget)
			   (make-instance 'resource
					  :name :label-type
					  :type 'tk::label-type
					  :original-name
					  (excl:string-to-native
					   "labelType")))


;; Moved here as to be after loading xm-funs.

;;-- This is a problem cos we dont know the number of items

(defconstant xm-font-list-default-tag 
    ;; This must be converted to a native string before being passed
    ;; to any of the XmString functions.
    "FONTLIST_DEFAULT_TAG_STRING")

;;; Specialized function to destroy xm-strings, to support Motif2.1.
(defun destroy-generated-xm-string (string-pointer)
  (xm_string_free string-pointer)
  nil)

(defun destroy-generated-string (string-pointer)
  (clim-utils::system-free string-pointer)
  nil)

;; this needs to be made to deal with compound strings for ics.

(defmethod convert-resource-in ((parent t) (type (eql 'xm-string)) value)
  (when (not (zerop value))
    (multiple-value-prog1
        (with-ref-par ((string 0 *))
          ;;--- I think we need to read the book about
          ;;--- xm_string_get_l_to_r and make sure it works with multiple
          ;;-- segment strings
          (let ((result (xm_string_unparse value 0 0 tk:XmMULTIBYTE_TEXT 0 0 tk:XmOUTPUT_ALL)))
            (unwind-protect (excl:native-to-string result)
              (xt_free result))))
      (tk::add-widget-cleanup-function parent
                                       #'destroy-generated-xm-string
                                       value))))

(defmethod convert-resource-in ((parent t) (type (eql 'xm-string-table)) value)
  value)

#+broken
(defun convert-xm-string-table-in (parent table n)
  (let ((r nil))
    (dotimes (i n (nreverse r))
      (push (convert-resource-in parent 'xm-string (x-arglist table i))
	    r))))

(excl:ics-target-case
 (:+ics
  
  (defvar *empty-compound-string* nil)

  (eval-when (compile) (declaim (special *font-list-tags*)))
  
  (defun partition-compound-string (s f &key (start 0) (end (length s)))
    (unless (eql (length s) 0)
      (let ((c 0)
            (index start))
        (loop
          (setq c (clim-utils:char-character-set-and-index (char s index)))
          (let* ((break (position-if-not
                         #'(lambda (x)
                             (eq (clim-utils:char-character-set-and-index x) c))
                         s :start index :end end)))
            (funcall f c index (or break end))
            (if break
                (setq index break)
                (return-from partition-compound-string)))))))
  
  (defmethod convert-resource-out ((parent t) (type (eql 'xm-string)) value)
    (let ((result nil))
      (flet ((extract-element (codeset start end)
               (let* ((substring (subseq value start end))
                      (element (xm_string_create_l_to_r
                                (ecase codeset
                                  ((0 2) (lisp-string-to-string8 substring))
                                  ((1 3) (lisp-string-to-string16 substring)))
                                (aref *font-list-tags* codeset))))
                 (tk::add-widget-cleanup-function parent
                                                  #'destroy-generated-xm-string
                                                  element)
                 (let ((temp (if result
                                 (xm_string_concat result element)
                                 (xm_string_copy element))))
                   (tk::add-widget-cleanup-function parent
                                                    #'destroy-generated-xm-string
                                                    temp)
                   (setq result temp)))))
        (declare (dynamic-extent #'extract))
        (partition-compound-string value #'extract-element)
        (or result
            *empty-compound-string*
            (setq *empty-compound-string*
                  (xm_string_create_l_to_r (clim-utils:string-to-foreign "")
                                           (clim-utils:string-to-foreign 
                                            xm-font-list-default-tag))))))
    ;; this would be nice instead, but isn't happening anytime soon (spr30362):
    #+xm-string-supports-utf-8 
    (let ((native-string (clim-utils:string-to-foreign value)))
      (let ((xm-string (xm_string_create_localized native-string)))
        (tk::add-widget-cleanup-function parent #'destroy-generated-xm-string
                                         xm-string)
        xm-string)
      )))

 (:-ics

  (defun partition-compound-string (s f &key (start 0) (end (length s)))
    ;;(declare (ignore s))
    (funcall f nil start end))

  (defmethod convert-resource-out ((parent t) (type (eql 'xm-string)) value)
    (let ((s1 (clim-utils:string-to-foreign value))
	  (s2 (clim-utils:string-to-foreign "")))
      (tk::add-widget-cleanup-function parent
				       #'destroy-generated-string
				       s1)
      (tk::add-widget-cleanup-function parent
				       #'destroy-generated-string
				       s2)
      (let ((temp (xm_string_create_l_to_r
		   s1
		   s2)))
	(tk::add-widget-cleanup-function parent
					 #'destroy-generated-xm-string
					 temp) 
	temp))
    )))

(defmethod convert-resource-out ((parent t) (type (eql 'xm-background-pixmap)) value)
  (etypecase value
    (pixmap
     (encode-pixmap nil value))))

(defun encode-box-child (child)
  (let ((x (getf '(
                   :none                  0
                   :apply         1
                   :cancel    2
                   :default   3
                   :ok        4
                   :filter-label     5
                   :filter-text      6
                   :help      7
                   :list                  8
                   :history-list     :list
                   :list-label    9
                   :message-label    10
                   :selection-label  11
                   :prompt-label     :selection-label
                   :symbol-label     12
                   :text                  13
                   :value-text       :text
                   :command-text     :text
                   :separator             14
                   :dir-list         15
                   :dir-list-label   16
                   :file-list        :list
                   :file-list-label  :list-label
                   )
                 child)))
    (cond ((null x)
           (error "cannot encode child ~S" child))
          ((symbolp x)
           (encode-box-child x))
          (t x))))

(defmethod convert-resource-out ((parent t) (type (eql 'default-button-type)) value)
  (encode-box-child value))

;; JPM: Use excl:string-to-native at compile time for strings that will
;; never get freed, use string-to-foreign at run time for strings
;; that will get freed.

(tk::add-resource-to-class (find-class 'xm-text)
			   (make-instance 'resource
					  :name :scroll-horizontal
					  :type 'tk::boolean
					  :original-name
					  (excl:string-to-native
					   "scrollHorizontal")))

(tk::add-resource-to-class (find-class 'xm-text)
			   (make-instance 'resource
					  :name :scroll-vertical
					  :type 'tk::boolean
					  :original-name
					  (excl:string-to-native
					   "scrollVertical")))

(tk::add-resource-to-class (find-class 'xm-text)
			   (make-instance 'resource
					  :name :word-wrap
					  :type 'tk::boolean
					  :original-name
					  (excl:string-to-native
					   "wordWrap")))

(defun make-xm-string-table (&key (number 1))
  (clim-utils::allocate-cstruct 'xm-string-table
				:number number :initialize t))

(defmethod convert-resource-out ((parent t) (type (eql 'xm-string-table)) value)
  (if value
      (do* ((n (length value))
	    (r (note-malloced-object
		(make-xm-string-table :number n)))
	    (v value (cdr v))
	    (i 0 (1+ i)))
	  ((null v)
	   r)
	(setf (xm-string-table r i)
	  (convert-resource-out parent 'xm-string (car v))))
    0))

(defmethod convert-pixmap-out (parent (value string))
  (let* ((display (widget-display parent))
	 (screen (x11:xdefaultscreenofdisplay display))
	 (white (x11::xwhitepixel display 0))
	 (black (x11::xblackpixel display 0)))
    (xm_get_pixmap screen value white black)))

#+ignore
(defmethod convert-resource-out ((parent t) (type (eql 'text-position)) value)
  (error 'foo)
  value)

#+ignore				; now in resources...
(defmethod convert-resource-in ((parent t) (type (eql 'text-position)) value)
  (error 'foo)
  value)

(defvar *lookup-string-buffers* nil)

(defvar *lookup-string-buffer-size* 256)

(defmacro with-lookup-string-buffer ((var) &body body)
  `(let ((,var (or (excl:without-interrupts (pop *lookup-string-buffers*))
                   (excl::aclmalloc *lookup-string-buffer-size*))))
     (multiple-value-prog1
       (progn ,@body)
       (excl:without-interrupts (push buffer *lookup-string-buffers*)))))

(defun lookup-multibyte-string (event widget)
  (declare (optimize (speed 3) (safety 0)))
  (with-lookup-string-buffer (buffer)
    (with-ref-par ((keysym 0 :unsigned-long)
                   (status 0 :int))
      (let* ((nbytes (xm_im_mb_lookup_string widget event buffer
                                             *lookup-string-buffer-size*
                                             &keysym &status)))
        (declare (fixnum nbytes))
        (cond
          ((= status x11::XBufferOverflow)
           (warn "Too small buffer looking up the key event at size ~A."
                 *lookup-string-buffer-size*)
           (let ((*lookup-string-buffer-size* nbytes))
             (lookup-multibyte-string event widget)))
          ((= status x11::XLookupNone)
           (values nil nil))
          (t
           (let ((keysymp (or (= status x11::XLookupKeyBoth)
                              (= status x11::XLookupKeySym)))
                 (charsp (or (= status x11::XLookupKeyBoth)
                             (= status x11::XLookupChars))))
             (values (and charsp (excl:native-to-string buffer :length nbytes))
                     (and keysymp keysym)))))))))
