;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-1998 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; $Id: xm-widgets.lisp,v 1.30.34.1.14.1 2000/08/11 00:03:55 cley Exp $

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


(tk::add-resource-to-class (find-class 'xm-cascade-button-gadget)
			   (make-instance 'resource
					  :name :label-type
					  :type 'tk::label-type
					  :original-name
					  (excl:string-to-native
					   "labelType")))


;; Moved here as to be after loading xm-funs.

;;-- This is a problem cos we dont know the number of items

(defconstant xm-font-list-default-tag "FONTLIST_DEFAULT_TAG_STRING")

;; this needs to be made to deal with compound strings for ics.

(defmethod convert-resource-in ((parent t) (type (eql 'xm-string)) value)
  (and (not (zerop value))
       (with-ref-par ((string 0 *))
	 ;;--- I think we need to read the book about
	 ;;--- xm_string_get_l_to_r and make sure it works with multiple
	 ;;-- segment strings
	 (xm_string_get_l_to_r value xm-font-list-default-tag &string)
	 (values (excl:native-to-string string)))))

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
	      (return-from partition-compound-string))))))))

 (:-ics
  (defun partition-compound-string (s f &key (start 0) (end (length s)))
    ;;(declare (ignore s))
    (funcall f nil start end))))

(excl:ics-target-case
 (:+ics

  (defvar *empty-compound-string* nil)
  
  (eval-when (compile) (declaim (special *font-list-tags*)))

  (defmethod convert-resource-out ((parent t) (type (eql 'xm-string)) value)
    (let ((result nil))
      (flet ((extract-element (codeset start end)
	       (let* ((substring (subseq value start end))
		      (element (note-malloced-object
				(xm_string_create_l_to_r
				 (ecase codeset
				   ((0 2) (lisp-string-to-string8 substring))
				   ((1 3) (lisp-string-to-string16 substring)))
				 (svref *font-list-tags* codeset)))))
		 (setq result (note-malloced-object
			       (if result
				   (xm_string_concat result element)
				 (xm_string_copy element)))))))
	(declare (dynamic-extent #'extract))
	(partition-compound-string value #'extract-element)
	(or result
	    *empty-compound-string*
	    (setq *empty-compound-string*
	      (xm_string_create_l_to_r (clim-utils:string-to-foreign "")
				       (clim-utils:string-to-foreign xm-font-list-default-tag))))))))

 (:-ics
  (defmethod convert-resource-out ((parent t) (type (eql 'xm-string)) value)
    (note-malloced-object
     (xm_string_create_l_to_r
      (note-malloced-object (clim-utils:string-to-foreign value))
      (note-malloced-object (clim-utils:string-to-foreign "")))))))

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

