;; -*- mode: common-lisp; package: tk -*-
;;
;;				-[Mon Jul 19 18:51:48 1993 by colin]-
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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Header: /repo/cvs.copy/clim2/tk/widget.lisp,v 1.43 1997/02/05 01:53:11 tomj Exp $

(in-package :tk)

(defun app-create-shell (&rest args
			 &key (application-name "clim")
			      (application-class "Clim")
			      (widget-class (error "Class not specified"))
			      (display (error "Display not specified"))
			 &allow-other-keys)
  (with-malloced-objects
      (let* ((class (find-class widget-class))
	     (handle (class-handle class)))
	(multiple-value-bind (arglist n)
	    (make-arglist-for-class class nil args)
	  (register-address
	   (apply #'make-instance
		  class
		  :foreign-address
		  (xt_app_create_shell (lisp-string-to-string8 application-name)
				       (lisp-string-to-string8 application-class)
				       handle
				       display
				       arglist
				       n)
		  :display display
		  args))))))

;; These are so we don't need the foreign functions at run time.

(defvar *widget-count* 0)

(defun xt-create-widget (name class parent args num-args)
  (incf *widget-count*)
  (xt_create_widget name class parent args num-args))

(defun xt-create-managed-widget (name class parent args num-args)
  (incf *widget-count*)
  (xt_create_managed_widget name class parent args num-args))

(defun create-widget (name widget-class parent &rest args)
  (apply #'create-widget-1
	 #'xt-create-widget name widget-class parent
	 args))

(defun create-managed-widget (name widget-class parent &rest args)
  (apply #'create-widget-1
	 #'xt-create-managed-widget name widget-class parent
	 args))

(defun create-widget-1 (fn name widget-class parent &rest args)
  (assert parent)
  (with-malloced-objects
      (let* ((class (find-class-maybe widget-class))
	     (handle (class-handle class)))
	(multiple-value-bind (arglist n)
	    (make-arglist-for-class class parent args)
	  (funcall fn
		   (note-malloced-object (string-to-char* name))
		   handle
		   parent
		   arglist
		   n)))))

(defun realize-widget (widget)
  (xt_realize_widget widget))

(defun manage-child (child)
  (xt_manage_child child))

(defun unmanage-child (child)
  (xt_unmanage_child child))

(defun is-managed-p (widget)
    (not (zerop (xt_is_managed widget))))

(defun manage-children (children)
  (let ((n (length children))
	(i 0))
    (with-*-array (v n)
      (dotimes (i n)
	(setf (*-array v i)
	  (ff:foreign-pointer-address (pop children))))
      (xt_manage_children v n))))

(defun unmanage-children (children)
  (let ((n (length children))
	(i 0))
    (with-*-array (v n)
      (dotimes (i n)
	(setf (*-array v i)
	  (ff:foreign-pointer-address (pop children))))
      (xt_unmanage_children v n))))

(defun destroy-widget (widget)
  (xt_destroy_widget widget))

(defun popup (shell)
       (xt_popup shell 0))

(defun popdown (shell)
       (xt_popdown shell))

(defun create-popup-shell (name widget-class parent &rest args)
  (with-malloced-objects
      (let* ((class (find-class-maybe widget-class))
	     (handle (class-handle class)))
	(multiple-value-bind (arglist n)
	    (make-arglist-for-class class parent args)
	  (incf *widget-count*)
	  (xt_create_popup_shell (note-malloced-object (string-to-char* name))
				 handle
				 parent
				 arglist
				 n)))))

(defun find-class-maybe (x)
  (if (typep x 'clos::class) x
    (find-class x)))

(defmethod widget-window (widget &optional (errorp t) peek)
  (with-slots (window-cache) widget
    (or window-cache
	(and (not peek)
	     (setf window-cache
	       (let ((id (xt_window widget)))
		 (if (zerop id)
		     (and errorp
			  (error "Invalid window id ~D for ~S" id widget))
		   (intern-object-xid
		    id
		    'window
		    (widget-display widget)
		    :display (widget-display widget)))))))))

(defun widget-class-of (x)
  (intern-widget-class
   (xt-widget-widget-class x)))

(defun intern-widget-class (class)
  (find-object-from-address class))


(defmethod initialize-instance :after ((w xt-root-class)
				       &rest args
				       &key foreign-address
					    name parent display
				       &allow-other-keys)
  (when (or display parent)
    (setf (slot-value w 'display)
      (or display
	  (object-display parent))))
  (unless foreign-address
    (register-widget
     w
     (progn
       (remf args :foreign-address)
       (remf args :name)
       (remf args :parent)
       (setf (foreign-pointer-address w)
	 (apply #'make-widget w (tkify-lisp-name name) parent args))))))


(defmethod destroy-widget-cleanup ((widget xt-root-class))
  (dolist (cleanup (widget-cleanup-functions widget))
    (apply (car cleanup) (cdr cleanup)))
  ;;--- When we start using gadgets things will be fun!
  (let ((w (widget-window widget nil t)))
    (when w (unregister-xid w (object-display widget))))
  (unintern-widget widget))

(defun intern-widget (widget-address &rest args)
  (unless (zerop widget-address)
    (multiple-value-bind
	(widget newp)
	(apply
	 #'intern-object-address
	 widget-address
	 (widget-class-of widget-address)
	 args)
      (when newp
	(add-callback widget :destroy-callback #'destroy-widget-cleanup))
      widget)))

(defun register-widget (widget &optional (handle (foreign-pointer-address widget)))
  (register-address widget handle)
  (add-callback widget :destroy-callback #'destroy-widget-cleanup))

(defun unintern-widget (widget)
  (unintern-object-address (foreign-pointer-address widget)))

(defmethod widget-parent ((widget xt-root-class))
  (let ((x (xt_parent widget)))
    (and (not (zerop x)) (intern-widget x))))


(defconstant xt-geometry-yes 0)
(defconstant xt-geometry-no 1)
(defconstant xt-geometry-almost 2)
(defconstant xt-geometry-done 3)



(defmethod widget-best-geometry (widget &key width height)
  (let ((preferred (make-xt-widget-geometry)))
    (xt_query_geometry
     widget
     (if (or width height)
	 (let ((x (make-xt-widget-geometry)))
	   (when width
	     (setf (xt-widget-geometry-width x) (round width)))
	   (when height
	     (setf (xt-widget-geometry-height x) (round height)))
	   (setf (xt-widget-geometry-request-mode x)
	     (logior
	      (if width x11:cwwidth 0)
	      (if height x11:cwheight 0)))
	   x)
	   0)
     preferred)
    (let ((r (xt-widget-geometry-request-mode preferred)))
      (values
       (xt-widget-geometry-x preferred)
       (xt-widget-geometry-x preferred)
       (xt-widget-geometry-width preferred)
       (xt-widget-geometry-height preferred)
       (xt-widget-geometry-border-width preferred)
       (logtest r x11:cwx)
       (logtest r x11:cwy)
       (logtest r x11:cwwidth)
       (logtest r x11:cwheight)
       (logtest r x11:cwborderwidth)))))

;;--- Should call either XtResizeWidget or XtConfigureWidget

(defun tk::configure-widget (widget &key x y width height
					 (border-width
					  (get-values widget :border-width)))
  (xt_configure_widget widget x y width height
		       border-width))


(defun describe-widget (w)
  (flet ((describe-resources (resources)
	   (dolist (r resources)
	     (format t "~S : ~S~%"
		     (resource-name r)
		     (handler-case
			 (get-values w (intern (resource-name r) :keyword))
		       (error (c) c "Get-values failed!"))))))
    (describe-resources (class-resources (class-of w)))
    (when (tk::widget-parent w)
      (describe-resources (class-constraint-resources (class-of (tk::widget-parent w)))))))

(defun set-sensitive (widget value)
  (xt_set_sensitive widget (if value 1 0)))

(excl:ics-target-case
 (:+ics
  (defun setlocale (&optional (category 0) locale)
    (let ((r (setlocale-1 category (or (and locale
					    (ff:string-to-char* locale))
				       0))))
      (unless (zerop r)
	(ff:char*-to-string r))))))

;; Could not think of anywhere better!

(defparameter *fallback-resources*
    `("clim*dragInitiatorProtocolStyle: DRAG_NONE"
      "clim*dragreceiverprotocolstyle:	DRAG_NONE"
      ,@(excl:ics-target-case
	 (:+ics '("clim*xnlLanguage: japanese"))))
  "A list of resource specification strings")

;; note that this is different from xt-initialize which calls
;; XtToolkitInitialize. This is closer to XtAppInitialize

(defun initialize-toolkit (&rest args)
  (let ((context (create-application-context)))
    (when *fallback-resources*
      (xt_app_set_fallback_resources
       context
       (let ((n (length *fallback-resources*)))
	 (with-*-array (v (1+ n))
	   (dotimes (i n)
	     (setf (*-array v i)
	       (ff:string-to-char* (nth i *fallback-resources*))))
	   (setf (*-array v n) 0)
	   v))))
    (excl:ics-target-case
     (:+ics (xt_set_language_proc context 0 0)))
    (let* ((display (apply #'make-instance 'display
			   :context context
			   args))
	   (app (apply #'app-create-shell
		       :display display
		       :widget-class 'application-shell
		       args)))
      (values context display app))))

(defun xt-name (w) (char*-to-string (xt_name w)))
(defun xt-class (w) (char*-to-string (xt-class-name (xt_class w))))

(defun widget-resource-name-and-class (w)
  (do* ((names nil (cons (xt-name w) names))
	(classes nil (cons (xt-class w) classes))
	(w w parent)
	(parent (widget-parent w) (widget-parent w)))
      ((null parent)
       (multiple-value-bind (app-name app-class)
	   (get-application-name-and-class (widget-display w))
	 (values (cons app-name names)
		 (cons app-class classes))))))
