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
;; $fiHeader: widget.cl,v 1.7 92/01/17 17:49:22 cer Exp $

(in-package :tk)

(defun app-create-shell (&rest args
			       &key (application-name 0)
			       (application-class 0)
			       (widget-class (error "Class not specified"))
			       (display (error "Display not specifie"))
			       &allow-other-keys)
  (let* ((class (find-class widget-class))
	 (handle (class-handle class))
	 (arglist (make-arglist-for-class class nil args)))
    (register-address
     (apply #'make-instance
	    class
	    :handle
	    (app_create_shell application-name
			      application-class
			      handle
			      (display-handle display)
			      arglist
			      (truncate (length arglist) 2))
	    :display display
	    args))))

(defforeign 'create_widget :entry-point "_XtCreateWidget")
(defforeign 'create_managed_widget :entry-point "_XtCreateManagedWidget")

(defun create-widget (name widget-class parent &rest args)
  (apply #'create-widget-1 
	 #'create_widget name widget-class parent 
	 args))

(defun create-managed-widget (name widget-class parent &rest args)
  (apply #'create-widget-1 
	 #'create_managed_widget name widget-class parent 
	 args))


(defun create-widget-1 (fn name widget-class parent &rest args)
  (let* ((class (find-class-maybe widget-class))
	 (handle (class-handle class))
	 (arglist (make-arglist-for-class class parent args)))
    (funcall fn (string-to-char* name )
	      (class-handle class)
	      (object-handle parent)
	      arglist
	      (truncate (length arglist) 2))))

(defforeign 'realize_widget 
  :entry-point "_XtRealizeWidget")

(defun realize-widget (widget)
  (realize_widget (object-handle widget)))

(defforeign 'manage_child
    :entry-point "_XtManageChild")

(defun manage-child (child)
  (manage_child (object-handle child)))


(defforeign 'unmanage_child
    :entry-point "_XtUnmanageChild")

(defun unmanage-child (child)
  (unmanage_child (object-handle child)))

(defforeign 'manage_children
    :entry-point "_XtManageChildren")

(defun manage-children (children)
  (manage_children (map '(simple-array (signed-byte 32))
		     #'object-handle 
		     children)
		   (length children)))
		     

(defforeign 'destroy_widget
    :entry-point "_XtDestroyWidget")

(defun destroy-widget (widget)
  (destroy_widget (object-handle widget)))

(defforeign 'create_popup_shell 
    :entry-point "_XtCreatePopupShell")

(defforeign '_popup
    :entry-point "_XtPopup")

(defforeign '_popdown
    :entry-point "_XtPopdown")

(defun popup (shell)
       (_popup (object-handle shell)
	       0))

(defun popdown (shell)
       (_popdown (object-handle shell)))

(defun create-popup-shell (name widget-class parent &rest args)
  (let* ((class (find-class-maybe widget-class))
	 (handle (class-handle class))
	 (arglist (make-arglist-for-class class parent args)))
    (create_popup_shell
	     (string-to-char* name)
	     handle
	     (object-handle parent)
	     arglist
	     (truncate (length arglist) 2))))

(defun find-class-maybe (x)
  (if (typep x 'clos::class) x
    (find-class x)))

(defforeign 'xt_window :entry-point "_XtWindow")

(defmethod widget-window (widget &optional (errorp t))
  (with-slots (window-cache) widget
    (or window-cache
	(setf window-cache
	  (let ((id (xt_window (object-handle widget))))
	    (if (zerop id)
		(and errorp
		     (error "Invalid window id ~D for ~S" id widget))
	      (intern-object-xid
	       id
	       'window 
	       :display (widget-display widget))))))))

(defun make-clx-window (display widget)
  (let* ((window-id (xt_window (object-handle widget))))
    (make-clx-window-from-id display window-id)))

(defun widget-class-of (x)
  (intern-widget-class
   (xtk-widget-widget-class x)))

(defun intern-widget-class (class)
  (find-object-from-address class))


(defmethod initialize-instance :after ((w xt-root-class) 
				       &rest args 
				       &key handle 
				       parent display
				       &allow-other-keys)
  (when (or display parent)
    (setf (slot-value w 'display)
      (or display
	  (object-display parent))))
  (unless handle
    (register-address
     w
     (progn
       (remf :handle args)
       (setf (slot-value w 'handle)
	 (apply #'make-widget w args))))))

(defforeign 'xt_parent 
    :entry-point "_XtParent")

(defun intern-widget (widget &rest args)
  (and (not (zerop widget))
       (apply
	#'intern-object-address 
	widget 
	(widget-class-of widget)
	args)))

(defmethod widget-parent (widget)
  (let ((x (xt_parent (object-handle widget))))
    (and (not (zerop x)) (intern-widget x))))

(defforeign 'xt_query_geometry :entry-point "_XtQueryGeometry")

(def-c-type xt-geometry-mask :unsigned-int)
(def-c-type xt-position :short)
(def-c-type xt-dimension :unsigned-short)

(def-c-type (xt-widget-geometry :in-foreign-space) :struct
  (request-mode xt-geometry-mask)
  (x xt-position)
  (y xt-position)
  (width xt-dimension)
  (height xt-dimension)
  (border-width xt-dimension)
  (sibling xtk-widget)
  (stack-mode :int))

(defconstant xt-geometry-yes 0)
(defconstant xt-geometry-no 1)
(defconstant xt-geometry-almost 2)
(defconstant xt-geometry-done 3)



(defmethod widget-best-geometry (widget &key width height)
  (let ((preferred (make-xt-widget-geometry)))
    (xt_query_geometry
     (object-handle widget)
     (if (or width height)
	 (let ((x (make-xt-widget-geometry)))
	   (when width
	     (setf (xt-widget-geometry-width x) width))
	   (when height
	     (setf (xt-widget-geometry-height x) height))
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


