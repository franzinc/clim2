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
;; $fiHeader$

(in-package :tk)

					;
;
;
;(defmacro define-convenience-function (name entry-point class)
;  (let ((c-function-name (intern (substitute #\_ #\- (symbol-name name)))))
;    `(progn
;       (defforeign ',c-function-name
;	 :entry-point ,entry-point)
;       (defun ,name (parent name &rest arglist)
;	 (let ((arglist (make-arglist-for-class
;			 (find-class ',class)
;			 parent
;			 arglist)))
;	   (register-handle
;	    (make-instance
;	     ',class
;	     :display display
;	     :handle
;	     (,c-function-name
;	      (object-handle parent)
;	      (string-to-char* name)
;	      arglist
;	      (truncate (length arglist) 2)))))))))
;	
;
;(define-convenience-function create-menu-bar 
;			     "_XmCreateMenuBar"
;			     xm-row-column)
;
;(define-convenience-function create-pulldown-menu 
;			     "_XmCreatePulldownMenu" 
;			     xm-row-column)
;
;(define-convenience-function create-row-column
;			     "_XmCreateRowColumn"
;			     xm-row-column)
;
;
;(define-convenience-function create-radio-box
;			     "_XmCreateRadioBox"
;			     xm-row-column)

(defmacro define-convenience-class (class superclasses entry-point)
  (let ((c-function-name
	 (intern (substitute #\_ #\- 
			     (lispify-tk-name entry-point :package nil)))))
    `(progn
       (defforeign ',c-function-name :entry-point ,entry-point)
       (defclass ,class ,superclasses 
		 ()
	 (:metaclass xt-class))
       (defmethod make-widget ((w ,class) &rest args &key (managed t)
			       (name "") 
			       parent &allow-other-keys)
	 (remf :name args)
	 (remf :parent args)
	 (remf :managed args)
	 (let* ((arglist (make-arglist-for-class
			  (find-class ',class)
			  parent
			  args))
		(o 
		 (,c-function-name
		  (object-handle parent)
		  (string-to-char* name)
		  arglist
		  (truncate (length arglist) 2))))
	   (when managed
	     (manage_child o))
	   o)))))

(define-convenience-class xm-menu-bar (xm-row-column) "_XmCreateMenuBar")
(define-convenience-class xm-pulldown-menu (xm-row-column) "_XmCreatePulldownMenu")
(define-convenience-class xm-radio-box (xm-row-column) "_XmCreateRadioBox")
