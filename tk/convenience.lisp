;; -*- mode: common-lisp; package: tk -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; $Id: convenience.lisp,v 2.5 2004/01/16 19:15:44 layer Exp $

(in-package :tk)

#-(version>= 6 1)
(eval-when (eval compile)
  (defmacro define-convenience-class (class superclasses entry-point)
    (let ((c-function-name
	   (intern (substitute #\_ #\-
			       (lispify-tk-name entry-point :package nil)))))
      `(progn
	 (eval-when (eval compile)
	   (defforeign ',c-function-name
	       :entry-point (ff:convert-foreign-name ,entry-point)
	       :call-direct t
	       :arguments '(foreign-address foreign-address foreign-address fixnum)
	       :arg-checking nil
	       :return-type :foreign-address))
	 (defclass ,class ,superclasses
	   ()
	   (:metaclass xt-class))
	 (defmethod make-widget ((w ,class) name parent &rest args
				 &key (managed t) &allow-other-keys)
	   (remf args :managed)
	   (with-malloced-objects
	       (multiple-value-bind
		   (arglist n)
		   (make-arglist-for-class (find-class ',class)
					   parent
					   args)
	       (let ((o (,c-function-name
			 parent
			 (note-malloced-object (clim-utils:string-to-foreign name))
			 arglist
			 n)))
		 (when managed
		   (xt_manage_child o))
		 o))))))))
#+(version>= 6 1)
(eval-when (eval compile)
  (defmacro define-convenience-class (class superclasses entry-point)
    (let ((c-function-name
	   (intern (substitute #\_ #\-
			       (lispify-tk-name entry-point :package nil)))))
      `(progn
	 (eval-when (eval compile)
	   (def-foreign-call (,c-function-name ,entry-point)
	       ((w :foreign-address) (x :foreign-address) (y :foreign-address) (z :int fixnum))
	     :returning :foreign-address
	     :call-direct t
	     :arg-checking nil))
	 (defclass ,class ,superclasses
	   ()
	   (:metaclass xt-class))
	 (defmethod make-widget ((w ,class) name parent &rest args
				 &key (managed t) &allow-other-keys)
	   (remf args :managed)
	   (with-malloced-objects
	       (multiple-value-bind
		   (arglist n)
		   (make-arglist-for-class (find-class ',class)
					   parent
					   args)
	       (let ((o (,c-function-name
			 parent
			 (note-malloced-object (clim-utils:string-to-foreign name))
			 arglist
			 n)))
		 (when managed
		   (xt_manage_child o))
		 o))))))))

(define-convenience-class xm-menu-bar (xm-row-column) "XmCreateMenuBar")
(define-convenience-class xm-pulldown-menu (xm-row-column) "XmCreatePulldownMenu")
(define-convenience-class xm-radio-box (xm-row-column) "XmCreateRadioBox")
(define-convenience-class xm-popup-menu (xm-row-column) "XmCreatePopupMenu")

(define-convenience-class xm-option-menu (xm-row-column) "XmCreateOptionMenu")

(define-convenience-class xm-question-dialog (xm-message-box) "XmCreateQuestionDialog")
(define-convenience-class xm-warning-dialog (xm-message-box) "XmCreateWarningDialog")
(define-convenience-class xm-information-dialog (xm-message-box) "XmCreateInformationDialog")
(define-convenience-class xm-error-dialog (xm-message-box) "XmCreateErrorDialog")
(define-convenience-class xm-message-dialog (xm-message-box) "XmCreateMessageDialog")

(define-convenience-class xm-file-selection-dialog
    (xm-file-selection-box)
  "XmCreateFileSelectionDialog")


(define-convenience-class xm-working-dialog (xm-message-box) "XmCreateWorkingDialog")
