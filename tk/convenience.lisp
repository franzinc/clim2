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
;; $fiHeader: convenience.lisp,v 1.6 92/03/30 17:51:29 cer Exp Locker: cer $

(in-package :tk)

					;
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
		  parent
		  (string-to-char* name)
		  arglist
		  (truncate (length arglist) 2))))
	   (when managed
	     (manage_child o))
	   o)))))

(define-convenience-class xm-menu-bar (xm-row-column) "_XmCreateMenuBar")
(define-convenience-class xm-pulldown-menu (xm-row-column) "_XmCreatePulldownMenu")
(define-convenience-class xm-radio-box (xm-row-column) "_XmCreateRadioBox")
(define-convenience-class xm-popup-menu (xm-row-column) "_XmCreatePopupMenu")

(define-convenience-class xm-option-menu (xm-row-column) "_XmCreateOptionMenu")

(define-convenience-class xm-question-dialog (xm-message-box) "_XmCreateQuestionDialog")
(define-convenience-class xm-warning-dialog (xm-message-box) "_XmCreateWarningDialog")
(define-convenience-class xm-information-dialog (xm-message-box) "_XmCreateInformationDialog")
(define-convenience-class xm-error-dialog (xm-message-box) "_XmCreateErrorDialog")
