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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $fiHeader: convenience.lisp,v 1.16 1993/10/26 03:22:18 colin Exp $

(in-package :tk)

(eval-when (eval compile)
  (defmacro define-convenience-class (class superclasses entry-point)
    (let ((c-function-name
	   (intern (substitute #\_ #\-
			       (lispify-tk-name entry-point :package nil)))))
      `(progn
	 (eval-when (eval compile)
	   (defforeign ',c-function-name
	       :entry-point (ff:convert-to-lang ,entry-point)
	       :call-direct t
	       :arguments '(foreign-address foreign-address foreign-address fixnum)
	       :arg-checking nil
	       :return-type :unsigned-integer))
	 (defclass ,class ,superclasses
	   ()
	   (:metaclass xt-class))
	 (defmethod make-widget ((w ,class) name parent &rest args
				 &key (managed t) &allow-other-keys)
	   (remf args :managed)
	   (with-malloced-objects
	       (let* ((arglist (make-arglist-for-class
				(find-class ',class)
				parent
				args))
		      (o
		       (,c-function-name
			parent
			(note-malloced-object (string-to-char* name))
			arglist
			(truncate (length arglist) 2))))
		 (when managed
		   (xt_manage_child o))
		 o)))))))

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
