;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM; Base: 10; Lowercase: Yes -*-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca.  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, Ca.  All rights reserved.
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
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;

;; $fiHeader: basic-translators.lisp,v 1.1 91/09/09 18:42:21 cer Exp Locker: cer $

(in-package :clim)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved."
"Copyright (c) 1991, Franz Inc. All rights reserved"

;; The translator that normally runs.
(define-presentation-translator identity
    (t nil global-command-table
     :tester ((presentation context-type)
	      (identity-translator-applicable-p presentation context-type))
     :tester-definitive t
     :documentation ((object presentation stream)
		     (let ((*print-length* 3)
			   (*print-level* 3)
			   (*print-pretty* nil))
		       (present object (presentation-type presentation) :stream stream)))
     :gesture :select)
    (object presentation)
  (values object (presentation-type presentation)))

(defun identity-translator-applicable-p (presentation context-type)
  (let* ((type (presentation-type presentation))
	 (type-name (presentation-type-name type))
	 (object (presentation-object presentation)))
    (with-presentation-type-decoded (context-name context-parameters) context-type
      (if (eq type-name 'blank-area)
	  (eq context-name 'blank-area)
	;; Let MENU-ITEM-IDENTITY take care of pure menu items
	(unless (and (eq type-name 'menu-item)
		     (eq context-name 'menu-item))
	  ;; Either the types definitely match, or the types nominally match
	  ;; and the object must be validated.
	  (or (presentation-subtypep type context-type)
	      (and (not (null context-parameters))
		   (presentation-subtypep type-name context-name)
		   (presentation-typep object context-type))))))))

(eval-when (compile load eval)
  (warn "You really want to remove this"))
(define-presentation-type menu-item ())

;; This translator is here to compute menu item documentation
(define-presentation-translator menu-item-identity
    (menu-item menu-item global-command-table
     :priority 1		;prefer this to IDENTITY
     :tester-definitive t
     :documentation ((object stream)
		     (let ((documentation (or (menu-item-documentation object)
					      (menu-item-display object))))
		       (write documentation :stream stream :escape nil)))
     :gesture :select)
    (object presentation)
  (values object (presentation-type presentation)))

;; Only the PRESENTATION-MENU translator lives in this
(make-command-table 'presentation-menu-command-table :inherit-from nil)

;; If you change this, be sure to change the SETQ below
(define-presentation-action presentation-menu
    (t nil presentation-menu-command-table
     ;; You might think that there should be a tester that sees if there
     ;; are any applicable translators.  That's not necessary because
     ;; FIND-APPLICABLE-TRANSLATORS and PRESENTATION-MATCHES-CONTEXT-TYPE
     ;; treat this translator as a special case.
     :documentation "Menu"
     :menu nil				;this doesn't go into any menu
     :gesture :menu)
    (presentation frame window x y)
  (call-presentation-menu presentation *input-context*
			  frame window x y
			  :for-menu t))

;; "the SETQ below"...
(setq *presentation-menu-translator*
      (find-presentation-translator 'presentation-menu 'presentation-menu-command-table))

(defun call-presentation-menu (presentation input-context frame window x y
			       &key (for-menu t) label)
  (let* ((translators (find-applicable-translators presentation input-context 
						   frame window x y
						   :event nil :for-menu for-menu))
	 ;; Yecch.  The problem is each "translator stuff" is a list, which
	 ;; is indistinguishable from the item syntax.  Oh well, we're already
	 ;; consing up stuff already...
	 (item-list
	   (and translators (map 'list
				 #'(lambda (thing)
				     (list thing :value thing))
				 translators))))
    (when translators				;just in case...
      (flet ((translator-documentation (translator-stuff stream)
	       (setq translator-stuff (menu-item-value translator-stuff))
	       (let* ((translator (pop translator-stuff))
		      (context-type (pop translator-stuff)))
		 (document-presentation-translator translator presentation context-type
						   frame nil window x y
						   :stream stream
						   ;; Run the body if need be
						   :documentation-type :from-body))))
	(declare (dynamic-extent #'translator-documentation))
	(let ((chosen (menu-choose item-list
				   :associated-window window
				   :label label
				   :printer #'translator-documentation)))
	  (when chosen
	    ;; Anticipate the day when we can move the mouse off the menu
	    (let* ((translator (pop chosen))
		   (context-type (pop chosen))
		   (tag (pop chosen)))
	      (multiple-value-bind (translated-object translated-type options)
		  (call-presentation-translator translator presentation context-type
						frame nil window x y)
		(throw tag (values translated-object
				   (or translated-type context-type)
				   nil		;would be an event...
				   options))))))))))

(defun quote-expression-if-necessary (object type)
  (if (and (not (presentation-subtypep type 'form))
	   (or (consp object)
	       (and (symbolp object)
		    (not (keywordp object))
		    (not (eq object nil))
		    (not (eq object t)))))
      (list 'quote object)
    object))

;; Quote all but self-evaluating things when translating them.
;; This translates everything, not just the ones needing quoting, since
;; the IDENTITY translator's tester always fails for EXPRESSION to FORM.
(define-presentation-translator expression-to-form
    (expression form global-command-table
     :documentation
       ((object presentation stream)
	(let ((*print-length* 3)
	      (*print-level* 3)
	      (*print-pretty* nil))
	  (present (quote-expression-if-necessary object (presentation-type presentation))
		   'expression :stream stream)))
     :gesture :select)
    (object presentation)
  (quote-expression-if-necessary object (presentation-type presentation)))

;; Display some more of a history display
(define-presentation-action display-rest-of-history
    (display-rest-of-history nil global-command-table
     :gesture :select
     :documentation "Display the rest of the history")
    (object window)
  (display-history-contents (first object) window
			    :start-index (second object) :string (third object)))
