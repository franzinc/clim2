;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

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
                       (present object (presentation-type presentation)
                                :stream stream :view +pointer-documentation-view+)))
     :gesture :select)
    (object presentation)
  (values object (presentation-type presentation)))

(defun identity-translator-applicable-p (presentation context-type)
  (let* ((type (presentation-type presentation))
         (type-name (presentation-type-name type))
         (object (presentation-object presentation)))
    (with-presentation-type-decoded (context-name) context-type
      (if (eq type-name 'blank-area)
          (or (eq context-name 'blank-area)
              (presentation-subtypep-1 type context-type))
        ;; Let MENU-ITEM-IDENTITY take care of pure menu items
        (unless (and (eq type-name 'menu-item)
                     (eq context-name 'menu-item))
          ;; Either the types definitely match, or the types nominally match
          ;; and the object must be validated.
          (or (presentation-subtypep-1 type context-type)
              (and
               ;; the new condition is stricter (cim 2/20/96) - see
               ;; spr14044
               #+ignore (not (null context-parameters))
               (presentation-subtypep-1 type-name context-name)
               (presentation-typep object context-type))))))))

(eval-when (compile eval load)
  ;; Only the PRESENTATION-MENU translator lives in this
  (unless (find-command-table 'presentation-menu-command-table :errorp nil)
    (make-command-table 'presentation-menu-command-table :inherit-from nil))
  )

;; If you change this, be sure to change the SETQ below
(define-presentation-action presentation-menu
    (t nil presentation-menu-command-table
     ;; You might think that there should be a tester that sees if there
     ;; are any applicable translators.  That's not necessary because
     ;; FIND-APPLICABLE-TRANSLATORS and PRESENTATION-MATCHES-CONTEXT-TYPE
     ;; treat this translator as a special case.
       :documentation ((frame stream)
                       (format stream
                               (frame-menu-translator-documentation frame)))
     :menu nil                                ;this doesn't go into any menu
     :gesture :menu)
    (presentation frame window x y)
  (call-presentation-menu presentation *input-context*
                          frame window x y
                          :for-menu t))

;; "the SETQ below"...
(setq *presentation-menu-translator*
      (find-presentation-translator 'presentation-menu 'presentation-menu-command-table))

(defun call-presentation-menu (presentation input-context frame window x y
                               &key (for-menu t) label (gesture :menu))
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
    (when translators                                ;just in case...
      (flet ((translator-documentation (translator-stuff stream)
               (setq translator-stuff (menu-item-value translator-stuff))
               (let* ((translator (pop translator-stuff))
                      (presentation (pop translator-stuff))
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
                                   :printer #'translator-documentation
                                   :gesture gesture)))
          (when chosen
            ;; Anticipate the day when we can move the mouse off the menu
            (let* ((translator (pop chosen))
                   (presentation (pop chosen))
                   (context-type (pop chosen))
                   (tag (pop chosen)))
              (multiple-value-bind (translated-object translated-type options)
                  (call-presentation-translator translator presentation context-type
                                                frame nil window x y)
                (throw tag (values translated-object
                                   (or translated-type context-type)
                                   nil                ;would be an event...
                                   options))))))))))

(defun quote-expression-if-necessary (object type)
  (if (and (not (presentation-subtypep-1 type 'form))
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
  (object)
  (let ((stream (fourth object)))
    (with-input-editor-help stream
      (display-history-contents (first object) stream
                                :start-index (second object) :string (third object)))))

(define-presentation-action yank-kill-ring-element
    (kill-ring-element input-editor global-command-table
     :gesture :select
     :documentation "Yank this item from the kill ring")
    (object context-type)
  (let ((history (first object))
        (element (cdr object)))
    (with-presentation-type-parameters (input-editor context-type)
      (history-replace-input history stream element)
      (queue-rescan stream ':activation))))


;; Display a menu of completions
(define-presentation-action menu-of-completions
    (blank-area completer global-command-table
     :gesture :menu
     :documentation "Menu of completions")
    (context-type window)
  (with-presentation-type-parameters (completer context-type)
    (multiple-value-bind (string success object nmatches possibilities)
        (funcall function prefix :possibilities)
      (declare (ignore string object success))
      (unless (or (= nmatches 0) (null possibilities))
        ;;--- Just using the first non-COMPLETER context type is far too simplistic
        (let ((type (evacuate-list
                      (input-context-type (second *input-context*))))
              (tag (input-context-tag (second *input-context*))))
          (labels ((print-possibility (possibility stream)
                     (cond (possibility-printer
                            (funcall possibility-printer possibility type stream))
                           (type
                            (present (second possibility) type :stream stream))
                           (t
                            (format stream "~A" (first possibility)))))
                   (menu-choose-body (stream presentation-type)
                     (declare (ignore presentation-type))
                     (formatting-item-list (stream :max-height
                                                   (* 0.95 (bounding-rectangle-height
                                                            (graft stream))))
                       (dolist (possibility possibilities)
                         (formatting-cell (stream)
                           (print-possibility possibility stream))))))
            (declare (dynamic-extent #'print-possibility #'menu-choose-body))
            (with-menu (menu window :label "Menu of completions")
              (with-end-of-line-action (menu :allow)
                (let ((object
                        (menu-choose-from-drawer menu type #'menu-choose-body)))
                  (when object
                    (throw tag (values object type nil nil))))))))))))
