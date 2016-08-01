;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;;; Presentation Translators.

(defvar *translators-cache-size* 25)
(defvar *translators-cache-tick* 0)

;;--- This should be more incremental, but that costs in paging time
(defun invalidate-presentation-caches (class)
  (declare (ignore class))
  (incf *translators-cache-tick*))

#+Genera
(setq clos-internals::*invalidate-clim-handler-caches* #'invalidate-presentation-caches)

;;; maybe this wants to be taken out after the load-only phase?
(eval-when (compile load #+(or aclpc acl86win32) eval)

(defvar *translator-function-arglist*
        '(object presentation context-type frame event window x y)))

(defclass presentation-translator ()
     ((name      :initarg :name
                 :reader presentation-translator-name)
      (function  :initform nil :initarg :function
                 :reader presentation-translator-function)
      (tester    :initform nil :initarg :tester
                 :reader presentation-translator-tester)
      (from-type :initarg :from-type
                 :reader presentation-translator-from-type)
      (to-type   :initarg :to-type
                 :reader presentation-translator-to-type)
      (gesture   :initarg :gesture-name
                 :reader presentation-translator-gesture-name)
      (menu      :initform nil :initarg :menu
                 :reader presentation-translator-menu)
      (documentation :initform nil :initarg :documentation
                     :reader presentation-translator-documentation)
      (pointer-documentation :initform nil :initarg :pointer-documentation
                             :reader presentation-translator-pointer-documentation)
      (priority :initform 0 :initarg :priority
                :reader presentation-translator-priority)
      (tester-definitive :initform t :initarg :tester-definitive
                         :reader presentation-translator-tester-definitive)))

(defun-inline presentation-translator-p (object)
  (typep object 'presentation-translator))

(defclass presentation-action (presentation-translator) ())

(defun-inline presentation-action-p (object)
  (typep object 'presentation-action))

(defmethod presentation-translator-command-name ((translator presentation-translator))
  nil)

(defmethod print-object ((object presentation-translator) stream)
  (if *print-escape*
      (print-unreadable-object (object stream :type t :identity t)
        (with-slots (name from-type to-type) object
          (format stream "~S (~S -> ~S)" name from-type to-type)))
      (print-unreadable-object (object stream :type t :identity t)
        (with-slots (name from-type to-type) object
          (format stream "~S" name)))))

;; The :GESTURE in the list of options might better be called :GESTURE-NAME,
;; but that seems a bit compulsive.  The EVENT argument in the arglist is
;; the event object corresponding to the user's gesture.
(defmacro define-presentation-translator
          (name
           (from-type to-type command-table
            &key (gesture ':select) tester tester-definitive
                 documentation pointer-documentation
                 (menu t) priority)
           arglist
           &body body)
  #+Genera (declare (zwei:indentation 1 3 3 1))
  `(define-presentation-translator-1 ,name
       (,from-type ,to-type ,command-table
        :gesture ,gesture
        :tester ,tester
        :documentation ,documentation
        :pointer-documentation ,pointer-documentation
        :menu ,menu
        :priority ,priority
        :tester-definitive ,tester-definitive)
       ,arglist
     ,@body))

#+Genera
(progn
  (scl:defprop define-presentation-translator "CLIM Presentation Translator"
               si:definition-type-name)
  (scl:defprop define-presentation-translator remove-presentation-translator
               zwei:kill-definition))

(defmacro define-presentation-action
          (name
           (from-type to-type command-table
            &key (gesture ':select) tester
                 documentation pointer-documentation
                 (menu t) priority)
           arglist
           &body body &environment env)
  #+Genera (declare (zwei:indentation 1 3 3 1))
  (with-warnings-for-definition name define-presentation-translator
    (multiple-value-bind (doc-string declarations body)
        (extract-declarations body env)
      (when body
        (setq body `((progn ,@body))))
      (setq body `(,@declarations
                   ,doc-string
                   ;; The body is run for its side-effects only
                   (throw 'no-translation ,@body)))
      `(define-presentation-translator-1 ,name
           (,from-type ,to-type ,command-table
            :gesture ,gesture
            :tester ,tester
            :documentation ,documentation
            :pointer-documentation ,pointer-documentation
            :menu ,menu
            :priority ,priority
            :tester-definitive t
            :translator-class presentation-action)
           ,arglist
         ,@body))))

(defun warn-if-presentation-type-specifier-invalid-1 (type env complain)
  (declare (dynamic-extent complain))
  ;; Don't call expand-presentation-type-abbreviation because it signals an
  ;; error if the result is not a valid presentation type specifier
  (loop (multiple-value-bind (expansion expanded)
            (expand-presentation-type-abbreviation-1 type env)
          (if expanded
              (setq type expansion)
              (return))))
  (with-presentation-type-decoded (type-name parameters) type
    (let ((class (cond ((symbolp type-name)
                        (find-presentation-type-class type-name nil env))
                       ((acceptable-presentation-type-class type-name)
                        type-name)
                       (t nil))))
      (cond ((null class)
             (funcall complain type-name "not a defined presentation type name"))
            ;; Recurse on presentation types that take presentation type arguments
            ;; so as to give a better error message in the usual case.  If we
            ;; don't find anything wrong, call presentation-type-specifier-p.
            ((and (member type-name *presentation-type-parameters-are-types*)
                  (some #'(lambda (type)
                            (with-presentation-type-decoded (type-name) type
                              (unless (member type-name '(not satisfies))
                                (warn-if-presentation-type-specifier-invalid-1
                                  type env complain))))
                        parameters)))
            ((not (presentation-type-specifier-p type env))
             (funcall complain type "an invalid presentation type specifier"))))))

(defun warn-if-presentation-type-specifier-invalid (enclosing-form form env
                                                    &optional constant)
  (labels ((check (type)
             (warn-if-presentation-type-specifier-invalid-1 type env #'complain))
           (complain (thing string)
             (warn "~S is ~A~@[ in ~S~].~%It appears in a call to ~S."
                   thing string
                   (unless (and (consp form)
                                (eq (first form) 'quote)
                                (eq (second form) thing))
                     form)
                   (first enclosing-form))
             t))
    (declare (dynamic-extent #'check #'complain))
    (if constant
        (check form)
        (when (constantp form #+(or Genera Minima) env)
          (check (eval form #+Genera env))))))

#+Genera
(scl:defprop define-presentation-action define-presentation-translator
             zwei:definition-function-spec-type)

(defmacro define-presentation-translator-1
          (name
           (from-type to-type command-table
            &rest translator-keys
            &key (gesture ':select) tester tester-definitive
                 documentation pointer-documentation
                 (menu t) priority translator-class
            &allow-other-keys)
           arglist
           &body body &environment env)
  #+Genera (declare (zwei:indentation 1 3 3 1))
  (with-warnings-for-definition name define-presentation-translator
    (check-type name symbol)
    (warn-if-presentation-type-specifier-invalid-1 from-type env
      #'(lambda (thing string)
          (warn "~S is ~A in the from-type~:[ ~S~;~]."
                thing string (eq thing from-type) from-type)))
    (when to-type                                ;don't complain when to-type is NIL
      (warn-if-presentation-type-specifier-invalid-1 to-type env
        #'(lambda (thing string)
            (warn "~S is ~A in the to-type~:[ ~S~;~]."
                  thing string (eq thing to-type) to-type))))
    (unless (or (command-table-p command-table)
                (and (symbolp command-table)
                     (or (find-command-table command-table :errorp nil)
                         (compile-time-property command-table 'command-table-name))))
      (warn "~S is not a defined command table name." command-table))
    (unless (and (symbolp gesture)
                 (or (null gesture)
                     (eq gesture 't)
                     (gesture-name-button-and-modifiers gesture)
                     (compile-time-property gesture 'gesture-name)))
      (warn "~S is not a defined gesture name." gesture))
    (check-type tester (or symbol list))
    (check-type tester-definitive boolean)
    (check-type documentation (or string symbol list))
    (check-type pointer-documentation (or string symbol list))
    (check-type menu symbol)
    (check-type priority (or null integer))
    (check-type translator-class (or null symbol))
    (let ((defining-forms nil)
          (translator-functions nil))
      (macrolet ((emit (form)
                   `(push ,form defining-forms))
                 (do-translator-function (clause-name &optional extra-args string-ok)
                   `(when ,clause-name
                      (cond ((or (functionp ,clause-name)
                                 (symbolp ,clause-name))
                             (push `',,clause-name translator-functions))
                            ,@(when string-ok
                                `(((stringp ,clause-name)
                                   (push ,clause-name translator-functions))))
                            (t
                             (multiple-value-bind (function gensym)
                                 (write-translator-function ,clause-name
                                                            name ',clause-name ,extra-args)
                               (emit function)
                               (push `#',gensym translator-functions))))
                      (push (intern (symbol-name ',clause-name) *keyword-package*)
                            translator-functions))))
        (do-translator-function tester)
        (do-translator-function documentation '(stream) t)
        (do-translator-function pointer-documentation '(stream) t)
        (let ((function (cons arglist body)))
          (do-translator-function function))
        (when from-type
          (setq from-type (expand-presentation-type-abbreviation from-type env)))
        (when to-type
          (setq to-type (expand-presentation-type-abbreviation to-type env)))
        `(define-group ,name define-presentation-translator
           ,@defining-forms
           (define-presentation-translator-2
             ',name ',from-type ',to-type ',command-table
             :gesture ',gesture
             :menu ',menu
             :priority ',priority
             :tester-definitive ',tester-definitive
             :translator-class ',translator-class
             ,@translator-functions
             ;; WITH-KEYWORDS-REMOVED probably stack-conses...
             ,@(remove-keywords translator-keys
                 '(:gesture :tester :tester-definitive :menu :priority
                   :documentation :pointer-documentation :translator-class))))))))

(defun define-presentation-translator-2 (translator-name from-type to-type command-table
                                         &rest initargs
                                         &key translator-class gesture
                                              documentation pointer-documentation
                                              tester tester-definitive priority
                                         &allow-other-keys)
  (declare (dynamic-extent initargs))
  (with-keywords-removed (initargs initargs
                          '(:translator-class :gesture
                            :pointer-documentation :tester-definitive :priority))
    (let* ((translator-class
             (or translator-class 'presentation-translator))
           (translator
             (apply #'make-instance
                    translator-class
                    :name translator-name
                    :from-type from-type
                    :to-type to-type
                    :gesture-name gesture
                    :pointer-documentation (or pointer-documentation documentation)
                    ;; Err on the side of performance: if there's no tester,
                    ;; assume that it is definitive.
                    :tester-definitive (or (null tester) tester-definitive)
                    :priority (or priority 0)
                    initargs)))
      ;; You're allowed to explicitly supply NIL for the command table...
      (when command-table
        (add-presentation-translator-to-command-table command-table translator
                                                      :errorp nil))
      translator)))

(defun write-translator-function (function translator-name clause-name
                                  args &optional (use-default-args t))
  (let ((function-name (gensymbol translator-name clause-name))
        (arglist (first function))
        (body (rest function)))
    (multiple-value-bind (arglist ignores)
        (canonicalize-and-match-lambda-lists
         (if use-default-args
             (append *translator-function-arglist* args)
           args)
         arglist)
      (values `(defun ,function-name ,arglist
                 ,@(and ignores `((declare (ignore ,@ignores))))
                 ,@body)
              function-name))))


(defun-inline call-presentation-translator (translator presentation context-type
                                            frame event window x y)
  (let ((function (presentation-translator-function translator)))
    (funcall function
             (presentation-object presentation) presentation context-type
             frame event window x y)))

(defun-inline call-presentation-tester (translator presentation context-type
                                        frame event window x y)
  (let ((tester (presentation-translator-tester translator)))
    (or (null tester)
        (funcall tester
                 (presentation-object presentation) presentation context-type
                 frame event window x y))))

;; Can't blithely use the type-name as the key for everything, since some types
;; (such as AND and SEQUENCE) cannot work only using the type-name.
(defmacro with-presentation-type-translator-key ((key type) &body body)
  (let ((name (gensymbol 'name))
        (parameters (gensymbol 'parameters)))
    `(with-presentation-type-decoded (,name ,parameters) ,type
       (with-stack-list* (,key ,name ,parameters)
         (unless (member ,name *presentation-type-parameters-are-types*)
           (setq ,key ,name))
         ,@body))))

;; Returns a list of all translators that could possibly apply given
;; FROM-TYPE and TO-TYPE, in priority order (where the first element
;; is the highest priority translator).
(defun find-presentation-translators (from-type to-type command-table)
  (setq command-table (find-command-table command-table))
  (with-slots (translators-cache) command-table
    (let ((cache translators-cache))                ;for speed...
      (with-presentation-type-translator-key (from-key from-type)
        (with-presentation-type-translator-key (to-key to-type)
          (with-stack-list (key from-key to-key)
            (multiple-value-bind (translators found-p)
                (and cache (gethash key cache))
              (cond ((or (null found-p)
                         (/= (pop translators) *translators-cache-tick*))
                     (let ((translators (find-presentation-translators-1
                                          from-key to-key command-table)))
                       (when (null cache)
                         (setq translators-cache
                               (make-hash-table :size *translators-cache-size*
                                                :test #'equal))
                         (setq cache translators-cache))
                       ;; Need to copy the whole tree, since the from- and to-keys
                       ;; could themselves be stack-consed.
                       (setf (gethash (copy-tree key) cache)
                             (cons *translators-cache-tick* translators))
                       translators))
                    (t
                     ;; Already popped above
                     translators)))))))))

#+allegro
(excl:defun-proto map-over-command-table-translators (function 
						      command-table &key
						      (inherited t))
  (declare (dynamic-extent function)))

;;--- This traverses a lot of very non-local data structures (presentation types,
;;--- translators, command tables, etc).  What can we do to localize them?
(defun find-presentation-translators-1 (from-key to-key command-table)
  (let ((translators nil))
    (flet ((collect-translators (translator)
             (with-presentation-type-translator-key
                 (translator-from-key (presentation-translator-from-type translator))
               (with-presentation-type-translator-key
                   (translator-to-key (presentation-translator-to-type translator))
                 (when (and (presentation-subtypep-1 from-key translator-from-key)
                            ;; PRESENTATION-SUBTYPEP will return T when the
                            ;; translator is "context independent", that is,
                            ;; translator-to-key is NIL.
                            (presentation-subtypep-1 translator-to-key to-key))
                   ;; If we're looking for a translator from LMFS-PATHNAME to COMMAND
                   ;; a PATHNAME->FOO-COMMAND translator should apply.
                   (pushnew translator translators))))))
      (declare (dynamic-extent #'collect-translators))
      (map-over-command-table-translators #'collect-translators command-table))
    ;; The translator ordering is as follows, in this order:
    ;;  - Translators with higher high-order priority precede ones with lower
    ;;    high-order priority
    ;;  - Translators on more specific types precede ones on less specific types
    ;;  - Translators with higher low-order priority precede ones with lower
    ;;    low-order priority
    ;;  - Translators from this command table precede inherited translators
    ;; First get the command table ordering correct.
    (setq translators (nreverse translators))
    ;; Then stable-sort by high-order priority, class precedence, and low-order
    ;; priority.  The list of translators is usually short, so using a bubble
    ;; sort will suffice.
    (dorest (translators translators)
      (dorest (remaining (cdr translators))
        (let* ((translator1 (car translators))
               (type1 (presentation-translator-from-type translator1))
               (name1 (presentation-type-name type1))
               (translator2 (car remaining))
               (type2 (presentation-translator-from-type translator2))
               (name2 (presentation-type-name type2)))
          (multiple-value-bind (high1 low1)
              (floor (presentation-translator-priority translator1) 10)
            (multiple-value-bind (high2 low2)
                (floor (presentation-translator-priority translator2) 10)
              (unless (< high2 high1)
                (if (< high1 high2)
                    (setf (car translators) translator2
                          (car remaining) translator1)
                  (cond ((eq name1 name2)
                         ;; If the two types are the same, then use the low
                         ;; order part of the priority to break the tie
                         (when (< low1 low2)
                           (setf (car translators) translator2
                                 (car remaining) translator1)))
                        ((presentation-subtypep-1 type2 type1)
                         ;; The second type is more specific than the first, swap.
                         (setf (car translators) translator2
                               (car remaining) translator1))))))))))
    translators))

;; Return a list of all classes that are not provably disjoint from CLASS
(defun class-nondisjoint-classes (class)
  (let ((class (find-presentation-type-class class)))
    (when (or (eq class (find-class 't))
              (eq class (find-class 'standard-object))
              #+Symbolics                        ;Symbolics CLOS, that is
              (eq class (find-class 'clos:structure-object)))
      (return-from class-nondisjoint-classes t))
    (labels ((transitive-closure (function element set)
               (pushnew element set)
               (dolist (new-element (funcall function element))
                 (unless (member new-element set)
                   (setq set (union (transitive-closure function new-element set) set))))
               set))
      (let ((set (transitive-closure #'class-direct-subclasses class nil)))
        (dolist (element set)
          (setq set (transitive-closure #'class-direct-superclasses element set)))
        (delete-if-not #'acceptable-presentation-type-class set)))))

(defun test-presentation-translator (translator presentation context-type
                                     frame window x y
                                     &key event (modifier-state 0) for-menu)
  (and (presentation-translator-matches-event translator event modifier-state for-menu)
       (test-presentation-translator-1 translator presentation context-type
                                       frame event window x y)))

;; Does the translator match the pointer event?
(defun test-presentation-translator-1 (translator presentation context-type
                                       frame event window x y)
  (and
    ;; Make sure that the presentation matches the from-type's parameters
    (let ((from-type (presentation-translator-from-type translator)))
      (with-presentation-type-decoded (name parameters) from-type
        (declare (ignore name))
        (or (null parameters)
            (presentation-typep (presentation-object presentation) from-type))))
    ;; From-type parameters matched, run the tester
    (call-presentation-tester translator presentation context-type
                              frame event window x y)
    ;; Tester matched, it has to either be definitive, or we need to run
    ;; the body in the case where the input context has type parameters
    ;; and verify that the body matches the context type
    (or (presentation-translator-tester-definitive translator)
        (with-presentation-type-decoded (name parameters) context-type
          (declare (ignore name))
          (null parameters))
        (multiple-value-bind (translated-object translated-type)
            (call-presentation-translator translator presentation context-type
                                          frame event window x y)
          (declare (ignore translated-type))
          (presentation-typep translated-object context-type)))))

;; EVENT is a button press event.  If it is NIL, then MODIFIER-STATE should
;; be the current modifier state.
(defun presentation-translator-matches-event (translator event modifier-state
                                              &optional for-menu)
  (let ((gesture-name (presentation-translator-gesture-name translator)))
    (or (eq gesture-name t)
        for-menu
        (if (null event)
            (modifier-state-matches-gesture-name-p modifier-state gesture-name)
            (button-press-event-matches-gesture-name-p event gesture-name)))))

(defvar *presentation-menu-translator* nil)

;; Returns the higherst priority translator that matches the presentation
;; in this context, or NIL if there is no matching translator.
;; Returns a second value of T if there was any translator that matched
;; on everything except for the gesture, and the menu translator matched
;; (that is, a second value of T means that the menu translator will apply)
(defun presentation-matches-context-type (presentation context-type
                                          frame window x y
                                          &key event (modifier-state 0))
  (declare (values translator any-match-p))
  (let ((one-matched nil)
        (translators
          (find-presentation-translators
            (presentation-type presentation) context-type (frame-command-table frame))))
    (when translators
      (dolist (translator translators)
        (let ((by-gesture
                (presentation-translator-matches-event translator event modifier-state))
              (by-tester
                (test-presentation-translator-1 translator presentation context-type
                                                frame event window x y)))
          (when (and by-gesture by-tester)
            ;; Matched by both gesture and by the tester, we're done
            (return-from presentation-matches-context-type
              (values translator t)))
          (when by-tester
            ;; We matched by the tester, it's OK to try the menu translator
            ;; unless the translator is not supposed to be in a menu.
            (setq one-matched (or one-matched
                                  (presentation-translator-menu translator))))))
      ;; If EVENT is non-NIL, then we are running on behalf of the user having
      ;; pressed a pointer button, which means that some translator must have
      ;; matched during the test phase, which means that the PRESENTATION-MENU
      ;; translator might be applicable, even though no others were found.
      (let ((menu-applicable
              (and one-matched
                   *presentation-menu-translator*
                   (test-presentation-translator *presentation-menu-translator*
                                                 presentation context-type
                                                 frame window x y
                                                 :modifier-state modifier-state
                                                 :event event))))
           (if (and event menu-applicable)
               (values *presentation-menu-translator* t)
               (values nil (and one-matched
                                (presentation-translator-matches-event
                                  *presentation-menu-translator* event modifier-state))))))))

;; When FASTP is T, that means "as soon as you find a matching translator, return
;; that translator".  Otherwise, return a list of all applicable translators.
(defun find-applicable-translators (presentation input-context frame window x y
                                    &key event modifier-state (for-menu nil for-menu-p) fastp)
  (let ((applicable-translators nil))
    (do ((presentation presentation
                       (parent-presentation-with-shared-box presentation window)))
        ((null presentation))
      (let ((from-type (presentation-type presentation)))
        ;; Loop over the contexts, from the most specific to the least specific
        (dolist (context input-context)
          (let ((context-type (pop context))        ;input-context-type = first
                (tag (pop context)))                ;input-context-tag = second
            (let ((translators (find-presentation-translators
                                 from-type context-type (frame-command-table frame))))
              (when translators
                (dolist (translator translators)
                  (when (and (or (not for-menu-p)
                                 (eq (presentation-translator-menu translator) for-menu))
                             (test-presentation-translator translator
                                                           presentation context-type
                                                           frame window x y
                                                           :event event
                                                           :modifier-state modifier-state
                                                           :for-menu for-menu))
                    (when fastp
                      (return-from find-applicable-translators translator))
                    ;; Evacuate the context-type, but don't bother evacuating the
                    ;; tag since it will get used before its extent expires.
                    (push (list translator presentation
                                (evacuate-list context-type) tag)
                          applicable-translators))))
              ;; If we've accumulated any translators, maybe add on PRESENTATION-MENU.
              ;; If FASTP is T, we will have returned before we get here.
              (when (and applicable-translators
                         *presentation-menu-translator*
                         (or (not for-menu-p)
                             (eq (presentation-translator-menu *presentation-menu-translator*)
                                 for-menu))
                         (test-presentation-translator *presentation-menu-translator*
                                                       presentation context-type
                                                       frame window x y
                                                       :event event
                                                       :modifier-state modifier-state
                                                       :for-menu for-menu))
                (push (list *presentation-menu-translator* presentation
                            (evacuate-list context-type) tag)
                      applicable-translators)))))))
    ;; Since we pushed translators onto the list, the least specific one
    ;; will be at the beginning of the list.  DELETE-DUPLICATES is defined to
    ;; remove duplicated items which appear earlier in the list, so it will
    ;; remove duplicated less specific translators.  Finally, NREVERSE will
    ;; get the translators in most-specific to least-specific order.
    (nreverse (delete-duplicates applicable-translators
                                 :test #'(lambda (x y)
                                           (and (eq (first x) (first y))
                                                (eq (second x) (second y))))))))

(defun document-presentation-translator (translator presentation context-type
                                         frame event window x y
                                         &key (stream *standard-output*)
                                              documentation-type)
  (let ((documentation
          (if (eq documentation-type :pointer)
              (or (presentation-translator-pointer-documentation translator)
                  (presentation-translator-documentation translator))
              (presentation-translator-documentation translator))))
    (handler-bind ((error
                     ;; Don't blow out if there was an error, just be ugly
                     #'(lambda (anerror)
                         (declare (ignore anerror))
                         (format stream "Translator ~S"
                           (presentation-translator-name translator)))))
      (cond ((stringp documentation)
             (write-string documentation stream))
            (documentation
             (funcall documentation
                      (presentation-object presentation) presentation context-type
                      frame event window x y
                      stream))
            (t
             (when (eq documentation-type :from-body)
               ;; In general, it is not safe to run the body of a translator to get
               ;; its documentation, but sometimes that's the only way.  Command
               ;; menus are one such example.
               (unless (presentation-action-p translator)
                 (catch 'no-translation                ;just in case...
                   (multiple-value-bind (translated-object translated-type)
                       (call-presentation-translator translator presentation context-type
                                                     frame event window x y)
                     (present translated-object (or translated-type context-type)
                              :stream stream :view +pointer-documentation-view+)
                     (return-from document-presentation-translator (values))))))
             ;; If we didn't get asked to run the body for the purpose of command
             ;; menus, then we might be able to take a different kind of short
             ;; cut for to-command translators.
             (when (eq (presentation-type-name (presentation-translator-to-type translator))
                       'command)
               (return-from document-presentation-translator
                 (document-presentation-to-command-translator
                   translator presentation context-type frame event window x y stream)))
             ;; Final fallback, not pretty
             (format stream "Translator ~S"
               (presentation-translator-name translator)))))))
