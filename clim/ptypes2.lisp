;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; $Id: ptypes2.lisp,v 2.6 2005/12/08 21:25:43 layer Exp $

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;;; This file contains portions of the presentation type implementation that
;;; cannot be compiled until PTYPES1.LISP is loaded

;;; True if object is a valid presentation type specifier
(defun presentation-type-specifier-p (object &optional environment)
  (with-presentation-type-decoded (type-name parameters-given options-given) object
    (when (typep type-name '(or symbol (satisfies acceptable-presentation-type-class)))
      (let ((class (find-presentation-type-class type-name nil environment)))
        (when class
          ;; The type-name is okay, validate the parameters and options
          (let ((parameters-allowed (presentation-type-parameters class environment))
                (options-allowed (presentation-type-options class environment)))
            (let ((parameters-allowed parameters-allowed)
                  (options-allowed options-allowed)
                  (parameters-given parameters-given))
              (do ((item (car parameters-allowed) (car parameters-allowed))
                   (optional nil))
                  ((null parameters-allowed)
                   (when parameters-given
                     ;; Too many parameters
                     (return-from presentation-type-specifier-p nil)))
                (cond ((member item lambda-list-keywords)
                       (case item
                         (&optional (setq optional t))
                         (&rest (return))
                         (&key
                           (pop parameters-allowed)
                           (unless (member '&allow-other-keys parameters-allowed)
                             (do* ((l parameters-given (cddr l))
                                   (kwd (car l) (car l)))
                                  ((null l))
                               (unless (member kwd parameters-allowed
                                               :key #'parameter-specifier-keyword)
                                 (return-from presentation-type-specifier-p nil))))
                           (return))))
                      (parameters-given
                       (pop parameters-given))
                      ((not optional)
                       ;; Too few parameters
                       (return-from presentation-type-specifier-p nil)))
                (pop parameters-allowed))
              (do* ((l options-given (cddr l))
                    (kwd (car l) (car l)))
                   ((null l))
                (or (member kwd *standard-presentation-options*)
                    (member kwd options-allowed :key #'parameter-specifier-keyword)
                    (return-from presentation-type-specifier-p nil))))
            (if (and (or parameters-allowed options-allowed)
                     ;; If the class exists but we can't get its prototype, which tends to
                     ;; happen during compile-file, then we can't call its method so we have
                     ;; to just assume that the parameters are correct.  I don't like it
                     ;; but there really is no other choice.
                     (handler-case
                         (class-prototype class)
                       (error () nil)))
                ;; Do any additional user-defined validation
                ;; Must trap errors because wrong number of parameters might signal
                ;; an error inside bind-to-list
                ;; Must use class rather than type-name so the right environment gets used
                (handler-case
                    (with-stack-list* (class-and-parameters class parameters-given)
                      (with-stack-list* (type-specifier class-and-parameters options-given)
                        (funcall-presentation-generic-function
                          presentation-type-specifier-p type-specifier)))
                  (error () nil))
                ;; If no parameters nor options, no need for user-defined validation
                t)))))))

;;; The default is that any parameters or options are okay
(define-default-presentation-method presentation-type-specifier-p (type)
  (declare (ignore type))
  t)

;;; Return a list of the names of the direct supertypes
(defun presentation-type-direct-supertypes (presentation-type)
  (with-presentation-type-decoded (type) presentation-type
    (mapcar #'class-presentation-type-name
            (class-direct-superclasses (find-presentation-type-class type)))))


;;;; Describe

;;; Output a brief description of type to stream
(defun describe-presentation-type (presentation-type
                                   &optional (stream *standard-output*) (plural-count 1))
  (if (null stream)
      (with-output-to-string (stream)
        (describe-presentation-type presentation-type stream plural-count))
      (let ((type (expand-presentation-type-abbreviation presentation-type)))
        (with-presentation-type-decoded (nil nil options) type
          (let ((description (getf options :description)))
            (if description
                (default-describe-presentation-type description stream plural-count)
                (funcall-presentation-generic-function describe-presentation-type
                  type stream plural-count)))))))

;;; The default method that works for describing most presentation types
(define-default-presentation-method describe-presentation-type
                                    (presentation-type stream plural-count)
  (with-presentation-type-decoded (name) presentation-type
    (default-describe-presentation-type
      (gethash (find-presentation-type-class name) *presentation-type-description-table* name)
      stream plural-count)))

;;; The default way of pluralizing a presentation type
;;; description is a string that names a single member of the type
(defun default-describe-presentation-type (description stream plural-count)
  (setq description (string description))        ;can be a symbol sometimes
  (etypecase plural-count
    ((member nil)
     (write-string description stream)
     (return-from default-describe-presentation-type nil))
    ((member 1)
     (write-string (string-a-or-an description) stream)
     (write-string description stream)
     (return-from default-describe-presentation-type nil))
    ((member t))
    ((integer 2 *)
     (format stream "~R " plural-count)))
  (write-string (string-pluralize description) stream))

;; Returns the plural for the specified string
(defun string-pluralize (string)
  (let ((length (length string))
        (pos (1+ (or (string-search-set '(#\Space #\Tab) string :from-end t) -1))))
    (when (zerop length)
      (return-from string-pluralize string))
    (let* ((flush nil)
           (suffix nil)
           (last-char (aref string (1- length)))
           (penult-char (if (> length 1) (aref string (- length 2)) #\*)))
      (cond ((and (char-equal last-char #\y)
                  (not (find penult-char '(#\a #\e #\i #\o #\u) :test #'char-equal)))
             (setq flush 1
                   suffix "ies"))
            ((or (string-equal string "ox"  :start1 pos)
                 (string-equal string "vax" :start1 pos))
             (setq suffix "en"))
            ((or (and (char-equal last-char #\h)
                      (find penult-char '(#\c #\s) :test #'char-equal))
                 (find last-char '(#\s #\z #\x) :test #'char-equal))
             (setq suffix "es"))
            ((and (>= length 3)
                  (string-equal string "man" :start1 (- length 3))
                  (not (string-equal string "human" :start1 pos)))
             (setq flush 2
                   suffix "en"))
            ((and (>= length 3)
                  (string-equal string "ife" :start1 (- length 3)))
             (setq flush 2
                   suffix "ves"))
            ((and (>= length 5)
                  (string-equal string "child" :start1 (- length 5)))
             (setq suffix "ren"))
            (t (setq suffix "s")))
      (concatenate 'string
        (if flush (subseq string 0 (- length flush)) string)
        suffix))))

;; Returns an article to be used with the specified string
(defun string-a-or-an (string)
  (let* ((length (length string))
         (char (and (not (zerop length))
                    (char string 0))))
    (when (zerop length)
      (return-from string-a-or-an ""))
    (if (digit-char-p char)
        ;; Pronounce leading digits number!
        (string-a-or-an (format nil "~:R" (parse-integer string :junk-allowed t)))
        (cond ((or (string-equal string "one")
                   (and (>= length 4)
                        (string-equal string "one " :end1 4)))
               "a ")
              ((or (= length 1)                        ;"an x", but "a xylophone"
                   ;; "an fff" but "a frog"
                   (not (string-search-set "aeiou" string))
                   ;; "an xl400" but "a xylophone"
                   (string-search-set "0123456789" string))
               (if (find char "aefhilmnorsx" :test #'char-equal) "an " "a "))
              (t
               (if (or (find char "aio" :test #'char-equal)
                       ;; "an egg", but "a eunich"
                       (and (char-equal char #\e)
                            (not (string-equal string "eu" :end1 2)))
                       ;; "an umbrella", but "a unicorn"
                       ;; "a uniform", but "an uninformed ..."
                       ;; And of course, "a unix".  We admittedly heuristicate
                       (and (char-equal char #\u)
                            (not (and (string-equal string "uni" :end1 3)
                                      (or (< length 5)
                                          (not (find (char string 4)
                                                     ;; Treat "y" as a vowel here.
                                                     ;; e.g., "unicycle"
                                                     "bcdfghjklmnpqrstvwxz"
                                                     :test #'char-equal)))))))
                   "an "
                   "a "))))))

(defun string-search-set (char-set string &key (start 0) end from-end (test #'char-equal))
  (let (#+(or Genera Minima) (string string))
    (declare (type vector string))
    (unless end
      (setq end (length string)))
    (if from-end
        (do ((i (1- end) (1- i)))
            ((< i start) nil)
          (when (find (aref string i) char-set :test test)
            (return i)))
        (do ((i start (1+ i)))
            ((>= i end) nil)
          (when (find (aref string i) char-set :test test)
            (return i))))))


;;;; Histories

(defun presentation-type-history (presentation-type)
  (let ((type (expand-presentation-type-abbreviation presentation-type)))
    (loop
      (let ((history
              (funcall-presentation-generic-function presentation-type-history type)))
        (cond ((null history)
               (return-from presentation-type-history nil))
              ((typep history 'basic-history)
               (return-from presentation-type-history history))
              (t
               (setq type history)))))))

(define-default-presentation-method presentation-type-history (presentation-type)
  (let ((name (presentation-type-name presentation-type)))
    (values (gethash name *presentation-type-history-table*))))

;; Some histories are maintained for each instance of an application frame
(defun presentation-type-history-for-frame (type frame)
  (when (frame-maintain-presentation-histories frame)
    (let* ((type-name (presentation-type-name type))
           (history (find type-name (slot-value frame 'histories)
                          :key #'presentation-history-type)))
      (unless history
        (setq history (make-presentation-type-history type-name))
        (push history (slot-value frame 'histories)))
      history)))


(defun presentation-default-preprocessor (default presentation-type &key default-type)
  (let ((type (expand-presentation-type-abbreviation presentation-type)))
    (funcall-presentation-generic-function presentation-default-preprocessor
      default type :default-type default-type)))

(define-default-presentation-method presentation-default-preprocessor
                                    (default presentation-type &key default-type)
  (declare (ignore presentation-type))
  (values default default-type))


;;;; Map Over Supertypes

;;; Successively calls FUNCTION on the type and all its supertypes, in order.
;;; FUNCTION is called with two arguments, the new type name and the new
;;; presentation type specifier with parameters and options.
#+Genera (zwei:defindentation (map-over-presentation-type-supertypes 1 2))
(defun map-over-presentation-type-supertypes (presentation-type function)
  (declare (dynamic-extent function))
  (funcall-presentation-generic-function map-over-presentation-type-supertypes
    presentation-type function))

;;; Map over unparameterized presentation types, including some mappings
;;; generated by ADDITIONAL-FORMS.
(defmacro map-over-presentation-type-supertypes-augmented
          (presentation-type function &body additional-forms)
  (let ((name (gensymbol 'name))
        (class (gensymbol 'class))
        (type-name (gensymbol 'type-name)))
    `(with-presentation-type-decoded (,name) ,presentation-type
       (funcall ,function ,name ,presentation-type)
       ,@additional-forms
       (dolist (,class (cdr (find-class-precedence-list
                              (find-presentation-type-class ,name))))
         (let ((,type-name (class-presentation-type-name ,class)))
           (funcall function ,type-name ,type-name))))))

;;; The default method that works for mapping over unparameterized presentation types
(define-default-presentation-method map-over-presentation-type-supertypes
                                    (presentation-type function)
  (map-over-presentation-type-supertypes-augmented presentation-type function))


;;;; TYPEP

;;; Is OBJECT of type TYPE?
(defun presentation-typep (object type)
  (with-presentation-type-decoded (type-name parameters) type
    (case type-name
      ((t) t)                ;the type of all objects
      ((nil) nil)        ;the type of no objects
      (otherwise
        ;; If this type is a Common Lisp type, first call TYPEP and only use the
        ;; method to check the parameters.  Otherwise the method does it all.
        ;; This actually applies not to all Common Lisp types, but just to the
        ;; ones that have corresponding classes, since the others either are not
        ;; presentation types or have to have explicit presentation-typep methods.
        (let ((class (if (symbolp type-name) (find-class type-name nil) type-name)))
          (unless (or (null class) (presentation-type-class-p class))
            (unless #+Genera (clos-internals:typep-class object class)
                    #+CCL-2  (ccl::class-typep object class)
                    #-(or Genera CCL-2) (typep object class)
              (return-from presentation-typep nil))
            (unless parameters
              (return-from presentation-typep t))))
        ;; Call the method, either to check the parameters after determining
        ;; that object is a member of the correct class, or to do it all.
        (funcall-presentation-generic-function presentation-typep object type)))))

;; The "real" default method is inlined above in PRESENTATION-TYPEP.  What
;; it does is to call TYPEP on anything that is a class or built-in type.
;; If that did not get executed, then this type must be a user-defined type
;; that does not correspond to a class.  Let him know.
(define-default-presentation-method presentation-typep (object type)
  (declare (ignore object))
  (error "You must supply a ~S method for the presentation type ~S"
         'presentation-typep (presentation-type-name type)))


;;; SUBTYPEP

;; SWM wanted this to have a cache, but let's hold off since it looks
;; like MAP-OVER-PRESENTATION-TYPE-SUPERTYPES is faster than caching.
;; Here is some metering information (Genera 8.0, MacIvory model 2):
;; subtype        supertype       Common Lisp    CLIM    Dynamic Windows (cached)
;; integer        integer               9          10         387
;; integer        rational            907         307         382
;; (integer 1 2)  (integer 3 5)       342         213         698
;; (integer 1 2)  (rational 3 5)     2296         522         729
;; integer        float              3180         224         450
;; integer        number              897         336         419
;; Current numbers for CLIM will be a few microseconds slower, since some code was added

;; Is TYPE a subtype of PUTATIVE-SUPERTYPE?
;; Should not accept abbreviations, you gotta expand them before calling this
;; Possible returned values are:
;;  T,T     => TYPE is definitely a subtype of PUTATIVE-SUPERTYPE
;;  NIL,T   => TYPE is definitely *not* a subtype of PUTATIVE-SUPERTYPE
;;  NIL,NIL => Don't know, the answer cannot definitively be determined
;;  T,NIL   => Can't ever happen
(defun presentation-subtypep (type putative-supertype)
  (declare (values subtype-p known-p))
  (multiple-value-bind (subtype-p known-p)
      (presentation-subtypep-1 type putative-supertype)
    (when (and (eq subtype-p nil)
               (eq known-p t)
               (gethash (presentation-type-name putative-supertype)
                        *presentation-type-abbreviation-table*))
      ;; One reason we might have gotten this result is that the supertype
      ;; was an abbreviation, so it never was reached during the type walk.
      ;; Check this out now.
      (error "~S is a presentation type abbreviation, not the name of a presentation type"
             (presentation-type-name putative-supertype)))
    (values subtype-p known-p)))

;; The guts of the above, but faster since we don't worry about the case
;; where a type abbreviation was passed in.
(defun presentation-subtypep-1 (type putative-supertype)
  (declare (values subtype-p known-p))
  (with-presentation-type-decoded (type-name type-parameters) type
    (with-presentation-type-decoded (supertype-name supertype-parameters) putative-supertype

      ;; Work even if classes are used as presentation type specifiers.
      ;; Canonicalize to symbols rather than to classes on the theory that
      ;; this will be faster because symbols are more commonly used here.
      (unless (symbolp type-name)
        (setq type-name (class-presentation-type-name type-name)))
      (unless (symbolp supertype-name)
        (setq supertype-name (class-presentation-type-name supertype-name)))

      ;; Do some quick tests first
      (cond ((eq type-name supertype-name)        ;No inheritance involved
             (cond ((null supertype-parameters)
                    (values t t))                ;Parameters can only restrict
                   ((equal supertype-parameters type-parameters)
                    (values t t))                ;Everything is its own subtype
                   (t
                    (multiple-value-bind (subtype-p known-p)
                        (funcall-presentation-generic-function presentation-subtypep
                          type putative-supertype)
                      ;; Catch a common mistake in PRESENTATION-SUBTYPEP methods
                      (when (and subtype-p (not known-p))
                        (cerror "Return the two values T and T"
                                "The call to ~S on type ~S only returned a single value of T"
                                'presentation-subtypep type)
                        (setq known-p t))
                      (values subtype-p known-p)))))
            ((eq type-name 't)
             (values nil t))                        ;Nothing has T as a subtype
            ((eq type-name 'nil)
             (values t t))                        ;NIL is a subtype of everything
            ((eq supertype-name 't)
             (values t t))                        ;T is a supertype of everything
            ((eq supertype-name 'nil)
             (values nil t))                        ;Nothing has NIL as a supertype
            ((eq type-name 'and)
             ;; Yes, we really call SOME, not EVERY.  For example,
             ;; (AND INTEGER (SATISFIES ODDP)) is a subtype of INTEGER
             (if (some #'(lambda (type)
                           (with-presentation-type-decoded (type-name) type
                             (unless (member type-name '(not satisfies))
                               (presentation-subtypep-1 type putative-supertype))))
                       type-parameters)
                 (values t t)
                 (values nil nil)))
            ((eq type-name 'or)
             (if (every #'(lambda (type)
                            (presentation-subtypep-1 type putative-supertype))
                        type-parameters)
                 (values t t)
                 (values nil nil)))
            ((eq supertype-name 'and)
             (if (every #'(lambda (supertype)
                            (with-presentation-type-decoded (type-name) supertype
                              (unless (member type-name '(not satisfies))
                                (presentation-subtypep-1 type supertype))))
                        supertype-parameters)
                 (values t t)
                 (values nil nil)))
            ((eq supertype-name 'or)
             (if (some #'(lambda (supertype) (presentation-subtypep-1 type supertype))
                       supertype-parameters)
                 (values t t)
                 (values nil nil)))
            (t
             ;; Must resort to inheritance to figure it out
             (flet ((psubtypep (ntype-name ntype)
                      (when (eq ntype-name supertype-name)
                        ;; Aside from parameters, type is a subtype of putative-supertype
                        (return-from presentation-subtypep-1
                          (cond ((null supertype-parameters)
                                 (values t t))        ;Parameters can only restrict
                                ((equal supertype-parameters
                                        (with-presentation-type-decoded (nil parameters) ntype
                                          parameters))
                                 (values t t))        ;Everything is its own subtype
                                (t                ;Call method to figure out the parameters
                                 (multiple-value-bind (subtype-p known-p)
                                     (funcall-presentation-generic-function
                                       presentation-subtypep
                                       ntype putative-supertype)
                                   (when (and subtype-p (not known-p))
                                     (cerror "Return the two values T and T"
                                             "The call to ~S on type ~S only returned a single value of T"
                                             'presentation-subtypep ntype)
                                     (setq known-p t))
                                   (values subtype-p known-p))))))))
               (declare (dynamic-extent #'psubtypep))
               (map-over-presentation-type-supertypes type #'psubtypep))
             ;; TYPE is not even a subclass of PUTATIVE-SUPERTYPE
             (values nil t))))))


;;;; TYPE-OF

(defun presentation-type-of (object)
  (dolist (class (find-class-precedence-list (class-of object)) 'expression)
    (let ((name (class-proper-name class)))
      (unless (eq name 't)        ;prefer EXPRESSION over T
        (when (or (acceptable-presentation-type-class class)
                  (and (symbolp name)
                       (find-presentation-type-class name nil)
                       (presentation-type-specifier-p name)))
          (return name))))))


;;;; ACCEPT-PRESENT-DEFAULT, prompting, and views

;;; Called when ACCEPT turns into PRESENT
(defun accept-present-default (presentation-type stream view default default-supplied-p
                               present-p query-identifier
                               &key (prompt t) (active-p t))
  (funcall-presentation-generic-function accept-present-default
    presentation-type stream view default default-supplied-p present-p query-identifier
    :prompt prompt :active-p active-p))

(define-default-presentation-method accept-present-default
    (presentation-type stream view
                       default default-supplied-p
                       present-p query-identifier
                       &key prompt &allow-other-keys)
  (declare (ignore query-identifier prompt))
  (with-output-as-presentation (stream (second present-p) (first present-p))
    (if default-supplied-p
        (present default presentation-type :stream stream :view view)
      (with-text-face (stream :italic)
        (describe-presentation-type presentation-type stream 1)))))


;; Most gadgets do not include a prompt...
(define-default-presentation-method gadget-includes-prompt-p
                                    (presentation-type stream view)
  (declare (ignore presentation-type stream view))
  nil)

(defun gadget-includes-prompt-p (type stream view)
  (funcall-presentation-generic-function
    gadget-includes-prompt-p type stream view))


(define-default-presentation-method decode-indirect-view
                                    (presentation-type view framem &key query-identifier)
  (declare (ignore presentation-type framem query-identifier))
  view)

(defun decode-indirect-view (type view frame-manager &rest args
                             &key query-identifier read-only)
  (declare (dynamic-extent args))
  (declare (ignore query-identifier read-only))
  (apply-presentation-generic-function decode-indirect-view
    type view frame-manager args))


;;;; Refined position test and highlighting

(defun presentation-refined-position-test (record presentation-type x y)
  (or (eq record *null-presentation*)
      (funcall-presentation-generic-function presentation-refined-position-test
        presentation-type record x y)))

(define-default-presentation-method presentation-refined-position-test
                                    (presentation-type record x y)
  (declare (ignore presentation-type))
  (output-record-refined-position-test record x y))


(defun highlight-presentation (record presentation-type stream state)
  (unless (eq record *null-presentation*)
    (with-output-recording-options (stream :record nil)
      (funcall-presentation-generic-function highlight-presentation
        presentation-type record stream state)
      (force-output stream))))

(define-default-presentation-method highlight-presentation
                                    (presentation-type record stream state)
  (declare (ignore presentation-type))
  (highlight-output-record record stream state))


;;;; More Development Environment Support

#+Genera
(defun (compiler:style-checker warn-if-presentation-type-specifier-invalid accept) (form)
  (warn-if-presentation-type-specifier-invalid form (second form)
                                               compiler:*optimizer-environment*))

#+Genera
(defun (compiler:style-checker warn-if-presentation-type-specifier-invalid
                               accept-from-string) (form)
  (warn-if-presentation-type-specifier-invalid form (second form)
                                               compiler:*optimizer-environment*))

#+Genera
(defun (compiler:style-checker warn-if-presentation-type-specifier-invalid
                               presentation-typep) (form)
  (warn-if-presentation-type-specifier-invalid form (third form)
                                               compiler:*optimizer-environment*))

#+Genera
(defun (compiler:style-checker warn-if-presentation-type-specifier-invalid
                               presentation-subtypep) (form)
  (warn-if-presentation-type-specifier-invalid form (second form)
                                               compiler:*optimizer-environment*)
  (warn-if-presentation-type-specifier-invalid form (third form)
                                               compiler:*optimizer-environment*))

#+Genera
(defun (compiler:style-checker warn-if-presentation-type-specifier-invalid present) (form)
  (when (cddr form)
    (warn-if-presentation-type-specifier-invalid form (third form)
                                                 compiler:*optimizer-environment*)))

#+Genera
(defun (compiler:style-checker warn-if-presentation-type-specifier-invalid
                               present-to-string) (form)
  (when (cddr form)
    (warn-if-presentation-type-specifier-invalid form (third form)
                                                 compiler:*optimizer-environment*)))

#+Genera
(defun (compiler:style-checker warn-if-presentation-type-specifier-invalid
                               with-output-as-presentation) (form)
  (let ((type (third (second form))))
    (when type
      (warn-if-presentation-type-specifier-invalid form type
                                                   compiler:*optimizer-environment*))))

#+Genera
(defun (compiler:style-checker warn-if-presentation-type-specifier-invalid
                               with-input-context) (form)
  (warn-if-presentation-type-specifier-invalid form (first (second form))
                                               compiler:*optimizer-environment*))

;;; It appears that the compiler fails to call the next three style checkers
;;; for top-level macros.  They will error at load time without a compile time warning.

#+Genera
(defun (compiler:style-checker warn-if-presentation-type-specifier-invalid
                               define-presentation-translator) (form)
  (warn-if-presentation-type-specifier-invalid form (first (third form))
                                               compiler:*optimizer-environment*
                                               t)
  (warn-if-presentation-type-specifier-invalid form (second (third form))
                                               compiler:*optimizer-environment*
                                               t))

#+Genera
(defun (compiler:style-checker warn-if-presentation-type-specifier-invalid
                               define-presentation-action) (form)
  (warn-if-presentation-type-specifier-invalid form (first (third form))
                                               compiler:*optimizer-environment*
                                               t)
  (warn-if-presentation-type-specifier-invalid form (second (third form))
                                               compiler:*optimizer-environment*
                                               t))

#+Genera
(defun (compiler:style-checker warn-if-presentation-type-specifier-invalid
                               define-presentation-to-command-translator) (form)
  (warn-if-presentation-type-specifier-invalid form (first (third form))
                                               compiler:*optimizer-environment*
                                               t))

