;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-1998 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; $Id: standard-types.lisp,v 1.40 1998/08/06 23:16:04 layer Exp $

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;;; This file contains the standard, canned presentation types


;;;; Basic Presentation Types

(define-presentation-type t () :description "object")

(define-presentation-method present (object (type t) stream (view textual-view)
                                            &key acceptably)
  ;; Kludge!
  (write-token (with-output-to-string (s)
                 (write object :escape nil :stream s))
               stream :acceptably acceptably))

(define-presentation-method accept ((type t) stream (view textual-view) &key)
  ;; Allow the user to only click on things if he did not supply any
  ;; ACCEPT method for the type.  This is a perfectly reasonable thing
  ;; to do in direct-manipulation user interfaces.
  (with-delimiter-gestures (() :override t)
    (read-token stream :click-only t)))


;;; :INHERIT-FROM T is to suppress inheriting from (AND SYMBOL LIST)
;;; since LIST is not a presentation type
(define-presentation-type null ()
  :inherit-from t)

(define-presentation-method describe-presentation-type ((type null) stream plural-count)
  (declare (ignore plural-count))        ;deliberately ignored
  (write-string "None" stream))

(define-presentation-method present (object (type null) stream (view textual-view) &key)
  (declare (ignore object))
  (write-string "None" stream))

(define-presentation-method accept ((type null) stream (view textual-view) &key)
  (let ((token (read-token stream)))
    (unless (string-equal token "None")
      (input-not-of-required-type token type))
    nil))


;; The inherited PRESENT method is fine
(define-presentation-type symbol ()
  :inherit-from t                        ;enforce CL definition
  :history t)

(define-presentation-method accept ((type symbol) stream (view textual-view) &key)
  (simple-lisp-object-parser type stream))

(defun simple-lisp-object-parser (type stream &optional coerce-test coerce-function (package *package*))
  (loop
    (let ((token (read-token stream)))
      (when (input-editing-stream-p stream)
        (rescan-if-necessary stream))
      (multiple-value-bind (object index)
          (let ((*read-eval* nil)
                (*package* package))        ;disable "#."
            (read-from-string token nil token))
        (when (eq object token)
          (simple-parse-error 
             #-(or aclpc acl86win32) "Unexpected EOF"
             #+(or aclpc acl86win32) ;; pr Aug97
             (if (and (stringp token) (> (length token) 0))
                (format nil "Input ~S was not a ~A."
                        token (describe-presentation-type type nil nil))
                "Input was empty.")))
        ;; Too bad read-from-string doesn't take a :junk-allowed argument
        ;; Simulate what it would do
        (unless (>= index (length token))
          (when (find-if-not #'whitespace-char-p token :start index)
            (simple-parse-error "Extra junk ~S found after the ~A."
                                (subseq token index)
                                (describe-presentation-type type nil nil))))
        (when (presentation-typep object type)
          (return-from simple-lisp-object-parser (values object type)))
        (when coerce-function
          (when (funcall coerce-test object)
            (setq object (funcall coerce-function object))
            (when (presentation-typep object type)
              (return-from simple-lisp-object-parser (values object type)))))
        (input-not-of-required-type token type)))))


(define-presentation-type keyword ()
  :inherit-from 'symbol
  :history symbol)

(define-presentation-method accept ((type keyword) stream (view textual-view) &key)
  (simple-lisp-object-parser type stream nil nil *keyword-package*))


;;; This is needed because KEYWORD is not a built-in-class
(define-presentation-method presentation-typep (object (type keyword))
  (keywordp object))


;;;; Numeric Presentation Types

;; For the most part, all the number types share ACCEPT and PRESENT methods
(define-presentation-type number ()
  :inherit-from t                        ;enforce CL definition
  :history t)

;; The PRESENT method inherited from T is fine
(define-presentation-method accept ((type number) stream (view textual-view) &key)
  (simple-lisp-object-parser type stream))


(define-presentation-type complex (&optional type)
  :inherit-from 'number
  :history number
  :description "complex number")

(define-presentation-method presentation-typep (object (type complex))
  (or (eq type '*)
      (and (presentation-typep (realpart object) type)
           (presentation-typep (imagpart object) type))))

(define-presentation-method presentation-subtypep ((type complex) putative-supertype)
  (let ((type1
          (with-presentation-type-parameters (complex type) type))
        (type2
          (with-presentation-type-parameters (complex putative-supertype) type)))
    (cond ((eq type2 '*)
           (values t t))
          ((eq type1 '*)
           (values nil t))
          (t
           (presentation-subtypep-1 type1 type2)))))

(define-presentation-method describe-presentation-type :after
                            ((type complex) stream plural-count)
  (declare (ignore plural-count))
  (unless (eq type '*)
    (format stream " whose components are ")
    (describe-presentation-type type stream t)))
  
(define-presentation-type real (&optional low high)
  :options ((base 10 nil (integer 2 36))
            (radix nil nil boolean (:prompt "Display the base")))
  :inherit-from 'number
  :history number
  :description "real number")

(define-presentation-method present (object (type real) stream (view textual-view)
                                     &key acceptably)
  ;; The silliness with the string is because CLIM has its own incompatible streams
  ;; and WRITE blows up if given a CLIM stream
  (write-token (write-to-string object :base base :radix radix)
               stream :acceptably acceptably))

(define-presentation-method accept ((type real) stream (view textual-view) &key)
  (let ((*read-base* base)
        #+Genera (si:*read-extended-ibase-signed-number*
                   (or (> base 10) si:*read-extended-ibase-signed-number*))
        #+Genera (si:*read-extended-ibase-unsigned-number*
                   (or (> base 10) si:*read-extended-ibase-unsigned-number*)))
    (simple-lisp-object-parser type stream)))

(defmacro rangecase (endpoint &body clauses)
  `(cond ,@(mapcar #'(lambda (clause)
                       (ecase (first clause)
                         ((*)         `((eq ,endpoint '*) ,@(rest clause)))
                         ((inclusive) `((numberp ,endpoint) ,@(rest clause)))
                         ((exclusive) `((consp ,endpoint)
                                        (let ((,endpoint (car ,endpoint)))
                                          ,@(rest clause))))
                         ((otherwise) `(t ,@(rest clause)))))
                   clauses)))

(define-presentation-method describe-presentation-type :after
                            ((type real) stream plural-count)
  (declare (ignore plural-count))        ;primary method gets it
  (describe-numeric-subrange low high stream base)
  (unless (= base 10)
    (format stream " in base ~D" base)))

(defun describe-numeric-subrange (low high stream &optional (base 10))
  (let ((*print-base* base))
    (if (eq low '*)
        (if (eq high '*)
            nil
            (rangecase high
              (inclusive (format stream " less than or equal to ~S" high))
              (exclusive (format stream " less than ~S" high))))
        (if (eq high '*)
            (rangecase low
              (inclusive (format stream " greater than or equal to ~S" low))
              (exclusive (format stream " greater than ~S" low)))
            (rangecase low
              (inclusive
                (rangecase high
                  (inclusive (format stream " between ~S and ~S" low high))
                  (exclusive (format stream " >= ~S and < ~S" low high))))
              (exclusive
                (rangecase high
                  (inclusive (format stream " > ~S and <= ~S" low high))
                  (exclusive (format stream " between ~S and ~S (exclusive)"
                                     low high)))))))))

(define-presentation-method presentation-type-specifier-p ((type real))
  (macrolet (#-(or Genera ANSI-90)
             (realp (object)
               `(typep ,object '(or rational float))))
    (and (if (atom low)
             (or (eq low '*)
                 (realp low))
             (and (realp (car low))
                  (null (cdr low))))
         (if (atom high)
             (or (eq high '*)
                 (realp high))
             (and (realp (car high))
                  (null (cdr high))))
         (typep base '(integer 2 36)))))

(define-presentation-method presentation-typep (object (type real))
  (and (rangecase low
         (* t)
         (inclusive (>= object low))
         (exclusive (> object low)))
       (rangecase high
         (* t)
         (inclusive (<= object high))
         (exclusive (< object high)))))

;;; Both presentation type specifiers are the same subtype of REAL, except for parameters
;;; Change that the subtype's range is included in the supertype's range
;;; Where the supertype range is exclusive and the subtype range is inclusive,
;;; this can return the wrong answer for integers.
(define-presentation-method presentation-subtypep ((subtype real) putative-supertype)
  (multiple-value-bind (low1 high1)
      (with-presentation-type-parameters (real subtype) (values low high))
    (multiple-value-bind (low2 high2)
        (with-presentation-type-parameters (real putative-supertype) (values low high))
      (values
        (and (rangecase low2
               (* t)
               (inclusive (rangecase low1
                            (* nil)
                            (inclusive (>= low1 low2))
                            (exclusive (>= low1 low2))))
               (exclusive (rangecase low1
                            (* nil)
                            (inclusive (> low1 low2))
                            (exclusive (>= low1 low2)))))
             (rangecase high2
               (* t)
               (inclusive (rangecase high1
                            (* nil)
                            (inclusive (<= high1 high2))
                            (exclusive (<= high1 high2))))
               (exclusive (rangecase high1
                            (* nil)
                            (inclusive (< high1 high2))
                            (exclusive (<= high1 high2))))))
        t))))


(define-presentation-type rational (&optional low high)
  :options ((base 10 nil (integer 2 36))
            (radix nil nil boolean (:prompt "Display the base")))
  :inherit-from `((real ,low ,high)
                  :base ,base :radix ,radix)
  :history number
  :description "rational number")


(define-presentation-type ratio (&optional low high)
  :options ((base 10 nil (integer 2 36))
            (radix nil nil boolean (:prompt "Display the base")))
  :history number
  :inherit-from `((rational ,low ,high)
                  :base ,base :radix ,radix))


(define-presentation-type float (&optional low high)
  :inherit-from `(real ,low ,high)
  :history number
  :description "floating-point number")

(define-presentation-method accept ((type float) stream (view textual-view) &key)
  (simple-lisp-object-parser type stream #'rationalp #'float))


(define-presentation-type integer (&optional low high)
  :options ((base 10 nil (integer 2 36))
            (radix nil nil boolean (:prompt "Display the base")))
  :history number
  :inherit-from `((rational ,low ,high)
                  :base ,base :radix ,radix))

;;; Can't use the inherited method because it can return the wrong answer
;;; for mixed inclusive/exclusive subranges of integer
(define-presentation-method presentation-subtypep ((subtype integer) putative-supertype)
  (multiple-value-bind (low1 high1)
      (with-presentation-type-parameters (integer subtype)
        (values (rangecase low
                  (exclusive (1+ low))
                  (otherwise low))
                (rangecase high
                  (exclusive (1- high))
                  (otherwise high))))
    (multiple-value-bind (low2 high2)
        (with-presentation-type-parameters (integer putative-supertype)
          (values (rangecase low
                    (exclusive (1+ low))
                    (otherwise low))
                  (rangecase high
                    (exclusive (1- high))
                    (otherwise high))))
      (values
        (and (or (eq low2 '*)
                 (and (not (eq low1 '*))
                      (>= low1 low2)))
             (or (eq high2 '*)
                 (and (not (eq high1 '*))
                      (<= high1 high2))))
        t))))


;;;; Character and String Presentation Types

(define-presentation-type character ()
  :inherit-from t)                        ;enforce CL definition

;; The inherited PRESENT method is fine
(define-presentation-method accept ((type character) stream (view textual-view) &key)
  (values (read-char stream)))


;; :INHERIT-FROM T is to avoid inheriting from ARRAY, which is not a presentation type
(define-presentation-type string (&optional length)                ;exact length
  :inherit-from t                        ;enforce CL definition
  :history t)

(define-presentation-method presentation-type-specifier-p ((type string))
  (or (eq length '*)
      (typep length '(integer 0 *))))

(define-presentation-method presentation-typep (object (type string))
  (or (eq length '*)
      (= (length object) length)))

(define-presentation-method presentation-subtypep ((subtype string) supertype)
  (let ((length1 (with-presentation-type-parameters (string subtype) length))
        (length2 (with-presentation-type-parameters (string supertype) length)))
    (values (or (eq length2 '*) (eql length1 length2)) t)))

(define-presentation-method describe-presentation-type ((type string) stream plural-count)
  (default-describe-presentation-type "string" stream plural-count)
  (unless (eq length '*)
    (format stream " of length ~D" length)))

(define-presentation-method present (object (type string) stream (view textual-view)
                                     &key acceptably)
  (if acceptably
      (with-standard-io-syntax
        (write object :stream stream :escape t))
      (write-string object stream)))

(define-presentation-method present (object (type string) stream (view dialog-view-mixin)
                                     &key acceptably)
  (if (or acceptably (zerop (length object)))
      (with-standard-io-syntax
        (write object :stream stream :escape t))
      (write-string object stream)))

(define-presentation-method accept ((type string) stream (view textual-view) &key)
  (let ((token (read-token stream)))
    (unless (or (eq length '*) (= (length token) length))
      (input-not-of-required-type token type))
    token))


;;;; Pathname Presentation Type

#-Minima (progn

;; :INHERIT-FROM T in order to avoid inheriting from any structure classes
(define-presentation-type pathname ()
  :inherit-from t                        ;enforce CL definition
  :history t
  :options ((default-type nil)
            (default-version #-Allegro :newest #+Allegro :unspecific)
            (merge-default t)))

;;; PATHNAME is supposed to be a built-in-class, but since it's missing in Genera,
;;; we need this method.
(define-presentation-method presentation-typep (object (type pathname))
  (pathnamep object))

(define-presentation-method present (object (type pathname) stream (view textual-view)
                                     &key acceptably)
  (write-token (namestring object) stream :acceptably acceptably))

(define-presentation-method presentation-default-preprocessor
                            (default (type pathname) &key default-type)
  (when (null default)
    (setq default *default-pathname-defaults*))
  (with-presentation-type-options (pathname default-type)
    (when merge-default
      (setq default
            (make-pathname :host (pathname-host default)
                           :device (pathname-device default)
                           :directory (pathname-directory default)
                           :name (pathname-name default)
                           :type (or default-type (pathname-type default))
                           :version default-version))))
  (values default default-type))

(define-presentation-method accept ((type pathname) stream (view textual-view) &key default)
  (when (null default)
    (setq default *default-pathname-defaults*))
  (when (and merge-default default-type)
    (setq default (make-pathname :host (pathname-host default)
                                 :device (pathname-device default)
                                 :directory (pathname-directory default)
                                 :name (pathname-name default)
                                 :type default-type
                                 :version default-version)))
  (let ((buffer-start (stream-scan-pointer stream)))
    (multiple-value-bind (pathname success string)
        (complete-input stream #'(lambda (string action)
                                   (pathname-complete string action default))
                        :allow-any-input t :help-displays-possibilities nil)
      (declare (ignore success))
      (handler-bind ((error
                       #'(lambda (anerror)
                           (declare (ignore anerror))
                           (simple-parse-error "Error parsing pathname string ~A" string))))
        (unless pathname
          (setq pathname (parse-namestring string nil default)))
        (when merge-default
          (setq pathname (merge-pathnames pathname default default-version))))
      (unless (stream-rescanning-p stream)
        (presentation-replace-input stream pathname type view
                                    :buffer-start buffer-start))
      (values pathname type))))

;;; The FUNCTION to complete-input is called with two arguments,
;;; string-so-far and a completion type, which is one of :complete or :complete-limited
;;; for now.  It returns string, success, object, nmatches (first-interesting-index?)
#+Genera
(defun pathname-complete (string action &optional (default *default-pathname-defaults*))
  (if (or (eq action :possibilities)
          (eq action :apropos-possibilities))
      (pathname-complete-1 string action default)
      (multiple-value-bind (string success)
          (fs:complete-pathname default string nil :newest :read)
        (values string success (and success (pathname string)) 1))))

#+CCL-2
(defun pathname-complete (string action &optional (default *default-pathname-defaults*))
  #-aclpc (declare (ignore default))                        ;--- for now
  ;; Slow but accurate
  (let ((pathname (lisp:pathname string))
        completions)
    (cond ((pathname-version pathname)
           ;; get around file-system braino I don't know how to resolve
           (setq completions (directory pathname)))
          (t (setq completions (directory (concatenate 'string string "*")))))
    (complete-from-possibilities string completions '(#\space)
                                 :action action :name-key #'namestring :value-key #'identity)))

#-(or Genera CCL-2)
(defun pathname-complete (string action &optional (default *default-pathname-defaults*))
  (pathname-complete-1 string action default))

#-CCL-2
(defun pathname-complete-1 (string action &optional (default *default-pathname-defaults*))
  ;; Slow but accurate
  (let* ((pathname (pathname string))
         (merged-pathname (merge-pathnames pathname default))
         completions)
    (cond ((pathname-version pathname)
           ;; Get around file-system braino I don't know how to resolve
           (setq completions (directory pathname)))
          (t
           (setq completions (directory
                               (make-pathname :host (pathname-host merged-pathname)
                                              :device (pathname-device merged-pathname)
                                              :directory (pathname-directory merged-pathname))))))
    ;; Now prune out all completions that don't start with the string
    (let ((name (pathname-name pathname))
          (type (pathname-type pathname)))
      (setq completions
            (delete-if-not
              #'(lambda (pn)
                  (let* ((pn-name (pathname-name pn))
                         (pn-type (pathname-type pn)))
                    (cond (type
                           (and
                             (string-equal pn-name name)
                             (let ((s (search type pn-type :test #'char-equal)))
                               (and s (zerop s)))))
                          (t
                           (let ((s (search name (pathname-name pn)
                                            :test #'char-equal)))
                             (if (eq action :apropos-possibilities)
                                 (not (null s))
                                 (and s (zerop s))))))))
              completions))
      (when (null type)
        ;; If the user didn't supply a file type, don't burden him with all
        ;; sorts of version numbers right now.
        (let ((new-completions nil))
          (dolist (pathname completions)
            (pushnew (make-pathname :host (pathname-host pathname)
                                    :device (pathname-device pathname)
                                    :directory (pathname-directory pathname)
                                    :name (pathname-name pathname)
                                    :type (pathname-type pathname)) new-completions))
          (setq completions (nreverse new-completions)))))
    (complete-from-possibilities string completions '(#\space)
                                 :action action
                                 :name-key #'namestring :value-key #'identity)))

)        ;#-Minima


;;;; "One-of" Presentation Types

(define-presentation-type completion (&optional (sequence nil)
                                      &key (test 'eql) (value-key 'identity))
  :options ((name-key 'completion-default-name-key)
            (documentation-key 'completion-default-doc-key)
            (partial-completers '(#\Space))
            ;; These are here so that they can be specialized in dialogs
            (printer #'write-token)
            (highlighter #'highlight-completion-choice)))

(defun completion-default-name-key (item)
  (typecase item
    (string item)
    (null "NIL")
    (cons (string (car item)))
    (symbol (string-capitalize (symbol-name item)))
    (otherwise (princ-to-string item))))

(defun completion-default-doc-key (item)
  (declare (ignore item))
  nil)

(defun completion-alist-value-key (item)
  (cond ((atom item) item)
        ((atom (cdr item)) (cdr item))
        ((atom (cddr item)) (cadr item))
        (t (getf (cdr item) :value))))

(defun completion-alist-default-doc-key (item)
  (and (consp item)
       (consp (cdr item))
       (consp (cddr item))
       (getf (cdr item) :documentation)))

(define-presentation-method present (object (type completion) stream (view textual-view)
                                     &key acceptably)
  (funcall printer
            (funcall name-key (find object sequence :key value-key :test test))
            stream :acceptably acceptably))

(define-presentation-method accept ((type completion) stream (view textual-view) &key)
  (flet ((possibility-printer (possibility type stream)
            (with-output-as-presentation (stream (funcall value-key (second possibility)) type)
              (funcall printer
                       (funcall name-key (find (second possibility) sequence
                                               :key value-key :test test))
                       stream))))
    (declare (dynamic-extent #'possibility-printer))
    (values
      (completing-from-suggestions
       (stream :partial-completers partial-completers
               :possibility-printer #'possibility-printer
               :help-displays-possibilities (<= (length sequence) 10))
       (flet ((suggest-item (item)
                (suggest (funcall name-key item) (funcall value-key item))))
         (declare (dynamic-extent #'suggest-item))
         (map nil #'suggest-item sequence))))))

(define-presentation-method accept-present-default ((type completion) stream
                                                    (view dialog-view-mixin)
                                                    default default-supplied-p
                                                    present-p query-identifier &key)
  (declare (ignore default-supplied-p present-p))
  (accept-values-choose-from-sequence stream sequence
                                      value-key default test
                                      type query-identifier
                                      #'(lambda (choice-value query-value)
                                          (declare (ignore query-value))
                                          choice-value)
                                      highlighter))

(defun highlight-completion-choice (continuation object stream)
  (with-text-face (stream :bold)
    (funcall continuation object stream)))

;;--- Limit output if length is too long?
(define-presentation-method describe-presentation-type ((type completion) stream plural-count)
  (when (= (length sequence) 1)
    (return-from describe-presentation-type
      (default-describe-presentation-type (funcall name-key (elt sequence 0))
                                          stream
                                          (unless (eq plural-count 1)        ;suppress a/an
                                            plural-count))))
  (etypecase plural-count
    ((member nil 1)
     (write-string "one of " stream))
    ((member t)
     (write-string "one or more of " stream))
    ((integer 2 *)
     (format stream "~R of " plural-count)))
  (let ((position 0)
        (last (1- (length sequence))))
    (map nil #'(lambda (item)
                 (funcall printer (funcall name-key item) stream)
                 (unless (= position last)
                   (write-string (if (= last 1) " " ", ") stream)
                   (when (= (incf position) last)
                     (write-string "or " stream))))
         sequence)))

(define-presentation-method presentation-typep (object (type completion))
  (and (position object sequence :key value-key :test test) t))

;;; I admit this is not the most efficient way in the world to compute this
(define-presentation-method presentation-subtypep ((subtype completion) supertype)
  (multiple-value-bind (sequence1 test1 value-key1)
      (with-presentation-type-parameters (completion subtype)
        (values sequence test value-key))
    (multiple-value-bind (sequence2 test2 value-key2)
        (with-presentation-type-parameters (completion supertype)
          (values sequence test value-key))
      (if (and (eq test1 test2) (eq value-key1 value-key2))
          (values (null (set-difference sequence1 sequence2 :test test1 :key value-key1)) t)
          (values nil nil)))))

;;; A convenient way to assemble a presentation type specifier with
;;; options equal to their default values omitted.  This is only useful
;;; for abbreviation expanders, not for :inherit-froms.
(defun make-presentation-type-specifier (type-name-and-parameters &rest options)
  (declare (dynamic-extent options))
  (with-presentation-type-decoded (type-name) type-name-and-parameters
    (let ((parent-options (presentation-type-options type-name))
          (actual-options nil))
      (do () ((null options))
        (let* ((keyword (pop options))
               (value (pop options))
               (item (find keyword parent-options :key #'parameter-specifier-keyword)))
          (unless (and (consp item)
                       (consp (cdr item))
                       (typecase (cadr item)
                         ((and atom (or keyword (member t nil) (not symbol)))
                          (equal value (cadr item)))
                         (cons
                           (and (eq (caadr item) 'quote)
                                (equal value (cadadr item))))))
            (push value actual-options)
            (push keyword actual-options))))
      (if actual-options
          (cons type-name-and-parameters actual-options)
          type-name-and-parameters))))


(define-presentation-type-abbreviation member (&rest elements)
  (make-presentation-type-specifier `(completion ,elements)
                                    :name-key name-key
                                    :documentation-key documentation-key
                                    :partial-completers partial-completers
                                    :printer printer
                                    :highlighter highlighter)
  :options ((name-key 'completion-default-name-key)
            (documentation-key 'completion-default-doc-key)
            (partial-completers '(#\Space))
            (printer #'write-token)
            (highlighter #'highlight-completion-choice)))


(define-presentation-type-abbreviation member-sequence (sequence &key (test 'eql))
  (make-presentation-type-specifier `(completion ,sequence :test ,test)
                                    :name-key name-key
                                    :documentation-key documentation-key
                                    :partial-completers partial-completers
                                    :printer printer
                                    :highlighter highlighter)
  :options ((name-key 'completion-default-name-key)
            (documentation-key 'completion-default-doc-key)
            (partial-completers '(#\Space))
            (printer #'write-token)
            (highlighter #'highlight-completion-choice)))


(define-presentation-type-abbreviation member-alist (alist &key (test 'eql))
  (make-presentation-type-specifier `(completion ,alist
                                                 :test ,test
                                                 :value-key completion-alist-value-key)
                                    :name-key name-key
                                    :documentation-key documentation-key
                                    :partial-completers partial-completers
                                    :printer printer
                                    :highlighter highlighter)
  :options ((name-key 'completion-default-name-key)
            (documentation-key 'completion-alist-default-doc-key)
            (partial-completers '(#\Space))
            (printer #'write-token)
            (highlighter #'highlight-completion-choice)))


;;;; "Some-of" Presentation Types

(define-presentation-type subset-completion (&optional (sequence nil)
                                             &key (test 'eql) (value-key 'identity))
  :options ((name-key 'completion-default-name-key)
            (documentation-key 'completion-default-doc-key)
            (partial-completers '(#\Space))
            (separator #\,)
            (echo-space t)
            ;; These are here so that they can be specialized in dialogs
            (printer #'write-token)
            (highlighter #'highlight-completion-choice)))

;;--- Is there supposed to be a way to input and output the empty list?
;;--- In DW there is "no tokens selected" on output and blank on input
;;--- which presumably only works when the default is NIL.
(define-presentation-method present (object (type subset-completion) stream (view textual-view)
                                     &key acceptably)
  (let ((length (length object)))
    (dotimes (i length)
      (funcall printer
               (funcall name-key (find (elt object i) sequence
                                       :key value-key :test test))
               stream :acceptably acceptably)
      (unless (= i (1- length))
        (write-char separator stream)
        (when (and echo-space (not (eql separator #\space)))
          (write-char #\space stream))))))

(define-presentation-method accept ((type subset-completion) stream (view textual-view) &key)
  (let ((list nil)
        (element-type `(completion ,sequence :test ,test :value-key ,value-key))
        (separators (list separator))
        (echo-string
          (if (eql separator #\space)
              " "
              (make-array 2 :initial-contents (list separator #\space)))))
    (flet ((possibility-printer (possibility type stream)
             (with-output-as-presentation (stream (list (funcall value-key (second possibility))) type)
               (funcall printer
                        (funcall name-key (find (second possibility) sequence
                                                :key value-key :test test))
                        stream))))
      (declare (dynamic-extent #'possibility-printer))
      (loop
        (when (eq (read-gesture :stream stream :peek-p t :timeout 0) *end-of-file-marker*)
          (return-from accept (nreverse list)))
        (let ((element
                (with-delimiter-gestures (separators)
                  (completing-from-suggestions
                      (stream :partial-completers partial-completers
                              :possibility-printer #'possibility-printer
                              :help-displays-possibilities (<= (length sequence) 10))
                    (flet ((suggest-item (item)
                             (suggest (funcall name-key item)
                                      (funcall value-key item))))
                      (declare (dynamic-extent #'suggest-item))
                      (map nil #'suggest-item sequence))))))
          (push element list))
        (loop
          (multiple-value-bind (more-to-come object)
              (accept-comma stream element-type view
                            :delimiter-character separator
                            :echo-space echo-space :echo-string-before-blip echo-string)
            (ecase more-to-come
              ((nil)
               (return-from accept
                 (nreverse list)))
              ((t) (return))        ;return to outer loop, call completing-from-suggestions again
              ((:accepted)
               (push object list)))))))))

;;; A handy routine for accepting comma-separated lists in textual views.
;;; If the next character is a comma, absorbs it and any following space and returns T.
;;; Otherwise it absorbs no characters and returns NIL.
;;; Or if a presentation of type desired-type is clicked, return :ACCEPTED, object, and type
(defun accept-comma (stream desired-type view
                     &key (delimiter-character #\,)
                          echo-space echo-string-before-blip)
  (declare (values more-to-come object type))
  (with-input-context (desired-type) (object type)
       (loop
         (let ((char (read-gesture :stream stream)))
           (cond ((typep char 'pointer-button-event)
                  ;; These events come through even though they have already been handled
                  ;; just ignore them, as read-token does
                  )
                 ((eql char delimiter-character)
                  (when (and echo-space (not (eql delimiter-character #\space)))
                    (if (stream-rescanning-p stream)
                        (let ((space (read-gesture :stream stream)))
                          (unless (whitespace-char-p space)
                            ;; It wasn't a space so put it back
                            (unread-gesture space :stream stream)))
                        ;; Add a space if the comma was just typed in for the first time
                        (replace-input stream " ")))
                  (return-from accept-comma t))
                 (t
                  ;; It wasn't a comma so put it back
                  (unread-gesture char :stream stream)
                  (return-from accept-comma nil)))))
     (t
       (when echo-string-before-blip
         (replace-input stream echo-string-before-blip))
       (presentation-replace-input stream object type view)
       (values :accepted object type))))

(define-presentation-method accept-present-default ((type subset-completion)
                                                    stream (view dialog-view-mixin)
                                                    default default-supplied-p
                                                    present-p query-identifier &key)
  (declare (ignore default-supplied-p present-p))
  (accept-values-choose-from-subset stream sequence
                                    value-key default test
                                    type query-identifier
                                    #'(lambda (choice-value query-value)
                                        (declare (ignore query-value))
                                        choice-value)
                                    highlighter))

;;--- Limit output if length is too long?
(define-presentation-method describe-presentation-type ((type subset-completion) stream
                                                        plural-count)
  (default-describe-presentation-type "subset" stream plural-count)
  (write-string " of " stream)
  (let ((position 0)
        (last (1- (length sequence))))
    (map nil #'(lambda (item)
                 (write-string (funcall name-key item) stream)
                 (unless (= position last)
                   (write-string (if (= last 1) " " ", ") stream)
                   (when (= (incf position) last)
                     (write-string "and " stream))))
         sequence)))

(define-presentation-method presentation-typep (object (type subset-completion))
  (and (listp object)
       (every #'(lambda (element)
                  (position element sequence :key value-key :test test))
              object)
       t))

(define-presentation-method presentation-subtypep ((subtype subset-completion) supertype)
  (multiple-value-bind (sequence1 test1 value-key1)
      (with-presentation-type-parameters (subset-completion subtype)
        (values sequence test value-key))
    (multiple-value-bind (sequence2 test2 value-key2)
        (with-presentation-type-parameters (subset-completion supertype)
          (values sequence test value-key))
      (if (and (eq test1 test2) (eq value-key1 value-key2))
          (values (null (set-difference sequence1 sequence2 :test test1 :key value-key1)) t)
          (values nil nil)))))


(define-presentation-type-abbreviation subset (&rest elements)
  (make-presentation-type-specifier `(subset-completion ,elements)
                                    :name-key name-key
                                    :documentation-key documentation-key
                                    :partial-completers partial-completers
                                    :separator separator
                                    :echo-space echo-space
                                    :printer printer
                                    :highlighter highlighter)
  :options ((name-key 'completion-default-name-key)
            (documentation-key 'completion-default-doc-key)
            (partial-completers '(#\Space))
            (separator #\,)
            (echo-space t)
            (printer #'write-token)
            (highlighter #'highlight-completion-choice)))


(define-presentation-type-abbreviation subset-sequence (sequence &key (test 'eql))
  (make-presentation-type-specifier `(subset-completion ,sequence :test ,test)
                                    :name-key name-key
                                    :documentation-key documentation-key
                                    :partial-completers partial-completers
                                    :separator separator
                                    :echo-space echo-space
                                    :printer printer
                                    :highlighter highlighter)
  :options ((name-key 'completion-default-name-key)
            (documentation-key 'completion-default-doc-key)
            (partial-completers '(#\Space))
            (separator #\,)
            (echo-space t)
            (printer #'write-token)
            (highlighter #'highlight-completion-choice)))


(define-presentation-type-abbreviation subset-alist (alist &key (test 'eql))
  (make-presentation-type-specifier `(subset-completion ,alist
                                                        :test ,test
                                                        :value-key completion-alist-value-key)
                                    :name-key name-key
                                    :documentation-key documentation-key
                                    :partial-completers partial-completers
                                    :separator separator
                                    :echo-space echo-space
                                    :printer printer
                                    :highlighter highlighter)
  :options ((name-key 'completion-default-name-key)
            (documentation-key 'completion-alist-default-doc-key)
            (partial-completers '(#\Space))
            (separator #\,)
            (echo-space t)
            (printer #'write-token)
            (highlighter #'highlight-completion-choice)))


;;;; Sequence Presentation Types

(define-presentation-type sequence (element-type)
  :inherit-from t                        ;enforce CL definition
  :parameters-are-types t
  :options ((separator #\,)
            (echo-space t)))

(define-presentation-method describe-presentation-type :after
                            ((type sequence) stream plural-count)
  (declare (ignore plural-count))        ;primary method gets it
  (write-string " of " stream)
  (describe-presentation-type element-type stream t))

(define-presentation-method presentation-type-specifier-p ((type sequence))
  (presentation-type-specifier-p element-type))

(define-presentation-method presentation-typep (object (type sequence))
  (and (typep object 'sequence)
       (not (and (consp object) (not (null (cdr (last object))))))        ;not a dotted list
       (every #'(lambda (element) (presentation-typep element element-type)) object)))

(define-presentation-method presentation-subtypep ((subtype sequence) supertype)
  (let ((element-type-1 (with-presentation-type-parameters (sequence subtype) element-type))
        (element-type-2 (with-presentation-type-parameters (sequence supertype) element-type)))
    (presentation-subtypep-1 element-type-1 element-type-2)))

(define-presentation-method present (sequence (type sequence) stream (view textual-view)
                                     &rest options)
  (declare (dynamic-extent options))
  (apply #'present-sequence sequence type stream view options))

(define-presentation-method present (sequence (type sequence) stream (view dialog-view-mixin)
                                     &rest options)
  (declare (dynamic-extent options))
  ;; Give the user a target to click on when the sequence is empty
  (if (null sequence)
      (with-text-face (stream :italic)
        (describe-presentation-type type stream 1))
      (apply #'present-sequence sequence type stream view options)))

;;--- Is it right that an empty sequence prints nothing at all?
(defun present-sequence (sequence type stream view &rest options)
  (declare (dynamic-extent options))
  (with-presentation-type-parameters (sequence type)
    (with-presentation-type-options (sequence type)
      (let ((position 0)
            (last (1- (length sequence))))
        (map nil #'(lambda (item)
                     (apply #'present item element-type :stream stream :view view options)
                     (unless (= position last)
                       (write-char separator stream)
                       (when (and echo-space (not (eql separator #\space)))
                         (write-char #\space stream)))
                     (incf position))
             sequence)))))

(define-presentation-method presentation-default-preprocessor
                            (default (type sequence) &key default-type)
  ;; The default can be either a sequence or an element.  If it's an element,
  ;; coerce it to be a sequence consisting of that single element.
  (unless (and (presentation-subtypep-1 default-type type)
               (presentation-typep default type))
    (with-presentation-type-parameters (sequence default-type)
      (setq default (list default)
            default-type `(sequence ,(if (eq default-type type) element-type default-type)))))
  (values default default-type))

(define-presentation-method accept ((type sequence) stream (view textual-view)
                                    &key (default nil default-supplied-p) (default-type type))

  (when (null default)
    (setq default-supplied-p nil))
  ;; Read sequence elements, defaulting each from the previous
  (let ((values nil)
        (types nil)
        (separators (list separator))
        (echo-string
          (if (eql separator #\space)
              " "
              (make-array 2 :initial-contents (list separator #\space))))
        (element-default-type
          (with-presentation-type-decoded (type-name parameters) default-type
            (if (eq type-name 'sequence)
                (first parameters)
                element-type))))
    (loop
      ;; Get the default for the sequence element
      ;;--- This makes c-m-Y yank the wrong thing when a default has been
      ;;--- supplied, but just removing it will cause the following to lose:
      ;;--- (ACCEPT '(SEQUENCE PATHNAME) :DEFAULT '(#P"FOO"))
      (let ((element-default (and default-supplied-p (pop default)))
            (element-default-type element-default-type)
            (default-supplied-p default-supplied-p))
        (when (and values (null element-default))
          (setq element-default (car (last values))
                element-default-type (car (last types))
                default-supplied-p t))
        (multiple-value-bind (object object-type)
            (accept-sequence-element stream element-type view separators
                                     element-default element-default-type default-supplied-p)
          (push object values)
          (push (or object-type element-type) types)))
      (loop
        (multiple-value-bind (more-to-come object object-type)
            (accept-comma stream element-type view
                          :delimiter-character separator
                          :echo-space echo-space :echo-string-before-blip echo-string)
          (ecase more-to-come
            ((nil)
             (return-from accept
               (values (nreverse values)
                       (if (every #'(lambda (type) (equal type element-type)) types)
                           type
                           `(sequence-enumerated ,@(nreverse types))))))
            ((t) (return))        ;return to outer loop, call accept element-type again
            ((:accepted)
             (push object values)
             (push (or object-type element-type) types))))))))

(defun accept-sequence-element (stream element-type view separators
                                element-default element-default-type default-supplied-p)
  (declare (values object object-type))
  (multiple-value-bind (object object-type)
      (with-input-context (element-type) (object object-type)
           (with-delimiter-gestures (separators)
             (let* ((history (presentation-type-history element-default-type))
                    (*presentation-type-for-yanking*
                      (if history element-default-type *presentation-type-for-yanking*))
                    (default-element
                      (and history
                           (make-presentation-history-element
                             :object element-default :type element-default-type))))
               (if default-supplied-p
                   (if history
                       (with-default-bound-in-history history default-element
                         (funcall-presentation-generic-function accept
                           element-type stream view
                           :default element-default :default-type element-default-type))
                       (funcall-presentation-generic-function accept
                         element-type stream view
                         :default element-default :default-type element-default-type))
                   (funcall-presentation-generic-function accept
                     element-type stream view))))
         ;; The user clicked on an object having the element type
         (t
           (presentation-replace-input stream object object-type view)
           (values object object-type)))
    (values object object-type)))

(defvar *sequence-type-history*
        (make-presentation-type-history 'sequence :history-name "sequence"))

(define-presentation-method presentation-type-history ((type sequence))
  *sequence-type-history*)


(define-presentation-type sequence-enumerated (&rest element-types)
  :parameters-are-types t
  :history t
  :description "sequence"
  :options ((separator #\,)
            (echo-space t)))

;; Useful abbreviation...
(define-presentation-type-abbreviation bounded-sequence (element-type length)
  `((sequence-enumerated ,@(make-list length :initial-element element-type))
    :separator ,separator :echo-space ,echo-space)
  :options ((separator #\,)
            (echo-space t)))

(define-presentation-method presentation-type-specifier-p ((type sequence-enumerated))
  ;; must have at least one parameter
  (and element-types
       (every #'presentation-type-specifier-p element-types)))

(define-presentation-method describe-presentation-type
                            ((type sequence-enumerated) stream plural-count)
  (declare (ignore plural-count))                ;it's too hard to handle
  (cond ((let ((type (first element-types)))
           (dolist (x (cdr element-types) t)
             (unless (equal x type)
               (return nil))))
         ;; Do this if all the element types are the same
         #+(or aclpc acl86win32)
         (format stream "~D " (length element-types))
         (describe-presentation-type 
           (car element-types) stream (length element-types)))
        (t
         (let ((position 0)
               (last (1- (length element-types))))
           (dolist (element-type element-types)
             (describe-presentation-type element-type stream 1)
             (unless (= position last)
               (write-string (if (= last 1) " " ", ") stream)
               (when (= (incf position) last)
                 (write-string "and " stream))))))))

(define-presentation-method presentation-typep (object (type sequence-enumerated))
  (and (typep object 'sequence)
       (not (and (consp object) (not (null (cdr (last object))))))        ;not a dotted list
       (= (length object) (length element-types))
       (every #'(lambda (element element-type)
                  (presentation-typep element element-type))
              object element-types)))

(define-presentation-method presentation-subtypep ((subtype sequence-enumerated) supertype)
  (let ((element-types-1 (with-presentation-type-parameters (sequence-enumerated subtype)
                           element-types))
        (element-types-2 (with-presentation-type-parameters (sequence-enumerated supertype)
                           element-types)))
    (if (and (= (length element-types-1) (length element-types-2))
             (every #'(lambda (element-type-1 element-type-2)
                        (presentation-subtypep-1 element-type-1 element-type-2))
                    element-types-1 element-types-2))
        (values t t)
        (values nil nil))))

(define-presentation-method map-over-presentation-type-supertypes
                            ((type sequence-enumerated) function)
  (map-over-presentation-type-supertypes-augmented type function
    (with-presentation-type-parameters (sequence-enumerated type)
      (let ((first-type (first element-types)))
        (when (and element-types
                   (every #'(lambda (x) (equal x first-type)) (rest element-types)))
          ;; If all of the element types are exactly the same, then include
          ;; SEQUENCE in the list of supertypes
          (with-stack-list (sequence-type 'sequence first-type)
            (funcall function 'sequence sequence-type)))))))

(define-presentation-method present (sequence (type sequence-enumerated)
                                     stream (view textual-view)
                                     &rest options)
  (declare (dynamic-extent options))
  (let ((position 0)
        (last (1- (length sequence))))
    (map nil #'(lambda (item element-type)
                 (apply #'present item element-type :stream stream :view view options)
                 (unless (= position last)
                   (write-char separator stream)
                   (when (and echo-space (not (eql separator #\space)))
                     (write-char #\space stream)))
                 (incf position))
         sequence element-types)))

(define-presentation-method accept ((type sequence-enumerated) stream (view textual-view)
                                    &key (default nil default-supplied-p) (default-type type))
  (when (null default)
    (setq default-supplied-p nil))
  (let ((values nil)
        (types nil)
        (separators (list separator))
        (echo-string
          (if (eql separator #\space)
              " "
              (make-array 2 :initial-contents (list separator #\space))))
        (element-default-types
          (with-presentation-type-decoded (type-name parameters) default-type
            (if (eq type-name 'sequence-enumerated)
                parameters
                element-types))))
    (loop
      (unless element-types (return))
      (let* ((element-type (pop element-types))
             (element-default (and default-supplied-p (pop default)))
             (element-default-type (pop element-default-types)))
        (multiple-value-bind (object object-type)
            (accept-sequence-element stream element-type view separators
                                     element-default element-default-type default-supplied-p)
          (push object values)
          (push (or object-type element-type) types)))
      (loop
        (unless element-types (return))
        (multiple-value-bind (more-to-come object object-type)
            (accept-comma stream (car element-types) view
                          :delimiter-character separator
                          :echo-space echo-space :echo-string-before-blip echo-string)
          (ecase more-to-come
            ((nil)
             (simple-parse-error "~R more item~:P required."
                                 (length element-types)))
            ((t) (return))        ;return to outer loop, call accept element-type again
            ((:accepted)
             (push object values)
             (push (or object-type (car element-types)) types)
             (pop element-types))))))
    (values (nreverse values) (cons 'sequence-enumerated (nreverse types)))))


;;;; "Meta" Presentation Types

(define-presentation-type or (&rest types)
  :parameters-are-types t)

(define-presentation-method presentation-type-specifier-p ((type or))
  ;; must have at least one parameter
  (and types
       (every #'presentation-type-specifier-p types)))

(define-presentation-method presentation-typep (object (type or))
  (some #'(lambda (type) (presentation-typep object type)) types))

(define-presentation-method presentation-subtypep ((subtype or) supertype)
  (with-presentation-type-decoded (nil subtypes) subtype
    (with-presentation-type-decoded (nil supertypes) supertype
      (if (every #'(lambda (subtype)
                     (some #'(lambda (supertype) (presentation-subtypep-1 subtype supertype))
                           supertypes))
                 subtypes)
          (values t t)
          (values nil nil)))))

(define-presentation-method describe-presentation-type ((type or) stream plural-count)
  (let ((length (length types)))
    (dotimes (i length)
      (describe-presentation-type (elt types i) stream plural-count)
      (cond ((= i (1- length)))
            ((= length 2) (write-string " or " stream))
            ((= i (- length 2)) (write-string ", or " stream))
            (t (write-string ", " stream))))))

(define-presentation-method present (object (type or) stream view &rest options)
  (declare (dynamic-extent options))
  (dolist (type types)
    (when (presentation-typep object type)
      (return-from present
        ;; This must call the real present function, rather than using
        ;; CALL-PRESENTATION-GENERIC-FUNCTION, so that a nested presentation
        ;; of the correct type will be created
        ;;--- Maybe.  See RWK comment in the DW version.
        (apply #'present object type :stream stream :view view options))))
  (error "The object ~S is not of type ~S" object type))

(define-presentation-method accept ((type or) stream view
                                    &key (default nil default-supplied-p)
                                         (default-type type))
  (let* ((location (stream-scan-pointer stream))
         last-error
         ;; Since we need to bind a handler for PARSE-ERROR, we cannot call
         ;; ACCEPT down below, since it binds a handler for PARSE-ERROR, too.
         ;; So, we handle the history rigamarole here.  It suffices to use
         ;; the history of the first type if no default-type was supplied.
         (history-type (if (eq default-type type) (first types) default-type))
         (history (presentation-type-history history-type))
         (*presentation-type-for-yanking*
           (if history history-type *presentation-type-for-yanking*))
         (default-element
           (and history
                (make-presentation-history-element
                  :object default :type history-type))))
    (dolist (type types)
      (block fail
        (handler-bind ((parse-error
                         #'(lambda (anerror)
                             (when (and default-supplied-p
                                        (check-for-default stream location
                                                           default default-type view))
                               (return-from accept
                                 (values default default-type)))
                             ;; That parser didn't work, try another on the same input
                             (setq last-error anerror)
                             (setf (stream-scan-pointer stream) location)
                             (return-from fail))))
          (multiple-value-bind (object object-type)
              (if (and default-supplied-p
                       (presentation-typep default type))
                  (if history
                      (with-default-bound-in-history history default-element
                        (funcall-presentation-generic-function accept
                          type stream view
                          :default default :default-type default-type))
                      (funcall-presentation-generic-function accept
                        type stream view
                        :default default :default-type default-type))
                  (funcall-presentation-generic-function accept
                    type stream view))
            (return-from accept
              (values object (or object-type type)))))))
    ;; No parser worked.  Resignal the most recent parse-error.
    (if (typep last-error 'input-not-of-required-type)
        (input-not-of-required-type (input-not-of-required-type-string last-error)
                                    type)
        (simple-parse-error "~A" last-error))))

(define-presentation-method presentation-type-history ((type or))
  (with-presentation-type-parameters (or type)
    (presentation-type-history (first types))))


(define-presentation-type and (&rest types)
  :parameters-are-types t)

(define-presentation-method presentation-type-specifier-p ((type and))
  (labels ((and-presentation-type-specifier-p (type)
             (with-presentation-type-decoded (name parameters) type
               (case name
                 (satisfies (typep (first parameters) '(or symbol function)))
                 (not (and-presentation-type-specifier-p (first parameters)))
                 (otherwise (presentation-type-specifier-p type))))))
    (declare (dynamic-extent #'and-presentation-type-specifier-p))
    (and types                ;must have at least one parameter
         (every #'and-presentation-type-specifier-p types))))

(define-presentation-method presentation-typep (object (type and))
  (every #'(lambda (type) (and-presentation-typep object type)) types))

(defun and-presentation-typep (object type)
  (with-presentation-type-decoded (name parameters) type
    (case name
      (satisfies (funcall (first parameters) object))
      (not (not (and-presentation-typep object (first parameters))))
      (otherwise (presentation-typep object type)))))

;;--- This is only correct if the CPLs of the component types don't intersect
(define-presentation-method map-over-presentation-type-supertypes ((type and) function)
  (funcall function 'and type)
  (dolist (type types)
    (with-presentation-type-decoded (name) type
      (unless (member name '(satisfies not))
        (map-over-presentation-type-supertypes type function)))))

(define-presentation-method describe-presentation-type ((type and) stream plural-count)
  (describe-presentation-type (first types) stream plural-count)
  (let ((first t))
    (dolist (type (rest types))
      (if first (setq first nil) (write-string " and" stream))
      (let ((not nil))
        (block nil
          (loop
            (with-presentation-type-decoded (name parameters) type
              (cond ((eq name 'satisfies)
                     (format stream " that ~:[satisfies~;doesn't satisfy~] ~S"
                             not (first parameters))
                     (return))
                    ((eq name 'not)
                     (setq not (not not))
                     (setq type (first parameters)))
                    (t
                     (format stream " that ~:[is~;is not~] " not)
                     (describe-presentation-type type stream 1)
                     (return))))))))))

(define-presentation-method presentation-subtypep ((subtype and) supertype)
  (with-presentation-type-decoded (nil subtypes) subtype
    (with-presentation-type-decoded (nil supertypes) supertype
      (if (every #'(lambda (supertype)
                     (with-presentation-type-decoded (supertype-name) supertype
                       (if (member supertype-name '(not satisfies))
                           (member supertype subtypes :test #'equal)
                           (some #'(lambda (subtype) (presentation-subtypep-1 subtype supertype))
                                 subtypes))))
                 supertypes)
          (values t t)
          (values nil nil)))))

(define-presentation-method present (object (type and) stream view &rest options)
  (declare (dynamic-extent options))
  (apply-presentation-generic-function present
    object (first types) stream view options))

(define-presentation-method accept ((type and) stream view &rest options)
  (declare (dynamic-extent options))
  (loop
    (multiple-value-bind (object input-type)
        (apply-presentation-generic-function accept
          (first types) stream view options)
      (when (every #'(lambda (type) (and-presentation-typep object type)) (rest types))
        (return-from accept (values object input-type)))
      (input-not-of-required-type object type))))

(define-presentation-method presentation-type-history ((type and))
  (with-presentation-type-parameters (and type)
    (presentation-type-history (first types))))


;;;; Compound Presentation Types

;; BOOLEAN must come after MEMBER so that we can expand this type
;; abbreviation at compile time.
(defparameter *boolean-member-alist* '(("Yes" t) ("No" nil)))
(defparameter *boolean-member-type*
              (expand-presentation-type-abbreviation `(member-alist ,*boolean-member-alist*)))

(define-presentation-type boolean ()
  :inherit-from t)                        ;enforce CL definition

(define-presentation-method accept ((type boolean) stream view &rest options)
  (declare (dynamic-extent options))
  (values
    (apply-presentation-generic-function accept
      *boolean-member-type* stream view options)))

(define-presentation-method present (object (type boolean) stream view &rest options)
  (declare (dynamic-extent options))
  (apply-presentation-generic-function present
    object *boolean-member-type* stream view options))

;; Supplying this gives us a nice "pushbutton" effect for booleans, too
(define-presentation-method accept-present-default ((type boolean) stream
                                                    (view dialog-view-mixin)
                                                    default default-supplied-p
                                                    present-p query-identifier &key)
  (declare (ignore default-supplied-p present-p))
  (accept-values-choose-from-sequence stream *boolean-member-alist*
                                      #'second default #'eql
                                      type query-identifier
                                      #'(lambda (choice-value query-value)
                                          (declare (ignore query-value))
                                          choice-value)
                                      #'(lambda (continuation object stream)
                                          (with-text-face (stream :bold)
                                            (funcall continuation object stream)))))

(define-presentation-method presentation-typep (object (type boolean))
  (or (null object)
      (eq object 't)))


(define-presentation-type-abbreviation token-or-type (tokens type)
  `(or (member-alist ,tokens) ,type))

(define-presentation-type-abbreviation null-or-type (type)
  `(or (member-alist (("None" nil))) ,type))

(define-presentation-type-abbreviation type-or-string (type)
  `(or ,type string))


;;;; Command and Form Presentation Types

(defvar *read-recursive-objects*)

;;--- Maybe this should have WRITE options
(define-presentation-type expression ()
  :options ((auto-activate nil boolean)))

(define-presentation-method presentation-type-history ((type expression))
  (presentation-type-history-for-frame type *application-frame*))

(define-presentation-method accept ((type expression) stream (view textual-view) &key)
  (let ((*read-recursive-objects* nil))
    (with-temporary-string (string)
      (with-activation-gestures ('(:end))
        (read-recursive stream string nil auto-activate))
      (when (input-editing-stream-p stream)
        (rescan-if-necessary stream))
      (multiple-value-bind (expression index)
          (handler-case
              (read-from-string string)
            (error ()
              (simple-parse-error "The input ~S is not a complete Lisp expression."
                                  (evacuate-temporary-string string))))
        ;; Too bad READ-FROM-STRING doesn't take a :JUNK-ALLOWED argument.
        ;; Simulate what it would do
        (unless (>= index (length string))
          (when (find-if-not #'whitespace-char-p string :start index)
            (simple-parse-error "Extra junk ~S found after the expression."
                                (subseq string index))))
        expression))))

(define-presentation-method present (object (type expression) stream (view textual-view)
                                     &key (acceptably *print-readably*))
  (print-recursive object stream :make-presentation nil :readably acceptably))

(define-presentation-method presentation-typep (object (type expression))
  (declare (ignore object))                        ;everything is an expression
  t)


;; Same as EXPRESSION except for quoting issues in some presentation translators
;;--- Maybe this should have WRITE options
(define-presentation-type form ()
  :options ((auto-activate nil boolean))
  ;; Handling of AUTO-ACTIVATE is done below...
  :inherit-from `((expression) :auto-activate ,auto-activate))

(define-presentation-method presentation-type-history ((type form))
  ;; Share history with EXPRESSION
  (presentation-type-history-for-frame 'expression *application-frame*))

;;; this overwrites the method defined by the above
;;; define-presentation-type form. This doesn't seem very satisfactory

(define-presentation-method map-over-presentation-type-supertypes ((type form) function)
  (with-presentation-type-decoded (name) type
    (funcall function name type)
    (with-stack-list (new-name 'command-or-form
                               :command-table (frame-command-table *application-frame*))
      (with-stack-list (new-type new-name :auto-activate auto-activate)
        (funcall function 'command-or-form new-type))
      (with-stack-list (new-name 'expression)
        (with-stack-list (new-type new-name :auto-activate auto-activate)
          (map-over-presentation-type-supertypes new-type function))))))

(defvar *char-associations* '((#\( . #\))
                              (#\" . #\")
                              (#\| . #\|)))

;;; Ignore issues like comments for now.

(defun read-recursive (stream input-buffer desired-delimiter auto-activate)
  (let ((top-level (not desired-delimiter)))
    (flet ((read-recursive-1 ()
             (let ((char (read-char stream nil :eof)))
               (cond
                ((eq char :eof)
                 (return-from read-recursive))

                ;; ignore leading space
                ((and (zerop (fill-pointer input-buffer))
                      (whitespace-char-p char)))

                ;; ALWAYS terminate with activation gesture or delimiter
                ;; gesture at top level
                ((or (activation-gesture-p char)
                     (and top-level (delimiter-gesture-p char)))
                 (unread-char char stream)
                 (return-from read-recursive))

                ((not (or (ordinary-char-p char)
                          (diacritic-char-p char)))
                 (beep stream))

                ((char-equal char #\\) ;check for quoting character
                 (vector-push-extend char input-buffer)
                 (let ((escaped-char (read-char stream nil)))
                   (when escaped-char
                     (vector-push-extend escaped-char input-buffer))))

                (t
                 (vector-push-extend char input-buffer)

                 (if (and desired-delimiter
                          (char-equal char desired-delimiter))
                     (return-from read-recursive)
                   (let ((other-delimiter (cdr (assoc char *char-associations*))))
                     (when other-delimiter
                       (read-recursive stream input-buffer other-delimiter
                                       auto-activate)
                       (when (and top-level
                                  auto-activate)
                         (return-from read-recursive))))))))))
      (if top-level
          (loop
            (read-recursive-1))
        (loop
          (with-input-context ('expression)
            (object type)
            (read-recursive-1)
            (t
             ;; Put this thing into the input editor's buffer
             (when (input-editing-stream-p stream)
               (presentation-replace-input stream object
                                           type +textual-view+))
             ;; And into the buffer we ourselves are maintaining
             (let ((n (length *read-recursive-objects*)))
               (setq *read-recursive-objects*
                 (nconc *read-recursive-objects* (list object)))
               (doseq (char (format nil " #.(~S ~D ~S) "
                                    'nth n '*read-recursive-objects*))
                      (vector-push-extend char input-buffer))))))))))


(defun print-recursive (object stream
                        &key (make-presentation t)
                             (length *print-length*)
                             (level *print-level*)
                             (readably *print-readably*)
                             #+ignore (radix *print-radix*)
                             #+ignore (base *print-base*)
                             #+ignore (escape *print-escape*)
                             #+ignore (case *print-case*)
                             #+ignore (gensym *print-gensym*)
                             #+ignore (array *print-array*))
  (let ((*print-length* length)
        (*print-level* level)
        (*print-readably* readably)
        #+ignore (*print-radix* radix)
        #+ignore (*print-base* base)
        #+ignore (*print-escape* escape)
        #+ignore (*print-case* case)
        #+ignore (*print-gensym* gensym)
        #+ignore (*print-array* array))
    (print-recursive-1 object stream length level make-presentation)))

(defun print-recursive-1 (object stream length level make-presentation)
  (flet ((body (object stream)
           (cond ((atom object)
                  #-(or aclpc acl86win32) (write object :escape t :stream stream)
                  #+(or aclpc acl86win32) (clim-lisp:format stream "~S" object)
                  )
                 ((eq (first object) 'quote)
                  (write-string "'" stream)
                  (print-recursive-1 (second object) stream length level t))
                 ((eq (first object) 'function)
                  (write-string "#'" stream)
                  (print-recursive-1 (second object) stream length level t))
                 (t
                  (write-string "(" stream)
                  (let ((count 0))
                    (dorest (elements object)
                      (when (and length (> (incf count) length))
                        (write-string "..." stream)
                        (return))
                      (let ((element (first elements))
                            (new-level (and level (1- level))))
                        (when (and new-level (zerop new-level))
                          (write-string "..." stream)
                          (return))
                        (print-recursive-1 element stream length new-level t)
                        (cond ((null (rest elements)))
                              ((atom (setq element (rest elements)))
                               (write-string " . " stream)
                               (print-recursive-1 element stream length new-level t)
                               (return))
                              (t (write-string " " stream))))))
                  (write-string ")" stream)))))
    ;;--- WITH-OUTPUT-AS-PRESENTATION should actually do this for us!
    (if (and (output-recording-stream-p stream) make-presentation)
        (with-output-as-presentation (stream object 'expression
                                      ;; Better highlighting performance!
                                      :single-box :highlighting)
          (body object stream))
        (body object stream))))


;;; Genera compatibility

#+Genera
(defvar *dw-type-to-clim-type-alist*
        '((sys:expression expression t)
          (sys:form form t)
          (dbg:named-locative-slot expression t safe-location-contents)
          (dbg:named-form-slot expression t eval)
          ;; The four most common are at the front, and here are the rest
          (t t)
          (null null)
          (scl:boolean boolean)
          (symbol symbol)
          (keyword keyword)
          (number number (:base :radix :exact-float-value))
          (complex complex (:base :radix :exact-float-value))
          (real real (:exact-float-value))
          (rational rational)
          (ratio ratio)
          (float float (:exact-float-value))
          (integer integer)
          (character character)
          (string string (:delimiters))
          (scl:pathname pathname (:default-name :dont-merge-default :direction :format))
          (member member (:highlighting-test))
          (dw:member-sequence member-sequence)
          (dw:alist-member member-alist (:convert-spaces-to-dashes :style-keyword
                                         :highlighting-test :fill-p :row-wise))
          (subset subset (:highlighting-test))
          (sequence-subset subset-sequence (:highlighted-style-key :sensitive-predicate
                                            :style-key :fill-p :row-wise))
          (alist-subset subset-alist (:convert-spaces-to-dashes :style-keyword
                                      :highlighting-test :fill-p :row-wise))
          (sequence sequence (:sequence-delimiter :element-default))
          (scl:sequence-enumerated sequence-enumerated (:sequence-delimiter :element-default))
          (or or)
          (and and)
          (scl:null-or-type null-or-type)
          (scl:token-or-type token-or-type)
          (scl:type-or-string type-or-string)))

#+Genera
(defun safe-location-contents (location)
  (if (scl:location-boundp location)
      (scl:location-contents location)
      "**unbound**"))

#+Genera
(defun dw-type-to-clim-type (object dw-type)
  (declare (values new-object clim-type changed-p))
  (dw:with-type-decoded (type-name data-args pr-args) dw-type
     (let ((new (assoc type-name *dw-type-to-clim-type-alist*)))
       (if new
           (let ((new-type (second new))
                 (removals (third new))
                 (coercion (or (fourth new) #'identity)))
             (setq object (funcall coercion object))
             (if (eq removals 't)
                 (if data-args
                     (values object `((,new-type ,@data-args)) t)
                     (values object new-type t))
                 (with-keywords-removed (pr-args pr-args removals)
                   (if pr-args
                       (values object `((,new-type ,@data-args) ,@(evacuate-list pr-args)))
                       (if data-args
                           (values object `((,new-type ,@data-args)) t)
                           (values object new-type t))))))
           (values object dw-type nil)))))
