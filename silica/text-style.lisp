;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-
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
;; $Id: text-style.lisp,v 2.6.16.2 2006/10/10 20:27:49 afuchs Exp $

(in-package :silica)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
;;; Portions copyright (c) 1989, 1990 International Lisp Associates."

;;; To consider: how to merge Genera styles with styles.

(define-protocol-class text-style ())

(defclass standard-text-style (text-style)
     ((family :initarg :family :initform nil :reader text-style-family)
      (face   :initarg :face   :initform nil)
      (size   :initarg :size   :initform nil :reader text-style-size)
      (index  :initarg :index  :initform nil)))

(defmethod text-style-face ((style standard-text-style))
  (with-slots (face) style
    (face-code->face face)))

(define-constructor make-text-style-1 standard-text-style (family face size index)
  :family family :face face :size size :index index)

(defmethod text-style-components ((style standard-text-style))
  (with-slots (family face size) style
    (values family (face-code->face face) size)))

#+(or aclpc acl86win32)
(defmethod fully-merged-text-style-p ((style standard-text-style))
  (with-slots (family face size) style
    (and family face size)))

(defmethod make-load-form ((object standard-text-style) #-aclpc &optional #-aclpc environment)
  #-aclpc (declare (ignore environment))
  (with-slots (family face size) object
    `(make-text-style ,family ,face ,size)))

(defmethod print-object ((style standard-text-style) stream)
  #-(or aclpc acl86win32)
  (print-unreadable-object (style stream :type t :identity t)
    (with-slots (family face size) style
      (format stream "~S ~S ~S" family  (face-code->face face) size)))
  #+(or aclpc acl86win32)
  (with-slots (family face size) style
    (let ((true-face (face-code->face face)))
      (macrolet ((do-it ()
                   `(format stream "~S.~S.~S" family true-face size)))
        (if *print-escape*
            (print-unreadable-object (style stream :type t :identity t)
              (do-it))
            (do-it))))))

(defclass device-font (text-style)
     ((display-device :initarg :display-device :reader device-font-display-device)
      (font-name :initarg :font-name :accessor device-font-name)))

(defun make-device-font-text-style (display-device font-name)
  (make-instance 'device-font :display-device display-device :font-name font-name))

(defmethod print-object ((df device-font) stream)
  (print-unreadable-object (df stream :type t :identity t)
    (format stream "~A" (slot-value df 'font-name))))

(defconstant +maximum-text-style-index+ 256)

(defvar *standard-character-set* 0)

(defvar *all-character-sets*
    #+allegro
    (excl:ics-target-case
     (:+ics nil)
     (:-ics *standard-character-set*))
    #-allegro *standard-character-set*)

(defvar *null-text-style*)
(defvar *undefined-text-style*)
(defvar *default-text-style*)

(defvar *text-style-index-table*)
(defvar *next-text-style-index*)
(defvar *text-style-intern-table*)
(defvar *text-style-merging-cache*)

(defun initialize-text-style-tables ()
  (setf *null-text-style* (make-text-style-1 nil nil nil 0)
	#+(or aclpc acl86win32)
        *undefined-text-style*
	#+(or aclpc acl86win32)
          (make-text-style-1 (make-symbol (symbol-name 'undefined)) nil nil 1)
        *next-text-style-index* 2
        *text-style-index-table*
          (make-array +maximum-text-style-index+
                      :initial-element #-(or aclpc acl86win32)
                                       *null-text-style*
				       #+(or aclpc acl86win32)
				       *undefined-text-style*)
        *text-style-intern-table*
          (copy-tree `((nil . ((nil . ((nil . ,*null-text-style*)))))))
        (aref *text-style-index-table* 0) *null-text-style*
        *text-style-merging-cache*
          (make-array (* +maximum-text-style-index+ 10)
                      :element-type `(integer 0 (,+maximum-text-style-index+))
                      :initial-element 0)
        *default-text-style*
        (make-text-style :fix :roman :normal)
        *undefined-text-style*
        (make-text-style :undefined :roman 10)))

(defmethod parse-text-style ((text-style standard-text-style))
  text-style)

(defmethod parse-text-style ((text-style device-font))
  text-style)

(defmethod parse-text-style ((style null))
  *null-text-style*)

(defmethod parse-text-style ((style list))
  (assert (= (length style) 3) (style)
          "A text style is a three element list (face family size), not ~S"
          style)
  (apply #'make-text-style style))


(defvar *text-style-lock* (make-lock "text style lock"))

;;; The text-style table is an alist-structured database; the
;;; first key is family, then face, then size.  This might or might not
;;; be better than an EQUAL hash table.  We can decide later to do this
;;; if we want.
(defun make-text-style (family face size &aux changed-p original-face)
  (unless (numberp face)
    (setf original-face face
          face (face->face-code face))) ;"Intern" the face code.
  (loop
    (let* ((family-stuff (assoc family *text-style-intern-table*))
           (face-stuff (and family-stuff (assoc face (cdr family-stuff))))
           (size-stuff (and face-stuff (assoc size (cdr face-stuff)))))
      (when size-stuff (return-from make-text-style (cdr size-stuff)))
      (multiple-value-setq (family face size changed-p original-face)
          (validate-text-style-components family face size original-face))
      (unless changed-p
        (macrolet ((ensure-stuff (stuff thing from-stuff)
                     `(unless ,stuff
                        (setf ,stuff (cons ,thing nil))
                        (with-lock-held (*text-style-lock* "Text style lock")
                          (setf ,from-stuff (nconc ,from-stuff (cons ,stuff nil)))))))
          (ensure-stuff family-stuff family *text-style-intern-table*)
          (ensure-stuff face-stuff face family-stuff)
          (ensure-stuff size-stuff size face-stuff))
        (let* ((new-style (make-text-style-1 family face size nil)))
          (setf (cdr size-stuff) new-style)
          (return-from make-text-style new-style))))))

(defmethod text-style-index ((text-style standard-text-style))
  (let ((index (slot-value text-style 'index)))
    (when index
      (return-from text-style-index index))
    (let* ((new-index (with-lock-held (*text-style-lock* "Text style lock")
                        (prog1 *next-text-style-index*
                               (incf *next-text-style-index*)))))
      (when (> new-index +maximum-text-style-index+)
        (error "Too many text style indices; can't assign an index to ~A"
               text-style))
      (setf (aref *text-style-index-table* new-index) text-style)
      (setf (slot-value text-style 'index) new-index))))

(defvar *valid-text-style-sizes*
        '(:tiny :very-small :small :normal :large :very-large :huge))

(defun validate-text-style-components (family face size &optional original-face)
  (when (consp face)                                ;This is an error in face->face-code.
    (apply #'error face))
  (unless (or (null size)
              (numberp size)
              (member size *valid-text-style-sizes*)
              (eq size :smaller)
              (eq size :larger))
    (error "The size ~S is not valid" size))
  ;; This needs to be fleshed out with error handlers, etc.
  (values family face size nil original-face))

(defun-inline index->text-style (index)
  (aref *text-style-index-table* index))

;;; General case
(defmethod merge-text-styles (style1 style2)
  (setf style1 (parse-text-style style1)
        style2 (parse-text-style style2))
  (index->text-style
    (merge-text-style-indices (text-style-index style1)
                              (text-style-index style2))))

;;; Device fonts can't be merged against anything.
(defmethod merge-text-styles ((style1 device-font) style2)
  (declare (ignore style2))
  style1)

(defmethod merge-text-styles (style1 (style2 device-font))
  (declare (ignore style1))
  style2)

(defun merge-text-style-indices (index1 index2)
  (when (zerop index1) (return-from merge-text-style-indices index2))
  (let ((subscript (+ index1 (* index2 +maximum-text-style-index+))))
    (when (>= subscript (array-total-size *text-style-merging-cache*))
      (let ((new (make-array (* (min (+ index2 5) +maximum-text-style-index+)
                                +maximum-text-style-index+)
                             :element-type `(integer 0 (,+maximum-text-style-index+))
                             :initial-element 0)))
        (replace new *text-style-merging-cache*)
        (setq *text-style-merging-cache* new)))
    (let ((merged-style-index (aref *text-style-merging-cache* subscript)))
      (when (plusp merged-style-index)
        (return-from merge-text-style-indices merged-style-index)))
    (let ((merged-style-index
            (text-style-index
              (multiple-value-bind (result-family result-face result-size)
                  (with-slots ((family1 family) (face1 face) (size1 size))
                              (index->text-style index1)
                    (with-slots ((family2 family) (face2 face) (size2 size))
                                (index->text-style index2)
                      (merge-text-style-components family1 face1 size1
                                                   family2 face2 size2)))
                (make-text-style result-family result-face result-size)))))
      (setf (aref *text-style-merging-cache* subscript) merged-style-index)
      merged-style-index)))

(defun merge-text-style-components (family1 face1 size1 family2 face2 size2)
  (values (merge-text-style-families family1 family2)
          (merge-text-style-faces face1 face2)
          (merge-text-style-sizes size1 size2)))

(defun merge-text-style-families (family1 family2)
  (or family1 family2))

(defun merge-text-style-sizes (size1 size2)
  (let ((max-larger-size 24)                        ;limits for :LARGER and :SMALLER
        (min-smaller-size 4))
    (case size1
      (:larger
        (cond ((numberp size2) (min (+ size2 2) max-larger-size))
              ((eq size2 :smaller) nil)                ;let a higher level decide...
              (t (let ((index (position size2 *valid-text-style-sizes*)))
                   (if (null index)
                       size1
                       (or (nth (1+ index) *valid-text-style-sizes*) :huge))))))
      (:smaller
        (cond ((numberp size2) (max (- size2 2) min-smaller-size))
              ((eq size2 :larger) nil)                ;let a higher level decide...
              (t (let ((index (position size2 *valid-text-style-sizes*)))
                   (if (null index)
                       size1
                       (if (zerop index) :tiny (nth (1- index) *valid-text-style-sizes*)))))))
      (otherwise
        (or size1 size2)))))

(defmacro define-text-face-class (class name-list lambda-list &body merging-function
                                  &environment env)
  (multiple-value-bind (documentation declarations body)
      (extract-declarations merging-function env)
    (let ((text-face-class-definitions
            (mapcar #'(lambda (name) `(add-text-face-class ',name ',class)) name-list))
          (face-code-mask-macro-name (fintern "~A-~A" class 'face-code-mask))
          (set-bits-macro-name (fintern "~A-~A-~A" 'set class 'face-code-bits))
          (reset-bits-macro-name (fintern "~A-~A-~A" 'reset class 'face-code-bits))
          (test-bits-macro-name (fintern "~A-~A-~A" 'test class 'face-code-bits)))
      `(define-group ,class define-text-face-class
         (define-text-face-class-load-time ',class)

         ;; Cloe doesn't appear able to find the MACROLETted versions below.
         #+Cloe-Runtime                                ;Cloe bug?
         (progn
           (defmacro ,face-code-mask-macro-name (&rest face-codes)
             (let ((result 0))
               (dolist (face-code face-codes result)
                 (assert (member face-code ',name-list))
                 (setf result (logior result
                                      (ash 1 (position face-code ',name-list)))))))
           (defmacro ,test-bits-macro-name (datum &rest face-codes)
             `(logtest ,datum (,',face-code-mask-macro-name ,@face-codes)))
           (defmacro ,set-bits-macro-name (datum &rest face-codes)
             `(setf ,datum
                    (logior ,datum (,',face-code-mask-macro-name ,@face-codes))))
           (defmacro ,reset-bits-macro-name (datum &rest face-codes)
             `(setf ,datum
                    (logand ,datum
                            (lognot (,',face-code-mask-macro-name ,@face-codes))))))
         (defun-property (,class face-class-merging-function) ,lambda-list
           ,@declarations
           ,@(when documentation (list documentation))
           (macrolet ((,face-code-mask-macro-name (&rest face-codes)
                       (let ((result 0))
                         (dolist (face-code face-codes result)
                           (assert (member face-code ',name-list))
                           (setf result (logior result
                                                (ash 1 (position face-code ',name-list)))))))
                      (,test-bits-macro-name (datum &rest face-codes)
                       `(logtest ,datum (,',face-code-mask-macro-name ,@face-codes)))
                      (,set-bits-macro-name (datum &rest face-codes)
                       `(setf ,datum
                              (logior ,datum (,',face-code-mask-macro-name ,@face-codes))))
                      (,reset-bits-macro-name (datum &rest face-codes)
                       `(setf ,datum
                              (logand ,datum
                                      (lognot (,',face-code-mask-macro-name ,@face-codes))))))
             ,@body))
         ,@text-face-class-definitions))))

(defmacro define-text-face-added-mappings (class &body mappings)
  `(add-face-added-text-mappings ',class ',mappings))

(defvar *face->face-code-cache* nil)
(defvar *face-added-text-mapping-cache* nil)

(defun define-text-face-class-load-time (class)
  (let ((alist (assoc class *face->face-code-cache*)))
    (when (null alist)
      (setf alist (cons class nil))
      (setf *face->face-code-cache* (nconc *face->face-code-cache* (list alist))))))

(defmacro define-text-face (name class)
  `(add-text-face-class ',name ',class))

(defun add-text-face-class (name class)
  (let ((alist (assoc class *face->face-code-cache*)))
    (when (null alist)
      (error "Can't define face ~S in unknown face class ~S" name class))
    (unless (member name (cdr alist))
      (nconc alist (list name)))))

(defun add-face-added-text-mappings (class mappings)
  (let ((pair (assoc class *face-added-text-mapping-cache*)))
    (unless pair
      (setf pair (list class))
      (push pair *face-added-text-mapping-cache*))
    (dorest (mapping-pair mappings cddr)
      (let* ((logical (first mapping-pair))
             (underlying (face->face-code (second mapping-pair)))
             (mapping-pair (assoc logical (cdr pair))))
        (if mapping-pair
            (setf (cdr mapping-pair) underlying)
            (push (cons logical underlying) (cdr pair)))))))

(defconstant %%face-code-no-merge (byte 1 28))
(defconstant %%face-code-class (byte 4 24))
(defconstant %%face-code-faces (byte 24 0))

(defun face->face-code (face)
  (when (null face) (return-from face->face-code nil))
  (let ((class-alist nil)
        (added-mapping-alist nil)
        (class-number nil)
        (value 0)
        (no-merge 0))
    (flet ((parse-face (face)
             (when (eq face :no-merge)
               (setf no-merge 1)
               (return-from parse-face 0))
             (if class-number
                 ;; We already know where to look
                 (or
                  (cdr (assoc face (cdr added-mapping-alist)))
                  (ash 1
                       (or (position face (cdr class-alist))
                           (return-from face->face-code ;Return an error indication.
                             (list "Can't parse face ~S in class ~S"
                                   face (car class-alist))))))
               ;; Iterate over the ((family . faces) ...) list
               (do ((face-cache *face->face-code-cache* (cdr face-cache))
                    (class-no 0 (1+ class-no)))
                   ((null face-cache)
                    (return-from face->face-code (list "Can't parse face ~S" face)))
                 (let ((value (position face (cdar face-cache))))
                   (when value
                     ;; If we have found it return
                     (setf class-number class-no
                           class-alist (car face-cache)
                           added-mapping-alist (assoc (car class-alist)
                                                      *face-added-text-mapping-cache*))
                     (return (ash 1 value))))
                 ;; Otherwise look in the added list
                 ;; for :bold-italic type stuff
                 (let* ((added-value-mapping-alist
                         (assoc (caar face-cache) *face-added-text-mapping-cache*))
                        (value (cdr (assoc face (cdr added-value-mapping-alist)))))
                   (when value
                     (setf class-number class-no
                           class-alist (car face-cache)
                           added-mapping-alist added-value-mapping-alist)
                     (return value)))))))
      (if (listp face)
          (dolist (face face)
            (setf value (logior value (parse-face face))))
        (setf value (parse-face face))))
    (dpb class-number %%face-code-class
         (dpb value %%face-code-faces
              (dpb no-merge %%face-code-no-merge 0)))))

(defvar *face-code->face-cache* (make-hash-table))

(defun face-code->face (face-code)
  (when (or (null face-code) (zerop face-code))        ;?? zerop?
    (return-from face-code->face nil))
  (or (gethash face-code *face-code->face-cache*)
      (let* ((class-number (ldb %%face-code-class face-code))
             (face-bits (ldb %%face-code-faces face-code))
             (class-alist (cdr (nth class-number *face->face-code-cache*)))
             (result (if (ldb-test %%face-code-no-merge face-code)
                         (list :no-merge)
                         (= (logcount face-bits) 1))))
        (loop
          (when (zerop face-bits)
            (return (setf (gethash face-code *face-code->face-cache*) (nreverse result))))
          (when (logtest face-bits 1)
            (when (eq result t) (return (car class-alist)))
            (push (car class-alist) result))
          (pop class-alist)
          (setf face-bits (ash face-bits -1))))))

(defun merge-text-style-faces (face1 face2)
  (when (null face1) (return-from merge-text-style-faces face2))
  (when (null face2) (return-from merge-text-style-faces face1))
  (let ((class1 (ldb %%face-code-class face1)))
    (unless (= class1 (ldb %%face-code-class face2))
      (return-from merge-text-style-faces face1))
    (funcall (get (car (nth class1 *face->face-code-cache*)) 'face-class-merging-function)
             face1 face2)))

(define-text-face-class :roman (:roman :extra :bold :italic :condensed :extended
                                :caps :underline :outline :shadow)
                        (face1 face2)
  (let ((result (logior face1 face2)))
    (when (ldb-test %%face-code-no-merge face1)
      (setf result (dpb 0 %%face-code-no-merge face1)))
    ;; Some heuristics: when condensed, we're not extended and vice-versa
    (when (test-roman-face-code-bits face1 :condensed)
      (reset-roman-face-code-bits result :extended))
    (when (test-roman-face-code-bits face1 :extended)
      (reset-roman-face-code-bits result :condensed))
    ;; And a couple more: when roman, we're not bold or italic, and vice-versa
    (when (test-roman-face-code-bits face1 :roman)
      (reset-roman-face-code-bits result :bold :italic :extra))
    (when (test-roman-face-code-bits face1 :bold :italic)
      (reset-roman-face-code-bits result :roman))
    result))

(define-text-face-added-mappings :roman
  :bold-italic (:bold :italic)
  :bold-condensed-caps (:bold :condensed :caps)
  :bold-extended (:bold :extended)
  :extra-condensed (:extra :condensed))

(define-text-face-class :special (:uppercase)
                        (face1 face2)
  (or face1 face2))

(define-text-face-class :logical ()
                        (face1 face2)
  (declare (ignore face2))
  face1)

(when (not (boundp '*null-text-style*))
  (initialize-text-style-tables))


(defgeneric text-style-height (text-style medium))
(defgeneric text-style-width (text-style medium))
(defgeneric text-style-ascent (text-style medium))
(defgeneric text-style-descent (text-style medium))
(defgeneric text-style-fixed-width-p (text-style medium))
(defgeneric text-size (medium string &key text-style start end))


(defmethod text-style-height ((style list) medium)
  (text-style-height (parse-text-style style) medium))

(defmethod text-style-width ((style list) medium)
  (text-style-width (parse-text-style style) medium))

(defmethod text-style-ascent ((style list) medium)
  (text-style-ascent (parse-text-style style) medium))

(defmethod text-style-descent ((style list) medium)
  (text-style-descent (parse-text-style style) medium))

(defmethod text-style-fixed-width-p ((style list) medium)
  (text-style-fixed-width-p (parse-text-style style) medium))

(defmethod text-size ((medium basic-medium) string
                      &key (text-style (medium-merged-text-style medium))
                           (start 0) end)
  (declare (values largest-x total-height last-x last-y baseline))
  ;; this shouldn't be necessary because of trampolines directly above
  ;;  31jan97 tjm w/colin
  #+(or aclpc acl86win32)
  (when (consp text-style)
    (setq text-style (clim:parse-text-style text-style)))
  (when (characterp string)
    (multiple-value-bind  (index font escapement-x escapement-y
                           origin-x origin-y bb-x bb-y)
        (port-glyph-for-character (port medium) string text-style)
      (declare (ignore index font origin-x))
      (return-from text-size
        (values bb-x bb-y escapement-x escapement-y origin-y))))
  (let ((largest-x 0)
        (total-height 0)
        (last-x 0)
        (last-y 0)
        (baseline (- (text-style-height text-style medium)
                     (text-style-descent text-style medium))))
    (dovector (char string :start start :end end)
      (cond ((or (eql char #\Newline)
                 (eql char #\Return))
             (incf total-height (text-style-height text-style medium))
             (incf last-y (text-style-height text-style medium))
             (setq last-x 0))
            (t
             (multiple-value-bind (index font escapement-x escapement-y
                                   origin-x origin-y bb-x bb-y)
                 (port-glyph-for-character (port medium) char text-style)
               (declare (ignore index font
                                escapement-y origin-x origin-y bb-x))
               (incf last-x escapement-x)
               (maxf largest-x last-x)
               (maxf total-height bb-y)))))
    (values largest-x total-height last-x last-y baseline)))

(defun compute-text-x-adjustment (align-x stream string text-style &optional start end)
  (ecase align-x
    (:left 0)
    (:right (- (text-size stream string :text-style text-style
                          :start start :end end)))
    (:center (- (round (text-size stream string :text-style text-style
                                  :start start :end end) 2)))))

(defun compute-text-y-adjustment (align-y descent ascent height)
  (ecase align-y
    (:baseline 0)
    (:top ascent)
    (:bottom (- descent))
    (:center (- ascent (round height 2)))))


#-Genera
(defun merged-text-style (char style)
  (values char style))

#+Genera
(progn

(defvar *genera-style-to-style-table* (make-array 256 :initial-element nil))

(defun genera-style->style (character-style)
  (let ((index (si:style-index character-style)))
    (or (aref *genera-style-to-style-table* index)
        (setf (aref *genera-style-to-style-table* index)
              (parse-text-style (si:unparse-character-style character-style))))))

(defun merged-text-style (character style)
  (let* ((char-style (si:char-style character))
         (char-style (genera-style->style char-style))
         (merged-style (merge-text-styles style char-style)))
    (values (sys:make-character character :style nil) merged-style)))
) ;;; End of #+Genera


(defmacro define-display-device (name class &key font-for-undefined-style)
  `(defvar ,name (make-instance ',class
                   :name ',name
                   :font-for-undefined-style ',font-for-undefined-style)))

;;; [ 1/4/90 naha -- determined by reading DEFINE-TEXT-STYLE-MAPPINGS-1]
;;; each element of SPECS can be one of
;;;     `(:style ,family ,face ,size)   [what will this do?]
;;;     `(:family ,family ,specs)       ==> recurse on specs for specified family
;;;     `(:face ,face ,specs)           ==> recurse on specs for specified face
;;;     `(:size ,size ,specs)           ==> recurse on specs for specified size
;;;     the name of a font to map the specified text style to.

(defmacro define-text-style-mappings (port character-set &body specs)
  `(define-text-style-mappings-1 ,port ,character-set ',specs))

(defun define-text-style-mappings-1 (port character-set specs)
  (labels ((load-specs (family face size specs)
             (when (and (consp specs) (eq (first specs) :style))
               (setf specs (apply #'make-text-style (rest specs))))
             (if (and (consp specs) (not (eq (car specs) :style)))
                 (do* ((type (first specs))
                       (my-specs (cdr specs) (cddr my-specs))
                       (value (first my-specs) (first my-specs))
                       (rest (second my-specs) (second my-specs)))
                      ((null my-specs))
                   (case type
                     (:family (setf family value))
                     (:face (setf face value))
                     (:size (setf size value))
                     (otherwise (warn "Ill-formed mapping contains ~S" specs)))
                   (load-specs family face size rest))
                 (if (and family face size)
                     (setf (text-style-mapping
                             port (make-text-style family face size) character-set)
                           specs)
                     (error "Can't do [~A.~A.~A]" family face size)))))
    (dolist (spec specs)
      (load-specs nil nil nil spec))))

(defmethod port-mapping-table ((port basic-port) character-set)
  (with-slots (mapping-table) port
     #+allegro
     (excl:ics-target-case
       (:-ics character-set mapping-table)
       (:+ics (let ((old-length (length mapping-table)))
                (when (>= character-set (length mapping-table))
                  (setf mapping-table (adjust-array mapping-table (1+ character-set)))
                  (dotimes (i (- (length mapping-table) old-length))
                    (setf (aref mapping-table (+ i old-length)) (make-hash-table :test #'equal)))))
              (aref mapping-table character-set)))
     #-allegro
     mapping-table))

(defmethod port-mapping-cache ((port basic-port) character-set)
  (with-slots (mapping-cache) port
     #+allegro
     (excl:ics-target-case
       (:-ics character-set mapping-cache)
       (:+ics (let ((old-length (length mapping-cache)))
                (when (>= character-set (length mapping-cache))
                  (setf mapping-cache (adjust-array mapping-cache (1+ character-set)))
                  (dotimes (i (- (length mapping-cache) old-length))
                    (setf (aref mapping-cache (+ i old-length)) (cons nil nil)))))
              (aref mapping-cache character-set)))
     #-allegro
     mapping-cache))

(defmethod (setf text-style-mapping) (mapping (port basic-port) style
                                              &optional (character-set *standard-character-set*)
                                              window)
  (declare (ignore window))
  (setq style (standardize-text-style port (parse-text-style style) character-set))
  (when (listp mapping)
    (assert (eq (first mapping) :style) ()
            "Text style mappings must be atomic font names ~
             or (:STYLE . (family face size))")
    (setf mapping (parse-text-style (cdr mapping))))
  (with-slots (allow-loose-text-style-size-mapping) port
     (let ((mapping-table (port-mapping-table port character-set))
           (mapping-cache (port-mapping-cache port character-set)))
       (without-scheduling
         (setf (car mapping-cache) nil
               (cdr mapping-cache) nil))
       (if allow-loose-text-style-size-mapping ; ### <----!!!
           (multiple-value-bind (family face size) (text-style-components style)
             (declare (ignore size))
             ;; NB: loose size mapping requires an EQUAL hash table!
             (with-stack-list (key family face)
               (let* ((fonts (gethash key mapping-table))
                      (old (assoc style fonts)))
                 (cond (old
                        (setf (second old) mapping))
                       (t
                        (push (list style mapping) fonts)
                        (setq fonts (sort fonts #'(lambda (e1 e2)
                                                    (< (text-style-size (first e1))
                                                       (text-style-size (first e2))))))
                        (setf (gethash (copy-list key) mapping-table)
                              fonts)))
                 mapping)))
           (setf (gethash style mapping-table) mapping)))))

(defmethod (setf text-style-mapping) (mapping (port basic-port) (style device-font)
				      &optional (character-set *standard-character-set*)
						window)
  ;;--- What about the character set when using device fonts?
  (declare (ignore character-set window))
  ;;--- EQL? TYPE-EQUAL?  This is too restrictive as it stands
  (unless (eq port (device-font-display-device style))
    (error "An attempt was made to map device font ~S on port ~S, ~@
	    but it is defined for the port ~S"
	   style port (device-font-display-device style)))
  (setf (device-font-name style) mapping))

;;; This is broken up into two methods so any :AROUND method will only
;;; be called on the outermost recursion.

(defmethod text-style-mapping ((port basic-port) style
			       &optional (character-set *standard-character-set*)
					 window)
  (let ((mapping-cache (port-mapping-cache port character-set)))
    (when (eq style (car mapping-cache))
      (return-from text-style-mapping (cdr mapping-cache)))
    (let ((font (text-style-mapping* port style character-set window)))
      (without-scheduling
	(setf (car mapping-cache) style
	      (cdr mapping-cache) font))
      font)))

(defmethod text-style-mapping ((port basic-port) (style device-font)
                               &optional (character-set *standard-character-set*) window)
  ;;--- What about the character set when using device fonts?
  #-aclpc (declare (ignore character-set window))
  ;;--- EQL? TYPE-EQUAL?  This is too restrictive as it stands
  (unless (eq port (device-font-display-device style))
    (error "An attempt was made to map device font ~S on port ~S, ~@
            but it is defined for the port ~S"
           style port (device-font-display-device style)))
  (device-font-name style))

(defmethod text-style-mapping* ((port basic-port) style
                                &optional (character-set *standard-character-set*)
                                          window)
  (setq style (standardize-text-style port (parse-text-style style) character-set))
  (let* ((loose (slot-value port 'allow-loose-text-style-size-mapping))
         (mapping-table (port-mapping-table port character-set))
         (result
          (or (if loose
                  (lookup-closest-font style mapping-table)
                (gethash style mapping-table))
              (if loose
                  (lookup-closest-font *undefined-text-style* mapping-table)
                (gethash *undefined-text-style* mapping-table)))))
    (when (text-style-p result)                ;logical translations
      (setf result (text-style-mapping* port result character-set window)))
    result))

(defmethod text-style-mapping-exists-p ((port basic-port) style
                                        &optional
                                        (character-set *standard-character-set*)
                                        exact-size-required)
  (setq style (standardize-text-style port (parse-text-style style) character-set))
  (let* ((loose (slot-value port 'allow-loose-text-style-size-mapping))
         (mapping-table (port-mapping-table port character-set))
         (result (if loose
                     (lookup-closest-font style mapping-table exact-size-required)
                   (gethash style mapping-table))))
    (cond ((null result) nil)
          ((text-style-p result)        ;logical translations
           (text-style-mapping-exists-p port style character-set))
          (t t))))

(defun lookup-closest-font (style mapping-table &optional exact-size-required)
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (family face size) (text-style-components style)
    (with-stack-list (key family face)
      (let ((tuples (gethash key mapping-table))
            (last-tuple nil)
            (last-size nil))
        (dolist (tuple tuples (and tuples (not exact-size-required)
                                   (cadar (last tuples))))
          (let ((font-size (text-style-size (car tuple))))
            (if exact-size-required
                (when (= size font-size)
                  (return (cadr tuple)))
              (when (<= size font-size)
                ;; Know which one to pick.
                (cond ((null last-tuple)
                       (return (cadr tuple)))
                      ((< (abs (- size font-size)) (abs (- size last-size)))
                       (return (cadr tuple)))
                      (t
                       (return (cadr last-tuple))))))
            (setq last-tuple tuple)
            (setq last-size font-size)))))))


;; This method allows the device to convert logical sizes into point
;; sizes, etc.  The default method doesn't do much of anything.
#-(or aclpc acl86win32)
(defmethod standardize-text-style ((port basic-port) style
                                   &optional (character-set *standard-character-set*))
  (declare (ignore character-set))
  (unless (numberp (text-style-size style))
    (standardize-text-style-error style))
  style)

(defmethod standardize-text-style-error ((style standard-text-style))
  (with-slots (family face size) style
    (if (and family face size)
        (cerror "Use the undefined text style stand-in instead"
                "The size component of ~S is not numeric.  This port does not know ~
               how to map logical text style sizes"
                style)
      (cerror "Use the undefined text style stand-in instead"
              "The text style ~S must be a fully merged text style"
              style))
    *undefined-text-style*))

;; For use by more specific STANDARDIZE-TEXT-STYLE methods
(defun-inline standardize-text-style-1 (port style character-set size-alist)
  (declare (ignore port character-set))
  (let ((size (text-style-size style)))
    (if (numberp size)
        style
        (let ((new-size (assoc size size-alist)))
          (cond (new-size
                 (make-text-style (text-style-family style)
                                  (text-style-face style)
                                  (second new-size)))
                (t
                 (standardize-text-style-error style)))))))


;;; Extensions to Common Lisp character operations.

(defun-inline diacritic-char-p (character)
  #-Genera (declare (ignore character))
  #+Genera (si:diacritic-char-p character)
  #-Genera nil)

#-(or aclpc acl86win32)
(defun-inline char-character-set-and-index (character)
  (declare (values character-set index))
  (values #+allegro
	  (excl:ics-target-case
           (:-ics *standard-character-set*)
           (:+ics (typecase character
                    (excl::codeset-0 0)
                    (excl::codeset-1 1)
                    (excl::codeset-2 2)
                    (excl::codeset-3 3))))
          #-allegro *standard-character-set*
          (char-code character)))

#+(or aclpc acl86win32)
;;; For now, only standard character set characters are understood...
(defun-inline char-character-set-and-index (character)
  (declare (values character-set index))
  (values *standard-character-set* (char-code character)))



