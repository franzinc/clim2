;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: text-style.lisp,v 1.4 91/03/26 12:49:00 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 International Lisp Associates."

;;; To consider: how to merge Genera styles with styles.

(define-protocol-class text-style ())

(defclass standard-text-style (text-style)
     ((family :initarg :family :initform nil :reader text-style-family)
      (face   :initarg :face   :initform nil :reader text-style-face)
      (size   :initarg :size   :initform nil :reader text-style-size)
      (index  :initarg :index  :initform nil)))

(define-constructor make-text-style-1 standard-text-style (family face size index)
  :family family :face face :size size :index index)

(defmethod text-style-components ((style standard-text-style))
  (with-slots (family face size) style
    (values family face size)))

(defmethod fully-merged-text-style-p ((style standard-text-style))
  (with-slots (family face size) style
    (and family face size)))

(defmethod make-load-form ((object standard-text-style))
  (with-slots (family face size) object
    `(make-text-style ,family ,face ,size)))

(defmethod print-object ((style standard-text-style) stream)
  (with-slots (family face size) style
    (let ((true-face (face-code->face face)))
      #+Genera
      (when *print-readably*
	(format stream "#.(~S ~S)" 'parse-text-style
		`'(,family ,true-face ,size))
	(return-from print-object style))
      (macrolet ((do-it ()
		   `(format stream "~S.~S.~S" family true-face size)))
	(if *print-escape*
	    (print-unreadable-object (style stream :type t :identity t)
	      (do-it))
	    (do-it))))))

(defclass device-font ()
     ((display-device :initarg :display-device :reader device-font-display-device)
      (font-name :initarg :font-name :reader device-font-name)))

(defun make-device-font (display-device font-name)
  (make-instance 'device-font :display-device display-device :font-name font-name))

(defmethod print-object ((df device-font) stream)
  (print-unreadable-object (df stream :type t :identity t)
    (format stream "~A" (slot-value df 'font-name))))

(defconstant maximum-text-style-index 256)	;Why not?  That's the Genera maximum

(defvar *standard-character-set* nil)		;Not really used yet.

(defvar *null-text-style*)
(defvar *undefined-text-style*)
(defvar *text-style-index-table*)
(defvar *next-text-style-index*)
(defvar *text-style-intern-table*)
(defvar *text-style-merging-cache*)
(defvar *default-text-style*)

(defun initialize-text-style-tables ()
  (setf *null-text-style* (make-text-style-1 nil nil nil 0)
	*undefined-text-style* (make-text-style-1 (make-symbol "UNDEFINED") nil nil 1)
	*next-text-style-index* 2
	*text-style-index-table*
	  (make-array maximum-text-style-index 
		      :initial-element *undefined-text-style*)
	*text-style-intern-table*
	  (copy-tree `((nil . ((nil . ((nil . ,*null-text-style*)))))))
	(aref *text-style-index-table* 0) *null-text-style*
	*text-style-merging-cache*
	  (make-array (* maximum-text-style-index 10)
		      :element-type `(integer 0 (,maximum-text-style-index))
		      :initial-element 0)
	*default-text-style*
	  (make-text-style :fix :roman :normal)
	*undefined-text-style*
	  (make-text-style :stand-in-for-undefined-style :roman :normal))
  *null-text-style*)

(defmethod parse-text-style ((text-style standard-text-style))
  text-style)

(defmethod parse-text-style ((text-style device-font))
  text-style)

(defmethod parse-text-style ((null null))
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
	  face (face->face-code face)))		;"Intern" the face code.
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
  (with-slots (index) text-style
    (when index (return-from text-style-index index))
    (let* ((new-index (with-lock-held (*text-style-lock* "Text style lock")
			(prog1 *next-text-style-index*
			       (incf *next-text-style-index*)))))
      (when (> new-index maximum-text-style-index)
	(error "Too many text style indices; can't assign an index to ~A"
	       text-style))
      (setf (aref *text-style-index-table* new-index) text-style)
      (setf index new-index))))

(defvar *valid-text-style-sizes*
	'(:tiny :very-small :small :normal :large :very-large :huge))

(defun validate-text-style-components (family face size &optional original-face)
  (when (consp face)				;This is an error in face->face-code.
    (apply #'error face))
  ;;--- Handle point sizes
  (unless (or (null size)
	      (numberp size)
	      (member size *valid-text-style-sizes*)
	      (eql size :smaller)
	      (eql size :larger))
    (error "The size ~S is not valid." size))
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
  (let ((subscript (+ index1 (* index2 maximum-text-style-index))))
    (when (>= subscript (array-total-size *text-style-merging-cache*))
      (let ((new (make-array (* (min (+ index2 5) maximum-text-style-index)
				maximum-text-style-index)
			     :element-type `(integer 0 (,maximum-text-style-index))
			     :initial-element 0)))
	(replace new *text-style-merging-cache*)
	(setq *text-style-merging-cache* new)))
    (let ((merged-style-index (aref *text-style-merging-cache* subscript)))
      (when (plusp merged-style-index)
	(return-from merge-text-style-indices merged-style-index)))
    (let ((merged-style-index
	    (text-style-index
	      (multiple-value-bind (result-family result-face result-size)
		  (multiple-value-bind (family1 face1 size1)
		      (text-style-components (index->text-style index1))
		    (multiple-value-bind (family2 face2 size2)
			(text-style-components (index->text-style index2))
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
  (let ((max-larger-size 24)			;limits for :LARGER and :SMALLER
	(min-smaller-size 4))
    (case size1
      (:larger
	(cond ((numberp size2) (min (+ size2 2) max-larger-size))
	      ((eql size2 :smaller) nil)	;let a higher level decide...
	      (t (let ((index (position size2 *valid-text-style-sizes*)))
		   (if (null index)
		       size1
		       (or (nth (1+ index) *valid-text-style-sizes*) :huge))))))
      (:smaller
	(cond ((numberp size2) (max (- size2 2) min-smaller-size))
	      ((eql size2 :larger) nil)		;let a higher level decide...
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
	 #+Cloe-Runtime				;Cloe bug?
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
	 (defun-property (:property ,class face-class-merging-function) ,lambda-list
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
      (error "Can't define face ~S in unknown face class ~S." name class))
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
	     (when (eql face :no-merge)
	       (setf no-merge 1)
	       (return-from parse-face 0))
	     (if class-number
		 (or
		   (cdr (assoc face (cdr added-mapping-alist)))
		   (ash 1
			(or (position face (cdr class-alist))
			    (return-from face->face-code	;Return an error indication.
			      (list "Can't parse face ~S in class ~S"
				    face (car class-alist))))))
		 (do ((face-cache *face->face-code-cache* (cdr face-cache))
		      (class-no 0 (1+ class-no)))
		     ((null face-cache)
		      (return-from face->face-code (list "Can't parse face ~S" face)))
		   (let ((value (position face (cdar face-cache))))
		     (when value
		       (setf class-number class-no
			     class-alist (car face-cache)
			     added-mapping-alist (assoc (car class-alist)
							*face-added-text-mapping-cache*))
		       (return (ash 1 value))))
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
  (when (or (null face-code) (zerop face-code))	;?? zerop?
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
	    (when (eql result t) (return (car class-alist)))
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

(defgeneric text-size (medium string &key text-style start end)
  (declare (values width height final-x final-y baseline)))


(defmethod text-style-height ((text-style standard-text-style)
			      (stream standard-encapsulating-stream))
  (text-style-height text-style (slot-value stream 'stream)))

(defmethod text-style-width ((text-style standard-text-style)
			     (stream standard-encapsulating-stream))
  (text-style-width text-style (slot-value stream 'stream)))

(defmethod text-style-ascent ((text-style standard-text-style)
			      (stream standard-encapsulating-stream))
  (text-style-ascent text-style (slot-value stream 'stream)))

(defmethod text-style-descent ((text-style standard-text-style) 
			       (stream standard-encapsulating-stream))
  (text-style-descent text-style (slot-value stream 'stream)))

(defmethod text-style-fixed-width-p ((text-style standard-text-style) 
				     (stream standard-encapsulating-stream))
  (text-style-fixed-width-p text-style (slot-value stream 'stream)))

(defmethod text-size ((stream standard-encapsulating-stream) string
		      &key (text-style (stream-merged-text-style stream)) (start 0) end)
  (text-size stream string :text-style text-style :start start :end end))


#-Genera
(defun merged-text-style (character style)
  (values character style))

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


;; See SHEET-GRAPHICS-IMPLEMENTATION for an example of its use...
(defmacro define-display-device (name class &key font-for-undefined-style)
  `(defvar ,name (make-instance ',class 
				:name ',name
				:font-for-undefined-style ',font-for-undefined-style)))

;;; [ 1/4/90 naha -- determined by reading DEFINE-TEXT-STYLE-MAPPINGS-LOAD-TIME ]
;;; each element of SPECS can be one of
;;;     `(:style ,family ,face ,size)   [what will this do?]
;;;     `(:family ,family ,specs)       ==> recurse on specs for specified family
;;;     `(:face ,face ,specs)           ==> recurse on specs for specified face
;;;     `(:size ,size ,specs)           ==> recurse on specs for specified size
;;;     the name of a font to map the specified text style to.

(defmacro define-text-style-mappings (device character-set &body specs)
  `(define-text-style-mappings-load-time ,device ,character-set ',specs))

(defun define-text-style-mappings-load-time (device character-set specs)
  (labels ((load-specs (family face size specs)
	     (when (and (consp specs) (eql (first specs) :style))
	       (setf specs (apply #'make-text-style (rest specs))))
	     (if (and (consp specs) (not (eql (car specs) :style)))
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
			     device character-set (make-text-style family face size))
			   specs)
		     (error "Can't do [~A.~A.~A]" family face size)))))
    (dolist (spec specs)
      (load-specs nil nil nil spec))))

(defclass display-device ()
     ((name :initarg :name :reader display-device-name)
      (mapping-table :initform (make-hash-table))))

(defvar *display-devices* nil)

(defmethod initialize-instance :after
    ((device display-device) &key font-for-undefined-style)
  ;; Keep track of all of the display devices
  (push-unique device *display-devices* :key #'display-device-name)
  (when font-for-undefined-style
    (setf (text-style-mapping
	    device *standard-character-set* *undefined-text-style*)
	  font-for-undefined-style)))

(defmethod print-object ((device display-device) stream)
  (print-unreadable-object (device stream :type t :identity t)
    (format stream "~A" (slot-value device 'name))))

(defmethod (setf text-style-mapping)
	   (mapping (device display-device) character-set style &optional window)
  (declare (ignore window))
  (setq style (standardize-text-style device character-set style))
  (when (listp mapping)
    (assert (eql (first mapping) :style) ()
	    "Text style mappings must be atomic font names ~
	     or (:STYLE . (family face size))")
    (setf mapping (parse-text-style (cdr mapping))))
  (setf (gethash style (slot-value device 'mapping-table)) mapping))

#+CLIM-1-compatibility
(define-compatibility-function (add-text-style-mapping (setf text-style-mapping))
			       (device character-set style mapping)
  (setf (text-style-mapping device character-set style) mapping))

;;; This is broken up into two methods so any :AROUND method will only
;;; be called on the outermost recursion.
(defmethod text-style-mapping
	   ((device display-device) character-set style &optional window)
  (text-style-mapping* device character-set style window))

(defmethod text-style-mapping
	   ((device display-device) character-set (style device-font) &optional window)
  (declare (ignore character-set window))
  ;;--- What about character-set when using device fonts?
  ;;--- EQL? TYPE-EQUAL?  This is too restrictive as it stands
  (unless (eql device (device-font-display-device style))
    (error "An attempt was made to map device font ~S on device ~S, ~@
	    but it is defined for device ~S"
	   style device (device-font-display-device style)))
  (device-font-name style))

(defmethod text-style-mapping* ((device display-device) character-set style &optional window)
  (setq style (standardize-text-style device character-set (parse-text-style style)))
  (let* ((mapping-table (slot-value device 'mapping-table))
	 (result (or (gethash style mapping-table)
		     (gethash *undefined-text-style* mapping-table))))
    (when (text-style-p result)			;logical translations
      (setf result (text-style-mapping* device character-set result window)))
    result))

(defmethod text-style-mapping-exists-p ((device display-device) character-set style)
  (setq style (standardize-text-style device character-set (parse-text-style style)))
  (let* ((mapping-table (slot-value device 'mapping-table))
	 (result (gethash style mapping-table)))
    (cond ((null result) nil)
	  ((text-style-p result)		;logical translations
	   (text-style-mapping-exists-p device character-set style))
	  (t t))))

;; This method allows the device to convert logical sizes into point
;; sizes, etc.  The default method doesn't do much of anything.
(defmethod standardize-text-style ((device display-device) character-set style)
  (declare (ignore character-set))
  (unless (numberp (text-style-size style))
    (standardize-text-style-error style))
  style)

(defun standardize-text-style-error (style)
  (if (fully-merged-text-style-p style)
      (cerror "Use the undefined text style stand-in instead"
	      "The size component of ~S is not numeric.  This display-device does not know ~
	       how to map logical text style sizes"
	      style)
      (cerror "Use the undefined text style stand-in instead"
	      "The text style ~S must be a fully merged text style"
	      style))
  *undefined-text-style*)

;; For use by more specific STANDARDIZE-TEXT-STYLE methods
(defun-inline standardize-text-style-1 (display-device style character-set size-alist)
  (declare (ignore display-device character-set))
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

;;; For now, only standard character set characters are understood...
(defun-inline char-character-set-and-index (character)
  (declare (values character-set index))
  (values *standard-character-set* (char-code character)))
