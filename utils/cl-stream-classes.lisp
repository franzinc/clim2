;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-

(in-package :clim-utils)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 International Lisp Associates."

;;; All of this is taken from the STREAM-DEFINITION-BY-USER proposal to
;;; the X3J13 committee, made by David Gray of TI on 22 March 1989.  No
;;; Lisp implementation yet supports this proposal, so we implement it
;;; here in this separate package.  This way we will be ready when some
;;; Lisp implementation adopts it (or something like it).

;;; This file defines classes in the Gray proposal, plus class-based
;;; predicates.  This file is entirely #-CLIM-uses-Lisp-stream-classes,
;;; which means that any class and function names are shadowed in this
;;; package (not inherited from the CL package).

(defmacro define-class-and-predicate (class-name predicate-name &body superclasses)
  `(clim-utils:define-group ,class-name define-class-and-predicate
     (defclass ,class-name ,@superclasses ())
     ;;--- Genera can't put both a class and methods on that class in the same group.
     ,@(when predicate-name
	 `((define-stream-predicate-trampoline ,predicate-name ,class-name)))))

(defmacro define-stream-predicate-trampoline (predicate-name class-name
					      &key lucid-kludge)
  #-Lucid (declare (ignore lucid-kludge))
  (let ((lisp-predicate (or #+Lucid lucid-kludge
			    (find-symbol
			      (symbol-name predicate-name)
			      ;; #+Genera *Sigh*.  OPEN-STREAM-P is in
			      ;; FUTURE-COMMON-LISP only.  Of course, it's
			      ;; not defined yet, but at least it's exported.
			      #+Genera "FUTURE-COMMON-LISP"
			      #-Genera "COMMON-LISP"))))
    (when (null lisp-predicate)
      (error "No symbol ~S found in the Common-Lisp package." predicate-name))
    `(clim-utils:define-group ,predicate-name define-stream-predicate-trampoline
       (defgeneric ,predicate-name (object))
       (defmethod  ,predicate-name ((object t)) (,lisp-predicate object))
       ,@(when class-name
	   `((defmethod ,predicate-name ((object ,class-name)) 't))))))


;;; The classes themselves.

(define-class-and-predicate fundamental-stream streamp
  (#+CCL-2 ccl::stream))

(define-class-and-predicate fundamental-input-stream input-stream-p
  (fundamental-stream #+CCL-2 ccl:input-stream))

(define-class-and-predicate fundamental-output-stream output-stream-p
  (fundamental-stream #+CCL-2 ccl:output-stream))

(define-class-and-predicate fundamental-character-stream nil
  (fundamental-stream))

(define-class-and-predicate fundamental-binary-stream nil
  (fundamental-stream))

(define-class-and-predicate fundamental-character-input-stream nil
  (fundamental-input-stream fundamental-character-stream))
							
(define-class-and-predicate fundamental-character-output-stream nil
  (fundamental-output-stream fundamental-character-stream))
							 
(define-class-and-predicate fundamental-binary-input-stream nil
  (fundamental-input-stream fundamental-binary-stream))
						     
(define-class-and-predicate fundamental-binary-output-stream nil
  (fundamental-output-stream fundamental-binary-stream))

(define-stream-predicate-trampoline open-stream-p nil
  ;;--- For now.  PW will dig up the right predicate
  :lucid-kludge (lambda (stream) (streamp stream)))
