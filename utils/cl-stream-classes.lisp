;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; $Id: cl-stream-classes.lisp,v 2.5 2004/01/16 19:15:45 layer Exp $

(in-package :clim-utils)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1989, 1990 International Lisp Associates."

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
  `(define-group ,class-name define-class-and-predicate
     (defclass ,class-name ,@superclasses ())
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
                              #+Genera (find-package :future-common-lisp)
                              #-Genera (find-package :common-lisp)))))
    (when (null lisp-predicate)
      (error "No symbol ~S found in the Common-Lisp package." predicate-name))
    `(define-group ,predicate-name define-stream-predicate-trampoline
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

#-Cloe-Runtime                                        ;not there yet
(define-stream-predicate-trampoline open-stream-p nil
  ;;--- For now.  PW will dig up the right predicate
  :lucid-kludge (lambda (stream) (streamp stream)))
