;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; $Id: temp-strings.lisp,v 1.13.24.1 2002/02/08 19:11:21 layer Exp $

(in-package :clim-internals)

;;;"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved."

;;; A specific facility for temporary strings.  This could be replaced by
;;; a general STACK-LET facility if we write something like that.

#-Genera
(progn

(defresource temporary-string
             (&key (length 100) (adjustable t))
  :constructor (make-array length
                           :element-type 'character
                           :fill-pointer 0
                           :adjustable adjustable)
  :matcher (and (eq adjustable (adjustable-array-p temporary-string))
                (or (and (not adjustable)
                         (= length (array-dimension temporary-string 0)))
                    (<= length (array-dimension temporary-string 0))))
  :initializer (setf (fill-pointer temporary-string) 0))

(defun temporary-string-p (string)
  (with-resource-rd ('temporary-string RD)
    (dovector (ts (rd-objects RD))
      (when (eq (os-object ts) string)
        (return-from temporary-string-p t)))))

(defmacro with-temporary-string ((var &key (length 100) (adjustable t)) &body body)
  `(using-resource (,var temporary-string :length ,length :adjustable ,adjustable)
     ,@body))

(defmacro evacuate-temporary-string (string-var)
  `(if (temporary-string-p ,string-var)
       (make-array (length ,string-var)
                   :element-type 'character
                   :fill-pointer (length ,string-var)
                   :initial-contents ,string-var)
       ,string-var))

)        ;#-Genera


#+Genera
(progn

(defmacro with-temporary-string ((var &key (length 100) (adjustable t)) &body body)
  `(sys:with-stack-array (,var ,length :element-type 'character
                                       :adjustable ,adjustable
                                       :fill-pointer 0)
     ,@body))

(defun temporary-string-p (string)
  (si:in-stack string))

(defmacro evacuate-temporary-string (string-var)
  `(sys:copy-if-necessary ,string-var))

)        ;#+Genera


;;; Another utility...
(defmacro with-temporary-substring ((temp-string string &optional (start 0) end)
                                    &body body)
  (let ((string-var '#:string)
        (start-var '#:start)
        (end-var '#:end)
        (length-var '#:length))
    `(let* ((,string-var ,string)
            (,start-var ,start)
            (,end-var (or ,end (length ,string-var)))
            (,length-var (- ,end-var ,start-var)))
       (with-temporary-string (,temp-string :length ,length-var)
         (setf (fill-pointer ,temp-string) ,length-var)
         (replace ,temp-string ,string-var :start2 ,start-var :end2 ,end-var)
         ,@body))))
