;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
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
;; $fiHeader$


(in-package :clim-internals)

"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved."

;;; A specific facility for temporary strings.  This could be replaced by
;;; a general STACK-LET facility if we write something like that.

#-Genera
(clim-utils::defresource temporary-string
	     (&key (length 100) (adjustable t))
  :constructor (make-array length
			   :element-type +string-array-element-type+
			   :fill-pointer 0
			   :adjustable adjustable)
  :matcher (and (eq adjustable (adjustable-array-p temporary-string))
		(or (and (not adjustable)
			 (= length (array-dimension temporary-string 0)))
		    (<= length (array-dimension temporary-string 0))))
  :initializer (setf (fill-pointer temporary-string) 0)
  )

#-Genera ;; Reach into the guts of the resource mechanism
(defun temporary-string-p (string)
  (clim-utils::with-resource-rd ('temporary-string rd)
    (dovector (ts (clim-utils::rd-objects rd))
      (when (eq (clim-utils::os-object ts) string)
	(return-from temporary-string-p t)))))

#-Genera
(defmacro with-temporary-string ((var &key (length 100) (adjustable t)) &body body)
  `(clim-utils::using-resource (,var temporary-string :length ,length :adjustable ,adjustable)
     ,@body))

#-Genera
(defmacro copy-temporary-string-if-necessary (string-var)
  `(if (temporary-string-p ,string-var)
       (setf ,string-var 
	     (make-array (length ,string-var)
                         :element-type +string-array-element-type+
			 :initial-contents ,string-var))
       ,string-var))

#-Genera
(defmacro evacuate-temporary-string (string-var)
  `(if (temporary-string-p ,string-var)
       (make-array (length ,string-var)
		   :element-type 'extended-char
		   :fill-pointer (length ,string-var)
		   :initial-contents ,string-var)
     ,string-var))

#+Genera
(defmacro with-temporary-string ((var &key (length 100) (adjustable t)) &body body)
  `(sys:with-stack-array (,var ,length :element-type 'string-char :adjustable ,adjustable
			  :fill-pointer 0)
     ,@body))

#+Genera
(defun temporary-string-p (string)
  (si:in-stack string))

#+Genera
(defmacro copy-temporary-string-if-necessary (string-var)
  `(setf ,string-var (sys:copy-if-necessary ,string-var)))


;;; Utility.
(defmacro with-temp-substring ((string-var string start end) &body body)
  ;; --- this probably wants to be inline rather than
  ;; creating a continuation, but for testing I'll do it this way.
  `(flet ((with-temp-substring
	    (,string-var) ,@body))
     (declare (dynamic-extent #'with-temp-substring))
     (with-temp-substring-1 ,string ,start ,end #'with-temp-substring)))

(defun with-temp-substring-1 (string start end continuation)
  (unless end (setq end (length string)))
  (let ((length (- end start)))
    (with-temporary-string (temp-string :length length)
      (setf (fill-pointer temp-string) length)
      (replace temp-string string :start2 start :end2 end)
      (funcall continuation temp-string))))

