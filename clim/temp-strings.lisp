;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: temp-strings.lisp,v 1.4 92/02/24 13:08:38 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved."

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
    (dovector (ts (RD-objects RD))
      (when (eq (os-object ts) string)
	(return-from temporary-string-p t)))))

(defmacro with-temporary-string ((var &key (length 100) (adjustable t)) &body body)
  ;;--- What happens if the temporary string gets grown via VECTOR-PUSH-EXTEND?
  ;;--- Will that cause deallocation to blow out?
  `(using-resource (,var temporary-string :length ,length :adjustable ,adjustable)
     ,@body))

(defmacro evacuate-temporary-string (string-var)
  `(if (temporary-string-p ,string-var)
       (make-array (length ,string-var)
		   :element-type 'character
		   :fill-pointer (length ,string-var)
		   :initial-contents ,string-var)
       ,string-var))

)	;#-Genera


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

)	;#+Genera


;;; Utility.
(defmacro with-temporary-substring ((string-var string start end) &body body)
  ;; --- this probably wants to be inline rather than
  ;; creating a continuation, but for testing I'll do it this way.
  `(flet ((with-temporary-substring-body (,string-var) ,@body))
     (declare (dynamic-extent #'with-temporary-substring-body))
     (invoke-with-temporary-substring ,string ,start ,end #'with-temporary-substring-body)))

(defun invoke-with-temporary-substring (string start end continuation)
  (unless end (setq end (length string)))
  (let ((length (- end start)))
    (with-temporary-string (temp-string :length length)
      (setf (fill-pointer temp-string) length)
      (replace temp-string string :start2 start :end2 end)
      (funcall continuation temp-string))))

