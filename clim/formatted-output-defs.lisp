;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM; Base: 10; Lowercase: Yes -*-
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

;; $fiHeader: formatted-output-defs.lisp,v 1.1 91/09/13 14:52:45 cer Exp Locker: cer $

(in-package :clim)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved.
Copyright (c) 1991, Franz Inc. All rights reserved
 Portions copyright (c) 1989, 1990 International Lisp Associates."

;;; We want to evaluate the user's :RECORD-TYPE option, right?
(defmacro formatting-table ((&optional stream
			     &rest options
			     &key inter-row-spacing inter-column-spacing
				  record-type
				  multiple-columns multiple-columns-inter-column-spacing
				  equalize-column-widths
				  (move-cursor t))
			    &body body)
  (declare (ignore inter-row-spacing inter-column-spacing record-type
		   multiple-columns multiple-columns-inter-column-spacing
		   equalize-column-widths move-cursor))
  #+Genera (declare (zwei:indentation 0 3 1 1))
  (default-output-stream stream formatting-table)
  `(flet ((formatting-table-body (,stream) ,@body))
     (declare (dynamic-extent #'formatting-table-body))
     (formatting-table-internal ,stream #'formatting-table-body ,@options)))

(defmacro formatting-row ((&optional stream &key record-type) &body body)
  #+Genera (declare (zwei:indentation 0 3 1 1))
  (default-output-stream stream formatting-row)
  (unless record-type (setq record-type `'row-output-record))
  `(with-new-output-record (,stream ,record-type)
     ,@body))

(defmacro formatting-column ((&optional stream &key record-type) &body body)
  #+Genera (declare (zwei:indentation 0 3 1 1))
  (default-output-stream stream formatting-column)
  (unless record-type (setq record-type `'column-output-record))
  `(with-new-output-record (,stream ,record-type)
     ,@body))

(defmacro formatting-cell ((&optional stream
			    &rest options
			    &key record-type (align-x ':left) (align-y ':top)
				 minimum-width minimum-height)
			   &body body)
  (declare (ignore record-type align-x align-y minimum-width minimum-height))
  #+Genera (declare (zwei:indentation 0 3 1 1))
  (default-output-stream stream formatting-cell)
  `(flet ((formatting-cell-body (,stream) ,@body))
     (declare (dynamic-extent #'formatting-cell-body))
     (formatting-cell-internal ,stream #'formatting-cell-body ,@options)))

(defmacro formatting-item-list ((&optional stream
				 &rest options
				 &key record-type
				      inter-row-spacing inter-column-spacing
				      (no-initial-spacing t)
				      n-columns n-rows
				      max-width max-height
				      stream-width stream-height
				      (move-cursor t))
				&body body)
  (declare (ignore inter-row-spacing inter-column-spacing no-initial-spacing
		   record-type n-columns n-rows max-width max-height
		   stream-width stream-height move-cursor))
  #+Genera (declare (zwei:indentation 0 3 1 1))
  (default-output-stream stream formatting-item-list)
  `(flet ((formatting-item-list-body (,stream) ,@body))
     (declare (dynamic-extent #'formatting-item-list-body))
     (formatting-item-list-internal ,stream #'formatting-item-list-body ,@options)))


(defmacro surrounding-output-with-border ((&optional stream
					   &key (shape ':rectangle) (move-cursor t))
					  &body body)
  #+Genera (declare (zwei:indentation 0 3 1 1))
  (default-output-stream stream surrounding-output-with-border)
  `(flet ((surrounding-output-with-border-body (,stream) ,@body))
     (declare (dynamic-extent #'surrounding-output-with-border-body))
     (surrounding-output-with-border-1 ,stream ,shape #'surrounding-output-with-border-body
				       :move-cursor ,move-cursor)))

(defmacro filling-output ((&optional stream &rest keys
			   &key (fill-width '(80 :character))
				(break-characters '(#\space))
				after-line-break after-line-break-initially)
			  &body body)
  #+Genera (declare (zwei:indentation 0 3 1 1))
  (declare (ignore fill-width break-characters after-line-break after-line-break-initially))
  (default-output-stream stream filling-output)
  `(flet ((filling-output-body (,stream) ,@body))
     (declare (dynamic-extent #'filling-output-body))
     (filling-output-1 ,stream #'filling-output-body ,@keys)))

(defmacro indenting-output ((stream indentation &key (move-cursor t))
			    &body body)
  #+Genera (declare (zwei:indentation 0 3 1 1))
  (default-output-stream stream indenting-output)
  `(flet ((indenting-output-body (,stream) ,@body))
     (declare (dynamic-extent #'indenting-output-body))
     (indenting-output-1 ,stream ,indentation #'indenting-output-body
			 :move-cursor ,move-cursor)))


(defmacro updating-output
	  ((stream &rest args
		   &key (record-type `'updating-output-record)
			(unique-id `'assign-sequential-unique-ids) (id-test `#'eql)
			(cache-value `'unsupplied-cache-value) (cache-test `#'eql)
			copy-cache-value parent-cache
			output-record fixed-position all-new
		   &allow-other-keys)
	   &body body)
  (declare (ignore fixed-position all-new))
  #+Genera (declare (zwei:indentation 0 3 1 1))
  (default-output-stream stream updating-output)
  `(flet ((updating-output-body (,stream) ,@body))
     (updating-output-internal ,stream #'updating-output-body
			       ,record-type ,unique-id ,id-test
			       ,cache-value ,cache-test
			       ,copy-cache-value ,parent-cache ,output-record 
			       ,@(rem-keywords args
				  ;; remove all args that aren't init-args to the record.
				  '(:record-type :id-test :cache-test :copy-cache-value
				    :parent-cache :output-record)))))
