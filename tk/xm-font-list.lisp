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
;; $Id: xm-font-list.lisp,v 1.20.22.1 1998/07/06 23:10:14 layer Exp $

(in-package :tk)

(defconstant xm-font-is-font 0)
(defconstant xm-font-is-fontset 1)

(defmethod convert-resource-out (parent (type (eql 'font-list)) value)
  (declare (ignore parent))
  (export-font-list value))

(defun free-font-list (font-list)
  (xm_font_list_free font-list))



(excl:ics-target-case
 (:+ics
  (defvar *font-list-tags*
      (vector (string-to-char* "ascii")
	      (string-to-char* "kanji")
	      (string-to-char* "katakana")
	      (string-to-char* "gaiji")))))

(defun export-font-list (value)
  (when (atom value)
    (setq value (list value)))
    (flet ((create-font-list-entry (font)
	     (note-malloced-object
	      (excl:ics-target-case
	       (:+ics
		(let ((tag ""))
		  (when (consp font)
		    (setq tag (svref *font-list-tags* (car font))
			  font (cdr font)))
		  (xm_font_list_entry_create tag
					     (etypecase font
					       (font xm-font-is-font)
					       (font-set xm-font-is-fontset))
					     font)))
	       (:-ics
		;; perhaps this should be xm-font-list-default-tag
	        (xm_font_list_entry_create ""
					   xm-font-is-font
					   font))))))
      (let ((font-list
	     (xm_font_list_append_entry
	      0				; old entry
	      (create-font-list-entry (car value)))))
	(dolist (font (cdr value))
	  (setq font-list
	    (xm_font_list_append_entry font-list
				       (create-font-list-entry font))))
	(note-malloced-object font-list
			      #'free-font-list))))

#+:dec3100
(defmethod convert-resource-out ((parent t) (type (eql 'xm-font-list)) value)
  (convert-resource-out parent 'font-list value))

(defmethod convert-resource-in (class (type (eql 'font-list)) value)
  (declare (ignore class))
  (import-font-list value))

(defun import-font-list (font-list)
  (let ((context
	 (with-ref-par ((context 0 *))
	   (assert
	       (not (zerop (xm_font_list_init_font_context &context font-list))))
	   context))
	(res nil))
    (with-ref-par
	;; this is actually an enumeration type (XmFontType) - let's
	;; guess at an int. in any case I don't think it matters (cim 12/14/95)
	((type 0 :unsigned-int))
      (loop
	(let ((entry (xm_font_list_next_entry context)))
	  (when (zerop entry)
	    (return nil))
	  (setf type 0)
	  (push (list
		 ""
		 (let ((font (xm_font_list_entry_get_font entry &type)))
		   (if (eq type xm-font-is-fontset)
		       ;; we should really make the whole backend
		       ;; fontset aware so that we get the right
		       ;; metrics - this will do for now (cim 3/9/95)
		       (car (fonts-of-font-set font))
		     (intern-object-address
		      font
		      'font
		      :name :unknown!))))
		res)))
      (xm_font_list_free_font_context context)
      res)))

