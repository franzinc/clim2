;; -*- mode: common-lisp; package: tk -*-
;;
;;				-[Fri Dec 16 17:15:52 1994 by duane]-
;;
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, CA  All rights reserved.
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
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: xm-font-list.lisp,v 1.13 1994/12/18 06:44:58 duane Exp $

(in-package :tk)

(defconstant xm-font-is-font 0)

(defmethod convert-resource-out (parent (type (eql 'font-list)) value)
  (declare (ignore parent))
  (export-font-list value))

(defun free-font-list (font-list)
  (xm_font_list_free font-list))

(defun export-font-list (value)
  (when (atom value)
    (setq value (list value)))
  (let* ((font-list
	  (xm_font_list_append_entry
	   0			; old entry
	   (note-malloced-object (xm_font_list_entry_create
				  xm-font-list-default-tag
				  xm-font-is-font
				  (car value))))))
    (dolist (font (cdr value))
      (setq font-list
	(xm_font_list_append_entry font-list
				   (note-malloced-object (xm_font_list_entry_create
							  ""
							  xm-font-is-font
							  font)))))
    (note-malloced-object font-list
			  #'free-font-list)))

#+:dec3100
(defmethod convert-resource-out ((parent t) (type (eql 'xm-font-list)) value)
  (convert-resource-out parent 'font-list value))

(defmethod convert-resource-in (class (type (eql 'font-list)) value)
  (declare (ignore class))
  (import-font-list value))

(defun import-font-list (font-list)
  (let ((context
	 (with-ref-par ((context 0))
	   (assert (not (zerop (xm_font_list_init_font_context context font-list))))
	   (aref context 0)))
	(res nil))
    (with-ref-par
	((type 0))
      (loop
	(let ((entry (xm_font_list_next_entry context)))
	  (when (zerop entry)
	    (return nil))
	  (setf (aref type 0) 0)
	  (push (list
		 ""
		 (let ((font (xm_font_list_entry_get_font entry type)))
		   (assert (= (aref type 0) 0))
		   (intern-object-address
		    font
		    'font
		    :name :unknown!)))
		res)))
      (xm_font_list_free_font_context context)
      res)))

