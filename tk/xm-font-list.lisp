;; See the file LICENSE for the full license governing this code.
;;

(in-package :tk)

(defconstant xm-font-is-font 0)
(defconstant xm-font-is-fontset 1)

(defmethod convert-resource-out (parent (type (eql 'font-list)) value)
  (declare (ignore parent))
  (export-font-list value))

(defun free-font-list (font-list)
  (xm_font_list_free font-list))

;;; Bascially an alias for 'font-list in Motif2.1
(defmethod convert-resource-out (parent (type (eql 'tk::label-font-list)) value)
  (declare (ignore parent))
  (export-font-list value))
(defmethod convert-resource-in (class (type (eql 'tk::label-font-list)) value)
  (declare (ignore class))
  (import-font-list value))

;;; Bascially an alias for 'font-list in Motif2.1
(defmethod convert-resource-out (parent (type (eql 'tk::button-font-list)) value)
  (declare (ignore parent))
  (export-font-list value))
(defmethod convert-resource-in (class (type (eql 'tk::button-font-list)) value)
  (declare (ignore class))
  (import-font-list value))

;;; Bascially an alias for 'font-list in Motif2.1
(defmethod convert-resource-out (parent (type (eql 'tk::text-font-list)) value)
  (declare (ignore parent))
  (export-font-list value))
(defmethod convert-resource-in (class (type (eql 'tk::text-font-list)) value)
  (declare (ignore class))
  (import-font-list value))




(excl:ics-target-case
 (:+ics
  (defvar *font-list-tags*
      (make-array 4 :adjustable t :fill-pointer t
                  :initial-contents
                  (list
                   ;; bug13059/spr27144 - we want these strings to
                   ;; survive a dumplisp       
                   (excl:string-to-native "ascii")
                   (excl:string-to-native "kanji")
                   (excl:string-to-native "katakana")
                   (excl:string-to-native "gaiji"))
       
                  ))))


(defun export-font-list (value)
  (when (atom value)
    (setq value (list value)))
  (flet ((create-font-list-entry (font)
           (note-malloced-object
            (excl:ics-target-case
              (:+ics
               (let ((tag ""))
                 (when (consp font)
                   (setq tag (aref *font-list-tags* (car font))
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

