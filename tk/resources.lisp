;; -*- mode: common-lisp; package: tk -*-
;;
;;				-[]-
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
;; $fiHeader: resources.cl,v 1.3 92/01/06 20:43:48 cer Exp Locker: cer $

(in-package :tk)

(defmacro define-enumerated-resource (type elements)
  `(progn
     (defmethod convert-resource-out ((parent t) (type (eql ',type)) value)
       (or (position value ',elements)
	   (error "cannot convert ~S to type ~S" value ',type)))
     (defmethod convert-resource-in ((parent t) (type (eql ',type)) value)
       (setq value (ash value -24))
       (elt ',elements value))))


(defforeign 'set_values 
    :entry-point "_XtSetValues")


(defun set-values (widget &rest values)
  (let ((arglist (make-arglist-for-class (class-of widget) widget values)))
    (set_values (object-handle widget)
		arglist
		(truncate (length arglist) 2))))


(defun make-arglist-for-class (class parent args)
  (do ((new-args nil)
       (resources-1 (class-resources class))
       (resources-2 (and parent (class-constraint-resources
				    (class-of parent))))
       keyword
       value)
      ((null args)
       (make-array (length new-args)
		   :element-type '(unsigned-byte 32)
		   :initial-contents (nreverse new-args)))
    (setq keyword (pop args)
	  value (pop args))
    (let ((resource (or (find keyword resources-1 :key #'resource-name)
			(find keyword resources-2 :key #'resource-name))))
      (when resource
	(push (resource-original-name resource) new-args)
	(push (convert-resource-out parent (resource-type resource) value)
	      new-args)))))


(defmethod convert-resource-out (parent type value)
  (cerror "Try again" "cannot convert-out resource for ~S,~S,~S" parent type value)
  (convert-resource-out parent type value))


(defmethod convert-resource-out ((parent t) (type (eql 'xm-string)) value)
  (string_create_l_to_r (string-to-char* value) (string-to-char* "")))

(defmethod convert-resource-out ((parent t) (type (eql 'orientation)) value)
  (ecase value
    (:vertical 1)
    (:horizontal 2)))

(defforeign 'get_values 
    :entry-point "_XtGetValues")

(def-c-type (x-arglist :in-foreign-space) 1 :unsigned-long)

(defun get-values (widget &rest resources)
  (let* ((class (class-of widget))
	 (class-resources (class-resources class))
	 (arglist nil)
	 rds)
    
    (dolist (r resources)
      (let ((resource (find r class-resources :key #'resource-name)))
	(unless resource
	  (error "No such resource ~S for class ~S widget ~S"
		 r class widget))
	(push resource rds)
	(push (resource-original-name resource) arglist)
	(push (make-x-arglist) arglist)))
    
    (setq arglist (coerce (nreverse arglist) '(vector (unsigned-byte 32))))
    (get_values (object-handle widget)
		arglist
		(truncate (length arglist) 2))
    (do ((rs (nreverse rds) (cdr rs))
	 (i 1 (+ i 2))
	 values)
	((null rs)
	 (values-list (nreverse values)))
      (push
       (convert-resource-in
	widget
	(resource-type (car rs))
	(x-arglist (aref arglist i) 0))
       values))))

(defmethod convert-resource-in (class type value)
  (cerror "Try again" "cannot convert-in resource for ~S,~S,~S" class type value)
  (convert-resource-in class type value))

(defconstant xm_string_default_char_set "")

(defmethod convert-resource-in (class (type (eql 'xm-string)) value)
  (with-ref-par ((string 0))
		(string_get_l_to_r value xm_string_default_char_set string)
		(char*-to-string (aref string 0))))

(defmethod convert-resource-out ((parent t) (type (eql 'separator-type)) value)
  (ecase value
    (:single-line 1)))
      
(defmethod convert-resource-out ((parent t) (type (eql 'menu-widget)) value)
  (object-handle value))

(defmethod convert-resource-out ((parent t) (type (eql 'horizontal-dimension)) value)
  (check-type value integer)
  value)


(defmethod convert-resource-out ((parent t) (type (eql 'vertical-dimension)) value)
  value)

(defmethod convert-resource-out ((parent t) (type (eql 'visual-policy)) value)
  (ecase value
    (:constant 1)))


(define-enumerated-resource scrolling-policy (:automatic :application-defined))



(defmethod convert-resource-out ((parent t) (type (eql 'scroll-bar-display-policy)) value)
  (ecase value
    (:static 0)))


(defmethod convert-resource-out ((parent t) (type (eql 'packing)) value)
  (ecase value
    (:pack-column 2)))

(defmethod convert-resource-out ((parent t) (type (eql 'short)) value)
  value)

(defmethod convert-resource-out ((parent t) (type (eql 'label-type)) value)
  (ecase value
    (:pixmap 1)
    (:string 2)))


(defmethod convert-resource-out ((parent t) (type (eql 'alignment)) value)
  (ecase value
    (:center 1)
    (:beginning 0)
    (:end 2)))

(defmethod convert-resource-out ((parent t) (type (eql 'prim-foreground-pixmap)) value)
  (convert-pixmap-out parent value))

(defmethod convert-resource-out ((parent t) (type (eql 'gadget-pixmap)) value)
  (convert-pixmap-out parent value))

(defun convert-pixmap-out (parent value)
  (let* ((display (widget-display parent))
	 (screen (display-default-screen display))
	 (white (screen_white_pixel screen))
	 (black (screen_black_pixel screen)))
    (get_pixmap screen (string-to-char* value) white black)))

(defmethod convert-resource-out ((parent t) (type (eql 'boolean)) value)
  (if value 1 0))

(defmethod convert-resource-out ((parent  t) (type (eql 'string))
				 value)
  (string-to-char* value))

(define-enumerated-resource dialog-style (:modeless
					  :primary-application-modal
					  :full-application-modal
					  :system-modal))

(defmethod convert-resource-out ((parent t) (type (eql 'font-struct)) value)
  (object-handle (load-query-font (widget-display parent) 
				  value)))

(defmethod convert-resource-in ((parent t) (type (eql 'string)) value)
  (unless (zerop value)
    (char*-to-string value)))

(defmethod convert-resource-in ((parent t) (type (eql 'boolean))
				value)
  (not (zerop (ash value -24))))

(defmethod convert-resource-in ((parent t) (type (eql 'cardinal))
				value)
  value)

;; We have to do this because we pass a pointer to :unsigned-long but
;; the actual type is :short
(defmethod convert-resource-in ((parent t) (type (eql 'vertical-dimension)) value)
  (ash value -16))

(defmethod convert-resource-in ((parent t) (type (eql 'horizontal-dimension)) value)
  (ash value -16))

(defmethod convert-resource-in ((parent t) (type (eql 'horizontal-position)) value)
  (ash value -16))

(defmethod convert-resource-in ((parent t) (type (eql 'vertical-position)) value)
  (ash value -16))

(defmethod convert-resource-out ((parent t) (type (eql 'pixel)) value)
  (etypecase value
	     (integer value)
	     (color (allocate-color (default-colormap (widget-display parent))
				    value))))
			   

(defmethod convert-resource-in ((parent t) (type (eql 'pixel)) value)
  value)

(defmethod convert-resource-out ((parent t) (type (eql
						   'horizontal-position))  value)
  value)

(defmethod convert-resource-out ((parent t) (type (eql
						   'vertical-position))  value)
  value)

(define-enumerated-resource resize-policy (:none :grow :any))

(defmethod convert-resource-in ((parent t) (type (eql 'int)) value) value)
(defmethod convert-resource-out ((parent t) (type (eql 'int)) value) value)


(define-enumerated-resource indicator-type (nil :n-of-many :one-of-many))


(defmethod convert-resource-out ((parent t) (type (eql 'shell-vert-dim)) x) x)
(defmethod convert-resource-out ((parent t) (type (eql 'shell-vert-pos)) x) x)
(defmethod convert-resource-out ((parent t) (type (eql 'shell-horiz-dim)) x) x)
(defmethod convert-resource-out ((parent t) (type (eql
						   'shell-horiz-pos)) x) x)

(defmethod convert-resource-in ((parent t) (type (eql 'shell-vert-dim)) x) x)
(defmethod convert-resource-in ((parent t) (type (eql 'shell-vert-pos)) x) x)
(defmethod convert-resource-in ((parent t) (type (eql 'shell-horiz-dim)) x) x)
(defmethod convert-resource-in ((parent t) (type (eql
						  'shell-horiz-pos)) x) x)


(define-enumerated-resource delete-response (:destroy :unmap :do-nothing))

(defmethod convert-resource-out ((parent t) (type (eql 'window)) x)
  (object-handle x))


(define-enumerated-resource keyboard-focus-policy (:explicit :pointer))
  


(defmethod convert-resource-out ((parent t) (type (eql 'xm-background-pixmap)) value)
  (etypecase value
    (pixmap
     (encode-pixmap value))))



(def-c-type (xtk-widget-list :in-foreign-space) 1 * xtk-widget)

(defmethod convert-resource-in ((widget t) (type (eql 'widget-list)) x)
  (let ((r nil))
    (dotimes (i (widget-num-children widget))
      (push (convert-resource-in 
	     widget 'widget (xtk-widget-list x i))
	    r))
    (nreverse r)))


(defmethod convert-resource-in ((widget t) (type (eql 'widget)) x)
  (intern-widget x))
