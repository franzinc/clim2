;; -*- mode: common-lisp; package: tk -*-
;;
;;				-[Fri Jul 30 16:14:28 1993 by colin]-
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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $fiHeader: resources.lisp,v 1.56 1996/03/01 05:43:35 colin Exp $


(in-package :tk)

#+not-yet
(defmacro define-convert-out-method ((type) &body body)
  `(defmethod convert-resource-out ((parent t) (type (eql ',type)) value)
     ,@body))

#+not-yet
(defmacro define-convert-in-method ((type) &body body)
  `(defmethod convert-resource-in ((parent t) (type (eql ',type)) value)
     ,@body))


(defmacro define-enumerated-resource (type elements)
  (if (consp (car elements))
      `(progn
	 (defmethod convert-resource-out (parent (type (eql ',type)) value)
	   (declare (ignore parent))
	   (case value
	     ,@elements
	     (t (error "cannot convert ~S to type ~S" value ',type))))
	 (defmethod resource-type-get-memref-type ((type (eql ',type)))
	   :unsigned-byte)
	 (defmethod convert-resource-in (parent (type (eql ',type)) value)
	   (declare (ignore parent))
	   (ecase value
	     ,@(mapcar #'(lambda (element) (list (second element) (first element))) elements))))
    `(progn
       (defmethod convert-resource-out (parent (type (eql ',type)) value)
	 (declare (ignore parent))
	 (or (position value ',elements)
	     (error "cannot convert ~S to type ~S" value ',type)))
       (defmethod resource-type-get-memref-type ((type (eql ',type)))
	 :unsigned-byte)
       (defmethod convert-resource-in (parent (type (eql ',type)) value)
	 (declare (ignore parent))
	 (elt ',elements value)))))


(defmethod convert-resource-in (class type value)
  (cerror "Try again" "cannot convert-in resource for ~S,~S,~S" class type value)
  (convert-resource-in class type value))

(define-enumerated-resource separator-type (:no-line
					    :single-line
					    :double-line
					    :single-dashed-line
					    :double-dashed-line
					    :shadow-etched-in
					    :shadow-etched-out))


(define-enumerated-resource scrolling-policy (:automatic :application-defined))


(define-enumerated-resource dialog-style (:modeless
					  :primary-application-modal
					  :full-application-modal
					  :system-modal))


(define-enumerated-resource resize-policy (:none :grow :any))

(define-enumerated-resource indicator-type (nil :n-of-many :one-of-many))

(define-enumerated-resource delete-response (:destroy :unmap :do-nothing))

(define-enumerated-resource keyboard-focus-policy (:explicit :pointer))

(define-enumerated-resource packing (:no-packing
				     :tight
				     :column
				     :none))

(define-enumerated-resource row-column-type (:work-area
					     :menu-bar
					     :menu-pulldown
					     :menu-popup
					     :menu-option))

(define-enumerated-resource edit-mode (:multi-line :single-line))

(define-enumerated-resource visual-policy (:variable :constant))

(define-enumerated-resource scroll-bar-display-policy (:static :as-needed))

(define-enumerated-resource selection-policy (:single-select
					      :multiple-select
					      :extended-select :browse-select))


;; from OpenLook.h

(defparameter *ol-defines* '(
			     (:ignore -1)
			     (:ignore 65535)
			     (:absent-pair		0)
			     (:all			1)
			     (:always		2)
			     (:atom-help		3)
			     (:both			4)
			     (:bottom		5)
			     (:buttonstack		6)
			     (:center		7)
			     (:class-help		8)
			     (:columns		9)
			     (:copy-mask-value	10)
			     (:copy-size		11)
			     (:copy-source-value	12)
			     (:current		13)
			     (:default-pair		14)
			     (:disk-source		15)
			     (:display-form		16)
			     (:down			17)
			     (:existing-source	18)
			     (:fixedcols		19)
			     (:fixedheight		20)
			     (:fixedrows		21)
			     (:fixedwidth		22)
			     (:flat-button		23)
			     (:flat-checkbox	24)
			     (:flat-container	25)
			     (:flat-exclusives	26)
			     (:flat-help		27)
			     (:flat-nonexclusives	28)
			     (:halfstack		29)
			     (:horizontal		30)
			     (:image		31)
			     (:in			32)
			     (:indirect-source	33)
			     (:label		34)
			     (:left			35)
			     (:mask-pair		36)
			     (:maximize		37)
			     (:millimeters		38)
			     (:minimize		39)
			     (:never		40)
			     (:next			41)
			     (:none			42)
			     (:nonebottom		43)
			     (:noneleft		44)
			     (:noneright		45)
			     (:nonetop		46)
			     (:notices		47)
			     (:no-virtual-mapping	48)
			     (:oblong		49)
			     (:out			50)
			     (:override-pair	51)
			     (:pixels		52)
			     (:points		53)
			     (:popup		54)
			     (:previous		55)
			     (:prog-defined-source	56)
			     (:rectbutton		57)
			     (:right		58)
			     (:rows			59)
			     (:source-form		60)
			     (:source-pair		61)
			     (:stayup		62)
			     (:string		63)
			     (:string-source	64)
			     (:text-append		65)
			     (:text-edit		66)
			     (:text-read		67)
			     (:top			68)
			     (:transparent-source	69)
			     (:vertical		70)
			     (:virtual-button	71)
			     (:virtual-key		72)
			     (:widget-help		73)
			     (:window-help		74)
			     (:wrap-any		75)
			     (:wrap-white-space	76)
			     (:continuous		77)
			     (:granularity		78)
			     (:release		79)
			     (:tickmark		80)
			     (:percent		81)
			     (:slidervalue		82)
			     (:wt-base		83)
			     (:wt-cmd		84)
			     (:wt-notice		85)
			     (:wt-help		86)
			     (:wt-other		87)
			     (:success		88)
			     (:duplicate-key	89)
			     (:duplicatekey		89)
			     (:bad-key		90)
			     (:menu-full		91)
			     (:menu-limited		92)
			     (:menu-cancel		93)
			     (:selectkey		94)
			     (:menukey		95)
			     (:menudefault		96)
			     (:menudefaultkey	97)
			     (:hsbmenu		98)
			     (:vsbmenu		99)
			     (:adjustkey		100)
			     (:nextapp		101)
			     (:nextwindow		102)
			     (:prevapp		103)
			     (:prevwindow		104)
			     (:windowmenu		105)
			     (:workspacemenu	106)
			     (:defaultaction	108)
			     (:drag			109)
			     (:drop			110)
			     (:togglepushpin	111)
			     (:pageleft		112)
			     (:pageright		113)
			     (:scrollbottom		114)
			     (:scrolltop		115)
			     (:multiright		116)
			     (:multileft		117)
			     (:multidown		118)
			     (:multiup		119)
			     (:immediate		120)
			     (:moveup		121)
			     (:movedown		122)
			     (:moveright		123)
			     (:moveleft		124)
			     (:click-to-type	125)
			     (:realestate		126)
			     (:underline		127)
			     (:highlight		128)
			     (:inactive		129)
			     (:display		130)
			     (:proc			131)
			     (:size-proc		132)
			     (:draw-proc		133)
			     (:pinned-menu		134)
			     (:press-drag-menu	135)
			     (:stayup-menu		136)
			     (:pointer		137)
			     (:inputfocus		138)
			     (:quit			142)
			     (:destroy		143)
			     (:dismiss		144)
			     (:pre			145)
			     (:post			146)
			     (:grow-off     	147)
			     (:grow-horizontal      148)
			     (:grow-vertical        149)
			     (:grow-both    	150)
			     ))

(defmethod convert-resource-in (parent (type (eql 'ol-define)) value)
  (declare (ignore parent))
  (or (car (find value *ol-defines* :key #'second))
      (call-next-method)))


(defmethod convert-resource-out (parent (type (eql 'ol-define)) value)
  (declare (ignore parent))
  (or (second (find value *ol-defines* :key #'first))
      (call-next-method)))




(defun fill-sv-cache (parent-class class resources)
  (let* ((len (length resources))
	 (arglist (make-xt-arglist :number len))
	 (rds nil)
	 (constraint-resource-used nil)
	 (i 0))
    (dolist (r resources)
      (let ((resource (or (find-class-resource class r)
			  (psetq constraint-resource-used t)
			  (and parent-class (find-class-constraint-resource parent-class r)))))
	(unless resource
	  (error "No such resource ~S for widget of class ~S "
		 r class))
	(let ((type (resource-type resource)))
	  (push (list (resource-type-set-conversion-p type)
		      type)
		rds))
	(setf (xt-arglist-name arglist i) (resource-original-name resource))
	(incf i)))
    (let ((r (list* arglist len (nreverse rds))))
      (if constraint-resource-used
	  r
	(setf (gethash resources (class-set-values-cache class)) r)))))


(defun set-values (widget &rest resources-and-values)
  (declare (optimize (speed 3))
	   (dynamic-extent resources-and-values))
  (with-malloced-objects
      (let ((resources nil)
	    (values nil))
	(do ((rvs resources-and-values (cddr rvs)))
	    ((null rvs))
	  (push (car rvs) resources)
	  (push (cadr rvs) values))
	(setq resources (nreverse resources))
	(setq values (nreverse values))
	(excl::without-interrupts
	  ;; We don't really want anyone else to grab the same cache entry.
	  (let* ((class (class-of widget))
		 (parent-class (let ((p (tk::widget-parent widget)))
				 (and p (class-of p))))
		 (entry (gethash resources (class-set-values-cache class))))
	    (declare (optimize (safety 0)))
	    (unless entry
	      (setf entry (fill-sv-cache parent-class class resources)))

	    (destructuring-bind (arglist length &rest resource-descriptions)
		entry
	      (do ((resdess resource-descriptions (cdr resdess))
		   (i 0 (1+ i))
		   (values values (cdr values)))
		  ((null resdess))
		(destructuring-bind (convert-p type)
		    (car resdess)
		  (setf (xt-arglist-value arglist i)
		    (if convert-p
			(convert-resource-out widget type (car values))
		      (car values)))))
	      (xt_set_values widget arglist length)))))))

;; This is used by initialize-instance methods for widgets...

(defun make-arglist-for-class (class parent args)
  (declare (optimize (speed 3) (safety 0)))
  (do ((new-args nil)
       keyword
       value)
      ((null args)
       (let* ((new-args (nreverse new-args))
	      (n (truncate (length new-args) 2))
	      (arglist (make-xt-arglist :number n :in-foreign-space nil)))
	 (dotimes (i n)
	   (setf (xt-arglist-name arglist i) (pop new-args)
		 (xt-arglist-value arglist i) (pop new-args)))
	 (values arglist n)))
    (setq keyword (pop args)
	  value (pop args))
    (let ((resource (or (find-class-resource class keyword)
			(find-class-constraint-resource class keyword))))
      (when resource
	(push (resource-original-name resource) new-args)
	(let ((type (resource-type resource)))
	  (push
	   (if (resource-type-set-conversion-p type)
	       (convert-resource-out parent type value)
	     value)
	   new-args))))))



;; I would really prefer to define these as separate methods.  However
;; that leads to a lot of code!
(defmethod resource-type-get-conversion-p (type)
  (case type
    (position nil)
    (vertical-position nil)
    (horizontal-position nil)
    (shell-vert-pos nil)
    (shell-horiz-pos nil)
    (dimension nil)
    (vertical-dimension nil)
    (horizontal-dimension nil)
    (shell-vert-dim nil)
    (shell-horiz-dim nil)
    (cardinal nil)
    (pixel nil)
    (int nil)
    (short nil)
    (function nil)
    (top-item-position nil)
    (t t)))

(defmethod resource-type-set-conversion-p (type)
  (case type
    (position nil)
    (short nil)
    (vertical-position nil)
    (horizontal-position nil)
    (shell-vert-pos nil)
    (shell-horiz-pos nil)
    (dimension nil)
    (vertical-dimension nil)
    (vertical-int nil)
    (horizontal-dimension nil)
    (horizontal-int nil)
    (shell-vert-dim nil)
    (shell-horiz-dim nil)
    (cardinal nil)
    (pixel nil)
    (short nil)
    (int nil)
    (function nil)
    (top-item-position nil)
    (t t)))

(defmethod resource-type-get-memref-type (type)
  (case type
    (short :signed-word)
    (position :signed-word)
    (vertical-position :signed-word)
    (horizontal-position :signed-word)
    (shell-vert-pos :signed-word)
    (shell-horiz-pos :signed-word)
    (dimension :signed-word)
    (vertical-dimension :signed-word)
    (horizontal-dimension :signed-word)
    (shell-vert-dim :signed-word)
    (shell-horiz-dim :signed-word)
    (int :signed-long)
    (boolean :unsigned-byte)
    (ol-define :unsigned-word)
    (t :unsigned-long)))


(defun fill-gv-cache (parent-class class resources)
  (setq resources (copy-list resources)) ; So get-values can declare dynamic-extent
  (let* ((len (length resources))
	 (arglist (make-xt-arglist :number len))
	 (rds nil)
	 (constraint-resource-used nil)
	 (i 0))
    (dotimes (j len)
      (setf (xt-arglist-value arglist j)
	(excl::malloc 8)))	;--- A crock...
    (dolist (r resources)
      (let ((resource (or (find-class-resource class r)
			  (psetq constraint-resource-used t)
			  (and parent-class
			       (find-class-constraint-resource parent-class r)))))
	(unless resource
	  (error "No such resource ~S for widget of class ~S "
		 r class))
	(let ((type (resource-type resource)))
	  (push (list (resource-type-get-memref-type type)
		      (resource-type-get-conversion-p type)
		      type)
		rds))
	(setf (xt-arglist-name arglist i) (resource-original-name resource))
	(incf i)))
    (let ((r (list* arglist len (nreverse rds))))
      (if constraint-resource-used
	  ;; so the crock continues
	  (progn
	    (dotimes (j len)
	      (note-malloced-object (xt-arglist-value arglist j)))
	    r)
	(setf (gethash resources (class-get-values-cache class)) r)))))

(defun get-values (widget &rest resources)
  (declare (optimize (speed 3))
	   (dynamic-extent resources))
  (with-malloced-objects
      (excl::without-interrupts
	;; We don't really want anyone else to grab the same cache entry.
	(let* ((class (class-of widget))
	       (parent-class (let ((p (tk::widget-parent widget)))
			       (and p (class-of p))))
	       (entry (gethash resources (class-get-values-cache class))))
	  (declare (optimize (safety 0)))
	  (unless entry
	    (setf entry (fill-gv-cache parent-class class resources)))

	  (destructuring-bind (arglist length &rest resource-descriptions)
	      entry
	    (xt_get_values widget arglist length)

	    (let ((i 0)
		  (values nil))
	      (dolist (resource-desc resource-descriptions)
		(destructuring-bind (get-memref-type convert-p type)
		    resource-desc
		  (let ((value (sys:memref-int (xt-arglist-value arglist i) 0 0
					       get-memref-type)))
		    (incf i)
		    (if convert-p
			(setq value (convert-resource-in widget type value)))
		    (push value values))))
	      (values-list (nreverse values))))))))



(defmethod convert-resource-out (parent type value)
  (cerror "Try again" "cannot convert-out resource for ~S,~S,~S" parent type value)
  (convert-resource-out parent type value))

(defmethod convert-resource-out :around (parent type value)
  (declare (optimize (speed 3))
	   (ignore parent type value))
  (let ((result (call-next-method)))
    (if (integerp result)
	result
      (ff:foreign-pointer-address result))))

(defmethod convert-resource-out ((parent t) (type (eql 'orientation)) value)
  (ecase value
    (:vertical 1)
    (:horizontal 2)))

#+dec3100
(defmethod convert-resource-out ((parent t) (type (eql 'xm-orientation)) value)
  (ecase value
    (:vertical 1)
    (:horizontal 2)))

(defmethod convert-resource-out ((parent t) (type (eql 'menu-widget)) value)
  value)

(defmethod convert-resource-out ((parent t) (type (eql 'label-type)) value)
  (ecase value
    (:pixmap 1)
    (:string 2)))

#+:dec3100
(defmethod convert-resource-out ((parent t) (type (eql 'xm-label-type)) value)
  (convert-resource-out parent 'label-type value))

(defmethod convert-resource-out ((parent t) (type (eql 'alignment)) value)
  (ecase value
    (:center 1)
    (:beginning 0)
    (:end 2)))

#+:dec3100
(defmethod convert-resource-out ((parent t) (type (eql 'xm-alignment)) value)
  (convert-resource-out parent 'alignment value))

(defmethod convert-resource-in ((parent t) (type (eql 'alignment)) value)
  (ecase value
    (1 :center)
    (0 :beginning)
    (2 :end)))

(defmethod convert-resource-out ((parent t) (type (eql 'prim-foreground-pixmap)) value)
  (convert-pixmap-out parent value))

(defmethod convert-resource-out ((parent t) (type (eql 'man-foreground-pixmap)) value)
  (convert-pixmap-out parent value))

(defmethod convert-resource-out ((parent t) (type (eql 'gadget-pixmap)) value)
  (convert-pixmap-out parent value))

(defmethod convert-resource-out ((parent t) (type (eql 'pixmap)) value)
  (convert-pixmap-out parent value))

(defmethod convert-resource-out ((parent t) (type (eql 'bitmap)) value)
  (convert-pixmap-out parent value))

(defmethod convert-pixmap-out (parent (value pixmap))
  value)

(defmethod convert-resource-out ((parent t) (type (eql 'boolean)) value)
  (if value 1 0))

;; so why does ol descide to call this resource type boolean rather
;; than bool? Who knows? -cim

(defmethod convert-resource-out ((parent t) (type (eql 'bool)) value)
  (if value 1 0))

(defmethod convert-resource-out ((parent  t) (type (eql 'string)) value)
  (note-malloced-object
   (excl:ics-target-case
    (:+ics (let ((euc (excl:string-to-euc value)))
	     (ff:euc-to-char* euc)))
    (:-ics (string-to-char* value)))))

(defvar *font-counter* 0)
(defmethod convert-resource-out ((parent t) (type (eql 'font-struct)) value)
  (etypecase value
    (string
     ;;--- Allocate-no-free
     (incf *font-counter*)
     (make-instance 'font
		    :display (widget-display parent)
		    :name value))
    (font value)))

(defvar *color-counter* 0)

(defmethod convert-resource-out ((parent t) (type (eql 'pixel)) value)
  (etypecase value
	     (integer value)
	     ;;--- Allocate-no-free
	     (incf *color-counter*)
	     (color (allocate-color (default-colormap (widget-display parent))
				    value))))

(defmethod convert-resource-out ((parent t) (type (eql 'window)) x)
  x)

(defmethod convert-resource-out ((widget t) (type (eql 'widget)) x)
  x)

;;; Accelerator table stuff

(defvar *empty-table* nil)

(defmethod convert-resource-out (parent (type (eql 'xt::accelerator-table))
				 (value (eql nil)))
  (declare (ignore parent))
  (or *empty-table*
      (setq *empty-table* (xt_parse_accelerator_table ""))))



(defmethod convert-resource-in ((widget t) (type (eql 'widget-list)) x)
  (let ((r nil))
    (dotimes (i (widget-num-children widget))
      (push (convert-resource-in
	     widget 'widget (xt-widget-list x i))
	    r))
    (nreverse r)))

(defmethod convert-resource-in ((parent t) (type (eql 'string)) value)
  (unless (zerop value)
    (excl:ics-target-case
     (:+ics (let ((euc (ff:char*-to-euc value)))
	      (excl:euc-to-string euc)))
     (:-ics (char*-to-string value)))))

(defmethod convert-resource-in ((parent t) (type (eql 'boolean)) value)
  (not (zerop value)))

(defmethod convert-resource-in ((widget t) (type (eql 'widget)) x)
  (intern-widget x :display (widget-display widget)))

(defmethod convert-resource-in ((widget t) (type (eql 'menu-widget)) x)
  (intern-widget x :display (widget-display widget)))

(defmethod convert-resource-in ((widget t) (type (eql 'window)) x)
  (and (not (zerop x))
       (intern-widget x :display (widget-display widget))))

;; Accelerator table stuff
(defmethod convert-resource-in (parent (type (eql 'xt::accelerator-table))
				(value (eql 0)))
  (declare (ignore parent))
  nil)


(defmethod convert-resource-out ((parent t) (type (eql 'proc)) value)
  (etypecase value
    ;;-- Should check to see if its registered
    (integer value)))


(define-enumerated-resource attachment (:none :form :opposite-form
					      :widget :opposite-widget
					      :position :self))



;;;--- Dont ask this is what OLIT sez the label-image of a oblong
;;;--- button is. I know why! its because its a f*cking image (rather
;;;--- than a pixmap

(defmethod convert-resource-out ((parent t) (typep (eql 'pointer)) value)
  value)


;;;

(defmethod convert-resource-out ((parent t) (typep (eql 'key-sym)) value)
  (char-int value))

(defmethod convert-resource-in ((parent t) (typep (eql 'key-sym)) value)
  (cltl1:int-char value))

(defmethod convert-resource-out ((parent t) (typep (eql 'colormap)) value)
  value)

(defmethod convert-resource-in ((parent t) (typep (eql 'colormap)) value)
  value)

;; Openlook

(defmethod convert-resource-out ((parent t) (typep (eql 'char)) value)
  (char-int value))

(defmethod convert-resource-in ((parent t) (typep (eql 'char)) value)
  (cltl1:int-char (ash value -24)))

(defmethod convert-resource-in ((parent t) (typep (eql 'font-struct)) value)
  (make-instance 'font
		 :display (widget-display parent)
		 :foreign-address value))


(define-enumerated-resource processing-direction (:max-on-top
						  :max-on-bottom
						  :max-on-left
						  :max-on-right))


(defmethod convert-resource-out ((parent t) (typep (eql 'ol-edit-mode)) value)
  (ecase value
    (:text-edit 66)
    (:text-read 67)))

(define-enumerated-resource list-size-policy (:variable :constant :resize-if-possible))

(define-enumerated-resource shadow-type  ((:etched-in 5)
					  (:etched-out 6)
					  (:in 7)
					  (:out 8)))

(define-enumerated-resource navigation-type (:none
					     :tab-group
					     :sticky-tab-group
					     :exclusive-tab-group))

(define-enumerated-resource ol-wrap-mode  ((:wrap-off 74)
					   (:wrap-any 75)
					   (:wrap-white-space 76)))


(defmethod convert-resource-out (parent (type (eql 'xt::translation-table)) value)
  (declare (ignore parent))
  ;;--- Allocate-no-free
  (xt_parse_translation_table
   (etypecase value
     (string value)
     (null ""))))

;; solaris 2.2 stuff

(defmethod convert-resource-out ((parent  t) (type (eql 'ol-str)) value)
  (note-malloced-object
   (string-to-char* value)))

(defmethod convert-resource-in ((parent t) (type (eql 'ol-str)) value)
  (unless (zerop value)
    (char*-to-string value)))


(defmethod convert-resource-out ((parent t) (type (eql 'ol-font)) value)
  (etypecase value
    (string
     ;;--- Allocate-no-free
     (incf *font-counter*)
     (make-instance 'font
		    :display (widget-display parent)
		    :name value))
    (font value)))


(defmethod convert-resource-in ((parent t) (typep (eql 'ol-font)) value)
  (make-instance 'font
		 :display (widget-display parent)
		 :foreign-address value))
