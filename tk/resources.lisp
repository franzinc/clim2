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
;; $fiHeader: resources.lisp,v 1.12 92/03/30 17:51:49 cer Exp Locker: cer $

(in-package :tk)

(defmacro define-enumerated-resource (type elements)
  `(progn
     (defmethod convert-resource-out ((parent t) (type (eql ',type)) value)
       (or (position value ',elements)
	   (error "cannot convert ~S to type ~S" value ',type)))
     (defmethod convert-resource-in ((parent t) (type (eql ',type)) value)
       (setq value (ash value -24))
       (elt ',elements value))))




(defun set-values (widget &rest values)
  (let ((arglist (make-arglist-for-class (class-of widget) widget values)))
    (set_values widget
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
		   :element-type '(signed-byte 32)
		   :initial-contents (nreverse new-args)))
    (setq keyword (pop args)
	  value (pop args))
    (let ((resource (or (find keyword resources-1 :key #'resource-name)
			(find keyword resources-2 :key #'resource-name))))
      (when resource
	(push (resource-original-name resource) new-args)
	(push (convert-resource-out parent (resource-type resource) value)
	      new-args)))))

(defmethod convert-resource-out :around (parent type value)
  (declare (optimize (speed 3))
	   (ignore parent type value))
  (let ((result (call-next-method)))
    (if (integerp result)
	result
      (ff:foreign-pointer-address result))))

(defmethod convert-resource-out (parent type value)
  (cerror "Try again" "cannot convert-out resource for ~S,~S,~S" parent type value)
  (convert-resource-out parent type value))


(defmethod convert-resource-out ((parent t) (type (eql 'xm-string)) value)
  (string_create_l_to_r (string-to-char* value) (string-to-char* "")))

(defmethod convert-resource-out ((parent t) (type (eql 'orientation)) value)
  (ecase value
    (:vertical 1)
    (:horizontal 2)))



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
    
    (setq arglist (coerce (nreverse arglist) '(vector (signed-byte 32))))
    (get_values widget
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
  (and (not (zerop value))
       (with-ref-par ((string 0))
	 (string_get_l_to_r value xm_string_default_char_set string)
	 (char*-to-string (sys:memref-int (foreign-pointer-address string) 0 0
					  :signed-long)))))


(define-enumerated-resource separator-type (:no-line 
					    :single-line
					    :double-line 
					    :single-dashed-line
					    :double-dashed-line
					    :shadow-etched-in
					    :shadow-etched-out))

      
(defmethod convert-resource-out ((parent t) (type (eql 'menu-widget)) value)
  value)

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


#+ignore
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
  (etypecase value
    (pixmap value)
    (string
     (let* ((display (widget-display parent))
	    (screen (x11:xdefaultscreenofdisplay display))
	    (white (screen_white_pixel screen))
	    (black (screen_black_pixel screen)))
       (get_pixmap screen (string-to-char* value) white black)))))

(defmethod convert-resource-out ((parent t) (type (eql 'boolean)) value)
  (if value 1 0))

(defmethod convert-resource-out ((parent  t) (type (eql 'string)) value)
  (string-to-char* value))

(define-enumerated-resource dialog-style (:modeless
					  :primary-application-modal
					  :full-application-modal
					  :system-modal))

(defmethod convert-resource-out ((parent t) (type (eql 'font-struct)) value)
  (make-instance 'font
    :display (widget-display parent)
    :name value))


(defmethod convert-resource-in ((parent t) (type (eql 'string)) value)
  (unless (zerop value)
    (char*-to-string value)))

(defmethod convert-resource-in ((parent t) (type (eql 'boolean))
				value)
  (not (zerop (ash value -24))))

(defmethod convert-resource-in ((parent t) (type (eql 'cardinal)) value)
  value)

;; We have to do this because we pass a pointer to :unsigned-long but
;; the actual type is :short
(defmethod convert-resource-in ((parent t) (type (eql 'vertical-dimension)) value)
  (ash value -16))

(defmethod convert-resource-in ((parent t) (type (eql 'horizontal-dimension)) value)
  (ash value -16))

(defmethod convert-resource-in ((parent t) (type (eql 'horizontal-position)) value)
  (convert-16bit-resource-in value))

(defmethod convert-resource-in ((parent t) (type (eql 'vertical-position)) value)
  (convert-16bit-resource-in value))

(defmethod convert-resource-out ((parent t) (type (eql 'pixel)) value)
  (etypecase value
	     (integer value)
	     (color (allocate-color (default-colormap (widget-display parent))
				    value))))
			   

(defmethod convert-resource-in ((parent t) (type (eql 'pixel)) value)
  value)

(defmethod convert-resource-out ((parent t) (type (eql 'horizontal-position))  value)
  value)

(defmethod convert-resource-out ((parent t) (type (eql 'vertical-position))  value)
  value)

(define-enumerated-resource resize-policy (:none :grow :any))

(defmethod convert-resource-in ((parent t) (type (eql 'int)) value) value)
(defmethod convert-resource-out ((parent t) (type (eql 'int)) value) value)


(define-enumerated-resource indicator-type (nil :n-of-many :one-of-many))


(defmethod convert-resource-out ((parent t) (type (eql 'shell-vert-dim)) x) x)
(defmethod convert-resource-out ((parent t) (type (eql 'shell-vert-pos)) x) x)
(defmethod convert-resource-out ((parent t) (type (eql 'shell-horiz-dim)) x) x)
(defmethod convert-resource-out ((parent t) (type (eql 'shell-horiz-pos)) x) x)

(defmethod convert-resource-in ((parent t) (type (eql 'shell-vert-dim)) x) x)
(defmethod convert-resource-in ((parent t) (type (eql 'shell-vert-pos)) x) x)
(defmethod convert-resource-in ((parent t) (type (eql 'shell-horiz-dim)) x) x)
(defmethod convert-resource-in ((parent t) (type (eql 'shell-horiz-pos)) x) x)


(define-enumerated-resource delete-response (:destroy :unmap :do-nothing))

(defmethod convert-resource-out ((parent t) (type (eql 'window)) x)
  x)


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
  (intern-widget x :display (widget-display widget)))

(defmethod convert-resource-out ((widget t) (type (eql 'widget)) x)
  x)

;; Could not think of anywhere better!


(defun setup (&optional (hostspec "localhost:0"))
  (setq context (create-application-context))
  (setq display (make-instance 'display 
			       :host hostspec
			       :context context))
  (setq app (app-create-shell :display display :widget-class 'application-shell)))

(defun initialize-motif-toolkit (hostspec)
  (setup hostspec)
  (values context display app))


;;; ol resource

(defmethod convert-resource-out ((parent t) (type (eql 'position)) value)
  value)

(defmethod convert-resource-in ((parent t) (type (eql 'position)) value)
  (convert-16bit-resource-in value))

(defun convert-16bit-resource-in (value)
  (let ((x (ash value -16)))
    ;; 16bit signed value!
    (if (>= x (ash 1 15))
	(- x (ash 1 16))
      x)))

(defmethod convert-resource-out ((parent t) (type (eql 'dimension)) value)
  value)

(defmethod convert-resource-in ((parent t) (type (eql 'dimension)) value)
  (ash value -16))

;; from OpenLook.h

(defparameter *ol-defines* '(
		       (:ignore -1)
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
  (setq value (ash value -16))
  (or (car (find value *ol-defines* :key #'second))
      (call-next-method)))


(defmethod convert-resource-out (parent (type (eql 'ol-define)) value)
  (or (second (find value *ol-defines* :key #'first))
      (call-next-method)))
  

(define-enumerated-resource packing (:no-packing
				     :tight
				     :column
				     :none))


(define-enumerated-resource edit-mode (:multi-line :single-line))
