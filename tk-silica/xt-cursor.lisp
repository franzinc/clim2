;; -*- mode: common-lisp; package: xm-silica -*-
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
;; $fiHeader: xm-cursor.lisp,v 1.1 92/02/25 16:52:41 cer Exp Locker: cer $

(in-package :xm-silica)

(defmethod port-note-cursor-change ((port xt-port) cursor stream type
				    old new)
  (declare (ignore new old type))
  (let ((active (clim-internals::cursor-active cursor))
	(state (clim-internals::cursor-state cursor))
	(focus (clim-internals::cursor-focus cursor)))
    (let* ((mirror (sheet-mirror stream))
	   (blinker (ensure-blinker-for-cursor stream mirror cursor)))
      (when blinker
	(let ((transformation (sheet-native-transformation stream)))
	  (multiple-value-bind (x y) (bounding-rectangle* cursor)
	    (convert-to-device-coordinates transformation x y)
	    (cond ((and active state focus)
		   (tk::set-values blinker :x x :y y)
		   ;;--- This should indicate that we do not have focus
		   (tk::manage-child blinker))
		  ((and active state)
		   (tk::set-values blinker :x x :y y)
		   ;;--- This should indicate that we do have focus
		   (tk::manage-child blinker))
		  (t 
		   (tk::unmanage-child blinker)))))))))

(defmethod ensure-blinker-for-cursor (sheet mirror cursor)
  ;; Make sure that is the widget for the cursor
  ;;----- This is going to fail in all kinds of ways
  ;;----- For example if the window is ungrafted and then regrafted
  ;;----- the cursor needs to follow
  (with-slots (clim-internals::plist) cursor
    (or (getf clim-internals::plist 'gadget)
	(setf (getf clim-internals::plist 'gadget)
	  (let ((gadget 
		 (make-instance 'tk::xm-drawing-area
				:parent mirror
				:width 2
				:height 11
				:managed t)))
	    (xt::realize-widget gadget)
	    (let ((window (tk::widget-window gadget)))
	      (setf (tk::drawable-save-under window) t
		    (xt::drawable-backing-store window) t))
	    ;;--- This really should be done somewhere else but where??????
	    (let ((window (tk::widget-window (sheet-mirror sheet))))
		(setf (tk::drawable-save-under window) t
		      (xt::drawable-backing-store window) t))
	    gadget)))))
