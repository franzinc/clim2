;;; -*- Mode: Lisp; Package: silica; -*-
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
;; $Id: acl-widget.lisp,v 1.7.8.8 1998/08/12 21:15:14 layer Exp $

#|****************************************************************************
*                                                                            *
*                                                                            *
*  This file implements the interface to the winwidget library.  It includes *
*  support for push buttons, toggle buttons, radio and check boxes, list     *
*  panes, option panes, sliders and menu bars.                               *
*                                                                            *
*                                                                            *
****************************************************************************|#

(in-package :silica)

(defvar *hbutton-width* 21)
(defvar *hbutton-height* 21)

;;mm: Some new mixins

(defclass acl-gadget-id-mixin ()
    ((gadget-id :initform 0)
     (gadget-id->window-map :initform (make-array '(256)))
     ))

(defmethod note-gadget-activated :after ((client t)
					 (gadget acl-gadget-id-mixin))
  (let (m)
    (when (setq m (sheet-direct-mirror gadget))
      (win:enablewindow m 1))))

(defmethod note-gadget-deactivated :after ((client t)
					   (gadget acl-gadget-id-mixin))
  (let (m)
    (when (setq m (sheet-direct-mirror gadget))
      (win:enablewindow m 0))))



;;;acl-gadget-id-mixin   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list panes

(defclass hlist-pane
	  (
	   ;;mm: to keep track of Windows gadget ids
	   acl-gadget-id-mixin   
           mirrored-sheet-mixin
	   list-pane
	   sheet-permanently-enabled-mixin
	   ; try without this sheet-mute-input-mixin
	   space-requirement-mixin
	   basic-pane)
  ()
  (:default-initargs :background +white+))

(defmethod handle-event ((pane hlist-pane)
			 ;; use window-change-event to workaround bug
			 ;; with bad redirection of pointer-events -
			 ;; see comment in silica/event.lisp (cim 9/17/96)
			 (event window-change-event))
  (let ((mirror (sheet-direct-mirror pane))
	(index 0))
    (when mirror
      (setf index (win:sendmessage mirror win:lb_getcursel 0 0))
      (with-slots (items value mode value-key name-key) pane
	;;mm: 11Jan95 - we need to invoke the callback so that list-pane-view 
	;;              will return a value.
	(ecase mode
	  (:exclusive
	   (setf (gadget-value pane :invoke-callback t)
	     (funcall value-key (elt items index))))
	  (:nonexclusive
	   (let ((new (funcall value-key (elt items index))))
	     (setf (gadget-value pane :invoke-callback t)
	       (if (member new (gadget-value pane))	      
		   (delete new (gadget-value pane))
		 (push new (gadget-value pane)))))))))))

(defmethod (setf gadget-value) :after
	   (value (pane hlist-pane) &key invoke-callback)
  (declare (ignore invoke-callback))
  (with-slots (mode items value-key test) pane
    (let ((hwnd (sheet-direct-mirror pane)))
      (when hwnd
	(if (eq mode :nonexclusive)
	    (let ((i 0))
	      (dolist (item items)
		(win:sendMessage
		 ;; what's the "correct" way of passing
		 ;; both lo and hi parts without
		 ;; combining them with an ash? (cim 9/20/96)
		 hwnd win:LB_SELITEMRANGE
		 (if (member (funcall value-key item)
			     value :test test) 
		     1
		   0) 
		 (+ i (ash i 16)))
		(incf i)))
	  (let ((i (position value items
			     :key value-key :test test)))
	    (when i 
	      (win:sendMessage 
	       hwnd win:LB_SETCURSEL i 0))))

	;; check out compute-list-pane-selected-items
	;; in the unix code to see how to do this 
	;; (cim 9/25/96)
	#+ignore
	(win:sendMessage hwnd win:LB_settopindex value 0)))))

(defmethod handle-event :after ((pane hlist-pane) (event pointer-event))
  (deallocate-event event))


;; this function combines the code for compose-space methods for
;; list-panes and combo-boxes. Note that it is inherently wrong
;; because the text-style used in the calculation is typically not
;; that used for the actual gadget - this needs to be looked at
;; sometime (cim 9/25/96)

(defmethod compute-set-gadget-dimensions ((pane set-gadget-mixin))
  (let ((name-width 0)
	(tsh 0))
    (with-slots (items name-key text-style) pane
      (with-sheet-medium (medium pane)
	(multiple-value-setq (name-width tsh)
	  (text-size medium "Frobnitz" :text-style text-style))
	(dolist (item items)
	  (multiple-value-bind (w h)
	      (text-size medium (funcall name-key item) :text-style text-style)
	    (declare (ignore h))
	    (setq name-width (max name-width w))))))
    (values name-width tsh)))
    
(defmethod compose-space ((pane hlist-pane) &key width height)
  (declare (ignore width height))
  (with-slots (items name-key text-style visible-items
		     initial-space-requirement) pane
    (let ((name-width 0)
	  (name-height 0)
          (tsh 0)
          (iwid (or (space-requirement-width initial-space-requirement) 0))
          (ihgt (or (space-requirement-height initial-space-requirement) 0))
	  )
      (multiple-value-setq (name-width tsh)
	(compute-set-gadget-dimensions pane))
      (setq name-height (* (if visible-items visible-items (max (length items) 1))
                           tsh))
      (if (and (> iwid 0) #||(null items)||#)
          (setq name-width iwid)
          (setq name-width (max name-width iwid)))
      (setq name-height (max name-height ihgt))
      (make-space-requirement
        :width (+ name-width 20)
        :height name-height))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mswin-text-edit

(defclass mswin-text-edit (acl-gadget-id-mixin 
			   mirrored-sheet-mixin 
			   text-field	;-pane
			   sheet-permanently-enabled-mixin
			   space-requirement-mixin
			   basic-pane)
  ((external-label :initarg :external-label)
   (depth :initarg :depth)
   (x-margin :initarg :x-margin)
   (y-margin :initarg :y-margin)
   (default-window-procedure :initform (ct:callocate (:void *)))
   ;; needed for text-editor
   (ncolumns :initarg :ncolumns
	     :accessor gadget-columns)
   (nlines :initarg :nlines
	   :accessor gadget-lines)
   (word-wrap :initarg :word-wrap
	      :accessor gadget-word-wrap))
  (:default-initargs 
      :text-style nil
    :background +white+
    :external-label nil
    :x-margin 2 :y-margin 0
    ;; needed for text-editor
    :ncolumns nil
    :nlines nil
    :editable-p t
    :word-wrap nil))


(defmethod handle-repaint ((pane mswin-text-edit) region)
  (declare (ignore region))
  nil)


(defclass mswin-text-field (mswin-text-edit)
  ()
  (:default-initargs 
      :text-style nil
      :external-label nil
      :x-margin 2 :y-margin 0))


(defmethod initialize-instance :after ((sheet mswin-text-edit) &key background label) 
  (declare (ignore background label))
  nil)

(defmethod compute-gadget-label-size ((pane mswin-text-edit))
  (values 50 25))

(defmethod compose-space ((pane mswin-text-edit) &key width height)
  (declare (ignore width height))
  (with-slots (external-label x-margin y-margin initial-space-requirement
	       ncolumns nlines text-style) pane
    (let* ((p (sheet-parent pane))
	   (ext-label-width 0)
	   (ext-label-height 0)
	   (tswid 20)
	   (tshgt 20)
	   (space-for-scrollbars
	    (if (and (typep p 'silica::scroller-pane) 
		     (silica::scroller-pane-scroll-bar-policy p)) 
		2 0))
	   (iwid (or (space-requirement-width initial-space-requirement) 0))
	   (ihgt (or (space-requirement-height initial-space-requirement) 0)))
      (when (port pane)
	(with-sheet-medium (medium pane)
	  (multiple-value-setq (tswid tshgt)
	    (text-size medium "W" :text-style text-style))))
      (if (numberp ncolumns) 
	  (setq iwid (max iwid (* (+ space-for-scrollbars ncolumns) tswid))))
      (if (numberp nlines) 
	  (setq ihgt (max ihgt (* (+ space-for-scrollbars nlines) tshgt))))
      (when (and external-label (port pane))
	(let ((text-style (slot-value pane 'text-style)))
	  (with-sheet-medium (medium pane)
	    (multiple-value-bind (w h)
		(text-size medium external-label :text-style text-style)
	      (setq ext-label-width (+ w (text-style-width text-style medium))
		    ext-label-height (+ h (floor
					   (text-style-height 
					    text-style medium) 
					   2)))))))
      (multiple-value-bind (width height)
	  (compute-gadget-label-size pane)
	(when (member (acl-clim::get-system-version) 
		      ;;mm: Windows NT seems to look better too this way
		      '(:win31 :winnt))
	  (setq width (floor (* width 4) 3)))
	(let ((w (+ x-margin ext-label-width width x-margin))
	      (h (+ y-margin (max ext-label-height height) y-margin)))
	  (make-space-requirement
	   :width  (max iwid w) :min-width w
	   :height (max ihgt h) :min-height h)))
      )))

(defmethod compose-space ((pane mswin-text-field) &key width height)
  (declare (ignore width height))
  (with-slots (external-label x-margin y-margin initial-space-requirement) pane
    (let* ((ext-label-width 0)
	   (ext-label-height 0)
           (iwid (or (space-requirement-width initial-space-requirement) 0))
	   )
      (when external-label
	(let ((text-style (slot-value pane 'text-style)))
	  (with-sheet-medium (medium pane)
	    (multiple-value-bind (w h)
		(text-size medium external-label :text-style text-style)
	      (setq ext-label-width (+ w (text-style-width text-style medium))
		    ext-label-height (+ h (floor
					    (text-style-height 
					     text-style medium) 
					    2)))))))
      (multiple-value-bind (width height)
          (compute-gadget-label-size pane)
         (when (member (acl-clim::get-system-version) 
		       '(:win31 :winnt))
	    (setq width (floor (* width 4) 3)))
	(let ((w (+ x-margin ext-label-width width x-margin))
              (h (+ y-margin (max ext-label-height height) y-margin)))
          (make-space-requirement
	    :width  (if (numberp iwid) (max iwid w) w) :min-width w
	    :height h :min-height h)))
      )))

(defvar *event* nil)

(defmethod handle-event ((pane mswin-text-edit) (event key-press-event))
  (let ((mirror (sheet-direct-mirror pane)))
    (declare (ignore mirror))
    ;; Give up the focus
    (win:setfocus (win:getactivewindow))))

(defmethod handle-event ((pane mswin-text-edit) (event window-change-event))
  (let ((mirror (sheet-direct-mirror pane)))
    (declare (ignore mirror))
    (setf (gadget-value pane :invoke-callback t) (gadget-value pane))))

(defmethod handle-event ((pane mswin-text-edit) (event focus-out-event))
  (let ((mirror (sheet-direct-mirror pane)))
    (declare (ignore mirror))
    (setf (gadget-value pane :invoke-callback t) (gadget-value pane))))

(defmethod handle-event :after ((pane mswin-text-edit) (event window-change-event))
  (focus-out-callback pane (gadget-client pane) (gadget-id pane)))

(defun xlat-newline-return (str)
  ;; Given a Lisp string, create an equivalent C string.
  ;; Replace Newline with Return&Newline.
  (if (not (find #\Newline str :test #'char=))
      (values str (length str))
    (let* ((subsize (length str))
	   (nnl (let ((nl 0))
		  (dotimes (i subsize)
		    (when (char= (char str i) #\Newline)
		      (incf nl)))
		  nl))
	   (cstr (ct:callocate (:char *) :size (+ 1 nnl subsize)))
	   (pos 0))
      (dotimes (i subsize)
	(when (char= (char str i) #\Newline)
	  (ct:cset (:char 256) cstr ((fixnum pos)) (char-int #\Return))
	  (incf pos))
	(ct:cset (:char 256) cstr ((fixnum pos)) (char-int (char str i)))
	(incf pos))
      ;; terminate with null
      (ct:cset (:char 256) cstr ((fixnum pos)) 0)
      (values cstr pos))))

(defun unxlat-newline-return (str)
  ;; Given a C string, create an equivalent Lisp string.
  ;; Replace Return&Newline with Newline.
  (let* ((subsize (length str))
	 (nnl (let ((nl 0))
		(dotimes (i subsize)
		  (when (char= (char str i) #\Return) (incf nl)) )
		(- nl)))
	 (cstr (make-string (+ nnl subsize)))
	 (pos 0))
    (dotimes (i subsize)
      (unless (char= (char str i) #\Return)
	(setf (char cstr pos) (char str i))
	(incf pos)))
    cstr))

;; added back with mods by pr 1May97 (from whence?) -tjm 23May97
(defmethod (setf gadget-value) :after (str (pane mswin-text-edit) &key invoke-callback)
  (declare (ignore invoke-callback))
  (with-slots (mirror value) pane
    (setq value str)			; Moved outside conditional - smh 26Nov96
    (when mirror
      (win:setWindowText
       mirror (xlat-newline-return str))
      ;; I wonder whether this avoidance of the callback is really correct,
      ;; or whether it was a quick workaround for bad control structure elsewhere.
      ;; It could be causing some of our ds problems, but I'm not changing it
      ;; right now.  Also, should it be moved outside the conditional? -smh 26Nov96
      (when nil				;+++ invoke-callback
	(value-changed-callback pane
				(gadget-client pane) (gadget-id pane) str)))))

(defmethod gadget-value ((pane mswin-text-edit))
  (with-slots (mirror value) pane
    (if mirror				; else clause added - smh 26Nov96
	(let* ((wl (win:SendMessage mirror 
				     win:WM_GETTEXTLENGTH 
				     0 0))
	     (teb (make-string wl))
	     (tlen (win:GetWindowText mirror teb (1+ wl))))
	(declare (ignorable tlen))
	(setf teb (unxlat-newline-return teb)) ;; pr Aug97
        (setf value teb)
        ;; By the way, does anyone know why the second value is returned? -smh
	(values teb (length teb)))
      ;;this used to be (values value (length value)) which I believe is
      ;;right -- K. Reti
      (values value (if (listp value) (length value) 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buttons

(defvar *default-picture-button-op*
    boole-and)

(defclass hpbutton-pane (acl-gadget-id-mixin 
			 mirrored-sheet-mixin 
			 push-button 
			 button-pane-mixin
			 )
  ((external-label :initarg :external-label)
   (depth :initarg :depth)
   ;; These slots hold the patterns we'll really use...
   (normal-pattern :initarg :normal-pattern)
   (depressed-pattern :initarg :depressed-pattern)
   (x-margin :initarg :x-margin)
   (y-margin :initarg :y-margin)
   ;;--- kludge because DRAW-TEXT* :ALIGN-X :CENTER is wrong
   (internal-label-offset :initform 0)
     
   ;; yet another slot for picture buttons - who knows what all the
   ;; above slots are required for - this mix-up with the generic
   ;; gadgets is a real mess - some day this needs to be cleaned up
   ;; (cim 10/4/96)
   (pixmap :initform nil)
   (raster-op :initform *default-picture-button-op* :initarg :raster-op))
  (:default-initargs :label nil
    :text-style nil
    :show-as-default nil
    :external-label nil
    ;; changed default-margins to make
    ;; picture-buttons look better (cim 10/4/96)
    :x-margin 2 :y-margin 2))

(defmethod compose-space ((pane hpbutton-pane) &key width height)
  (declare (ignore width height))
  (with-slots (external-label x-margin y-margin initial-space-requirement) pane
    (let* ((ext-label-width 0)
	   (ext-label-height 0))
      (when external-label
	(let ((text-style (slot-value pane 'text-style)))
	  (with-sheet-medium (medium pane)
	    (multiple-value-bind (w h)
		(text-size medium external-label :text-style text-style)
	      (setq ext-label-width (+ w (text-style-width text-style medium))
		    ext-label-height (+ h (floor
					    (text-style-height 
					     text-style medium) 
					    2)))))))
      (multiple-value-bind (width height)
          (compute-gadget-label-size pane)
         (when (member (acl-clim::get-system-version) 
		       ;;mm: Windows NT seems to look better too this way
		       '(:win31 :winnt))
	    (setq width (floor (* width 4) 3)))
	(let ((w (+ x-margin ext-label-width width x-margin))
              (h (+ y-margin (max ext-label-height height) y-margin)))
	  ;;mm: set minimum dimensions for buttons
          (make-space-requirement
	    :width  w :min-width w
	    :height h :min-height h)))
      )))

(defmethod draw-picture-button ((pane hpbutton-pane) state hdc rect)
  (multiple-value-bind (bwidth bheight)
      (bounding-rectangle-size pane)
    (let* ((pixmap (slot-value pane 'pixmap))
	   (op (slot-value pane 'raster-op))
	   (width (pixmap-width pixmap))
	   (height (pixmap-height pixmap))
	   (x (floor (- bwidth width) 2))
	   (y (floor (- bheight height) 2))
	   (selected (logtest state win:ods_selected)))
      (when selected
	(incf x)
	(incf y))
      (win:DrawEdge hdc
		     rect 
		     (if selected
			 win:BDR_SUNKEN
		       win:BDR_RAISED)
		     (+ win:BF_RECT win:BF_MIDDLE))
      (win:bitblt hdc x y width height (acl-clim::pixmap-cdc pixmap) 0 0
		   (acl-clim::bop->winop op)))))

;; deallocate and pixmap associated with a picture button when it's
;; destroyed - this is the only note-sheet-degrafted method in the
;; aclpc directory - someone should check what other resources 
;; (if any) need to be deallocated when controls are destroyed
;; (cim 10/11/96) 

;; only destroy the pixmap if it was created from a pattern label - if 
;; the the label was given as a pixmap leave it to the user to destroy 
;; (cim 10/14/96)

(defmethod note-sheet-degrafted :after ((pane hpbutton-pane))
  (let ((pixmap (slot-value pane 'pixmap))
	(label (gadget-label pane)))
    (when (and pixmap (typep label 'pattern))
      (port-deallocate-pixmap (port pixmap) pixmap))))

;; This was copied from silica/db-button.lisp and specialized on
;; hpbutton-pane and then the event class changed to
;; window-change-event as with all the other fixes in this file. 
;; silica/db-button.lisp should only be used for using the generic
;; clim gadgets - when using native windows controls this code
;; shouldn't be used - the fact that it was, was introducing strange
;; bugs in the handling of push-button events. (cim 9/17/96)

(defmethod handle-event ((pane hpbutton-pane) 
			 ;; use window-change-event to workaround bug
			 ;; with bad redirection of pointer-events -
			 ;; see comment in silica/event.lisp (cim 9/17/96)
			 (event window-change-event))
  ;; removed the armed test that came from db-button.lisp - not
  ;; applicable for built in gadgets - check for other gadget classes
  ;; (cim 9/17/96) 
  (activate-callback pane (gadget-client pane) (gadget-id pane)))

;; let's shadow generic db-butto methods (cim 9/18/96) - this needs
;; some more work

(defmethod handle-event ((pane hpbutton-pane) 
			 (event pointer-enter-event))
  nil)

(defmethod handle-event ((pane hpbutton-pane) 
			 (event pointer-exit-event))
  nil)


(defmethod (setf gadget-label) :after (str (pane hpbutton-pane))
  (with-slots (mirror) pane
    (when mirror
      (win:setWindowText mirror str))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hbutton-pane

(defclass hbutton-pane (acl-gadget-id-mixin 
			mirrored-sheet-mixin 
			toggle-button 
			button-pane-mixin)
    ((pixmap :initform nil)
     (raster-op :initform *default-picture-button-op* :initarg :raster-op))
  (:default-initargs :label nil
		     :indicator-type ':one-of
		     ;; We no longer want this as it overrides the the
		     ;; system font returned by get-sheet-resources
		     ;; in acl-medi.lisp (cim 10/12/96)
		     :text-style nil
		     ))

(defmethod draw-picture-button ((pane hbutton-pane) state hdc rect)
  (multiple-value-bind (bwidth bheight)
      (bounding-rectangle-size pane)
    (let* ((pixmap (slot-value pane 'pixmap))
	   (op (slot-value pane 'raster-op))
	   (width (pixmap-width pixmap))
	   (height (pixmap-height pixmap))
	   (x (floor (- bwidth width) 2))
	   (y (floor (- bheight height) 2))
	   (selected (logtest state win:ods_selected)))
      (when selected
	(incf x)
	(incf y))
      (win:DrawEdge hdc
		     rect 
		     (if selected
			 win:BDR_SUNKEN
		       win:BDR_RAISED)
		     (+ win:BF_RECT win:BF_MIDDLE))
      (win:bitblt hdc x y width height (acl-clim::pixmap-cdc pixmap) 0 0
		   (acl-clim::bop->winop op)))))

(defmethod compose-space ((pane hbutton-pane) &key width height)
  (declare (ignore width height))
  (multiple-value-bind (width height)
      (compute-gadget-label-size pane)
    ;;--- Should either make radio buttons and check boxes different classes
    ;;--- or generalize this
    (when (member (acl-clim::get-system-version) 
		  ;;mm: looks better in winnt
		  '(:win31 :winnt))
      (setq width (floor (* width 4) 3)))
    (let* ((button-width *hbutton-width*)
	   (button-height *hbutton-height*)
           (w (+ width (* button-width 1)))
           (h (max height (* button-height 1))))
      ;;mm: set min dimensions too
      ;; We allow 1/2 button width on each side as margin
      (make-space-requirement :width w  :min-width w
			      :height h :min-height h))
    ))

;; Highlighting is a no-op
(defmethod highlight-button ((pane hbutton-pane) medium)
  (declare (ignore medium)))

(defmethod handle-event ((pane hbutton-pane) 
			 ;; use window-change-event to workaround bug
			 ;; with bad redirection of pointer-events -
			 ;; see comment in silica/event.lisp (cim 9/17/96)
			 (event window-change-event))
  ;; removed the armed test that came from db-button.lisp - not
  ;; applicable for built in gadgets - check for other gadget classes
  ;; (cim 9/17/96) 
  (setf (gadget-value pane :invoke-callback t) (not (gadget-value pane))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; option panes
;;;
;;; mswin option panes have been replced by mswin-combo-box

;;; clim\db-list
(defclass acl-clim::winwidget-mixin () ())

;;; When an hbutton is set, update its checkmark appropriately.
(defmethod (setf gadget-value) :after (value (pane hbutton-pane) 
				       &key invoke-callback)
  (declare (ignore invoke-callback))
  (with-slots (mirror) pane
    (when mirror
      ;;(break "About to set value of ~a to ~a." pane value)
      (win:sendmessage mirror win:BM_SETCHECK (if value 1 0) 0))))

;;; When items are set in an hlist-pane the  mirror must be
;;; made to update its appearance appropriately.
(defmethod (setf set-gadget-items) :after (items (pane hlist-pane))
  (with-slots (name-key mirror) pane
    (when mirror
      (win:SendMessage mirror win:LB_RESETCONTENT 0 0)
      (dolist (item items)
	(let ((str (acl-clim::nstringify (funcall name-key item)))
	      (pos (position item items)))
	  ;;(break "insert gadget item [~a @ ~a]" str pos)
	  (win:SendMessage mirror win:LB_INSERTSTRING pos str)))
      (win:InvalidateRect mirror ct:hnull win:true)))) ;; make sure it updates

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Combo Box

(defclass mswin-combo-box-pane
	  (
	   ;;mm: to keep track of Windows gadget ids
	   acl-gadget-id-mixin   
           mirrored-sheet-mixin
	   option-pane
	   sheet-permanently-enabled-mixin
	   ; try without this sheet-mute-input-mixin
	   space-requirement-mixin
	   basic-pane)
  ()
  (:default-initargs :background +white+))

(defmethod initialize-instance :after ((sheet mswin-combo-box-pane) &key visible-items) 
  (declare (ignore visible-items))
  nil)

(defmethod initialize-instance :after ((sheet application-pane) &key foreground) 
  (declare (ignore foreground))
  nil)

(defmethod handle-event ((pane mswin-combo-box-pane) (event window-change-event))
  (let ((mirror (sheet-direct-mirror pane))
	(index 0))
    (with-slots (items value mode value-key) pane
      (when (and mirror items)
        (setf index (win:sendmessage mirror win:cb_getcursel 0 0))
        (setf (gadget-value pane :invoke-callback t) (funcall value-key (elt items index)))))))

(defmethod (setf gadget-value) :after
	   (value (pane mswin-combo-box-pane) &key invoke-callback)
  (declare (ignore invoke-callback))
  (with-slots (mode items value-key test) pane
    (let ((hwnd (sheet-direct-mirror pane))
	  (i (position value items
		       :key value-key :test test)))
      (when (and hwnd i)
	(win:sendMessage hwnd win:CB_SETCURSEL i 0)))))

(defmethod handle-event :after ((pane mswin-combo-box-pane) (event pointer-event))
  (deallocate-event event))

(defmethod compose-space ((pane mswin-combo-box-pane) &key width height)
  (declare (ignore width height))
  (with-slots (items name-key text-style visible-items
		     space-requirement) pane
    (let (;;(name "")
	  (name-width 0)
	  (name-height 0)
	  ;;(index 0)
          (tsh 0))
      (multiple-value-setq (name-width tsh)
	(compute-set-gadget-dimensions pane))
      ;; this specifies the regular size, not the dropped-down size
      (setq name-height tsh)
      (make-space-requirement
       :width (+ name-width 20)
       :height (+ name-height 7)))))

(in-package :acl-clim)

;;; This controls the space allocated to a combo-box control.  Space is
;;; allocated on the basis of its closed up size, but the control is sized
;;; to its dropped down size per windows usage.  +++ currently the dropped
;;; down size is set to a constant 250 pixels -- we can do better!

;;; it now makes an attempt to calculate the drop down size - not
;;; quite right but it errs on the large size (cim 9/25/96)

;;; the previous version of set-sheet-mirror-edges* appeared to be a
;;; modified version of the method specialized for top-level-sheet (in
;;; acl-mirr). This seems like the wrong thing to use - in particular
;;; it was breaking scrolling of combo boxes - changed it to be based
;;; on the method for non top-level-sheet (cim 9/25/96) 

(defmethod set-sheet-mirror-edges* ((port acl-port) 
				    (sheet silica::mswin-combo-box-pane)
				    left top right bottom)
  (fix-coordinates left top right bottom)
  (let* ((hwnd (sheet-mirror sheet))
	 (height (* (+ 2 (win:sendMessage hwnd win:CB_GETCOUNT 0 0))
		    ;; I'd have expected the wparam to be 0 here
		    ;; according to the docs but this doesn't work
		    ;; right (cim 9/25/96)
		    (win:sendMessage hwnd win:CB_GETITEMHEIGHT -1 0))))
    (win:setWindowPos hwnd
		       (ct:null-handle win:hwnd) ; we really want win:HWND_TOP
		       left top
		       (- right left)
		       height #| (- bottom top) |#
		       (logior win:swp_noactivate
			       win:swp_nozorder))))

(in-package :silica)

;;; When items are set in an combo-pane the  mirror must be
;;; made to update its appearance appropriately.
(defmethod (setf set-gadget-items) :after (items (pane mswin-combo-box-pane))
  (with-slots (name-key mirror) pane
    (when mirror
      (win:SendMessage mirror win:CB_RESETCONTENT 0 0)
      (dolist (item items)
	(let ((str (acl-clim::nstringify (funcall name-key item)))
	      (pos (position item items)))
	  ;;(break "insert gadget item [~a @ ~a]" str pos)
	  (win:SendMessage mirror win:CB_INSERTSTRING pos str)))
      ;; make sure it updates
      (win:InvalidateRect mirror ct:hnull win:true)
      (note-sheet-region-changed pane))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; widget button Menu bars

;;; Windoz will not give you a menu bar anywhere but the
;;; top level.  Period.  To implement this, we have to
;;; use "non-native" menu bars.

(defclass mswin-menu-bar-pane (menu-bar
			       sheet-permanently-enabled-mixin
			       space-requirement-mixin
			       sheet-single-child-mixin
			       basic-pane)
    ())

(defmethod compose-space ((pane mswin-menu-bar-pane) &key width height)
  (unless width (setq width 150))
  (unless height (setq height 25))
  (make-space-requirement 
   :width (max width 40)
   :min-width 40
   :height (max height 25)
   :min-height 25))

(defmethod allocate-space ((pane mswin-menu-bar-pane) width height)
  (let ((child (sheet-child pane)))
    (when child (move-and-resize-sheet child 0 0 width height))))

(defmethod initialize-instance :after ((object mswin-menu-bar-pane)
				       &rest options
				       &key command-table frame)
  (when (and frame command-table)
    (let ((inferiors (silica::compute-menu-bar-pane-1 frame command-table)))
      (sheet-adopt-child object inferiors))))

(defclass mswin-menu-bar-button (hpbutton-pane)
    ((next-menu :initform nil :initarg :next-menu)))

(defmethod handle-event ((pane mswin-menu-bar-button)
		         (event pointer-enter-event))
  (with-slots (armed next-menu) pane
    (unless armed
      (setf armed t)
      (armed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event ((pane mswin-menu-bar-button)
		         (event pointer-exit-event))
  (with-slots (armed next-menu) pane
    (when armed
      (setf armed nil))))

(defmethod handle-event ((pane mswin-menu-bar-button)
			 (event window-change-event))
  (with-slots (armed next-menu) pane
    (with-sheet-medium (medium pane)
      (declare (ignore medium))
      (when armed (setf armed :active))
      (let ((choice 
	     (menu-choose next-menu :associated-window pane)))
	(when choice
	  (apply #'queue-command pane choice)))
      (setf armed t)
      )))

(defun queue-command (button command command-table)
  (let ((frame (pane-frame button)))
    (distribute-event
      (port button)
      (allocate-event 'presentation-event
        :frame frame
        :sheet (frame-top-level-sheet frame)
        :presentation-type `(command :command-table ,command-table)
        :value command))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scroll-bar

;; Not to be confused with a scroller-pane, which provides scroll bars
;; to an application pane.  This is a naked scroll-bar gadget that
;; acts like a slider gadget.

(defclass mswin-scroll-bar (silica::acl-gadget-id-mixin 
                            mirrored-sheet-mixin 
			    scroll-bar
                            sheet-permanently-enabled-mixin
                            space-requirement-mixin
                            basic-pane)
  ())

(defmethod initialize-instance :after ((object mswin-scroll-bar) &key &allow-other-keys)
   ;; It is a mystery to me how size is 1.0 at this point.
   ;; This is in my opinion a poor default value.
   ;; The slot has NIL as its initform, and (SETF SCROLL-BAR-SIZE) is not called.
  (setf (scroll-bar-size object) (float (/ (gadget-range object) 10))))

(defmethod realize-mirror ((port acl-clim::acl-port) (sheet mswin-scroll-bar))
  (multiple-value-bind (left top right bottom)
      (sheet-native-region* sheet)
    (fix-coordinates left top right bottom)
    (let* ((parent (sheet-mirror sheet))
	   (parent2 (sheet-mirror (sheet-parent sheet)))
           (window nil)
           (orientation (gadget-orientation sheet))
	   (width (- right left))
	   (height (- bottom top))
           (gadget-id (silica::allocate-gadget-id sheet)))
      (assert (eq parent parent2) () "parents don't match!")
      (setq window
	(acl-clim::scrollbar-open parent left top width height orientation))
      (setf (sheet-native-transformation sheet)
	(sheet-native-transformation (sheet-parent sheet)))
      (setf (silica::gadget-id->window sheet gadget-id) window)
      (win:showWindow window win:sw_show)
      (setf (sheet-direct-mirror sheet) window) ; needed only to initialize
      (change-scroll-bar-values sheet)	; initialize
      window)))

(defmethod compose-space ((m mswin-scroll-bar) &key width height)
  (declare (ignore width height))
  (let (x)
    (ecase (gadget-orientation m)
      (:vertical 
       (setq x (silica::win-scroll-thick :y))
       (make-space-requirement :width x
                               :min-height x
                               :height (* 2 x)
                               :max-height +fill+))
      (:horizontal
       (setq x (silica::win-scroll-thick :x))
       (make-space-requirement :height x
                               :min-width x
                               :width (* 2 x)
                               :max-width +fill+)))))

(defmethod change-scroll-bar-values ((sb mswin-scroll-bar) 
				     &key
				     slider-size
				     value
				     line-increment
				     (page-increment slider-size))
  (declare (ignore page-increment line-increment))
  (let ((mirror (sheet-direct-mirror sb))
	(range (gadget-range sb)))
    (when mirror
      (unless slider-size (setq slider-size (scroll-bar-size sb)))
      (setq slider-size (min slider-size range)); sanity check
      (unless value (setq value (gadget-value sb)))
      (multiple-value-bind (min max) (gadget-range* sb)
	(setf value (max min (min max value))); sanity check
	(decf value min)
	(let* ((struct (ct:ccallocate win:scrollinfo))
	       (page
		(floor 
		 (* acl-clim::*win-scroll-grain* 
		    (if (zerop slider-size) 
			slider-size (/ slider-size range)))))
	       (position
		(floor
		 (* acl-clim::*win-scroll-grain* 
		    (if (zerop value) value (/ value range))))))
	  (ct:csets
	   win:scrollinfo struct
	   cbSize (ct:sizeof win:scrollinfo)
	   fMask win:SIF_ALL
	   nMin 0
	   nMax acl-clim::*win-scroll-grain*
	   nPage page
	   nPos position)
	  (win:SetScrollInfo mirror win:SB_CTL struct 1))))))

(defmethod (setf gadget-value) :after
	   (nv (gadget mswin-scroll-bar) &key invoke-callback)
  (declare (ignore invoke-callback))
  (change-scroll-bar-values gadget :value nv))

(defmethod (setf scroll-bar-size) :after (nv (gadget mswin-scroll-bar))
  (change-scroll-bar-values gadget :slider-size nv))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pull-down-menu

(defclass mswin-pull-down-menu-button (hpbutton-pane) 
    ((next-menu :initform nil :initarg :next-menu)))

(defmethod handle-event ((pane mswin-pull-down-menu-button)
			 (event pointer-enter-event))
  
  (when (port pane)		;the menu is sometimes disabled...
    (let* ((pointer (port-pointer (port pane)))
	   (pointer-button-state (pointer-button-state pointer)))
      (unless (= pointer-button-state 0)
	(with-slots (armed) pane
	  (unless (eq armed :active)
	    (with-sheet-medium (medium pane)
	      (setq armed :active)
	      (highlight-button pane medium))
	    (armed-callback pane (gadget-client pane) (gadget-id pane))))))))

(defmethod handle-event ((pane mswin-pull-down-menu-button)
			 (event pointer-exit-event))
  (when (port pane)			;the menu is often disabled...
    (with-slots (armed) pane
      (when armed
        (setq armed nil)
	(disarmed-callback pane (gadget-client pane) (gadget-id pane))))))

(defmethod handle-event ((pane mswin-pull-down-menu-button)
			 (event pointer-motion-event))
  (with-slots (next-menu x-margin normal-pattern) pane
    (when next-menu
      (with-bounding-rectangle* (left top right bottom) (sheet-region pane)
	(declare (ignore top right bottom))
	(let* ((pattern-width (pattern-width normal-pattern))
	       (sensitive-region 16)
	       (x (pointer-event-x event)))
	  (when (and next-menu
		     (> x (- (+ left x-margin pattern-width)
			     sensitive-region)))
	    (if (typep next-menu 'pull-down-menu)
		(choose-from-pull-down-menu next-menu pane :cascade-p t)
		(funcall next-menu pane))))))))

;; We really shouldn't ever get one of these - the button must have been down
;; for us to get here.
(defmethod handle-event ((pane mswin-pull-down-menu-button)
			 (event pointer-button-press-event))
  (with-slots (armed) pane
    (setf armed :active))
  (armed-callback pane (gadget-client pane) (gadget-id pane))
  )

(defmethod handle-event ((pane mswin-pull-down-menu-button)
			 ;; use window-change-event to workaround bug
			 ;; with bad redirection of pointer-events -
			 ;; see comment in silica/event.lisp (cim 9/17/96)
			 (event window-change-event))
  (with-slots (armed) pane
    (when (eq armed :active)
      (setf armed t)
      (activate-callback pane (gadget-client pane) (gadget-id pane))
      ;;--- This modularity is a bit dubious.  Oh well.
      (throw 'exit-pull-down-menu (values)))))

(defun winhandle-equal (x y)
  (cond (nil
         (eql (ct:lhandle-value x)
              (ct:lhandle-value y)))
        (t (equal x y))))

;;; +++ needs work for integration: *generic-gadgets*
;;; clim\db-menu
(defmethod handle-event ((pane pull-down-menu) (event pointer-exit-event))
  ;; Don't punt if we've never entered the menu, or if we are entering
  ;; one of the buttons within the menu
  (when (and acl-clim:*generic-gadgets*
             (pull-down-menu-entered pane)
	     (not (eq (pointer-boundary-event-kind event) :inferior)))
    (throw 'exit-pull-down-menu (values))
    ))

(defvar *subsidiary-pull-down-menu* nil)

;;; +++ needs work for integration: *generic-gadgets*
;;; clim\db-menu
(defun choose-from-pull-down-menu (menu &optional button &key cascade-p)
  (let ((menu-frame (pane-frame menu))
	(event-queue (sheet-event-queue menu))
	(mirror (sheet-mirror menu)))
    (when (and acl-clim::*generic-gadgets* button)
      (with-bounding-rectangle* (bleft btop bright bbottom)
	  (sheet-device-region button)
	(declare (ignore bright))
	(multiple-value-bind (fleft ftop fright fbottom)
	    (let ((tls (get-top-level-sheet button)))
	      (mirror-region* (port tls) tls))
	  (declare (ignore fright fbottom))
	  (if cascade-p
	      (let ((pattern-width (pattern-width (slot-value button 'normal-pattern)))
		    (button-x-margin (slot-value button 'x-margin)))
		(move-sheet (frame-top-level-sheet menu-frame)
			    (+ bleft fleft pattern-width button-x-margin -16)
			    (+ btop ftop)))
	      (move-sheet (frame-top-level-sheet menu-frame)
			  (+ bleft fleft 4)
			  (+ bbottom ftop 23))))))
    (when (and (not acl-clim::*generic-gadgets*) button)
      (let ()
        (multiple-value-bind (bleft btop bright bbottom)
            (acl-clim::mirror-native-edges*
	     acl-clim::*acl-port* button)
	  (declare (ignore bright))
	  (if cascade-p
	      (let ((pattern-width (pattern-width (slot-value button 'normal-pattern)))
		    (button-x-margin (slot-value button 'x-margin)))
		(move-sheet (frame-top-level-sheet menu-frame)
			    (+ bleft pattern-width button-x-margin -16)
			    (+ btop)))
	      (move-sheet (frame-top-level-sheet menu-frame)
			  (+ bleft 0)
			  (+ bbottom 0))))))
    (enable-frame menu-frame)
    ;; Share the event queue with the application frame
    (setf (sheet-event-queue (frame-top-level-sheet (pane-frame menu)))
	  (sheet-event-queue (frame-top-level-sheet *application-frame*)))
    ;; Ensure no surprise exit events
    (setf (pull-down-menu-entered menu) nil)
    ;; Wait for an event and then handle it

    ;; make sure that the pulldown has the focus
    (win:setFocus mirror)
    (setf (acl-clim::acl-port-mirror-with-focus
            acl-clim::*acl-port*) mirror)
    
    (unwind-protect
	(flet ((waiter ()
		 (not (queue-empty-p event-queue))))
	  (declare (dynamic-extent #'waiter))
	  (catch (if *subsidiary-pull-down-menu* 
		     '|Not exit-pull-down-menu|
		     'exit-pull-down-menu)
	    (let ((*subsidiary-pull-down-menu* t))
	      (loop
	        (unless (winhandle-equal mirror
			       (acl-clim::acl-port-mirror-with-focus
			         acl-clim::*acl-port*))
                  #||(format *terminal-io* "mirror=~a focus=~a~%" mirror (acl-clim::acl-port-mirror-with-focus
			         acl-clim::*acl-port*))||#
		  (throw 'exit-pull-down-menu (values)))
		(port-event-wait (port menu) #'waiter 
                            :wait-reason "Pull-Down Menu" :timeout 2)
		(let ((event (queue-get event-queue)))
		  (when event
		    (handle-event (event-sheet event) event)))))))
      (disable-frame menu-frame))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; slider fixes

(setq *default-horizontal-slider-pattern*
	      (make-pattern #2A((0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				(0 0 0 0 0 0 0 0 0 0 0 0 1 1)
				(0 0 1 0 1 0 1 0 1 0 1 0 1 1)
				(0 0 1 1 0 1 1 0 0 1 0 1 1 1)
				(0 0 1 0 1 0 1 0 1 0 1 0 1 1)
				(0 0 1 1 0 1 1 0 0 1 0 1 1 1)
				(0 0 1 0 1 0 1 0 1 0 1 0 1 1)
				(0 0 1 1 0 1 1 0 0 1 0 1 1 1)
				(0 0 1 0 1 0 1 0 1 0 1 0 1 1)
				(0 0 1 1 0 1 1 0 0 1 0 1 1 1)
				(0 0 1 0 1 0 1 0 1 0 1 0 1 1)
				(0 0 1 1 0 1 1 0 0 1 0 1 1 1)
				(0 0 1 0 1 0 1 0 1 0 1 0 1 1)
				(0 0 1 1 0 1 1 0 0 1 0 1 1 1)
				(0 0 1 0 1 0 1 0 1 0 1 0 1 1)
				(0 0 1 1 0 1 1 0 0 1 0 1 1 1)
				(0 0 1 0 1 0 1 0 1 0 1 0 1 1)
				(0 0 1 1 0 1 1 0 0 1 0 1 1 1)
				(0 0 1 0 1 0 1 0 1 0 1 0 1 1)
				(0 1 1 1 1 1 1 1 1 1 1 1 1 1)
				(0 1 1 1 1 1 1 1 1 1 1 1 1 0))
			    (list +background-ink+ +foreground-ink+)))

(setq *slider-rail-ink* (make-gray-color .5))

(setq *default-vertical-slider-pattern*
	      (make-pattern #2a((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
				(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) 
				(0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 
				(0 0 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1) 
				(0 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1) 
				(0 0 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1) 
				(0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 
				(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) 
				(0 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1) 
				(0 0 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1) 
				(0 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1) 
				(0 0 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1) 
				(0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 
				(0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0))
			    (list +background-ink+ +foreground-ink+)))


;;mm: Some new mixins

(defmethod allocate-gadget-id ((x acl-gadget-id-mixin))
   (with-slots (gadget-id gadget-id->window-map) x
      (let ((l (length gadget-id->window-map)))
         (if (< gadget-id l)
            (prog1 gadget-id (incf gadget-id))
            (error "Too many gadgets in pane: ~S" x)))))

(defmethod gadget-id->window ((x acl-gadget-id-mixin) id)
   (with-slots (gadget-id->window-map) x
      (aref gadget-id->window-map id)))

(defmethod (setf gadget-id->window) (window (x acl-gadget-id-mixin) id)
   (with-slots (gadget-id->window-map) x
      (setf (aref gadget-id->window-map id) window)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text editor

(in-package :acl-clim)

(defmethod update-mirror-transformation :around ((port acl-port)
						 (sheet winwidget-mixin))
  (call-next-method)
  (setf (sheet-native-transformation sheet) (sheet-transformation sheet)))
	



;;mm: allow :LABEL initarg

(defclass acl-text-editor-pane (silica::mswin-text-edit)
    ()
    (:default-initargs :ncolumns 30 :nlines 1))

(defmethod initialize-instance :after ((x acl-text-editor-pane) 
                                       &key label &allow-other-keys)
  (declare (ignore label))
   )

;; new code to deal with resources - ie background foreground and
;; text-stlye for various windows controls (cim 10/12/96)

;; There is a question here of when is it safe to delete the brush
;; used for painting the background. We assume in the code below that
;; by the time the next wm_ctlcolorxxx message arrives the previously
;; returned brush can be freed - is this a reasonable assumption? 
;; (cim 10/11/96)

(defvar *background-brush* nil)

(defmethod adjust-gadget-colors (pane hdc)
  (let* ((bg (color->wincolor (pane-background pane)))
	 (fg (color->wincolor (pane-foreground pane)))
	 (new-brush (win:createSolidBrush bg))
	 (old-brush *background-brush*))
    (win:setBkColor hdc bg)
    (win:setTextColor hdc fg)
    (when old-brush
      (win:deleteObject old-brush))
    (setq *background-brush* new-brush)
    new-brush))

(defmethod get-sheet-resources ((port acl-port) sheet)
  (declare (ignore sheet))
  (port-default-resources port))

(defparameter *windows-system-text-style* nil)
(defparameter *gadget-default-resources* nil)

;; specializing the following method on acl-gadget-id-mixin causes
;; the text-style to be specified for those sheets that are mirrored
;; directly by windows controls - as opposed to for CLIM stream panes
;; which should probably fallback to using *default-text-style* if no
;; explicit text-style is given. (cim 10/14/96)

(defmethod get-sheet-resources :around ((port acl-port)
					(sheet t))
  (or *windows-system-text-style*
      (setq *windows-system-text-style* 
	#+ignore
	(make-device-font-text-style *acl-port* win:system_font)
	#-ignore
	(make-text-style :sans-serif :roman :small)
	;; this should get the real system font but I device fonts don't
	;; seem to work as expected - for the moment though the above
	;; looks pretty good (cim 10/12/96) 
	))	
  (or *gadget-default-resources*
      (setq *gadget-default-resources*
	(let ((resources (call-next-method)))
	  `(:text-style ,*windows-system-text-style* ,@resources)))))

(defmethod standardize-text-style ((port basic-port) style 
				   &optional (character-set 
					      *standard-character-set*))
  (standardize-text-style-1 port style character-set 
			    acl-clim::*acl-logical-size-alist*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outlined-pane

(defclass mswin-outlined-pane (silica::acl-gadget-id-mixin 
			       mirrored-sheet-mixin 
			       outlined-pane)
  ()
  (:default-initargs :thickness 2))

(defun outline-open (parent left top width height)
  (let ((winstyle 
	 (logior win:WS_CHILD
		 win:WS_CLIPCHILDREN 
		 win:WS_CLIPSIBLINGS))
	(exstyle win:WS_EX_CLIENTEDGE)
	(window nil))
    (setq window
      (win:createWindowEx exstyle
			  *clim-class*
			  *win-name*
			  winstyle
			  left top width height
			  (or parent 0)
			  0		; menu
			  *hinst*
			  (symbol-name (gensym)) )) 
    (when (zerop window)
      (or (check-last-error "CreateWindowEx")
	  (error "CreateWindowEx: unknown error")))
    window))

(defmethod realize-mirror ((port acl-port) 
			   (sheet mswin-outlined-pane))
  (multiple-value-bind (left top right bottom)
      (sheet-native-region* sheet)
    (fix-coordinates left top right bottom)
    (let* ((parent (sheet-mirror sheet))
           (window nil)
	   (width (- right left))
	   (height (- bottom top))
           (gadget-id (silica::allocate-gadget-id sheet)))
      (setq window
	(outline-open parent left top width height))
      (setf (sheet-native-transformation sheet)
	(sheet-native-transformation (sheet-parent sheet)))
      (setf (silica::gadget-id->window sheet gadget-id) window)
      (win:showWindow window win:sw_show)
      (setf (sheet-direct-mirror sheet) window)
      window)))

(defmethod handle-repaint ((pane mswin-outlined-pane) region)
  (declare (ignore region))
  nil)

(defmethod compose-space ((pane mswin-outlined-pane) &key width height)
  (let ((thickness (slot-value pane 'silica::thickness))
        (child (sheet-child pane)))
    (space-requirement+
      (compose-space child :width width :height height)
      (make-space-requirement
        :width (* 2 thickness)
        :height (* 2 thickness)))))

(defmethod allocate-space ((pane mswin-outlined-pane) width height)
  (let ((thickness (slot-value pane 'silica::thickness)))
    (move-and-resize-sheet
     (sheet-child pane)
     0 0
     (- width (* 2 thickness)) (- height (* 2 thickness)))))
