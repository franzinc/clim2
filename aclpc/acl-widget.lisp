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
;; $Id: acl-widget.lisp,v 1.7.8.17 1999/06/08 16:50:03 layer Exp $

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

;;mm: Some new mixins

(defclass acl-gadget-id-mixin ()
    ((gadget-id :initform 0)
     (gadget-id->window-map :initform (make-array '(256)))
     ))

(defmethod note-gadget-activated :after ((client t)
					 (gadget acl-gadget-id-mixin))
  (let (m)
    (when (setq m (sheet-direct-mirror gadget))
      (win:EnableWindow m 1))))

(defmethod note-gadget-deactivated :after ((client t)
					   (gadget acl-gadget-id-mixin))
  (let (m)
    (when (setq m (sheet-direct-mirror gadget))
      (win:EnableWindow m 0))))



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
      (setf index (acl-clim::frame-send-message (pane-frame pane)
				      mirror win:LB_GETCURSEL 0 0))
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

(defmethod (setf gadget-value) :before
	   (value (pane hlist-pane) &key invoke-callback)
  (declare (ignore invoke-callback))
  (with-slots (mode items value-key test) pane
    (let ((hwnd (sheet-direct-mirror pane)))
      (when hwnd
	(if (eq mode :nonexclusive)
	    (let ((i 0))
	      (dolist (item items)
		(acl-clim::frame-send-message
		 (pane-frame pane)
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
	      (acl-clim::frame-send-message
	       (pane-frame pane)
	       hwnd win:LB_SETCURSEL i 0))))
	))))

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

;;; When items are set in an hlist-pane the  mirror must be
;;; made to update its appearance appropriately.
(defmethod (setf set-gadget-items) :before (items (pane hlist-pane))
  (with-slots (name-key mirror) pane
    (when mirror
      (acl-clim::frame-send-message
       (pane-frame pane)
       mirror win:LB_RESETCONTENT 0 0)
      (dolist (item items)
	(let ((str (acl-clim::nstringify (funcall name-key item)))
	      (pos (position item items)))
	  ;;(break "insert gadget item [~a @ ~a]" str pos)
	  (excl:with-native-string (str str)
	    (acl-clim::frame-send-message
	     (pane-frame pane)
	     mirror win:LB_INSERTSTRING pos str))))
      (win:InvalidateRect mirror ct:hnull win:TRUE)))) ;; make sure it updates

(defmethod acl-clim::command-event :around ((gadget hlist-pane) 
					    port sheet wparam lparam)
  (let ((notifycode (acl-clim::hiword wparam)))
    (when (= notifycode win:LBN_SELCHANGE)
      ;; Selection in list box is about to change.
      (win:SetFocus (sheet-mirror sheet))
      (call-next-method gadget port sheet wparam lparam))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mswin-text-edit

(defclass mswin-text-edit (acl-gadget-id-mixin 
			   mirrored-sheet-mixin 
			   text-field 
			   sheet-permanently-enabled-mixin
			   space-requirement-mixin
			   basic-pane)
  ((external-label :initarg :external-label)
   (depth :initarg :depth)
   (x-margin :initarg :x-margin)
   (y-margin :initarg :y-margin)
   ;; We probably should be inheriting from text-editor,
   ;; not text-field.
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
    :x-margin 2 :y-margin 2
    ;; needed for text-editor
    :ncolumns nil
    :nlines nil
    :editable-p t
    :word-wrap nil))

(defmethod handle-repaint ((pane mswin-text-edit) region)
  (declare (ignore region))
  nil)

;; text-editor is one of the few things that you can specify:
;;    :width '(4 :character)
(defun process-width-specification (sheet width)
  (typecase width
    (cons
     (assert (eq (second width) :character))
     (with-sheet-medium (medium sheet)
       (let* ((nchars (first width))
	      (style (medium-default-text-style medium))
	      (style-width (text-style-width style medium))
	      (margin (slot-value sheet 'x-margin))
	      (border 4))
	 (+ border margin (* style-width nchars) margin border))))
    (string 
     (with-sheet-medium (medium sheet)
       (let ((w 
	      (text-size sheet width
			 :text-style (medium-default-text-style medium)))
	     (border 4)
	     (margin (slot-value sheet 'x-margin)))
	 (+ border margin w margin border))))
    (otherwise width)))

;; text-editor is one of the few things that you can specify:
;;    :height '(4 :line)
(defun process-height-specification (sheet height)
  (typecase height
    (cons
     (assert (eq (second height) :line))
     (with-sheet-medium (medium sheet)
       (let* ((nlines (first height))
	      (style (medium-default-text-style medium))
	      (style-height (text-style-height style medium))
	      (margin (slot-value sheet 'y-margin))
	      (border 2))
	 (+ border margin (* style-height nlines) margin border))))
    (string 
     (with-sheet-medium (medium sheet)
       (multiple-value-bind (w h) 
	   (text-size sheet height
		      :text-style (medium-default-text-style medium))
	 (declare (ignore w))
	 (let ((border 2)
	       (margin (slot-value sheet 'y-margin)))
	   (+ border margin h margin border)))))
    (otherwise height)))

(defmethod compose-space ((pane mswin-text-edit) &key width height)
  (declare (ignore width height))
  ;;; Note that text-editors are scrolled by  being given 
  ;;; a scroller-pane as a parent, but they have their own 
  ;;; "interior" scrollbars (this is different than text-fields.)
  (with-slots (x-margin y-margin initial-space-requirement nlines ncolumns) pane
    (let* ((par (sheet-parent pane))
	   (scroll-mode (and (acl-clim::scroller-pane-p par)
			     (scroller-pane-scroll-bar-policy par))))
      (let ((w 0) 
	    (min-w (process-width-specification pane '(1 :character)))
	    (h 0)
	    (value (gadget-value pane))
	    (min-h (process-height-specification pane '(1 :line))))
	;; WIDTH
	(cond (ncolumns
	       (setq w (process-width-specification pane `(,ncolumns :character))))
	      (initial-space-requirement
	       ;; This is where accepting-values views factors in.
	       (setq w (process-width-specification 
			pane (space-requirement-width initial-space-requirement))))
	      ((stringp value)
	       (setq w (process-width-specification pane value)))
	      (t
	       (setq w (process-width-specification pane `(20 :character)))))
	(when (member scroll-mode '(:horizontal :both t :dynamic))
	  ;; Allow for the vertical scrollbar
	  (let ((wsty (win-scroll-thick :y)))
	    (setq min-w (+ min-w wsty))
	    (setq w (+ w wsty))))
	(setq w (max w min-w))

	;; HEIGHT
	(cond (nlines
	       (setq h (process-height-specification pane `(,nlines :line))))
	      (initial-space-requirement
	       ;; This is where accepting-values views factors in.
	       (setq h (process-height-specification 
			pane (space-requirement-height initial-space-requirement))))
	      ((stringp value)
	       (setq h (process-height-specification pane value)))
	      (t
	       (setq h (process-height-specification pane '(1 :line)))))

	(when (member scroll-mode '(:horizontal :both t :dynamic))
	  (let ((wstx (win-scroll-thick :x)))
	    ;; Allow for the horizontal scrollbar
	    (setq min-h (+ min-h wstx)
		  h (+ h wstx))))
	(setq h (max h min-h))

	(make-space-requirement
	 :width  w :min-width min-w
	 :height h :min-height min-h
	 )))))

(defmethod initialize-instance :after ((sheet mswin-text-edit) 
				       &key background label) 
  (declare (ignore background label))
  nil)

(defmethod handle-event ((pane mswin-text-edit) (event key-press-event))
  (let ((mirror (sheet-direct-mirror pane)))
    (declare (ignore mirror))
    ;; Give up the focus
    (win:SetFocus (win:GetActiveWindow))))

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

(defmethod (setf gadget-value) :before (new (pane mswin-text-edit) 
					&key invoke-callback)
  (declare (ignore invoke-callback))
  (let ((mirror (sheet-direct-mirror pane))
	(topline 0)
	(leftchar 0))
    (with-slots (value) pane
      (setq value new)
      (when mirror
	;; How do I get leftchar?
	(setq topline (acl-clim::frame-send-message 
		       (pane-frame pane)
		       mirror win:EM_GETFIRSTVISIBLELINE 0 0))
	;; Don't redraw till I tell you:
	(acl-clim::frame-send-message
	 (pane-frame pane)
	 mirror win:WM_SETREDRAW 0 0)
	;; Here's the text:
	(excl:with-native-string (s1 (xlat-newline-return new))
	  (win:SetWindowText mirror s1))
	;; Try to preserve the scroll position:
	(acl-clim::frame-send-message
	 (pane-frame pane)
	 mirror win:EM_LINESCROLL leftchar topline)
	;; Redraw now:
	(acl-clim::frame-send-message
	 (pane-frame pane)
	 mirror win:WM_SETREDRAW 1 0)
	))))

(defmethod gadget-value ((pane mswin-text-edit))
  (with-slots (mirror value) pane
    (if mirror	
	(let* ((wl (acl-clim::frame-send-message (pane-frame pane)
				       mirror 
				       win:WM_GETTEXTLENGTH 
				       0 0))
	       (teb
		#+removed (make-string wl)
		(make-array wl :element-type '(unsigned-byte 8)))
	       (tlen (win:GetWindowText mirror teb (1+ wl))))
	  (declare (ignorable tlen))
	  (setf teb (unxlat-newline-return (excl:mb-to-string teb))) 
	  (setf value teb)
	  ;; By the way, does anyone know why 
	  ;; the second value is returned? -smh
	  (values teb (length teb)))
      (values value (if (listp value) (length value) 0)))))

(defmethod acl-clim::command-event :around ((gadget mswin-text-edit) 
					    port sheet wparam lparam)
  (let ((notifycode (acl-clim::hiword wparam)))
    (when (= notifycode win:EN_KILLFOCUS)
      ;; Don't (setf gadget-value) with every keystroke.
      ;; Only when edit control loses keyboard focus.
      (call-next-method gadget port sheet wparam lparam))))

(defmethod text-edit-flags ((sheet mswin-text-edit))
  (logior 
   (if (gadget-editable-p sheet) 0 win:ES_READONLY)
   win:ES_AUTOHSCROLL win:ES_LEFT win:WS_BORDER
   win:ES_MULTILINE win:ES_AUTOVSCROLL))

(defmethod realize-mirror ((port acl-clim::acl-port) (sheet mswin-text-edit))
  (multiple-value-bind (left top right bottom)
      (sheet-native-region* sheet)
    (fix-coordinates left top right bottom)
    (let* ((parent (sheet-mirror sheet))
	   (parent2 (sheet-mirror (sheet-parent sheet)))
	   (window nil)
	   (width (- right left))
	   (height (- bottom top))
	   (gadget-id (allocate-gadget-id sheet)))
      (assert (eq parent parent2) () "parents don't match!")
      (setq window
	(acl-clim::hedit-open parent gadget-id
			      left top width height 
			      :editstyle (text-edit-flags sheet)
			      :value (slot-value sheet 'value)
			      :scroll-mode 
			      (let ((p (sheet-parent sheet)))
				(and (acl-clim::scroller-pane-p p)
				     (scroller-pane-scroll-bar-policy p)))
			      ))
      (setf (sheet-native-transformation sheet)
	(sheet-native-transformation (sheet-parent sheet)))
      (setf (gadget-id->window sheet gadget-id) window)
      (let ((text-style (pane-text-style sheet)))
	(when text-style
	  (let ((font (text-style-mapping port text-style)))
	    (acl-clim::frame-send-message
	     (pane-frame sheet)
	     window win:WM_SETFONT 
			     (acl-clim::acl-font-index font) 0))))
      ;; Don't know how to set the y margins, but they look pretty good anyway.
      (with-slots (x-margin) sheet
	(acl-clim::frame-send-message
	 (pane-frame sheet)
	 window acl-clim::EM_SETMARGINS
			 ;;acl-clim::EC_USEFONTINFO
			 (logior acl-clim::EC_LEFTMARGIN 
				 acl-clim::EC_RIGHTMARGIN)
			 x-margin))
      ;; It's too soon for this.  Need to do this later, 
      ;; after the layout has been processed, but where?
      (win:ShowWindow window win:SW_SHOW)
      window)))

(defclass mswin-text-field (mswin-text-edit)
  ()
  (:default-initargs 
      :text-style nil
      :external-label nil
      :x-margin 2 :y-margin 2))

(defmethod isa-textfield ((object t)) nil)
(defmethod isa-textfield ((object mswin-text-field)) t)

(defmethod text-edit-flags ((sheet mswin-text-field))
  (logior 
   (if (gadget-editable-p sheet) 0 win:ES_READONLY)
   win:ES_AUTOHSCROLL win:ES_LEFT win:WS_BORDER))

(defmethod isa-viewport ((object t)) nil)
(defmethod isa-viewport ((object viewport)) t)

(defmethod compose-space ((pane mswin-text-field) &key width height)
  ;;; Note that text-fields are scrolled by
  ;;; being given a viewport as a parent (this is different
  ;;; than text-editors.)
  ;;;
  ;;; As a result, if the parent is a viewport, and if width/height
  ;;; is specified, we want to use that value (i.e. in order
  ;;; to fill the space provided by the viewport).
  (with-slots (x-margin y-margin initial-space-requirement nlines ncolumns) pane
    (let* ((parent (sheet-parent pane))
	   (parent-viewport-p (isa-viewport parent)))
      (let ((w 0) 
	    (min-w (process-width-specification pane '(1 :character)))
	    (h 0)
	    (value (gadget-value pane))
	    (min-h (process-height-specification pane '(1 :line))))
	;; WIDTH
	(cond (parent-viewport-p
	       (setq w (process-width-specification pane width)))
	      (ncolumns
	       (setq w (process-width-specification pane `(,ncolumns :character))))
	      (initial-space-requirement
	       ;; This is where accepting-values views factors in.
	       (setq w (process-width-specification 
			pane (space-requirement-width initial-space-requirement))))
	      ((stringp value)
	       (setq w (process-width-specification pane value)))
	      (t
	       (setq w (process-width-specification pane `(20 :character)))))
	(setq w (max w min-w))

	;; HEIGHT
	(cond (parent-viewport-p
	       (setq h (process-height-specification pane height)))
	      (nlines
	       (setq h (process-height-specification pane `(,nlines :line))))
	      (initial-space-requirement
	       ;; This is where accepting-values views factors in.
	       (setq h (process-height-specification 
			pane (space-requirement-height initial-space-requirement))))
	      ((stringp value)
	       (setq h (process-height-specification pane value)))
	      (t
	       (setq h (process-height-specification pane '(1 :line))))) 
	(setq h (max h min-h))

	(make-space-requirement
	 :width  w :min-width min-w
	 :height h :min-height min-h
	 )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; push buttons

(defvar *default-picture-button-op*
    boole-and)

(defclass hpbutton-pane (acl-gadget-id-mixin 
			 mirrored-sheet-mixin 
			 push-button 
			 ;; JPM: button-pane-mixin is for push buttons,
			 ;; not toggle buttons, because it applies 
			 ;; ACTIVATE-CALLBACK to the object.  Toggle buttons
			 ;; do not and should not support that function,
			 ;; they use VALUE-CHANGED-CALLBACK instead.
			 ;; I hope we aren't losing some useful behavior
			 ;; by turning this off...
			 ;;button-pane-mixin
			 space-requirement-mixin
			 leaf-pane
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

(defvar *hbutton-width* 21)
(defvar *hbutton-height* 21)

(defmethod realize-mirror ((port acl-clim::acl-port) (sheet hpbutton-pane))
  (multiple-value-bind (left top right bottom)
      (sheet-native-region* sheet)
    (fix-coordinates left top right bottom)
    (let* ((parent (sheet-mirror sheet))
	   (parent2 (sheet-mirror (sheet-parent sheet)))
           (window nil)
           (buttonstyle nil)
	   (value nil)
	   (width (- right left))
	   (height (- bottom top))
	   gadget-id)
      (assert (eq parent parent2) () "parents don't match!")
      (setq gadget-id (silica::allocate-gadget-id sheet))
      (setf buttonstyle
	(if (push-button-show-as-default sheet)
	    win:BS_DEFPUSHBUTTON
	  win:BS_PUSHBUTTON))
      (multiple-value-bind (cwidth cheight)
	  (compute-gadget-label-size sheet)
	(setq top (+ top (* gadget-id 25)))
	(setq left (+ left 50))
	(setq width (+ cwidth (* 2 *hbutton-width*)))
	(setq height (max cheight (* 1 *hbutton-height*))))
      (setq window
	(let ((label (slot-value sheet 'silica::label))
	      (resources (acl-clim::port-default-resources port)))
	  (when (or (acl-clim::isa-pixmap label)
		    (acl-clim::isa-pattern label))
	    (setf (slot-value sheet 'silica::pixmap)
	      (if (acl-clim::isa-pattern label)
		  (with-sheet-medium (medium sheet)
		    (with-output-to-pixmap 
			(stream medium
				:width (pattern-width label)
				:height (pattern-height label))
		      (draw-pattern* stream label 0 0)))
		label))
	    (setq buttonstyle win:BS_OWNERDRAW ;; pnc Aug97 for clim2bug740
		  label nil))
	  (unless (eq (pane-background sheet) 
		      (getf resources :background))
	    (setq buttonstyle win:BS_OWNERDRAW))
	  (unless (eq (pane-foreground sheet) 
		      (getf resources :foreground))
	    (setq buttonstyle win:BS_OWNERDRAW))
	  (acl-clim::hbutton-open parent gadget-id
				  left top width height 
				  :buttonstyle buttonstyle
				  :value value
				  :label label)))
      (setf (sheet-native-transformation sheet)
	(sheet-native-transformation (sheet-parent sheet)))
      (setf (silica::gadget-id->window sheet gadget-id) window)
      (let ((text-style (pane-text-style sheet)))
	(when text-style
	  (let ((font (text-style-mapping port text-style)))
	    (acl-clim::frame-send-message
	     (pane-frame sheet)
	     window win:WM_SETFONT 
			     (acl-clim::acl-font-index font) 0))))
      (when (sheet-enabled-p sheet)
	;; It's too soon for this.  Need to do this later, 
	;; after the layout has been processed, but where?
	(win:ShowWindow window win:SW_SHOW))
      window)))

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

(defvar *background-brush* nil)

(defmethod draw-picture-button ((pane hpbutton-pane) state hdc rect)
  ;; Handle the drawing part of owner-drawn buttons (BS_OWNERDRAW).
  (assert (acl-clim::valid-handle hdc))
  (let ((bg (acl-clim::color->wincolor (pane-background pane)))
	(fg (acl-clim::color->wincolor (pane-foreground pane))))
    (multiple-value-bind (bwidth bheight)
	(bounding-rectangle-size pane)
      (win:SetBkMode hdc win:OPAQUE)
      (win:SetBkColor hdc bg)
      (win:SetTextColor hdc fg)
      (win:SetRop2 hdc win:R2_COPYPEN)
      (when (acl-clim::valid-handle *background-brush*)
	(or (win:DeleteObject *background-brush*) (error "DeleteObject")))
      (setq *background-brush* (win:CreateSolidBrush bg))
      (when (acl-clim::valid-handle *background-brush*)
	(win:SelectObject hdc *background-brush*))
      (win:DrawEdge hdc
		    rect 
		    (if (logtest state win:ODS_SELECTED)
			win:BDR_SUNKEN
		      win:BDR_RAISED)
		    (+ win:BF_RECT win:BF_MIDDLE))
      (win:rectangle hdc 1 1 (- bwidth 2) (- bheight 2))
      (let ((pixmap (slot-value pane 'pixmap))
	    (label (gadget-label pane)))
	(cond (pixmap
	       (let* ((op (slot-value pane 'raster-op))
		      (width (pixmap-width pixmap))
		      (height (pixmap-height pixmap))
		      (x (floor (- bwidth width) 2))
		      (y (floor (- bheight height) 2)))
		 (when (logtest state win:ODS_SELECTED)
		   (incf x)
		   (incf y))
		 (win:BitBlt hdc x y width height (acl-clim::pixmap-cdc pixmap) 0 0
			     (acl-clim::bop->winop op))))
	      (label
	       (acl-clim::adjust-gadget-colors pane hdc)
	       (with-sheet-medium (medium pane)
		 (let* ((port (port medium))
			(text-style (medium-merged-text-style medium))
			(font (text-style-mapping port text-style))
			(index (acl-clim::acl-font-index font)))
		   (when (acl-clim::valid-handle index) (win:selectobject hdc index))
		   (multiple-value-bind (cstr len)
		       (silica::xlat-newline-return label)
		     (multiple-value-bind (width height) 
			 (text-size medium label :text-style text-style)
		       (let ((x (floor (- bwidth width) 2))
			     (y (floor (- bheight height) 2)))
			 (or (win:TextOut hdc x y cstr len) 
			     (acl-clim::check-last-error "TextOut" :action :warn)))
		       ))))))))))

;; deallocate and pixmap associated with a picture button when it's
;; destroyed - this is the only note-sheet-degrafted method in the
;; aclpc directory - someone should check what other resources 
;; (if any) need to be deallocated when controls are destroyed
;; (cim 10/11/96) 

(defmethod note-sheet-degrafted :after ((pane hpbutton-pane))
  ;; only destroy the pixmap if it was created from a pattern label - if 
  ;; the the label was given as a pixmap leave it to the user to destroy 
  ;; (cim 10/14/96)
  (let ((pixmap (slot-value pane 'pixmap))
	(label (gadget-label pane)))
    (when (and pixmap (acl-clim::isa-pattern label))
      (port-deallocate-pixmap (port pixmap) pixmap))))

(defmethod handle-event ((pane hpbutton-pane) 
			 (event window-change-event))
  ;; SPR18779.  This code runs as a result of pushing a button.
  ;; Turn on output recording in case the callback does any
  ;; output.  Output recording is otherwise turned off by 
  ;; clim-internals::invoke-with-input-editing at this point.
  ;; JPM 11/98.
  (with-output-recording-options (*standard-input* :record t)
    (activate-callback pane (gadget-client pane) (gadget-id pane))))

(defmethod handle-event ((pane hpbutton-pane) 
			 (event pointer-enter-event))
  (armed-callback pane (gadget-client pane) (gadget-id pane)))

(defmethod handle-event ((pane hpbutton-pane) 
			 (event pointer-exit-event))
  (disarmed-callback pane (gadget-client pane) (gadget-id pane)))

(defmethod (setf gadget-label) :after (str (pane hpbutton-pane))
  (with-slots (mirror) pane
    (when mirror
      (excl:with-native-string (str str)
	(win:SetWindowText mirror str)))))

(defmethod (setf pane-background) :after (clr (pane hpbutton-pane))
  (declare (ignore CLR))
  (with-slots (mirror) pane
    (when mirror
      ;;; Work-around to force button to refresh.
      (win:SetWindowText mirror (or (gadget-label PANE) "")))))

(defmethod (setf pane-foreground) :after (clr (pane hpbutton-pane))
  (declare (ignore CLR))
  (with-slots (mirror) pane
    (when mirror
      ;;; Work-around to force button to refresh.
      (win:SetWindowText mirror (or (gadget-label PANE) "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hbutton-pane

(defclass hbutton-pane (acl-gadget-id-mixin 
			mirrored-sheet-mixin 
			toggle-button 
			button-pane-mixin)
    ((pixmap :initform nil)
     (raster-op :initform *default-picture-button-op* :initarg :raster-op))
  (:default-initargs :label nil
		     ;; We no longer want this as it overrides the the
		     ;; system font returned by get-sheet-resources
		     ;; in acl-medi.lisp (cim 10/12/96)
		     :text-style nil
		     ))

(defmethod realize-mirror ((port acl-clim::acl-port) (sheet hbutton-pane))
  (multiple-value-bind (left top right bottom)
      (sheet-native-region* sheet)
    (fix-coordinates left top right bottom)
    (let* ((parent (sheet-mirror sheet))
	   (parent2 (sheet-mirror (sheet-parent sheet)))
           (window nil)
           (buttonstyle nil)
	   (value nil)
	   (width (- right left))
	   (height (- bottom top))
	   gadget-id)
      (assert (eq parent parent2) () "parents don't match!")
      (setq gadget-id (silica::allocate-gadget-id sheet))
      (setq value (slot-value sheet 'silica::value))
      (setq buttonstyle
	(ecase (gadget-indicator-type sheet)
	  (:one-of win:BS_RADIOBUTTON)
	  (:some-of win:BS_CHECKBOX)))
      (multiple-value-bind (cwidth cheight)
	  (compute-gadget-label-size sheet)
	(setq top (+ top (* gadget-id 25)))
	(setq left (+ left 50))
	(setq width (+ cwidth (* 2 *hbutton-width*)))
	(setq height (max cheight (* 1 *hbutton-height*))))
      (setq window
	(let ((label (slot-value sheet 'silica::label))
	      (resources (acl-clim::port-default-resources port)))
	  (when (or (acl-clim::isa-pixmap label)
		    (acl-clim::isa-pattern label))
	    (setf (slot-value sheet 'silica::pixmap)
	      (if (acl-clim::isa-pattern label)
		  (with-sheet-medium (medium sheet)
		    (with-output-to-pixmap 
			(stream medium
				:width (pattern-width label)
				:height (pattern-height label))
		      (draw-pattern* stream label 0 0)))
		label))
	    (setq buttonstyle win:BS_OWNERDRAW ;; pnc Aug97 for clim2bug740
		  label nil))
	  (unless (eq (pane-background sheet) 
		      (getf resources :background))
	    (setq buttonstyle win:BS_OWNERDRAW))
	  (unless (eq (pane-foreground sheet) 
		      (getf resources :foreground))
	    (setq buttonstyle win:BS_OWNERDRAW))
	  (acl-clim::hbutton-open parent gadget-id
				  left top width height 
				  :buttonstyle buttonstyle
				  :value value
				  :label label)))
      (setf (sheet-native-transformation sheet)
	(sheet-native-transformation (sheet-parent sheet)))
      (setf (silica::gadget-id->window sheet gadget-id) window)
      (let ((text-style (pane-text-style sheet)))
	(when text-style
	  (let ((font (text-style-mapping port text-style)))
	    (acl-clim::frame-send-message
	     (pane-frame sheet)
	     window win:WM_SETFONT 
			     (acl-clim::acl-font-index font) 0))))
      (when (sheet-enabled-p sheet)
	;; It's too soon for this.  Need to do this later, 
	;; after the layout has been processed, but where?
	(win:ShowWindow window win:SW_SHOW))
      window)))

(defmethod draw-picture-button ((pane hbutton-pane) state hdc rect)
  (multiple-value-bind (bwidth bheight)
      (bounding-rectangle-size pane)
    (win:SetBkMode hdc win:OPAQUE)
    (win:SetBkColor hdc (acl-clim::color->wincolor (pane-background pane)))
    (win:SetTextColor hdc (acl-clim::color->wincolor (pane-foreground pane)))
    (win:SetRop2 hdc win:R2_COPYPEN)
    (let ((brush (win:CreateSolidBrush 
		  (acl-clim::color->wincolor (pane-background pane)))))
      (when (acl-clim::valid-handle brush) (win:SelectObject hdc brush)))
    (win:DrawEdge hdc
		  rect 
		  (if (logtest state win:ODS_SELECTED)
		      win:BDR_SUNKEN
		    win:BDR_RAISED)
		  (+ win:BF_RECT win:BF_MIDDLE))
    (let ((margin 1))
      (win:rectangle hdc margin margin 
		     (- bwidth margin margin) (- bheight margin margin)))
    (let* ((pixmap (slot-value pane 'pixmap)))
      (when pixmap
	(let* ((op (slot-value pane 'raster-op))
	       (width (pixmap-width pixmap))
	       (height (pixmap-height pixmap))
	       (x (floor (- bwidth width) 2))
	       (y (floor (- bheight height) 2)))
	  (when (logtest state win:ODS_SELECTED)
	    (incf x)
	    (incf y))
	  (win:BitBlt hdc x y width height (acl-clim::pixmap-cdc pixmap) 0 0
		      (acl-clim::bop->winop op)))))))

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
  (setf (gadget-value pane :invoke-callback t)
    (or (eq (gadget-indicator-type pane) :one-of)
	(not (gadget-value pane)))))

(defmethod (setf gadget-indicator-type) :before (value (pane hbutton-pane))
  (declare (ignore value))
  (error "Cannot change the indicator-type of a checkbox at this time"))

;;; When an hbutton is set, update its checkmark appropriately.
(defmethod (setf gadget-value) :before (value (pane hbutton-pane) 
					&key invoke-callback)
  (declare (ignore invoke-callback))
  (with-slots (mirror) pane
    (when mirror
      (acl-clim::frame-send-message (pane-frame pane)
			  mirror win:BM_SETCHECK (if value 1 0) 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; option panes
;;;
;;; mswin option panes have been replced by mswin-combo-box

;;; clim\db-list
(defclass acl-clim::winwidget-mixin () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Combo Box

(defclass mswin-combo-box-pane
	  (acl-gadget-id-mixin   
           mirrored-sheet-mixin
	   option-pane
	   sheet-permanently-enabled-mixin
	   space-requirement-mixin
	   basic-pane)
  ()
  (:default-initargs :background +white+))

(defmethod initialize-instance :after ((sheet mswin-combo-box-pane) 
				       &key visible-items) 
  (declare (ignore visible-items))
  nil)

(defmethod initialize-instance :after ((sheet application-pane) 
				       &key foreground) 
  (declare (ignore foreground))
  nil)

(defmethod handle-event ((pane mswin-combo-box-pane) 
			 (event window-change-event))
  (let ((mirror (sheet-direct-mirror pane))
	(index 0))
    (with-slots (items value mode value-key) pane
      (when (and mirror items)
        (setf index (acl-clim::frame-send-message (pane-frame pane)
					mirror win:CB_GETCURSEL 0 0))
        (setf (gadget-value pane :invoke-callback t) 
	  (funcall value-key (elt items index)))))))

(defmethod (setf gadget-value) :before
	   (value (pane mswin-combo-box-pane) &key invoke-callback)
  (declare (ignore invoke-callback))
  (with-slots (mode items value-key test) pane
    (let ((hwnd (sheet-direct-mirror pane))
	  (i (position value items
		       :key value-key :test test)))
      (when (and hwnd i)
	(acl-clim::frame-send-message (pane-frame pane)
			    hwnd win:CB_SETCURSEL i 0)))))

(defmethod handle-event :after ((pane mswin-combo-box-pane) 
				(event pointer-event))
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
       :width (+ name-width 27)
       :height (+ name-height 10)))))

(defmethod acl-clim::command-event ((gadget mswin-combo-box-pane) 
				    port sheet wparam lparam)
  (let ((notifycode (acl-clim::hiword wparam)))
    (when (= notifycode win:CBN_CLOSEUP)
      ;; List box of a combo box has been closed.
      (call-next-method gadget port sheet wparam lparam))))

(defvar *combo-box-maximum-height* 300)

;;; This next method controls the space allocated to a combo-box control.  
;;; Height is the dropped-down height, i.e. the number of list
;;; items multiplied by the line height.  We set an upper limit
;;; on height in case the height exceeds the height of the display.
;;; If there are more items than will fit within the selected
;;; height, a scroll bar should automatically appear.
(defmethod set-sheet-mirror-edges* ((port acl-clim::acl-port) 
				    (sheet mswin-combo-box-pane)
				    left top right bottom)
  (fix-coordinates left top right bottom)
  (let* ((hwnd (sheet-mirror sheet))
	 (height (min
		  (* (+ 2 (acl-clim::frame-send-message (pane-frame sheet)
					      hwnd win:CB_GETCOUNT 0 0))
		     ;; I'd have expected the wparam to be 0 here
		     ;; according to the docs but this doesn't work
		     ;; right (cim 9/25/96)
		     (acl-clim::frame-send-message (pane-frame sheet)
					 hwnd win:CB_GETITEMHEIGHT -1 0))
		  *combo-box-maximum-height*)))
    (win:SetWindowPos hwnd
		      (ct:null-handle win:HWND) ; we really want win:HWND_TOP
		      left top
		      (- right left)
		      height 
		      (logior win:SWP_NOACTIVATE
			      win:SWP_NOZORDER))))

;;; When items are set in an combo-pane the  mirror must be
;;; made to update its appearance appropriately.
(defmethod (setf set-gadget-items) :after (items (pane mswin-combo-box-pane))
  (with-slots (name-key mirror) pane
    (when mirror
      (acl-clim::frame-send-message
       (pane-frame pane)
       mirror win:CB_RESETCONTENT 0 0)
      (dolist (item items)
	(let ((str (acl-clim::nstringify (funcall name-key item)))
	      (pos (position item items)))
	  ;;(break "insert gadget item [~a @ ~a]" str pos)
	  (excl:with-native-string (str str)
	    (acl-clim::frame-send-message
	     (pane-frame pane)
	     mirror win:CB_INSERTSTRING pos str))))
      ;; make sure it updates
      (win:InvalidateRect mirror ct:hnull win:TRUE)
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
    (let ((inferiors (compute-menu-bar-pane-1 frame command-table)))
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

(defclass mswin-scroll-bar (acl-gadget-id-mixin 
                            mirrored-sheet-mixin 
			    scroll-bar
                            space-requirement-mixin
                            leaf-pane 
			    ;;sheet-permanently-enabled-mixin basic-pane
			    )
  ())

(defmethod initialize-instance :after ((object mswin-scroll-bar) 
				       &key &allow-other-keys)
   ;; It is a mystery to me how size is 1.0 at this point.
   ;; This is in my opinion a poor default value.
  ;; The slot has NIL as its initform, 
  ;; and (SETF SCROLL-BAR-SIZE) is not called.
  (assert (not (null (gadget-min-value object))))
  (assert (not (null (gadget-max-value object))))
  (unless (gadget-value object) 
    (setf (slot-value object 'silica::value) (gadget-min-value object)))
  (assert (not (null (gadget-value object))))
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
           (gadget-id (allocate-gadget-id sheet)))
      (assert (eq parent parent2) () "parents don't match!")
      (setq window
	(acl-clim::scrollbar-open parent left top width height orientation))
      (setf (sheet-native-transformation sheet)
	(sheet-native-transformation (sheet-parent sheet)))
      (setf (gadget-id->window sheet gadget-id) window)
      (win:ShowWindow window win:SW_SHOW)
      (setf (sheet-direct-mirror sheet) window) ; needed only to initialize
      (change-scroll-bar-values sheet)	; initialize
      window)))

(defmethod compose-space ((m mswin-scroll-bar) &key width height)
  (declare (ignore width height))
  (let (x)
    (ecase (gadget-orientation m)
      (:vertical 
       (setq x (win-scroll-thick :y))
       (make-space-requirement :width x
                               :min-height x
                               :height (* 2 x)
                               :max-height +fill+))
      (:horizontal
       (setq x (win-scroll-thick :x))
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
	(let* ((struct (ct:ccallocate win:SCROLLINFO))
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

(defmethod (setf gadget-value) :before
	   (nv (gadget mswin-scroll-bar) &key invoke-callback)
  (declare (ignore invoke-callback))
  (change-scroll-bar-values gadget :value nv))

(defmethod (setf scroll-bar-size) :before (nv (gadget mswin-scroll-bar))
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

(defmethod isa-pull-down-menu ((object t)) nil)
(defmethod isa-pull-down-menu ((object pull-down-menu)) t)

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
	    (if (isa-pull-down-menu next-menu)
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

(defvar acl-clim::*generic-gadgets* nil)

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
    (win:SetFocus mirror)
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

(defmethod adjust-gadget-colors (pane hdc)
  (when silica::*background-brush*
    (or (win:DeleteObject silica::*background-brush*) (error "DeleteObject")))
  (let* ((bg (color->wincolor (pane-background pane)))
	 (fg (color->wincolor (pane-foreground pane))))
    (win:SetBkColor hdc bg)
    (win:SetTextColor hdc fg)
    (setq silica::*background-brush* (win:CreateSolidBrush bg))))

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
	(make-device-font-text-style *acl-port* win:SYSTEM_FONT)
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
      (excl:with-native-string (*clim-class* *clim-class*)
	(excl:with-native-string (*win-name* *win-name*)
	  (win:CreateWindowEx exstyle
			      *clim-class*
			      *win-name*
			      winstyle
			      left top width height
			      (or parent 0)
			      0		; menu
			      *hinst*
			      (symbol-name (gensym)) )))) 
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
      (win:ShowWindow window win:SW_SHOW)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Labelled-pane

(defmethod (setf gadget-label) :after 
	   ((new-label string) (gadget labelled-gadget-mixin))
  (handle-repaint gadget T)
  )
