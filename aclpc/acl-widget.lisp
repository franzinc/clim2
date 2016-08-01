;;; -*- Mode: Lisp; Package: silica; -*-
;; See the file LICENSE for the full license governing this code.
;;

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

(defmethod port-move-focus-to-gadget ((port acl-clim::acl-port)
				      (gadget acl-gadget-id-mixin))
  (win:SetFocus (sheet-direct-mirror gadget)))

;;;acl-gadget-id-mixin   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list panes

(defclass hlist-pane
	  (acl-gadget-id-mixin   
           mirrored-sheet-mixin
	   list-pane
	   sheet-permanently-enabled-mixin
	   space-requirement-mixin
	   basic-pane)
  ((x-margin :initform 0)
   (y-margin :initform 0))
  (:default-initargs :background +white+))

(defmethod handle-event ((pane hlist-pane)
			 (event window-change-event))
  ;; Handle WM_COMMAND message.
  (let ((mirror (sheet-direct-mirror pane))
	(index 0))
    (when mirror
      (setf index (acl-clim::frame-send-message (pane-frame pane)
				      mirror win:LB_GETCURSEL 0 0))
      (with-slots (items value mode value-key name-key) pane
	(ecase mode
	  (:exclusive
	   (setf (gadget-value pane :invoke-callback t)
	     (funcall value-key (elt items index))))
	  (:nonexclusive
	   (let ((new (funcall value-key (elt items index))))
	     (setf (gadget-value pane :invoke-callback t)
	       (if (member new (gadget-value pane))	      
		   (remove new (gadget-value pane))
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
	    (acl-clim::frame-send-message
	     (pane-frame pane)
	     hwnd win:LB_SETCURSEL (or i -1) 0)))
	))))

(defmethod handle-event :after ((pane hlist-pane) (event pointer-event))
  (deallocate-event event))

(defvar *default-list-pane-width* '(8 :character))
(defvar *min-list-pane-height* '(1 :line))

(defmethod compute-set-gadget-dimensions ((pane set-gadget-mixin))
  (let ((name-width 0)
	(tsh 0))
    (with-slots (items name-key) pane
       (dolist (item items)
         (let ((name (funcall name-key item)))
           (setq name-width (max name-width
                                 (process-width-specification pane name)))
           (setq tsh (max tsh (process-height-specification pane name)))))
       (when (and (zerop tsh) (zerop name-width))
         (setq name-width (process-width-specification pane *default-list-pane-width*)
               tsh (process-height-specification pane *min-list-pane-height*))))
    (values name-width tsh)))

(defmethod compose-space ((pane hlist-pane) &key width height)
  (declare (ignore width height))
  ;; Note that hlists (like text-editors, whose compose-space method
  ;; served as a template for this) are scrolled by being given a
  ;; scroller-pane as a parent, but they have their own "interior"
  ;; scrollbars.
  (with-slots (initial-space-requirement items visible-items) pane
     (let* ((par (sheet-parent pane))
            (scroll-mode (and (acl-clim::scroller-pane-p par)
                              (scroller-pane-scroll-bar-policy par)))
            (name-width (compute-set-gadget-dimensions pane)))
       (let ((w 0) 
             (min-w (process-width-specification pane '(8 :character)))
             (h 0)
             (min-h (process-height-specification pane *min-list-pane-height*)))
         ;; WIDTH
         (cond ((and initial-space-requirement
                     (plusp (process-width-specification
                             pane (space-requirement-width initial-space-requirement))))
                ;; This is where accepting-values views factors in.
                (setq w (max (process-width-specification 
                              pane (space-requirement-width initial-space-requirement))
                             min-w)))
               (items
                (setq w name-width))
               (t
                (setq w (process-width-specification
                         pane *default-list-pane-width*))))
         (when (member scroll-mode '(:vertical :both t :dynamic))
           ;; Allow for the vertical scrollbar
           (let ((wsty (win-scroll-thick :y)))
             (setq min-w (+ min-w wsty))
             (setq w (+ w wsty))))
         (setq w (max w min-w))

         ;; HEIGHT
         (cond ((and initial-space-requirement
                     (plusp (process-height-specification
                             pane (space-requirement-height initial-space-requirement))))
                ;; This is where accepting-values views factors in.
                (setq h (max (process-height-specification 
                              pane (space-requirement-height initial-space-requirement))
                             (if visible-items
                                 (process-height-specification pane `(,visible-items :line))
                                 0)
                             min-h)))
               (visible-items
                (setq h (process-height-specification pane `(,visible-items :line))))
               (items
                (setq h (max (process-height-specification pane `(,(length items) :line))
                             min-h)))
               (t
                (setq h min-h)))

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

(defmethod (setf set-gadget-items) :before (items (pane hlist-pane))
  ;; When items are set in an hlist-pane the  mirror must be
  ;; made to update its appearance appropriately.
  (with-slots (name-key mirror) pane
    (when mirror
      (acl-clim::frame-send-message
       (pane-frame pane)
       mirror win:LB_RESETCONTENT 0 0)
      (dolist (item items)
	(let ((str (funcall name-key item))
	      (pos (position item items)))
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

(defun process-width-specification (sheet width)
  ;; text-editor is one of the few things that you can specify:
  ;;    :width '(4 :character)
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

(defun process-height-specification (sheet height)
  ;; text-editor is one of the few things that you can specify:
  ;;    :height '(4 :line)
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

;; Kludge.  Sorry.  This is a workaround for the fact that
;; the min height in a +text-field-view+ is zero.  At some
;; point, lets try modifying +text-field-view+ and 
;; +text-editor-view+ to supply better default sizes than zero.
;;
;; 2007-09-03/afuchs: This used to be *min-text-field-width. The
;; minimum width was computed twice. So far, this didn't work too well
;; for text boxes in accepting-values panes that should be smaller
;; than 75 pixels. min-w seems to work better, so let's use that
;; instead, and use 75 as a default width.
(defvar *default-text-field-width* 75)
(defvar *min-text-field-height* '(1 :line))

(defmethod compose-space ((pane mswin-text-edit) &key width height)
  (declare (ignore width height))
  ;; Note that text-editors are scrolled by  being given 
  ;; a scroller-pane as a parent, but they have their own 
  ;; "interior" scrollbars (this is different than text-fields.)
  (with-slots (x-margin y-margin initial-space-requirement nlines ncolumns) pane
    (let* ((par (sheet-parent pane))
	   (scroll-mode (and (acl-clim::scroller-pane-p par)
			     (scroller-pane-scroll-bar-policy par))))
      (let ((w 0) 
	    (min-w (process-width-specification pane '(1 :character)))
	    (h 0)
	    (value (gadget-value pane))
	    (min-h (process-height-specification pane *min-text-field-height*)))
	;; WIDTH
	(cond ((and initial-space-requirement
		    (plusp (process-width-specification
			    pane (space-requirement-width initial-space-requirement))))
	       ;; This is where accepting-values views factors in.
	       (setq w (max (process-width-specification 
			     pane (space-requirement-width initial-space-requirement))
			    min-w)))
	      (ncolumns
	       (setq w (process-width-specification pane `(,ncolumns :character))))
	      ((stringp value)
	       (setq w (max (process-width-specification pane value)
			    *default-text-field-width*)))
	      (t
	       (setq w *default-text-field-width*)))
	(when (member scroll-mode '(:horizontal :both t :dynamic))
	  ;; Allow for the vertical scrollbar
	  (let ((wsty (win-scroll-thick :y)))
	    (setq min-w (+ min-w wsty))
	    (setq w (+ w wsty))))
	(setq w (max w min-w))

	;; HEIGHT
	(cond ((and initial-space-requirement
		    (plusp (process-height-specification
			    pane (space-requirement-height initial-space-requirement))))
	       ;; This is where accepting-values views factors in.
	       (setq h (max (process-height-specification 
			     pane (space-requirement-height initial-space-requirement))
			    (if nlines 
				(process-height-specification pane `(,nlines :line))
			      0)
			    min-h)))
	      (nlines
	       (setq h (process-height-specification pane `(,nlines :line))))
	      ((stringp value)
	       (setq h (max (process-height-specification pane value)
			    min-h)))
	      (t
	       (setq h min-h)))

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

(defmethod handle-event ((pane mswin-text-edit) 
			 (event window-change-event))
  ;; Handle WM_COMMAND event
  ;; This event occurs when the control receives keystrokes that can
  ;; be entered into the buffer.
  (let ((old (slot-value pane 'value))
	(new (gadget-value pane)))
    (unless (equal old new)		
      (setf (gadget-value pane :invoke-callback t) new))))

(defmethod handle-event ((pane mswin-text-edit) (event key-press-event))
  ;; This event occurs when a text-field recevies a RETURN character.
  (activate-callback pane (gadget-client pane) (gadget-id pane))
  (let ((mirror (sheet-direct-mirror pane)))
    (declare (ignore mirror))
    ;; Give up the focus
    (win:SetFocus (win:GetActiveWindow))))

(defun xlat-newline-return (str &optional (convert-to-foreign nil))
  ;; Given a Lisp string, create an equivalent C string.
  ;; Replace Newline with Return&Newline.
  (let ((nl 0)
	(subsize (length str))
	(string nil)
	(cstr nil)
	(pos 0))
    (dotimes (i subsize)
      (when (char= (char str i) #\Newline)
	(incf nl)))
    (cond (convert-to-foreign
	   ;; This doesn't do the right thing for international lisp.
	   ;; Use excl:with-native-string for that, and phase this out.
	   ;; Unfortunately, excl:with-native-string ignores the 
	   ;; issue of Newline.
	   (setq cstr (ct:callocate (:char *) :size (+ 1 nl subsize)))
	   (dotimes (i subsize)
	     (when (char= (char str i) #\Newline)
	       (ct:cset (:char 256) cstr ((fixnum pos)) (char-int #\Return))
	       (incf pos))
	     (ct:cset (:char 256) cstr ((fixnum pos)) (char-int (char str i)))
	     (incf pos))
	   ;; terminate with null
	   (ct:cset (:char 256) cstr ((fixnum pos)) 0)
	   (values cstr pos))
	  ((zerop nl)
	   (values str subsize))
	  (t
	   (setq string (make-string (+ nl subsize)))
	   (dotimes (i subsize)
	     (when (char= (char str i) #\newline)
	       (setf (char string pos) #\return)
	       (incf pos))
	     (setf (char string pos) (char str i))
	     (incf pos))
	   (values string pos)))))

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

;;; Set the selection in a mswin-text-edit gadget.
;;; For endpos = -1 means the end;
;;; as a result, startpos = 0 and endpos = -1 means "select all".
(defmethod set-selection ((pane mswin-text-edit) startpos endpos)
  (let ((mirror (sheet-direct-mirror pane)))
    (when mirror
      (let* ((val (slot-value pane 'value))
           (val-len (if val
                          (+ (length val)
                             (count #\newline val))
                          0)))
                          ;; For endpos = -1, means the end.
                          (when (= endpos -1)
                            (setq endpos val-len))
                            ;; Ensure the correct order.
                            (when (< endpos startpos)
                              (let ((temp endpos))
                                  (setf endpos startpos
                                          startpos temp)))
                                          ;; Otherwise, trim to fit.
        (when (< val-len endpos)
          (setq endpos val-len))
        (when (< val-len startpos)
          (setq startpos val-len))
          (when (< startpos 0)
            (setq startpos 0))
            (acl-clim::frame-send-message (pane-frame pane)
                                          mirror
                                          win:EM_SETSEL
                                          startpos endpos)))))

;;; Retreive the start and end position of the
;;; selection in a mswin-text-edit gadget.
(defmethod get-selection ((pane mswin-text-edit)) 
  (declare (values startpos endpos))
  (let ((mirror (sheet-direct-mirror pane)))
    (cond (mirror
           (let ((startptr
                  (make-array 1  :element-type 'acl-clim::unsigned-nat
                              :initial-element 0))
                 (endptr (make-array 1 :element-type 'acl-clim::unsigned-nat
                                     :initial-element 0))) 
             (acl-clim::frame-send-message (pane-frame pane)
                                           mirror
                                           win:EM_GETSEL
                                           startptr endptr)
             (values (aref startptr 0)
                     (aref endptr 0))))
          (t
           (values 0 0)))))

;;; Set the postion of the caret in the mswin-text-edit gadget.
;;; NOTE:  This uses set-selection, which in turn uses the
;;; mechanism for setting the selected-text (with startpos and 
;;; endpos the same).  This is how the caret is set on MS 
;;; text-gadgets.
(defmethod set-caret-pos ((pane mswin-text-edit) pos)
  (set-selection pane pos pos))


(defmethod (setf gadget-value) :before (new (pane mswin-text-edit) 
					&key invoke-callback)
  (declare (ignore invoke-callback))
  (let ((mirror (sheet-direct-mirror pane))
        (leftchar 0)
	(topline 0))
    (unless (equal (gadget-value pane) new)
      (setf (slot-value pane 'value) new)
      (when mirror
	(setq topline (acl-clim::frame-send-message 
		       (pane-frame pane)
		       mirror win:EM_GETFIRSTVISIBLELINE 0 0))
	;; Disable redraw to avoid flicker.
	(acl-clim::frame-send-message
	 (pane-frame pane) mirror win:WM_SETREDRAW 0 0)

        ;; Set the new-value.
        (excl:with-native-string (s1 new)
          (win:SetWindowText mirror s1))

        ;; Removed some code here.
        ;; spr30683 (alemmens, 2005-11-30)

	;; Try to preserve the scroll position:
	(acl-clim::frame-send-message
	 (pane-frame pane) mirror win:EM_LINESCROLL leftchar topline)
	;; Enable redraw
	(acl-clim::frame-send-message
	 (pane-frame pane) mirror win:WM_SETREDRAW 1 0)
	;; Force redraw
	(win:InvalidateRect mirror 0 win:TRUE)

	(acl-clim::frame-update-window (pane-frame pane) mirror)))))
 

(defmethod gadget-value ((pane mswin-text-edit))
  (with-slots (mirror value) pane
    (if mirror	
	(let* ((length (acl-clim::frame-send-message (pane-frame pane)
                                                     mirror 
                                                     win:WM_GETTEXTLENGTH
                                                     0 0))
	       (buffer (make-array length :element-type '(unsigned-byte 8))))
          (win:GetWindowText mirror buffer (1+ length))
	  ;; Don't call unxlat-newline-return.
          ;; spr30683 (alemmens, 2005-11-30)
	  (let ((result (excl:mb-to-string buffer)))
            ;; By the way, does anyone know why 
            ;; the second value is returned? -smh
            (values result (length result))))
      (values value (if (stringp value) (length value) 0)))))



(defmethod gadget-current-selection ((pane mswin-text-edit))
  (let ((mirror (sheet-direct-mirror pane)))
    (when mirror
      (let* ((wl (acl-clim::frame-send-message (pane-frame pane)
                                               mirror 
                                               win:WM_GETTEXTLENGTH 
                                               0 0))
             (teb (make-array wl :element-type '(unsigned-byte 8)))
             (tlen (win:GetWindowText mirror teb (1+ wl)))
             (startptr (make-array 1 :element-type 'acl-clim::unsigned-nat
                                   :initial-element 0))
             (endptr (make-array 1 :element-type 'acl-clim::unsigned-nat
                                 :initial-element 0)))
        (declare (ignorable tlen))
        (acl-clim::frame-send-message (pane-frame pane)
                                      mirror
                                      win:EM_GETSEL
                                      startptr endptr)
        (unxlat-newline-return 
         (excl:mb-to-string teb 
                            :start (aref startptr 0) 
                            :end (aref endptr 0)))))))

(defmethod text-edit-flags ((sheet mswin-text-edit))
  (logior 
   (if (gadget-editable-p sheet) 0 win:ES_READONLY)
   (if (gadget-word-wrap sheet) 0  win:ES_AUTOHSCROLL)
   win:ES_LEFT win:WS_BORDER
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

      ;; bug12693 SPR26749
      ;; If Echo-character is specified, set it explicitly
      ;; See also (text-edit-flags mswin-text-field)
      (when (typep sheet 'mswin-text-field)
	(let ((echo-char (text-field-echo-character sheet)))
	  (when echo-char
	    (if (not (characterp echo-char))
		(setq echo-char #\*))
	    (let ((code (char-code echo-char)))
	      (acl-clim::frame-send-message (pane-frame sheet)
					    window
					    win:EM_SETPASSWORDCHAR
					    code 0)))))
      

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

;;; The EN_KILLFOCUS and EN_SETFOCUS messages are how the text-gadgets
;;; are told they have lost/gained focus.  They are sent as part of
;;; the WM_COMMAND message.
(defmethod acl-clim::command-event :around ((gadget mswin-text-edit) 
					    port sheet wparam lparam)
  (let ((notifycode (acl-clim::hiword wparam)))
    (cond ((= notifycode win:EN_KILLFOCUS) 
	   (handle-event
	    gadget
	    (allocate-event 
	     'focus-out-gadget-event
	     :gadget gadget)))
	  ((= notifycode win:EN_SETFOCUS)
	   (handle-event
	    gadget
	    (allocate-event 
	     'focus-in-gadget-event
	     :gadget gadget)))
	  (t
	   (call-next-method gadget port sheet wparam lparam)))))

(defclass silica::mswin-text-field (silica::mswin-text-edit)
  (;;mm: spr27890 
   (acl-clim::ignore-wm-char :initform nil))
  (:default-initargs 
      :text-style nil
      :external-label nil
      :x-margin 2 :y-margin 2))

(defmethod isa-textfield ((object t)) nil)
(defmethod isa-textfield ((object mswin-text-field)) t)

(defmethod text-edit-flags ((sheet mswin-text-field))
  (logior 
   (if (gadget-editable-p sheet) 0 win:ES_READONLY)
   win:ES_AUTOHSCROLL win:ES_LEFT win:WS_BORDER
   
   ;; bug12693 spr26749
   ;; If Echo-character is set, make this a password field.
   (if (text-field-echo-character sheet) win:ES_PASSWORD 0)
   
   ))

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
  (with-slots (x-margin y-margin initial-space-requirement nlines ncolumns) 
      pane
    (let* ((parent (sheet-parent pane))
           (parent-viewport-p (isa-viewport parent)))
      (let ((w 0) 
            (min-w (process-width-specification pane '(1 :character)))
            (h 0)
            (value (gadget-value pane))
            (min-h (process-height-specification pane *min-text-field-height*)))
        ;; WIDTH
        (cond (parent-viewport-p
               (setq w (process-width-specification pane width)))
              ((and initial-space-requirement
                    (plusp (process-width-specification
                            pane (space-requirement-width initial-space-requirement))))
               ;; This is where accepting-values views factors in.
               (setq w (max (process-width-specification 
                             pane (space-requirement-width initial-space-requirement))
                            min-w)))
              (ncolumns
               (setq w (process-width-specification pane `(,ncolumns :character))))
              ((stringp value)
               (setq w (max (process-width-specification pane value)
                            *default-text-field-width*)))
              (t
               (setq w *default-text-field-width*)))
        (setq w (max w min-w))

        ;; HEIGHT
        (cond (parent-viewport-p
               (setq h (process-height-specification pane height)))
              ((and initial-space-requirement
                    (plusp (process-height-specification
                            pane (space-requirement-height initial-space-requirement))))
               ;; This is where accepting-values views factors in.
               (setq h (max (process-height-specification 
                             pane (space-requirement-height initial-space-requirement))
                            min-h)))
              (nlines
               (setq h (process-height-specification pane `(,nlines :line))))
              ((stringp value)
               (setq h (process-height-specification pane value)))
              (t
               (setq h min-h))) 
        (setq h (max h min-h))

        (make-space-requirement
         :width  w :min-width min-w
         :height h :min-height min-h
         )))))

(defmethod handle-event ((pane mswin-text-field) (event key-press-event))
  
  ;;mm: spr27890: keep the simpler method for the more general
  ;;              mswin-text-edit class
  
  ;; This event occurs when a text-field recevies a RETURN character.
  ;;mm: spr27890 OR a TAB character.
  
  ;;afuchs: also, handle shift-TAB correctly, and support tabbing in text-fields 
  ;;        that are outside accepting-values panes.
  
  ;;(format t "~&handle-event in~%") (sleep 2) (format t "~&handle-event start~%")
  (let ((keysym (slot-value event 'key-name))
        (shifted-p (not (zerop (logand +shift-key+ (event-modifier-state event))))))
    (case keysym
      (:newline
       (acl-clim::win-catch-command
        #'(lambda (pane)
            (activate-callback pane (gadget-client pane) (gadget-id pane)))
        pane)))
    (labels ((text-panes (frame)
               (let ((result nil))
                 (map-over-sheets (lambda (sheet)
                                    (typecase sheet
                                      (mswin-text-edit (push sheet result))))
                                  (frame-top-level-sheet frame))
                 (nreverse result))))
    (or
     (let* ((frame (pane-frame pane))
            (children (text-panes frame)))
       (flet ((scan (all)
               (do (p (tl all (cdr tl)))
                   ((atom tl) nil)
                 (setf p (first tl))
                 (when (typecase p
                         ;; use TAB or ENTER to skip from one text-field 
                         ;; to the next
                         (mswin-text-edit (slot-value p 'silica::editable-p))
                         (otherwise nil))
                   (port-move-focus-to-gadget acl-clim::*acl-port* p)
                   (return t)))))
         (or (scan (cdr (member pane (if (and (eql keysym :tab) shifted-p)
                                         (reverse children)
                                       children))))
             (case keysym
               (:tab 
                ;; wrap around if TAB
                (scan (if shifted-p (reverse children) children)))
               (:newline
                ;; press the OK button if pressed ENTER on the last field
                (acl-clim::activate-default-gadget frame) nil)))))  
     
     ;; Give up the focus
     (win:SetFocus (win:GetActiveWindow)))
    ;;(format t "~&handle-event out~%")
    )))


;;
;; text-field-cursor
;; spr 30683 (alemmens, 2005-11-30)
;;

(defmethod text-field-cursor ((text-field silica::mswin-text-edit))
  (let ((text (clim:gadget-value text-field))
        (windows-cursor (silica::get-selection text-field)))
    ;; The Windows representation of the text uses
    ;; two characters (newline and return) for each
    ;; newline, so we need to correct the cursor.
    (let ((result 0))
      (loop with actual = 0
            for c = (if (< result (length text))
                        (char text result)
                        #\Space)
            while (< actual windows-cursor)
            do (progn
                 (incf actual
                       (if (char= c #\newline) 2 1))
                 (incf result)))
      result)))

(defmethod (setf text-field-cursor) (cursor (text-field mswin-text-edit))
  (let* ((text (clim:gadget-value text-field))
         ;; Add 1 for each newline before the cursor.
         (extra (count #\newline text :end cursor)))
    (set-caret-pos text-field (+ cursor extra))))

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
   (raster-op :initform *default-picture-button-op* :initarg :raster-op)
   
   ;; New slots to support "graying-out" deactivate
   ;; "colored" buttons on mswindows.
   (original-pane-foreground :initform nil :accessor original-pane-foreground)
   (deactivated-pane-foreground :initarg :deactivated-pane-foreground
				:initform nil 
				:accessor deactivated-pane-foreground)
   ;; Slot to hold "grayed-out" pattern for a pattern-label.
   (deactivated-label-pixmap :initarg :deactivated-label-pixmap
			     :initform nil 
			     :accessor deactivated-label-pixmap)
   )
  (:default-initargs :label nil
    :text-style nil
    :show-as-default nil
    :external-label nil
    ;; changed default-margins to make
    ;; picture-buttons look better (cim 10/4/96)
    :x-margin 2 :y-margin 2))

(defvar *hbutton-width* 21)
(defvar *hbutton-height* 21)

(defvar +hpbutton-deactivate-light-gray+ (clim-utils::make-gray-color-1 0.8))
(defvar +hpbutton-deactivate-dark-gray+ (clim-utils::make-gray-color-1 0.3))

(defun guess-deactivated-color (orig-color) 
  (cond ((and (typep orig-color 'clim-utils::gray-color)
	      (< 0.6 (color-rgb orig-color)))
	 +hpbutton-deactivate-dark-gray+)
	(t
	 +hpbutton-deactivate-light-gray+)))

(defun make-deactivated-label-pixmap (label deactivated-pane-foreground)
  (cond ((acl-clim::isa-pattern label)
	 (let ((orig-array (clim-utils::pattern-array label))
	       (orig-designs (clim-utils::pattern-designs label)))
	   (let ((num-designs (length orig-designs)))
	     (cond ((= num-designs 0)
		    ;; What does it mean to have no designs?
		    ;; Just return the original label
		    label)
		   (t
		    (let ((orig-background (aref orig-designs 0)) 
			  (new-designs (make-array num-designs)))
		      ;; Copy the original designs...
		      (loop for iii below num-designs
			  do (setf (aref new-designs iii)
			       (aref orig-designs iii)))
		      ;; ...assume the first color is the background, 
		      ;; and try to replace it...
		      (setf (aref new-designs 0) 
			(or deactivated-pane-foreground 
			    (guess-deactivated-color orig-background)))
		      ;; ...and return the new pattern.
		      (make-pattern orig-array
				    new-designs)))))))
	(t
	 ;; Can't do anything with a pixmap,
	 ;; So just use it.
	 label)))

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
	    ;; If we have pixmap-label, try to setup a "grayed-out" pixmap.
	    ;; 1] Use the deactivated-label-pixmap if it is already defined.
	    ;; 2] If not, try to make a "best-guess" pixmap from
	    ;;    the current label-pixmap
	    (let ((alt-pixmap (or (deactivated-label-pixmap sheet)
				  (make-deactivated-label-pixmap
				   label
				   (deactivated-pane-foreground sheet)))))
	      (when alt-pixmap
		(setf (deactivated-label-pixmap sheet)
		  (if (acl-clim::isa-pattern alt-pixmap)
		      (with-sheet-medium (medium sheet)
			(with-output-to-pixmap 
			    (stream medium
				    :width (pattern-width alt-pixmap)
				    :height (pattern-height alt-pixmap))
			  (draw-pattern* stream alt-pixmap 0 0)))
		    alt-pixmap))
		  ))
	    
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
      
      (setf (original-pane-foreground sheet) 
	(or (pane-foreground sheet)
	    (let ((resources (acl-clim::port-default-resources port)))
	      (getf resources :foreground))))
      
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
      (win:SetROP2 hdc win:R2_COPYPEN)
      (let* ((dc-image 
	      (with-sheet-medium (m pane)
		(acl-clim::dc-image-for-ink 
		 m (pane-background pane))))
	     (brush (acl-clim::dc-image-brush dc-image)))
	(win:SelectObject hdc brush)
	(win:DrawEdge hdc
		      rect 
		      (if (logtest state win:ODS_SELECTED)
			  win:BDR_SUNKEN
			win:BDR_RAISED)
		      (+ win:BF_RECT win:BF_MIDDLE))
	(win:Rectangle hdc 1 1 (- bwidth 2) (- bheight 2))
	(let ((pixmap (slot-value pane 'pixmap))
	      (label (gadget-label pane)))
	  (cond (pixmap
		 (let ((pixmap2 (if (gadget-active-p pane)
				    pixmap 
				  (or (deactivated-label-pixmap pane)
				      (slot-value pane 'pixmap)))))
		   (let* ((op (slot-value pane 'raster-op))
			  (width (pixmap-width pixmap2))
			  (height (pixmap-height pixmap2))
			  (x (floor (- bwidth width) 2))
			  (y (floor (- bheight height) 2)))
		     (when (logtest state win:ODS_SELECTED)
		       (incf x)
		       (incf y))
		     (win:BitBlt hdc x y width height (acl-clim::pixmap-cdc pixmap2) 0 0
				 (acl-clim::bop->winop op)))))
			
		(label
		 (acl-clim::adjust-gadget-colors pane hdc)
		 (with-sheet-medium (medium pane)
		   (let* ((port (port medium))
			  (text-style (medium-merged-text-style medium))
			  (font (text-style-mapping port text-style))
			  (index (acl-clim::acl-font-index font)))
		     (when (acl-clim::valid-handle index) (win:SelectObject hdc index))
		     (multiple-value-bind (cstr len)
			 (silica::xlat-newline-return label)
		       (multiple-value-bind (width height) 
			   (text-size medium label :text-style text-style)
			 (let ((x (floor (- bwidth width) 2))
			       (y (floor (- bheight height) 2)))
			   (excl:with-native-string (cstr cstr)
			     (or (win:TextOut hdc x y cstr len) 
				 (acl-clim::check-last-error "TextOut" :action :warn))))
			 )))))))
	))))

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
  ;; Handle WM_COMMAND event.
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
  (declare (ignore clr))
  (with-slots (mirror) pane
    (when mirror
      ;;; Work-around to force button to refresh.
      (win:SetWindowText mirror (or (gadget-label pane) "")))))

(defmethod (setf pane-foreground) :after (clr (pane hpbutton-pane))
  (declare (ignore clr))
  (with-slots (mirror) pane
    (when mirror
      ;;; Work-around to force button to refresh.
      (win:SetWindowText mirror (or (gadget-label pane) "")))))

(defmethod reload-label-string ((gadget hpbutton-pane))
  (with-slots (mirror) gadget
    (when mirror
      (let ((str (gadget-label gadget)))
	(when (stringp str)
	  (excl:with-native-string (str str)
	    (win:SetWindowText mirror str)))))))

(defmethod note-gadget-activated :after ((client t)
					 (gadget hpbutton-pane))
  (when (or (pane-foreground gadget)
	    (pane-background gadget))
    (setf (pane-foreground gadget)
      (or (original-pane-foreground gadget)
	  (let ((port (port gadget)))
	    (getf (acl-clim::port-default-resources port)
		  :background))
	  clim:+black+)))
  ;; Refresh the label after activation.
  (reload-label-string gadget)
  )

(defmethod note-gadget-deactivated :after ((client t)
					   (gadget hpbutton-pane))
  (when (or (pane-foreground gadget)
	    (pane-background gadget))
    (let ((dpf (deactivated-pane-foreground gadget)))
      (when (null dpf)
	;; If not specified, make a best guess at
	;; color to use for deactivated-pane-foreground.
	(let ((opf (original-pane-foreground gadget)))
	  (cond ((and (typep opf 'clim-utils::gray-color)
		      (< 0.6 (color-rgb opf)))
		 (setq dpf +hpbutton-deactivate-dark-gray+))
		(t
		 (setq dpf +hpbutton-deactivate-light-gray+))))	
	(setf (deactivated-pane-foreground gadget) dpf)) 

      (setf (pane-foreground gadget) dpf)))
  ;; Refresh the label after activation.
  (reload-label-string gadget)
  )

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
	   gadget-id
	   (button-label-justify (gadget-button-label-justify sheet)) ; bug12221
	   )
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
	  resources
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
	  ;; pnc Dec99 for spr20626
	  ;; If this is set, the toggle button is drawn
	  ;; as a OWNERDRAWn push-button.
	  #+IGNORE (unless (eq (pane-background sheet) 
			       (getf resources :background))
			   (setq buttonstyle win:BS_OWNERDRAW))
	  #+IGNORE (unless (eq (pane-foreground sheet) 
			       (getf resources :foreground))
			   (setq buttonstyle win:BS_OWNERDRAW))
	  (acl-clim::hbutton-open parent gadget-id
				  left top width height 
				  :buttonstyle buttonstyle
				  :value value
				  :label label
				  :button-label-justify button-label-justify  ; bug12221
				  )))
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
    (win:SetROP2 hdc win:R2_COPYPEN)
    (let* ((dc-image 
	    (with-sheet-medium (m pane)
	      (acl-clim::dc-image-for-ink 
	       m (pane-background pane))))
	   (brush (acl-clim::dc-image-brush dc-image)))
      (win:SelectObject hdc brush)
      (win:DrawEdge hdc
		    rect 
		    (if (logtest state win:ODS_SELECTED)
			win:BDR_SUNKEN
		      win:BDR_RAISED)
		    (+ win:BF_RECT win:BF_MIDDLE))
      (let ((margin 1))
	(win:Rectangle hdc margin margin 
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
			(acl-clim::bop->winop op))))))))

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
			 (event window-change-event))
  ;; Handle WM_COMMAND event.
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
     ((x-margin :initform 0)
      (y-margin :initform 0))
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
  ;; Handle WM_COMMAND event.
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
		      (ct:null-handle win:hwnd) ; we really want win:HWND_TOP
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
  ;; Handle WM_COMMAND event.
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


(defmethod initialize-instance :after ((scroll-bar mswin-scroll-bar) &key &allow-other-keys)
  ;; Initialization is now taken care of by an initialize-instance after method
  ;; on scroll-bar in silica/gadgets.lisp.  This dummy is only necessary as long
  ;; as CLIM hasn't been recompiled from scratch. (alemmens, 2004-12-2004)
  'ignore)


(defmethod native-gadget-range* ((scroll-bar mswin-scroll-bar))
  (values 0 (acl-clim::win-scroll-grain acl-clim::*acl-port*)))


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
      ;; this from rfe4072
      ;; Make sure defaults are sensible at the time the WIN32 scroll bar is 
      ;; created.
      (setf (gadget-min-value sheet) 
            (or (gadget-min-value sheet) 0))
      (setf (gadget-max-value sheet)
            (or (gadget-max-value sheet) 1))
      (change-scroll-bar-values	       
       sheet
       ;; remaining from rfe4072
       :slider-size (or (scroll-bar-size sheet) 1)
       :value (or (gadget-value sheet) 0)
       :line-increment (or (scroll-bar-line-increment sheet) 1))
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
  ;; I simplified this (and improved dealing with unusual gadget ranges)
  ;; by using convert-scroll-bar-xxx-out (alemmens, 2004-12-24).
  (declare (ignore page-increment line-increment))
  (let ((mirror (sheet-direct-mirror sb))
        (range (gadget-range sb)))
    (when mirror
      (unless slider-size (setq slider-size (scroll-bar-size sb)))
      (setq slider-size (min slider-size range)) ; sanity check
      (unless value (setq value (gadget-value sb)))
      (let* ((struct (ct:ccallocate win:scrollinfo))
             (page (convert-scroll-bar-size-out sb slider-size))
             (position (convert-scroll-bar-value-out sb value)))
        (ct:csets
         win:scrollinfo struct
         cbSize (ct:sizeof win:scrollinfo)
         fMask win:SIF_ALL
         nMin 0
         nMax (acl-clim::win-scroll-grain acl-clim::*acl-port*)
         nPage page
         nPos position)
        (win:SetScrollInfo mirror win:SB_CTL struct 1)))))

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
			 (event window-change-event))
  ;; Handle WM_COMMAND event.
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

(defmethod acl-clim::sheet-wants-default-pointer 
    ((object acl-clim::acl-text-editor-pane)) 
  t)

;; new code to deal with resources - ie background foreground and
;; text-stlye for various windows controls (cim 10/12/96)

;; There is a question here of when is it safe to delete the brush
;; used for painting the background. We assume in the code below that
;; by the time the next wm_ctlcolorxxx message arrives the previously
;; returned brush can be freed - is this a reasonable assumption? 
;; (cim 10/11/96)

(defmethod adjust-gadget-colors (pane hdc)
  (excl:without-interrupts			; due to global variable
    (when (acl-clim::valid-handle *background-brush*)
      (or (win:DeleteObject *background-brush*) 
	  (error "DeleteObject")))
    (let* ((bg (color->wincolor (pane-background pane)))
	   (fg (color->wincolor (pane-foreground pane))))
      (win:SetBkColor hdc bg)
      (win:SetTextColor hdc fg)
      (setq *background-brush* (win:CreateSolidBrush bg)))))

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
      (excl:with-native-string (clim-class (clim-class *acl-port*))
	(excl:with-native-string (win-name (win-name *acl-port*))
	  (win:CreateWindowEx exstyle
			      clim-class
			      win-name
			      winstyle
			      left top width height
			      (or parent 0)
			      0		; menu
			      (hinst *acl-port*)
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
  (handle-repaint gadget t)
  )
