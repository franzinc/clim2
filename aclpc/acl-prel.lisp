#|****************************************************************************
*                                                                            *
*                                                                            *
*  This file includes some preliminary support                               *
*                                                                            *
*                                                                            *
*                                                                            *
****************************************************************************|#

(in-package :acl-clim)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Open a combo box control.

(declaim (special *hinst*))

(defun combobox-scroll-bars (items)
  ;; If there are many items, you won't be able 
  ;; to see them without a scroll bar.
  (if (> (length items) 30) :vertical nil))

(defun hcombo-open (parent id left top width height 
		    &key (items nil)
			 (value nil)
			 (name-key #'identity)
			 (label "")
			 (scroll-mode (combobox-scroll-bars items)))
  (declare (ignore id))
  (let* ((hwnd
	  (win:CreateWindowEx 0		; extended-style
			      "COMBOBOX" ; classname
			      (nstringify label) ; windowname
			      (logior
			       (if (member scroll-mode '(:vertical :both)) win:WS_VSCROLL 0)
			       (if (member scroll-mode '(:horizontal :both)) win:WS_HSCROLL 0)
			       win:WS_CHILD
			       win:CBS_DROPDOWNLIST)
			      0 0 0 0
			      parent (ct::null-handle win::hmenu)
			      *hinst* (symbol-name (gensym)))))
    (if (ct:null-handle-p hwnd hwnd)
	;; failed
	(cerror "proceed" "failed")
      ;; else succeed if we can init the DC
      (progn
	(win:SetWindowPos hwnd (ct:null-handle hwnd) 
			  left top width height
			  0
			  ;;#.(logior win:SWP_NOACTIVATE win:SWP_NOZORDER)
			  )
	(let* ((index -1)
	       (item-name ""))
	  (dolist (item items)
	    (setf item-name (funcall name-key item))
	    (incf index)
	    (win:SendMessage hwnd win:CB_INSERTSTRING index item-name)
	    ))
	(win:sendMessage hwnd win:CB_SETCURSEL (or value 0) 0)
	(win:sendMessage hwnd win:CB_SETTOPINDEX (or value 0) 0)))
    hwnd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; open a list control

(defun hlist-open (parent id left top width height 
		   &key (mode :exclusive)
			(scroll-mode nil)
			(items nil)
			(value nil)
			(name-key #'identity)
			(value-key #'identity)
			(test #'eql)
			(border3d t)
			(sorted nil)
			(label "")
			(horizontal-extent 0))
  (declare (ignore border3d id))
  (let* ((hwnd
	  (win:CreateWindowEx 0		; extended-style
			  "LISTBOX"	; classname
			  (nstringify label) ; windowname
			  (logior
			   win:LBS_NOINTEGRALHEIGHT ; partial item displayed at bottom
			   win:LBS_NOTIFY
			   win:LBS_USETABSTOPS ; Expands tab characters in items
			   (if sorted win:LBS_SORT 0)
			   (if (eq mode :nonexclusive) win:LBS_MULTIPLESEL 0)
			   win:WS_CHILD
			   win:WS_BORDER
			   (if (member scroll-mode '(:horizontal :both t :dynamic))
			       win:WS_HSCROLL
			     0)
			   (if (member scroll-mode '(:vertical :both t :dynamic))
			       win:WS_VSCROLL
			     0)
			   (if (member scroll-mode '(:horizontal :vertical :both t))
			       win:LBS_DISABLENOSCROLL
			     0)
			   win:WS_CLIPCHILDREN 
			   win:WS_CLIPSIBLINGS) ; style
			  0 0 0 0
			  parent
			  #+acl86win32 (ct::null-handle win::hmenu)
			  #-acl86win32 (let ((hmenu (ccallocate hmenu)))
					 (setf (handle-value hmenu hmenu) id)
					; (or id (next-child-id parent))
					 hmenu)
			  *hinst*
			  #+acl86win32 (symbol-name (gensym))
			  #+aclpc acl-clim::*win-arg*)))
    (if (ct:null-handle-p hwnd hwnd)
	;; failed
	(cerror "proceed" "failed")
      ;; else succeed if we can init the DC
      (progn
	(win:SetWindowPos hwnd (ct:null-handle hwnd) 
		      left top width height
		      #.(logior win:SWP_NOACTIVATE win:SWP_NOZORDER))
	(let* ((index -1)
	       (item-name "")
	       ;;(cstr (ct::callocate (:char *) :size 256))
	       ;;(subsize 0)
	       )
	  (dolist (item items)
	    (setf item-name (funcall name-key item))
	    #+ignore
	    (setf subsize (length item-name))
	    #+ignore
	    (dotimes (i subsize)
	      (ct:cset (:char 256) cstr ((fixnum i))
		       (char-int (char item-name i))))
	    (incf index)
	    (win:SendMessage hwnd win:LB_INSERTSTRING index item-name)
	    ))
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

	;; we put in the 20% hack because
	;; compute-set-gadget-dimensions in acl-widg is
	;; inherently wrong - see the comment (cim 9/25/96)
	(win:sendMessage hwnd win:LB_SETHORIZONTALEXTENT
			 (floor (* horizontal-extent 1.2)) 0)))
    hwnd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Open a scrollbar control

(defun scrollbar-open (parent left top width height orientation)
  (let* ((hwnd
	  (win:CreateWindowEx 
	   0				; style
	   "SCROLLBAR"			; classname
	   (nstringify "")		; windowname
	   (logior (if (eql orientation :horizontal) 
		       win::SBS_HORZ win::SBS_VERT)
		   win::WS_CHILD
		   win::WS_BORDER
		   win::WS_CLIPCHILDREN 
		   win::WS_CLIPSIBLINGS) ; style
	   0 0 0 0			; x, y, width, height
	   parent
	   (ct:null-handle win::hmenu)
	   *hinst*
	   (symbol-name (gensym)))))
    (if (ct:null-handle-p hwnd hwnd)
	;; failed
	(cerror "proceed" "failed")
      ;; else succeed if we can init the position
      (win::SetWindowPos hwnd (ct:null-handle hwnd) 
			 left top width height
			 #.(logior win:SWP_NOACTIVATE win:SWP_NOZORDER)))
    hwnd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Open a botton control

(defun cleanup-button-label (label)
  (let ((nstr (make-string (length label))))
    (dotimes (i (length label))
      (if (char-equal (aref label i) #\newline)
          (setf (aref nstr i) #\Space)
          (setf (aref nstr i) (aref label i))))
    nstr))

(defun hbutton-open (parent id left top width height 
		     &key (buttonstyle win:BS_AUTORADIOBUTTON)
		          (value nil)
			  (nobutton nil)
			  (label ""))
  (declare (ignore nobutton id))
  (let* ((nlabel (cleanup-button-label label))
	 (hwnd
	  (win:CreateWindowEx 0
			       "BUTTON"	; classname
			       (nstringify nlabel) ; windowname
			       (logior buttonstyle
				       win:WS_CHILD
				       win:WS_CLIPCHILDREN 
				       win:WS_CLIPSIBLINGS) ; style
			       0 0 0 0
			       parent
			       (ct::null-handle win::hmenu)
			       *hinst*
			       (symbol-name (gensym)))))
    (if (ct:null-handle-p hwnd hwnd)
	;; failed
	(cerror "proceed" "failed")
      ;; else succeed if we can init the DC
      (progn
	(win:SetWindowPos hwnd (ct:null-handle hwnd) 
			  left top width height
			  #.(logior win:SWP_NOACTIVATE win:SWP_NOZORDER))
	(when value
	  (win:sendmessage hwnd
			   win:bm_setcheck
			   1 0))))
    hwnd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Open an edit control

(declaim (special std-ctrl-proc-address clim-ctrl-proc-address))

(defun hedit-open (parent id left top width height 
		   &key (editstyle 0)
		        (value nil)
			(label "")
			(scroll-mode nil))
  (declare (ignore id))
  (let* ((hwnd
	  (win:CreateWindowEx 
	   win:WS_EX_CLIENTEDGE
	   "EDIT"			; classname
	   (nstringify label)		; windowname
	   (logior editstyle
		   win:WS_CHILD
		   win:WS_BORDER
		   win:WS_TABSTOP
		   (if (member scroll-mode '(:horizontal :both t :dynamic))
		       win:WS_HSCROLL
		     0)
		   (if (member scroll-mode '(:vertical :both t :dynamic))
		       win:WS_VSCROLL
		     0)
					   
		   win:WS_CLIPCHILDREN 
		   win:WS_CLIPSIBLINGS)	; style
	   0 0 0 0
	   parent
	   (ct::null-handle win::hmenu)
	   *hinst*
	   (symbol-name (gensym)))))
    (if (ct:null-handle-p hwnd hwnd)
	;; failed
	(cerror "proceed" "failed")
      ;; else succeed if we can init the DC
      (progn
	(if (stringp value)
	    (win:setWindowText 
	     hwnd 
	     (silica::xlat-newline-return value)))
	;; Override the default window proc.
	(progn				;+++
	  (setf std-ctrl-proc-address
	    (win:GETWINDOWLONG hwnd WINDOWS::GWL_WNDPROC))
	  (win:SETWINDOWLONG hwnd
			     WINDOWS::GWL_WNDPROC
			     clim-ctrl-proc-address))
	(win:SetWindowPos hwnd (ct:null-handle hwnd) 
		      left top width height
		      #.(logior win:SWP_NOACTIVATE win:SWP_NOZORDER))))
    hwnd))

;;; bitmap support

(defconstant +bits-per-pixel+ 8)
(defconstant +maxcolors+ (expt 2 +bits-per-pixel+))
(defconstant +bitmapinfosize+ (+ (ct:sizeof win:bitmapinfoheader)
				 (* +maxcolors+ (ct:sizeof win:RgbQuad))))

;; The use of this prevents bitmap drawing from being reentrant.  JPM 5/98.
(defconstant +rgb-bitmapinfo+
    (ct:callocate win:bitmapinfo :size +bitmapinfosize+))

(defun acl-bitmapinfo (colors width height medium)
  (assert (< (length colors) +maxcolors+))
  ;; returns the appropriate bitmapinfo and DIB_XXX_COLORS constant
  (let ((bitcount +bits-per-pixel+)
	(bmi +rgb-bitmapinfo+))
    (ct:csets win:bitmapinfoheader bmi
	      bisize (ct:sizeof win:bitmapinfoheader)
	      biwidth width
	      biheight (- height)	; if negative, flips image
	      biplanes 1		; must be 1
	      bibitcount bitcount
	      bicompression win:BI_RGB	; no compression
	      bisizeimage 0		; zero for BI_RGB images
	      biXPelsPerMeter 0
	      biYPelsPerMeter 0
	      biClrUsed (length colors)
	      biClrImportant 0		; all colors are "important"
	      )
    (dotimes (i (length colors))
      (let ((rgb (aref colors i)))
	(cond ((eq rgb +foreground-ink+)
	       (setq rgb (medium-foreground medium)))
	      ((eq rgb +background-ink+)
	       (setq rgb (medium-background medium))))
	(multiple-value-bind (red green blue) (color-rgb rgb)
	  (ct:cset windows:bitmapinfo bmi
		   (windows::bmicolors (fixnum i) rgbreserved) 
		   0)
	  (ct:cset windows:bitmapinfo bmi
		   (windows::bmicolors (fixnum i) rgbred)
		   (floor (* 255 red)))
	  (ct:cset windows:bitmapinfo bmi
		   (windows::bmicolors (fixnum i) rgbblue)
		   (floor (* 255 blue)))
	  (ct:cset windows:bitmapinfo bmi
		   (windows::bmicolors (fixnum i) rgbgreen)
		   (floor (* 255 green))))))
    ;; return values
    (values bmi win:DIB_RGB_COLORS)))

(defmethod get-texture (device-context pixel-map colors medium)
  (let* ((width (array-dimension pixel-map 1))
	 (height (array-dimension pixel-map 0))
	 texture-handle)
    (multiple-value-bind (bitmapinfo dib-mode)
	(acl-bitmapinfo colors width height medium)
      (setq texture-handle
	(win:CreateDIBitmap
	 device-context
	 bitmapinfo 
	 win:CBM_INIT			; initialize bitmap bits
	 pixel-map
	 bitmapinfo 
	 dib-mode))
      (when (zerop texture-handle)
	(error "CreateDIBitmap: error"))
      texture-handle)))

;;; about box support

(defun pop-up-about-climap-dialog (frame &rest ignoreargs)
  (clim:notify-user frame 
		    (format nil "~%~A~%~%Version: ~A~%~%"
			    (clim-internals::frame-pretty-name frame)
			    (lisp-implementation-version))
		    :exit-boxes '((:exit "OK"))
		    :title (format nil "About ~A" 
				   (clim-internals::frame-pretty-name frame))))

(defun check-last-error (name &key (action :error))
  ;; All the calls to GetLastError should probably
  ;; be changed to go through here.
  (let ((code (win:getlasterror)))
    (cond ((zerop code) nil)
	  ((eq action :error)
	   (error "~A: system error ~A" name code))
	  ((eq action :warn)
	   (warn "~A: system error ~A" name code)
	   code)
	  (t code))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clipboard Support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This seems to work, but CLIM doesn't really use it
;; other than in one of the CLIM Demos.

(defun lisp->clipboard (object &key (printer #'prin1-to-string)
				    (format win:CF_TEXT))
  ;; Can't open clipboard if somebody else has it open.
  (if (win:OpenClipboard (ct:null-handle hwnd))
      (unwind-protect
	  (let* ((string 
		   (let ((*print-readably* t))
		     (funcall printer object)))
		 (cstring nil)
		 (l (length string))
		 (hmem (win:GlobalAlloc (logior win:GMEM_MOVEABLE
						win:GMEM_DDESHARE)
					(1+ l)))
		 (mem (win:GlobalLock hmem)))
	    (setq cstring (ff:string-to-char* string mem))
	    (win:GlobalUnlock hmem)
	    ;; After calling SetClipboardData, the
	    ;; clipboard owns hMem.  It must not be 
	    ;; modified or freed.
	    (or (win:emptyclipboard)
		(check-last-error "EmptyClipboard"))
	    (win:SetClipboardData format hmem)
	    (ff:free-fobject-c cstring)
	    t)
	(win:CloseClipboard))
    (check-last-error "OpenClipboard")))

(defun clipboard->lisp (&key (parser #'read-from-string)
			     (format win:CF_TEXT))
  ;; Can't open clipboard if somebody else has it open.
  (if (win:OpenClipboard (ct:null-handle hwnd))
      (unwind-protect
	  (let ((hmem (win:GetClipboardData format))
		(string nil))
	    ;; The clipboard owns hMem.  The application must
	    ;; not modify it, free it, or rely on it's value being
	    ;; stable after closing the clipboard.
	    (when (zerop hmem)
	      (check-last-error "GetClipboardData"))
	    (setq string (ct:handle-value win:handle hmem))
	    (cond ((zerop string) (values nil nil))
		  (t
		   (when (integerp string)
		     (setq string (ff:char*-to-string string)))
		   (values (funcall parser string) string))))
	(win:CloseClipboard))
    (check-last-error "OpenClipboard")))
