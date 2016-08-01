;; See the file LICENSE for the full license governing this code.
;;

#|****************************************************************************
*                                                                            *
*                                                                            *
*  This file includes some preliminary support                               *
*                                                                            *
*                                                                            *
*                                                                            *
****************************************************************************|#

(in-package :acl-clim)


(declaim (special *acl-port*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Open a combo box control.

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
	  (excl:with-native-string (x "COMBOBOX")
	    (excl:with-native-string (label label)
	      (win:CreateWindowEx 
	       0			; extended-style
	       x			; classname
	       label			; windowname
	       (logior
		(if (member scroll-mode '(:vertical :both t :dynamic)) 
		    win:WS_VSCROLL
		  0)
		(if (member scroll-mode '(:horizontal :both t :dynamic)) 
		    win:WS_HSCROLL
		  0)
		win:WS_CHILD
		win:WS_TABSTOP
		win:CBS_DROPDOWNLIST)
	       0 0 0 0
	       parent 0
	       (hinst *acl-port*) (symbol-name (gensym)))))))
    (if (zerop hwnd)
	;; failed
	(cerror "proceed" "failed")
      ;; else succeed if we can init the DC
      (progn
	(win:SetWindowPos hwnd 0 
			  left top width height
			  0
			  ;;#.(logior win:SWP_NOACTIVATE win:SWP_NOZORDER)
			  )
	(let* ((index -1)
	       (item-name ""))
	  (dolist (item items)
	    (setf item-name (funcall name-key item))
	    (incf index)
	    (excl:with-native-string (item-name item-name)
	      (win:SendMessage hwnd win:CB_INSERTSTRING index item-name))
	    ))
	(win:SendMessage hwnd win:CB_SETCURSEL (or value 0) 0)
	(win:SendMessage hwnd CB_SETTOPINDEX (or value 0) 0)))
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
  (let* ((style
 	  (logior
	   win:LBS_NOINTEGRALHEIGHT	; partial item displayed at bottom
	   win:LBS_NOTIFY
	   win:LBS_USETABSTOPS		; Expands tab characters in items
	   (if sorted win:LBS_SORT 0)
	   (if (eq mode :nonexclusive) win:LBS_MULTIPLESEL 0)
	   win:WS_CHILD
	   (if (member scroll-mode '(:horizontal :both t :dynamic))
	       win:WS_HSCROLL
	     0)
	   (if (member scroll-mode '(:vertical :both t :dynamic))
	       win:WS_VSCROLL
	     0)
	   (if (member scroll-mode '(:horizontal :vertical :both t))
	       LBS_DISABLENOSCROLL
	     0)
	   win:WS_CLIPCHILDREN 
	   win:WS_CLIPSIBLINGS))
	 (exstyle win:WS_EX_CLIENTEDGE)
	 (hwnd
	  (excl:with-native-string (classname "LISTBOX")
	    (excl:with-native-string (windowname label)
	      (win:CreateWindowEx exstyle
				  classname ; classname
				  windowname ; windowname
				  style
				  0 0 0 0
				  parent
				  0
				  (hinst *acl-port*)
				  (symbol-name (gensym)))))))
    (if (zerop hwnd)
	;; failed
	(cerror "proceed" "failed")
      ;; else succeed if we can init the DC
      (progn
	(win:SetWindowPos hwnd 0 
			  left top width height
			  #.(logior win:SWP_NOACTIVATE win:SWP_NOZORDER))
	(let* ((index -1)
	       (item-name "")
	       )
	  (dolist (item items)
	    (setf item-name (funcall name-key item))
	    (incf index)
	    (excl:with-native-string (item-name item-name)
	      (win:SendMessage hwnd win:LB_INSERTSTRING index item-name))
	    ))
	(if (eq mode :nonexclusive)
	    (let ((i 0))
	      (dolist (item items)
		(win:SendMessage
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
	      (win:SendMessage 
	       hwnd win:LB_SETCURSEL i 0)
	      (win:SendMessage 
	       hwnd win:LB_SETTOPINDEX i 0))))
	;; we put in the 20% hack because
	;; compute-set-gadget-dimensions in acl-widg is
	;; inherently wrong - see the comment (cim 9/25/96)
	(win:SendMessage hwnd win:LB_SETHORIZONTALEXTENT
			 (floor (* horizontal-extent 1.2)) 0)))
    hwnd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Open a scrollbar control

(defun scrollbar-open (parent left top width height orientation)
  (let* ((hwnd
	  (excl:with-native-string (classname "SCROLLBAR")
	    (excl:with-native-string (windowname "")
	      (win:CreateWindowEx 
	       0			; style
	       classname		; classname
	       windowname		; windowname
	       (logior (if (eql orientation :horizontal) 
			   win::SBS_HORZ win::SBS_VERT)
		       win::WS_CHILD
                       ;; Removed the WS_BORDER flag.
                       ;; It's non-standard and, what's worse, it resulted
                       ;; in some garbage pixels (alemmens, 2005-01-19)
		       win::WS_CLIPCHILDREN 
		       win::WS_CLIPSIBLINGS
                       ) ; style
	       0 0 0 0			; x, y, width, height
	       parent
	       0
	       (hinst *acl-port*)
	       (symbol-name (gensym)))))))
    (if (zerop hwnd)
	;; failed
	(cerror "proceed" "failed")
      ;; else succeed if we can init the position
      (win::SetWindowPos hwnd 0 
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
			  (label "")
			  (button-label-justify nil))
  (declare (ignore nobutton id))
  ;; Both push buttons and radio buttons are created here.
  (let ((style (logior buttonstyle
		       win:WS_TABSTOP
		       win:WS_CHILD
		       win:WS_CLIPCHILDREN 
		       win:WS_CLIPSIBLINGS)))
    (when (eq button-label-justify :left)
      ;; bug12221/SPR24998 -pnc
      ;; See discussion of button-label-justify on toggle-button (in silica/gadgets.lisp).
      ;; This is only really relevant for hbutton-pane.
      (setq style (logior style
			  win:BS_LEFTTEXT  ; Put the text on the left
			  #x0200           ; BS_RIGHT: Right-justify the text in the label
			  )))			  
    (let* ((nlabel (cleanup-button-label label))
	   (hwnd
	    (excl:with-native-string (classname "BUTTON")
	      (excl:with-native-string (windowname nlabel)
		(win:CreateWindowEx 0
				    classname ; classname
				    windowname ; windowname
				    style ; style
				    0 0 0 0
				    parent
				    0
				    (hinst *acl-port*)
				    (symbol-name (gensym)))))))
      (if (zerop hwnd)
	  ;; failed
	  (cerror "proceed" "failed")
	;; else succeed if we can init the DC
	(progn
	  (win:SetWindowPos hwnd 0
			    left top width height
			    #.(logior win:SWP_NOACTIVATE win:SWP_NOZORDER))
	  (win:SendMessage hwnd win:BM_SETCHECK (if value 1 0) 0)))
      hwnd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Open an edit control

(defun hedit-open (parent id left top width height 
		   &key (editstyle 0)
		        (value nil)
			(label "")
			(scroll-mode nil))
  (declare (ignore id))
  (let ((hwnd
         (excl:with-native-string (classname "EDIT")
           (excl:with-native-string (windowname label)
             (win:CreateWindowEx 
              win:WS_EX_CLIENTEDGE
              classname                 ; classname
              windowname		; windowname
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
                      win:WS_CLIPSIBLINGS) ; style
              0 0 0 0
              parent
              0
              (hinst *acl-port*)
              (symbol-name (gensym)))))))
    (cond ((zerop hwnd)
           (cerror "Continue anyway." "Can't create text-edit gadget.")
           hwnd)
          (t
           ;; Initialize contents.
           (when (stringp value)
             (excl:with-native-string (s1 value)
               ;; Don't call xlat-newline-return.
               ;; spr 30683 (alemmens, 2005-11-30)
               (win:SetWindowText hwnd s1)))
           ;; Override the default window proc.
           (setf (std-ctrl-proc-address *acl-port*)
                 (win:GetWindowLong hwnd win:GWL_WNDPROC))
           (win:SetWindowLong hwnd
                              win:GWL_WNDPROC
                              (clim-ctrl-proc-address *acl-port*))
           (win:SetWindowPos hwnd 0 
                             left top width height
                             #.(logior win:SWP_NOACTIVATE win:SWP_NOZORDER))
           hwnd))))



;;; bitmap support

(defconstant +bits-per-pixel+ 8)
(defconstant +maxcolors+ (expt 2 +bits-per-pixel+))
(defconstant +bitmapinfosize+ (+ (ct:sizeof win:bitmapinfoheader)
				 (* +maxcolors+ (ct:sizeof win:rgbquad))))

(defun acl-bitmapinfo (colors width height medium)
  (assert (<= (length colors) +maxcolors+))
  ;; returns the appropriate bitmapinfo and DIB_XXX_COLORS constant
  (let ((bitcount +bits-per-pixel+)
	(bmi (ct:callocate win:bitmapinfo :size +bitmapinfosize+)))
    (ct:csets win:bitmapinfoheader bmi
	      biSize (ct:sizeof win:bitmapinfoheader)
	      biWidth width
	      biHeight (- height)	; if negative, flips image
	      biPlanes 1		; must be 1
	      biBitCount bitcount
	      biCompression win:BI_RGB	; no compression
	      biSizeImage 0		; zero for BI_RGB images
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
	       (setq rgb (medium-background medium)))
	      ((eq rgb +transparent-ink+)
	       (setq rgb (medium-background medium))))
	(multiple-value-bind (red green blue) (color-rgb rgb)
	  (ct:cset windows:bitmapinfo bmi
		   (windows::bmiColors (fixnum i) windows::rgbReserved) 
		   0)
	  (ct:cset windows:bitmapinfo bmi
		   (windows::bmiColors (fixnum i) windows::rgbRed)
		   (floor (* 255 red)))
	  (ct:cset windows:bitmapinfo bmi
		   (windows::bmiColors (fixnum i) windows::rgbBlue)
		   (floor (* 255 blue)))
	  (ct:cset windows:bitmapinfo bmi
		   (windows::bmiColors (fixnum i) windows::rgbGreen)
		   (floor (* 255 green))))))
    ;; return values
    (values bmi win:DIB_RGB_COLORS)))

(defmethod get-texture (device-context pixel-map bitmapinfo)
  ;; The value of this function becomes the dc-image-bitmap.
  ;; It gets applied to the device context using SELECT-OBJECT.
  ;; Pixel-map is an array of integers.  The element type is
  ;; specific to bits-per-pixel specified in the BMP file.
  ;;
  ;; Create nondiscardable Device Dependent Bitmap.
  ;; The Windows docs suggest we should be using device independent bitmaps.
  (let ((texture-handle
	 (win:CreateDIBitmap
	  device-context
	  bitmapinfo 
	  win:CBM_INIT			; initialize bitmap bits
	  pixel-map
	  bitmapinfo 
	  win:DIB_RGB_COLORS)))
    (when (zerop texture-handle)
      ;; This code comes from Ken Cheetham's mail attached to
      ;; spr21608.  This should finally fix this problem as I think he
      ;; and/or microsoft understand why it is occuring now. The
      ;; underlying answer seems to be that you need to use space
      ;; which is really got from the C library malloc, not from any
      ;; lisp-runtime approximation to it.
      ;;
      ;; This is hugely simpler than the previous code, which had a
      ;; very hairy computation of the buffer size, which I've just
      ;; not done here.  I think this means that bitmaps may be
      ;; assumed 8bits deep, if they were deeper you'd need a suitable
      ;; factor in the computation of image-size.
      (let ((image-size (array-total-size pixel-map)))
	(with-malloced-space (buffer-address image-size)
	  (memcpy buffer-address pixel-map image-size)
	  (setq texture-handle
	    (win:CreateDIBitmap
	     device-context
	     bitmapinfo 
	     win:CBM_INIT		; initialize bitmap bits
	     buffer-address
	     bitmapinfo 
	     win:DIB_RGB_COLORS)))))
    (when (zerop texture-handle)
      (check-last-error "CreateDIBitmap"))
    texture-handle))

;;; about box support

(defun pop-up-about-climap-dialog (frame &rest ignoreargs)
  (clim:notify-user frame 
		    (format nil "~%~A~%~%Version: ~A~%~%"
			    (clim-internals::frame-pretty-name frame)
			    (lisp-implementation-version))
		    :exit-boxes '((:exit "OK"))
		    :title (format nil "About ~A" 
				   (clim-internals::frame-pretty-name frame))))

(defun errno-to-text (errno)
  (let* ((pointer (make-array 1 :element-type 'unsigned-nat
			      :initial-element 0))
	 (flags (logior #x100		; format_message_allocate_buffer
			#x1000		; format_message_from_system
			))
	 ;; Unfortunately, most of the interesting error codes are not
	 ;; in the system's message table.  Where are they?  If we had
	 ;; a handle to the relevant module, we could specify that
	 ;; to FormatMessage in order to search a module's message table.
	 (chars (FormatMessage flags
			       0 errno 0 
			       pointer 0 0)))
    (values (if (plusp chars)
		(nsubstitute #\space #\return 
			     (excl:native-to-string (aref pointer 0)))
	      "unidentified system error")
	    chars)))

(defun check-last-error (name &key (action :error))
  ;; Check the value of GetLastError.  It is quite
  ;; impossible to ensure correct operation of CLIM
  ;; without paying attention to the errors that come
  ;; back from the system calls.  Sometimes, however,
  ;; it is more appropriate to warn about the problem 
  ;; than to signal an error.
  (let* ((code (win:GetLastError)))
    (cond ((zerop code) nil)
	  ((eq action :error)
	   (error "~A: (error ~A) ~A" name code (errno-to-text code)))
	  ((eq action :warn)
	   ;; Printing warnings on clim windows risks recursive
	   ;; warning loop.
	   (format *trace-output*
		   "~&Warning: ~A: (error ~A) ~A~%" name code (errno-to-text code))
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
  (if (win:OpenClipboard 0)
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
	    (setq cstring (string-to-foreign string mem))
	    (win:GlobalUnlock hmem)
	    ;; After calling SetClipboardData, the
	    ;; clipboard owns hMem.  It must not be 
	    ;; modified or freed.
	    (or (win:EmptyClipboard)
		(check-last-error "EmptyClipboard"))
	    (win:SetClipboardData format hmem)
	    (ff:free-fobject-c cstring)
	    t)
	(win:CloseClipboard))
    (check-last-error "OpenClipboard")))

(defun clipboard->lisp (&key (parser #'read-from-string)
			     (format win:CF_TEXT))
  ;; Can't open clipboard if somebody else has it open.
  (if (win:OpenClipboard 0)
      (unwind-protect
	  (let ((hmem (win:GetClipboardData format))
		(string nil))
	    ;; The clipboard owns hMem.  The application must
	    ;; not modify it, free it, or rely on it's value being
	    ;; stable after closing the clipboard.
	    (when (zerop hmem)
	      (check-last-error "GetClipboardData"))
	    (setq string (ct:handle-value win:win-handle hmem))
	    (cond ((zerop string) (values nil nil))
		  (t
		   (when (integerp string)
		     (setq string (excl:native-to-string string)))
		   (values (funcall parser string) string))))
	(win:CloseClipboard))
    (check-last-error "OpenClipboard")))

