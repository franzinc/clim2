#|****************************************************************************
*                                                                            *
*                                                                            *
*  This file includes some preliminary support in the pc and cg packages.    *
*                                                                            *
*                                                                            *
*                                                                            *
****************************************************************************|#

(in-package :acl-clim)

(defmethod cg::device-listen ((stream sheet))
  (when (stream-input-buffer stream)
    (stream-listen stream)))

(defmethod cg::device-finish-output ((stream sheet))
  (when (sheet-medium stream)
    (stream-finish-output stream)))

(defmethod cg::device-force-output ((stream sheet))
  (when (sheet-medium stream)
    (stream-force-output stream)))

(defmethod cg::device-clear-output ((stream sheet))
  (when (sheet-medium stream)
    (stream-finish-output stream)))

(defmethod cg::device-clear-input ((stream sheet))
  (when (stream-input-buffer stream)
    (stream-clear-input stream)))

(in-package :cl-user)

;;;  prevents aclpc from complaining - possibly more could be
;;;  done here?
(defmethod cg::output-stream-p ((stream acl-clim::sheet)) t)

(defmethod printer::format2 ((stream acl-clim::sheet) str args)
  (printer::format2 *terminal-io* str args))

(defmethod cg::device-open ((stream acl-clim::sheet) options)
  ;; do nothing
  t)

(defmethod cg::device-close ((stream acl-clim::sheet) abort)
  ;; do nothing
  t)

(defmethod cg::device-eof-p ((stream acl-clim::sheet))
  ;; never at eof
  nil)

#+ignore ;; superceded by above version
(defmethod cg::device-listen ((stream acl-clim::sheet))
  (sys-listen))

(defmethod cg::device-nread-string ((stream acl-clim::sheet) string nchars)
   (setf (schar string 0) (acl-clim::read-char))
   1)

(defmethod cg::device-unread-char ((stream acl-clim::sheet) char)
  (acl-clim::unread-char char)
  t)

(defmethod cg::device-file-length ((stream acl-clim::sheet))
	;; pretend 
	1000)

(defmethod cg::device-file-position ((stream acl-clim::sheet))
	;; pretend 
	100)

(defmethod cg::device-set-file-position ((stream acl-clim::sheet) integer)
	;; do nothing
	t)

(defmethod cg::location-write-string ((stream acl-clim::sheet) string start end)
  ;; cheat, but does device-spaces too
  (acl-clim::write-string (subseq string start end)))

(defmethod cg::device-terpri ((stream acl-clim::sheet))
  (acl-clim::terpri))

(defmethod printer::call-print-basic ((stream acl-clim::sheet) func x)
  nil)

;;; +++rl needs more work
(defmethod printer::call-print-basic ((simple-stream
					clim-lisp::fundamental-stream)
				      fn x)
  (if *print-escape*
    (clim-lisp:format simple-stream "~S" x)
    (clim-lisp:format simple-stream "~A" x)))


(in-package :pc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Open a combo box control.

(defconstant CB_SETTOPINDEX #x015c)



(defun hcombo-open (parent id left top width height 
		     &key (mode :exclusive)
		          (items nil)
			  (value nil)
			  (name-key #'identity)
		          (border3d t)
			  (sorted nil)
			  (label ""))
  (let* ((hwnd
	   (CreateWindowEx 0 ; extended-style
		"COMBOBOX"		; classname
		(nstringify label)	; windowname
		(logior
		  WS_CHILD
		  CBS_DROPDOWNLIST
		  ;WS_CLIPCHILDREN 
		  ;WS_CLIPSIBLINGS
                )	; style
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
     (if (null-handle-p hwnd hwnd)
         ;; failed
         (cerror "proceed" "failed")
         ;; else succeed if we can init the DC
         (progn
           (SetWindowPos hwnd (null-handle hwnd) 
                         left top width 250 ;height
                         0 ;#.(ilogior SWP_NOACTIVATE SWP_NOZORDER)
                         #-acl86win32 :static)
           (let* ((index -1)
                  (item-name "")
                  (cstr (ct::callocate (:char *) :size 256))
                  (subsize 0))
             (dolist (item items)
               (setf item-name
                     (#+aclpc pc::lisp-string-to-scratch-c-string #-aclpc identity
                       (funcall name-key item)))
               (incf index)
               (pc::SendMessage-with-pointer
                 hwnd CB_INSERTSTRING index item-name #+aclpc :static #-aclpc 0 :static)
               ))
	   (win:sendMessage hwnd CB_SETCURSEL (or value 0) 0)
           (win:sendMessage hwnd CB_SETTOPINDEX (or value 0) 0)))
     hwnd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; open a list control

(defconstant LBS_DISABLENOSCROLL #x1000)

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
  (let* ((hwnd
	   (CreateWindowEx 0		; extended-style
		"LISTBOX"			; classname
		(nstringify label)	; windowname
		(logior
		  LBS_NOINTEGRALHEIGHT  ; partial item displayed at bottom
		  LBS_NOTIFY
		  LBS_USETABSTOPS       ; Expands tab characters in items
		  ; HLS_EXTRUDE ; (if border3d HLS_BORDER3D 0) ; 3Dness
		  ; HLS_HILITE ; highlight background when focussed
		  (if sorted LBS_SORT 0)
		  (if (eq mode :nonexclusive) LBS_MULTIPLESEL 0)
		  WS_CHILD
		  WS_BORDER
		  (if (member scroll-mode '(:horizontal :both t :dynamic))
		      WS_HSCROLL
		    0)
		  (if (member scroll-mode '(:vertical :both t :dynamic))
		      WS_VSCROLL
		    0)
		  (if (member scroll-mode '(:horizontal :vertical :both t))
		      LBS_DISABLENOSCROLL
		    0)
		  WS_CLIPCHILDREN 
		  WS_CLIPSIBLINGS)	; style
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
     (if (null-handle-p hwnd hwnd)
		 ;; failed
		 (cerror "proceed" "failed")
		 ;; else succeed if we can init the DC
		 (progn
		    (SetWindowPos hwnd (null-handle hwnd) 
		       left top width height
		       #.(ilogior SWP_NOACTIVATE SWP_NOZORDER)
		       #-acl86win32 :static)
		    (let* ((index -1)
			   (item-name "")
			   (cstr (ct::callocate (:char *) :size 256))
			   (subsize 0))
		      (dolist (item items)
		        (setf item-name
			      (#+aclpc pc::lisp-string-to-scratch-c-string
				   #+acl86win32 identity
				   (funcall name-key item)))
			#+ignore
			(setf subsize (length item-name))
			#+ignore
			(dotimes (i subsize)
			  (ct:cset (:char 256) cstr ((fixnum i))
				   (char-int (char item-name i))))
			(incf index)
			(pc::SendMessage-with-pointer
			  hwnd LB_INSERTSTRING index item-name :static :static)
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
		    (win:sendMessage hwnd LB_SETHORIZONTALEXTENT
				     (floor (* horizontal-extent 1.2)) 0)
		    
		    ;; check out compute-list-pane-selected-items
		    ;; in the unix code to see how to do this 
		    ;; (cim 9/25/96)
		    #+ignore
		    (win:sendMessage hwnd LB_settopindex value 0)


		    ;; (win:SendMessage hwnd HLM_SETREDRAW acl:TRUE 0 :static)
		    #+ignore
		    (showWindow hwnd SW_SHOW)))
     hwnd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Open a scrollbar control

(defun scrollbar-open (parent left top width height orientation)
  (let* ((hwnd
	  (win::createWindowEx 
	   0				; style
	   "SCROLLBAR"			; classname
	   (cg::nstringify "")		; windowname
	   (logior (if (eql orientation :horizontal) 
		       win::SBS_HORZ win::SBS_VERT)
		   win::WS_CHILD
		   win::WS_BORDER
		   win::WS_CLIPCHILDREN 
		   win::WS_CLIPSIBLINGS) ; style
	   0 0 0 0			; x, y, width, height
	   parent
	   (cg::null-handle win::hmenu)
	   cg::*hinst*
	   (symbol-name (gensym)))))
    (if (cg::null-handle-p hwnd hwnd)
	;; failed
	(cerror "proceed" "failed")
      ;; else succeed if we can init the position
      (win::SetWindowPos hwnd (cg::null-handle hwnd) 
			 left top width height
			 #.(logior win::SWP_NOACTIVATE win::SWP_NOZORDER)))
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

;;(defconstant BS_BITMAP #x80)
;;(defconstant BM_GETIMAGE #xf6)
;;(defconstant BM_SETIMAGE #xf7)

(defun hbutton-open (parent id left top width height 
		     &key (buttonstyle BS_AUTORADIOBUTTON)
		          (value nil)
			  (nobutton nil)
			  (label ""))
  (let* ((nlabel (cleanup-button-label label))
	 (hwnd
	  (win::createWindowEx 0
	   "BUTTON"			; classname
	   (nstringify nlabel)		; windowname
	   (logior buttonstyle
		   ;; BS_BITMAP
		   WS_CHILD
		   ;; WS_BORDER
		   WS_CLIPCHILDREN 
		   WS_CLIPSIBLINGS)	; style
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
    (if (null-handle-p hwnd hwnd)
	;; failed
	(cerror "proceed" "failed")
      ;; else succeed if we can init the DC
      (progn
	(SetWindowPos hwnd (null-handle hwnd) 
		      left top width height
		      #.(ilogior SWP_NOACTIVATE SWP_NOZORDER)
		      #-acl86win32 :static)
	#+ignore (showWindow hwnd SW_SHOW)
	(when value
	  (win:sendmessage hwnd
			   pc::bm_setcheck
			   #+ignore pc::HBM_PRESS
			   1 0))))
    hwnd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Open an edit control

(defun hedit-open (parent id left top width height 
		   &key (editstyle 0)
		        (value nil)
			(label "")
			(scroll-mode nil))
  (let* ((hwnd
	  (win::createWindowEx 
	   WS_EX_CLIENTEDGE
	   "EDIT"			; classname
	   (nstringify label)		; windowname
	   (logior editstyle
		   WS_CHILD
		   WS_BORDER
		   WS_TABSTOP
		   (if (member scroll-mode '(:horizontal :both t :dynamic))
		       WS_HSCROLL
		     0)
		   (if (member scroll-mode '(:vertical :both t :dynamic))
		       WS_VSCROLL
		     0)
					   
		   WS_CLIPCHILDREN 
		   WS_CLIPSIBLINGS)	; style
	   0 0 0 0
	   parent
	   (ct::null-handle win::hmenu)
	   *hinst*
	   (symbol-name (gensym)))))
    (if (null-handle-p hwnd hwnd)
	;; failed
	(cerror "proceed" "failed")
      ;; else succeed if we can init the DC
      (progn
	(if (stringp value)
	    (pc::setWindowText 
	     hwnd 
	     (#+aclpc 
	      pc::lisp-string-to-scratch-c-string 
	      #+acl86win32
	      identity
	      (silica::xlat-newline-return value))))
	;; Override the default window proc.
	(progn				;+++
	  (setf acl-clim::std-ctrl-proc-address
	    (pc::GETWINDOWLONG hwnd WINDOWS::GWL_WNDPROC))
	  (pc::SETWINDOWLONG hwnd
			     WINDOWS::GWL_WNDPROC
			     acl-clim::clim-ctrl-proc-address))
	(SetWindowPos hwnd (null-handle hwnd) 
		      left top width height
		      #.(ilogior SWP_NOACTIVATE SWP_NOZORDER))))
    hwnd))

;;; bitmap support

(defconstant acl-clim::rgb-bitmapinfo 
  (ct::callocate bitmapinfo :size (+ (sizeof bitmapinfoheader)
					 (i* 256 (sizeof RgbQuad)))))

(defun acl-clim::acl-bitmapinfo (colors width height)
  ;; returns the appropriate bitmapinfo and DIB_XXX_COLORS constant
  (let ((bitcount 8)
	(bmi acl-clim::rgb-bitmapinfo))
    ;; update bmi
    ;;  fixed fields
    (cset bitmapinfo bmi (bmiHeader bisize) (sizeof bitmapinfoheader))
    (cset bitmapinfo bmi (bmiHeader biPlanes) 1)
    (cset bitmapinfo bmi (bmiHeader biCompression) BI_RGB)
    (cset bitmapinfo bmi (bmiHeader biSizeImage) 0)
    (cset bitmapinfo bmi (bmiHeader biClrUsed) 0)
    (cset bitmapinfo bmi (bmiHeader biClrImportant) 0)
    ;;  width + height
    (cset bitmapinfo bmi (bmiHeader biWidth) width)
    (cset bitmapinfo bmi (bmiHeader biHeight) height)
    ;; bitcount
    (cset bitmapinfo bmi (bmiHeader biBitCount) bitcount)
    (cset bitmapinfo bmi (bmiHeader biXPelsPerMeter) 0)
    (cset bitmapinfo bmi (bmiHeader biYPelsPerMeter) 0)
    ;; colors are a vector of RGB ;; create correct colors
    (aclwin:for i aclwin:over-vector colors
	 do
	 (let ((rgb (aref (the vector colors) i)))
	   (multiple-value-bind (cred cgreen cblue)
	       (clim:color-rgb rgb)
	     (setf cred (floor (* 255 cred))
		   cgreen (floor (* 255 cgreen))
		   cblue (floor (* 255 cblue)))
	     (cset bitmapinfo bmi (bmiColors (fixnum i) rgbReserved)
		   0)
	     (cset bitmapinfo bmi (bmiColors (fixnum i) rgbRed)
		   cred)
	     (cset bitmapinfo bmi (bmiColors (fixnum i) rgbBlue)
		   cblue)
	     (cset bitmapinfo bmi (bmiColors (fixnum i) rgbGreen)
		   cgreen))))
    ;; return values
    (values bmi DIB_RGB_COLORS)))

#||
  (let ((result (ct:ccallocate (:void *))))
    (setf (pc::cpointer-value result)))
||#

(defmethod acl-clim::get-texture (device-context pixel-map colors)
   (let ((rpixmap (reflect-pixel-map-in-y pixel-map)) ; slow? use stretchblt?
	 texture-handle)
      (multiple-value-bind (bitmapinfo dib-mode)
          (acl-clim::acl-bitmapinfo colors
				    (array-dimension pixel-map 1)
				    (array-dimension pixel-map 0))
         (setq texture-handle
            (CreateDIBitmap
	      device-context
	      (ct::cref bitmapinfo bitmapinfo (bmiHeader &) :static) 
	      CBM_INIT
#+aclpc	      (acl::%get-pointer rpixmap 4 0)
#+acl86win32       rpixmap
	      bitmapinfo dib-mode))
         texture-handle)))


;;; clipboard support
#|| +++ broken (ct::callocate handle)
(defconstant clphdata (ct::callocate handle))
(defconstant clppdata (ct::callocate (:void *)))


(defun acl-clim::lisp->clipboard (object)
  (let ((*inside-convert-clipboard-from-lisp* t))
    (when (OpenClipboard (null-handle hwnd))
      (unwind-protect
	(add-string-to-clipboard object)
	(CloseClipboard))))) 

(defmethod acl-clim::clipboard->lisp ()
  (when (OpenClipboard (null-handle hwnd))
    (unwind-protect
      (progn
	(GetClipboardData CF_TEXT clphdata)
	(unless (null-handle-p handle clphdata)
	  (GlobalLock clphdata clppdata)  
	  (unless (null-cpointer-p clppdata)
	    (let* ((raw-clpsize (GlobalSize clphdata :static))
		   (clpsize 
		     (if (fixnump raw-clpsize)
		       raw-clpsize
		       most-positive-fixnum))
		   (string (make-string clpsize)))
	      (far-peek string clppdata 0 clpsize)
	      (GlobalUnlock clphdata)
	      (shorten-vector string (strlen string))
	      string))))
      (CloseClipboard))))
||#

;;; about box support

(defun pop-up-about-climap-dialog (frame &rest ignoreargs)
  (pop-up-message-dialog
    *screen*
    (format nil "About ~A" (clim-internals::frame-pretty-name frame))
    (format nil "~%~A~%~%Version: ~A~%~%"
	    (clim-internals::frame-pretty-name frame)
	    (lisp-implementation-version)
	    )
    pc::lisp-icon "OK"))


(in-package :printer)

#+aclpc
(fmakunbound 'set-format-destination)

(defun set-format-destination (destination)
  (cond
    ((null destination) (get-string-stream))
    ((eq destination t) *standard-output*)
    ((stringp destination) 
     (open-stream 'text 'supplied-string :output
       :string destination
       :start (length (the string destination))))
    ((clim-lisp:streamp destination) destination)
    (t 
      (if (output-stream-p destination) destination 
	(error 
	  "destination ~s given for format string ~A is not an output stream"
	  destination *format-string*)))))

(defmethod format2 ((clim-stream clim-lisp:fundamental-stream)
		    control-string format-args)
  (apply #'clim-lisp:format clim-stream control-string format-args))




