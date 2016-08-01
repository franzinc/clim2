;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: USER; Base: 10; Lowercase: Yes -*-


(in-package :win)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

(export '(START-WINDOWS))

;;; Buffer commands (MUST BE KEPT CONSISTENT WITH VMWIN.H!)

;;; Takes a one byte argument and sets the state.  This command is currently
;;; only processed by the windows side
(defconstant *VM-BCOM-SET-STATE* #x01)
(defconstant *VM-STATE-INITIALIZED* #x01)
(defconstant *VM-STATE-INPUT-WAIT* #x02)
(defconstant *VM-STATE-RUN* #x03)
(defconstant *VM-STATE-STOP* #x04)
(defconstant *VM-STATE-BREAK* #x05)

;;; Sends 2 byte wParam from windows message, which is the character code.
;;; When sent to windows, 2 byte character to output
(defconstant *VM-BCOM-CHARACTER* #x02)

;;; Get ready to received menu items.  No args.
(defconstant *VM-BCOM-START-MENU* #x03)

;;; Add a menu item.  Bytes are a null-terminated string.
(defconstant *VM-BCOM-MENU-ITEM* #x04)

;;; Expose and get a selection from the menu
(defconstant *VM-BCOM-MENU-CHOOSE* #x05)

;;; Returned to VM.  The menu item number selected in first byte, second
;;; byte 0 if single or 1 if double click
(defconstant *VM-BCOM-MENU-SELECTION* #x06)

(defconstant *VM-BCOM-MENU-CAPTION* #x07)

;;; Get pathname from user, blocks until response is generated
(defconstant *VM-BCOM-GET-PATHNAME* #x08)

;;; Draws a line, 4 16 bit args are X0, Y0, X1, Y1
(defconstant *VM-BCOM-DRAW-LINE* #x09)

;;; Draws a character at a particular point, args are char, X, Y
(defconstant *VM-BCOM-DRAW-CHAR* #x0A)

;;; Returned to VM: A two-byte short window handle (HWND)
(defconstant *VM-BCOM-WINDOW-VALUE* #x0B)

;;; Returned to VM: A two-byte short device context handle (HDC)
(defconstant *VM-BCOM-DC-VALUE* #x0C)

;;; Returned to VM: A two-byte short handle (HANDLE)
(defconstant *VM-BCOM-HANDLE-VALUE* #x0D)

;;; Returned to VM: A many-byte text metric structure
(defconstant *VM-BCOM-METRIC-VALUE* #x0E)

;;; Returned to VM: An eight-byte rectangle structure
(defconstant *VM-BCOM-RECT-VALUE* #x0F)

;;; Messages below are generic interface functions.
;;; The functions that interpret them are in winfe.c.
(defconstant *VM-BCOM-MOVE-TO* #x10)
(defconstant *VM-BCOM-LINE-TO* #x11)
(defconstant *VM-BCOM-TEXT-OUT* #x12)
(defconstant *VM-BCOM-ELLIPSE* #x13)
(defconstant *VM-BCOM-CREATE-WINDOW* #x14)
(defconstant *VM-BCOM-SHOW-WINDOW* #x15)
(defconstant *VM-BCOM-DESTROY-WINDOW* #x16)
(defconstant *VM-BCOM-SCROLL-DC* #x17)
(defconstant *VM-BCOM-GET-DC* #x18)
(defconstant *VM-BCOM-RELEASE-DC* #x19)
(defconstant *VM-BCOM-ARC* #x1A)
(defconstant *VM-BCOM-CREATE-PEN* #x1B)
(defconstant *VM-BCOM-CREATE-SOLID-BRUSH* #x1C)
(defconstant *VM-BCOM-CREATE-PATTERN-BRUSH* #x1D)
(defconstant *VM-BCOM-SET-BRUSH-ORG* #x1E)
(defconstant *VM-BCOM-GET-STOCK-OBJECT* #x1F)
(defconstant *VM-BCOM-SELECT-OBJECT* #x20)
(defconstant *VM-BCOM-GET-TERM-WINDOW* #x21)
(defconstant *VM-BCOM-BEGIN-PAINT* #x22)
(defconstant *VM-BCOM-END-PAINT* #x23)
(defconstant *VM-BCOM-STRING-ARG* #x24)
(defconstant *VM-BCOM-PIE* #x25)
(defconstant *VM-BCOM-RECTANGLE* #x26)
(defconstant *VM-BCOM-DELETE-OBJECT* #x27)
(defconstant *VM-BCOM-GET-ROP2* #x28)
(defconstant *VM-BCOM-SET-ROP2* #x29)
(defconstant *VM-BCOM-POLYGON* #x2A)
(defconstant *VM-BCOM-VANILLA-EVENT* #x2B)
(defconstant *VM-BCOM-SET-FOCUS* #x2C)
(defconstant *VM-BCOM-MOVE-WINDOW* #x2D)
(defconstant *VM-BCOM-CREATE-BITMAP* #x2E)
(defconstant *VM-BCOM-CREATE-FONT* #x2F)
(defconstant *VM-BCOM-GET-TEXT-METRICS* #x30)
(defconstant *VM-BCOM-WINDOW-FROM-POINT* #x31)
(defconstant *VM-BCOM-GET-UPDATE-RECT* #x32)
(defconstant *VM-BCOM-PAINT-EVENT* #x33)
(defconstant *VM-BCOM-SELECT-BRUSH* #x34)
(defconstant *VM-BCOM-SELECT-PEN* #x35)
(defconstant *VM-BCOM-SELECT-BITMAP* #x36)
(defconstant *VM-BCOM-SELECT-FONT* #x37)
(defconstant *VM-BCOM-SELECT-REGION* #x38)
(defconstant *VM-BCOM-SET-TEXT-COLOR* #x39)
(defconstant *VM-BCOM-BEEP* #x3A)
(defconstant *VM-BCOM-SET-SCROLL-POSITION* #x3B)
(defconstant *VM-BCOM-SET-BACKGROUND-COLOR* #x3C)
(defconstant *VM-BCOM-GET-CLIENT-RECTANGLE* #x3D)
(defconstant *VM-BCOM-GET-TEXT-EXTENT* #x3E)
(defconstant *VM-BCOM-CREATE-FONT-INDIRECT* #x3F)
(defconstant *VM-BCOM-SET-ERASE-BRUSH* #x40)
(defconstant *VM-BCOM-GET-SCREEN-SIZE* #x41)
(defconstant *VM-BCOM-SCREEN-SIZE* #x42)
(defconstant *VM-BCOM-DRAW-BITMAP* #x43)
(defconstant *VM-BCOM-SET-POINTER-POS* #x44)
(defconstant *VM-BCOM-GET-POINTER-POS* #x45)
(defconstant *VM-BCOM-POINTER-POSITION* #x45)
(defconstant *VM-BCOM-READ-BITMAP* #x46)
(defconstant *VM-BCOM-LOADCURSOR* #x47)
(defconstant *VM-BCOM-SET-MOUSE-CURSOR* #x48)
(defconstant *VM-BCOM-SET-WINDOW-TEXT* #x49)
(defconstant *VM-BCOM-SET-WINDOW-POS* #x4A)
(defconstant *VM-BCOM-IS-ICONIC* #x4B)
(defconstant *VM-BCOM-GET-WINDOW-RECTANGLE* #x4C)
(defconstant *VM-BCOM-SHOW-SCROLL-BAR* #x4D)
(defconstant *VM-BCOM-POLYLINE* #x4E)
(defconstant *VM-BCOM-NOTIFY-USER* #x4F)
(defconstant *VM-BCOM-IS-WINDOW-VISIBLE* #x50)
(defconstant *VM-BCOM-WINDOW-EXEC* #x51)
(defconstant *VM-BCOM-CHANGE-DIR* #x52)
(defconstant *VM-BCOM-GET-FOCUS* #x53)
(defconstant *VM-BCOM-SHOW-CARET* #x54)
(defconstant *VM-BCOM-SET-CARET-POS* #x55)
(defconstant *VM-BCOM-HIDE-CARET* #x56)
(defconstant *VM-BCOM-LOAD-BITMAP* #x57)
(defconstant *VM-BCOM-NONCLIENT-PAINT* #x59)

;;; Unique ID: sets the unique ID for the next command that requests a
;;; return value, or the return unique ID.  A UID is one byte.
(defconstant *VM-BCOM-UID* #xFC)

;;; Special long message operator
(defconstant *VM-BCOM-LONG-MESSAGE* #xFD)

;;; General cancellation
(defconstant *VM-BCOM-CANCEL* #xFE)

;;; No message available now
(defconstant *VM-BCOM-NULL* #xFF)


(defvar *windows-enabled* nil)
(defvar *stream*)
(defvar *old-terminal-io*)
(defvar *win2-com* nil)
(defvar *win3-com* nil)
(defvar *winfe-exe* "winfe.exe") ; "\\clim\\winfe.exe"
(defvar *win-com-file* "WIN.COM")
(defvar *win-cloe-file* "CLOEWIN.COM")

(defvar *windows-standard-mode* t)
(defvar *standard-stub* "STDSTUB.EXE")

(defvar *win-3-binary* '#x(EB 09 58 57 00 00 00 00
			   00 00 00 E8 71 06 BB 3C))

;;; Patches to WIN.COM V3.  Must be in numerical order by offset.
(defvar *win-3-patches* '#x((8C4 5D 00) (8E6 40 01)))

;;; Find the windows .COM file in the specified directory.  If it's
;;; a Windows 3 .COM and not converted for CLOE, do the conversion.
(defun find-windows-com-1 (dir)
  (setf dir (concatenate 'string dir "\\"))
  (let ((cloe-pn (merge-pathnames dir *win-cloe-file*))
        (win-pn (merge-pathnames dir *win-com-file*)))
    ;; First try for the converted file
    (when (probe-file cloe-pn) 
      (return-from find-windows-com-1
	(values (namestring cloe-pn) t t)))
    (unless (probe-file win-pn)
      (return-from find-windows-com-1 nil))
    (with-open-file (win-com win-pn :direction :input 
    				    :element-type '(unsigned-byte 8))
      (when win-com
        (let ((first-bytes (loop repeat 16 collect (read-byte win-com))))
	  (cond ((not (equal first-bytes *win-3-binary*))
	  	 (return-from find-windows-com-1 
		   (namestring win-pn) nil nil))
		(t
		 ;; Have a windows 3 .COM, patch it
		 (with-open-file (cloe-com cloe-pn :direction :output
		 	          :element-type '(unsigned-byte 8))
		   (dolist (n first-bytes) (write-byte n cloe-com))
		   (format t "~&Creating ~A..." (namestring cloe-pn))
		   (loop with patches = *win-3-patches*
		   	 for n = (read-byte win-com nil nil)
		         for idx from 16
		         until (null n)
			 when (neq idx (first (first patches)))
			 do (write-byte n cloe-com)
			 else 
			 do (when (not (= n (second (first patches))))
			      (error "Value to be patched at ~X is wrong: is ~X, expected ~X"
			      	     idx n (second (first patches))))
			    (write-byte (third (first patches)) cloe-com)
			    (pop patches)))
    		 (format t "done~%")
                 (values (namestring cloe-pn) t t))))))))

;;; Find the windows WIN.COM file by searching PATH
(defun find-windows-com ()
  (cond (*win2-com* (values *win-com* nil nil))
        (*win3-com* (values *win3-com* t nil))
        (t
	 (flet ((check (dir)
		  (declare (sys:downward-function))
		  (multiple-value-bind (com win3-p cloe-p)
		      (find-windows-com-1 dir)
		    (when com
		      (return-from find-windows-com
			(values com win3-p cloe-p))))))
	   (check ".")
	   (map-over-path #'check)
	   (check "\\WINDOWS")
	   (error "Cannot find windows .COM file")))))

(defun find-winfe-exe ()
  (cond ((position #\\ *winfe-exe*)
	 (let ((path (merge-pathnames *winfe-exe*)))
	   (when (probe-file path)
	     (return-from find-winfe-exe (namestring path)))))
	(t
	 (map-over-path
	   #'(lambda (dir)
	       (declare (sys:downward-function))
	       (let ((path (merge-pathnames (concatenate 'string dir "\\" *winfe-exe*))))
		 (when (probe-file path)
		   (return-from find-winfe-exe (namestring path))))))))
  (error "Cannot find the Winfe.exe file: ~A" *winfe-exe*))

;;note, this always causes 'unbalanced paren' error in zmacs, pay no mind
(defun map-over-path (continuation)
  (let ((path (sys::getenv "PATH")))
    (loop for oidx = 0 then (1+ idx)
	  for idx = (position #\; path :start oidx)
	  for dir = (subseq path oidx idx)
	  do (funcall continuation dir)
	  while idx)))

(defvar *command-string-in-use* nil)
(defvar *command-string* (make-array 260 :element-type 'string-char))

(defmacro with-fast-command ((command code length &key reply) &body body)
  (when (null command) (setf command '.command.))
  `(multiple-value-prog1
     (let ((,command *command-string*)
	   (*command-string-in-use* t))
       (setf (schar ,command 0) (code-char ,code))
       (setf (schar ,command 1) (code-char ,length))
       ,@body
       ,(if reply
	    `(send-command-await-response nil ,command)
	    `(send-command-string nil ,command)))
     (exit-gc-cursor)))

(defvar *gc-cursor*)
(defvar *old-cursor* nil)

(defun enter-gc-cursor (&rest ignore)
  (when (and (not *command-string-in-use*)
	     (not *processing-vanilla-event*)
	     (null *old-cursor*))
    (setq *old-cursor* (set-mouse-cursor *gc-cursor*))))

(defun exit-gc-cursor (&rest ignore)
  (when (and (not *command-string-in-use*)
	     (not *processing-vanilla-event*)
	     *old-cursor*)
    (set-mouse-cursor (shiftf *old-cursor* nil))))

(defun si::*gc-start-hook* (x)
  (funcall si::*gc-start-hook* x))

(defun si::*gc-stop-hook* (x)
  (funcall si::*gc-stop-hook* x))

(defun replace-com-string (com)
   (concatenate 'string (subseq com 0 (1+ (position #\\ com :from-end t)))
                *standard-stub*))


(defun start-windows ()
  (when *windows-enabled*
    (error "Windows is already started."))
  (multiple-value-bind (com win3-p) (find-windows-com)
    (when *windows-standard-mode*
          (setf com (replace-com-string com)))
    (let ((command-string (format nil "  ~:[~;/R ~]~A" win3-p 
				  (find-winfe-exe)))
	  (startup-directory (si::getcwd)))
      (format t "~&Starting WINDOWS ~:[2~;3~]: ~A~A~%" win3-p com command-string)
      (setf (aref command-string 0) (code-char (1- (length command-string))))
      
      (si::win%open com command-string)
      ;;restore the startup directory, which gets munged by standard mode
      (change-directory startup-directory)))
  (setq *old-terminal-io* *terminal-io*)
  (setq *stream* (si::make-encapsulated-primitive-stream lisp::*windows-io*))
  (setq *terminal-io* *stream*
        rh::*raw-outstream* *stream*
        rh::*stream* *stream*)
  (setq *windows-enabled* t)
  (setq *gc-cursor* (loadcursor IDC_WAIT))
  (setq si::*gc-start-hook* 'enter-gc-cursor
        si::*gc-stop-hook* 'exit-gc-cursor)
  t)

(defun stop-windows ()
  (setq si::*gc-start-hook* nil
        si::*gc-stop-hook* nil)
  (setq *windows-enabled* nil)
  (setq *terminal-io* *old-terminal-io*
        rh::*raw-outstream* *old-terminal-io*
        rh::*stream* *old-terminal-io*)
  (with-fast-command (command *vm-bcom-set-state* 1)
    (setf (schar command 2) (code-char *VM-STATE-STOP*)))
  (loop doing (si::win%idle)))


(defvar *uid-string* (make-array 3 :element-type 'string-char))

(defun send-command-string (code string &optional (zero-term nil) (uid nil))
  ;; First, process any events that might be pending
  (await-response -1 nil nil)
  (when uid
    (setf (schar *uid-string* 0) (code-char *VM-BCOM-UID*))
    (setf (schar *uid-string* 1) (code-char 1))
    (setf (schar *uid-string* 2) (code-char uid))
    (si::win%send *uid-string*))
  (cond ((null code)
	 ;; Superfast case
	 (si::win%send string))
	(t
	 ;; Need to copy
	 (let ((string-remaining (length string))
	       (*command-string-in-use* t))
	   (declare (fixnum string-remaining))
	   (cond ((< string-remaining 255)
		  ;; Fast case
		  (dotimes (i string-remaining)
		    (declare (fixnum i))
		    (setf (schar *command-string* (+ i 2)) (aref string i)))
		  (setf (schar *command-string* 0) (code-char code))
		  (cond (zero-term
			 (setf (schar *command-string* 1) (code-char (1+ string-remaining)))
			 (setf (schar *command-string* (+ string-remaining 2)) (code-char 0)))
			(t
			 (setf (schar *command-string* 1) (code-char string-remaining))))
		  (si::win%send *command-string*))
		 (t
		  (loop with string-idx = 0
			with idx
			for first-time = t then nil
			for string-remaining = (length string) then (- string-remaining 255)
			for this-len = (max 0 (min 255 string-remaining))
			while (or first-time (> string-remaining 0) zero-term)
			do (setq idx 2)
			   (dotimes (i this-len)
			     (setf (schar *command-string* idx) (aref string string-idx))
			     (incf idx)
			     (incf string-idx))
			   (when (and (< this-len 255) zero-term)
			     ;; Need zero termination and have room
			     (setf (schar *command-string* idx) (code-char 0))
			     (setq zero-term nil)
			     (incf this-len))
			   (setf (schar *command-string* 1) (code-char this-len))
			   (setf (schar *command-string* 0)
				 (if (and (<= string-remaining 255) (not zero-term))
				     (code-char code)
				     (code-char *VM-BCOM-LONG-MESSAGE*)))
			   (si::win%send *command-string*)))))
	 (exit-gc-cursor))))

(defconstant *bcom-max-uid* 16)
(defvar *bcom-uid-array* (make-array *bcom-max-uid*))
(defvar *bcom-uid* 0)

(defun await-response (uid wait-p event-p)
    (loop with code and length and args and ruid and res
          do (multiple-value-setq (code length args)
                 (receive-command-string wait-p t))
             (cond ((or (null code) (eq code T))
	            ;; An event or no wait; if a response for us came back
		    ;; somewhere else, return it.  Else, if an event and want
		    ;; to wake up, return, else loop.  Otherwise, wait flag
		    ;; must be false, just return NIL.
 		    (cond ((and (>= uid 0)
		    	        (setq res (aref *bcom-uid-array* uid)))
		           (return (values-list res)))
			  (t (return nil))))
	           ((/= code *VM-BCOM-UID*)
	            ;; Didn't get a UID back!  Hope this is the right response
		    ;; and just return it.
		    (return (values code length args)))
	           ((= (setq ruid (char-code (aref args 0))) uid)
		    ;; The next result is for us.  WINFE guarantees that there
		    ;; will be no intervening responses between the UID and
		    ;; the real response.
		    (return (receive-command-string t nil)))
		   (t ;; This result is for someone else
                    (multiple-value-setq (code length args) 
		      (receive-command-string t nil))
		    (setf (aref *bcom-uid-array* ruid)
		          (list code length (copy-seq args)))))))

(defun send-command-await-response (code string &optional (zero-term nil))
  (let ((uid (setq *bcom-uid* (mod (1+ *bcom-uid*) *bcom-max-uid*))))
    (setf (aref *bcom-uid-array* uid) nil)
    (send-command-string code string zero-term uid)
    (await-response uid t nil)))

(defvar *response-string* (make-array 260 :element-type 'string-char))
(defvar *args-portion* (make-array (- (length *response-string*) 2)
                                   :displaced-to *response-string*
				   :displaced-index-offset 2
                                   :element-type 'string-char))

(defvar *processing-vanilla-event* nil)

(defun receive-command-string (&optional (wait-p t) event-p)
  (when *processing-vanilla-event*
    (error "Can't send commands from the event handler."))
  (let ((events nil)
	(*processing-vanilla-event* t))
    (loop 
      (unless (si::win%receive *response-string* wait-p)
	(return events))
      (let ((code (char-code (schar *response-string* 0)))
	    (length (char-code (schar *response-string* 1)))
	    (args *args-portion*))
	(declare (fixnum code length))
	(unless (or (= code *VM-BCOM-VANILLA-EVENT*)
		    (= code *VM-BCOM-PAINT-EVENT*))
	  (return (values code length args)))
	(vanilla-event code length args)
	(when event-p
	  (setf events t)
	  (setf wait-p nil))))))

;;make it so we don't have to load cloe-eve after reloading this.
(unless (fboundp 'vanilla-event)
  (defun vanilla-event (code length args)
    (declare (ignore code length args))
    nil))

(defmacro get-16bit (string index)
  `(let ((.low. (char-code (aref ,string ,index)))
         (.high. (char-code (aref ,string (+ ,index 1)))))
     (declare (fixnum .high. .low.))
     (logior .low. (ash  .high. 8))))

(defun menu-choose (caption &rest items)
  (if *windows-enabled*
      (menu-choose-windows caption items)
      (menu-choose-dumb caption items)))

(defun menu-choose-dumb (caption items)
  (setq caption (string caption))
  (format t "~&~A~%" caption)
  (if (null items)
      nil
      (let ((count 0))
        (dolist (item items)
	  (format t "~D: ~A~%" (incf count) item))
	(format t "Choice: ")
	(loop for response = (read)
	      when (and (typep response 'fixnum) 
	      		(> response 0) 
			(<= response count))
              return (values (nth (- response 1) items)
                             (- response 1) 0)
	      do (format t "~&Please try again: ")))))

(defun menu-choose-windows (caption items)
  (send-command-string *VM-BCOM-START-MENU* "")
  (when caption
    (send-command-string *VM-BCOM-MENU-CAPTION* (string caption) t))
  (dolist (i items)
    (send-command-string *VM-BCOM-MENU-ITEM* 
      (format nil "  ~A  " (string i)) t))
  (multiple-value-bind (command l cstring)
      (send-command-await-response *VM-BCOM-MENU-CHOOSE* "")
    (cond ((= command *VM-BCOM-MENU-SELECTION*)
           (values (nth (setq l (char-code (aref cstring 0))) items)
                   l
		   (char-code (aref cstring 1))))
          (t ;(= command *VM-BCOM-CANCEL*)
            nil))))


(defun get-pathname (&optional (default "*.*"))
  (if *windows-enabled*
      (get-pathname-windows (win::get-focus) :default default)
      (get-pathname-dumb default)))

(defun get-pathname-dumb (default)
  (format t "~&Enter a filename (default ~A): " default)
  (merge-pathnames (parse-namestring (read-line))
                   (parse-namestring default))
  )

(defun get-pathname-windows (window &key (default "*.*") 
				    (caption "File Choose"))
  (store-string-arg default)
  (store-string-arg caption)
  (multiple-value-bind (command l cstring)
    (with-fast-command (*window* *VM-BCOM-GET-PATHNAME* 2 :reply t)
      (store-16bit *window* 2 window))
    (if (= command *VM-BCOM-GET-PATHNAME*)
	(subseq cstring 0 l)
	;(get-16bit cstring 0)
	nil)))

(defmacro store-16bit (string index value)
  (typecase value
    ((or integer symbol)
     `(progn
	(setf (schar ,string ,index) (code-char (logand ,value #x0FF)))
	(setf (schar ,string (+ ,index 1)) (code-char (logand (ash ,value -8) #x0FF)))))
    (otherwise
      `(let ((.value. ,value))
	 (declare (fixnum .value.))
	 (store-16bit ,string ,index .value.)))))

;;;*** Anti-bignum kludge.  Take a 28-bit fixnum, and store it so
;;;*** the C side will see it as a 32-bit fixnum with the four low
;;;*** bits 0.  This is useful for create-window flags and for
;;;*** RGB values, neither of which use the low bits.
(defmacro store-28bit (string index value)
  `(let ((.low. (ash (logand ,value #x0FFF) 4))
         (.high. (logand (ash ,value -12) #x0FFFF)))
     (declare (fixnum .high. .low.))
     (store-16bit ,string ,index .low.)
     (store-16bit ,string (+ ,index 2) .high.)))

(defmacro store-24bit (string index value)
  `(let ((.low. (logand ,value #x0FFFF))
         (.high. (logand (ash ,value -16) #x00FF)))
      (declare (fixnum .high. .low.))
    (store-16bit ,string ,index .low.)
    (store-16bit ,string (+ ,index 2) .high.)))

(defun store-string-arg (string)
  (send-command-string *VM-BCOM-STRING-ARG* string t))

(defun draw-line (x0 y0 x1 y1)
  (declare (fixnum x0 y0 x1 y1))
  (with-fast-command (*line* *VM-BCOM-DRAW-LINE* 8)
    (store-16bit *line* 2 x0)
    (store-16bit *line* 4 y0)
    (store-16bit *line* 6 x1)
    (store-16bit *line* 8 y1)))

(defun draw-char (char x y)
  (declare (fixnum x y))
  (with-fast-command (*char* *VM-BCOM-DRAW-CHAR* 5)
    (setf (schar *char* 2) char)
    (store-16bit *char* 3 x)
    (store-16bit *char* 5 y)))

;;; Generic windows functions

(defun get-term-window ()
  (multiple-value-bind (command l cstring)
      (with-fast-command (nil *VM-BCOM-GET-TERM-WINDOW* 0 :reply t))
    (if (= command *VM-BCOM-WINDOW-VALUE*)
	(get-16bit cstring 0)
        nil)))

(defun get-dc (window)
  (declare (fixnum window))
  (multiple-value-bind (command l cstring)
      (with-fast-command (*window* *VM-BCOM-GET-DC* 2 :reply t)
	(store-16bit *window* 2 window))
    (if (= command *VM-BCOM-DC-VALUE*)
	(get-16bit cstring 0)
	nil)))

(defun release-dc (window dc)
  (declare (fixnum window dc))
  (with-fast-command (*release* *VM-BCOM-RELEASE-DC* 4)
    (store-16bit *release* 2 window)
    (store-16bit *release* 4 dc)))

(defun begin-paint (window)
  (declare (fixnum window))
  (multiple-value-bind (command l cstring)
      (with-fast-command (*window* *VM-BCOM-BEGIN-PAINT* 2 :reply t)
	(store-16bit *window* 2 window))
    (if (= command *VM-BCOM-DC-VALUE*)
	(get-16bit cstring 0)
        nil)))

(defun end-paint (window)
  (declare (fixnum window))
  (with-fast-command (*window* *VM-BCOM-END-PAINT* 2)
    (store-16bit *window* 2 window)))

(defun create-window (cname wname style x y w h parent menu instance param)
  (declare (fixnum style x y w h parent menu instance))
  (store-string-arg cname)	/* ClassName */
  (store-string-arg wname)	/* WindowName */
  (store-string-arg param)
  (multiple-value-bind (command l cstring)
      (with-fast-command (*cwindow* *VM-BCOM-CREATE-WINDOW* 18 :reply t)
	(store-28bit *cwindow* 2 style)
	(store-16bit *cwindow* 6 x)
	(store-16bit *cwindow* 8 y)
	(store-16bit *cwindow* 10 w)
	(store-16bit *cwindow* 12 h)
	(store-16bit *cwindow* 14 parent)
	(store-16bit *cwindow* 16 menu)
	(store-16bit *cwindow* 18 instance))
    (if (= command *VM-BCOM-WINDOW-VALUE*)
	(get-16bit cstring 0)
        nil)))

(defun move-window (window x y w h repaint)
  (declare (fixnum window x y w h))
  (with-fast-command (*mwindow* *VM-BCOM-MOVE-WINDOW* 12)
    (store-16bit *mwindow* 2 window)
    (store-16bit *mwindow* 4 x)
    (store-16bit *mwindow* 6 y)
    (store-16bit *mwindow* 8 w)
    (store-16bit *mwindow* 10 h)
    (store-16bit *mwindow* 12 (if repaint 1 0))))

(defun move-to (dc x y)
  (declare (fixnum dc x y))
  (with-fast-command (*dcxy* *VM-BCOM-MOVE-TO* 6)
    (store-16bit *dcxy* 2 dc)
    (store-16bit *dcxy* 4 x)
    (store-16bit *dcxy* 6 y)))

(defun line-to (dc x y)
  (declare (fixnum dc x y))
  (with-fast-command (*dcxy* *VM-BCOM-LINE-TO* 6)
    (store-16bit *dcxy* 2 dc)
    (store-16bit *dcxy* 4 x)
    (store-16bit *dcxy* 6 y)))

(defun text-out (dc x y string &aux (slen (length string)))
  (declare (fixnum dc x y slen))
  (store-string-arg string)
  (with-fast-command (*text-out* *VM-BCOM-TEXT-OUT* 8)
    (store-16bit *text-out* 2 dc)
    (store-16bit *text-out* 4 x)
    (store-16bit *text-out* 6 y)
    (store-16bit *text-out* 8 slen)))

(defun ellipse (dc xLeft yTop xRight yBottom)
  (declare (fixnum dc xLeft yTop xRight yBottom))
  (with-fast-command (*ellipse* *VM-BCOM-ELLIPSE* 10)
    (store-16bit *ellipse* 2 dc)
    (store-16bit *ellipse* 4 xLeft)
    (store-16bit *ellipse* 6 yTop)
    (store-16bit *ellipse* 8 xRight)
    (store-16bit *ellipse* 10 yBottom)))

(defun rectangle (dc xLeft yTop xRight yBottom)
  (declare (fixnum dc xLeft yTop xRight yBottom))
  (incf xRight)
  (incf yBottom)
  (with-fast-command (*rectangle* *VM-BCOM-RECTANGLE* 10)
    (store-16bit *rectangle* 2 dc)
    (store-16bit *rectangle* 4 xLeft)
    (store-16bit *rectangle* 6 yTop)
    (store-16bit *rectangle* 8 xRight)
    (store-16bit *rectangle* 10 yBottom)))

(defun show-window (window &key (type :show-maximized))
  (declare (fixnum window))
  (let ((command (ecase type
		   (:hide sw_hide)
		   (:minimize sw_minimize)
		   (:show-normal sw_shownormal) (:restore sw_restore)
		   (:show sw_show)
		   (:show-maximized sw_showmaximized)
		   (:show-minimized sw_showminimized)
		   (:show-minimized-no-activate sw_showminnoactive)
		   (:show-na sw_showna)
		   (:show-no-activate sw_shownoactivate))))
    (with-fast-command (*show-window* *VM-BCOM-SHOW-WINDOW* 4)
      (store-16bit *show-window* 2 window)
      (store-16bit *show-window* 4 command))))

(defun destroy-window (window)
  (declare (fixnum window))
  (with-fast-command (*window* *VM-BCOM-DESTROY-WINDOW* 2)
    (store-16bit *window* 2 window)))

(defun set-focus (window)
  (declare (fixnum window))
  (multiple-value-bind (command l cstring)
      (with-fast-command (*window* *VM-BCOM-SET-FOCUS* 2 :reply t)
	(store-16bit *window* 2 window))
    (if (= command *VM-BCOM-WINDOW-VALUE*)
	(get-16bit cstring 0)
        nil)))

(defun get-focus ()
  (multiple-value-bind (command l cstring)
      (with-fast-command (nil *VM-BCOM-GET-FOCUS* 0 :reply t))
    (if (= command *VM-BCOM-WINDOW-VALUE*)
	(get-16bit cstring 0)
        nil)))

(defun get-update-rect (window erasep)
  (declare (fixnum window))
  (multiple-value-bind (command l cstring)
      (with-fast-command (*selobject* *VM-BCOM-GET-UPDATE-RECT* 4 :reply t)
	(store-16bit *selobject* 2 window)
	(store-16bit *selobject* 4 (if erasep 1 0)))
    (if (= command *VM-BCOM-RECT-VALUE*)
	(values (get-16bit cstring 0)		; left
		(get-16bit cstring 2)		; top
		(get-16bit cstring 4)		; bottom
		(get-16bit cstring 6)		; right
		)
	nil)))

(defun get-client-rectangle (window)
  (declare (fixnum window))
  (multiple-value-bind (command l cstring)
      (with-fast-command (*window* *VM-BCOM-GET-CLIENT-RECTANGLE* 2 :reply t)
	(store-16bit *window* 2 window))
    (if (= command *VM-BCOM-RECT-VALUE*)
	(values  (get-16bit cstring 0)		; left
		 (get-16bit cstring 2)		; top
		 (get-16bit cstring 4)		; bottom
		 (get-16bit cstring 6)		; right
		 )
        nil)))

(defun window-from-point (x y)
  (declare (fixnum x y))
  (multiple-value-bind (command l cstring)
      (with-fast-command (*selobject* *VM-BCOM-WINDOW-FROM-POINT* 4 :reply t)
	(store-16bit *selobject* 2 x)
	(store-16bit *selobject* 4 y))
    (if (= command *VM-BCOM-WINDOW-VALUE*)
	(get-16bit cstring 0)
	nil)))

(defun scroll-dc (dc x y l1 t1 r1 b1 l2 t2 r2 b2)
  (declare (fixnum dc x y l1 t1 r1 b1 l2 t2 r2 b2))
  (with-fast-command (*scroll* *VM-BCOM-SCROLL-DC* 22)
    (store-16bit *scroll* 2 dc)
    (store-16bit *scroll* 4 x)
    (store-16bit *scroll* 6 y)
    (store-16bit *scroll* 8 l1)
    (store-16bit *scroll* 10 t1)
    (store-16bit *scroll* 12 r1)
    (store-16bit *scroll* 14 b1)
    (store-16bit *scroll* 16 l2)
    (store-16bit *scroll* 18 t2)
    (store-16bit *scroll* 20 r2)
    (store-16bit *scroll* 22 b2)))

(defun arc (dc x1 y1 x2 y2 x3 y3 x4 y4)
  (declare (fixnum dc x1 y1 x2 y2 x3 y3 x4 y4))
  (with-fast-command (*arc* *VM-BCOM-ARC* 18)
    (store-16bit *arc* 2 dc)
    (store-16bit *arc* 4 x1)
    (store-16bit *arc* 6 y1)
    (store-16bit *arc* 8 x2)
    (store-16bit *arc* 10 y2)
    (store-16bit *arc* 12 x3)
    (store-16bit *arc* 14 y3)
    (store-16bit *arc* 16 x4)
    (store-16bit *arc* 18 y4)))

(defun pie (dc x1 y1 x2 y2 x3 y3 x4 y4)
  (declare (fixnum dc x1 y1 x2 y2 x3 y3 x4 y4))
  (with-fast-command (*arc* *VM-BCOM-PIE* 18)
    (store-16bit *arc* 2 dc)
    (store-16bit *arc* 4 x1)
    (store-16bit *arc* 6 y1)
    (store-16bit *arc* 8 x2)
    (store-16bit *arc* 10 y2)
    (store-16bit *arc* 12 x3)
    (store-16bit *arc* 14 y3)
    (store-16bit *arc* 16 x4)
    (store-16bit *arc* 18 y4)))

(defvar *polygon* (make-array 1 :element-type 'string-char :fill-pointer 0))

(defun polygon (dc points)
  (declare (fixnum dc))
  (let* ((len (length points))
	 (alen (+ (* 2 len) 4)))
    (declare (fixnum len alen))
    (when (oddp len) (error "Polygons must have even number of points"))
    (cond ((< alen 255)
	   (with-fast-command (command *VM-BCOM-POLYGON* alen)
	     (store-16bit command 2 dc)
	     (store-16bit command 4 (the fixnum (truncate len 2)))
	     (let ((i 0)
		   (ind 6))
	       (declare (fixnum i ind))
	       (loop
		 (when (null list) (return))
		 (store-16bit command ind (aref points i))
		 (incf ind 2) (incf i)
		 (store-16bit command ind (aref points i))
		 (incf ind 2) (incf i)))))
	  (t
	   (when (< (array-total-size *polygon*) alen)
	     (setf *polygon* (make-array alen :element-type 'string-char :fill-pointer 0)))
	   (setf (fill-pointer *polygon*) alen)
	   (store-16bit *polygon* 0 dc)
	   (store-16bit *polygon* 2 (the fixnum (truncate len 2)))
	   (let ((i 0)
		 (ind 4))
	     (declare (fixnum i ind))
	     (loop
	       (when (null list) (return))
	       (store-16bit *polygon* ind (aref points i))
	       (incf ind 2) (incf i)
	       (store-16bit *polygon* ind (aref points i))
	       (incf ind 2) (incf i)))
	   (send-command-string *VM-BCOM-POLYGON* *polygon*)))))

(defun polyline (dc points &optional closed)
  (declare (fixnum dc))
  (let* ((len (+ (length points) (if closed 2 0)))
	 (alen (+ (* 2 len) 4)))
    (declare (fixnum len alen))
    (when (oddp len) (error "Polygons must have even number of points"))
    (cond ((< alen 255)
	   (with-fast-command (command *VM-BCOM-POLYLINE* alen)
	     (store-16bit command 2 dc)
	     (store-16bit command 4 (the fixnum (truncate len 2)))
	     (let ((i 0)
		   (ind 6))
	       (declare (fixnum i ind))
	       (loop
		 (when (null list) (return))
		 (store-16bit command ind (aref points i))
		 (incf ind 2) (incf i)
		 (store-16bit command ind (aref points i))
		 (incf ind 2) (incf i))
	       (when closed
		 (store-16bit command ind (aref points 0))
		 (incf ind 2)
		 (store-16bit command ind (aref points 1))
		 (incf ind 2)))))
	  (t
	   (when (< (array-total-size *polygon*) alen)
	     (setf *polygon* (make-array alen :element-type 'string-char :fill-pointer 0)))
	   (setf (fill-pointer *polygon*) alen)
	   (store-16bit *polygon* 0 dc)
	   (store-16bit *polygon* 2 (the fixnum (truncate len 2)))
	   (let ((list list-of-points)
		 (ind 4))
	     (declare (fixnum ind))
	     (loop
	       (when (null list) (return))
	       (store-16bit *polygon* ind (aref points i))
	       (incf ind 2) (incf i)
	       (store-16bit *polygon* ind (aref points i))
	       (incf ind 2) (incf i))
	     (when closed
	       (store-16bit *polygon* ind (aref points 0))
	       (incf ind 2)
	       (store-16bit *polygon* ind (aref points 1))
	       (incf ind 2)))
	   (send-command-string *VM-BCOM-POLYLINE* *polygon*)))))

(defun create-pen (style width color)
  (declare (fixnum style width color))
  (multiple-value-bind (command l cstring)
      (with-fast-command (*create-pen* *VM-BCOM-CREATE-PEN* 8 :reply t)
	(store-16bit *create-pen* 2 style)
	(store-16bit *create-pen* 4 width)
	(store-24bit *create-pen* 6 color))
    (if (= command *VM-BCOM-HANDLE-VALUE*)
	(get-16bit cstring 0)
        nil)))

(defun create-font (height width escapement orientation weight
		    italic underline strikeout charset outputprecision
		    clipprecision quality pitchandfamily facename)
  (declare (fixnum height width escapement orientation weight
		   italic underline strikeout charset outputprecision
		   clipprecision quality pitchandfamily))
  (store-string-arg facename)
  (multiple-value-bind (command l cstring)
      (with-fast-command (*create-font* *VM-BCOM-CREATE-FONT* 26 :reply t)
	(store-16bit *create-font* 2 height)
	(store-16bit *create-font* 4 width)
	(store-16bit *create-font* 6 escapement)
	(store-16bit *create-font* 8 orientation)
	(store-16bit *create-font* 10 weight)
	(store-16bit *create-font* 12 italic)
	(store-16bit *create-font* 14 underline)
	(store-16bit *create-font* 16 strikeout)
	(store-16bit *create-font* 18 charset)
	(store-16bit *create-font* 20 outputprecision)
	(store-16bit *create-font* 22 clipprecision)
	(store-16bit *create-font* 24 quality)
	(store-16bit *create-font* 26 pitchandfamily))
    (if (= command *VM-BCOM-HANDLE-VALUE*)
	(get-16bit cstring 0)
	nil)))

(defun get-text-metrics (dc)
  (declare (fixnum dc))
  (multiple-value-bind (command l cstring)
      (with-fast-command (*dc* *VM-BCOM-GET-TEXT-METRICS* 2 :reply t)
	(store-16bit *dc* 2 dc))
    (if (= command *VM-BCOM-METRIC-VALUE*)
        (values (get-16bit cstring 0)		;height
		(get-16bit cstring 2)		;ascent
		(get-16bit cstring 4)		; descent
		(get-16bit cstring 6)		; internal leading
		(get-16bit cstring 8)		; external leading
		(get-16bit cstring 10)		; average char width
		(get-16bit cstring 12)		; max char width
		(get-16bit cstring 14)		; weight
		(char-code (char cstring 16))	;italic
		(char-code (char cstring 17))	;underlined
		(char-code (char cstring 18))	;struckout
		(char-code (char cstring 19))	;first char
		(char-code (char cstring 20))	;last char
		(char-code (char cstring 21))	;default char
		(char-code (char cstring 22))	;break char
		(char-code (char cstring 23))	;pitch and family
		(char-code (char cstring 24))	;char set
		(get-16bit cstring 25)		;overhang
		(get-16bit cstring 27)		;digitized aspect x
		(get-16bit cstring 29)		;digitized aspect y
		)
	nil)))

(defun create-solid-brush (color)
  (declare (fixnum color))
  (multiple-value-bind (command l cstring)
      (with-fast-command (*create-solid-brush* *VM-BCOM-CREATE-SOLID-BRUSH* 4 :reply t)
	(store-24bit *create-solid-brush* 2 color))
    (if (= command *VM-BCOM-HANDLE-VALUE*)
	(get-16bit cstring 0)
        nil)))

(defvar *bitmap* (make-array (+ (* 2 8) 8) :element-type 'string-char :fill-pointer 0))

(defun create-bitmap (bitarray)
  (multiple-value-bind (command l cstring)
      (let* ((len (array-total-size bitarray))
	     (alen (+ (* 2 len) 8)))
	(declare (fixnum len alen))
	(cond ((< alen 255)
	       (with-fast-command (command *VM-BCOM-CREATE-BITMAP* alen :reply t)
		 (store-16bit command 2 8)	;width
		 (store-16bit command 4 8)	;height
		 (store-16bit command 6 1)	; nplanes
		 (store-16bit command 8 1)	; nbitcount
		 (dotimes (i len)
		   (store-16bit command (+ 10 (* 2 i)) (aref bitarray i)))))	       
	      (t
	       (when (< (array-total-size *bitmap*) alen)
		 (setf *bitmap* (make-array alen :element-type 'string-char :fill-pointer 0)))
	       (setf (fill-pointer *bitmap*) alen)
	       (store-16bit *bitmap* 0 8)	;width
	       (store-16bit *bitmap* 2 8)	;height
	       (store-16bit *bitmap* 4 1)	; nplanes
	       (store-16bit *bitmap* 6 1)	; nbitcount
	       (dotimes (i len)
		 (store-16bit *bitmap* (+ 8 (* 2 i)) (aref bitarray i)))
	       (send-command-await-response *VM-BCOM-CREATE-BITMAP* *bitmap*))))
    (if (= command *vm-bcom-handle-value*)
	(get-16bit cstring 0)
	nil)))

(defun create-pattern-brush (bitmap)
  (declare (fixnum bitmap))
  (multiple-value-bind (command l cstring)
      (with-fast-command (*create-pattern-brush* *VM-BCOM-CREATE-PATTERN-BRUSH* 2 :reply t)
	(store-16bit *create-pattern-brush* 2 bitmap))
    (if (= command *VM-BCOM-HANDLE-VALUE*)
	(get-16bit cstring 0)
	nil)))

(defun set-brush-org (dc x y)
  (declare (fixnum dc x y))
  (with-fast-command (*dcxy* *VM-BCOM-SET-BRUSH-ORG* 6)
    (store-16bit *dcxy* 2 dc)
    (store-16bit *dcxy* 4 x)
    (store-16bit *dcxy* 6 y)))

(defun get-stock-object (n)
  (declare (fixnum n))
  (multiple-value-bind (command l cstring)
      (with-fast-command (*object* *VM-BCOM-GET-STOCK-OBJECT* 2 :reply t)
	(store-16bit *object* 2 n))
    (if (= command *VM-BCOM-HANDLE-VALUE*)
	(get-16bit cstring 0)
	nil)))

(defun delete-object (object)
  (declare (fixnum object))
  (with-fast-command (*object* *VM-BCOM-DELETE-OBJECT* 2)
    (store-16bit *object* 2 object)))

(defun get-rop2 (dc)
  (declare (fixnum dc))
  (multiple-value-bind (command l cstring)
      (with-fast-command (*object* *VM-BCOM-GET-ROP2* 2 :reply t)
	(store-16bit *object* 2 dc))
    (if (= command *VM-BCOM-HANDLE-VALUE*)
	(get-16bit cstring 0)
        nil)))

(defun set-rop2 (dc rop)
  (declare (fixnum dc rop))
  (with-fast-command (*setrop2* *VM-BCOM-SET-ROP2* 4)
    (store-16bit *setrop2* 2 dc)
    (store-16bit *setrop2* 4 rop)))

(defun set-text-color (dc color)
 (declare (fixnum dc color))
 (with-fast-command (*set-text-color* *VM-BCOM-SET-TEXT-COLOR* 6)
   (store-16bit *set-text-color* 2 dc)
   (store-24bit *set-text-color* 4 color)))

(defun message-beep ()
  (with-fast-command (nil *VM-BCOM-BEEP* 0)))

(defun set-scroll-position (window bar position &optional (draw-p t))
  (declare (fixnum window position))
  (with-fast-command (*set-scroll-position* *VM-BCOM-SET-SCROLL-POSITION* 8)
    (store-16bit *set-scroll-position* 2 window)
    (store-16bit *set-scroll-position* 4 bar)
    (store-16bit *set-scroll-position* 6 position)
    (store-16bit *set-scroll-position* 8 (if draw-p 1 0))))

(defun set-background-color (dc color)
 (declare (fixnum dc color))
 (with-fast-command (*set-background-color* *VM-BCOM-SET-BACKGROUND-COLOR* 6)
   (store-16bit *set-background-color* 2 dc)
   (store-24bit *set-background-color* 4 color)))

(defun set-erase-brush (dc color)
 (declare (fixnum dc color))
 (with-fast-command (*set-background-color* *VM-BCOM-SET-ERASE-BRUSH* 6)
   (store-16bit *set-background-color* 2 dc)
   (store-24bit *set-background-color* 4 color)))

(defun select-object (dc n)
  (declare (fixnum dc n))
  (multiple-value-bind (command l cstring)
      (with-fast-command (*selobject* *VM-BCOM-SELECT-OBJECT* 4 :reply t)
	(store-16bit *selobject* 2 dc)
	(store-16bit *selobject* 4 n))
    (if (= command *VM-BCOM-HANDLE-VALUE*)
	(get-16bit cstring 0)
        nil)))

(defun select-brush (dc n)
  (declare (fixnum dc n))
  (with-fast-command (*selobject* *VM-BCOM-SELECT-BRUSH* 4)
    (store-16bit *selobject* 2 dc)
    (store-16bit *selobject* 4 n)))

(defun select-pen (dc n)
  (declare (fixnum dc n))
  (with-fast-command (*selobject* *VM-BCOM-SELECT-PEN* 4)
    (store-16bit *selobject* 2 dc)
    (store-16bit *selobject* 4 n)))

(defun select-bitmap (dc n)
  (declare (fixnum dc n))
  (with-fast-command (*selobject* *VM-BCOM-SELECT-BITMAP* 4)
    (store-16bit *selobject* 2 dc)
    (store-16bit *selobject* 4 n)))

(defun select-font (dc n)
  (declare (fixnum dc n))
  (with-fast-command (*selobject* *VM-BCOM-SELECT-FONT* 4)
    (store-16bit *selobject* 2 dc)
    (store-16bit *selobject* 4 n)))

(defun select-region (dc n)
  (declare (fixnum dc n))
  (with-fast-command (*selobject* *VM-BCOM-SELECT-REGION* 4)
    (store-16bit *selobject* 2 dc)
    (store-16bit *selobject* 4 n)))

(defun get-text-extent (dc string)
  (declare (fixnum dc))
  (store-string-arg string)
  (multiple-value-bind (command l cstring)
      (with-fast-command (*get-text-extent* *VM-BCOM-GET-TEXT-EXTENT* 4 :reply t)
	(store-16bit *get-text-extent* 2 dc)
	(store-16bit *get-text-extent* 4 (length string)))
    (if (= command *VM-BCOM-RECT-VALUE*)
        (values (get-16bit cstring 4)		;right
		(get-16bit cstring 6)		;bottom
		)
        nil)))

(defmacro with-dc ((dc-variable window) &body body)
  `(let ((,dc-variable ,window))
    ,@body))

#+old-code
(defmacro with-dc ((dc-variable window) &body body)
  `(let ((,dc-variable (get-dc ,window)))
     (unwind-protect
       (progn
         ,@body)
       (release-dc ,window ,dc-variable))))

#+old-code
(defmacro with-term-dc ((dc-variable) &body body)
  `(let ((,dc-variable (get-dc (get-term-window))))
     (unwind-protect
       (progn
         ,@body)
       (release-dc (get-term-window) ,dc-variable))))

(defun get-screen-size ()
  (multiple-value-bind (command l cstring)
      (with-fast-command (nil *VM-BCOM-GET-SCREEN-SIZE* 0 :reply t))
    (values (get-16bit cstring 0)
	    (get-16bit cstring 2))))

(defun get-pointer-position ()
  (multiple-value-bind (command l cstring)
      (with-fast-command (nil *VM-BCOM-GET-POINTER-POSITION* 0 :reply t))
    (values (get-16bit cstring 0)
	    (get-16bit cstring 2))))

(defun set-pointer-position (screen-x screen-y)
  (declare (fixnum screen-x screen-y))
  (with-fast-command (*xyposition* *VM-BCOM-SET-POINTER-POS* 4)
	(store-16bit *xyposition* 2 screen-x)
	(store-16bit *xyposition* 4 screen-y)))

;;; Mouse cursor stuff

;;; Calls LoadCursor with id as 16 bit arg and returns the handle
(defun loadcursor (id)
  (multiple-value-bind (command l cstring)
      (with-fast-command (*cursobject* *VM-BCOM-LOADCURSOR* 2 :reply t)
	(store-16bit *cursobject* 2 id))
    (get-16bit cstring 0)))

;;; Sets the mouse cursor for all WINFE windows, and returns the either
;;; the current cursor, if WINFE has the focus, or the current value of
;;; WINFE's cursor as last set.
(defun set-mouse-cursor (hcursor)
  (multiple-value-bind (command l cstring)
      (with-fast-command (*cursobject* *VM-BCOM-SET-MOUSE-CURSOR* 2 :reply t)
	(store-16bit *cursobject* 2 hcursor))
    (get-16bit cstring 0)))

;;;for moving the window position and stacking order

(defun set-window-position (window after-window x y w h flags)
  (declare (fixnum window after-window x y w flags))
  (with-fast-command (*swindowp* *VM-BCOM-SET-WINDOW-POS* 14)
    (store-16bit *swindowp* 2 window)
    (store-16bit *swindowp* 4 after-window)
    (store-16bit *swindowp* 6 x)
    (store-16bit *swindowp* 8 y)
    (store-16bit *swindowp* 10 w)
    (store-16bit *swindowp* 12 h)
    (store-16bit *swindowp* 14 flags)))

;;sets the windows title pane
(defun set-window-text (window text)
  (declare (fixnum window))
  (store-string-arg text)
  (with-fast-command (*window* *VM-BCOM-SET-WINDOW-TEXT* 2)
    (store-16bit *window* 2 window)))

(defun get-window-rectangle (window)
  (declare (fixnum window))
  (multiple-value-bind (command l cstring)
      (with-fast-command (*window* *VM-BCOM-GET-WINDOW-RECTANGLE* 2 :reply t)
	(store-16bit *window* 2 window))
    (if (= command *VM-BCOM-RECT-VALUE*)
	(values  (get-16bit cstring 0)		; left
		 (get-16bit cstring 2)		; top
		 (get-16bit cstring 4)		; bottom
		 (get-16bit cstring 6)		; right
		 )
        nil)))

(defun show-scroll-bar (window wbar show-p)
  (declare (fixnum window))
  (with-fast-command (*scroll-bar* *VM-BCOM-SHOW-SCROLL-BAR* 6)
    (store-16bit *scroll-bar* 2 window)
    (store-16bit *scroll-bar* 4 wbar)
    (store-16bit *scroll-bar* 6 (if show-p 1 0))))

(defun is-iconic (window)
  (declare (fixnum window))
  (multiple-value-bind (command l cstring) 
      (with-fast-command (*window* *VM-BCOM-IS-ICONIC* 2 :reply t)
	(store-16bit *window* 2 window))
    (if (= command *VM-BCOM-WINDOW-VALUE*)
	(not (zerop (get-16bit cstring 0)))
	;;probably not important, but avoid confusion vs returning nil
	(progn         
          (cerror "Return T anyway" "Is-iconic didn't return")
          (values t)))))


(defun notify-user (window message)
  (declare (fixnum window)(string message))
  (store-string-arg message)
  (with-fast-command (*window* *VM-BCOM-NOTIFY-USER* 2)
    (store-16bit *window* 2 window)))

(defun run-windows-application (application &optional (show SW_NORMAL))
  (declare (string application)(fixnum show))
  (store-string-arg application)
  (multiple-value-bind (command l cstring) 
    (with-fast-command (*window* *VM-BCOM-WINDOW-EXEC* 2 :reply t)
      (store-16bit *window* 2 show))
    (if (= command *VM-BCOM-WINDOW-VALUE*)
	(let ((return-value (get-16bit cstring 0)))
	  (declare (fixnum return-value))
	  ;;a value less than 32 means it couldn't do it
	  (unless (> return-value 32)
	    (error "Unable to start ~A, error code returned was ~D"
		   application return-value))))
  t))

;;this should probably be in the low-level CLOE sources, with
;;getcwd, but it was easier and quicker here.
(defun change-directory (path)
  (declare (string path))
  (store-string-arg path)
  (multiple-value-bind (command l cstring) 
    (with-fast-command (nil *VM-BCOM-CHANGE-DIR* 0))))


;;these functions set up the blinking cursor, it needs to be 
;;cleaned up more before making it the default

(defun show-caret (window)
  (declare (fixnum window))
  (with-fast-command (*window* *VM-BCOM-SHOW-CARET* 2)))

(defun hide-caret ()
  (with-fast-command (nil *VM-BCOM-HIDE-CARET* 0)))


(defun set-caret-pos (x y)
  (declare (fixnum x y))
  (with-fast-command (*coordinates* *VM-BCOM-SET-CARET-POS* 4)
    (store-16bit *coordinates* 2 x)
    (store-16bit *coordinates* 4 y)))

;;;

(defun select-file-dialog-box (window &key (default "*.*") 
				    (caption "File Choose"))
  (store-string-arg default)
  (store-string-arg caption)
  (multiple-value-bind (command l cstring)
    (with-fast-command (*window* *VM-BCOM-GET-PATHNAME* 2 :reply t)
      (store-16bit *window* 2 window))
    (if (= command *VM-BCOM-GET-PATHNAME*); cancel returned if no selection
	(subseq cstring 0 l)
	nil)))


;;helps to redraw scrollbar buttons when they don't appear, but
;;calling this only works some times
(defun paint-nonclient-area (window)
  (declare (fixnum window))
  (with-fast-command (*window* *VM-BCOM-NONCLIENT-PAINT* 2)
    (store-16bit *window* 2 window)))


(defvar *editor-for-windows* "epsilon")

(in-package 'user)  ;;;NOTE!!!!!

;;starts up dos, then you can switch back and forth using ^tab
;;if you give it a string, it will do that command and then exit
(defun command (&optional string)
  (win::run-windows-application  
    (if string 
	(concatenate 'string "command.com /c " string)
	"command.com")))

;;poor mans editor
(defun notepad (&optional file)
    (win::run-windows-application (concatenate 'string
					       "notepad "
					       file)))



;;Frob the ed function to work for both windows and cloe
(defvar non-windows-ed nil)
(setf (symbol-function 'non-windows-ed) (symbol-function 'ed))

;;real editor, exit brings you back to windows
;;This needs to actually be merged with ED that works when
;;windows is not enabled...
(defun ed (&optional file)
  (if (not win::*windows-enabled*)
      (non-windows-ed file)
      (win::run-windows-application (concatenate 'string
						 "command.com /c "
						 *editor-for-windows*
						 " "
						 file))))

(in-package 'win)  ;; so when more code gets added




