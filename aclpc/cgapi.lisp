(in-package :user)

;; All the crap from common windows that we need.

(eval-when (compile load eval)
  (pushnew :aclmerge *features*)
  (pushnew :OS-THREADS *features*))

(defpackage acl)

(defpackage cg 
  (:use :common-lisp :excl :clos :ff :cltl1 :windows :ct)
  (:shadowing-import-from ct defcstruct)
  (:nicknames :common-graphics)
  (:export #:device-listen
	   #:device-finish-output
	   #:device-force-output
	   #:device-clear-output
	   #:output-stream-p
	   #:device-open
	   #:device-close
	   #:device-eof-p
	   #:device-nread-string
	   #:device-file-length
	   #:device-file-position
	   #:device-set-file-position
	   #:location-write-string
	   #:device-terpri
	   ;;#:cset
	   ;;#:csets
	   #:i*
	   #:ilogior
	   #:defbparameter
	   #:defbvar
	   #:for
	   #:*hinst*
	   #:*hprevinst*
	   #:*screen*
		     
	   #:error-icon 
	   #:warning-icon
	   #:question-icon
	   #:information-icon
	   #:application-icon
	   #:lisp-icon
	   #:over-vector
	   #:initialize-cg
	   #:pre-windows4-p
	   #:hnull
	   #:defctype
	   #:cvt-aclwin-ctype
	   #:sizeof
	   #:cref-slot-xformer
	   #:cset-slot-xformer
	   #:make-keyword-x
	   #:null-handle
	   #:null-handle-p
	   #:callocate
	   #:ccallocate
		     
	   #:open-stream
	   #:loword
	   #:hiword
	   #:nstringify
	   #:windows-screen-device
	   #:make-control-text
	   #:lisp-window-class-name
	   #:lisp-resizing-window-class-name
	   ))

(defpackage pc
  (:use :win :common-lisp :cg :ct :excl) ;removed :ff
  (:export
   #:error-icon 
   #:warning-icon
   #:question-icon
   #:information-icon
   #:application-icon
   #:lisp-icon))

(eval-when (:compile-toplevel :load-toplevel :eval-toplevel)
  (import (loop for name in '(
			      "FREE-CSTRUCT"
			      "ALLOCATE-FOBJECT-C"
			      "FSLOT-ADDRESS-TYPE"
			      "DEFUN-C-CALLABLE"
			      "FREE-FOBJECT-FP"
			      "FSLOT-ADDRESS-C"
			      "DEFFOREIGN-LIST"
			      "FSLOT-VALUE-FP"
			      "DESCRIBE-FOBJECT"
			      "FOREIGN-STRLEN"
			      "STRING-TO-CHAR*"
			      "REGISTER-FUNCTION"
			      "SIZEOF-FOBJECT"
			      "DEFFOREIGN"
			      "FSLOT-ADDRESS-TYPED"
			      "RESET-ENTRY-POINT-TABLE"
			      "FOREIGN-POINTER-ADDRESS"
			      "REMOVE-ENTRY-POINT"
			      "CHAR*-TO-STRING"
			      "REGISTER-VALUE"
			      "FREE-FOBJECT-C"
			      "FSLOT-VALUE"
			      "LIST-ALL-FOREIGN-LIBRARIES"
			      "WITH-STACK-FOBJECT"
			      "CONVERT-FOREIGN-NAME"
			      "UNREGISTER-FOREIGN-CALLABLE"
			      "DEF-FOREIGN-TYPE"
			      "CONVERT-TO-LANG"
			      "FSLOT-ADDRESS"
			      "DEF-FOREIGN-CALL"
			      "FSLOT-ADDRESS-FP"
			      "GET-FOREIGN-TYPE"
			      "FOREIGN-POINTER"
			      "&"
			      "LISP-CALL"
			      "FREE-FOBJECT"
			      "ALLOCATE-FOBJECT-FP"
			      "MAKE-CSTRUCT"
			      "REGISTER-LISP-VALUE"
			      "UNREGISTER-VALUE"
			      "GET-EXTERN-CODE-ADDRESS"
			      #+IGNORE ;;ACLWFFI shadows this one
			      "DEFCSTRUCT"
			      "GET-ENTRY-POINTS"
			      "FOREIGN-POINTER-TYPE"
			      "DEF-C-TYPEDEF"
			      "FOREIGN-POINTER-P"
			      "ENSURE-FOREIGN-TYPE"
			      "DEFUN-FOREIGN-CALLABLE"
			      "FSLOT-VALUE-TYPED"
			      "FOBJECTP"
			      "MALLOC-CSTRUCT"
			      "COMPILE-FOREIGN-TYPE"
			      "DEF-C-TYPE"
			      "ALLOCATE-FOBJECT"
			      "REMOVE-EXTERN-DATA-ADDRESS"
			      "REGISTER-FOREIGN-CALLABLE"
			      "MAKE-FOREIGN-POINTER"
			      "GET-ENTRY-POINT"
			      "REMOVE-EXTERN-CODE-ADDRESS"
			      "FSLOT-VALUE-C"
			      "UNREGISTER-LISP-VALUE"
			      "FOREIGN-ADDRESS"
			      "LISP-VALUE"
			      "UNLOAD-FOREIGN-LIBRARY"
			      "CHAR*-STRING-LENGTH"
			      "FOREIGN-ADDRESS-P"
			      "GET-EXTERN-DATA-ADDRESS"
			      "UNREGISTER-FUNCTION"
			      "FOREIGN-TYPE-P")
	      collect (find-symbol name (find-package :ff)))
	  (find-package :pc)))

(defpackage printer 
  (:use :win :common-lisp :cg)
  (:export #:format2
	   #:call-print-basic))

(in-package cg)

;;from xtra.cl
(defun pre-windows4-p () 
  ;; we always run in at least windows 4
  nil)

(eval-when (compile load eval)
  (defmacro defbparameter (&rest x)
    `(defparameter ,@x))
  (defmacro defbvar (&rest x)
    `(eval-when (compile load eval) 
       (defvar ,@x)))

  (defmacro defctype (name type)
    `(ff::def-foreign-type ,name
	 ,(cvt-aclwin-ctype type)))

(defvar *cstr-names* nil)

#+ignore
(defmacro defcstruct (name slots &optional tag)
  (declare (ignore tag))
  (push name *cstr-names*)
  `(ff:def-foreign-type ,name
       (:struct ,@(mapcar #'(lambda (form)
			      ; form is (name type)
			      ;      or (name type more-type ...)
			      (if* (cddr form)
				 then ; second type
				      `(,(car form) ,(cvt-aclwin-ctype
					       (cdr form)))
				 else `(,(car form) ,(cvt-aclwin-ctype
					       (cadr form)))))
			  slots))))


(defmacro defapientry (lispname c-name args return &rest tocall)
  (declare (ignore tocall))
  `(progn
     (note-c-name ',lispname)
     (win::defsystemcall ',lispname
	 :entry-point '(:index ,c-name)
	 :arguments ',(make-list (length args)
				 :initial-element t)
	 :return-type ,(if* (eq return :boolean)
			  then :boolean
			  else :integer))))


(defmacro defapientry-t (&rest args)
  ;; i'm not sure what the -t ones are..
  `(defapientry ,@args))

)					;end eval-when


(eval-when (compile eval)
  (defmacro make-i-macro (iname clname &optional (result-type 'fixnum))
    `(defmacro ,iname (&rest args)
       (let* ((vars (mapcar #'(lambda (x) (declare (ignore x)) (gensym)) args))
	      (lets (mapcar #'(lambda (var form) (list var form)) vars args))
	      (body (list 'let lets
			  (list 'declare (list* 'fixnum vars))
			  (list* ',clname vars))))
	 ,(case result-type
	   ((t) 'body)
	   ((nil) `(list 'the 'fixnum body))
	   (otherwise `(list 'the ',result-type body)))))))

(make-i-macro i* *)
(make-i-macro i/ /)
(make-i-macro i= = t)
(make-i-macro i<= <= t)
(make-i-macro i< < t)
(make-i-macro i> > t)
(make-i-macro i>= >= t)
(make-i-macro ilogior logior)
(make-i-macro ilogtest logtest)
(make-i-macro iabs abs)
(make-i-macro imax max)
(make-i-macro imin min)

;; in aclwin a slot-name can be a simple name or it can be a list
;; of names that navigate through the object to the final value to be
;; accessed.
;;
;; &   -- stands for the address of the slot named by the naviagation so far.
;;	  Generally won't be at 'top-level'.  When used it is always
;	  the last item.
;; symbol -  stands for a slot with that name
;; number -  for array slots, accesses the value indexed by number
;; (fixnum expression)  -- again for array slots where the value of
;;		expression is evaled at run time to index the array.
;;		the actual symbol 'fixnum' must appear.
;; (slot-name ... ) -- a sequence of slots to naviagate through.
;;
;;
;; In order to distinguish the slots navigation expression (fixnum i)
;; from the array access expression, you may need to put extra
;; parens around the fixnum expression, i.e.  ((fixnum i))
;;

(defun make-cstructure (type length)
  ;; create and return a region of memory of the
  ;; given length.
  ;; 
  ;; in the mm implementation this was in malloc space, but since
  ;; it is referenced using #. in files like message.cl, we better
  ;; us a lisp structure so it will exist when the definition
  ;; is fasled in.
  ;;
  ;; we've got to look into this later
  ;;
  ;;
  (declare (ignore type))
  
  (allocate-fobject `(:array :unsigned-char ,length)
		    #-allegro-v4.3 :lisp))

(defvar *screen* nil)
(defbvar *hinst* 0) ;; <1>
(defbvar *hprevinst* 0) ;; <1>


(in-package :pc)


(defmacro sendmessage-with-pointer (hwnd msg wparam lparam result-object 
				    static-long-p)
  #+aclmerge
  (declare (ignore result-object static-long-p))
  #+aclmerge
  (warn "sendmessage-with-pointer call seen")
  #+aclmerge 
  `(sendmessage ,hwnd ,msg ,wparam ,lparam #-aclmerge ,result-object)
  #-aclmerge `(SendMessageP ,hwnd ,msg ,wparam ,lparam ,result-object))

(defconstant lpcmdline "")

(defbparameter lisp-icon 0)

(defbvar *format-string* nil
  "The complete format string for this call to format.
  MUST BE A SIMPLE STRING.
  If the control string was a general string it is dereferenced into a simple 
  string.")

(cl:defparameter common-dialog-errors
 '((#xffff . cderr_dialogfailure)
   (#x0000 . cderr_generalcodes)
   (#x0001 . cderr_structsize)
   (#x0002 . cderr_initialization)
   (#x0003 . cderr_notemplate)
   (#x0004 . cderr_nohinstance)
   (#x0005 . cderr_loadstrfailure)
   (#x0006 . cderr_findresfailure)
   (#x0007 . cderr_loadresfailure)
   (#x0008 . cderr_lockresfailure)
   (#x0009 . cderr_memallocfailure)
   (#x000a . cderr_memlockfailure)
   (#x000b . cderr_nohook)
   (#x000c . cderr_registermsgfail)
   (#x1000 . pderr_printercodes)
   (#x1001 . pderr_setupfailure)
   (#x1002 . pderr_parsefailure)
   (#x1003 . pderr_retdeffailure)
   (#x1004 . pderr_loaddrvfailure)
   (#x1005 . pderr_getdevmodefail)
   (#x1006 . pderr_initfailure)
   (#x1007 . pderr_nodevices)
   (#x1008 . pderr_nodefaultprn)
   (#x1009 . pderr_dndmmismatch)
   (#x100a . pderr_createicfailure)
   (#x100b . pderr_printernotfound)
   (#x100c . pderr_defaultdifferent)
   (#x2000 . cferr_choosefontcodes)
   (#x2001 . cferr_nofonts)
   (#x2002 . cferr_maxlessthanmin)
   (#x3000 . fnerr_filenamecodes)
   (#x3001 . fnerr_subclassfailure)
   (#x3002 . fnerr_invalidfilename)
   (#x3003 . fnerr_buffertoosmall)
   (#x4000 . frerr_findreplacecodes)
   (#x4001 . frerr_bufferlengthzero)
   (#x5000 . ccerr_choosecolorcodes)
   ))

(in-package :cg)

(eval-when (:compile-toplevel :load-toplevel :eval-toplevel)

(defconstant *nstringify-buffer-default-size* 2048)

(defvar nstringify-buffer 
    (make-array *nstringify-buffer-default-size* :fill-pointer t :element-type 'character))

(defvar control-text-buffer
    (make-array 2048 :fill-pointer t :element-type 'character))


(defun nstringify (x)
  (typecase x
    (simple-string x)
    (string 
     (let ((new-length 
	    (imin (length-vector x) (length nstringify-buffer))))
       (ncopy-vector nstringify-buffer x 0 0 new-length)
       (set-strlen nstringify-buffer new-length)
       nstringify-buffer))
    (null "")
    (t (nprin1-to-string x))))


);;end eval-when

(defstruct (rgb ;;(:include faslable-structure)   ;; <5>
	    (:copier nil)
	    ;;(:constructor make-rgb)
	    ;;(:constructor build-rgb (red green blue))
	    )
  (red  0 :type byte)
  (green  0 :type byte)
  (blue  0 :type byte))

(defun length-vector (x) (check-type x vector) (length x))

;; CLIM makes one of these, gets the slot value, and then
;; throws the rest of it away.
(defclass windows-screen-device ()
  ((device-handle1 :initarg :device-handle1
		   :initform 0)
   (device-handle2 :initarg :device-handle1
		   :initform (win:CreateDC "DISPLAY" hnull hnull hnull))))

(defun open-stream (device location direction &rest options)
  ;; The only part of open-stream that we really need.
  (make-instance device))

(defun KILL-ACL-TIMER-EXIT-FN (&rest ignoreargs)
  (declare (ignore ignoreargs))
  ;; nothing to kill anymore
  nil)

(defun loword (long)
  (etypecase long
    (integer (logand long #xffff))
    (word-vector (word-vector-ref long 1)))
  )

(defun hiword (long)
  (etypecase long
    (integer (logand #xffff (ash long -16)))
    (word-vector (word-vector-ref long 0)))
  )


(defconstant lisp-window-class-name "LispWindow")
(defconstant lisp-resizing-window-class-name "LispResizingWindow")

(defun schar-byte (string index)
  (if (< index (length string))
      ;; Use aref so it will work on adjustable arrays
      (char-int (aref string index))
    0))

(defun set-schar-byte (string index new)
  (if (< index (length string))
      (etypecase string
	((simple-array character (*))
	 (setf (schar string index) (cltl1:int-char new)))
	(array
	 (setf (aref string index) (cltl1:int-char new)))
	))
  new)

#+ign
(defsetf schar-byte (string index) (v)
  `(set-schar-byte ,string index ,v))

(defmacro iincf (&rest x) `(incf ,@x))
(defmacro idecf (&rest x) `(decf ,@x))

(defun strlen (str)
  ;; compute length of null terminated string in a big buffer
  
  (typecase str
    (string (dotimes (i (length str) (length str))
	      (if* (eq #\null (aref str i))
		 then (return i))))
    (otherwise (error "???"))))

(defun set-strlen (stringvar newlen)
  ;;mm: Add a terminating zero-char for C code.
  (setf (aref stringvar newlen) #\null)
  (setf (fill-pointer stringvar) newlen))


(defun nstringify-for-control (x)
  (typecase x
    ((not symbol)
     (nstringify x))
    (null "")
    (t
     (let* ((symbol-name (symbol-name x))
	    (new-length 
	     (imin (length-vector symbol-name) #.(length nstringify-buffer))))
       (ncopy-vector nstringify-buffer symbol-name 0 0 new-length)
       (set-strlen nstringify-buffer new-length)
       (nstring-capitalize nstringify-buffer :end new-length)))))

(defun make-control-text (x)
  (let ((string (nstringify-for-control x)))
    ;;mm: This is not MP-safe ???
    ;;mm: allow access to the whole buffer 
    #+aclmerge (setf (fill-pointer control-text-buffer) 2047)
    (set-strlen 
     control-text-buffer
     ;; copy characters across and return final length
     (block nil				; macroexpanded the FOR loop to get
					; this... jpm.
	    (let* ((to-index 0) (from-index 0) (char-byte nil) (last-char-byte nil))
		  (declare (fixnum to-index) (fixnum from-index))
		  (tagbody
		   for-loop
		   (when (or (i>= from-index #.(length nstringify-buffer)) 
			     (i>= to-index #.(length control-text-buffer))) 
			 (go for-exit))
		   (setq last-char-byte char-byte)
		   (setq char-byte (schar-byte string from-index))
		   (set-schar-byte control-text-buffer to-index char-byte)
		   (case char-byte
			 (0 (go for-exit))
			 (#.(char-int #\&)
			    (when (i< from-index #.(1- (length nstringify-buffer)))
				  (set-schar-byte control-text-buffer 
						  (iincf to-index) 
						  #.(char-int #\&))))
			 (#.(char-int #\~)
			    (if (eql last-char-byte #.(char-int #\~))
				(progn (set-schar-byte control-text-buffer
						       (idecf to-index) 
						       #.(char-int #\~))
				       (setf char-byte 0))
				(set-schar-byte control-text-buffer to-index 
						#.(char-int #\&)))))
		   (iincf to-index 1)
		   (iincf from-index 1)
		   (go for-loop)
		   for-exit)
		  to-index)))
    ;; return the control text
    control-text-buffer))

(defmacro long-ref (p i) 
  `(sys::memref-int ,p 0 
		    (the fixnum (* 4 (the fixnum ,i)))
		    :unsigned-long))

(defmacro signed-long-ref (p i) 
  `(sys::memref-int ,p 0 
		    (the fixnum (* 4 (the fixnum ,i)))
		    :signed-long))

(defstruct (PCCStructure 
	    (:print-function print-pccstructure)
	    (:predicate PCCStructurep))
  (type-tag 0)
  (data-length 0)
  (data-pointer 0)
  )

(defun delimited-string-to-list ;; <8>
    (string delimiter-char-or-string)
  "Returns a list of substrings of STRING, separating it at DELIMETER-CHAR-OR-STRING"
  (do* ((stringp (stringp delimiter-char-or-string))
	(delimiter-length (if stringp
                              (length delimiter-char-or-string)
			    1))
	(s string (subseq s (+ index delimiter-length)))
	(index
	 (if stringp
	     (search delimiter-char-or-string s)
	   (position delimiter-char-or-string s))
	 (if stringp
	     (search delimiter-char-or-string s)
	   (position delimiter-char-or-string s)))
	(list
	 (list (subseq s 0 index))
	 (nconc list (list (subseq s 0 index)))))
      ((null index)
       list)))

(defun spaced-string-to-list (string) ;; <27>
  (delimited-string-to-list string #\space))

