;;; -*-Mode: Lisp; Syntax: ANSI-Common-lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;;;
;;; DEFSYSTEM Utility
;;;
;;; Copyright (c) 1986 Regents of the University of California
;;; 
;;; Permission to use, copy, modify, and distribute this software and its
;;; documentation for any purpose and without fee is hereby granted,
;;; provided that the above copyright notice appear in all copies and
;;; that both that copyright notice and this permission notice appear in
;;; supporting documentation, and that the name of the University of
;;; California not be used in advertising or publicity pertaining to
;;; distribution of the software without specific, written prior
;;; permission.  The University of California makes no representations
;;; about the suitability of this software for any purpose.  It is
;;; provided "as is" without express or implied warranty.
;;; 

;;;-----------------------------------------------------------
;;; NOTE
;;;
;;; Because of its utility when using defsystem, this file includes an
;;; implementation of (part of) the PATHANME-SUBDIRECTORY-LIST:NEW-REPRESENTATION
;;; proposal as accepted by the X3J13 committee.  Once that proposal actually
;;; gets in the lisps, it can be removed from this file.
;;;
;;;-----------------------------------------------------------

;; $fiHeader: defsystem.lisp,v 1.6 92/03/04 16:22:31 cer Exp Locker: cer $

;; Add a feature for ANSI-adhering Lisps.  So far, only Apple's
;; version 2.0 tries to do adhere to the ANSI spec instead of CLtL rev 1.
;; Yes, I know it's not yet an ANSI spec.  That's why this is called
;; ANSI-90, corresponding to what we think the spec will be, in late 1990.

#+Allegro
(eval-when (compile load eval)
  (when (and (find-package :defsys)
	     (not (find-package :excl-defsystem)))
    (let ((excl::*enable-package-locked-errors* nil))
      (rename-package 'defsys 'excl-defsystem))))
 


#+(or CCL-2 Allegro Minima)	;Have to assume this won't blow up anybody's lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :ansi-90 *features*))

#+Genera
(eval-when (compile load eval)
  (multiple-value-bind (major minor) (sct:get-system-version)
    (declare (ignore minor))
    (when (>= major 437)
      (pushnew :ansi-90 *features*))))

#+ANSI-90
(defpackage "DEFSYSTEM"
  (:use "COMMON-LISP")
  (:nicknames "DEFSYS" "DS")
  (:export
    *current-system*
    *defsystem-version*
    *language-descriptions*
    *load-all-before-compile*
    *sysdcl-pathname-defaults*
    compile-system 
    defsystem
    load-system
    load-system-def
    set-system-source-file
    show-system
    system-source-file 
    undefsystem
    with-compiler-options
    with-delayed-compiler-warnings))

#+ANSI-90
(in-package :defsystem)

#+(or Genera-Release-8-0 Genera-Release-8-1)
(lisp:in-package "DEFSYSTEM"
		 :use '("LISP")
		 :nicknames '("DEFSYS"))

#-(or ANSI-90 Genera-Release-8-0 Genera-Release-8-1)
(lisp:in-package #-(or lispworks ANSI-90) "DEFSYSTEM" 
            #+lispworks "PDEFSYS"
            #+ANSI-90 "DEFSYSTEM"
            #-ANSI-90 :use #-ANSI-90 '("LISP")
            #-ANSI-90 :nicknames #-ANSI-90 '("DEFSYS"))

#-ANSI-90
(export '(system-source-file set-system-source-file load-system-def
	  load-system *current-system* compile-system show-system 
	  *language-descriptions* undefsystem *defsystem-version* defsystem
	  *sysdcl-pathname-defaults*
	  ;; Hack
	  *load-all-before-compile*
	  with-compiler-options with-delayed-compiler-warnings)
	"DEFSYSTEM")

;;; *** A temporary workaround, easier than fixing all references to
;;; *** LISP:<foo>.  --RWK 20.Nov.90
#+ANSI-90
(eval-when (eval compile load)
  (flet ((fix-package (pack-name add-name)
	   (setq add-name (string add-name))
	   (let ((pack (find-package pack-name)))
	     (assert (not (null pack)) ()
		     "Attempting to add the name ~S to package ~S, which doesn't exist")
	     (when (null (find-package add-name))
	       (rename-package pack (package-name pack)
			       (list* add-name (package-nicknames pack)))))))
    (fix-package "COMMON-LISP" "LISP")
    (fix-package "COMMON-LISP" "CL") ;; ??
    (fix-package "COMMON-LISP-USER" "USER")
    (fix-package "COMMON-LISP-USER" "CL-USER")) ;; ??
  (when (null (find-package "SYSTEM"))
    (defpackage system)))


;;;
;;; Pathname stuff
;;;

#-(or Minima Genera)
(eval-when (eval load compile)
  (shadow (list (intern (symbol-name :make-pathname) :lisp)
		(intern (symbol-name :pathname-directory) :lisp))))

#-(or Minima Genera)
(eval-when (eval load compile)
  (export (list (intern (symbol-name :make-pathname) :defsystem)
		(intern (symbol-name :pathname-directory) :defsystem))))

;;
;; The implementations of PATHNAME-DIRECTORY and MAKE-PATHNAME use two
;; implementation-dependent functions.  When porting this code to a new lisp,
;; you'll need to define the following two function:
;;
;; EXTERNALIZE-DIRECTORY takes a pathname-directory as returned by the
;; underlying Lisp and returns a directory-list as specified in the proposal.
;;
;; INTERNALIZE-DIRECTORY takes a directory-list and returns something understood
;; by the Lisp as a kosher pathname-directory.
;;

#-(or Minima Genera)
(defun pathname-directory (pathname)
  (externalize-directory (lisp:pathname-directory pathname)))

#-(or Minima Genera)
(defun make-pathname (&rest options
			    &key host device directory name type version defaults)
  (declare (ignore host device name type version))
  (flet ((merge-directories (directory defaults)
	   (if (and (consp directory)
		    (eq (car directory) :relative)
		    defaults
		    (consp (pathname-directory defaults)))
	       (do ((start (pathname-directory defaults) (butlast start))
		    (tail (cdr directory) (cdr tail)))
		   ((not (and (member (car tail) '(:back :up))
			      (or (string (car (last start)))
				  (eq (car (last start)) :wild))))
                    (append start tail))
		 )
	       directory)))
    (apply #'lisp:make-pathname
	   :directory (internalize-directory (merge-directories directory defaults))
	   options)))


;;
;; IBCL
;; Uses :ROOT instead of :ABSOLUTE
;; Doesn't include :RELATIVE
;; Uses :RELATIVE instead of :UP
;; Uses "*" instead of :WILD
;; Merge-pathnames still loses, since it doesn't do the right thing wrt relative
;;  pathnames. 
;;
#+ibcl
(progn

(defun externalize-directory (directory)
  (if (eq (car directory) :root)
      (setq directory `(:absolute ,@(cdr directory)))
      (setq directory `(:relative ,@directory)))
  (mapcar #'(lambda (name)
	      (cond ((equal name "*") :wild)
		    ((eq name :relative) :up)
		    (t name)))
	  directory))

(defun internalize-directory (directory)
  (case (car directory)
    (:absolute (setq directory `(:root ,@(cdr directory))))
    (:relative (setq directory (cdr directory))))
  (mapcar #'(lambda (name)
	      (case name
		(:wild "*")
		(:up :relative)
		(otherwise name)))
	  directory))

) ; ibcl


;;
;; Harlequin
;;
#+lispworks
(progn

(defun externalize-directory (directory)
  (if (equal "/" (car directory))
      (setq directory `(:absolute ,@(cdr directory)))
      (setq directory `(:relative ,@directory)))
  (mapcar #'(lambda (name)
	      (cond ((equal name "*") :wild)
		    ((equal name "..") :up)
		    (t name)))
	  directory))

(defun internalize-directory (directory)
  (case (car directory)
    (:absolute (setq directory `("/" ,@(cdr directory))))
    (:relative (setq directory (cdr directory))))
  (mapcar #'(lambda (name)
	      (case name
		(:wild "*")
		(:up "..")
		(otherwise name)))
	  directory))

) ; lispworks


;;
;; Genera and Cloe
;; I'm reasonably sure these are correct, based on the "Current practice"
;; section of the proposal and some playing around with Genera.
;;
#+(or Genera Cloe)
(progn

(defun externalize-directory (directory)
  (typecase directory
    (symbol (case directory
	      (:root (setq directory (list :absolute)))
	      (otherwise )))
    (cons (case (car directory)
	    (:relative )
	    (otherwise (setq directory (cons :absolute directory)))))
    (otherwise ))
  directory)

(defun internalize-directory (directory)
  (case (car directory)
    (:absolute (if (null (cdr directory))
		   (setq directory :root)
		   (setq directory (cdr directory))))
    (otherwise ))
  directory)

) ; genera


;;
;; Other Unix implementations that just use strings to represent directories?
;;
#+(or (and Lucid Unix)
      (and Allegro Unix))
(progn

(defun externalize-directory (directory)
  (let ((delim #\/)
	(subdirs nil)
	type index)
    (if (stringp directory)
	(flet ((add-dir (name)
		 (cond ((equal name "..") (push :up subdirs))
		       ((equal name ".") )
		       ((equal name "*") (push :wild subdirs))
		       (t (push name subdirs)))))
	  (if (eql (elt directory 0) delim)
	      (setq type :absolute
		    index 1)
	      (setq type :relative
		    index 0))
	  (do ((start index (1+ end))
	       (end (position delim directory :start index)
		    (position delim directory :start (1+ end))))
	      ((null end) (unless (= start (length directory))
			    (add-dir (subseq directory start))))
	    (add-dir (subseq directory start end)))
	  (cons type (reverse subdirs)))
	directory)))

(defun internalize-directory (directory)
  (if (consp directory)
      (let ((dirstring (ecase (car directory)
			 (:absolute "/")
			 (:relative ""))))
	(dolist (subdir (rest directory))
	  (setq dirstring (concatenate 'string dirstring
				       (cond ((eq subdir :up) "..")
					     ((eq subdir :wild) "*")
					     ((stringp subdir) subdir)
					     (t (error "~S not a string" subdir)))
				       "/")))
	dirstring)
      directory))
  
)


;;
;; Xerox (nee EnVos) lisp has a number of conventions that it kinda follows.
;; Directories are delimited by Unix-style "/"s, or maybe in
;; "<root-dir>sub-dir1>sub-dir2>" style.
;;
#+xerox
(progn

(defun externalize-directory (directory)
  (let ((subdirs nil)
	type index)
    (if (stringp directory)
	(flet ((next-delim-position (string start)
		 (let ((p1 (position #\/ string :start start))
		       (p2 (position #\> string :start start)))
		   (cond ((null p1) p2)
			 ((null p2) p1)
			 (t (min p1 p2)))))
	       (add-dir (name)
		 (cond ((equal name "..") (push :up subdirs))
		       ((equal name "*") (push :wild subdirs))
		       ((equal name ".") )
		       (t (push name subdirs)))))
	  (if (member (elt directory 0) '(#\/ #\<))
	      (setq type :absolute
		    index 1)
	      (setq type :relative
		    index 0))
	  (do ((start index (1+ end))
	       (end (next-delim-position directory index)
		    (next-delim-position directory (1+ end))))
	      ((null end) (unless (= start (length directory))
			    (add-dir (subseq directory start))))
	    (add-dir (subseq directory start end)))
	  (cons type (reverse subdirs)))
	directory)))

(defun internalize-directory (directory)
  (if (consp directory)
      (let ((dirstring (ecase (car directory)
			 (:absolute "/")
			 (:relative ""))))
	(dolist (subdir (rest directory))
	  (setq dirstring (concatenate 'string dirstring
				       (cond ((eq subdir :up) "..")
					     ((eq subdir :wild) "*")
					     ((stringp subdir) subdir)
					     (t (error "~S not a string" subdir)))
				       "/")))
	dirstring)
      directory))
  
)

;; Macintosh Allegro CL
;;
#+(and ccl (not ANSI-90))
(progn ; slh
  
  (defun externalize-directory (directory)
    `(:absolute ,directory))
  
  (defun internalize-directory (directory)
    (flet ((string-append (str1 str2)
             (concatenate 'string str1 str2 ":")))
     (reduce #'string-append directory :start 1)))
  )

#+(and cmu (not ANSI-90))
(progn
  
  (defun externalize-directory (directory)
    (cons :absolute (coerce directory 'list)))
  
  (defun internalize-directory (directory)
    (assert (eq (car directory) :absolute))
    (coerce (cdr directory) 'vector))
  )

#+(and ANSI-90 (not Genera) (not Cloe) (not Minima) (not Allegro))
(progn ; rwk
  
  (defun externalize-directory (directory)
    directory)
  
  (defun internalize-directory (directory)
    directory)
)


(pushnew :defsystem *features*)

(defparameter *defsystem-version* 3.66)

;;; Let debugging and information stuff know what module we're on.

;;
;; CL "extensions"
;;
;; This includes all implementation-dependent code, except for stuff to handle
;; multiple languages.
;;

;; Quite compiler messages about references to undefined functions untill the
;; entire body is completed.
;;
(defmacro with-delayed-compiler-warnings (&body body)
  #+Lucid `(lcl:with-deferred-warnings ,@body)
  #+(and genera (not ANSI-90)) `(compiler:compiler-warnings-context-bind ,@body)
  #+TI `(compiler:compiler-warnings-context-bind ,@body)
  #+(and ANSI-90 (not Allegro)) `(with-compilation-unit () ,@body) ;; Allegro 4.0 nonconformant
  #-(or Lucid genera TI (and ANSI-90 (not Allegro))) `(progn ,@body))

;; Genera deals with pathname components that are all uppercase much prettier
;; then lowercase.
;;
(defun pretty-pathname-component (x)
  #+genera
  (typecase x
    (string (string-upcase x))
    (otherwise x))
  #-genera
  x)

;; Are the pathnames equal?  This needs to take into account systems where
;; file names are case sensitive.
;;
(defun path-equal (x y)
  #+genera
  (string-equal x y)
  #-genera
  (equal x y))

;; Return the pathname of the file that is currently being loaded, if any.
;;
(defun current-source-file ()
  #+Lucid lcl:*source-pathname*
  #+Allegro excl:*source-pathname*
  #+xerox (pathname *standard-input*)
  #+Genera sys:fdefine-file-pathname
  #+Minima *load-pathname*
  #-(or Lucid Allegro xerox Genera Minima) nil)

(defvar *current-system* nil
  "Name of the system currently being loaded or compiled")



;;;
;;; Description of languages
;;;

(defstruct (language (:type list))
  name					;Name of the language.  A keyword symbol
  source-types				;List of file types that might contain
					; source code
  binary-types				;List of file types that might contain
					; compiled code
  compile-fn				;Function to apply to the source-file
					; pathname, the binary-file pathname,
					; and the list of optimizations to
					; compile the source
  load-fn)				;Function to apply to a binary-file to
					; load it

(defvar *language-descriptions* nil
  "List of descriptions of languages understood by defsystem")

(defun find-language (name)
  (or (assoc name *language-descriptions*)
      (error "No description for language ~s" name)))


;;
;; LISP
;;

(defparameter lisp-file-types
  ;; Thanks to PCL for providing all this info
  #+(and Genera imach)                `("lisp" 		"ibin")
  #+(and Genera (not imach))          `("lisp" 		"bin")
  #+Cloe-Runtime		      `("l"		"fas")
  #+Minima			      `("lisp"		"mebin")
  #+(and dec common vax (not ultrix)) `("LSP"		"FAS")
  #+(and dec common vax ultrix)       `("lsp"		"fas")
  #+KCL                               `("lsp"		"o")
  #+xerox                             `(("lisp" "cl" "")"dfasl")
  #+(and Lucid MC68000)		      `(("lisp" "cl")	,lcl:*load-binary-pathname-types*)
  #+(and Lucid VMS)		      `(("lisp" "cl")	,lcl:*load-binary-pathname-types*)
  #+(and Lucid SPARC)		      `(("lisp" "cl")	,lcl:*load-binary-pathname-types*)
  #+Allegro			      `(("lisp" "cl")	"fasl")
  #+ibcl                              `(("lisp" "lsp")  "o")
  #+lispworks                         `("lisp"		,ccl::*binary-file-type*)
  #+system::cmu                       `("slisp"		"sfasl")
  #+cmu				      `("lisp"		"sparcf")
  #+prime                             `("lisp"		"pbin")
  #+hp                                `("l"		"b")
  #+TI                                `("lisp"		"xfasl")
  #+ccl                               `("lisp"          "fasl"))

(defun compile-lisp-file (pathname binary-pathname optimizations)
  #+(or Genera Cloe-Runtime lispworks CCL-2)
  (format t "~&; Compiling ~A~%" pathname)
  #+CCL-2
  (ccl:set-mini-buffer ccl:*top-listener* "~A: Compiling ~A." 
                       *current-system*
                       (namestring pathname))
  (let (#+Minima (*compile-verbose* t))
    (if optimizations
	(eval `(locally (declare (optimize ,@optimizations))
			(compile-file ',pathname :output-file binary-pathname)))
	(compile-file pathname :output-file binary-pathname))))
     

(defun load-lisp-file (pathname)
  #+Cloe-Runtime (format t "~&; Loading ~A~%" pathname)
  #+CCL-2 (ccl:set-mini-buffer ccl:*top-listener* "~A: Loading ~A."
                               *current-system*
                               (namestring pathname))
  (let (#+Minima (*load-verbose* t))
    (load pathname)))

(pushnew `(:lisp ,@lisp-file-types compile-lisp-file load-lisp-file)
	 *language-descriptions* :test #'equal)


;;
;; C
;;

(defun compile-c-file (pathname binary-pathname compiler-flags)
  (progn binary-pathname compiler-flags)	;Get rid of warnings when undefined.
  (format t "~&;;; Compiling C file ~a" pathname)
  #+ibcl
  (let* ((command (format nil "cc~{ ~A~} -c -o ~a ~a"
			  compiler-flags
			  binary-pathname
			  pathname))
	 (status (lisp:system command)))
    (if (= status 0)
	t
	(error "Error compiling file ~a; status = ~s" pathname status)))
  #+(and Allegro Unix)
  (let* ((command (format nil "cc~{ ~A~} -c -o ~a ~a"
			  compiler-flags
			  binary-pathname
			  pathname))
	 (status (excl:run-shell-command
		   command :wait t :input *standard-input* :output *standard-output*)))
    (if (= status 0)
	t
	(error "Error compiling file ~a; status = ~s" pathname status)))
  #+(and Lucid unix)
  (let* ((cc-args `(,@compiler-flags "-c" "-o"
				     ,(namestring binary-pathname)
				     ,(namestring pathname)))
	 (status (third (multiple-value-list (lcl:run-program "cc"
							      :arguments cc-args
							      :wait t)))))
    (if (= status 0)
	t
	(error "Error compiling file ~a; status = ~s" pathname status)))
  #-(or ibcl (and Allegro unix) (and Lucid unix))
  (error "Don't know how to compile C code"))

(defun load-c-file (binary-pathname)
  (progn binary-pathname)			;Get rid of warnings when undefined.
  #+ibcl
  ;; This is stupid.  You can't just load a C object file - you have to load it
  ;; as part of loading a compiled Lisp file.  Or am I just missing something?
  (let* ((tmp-file (pathname "/tmp/ibcl-defsystem-dummy-file"))
	 (tmp-o-file (make-pathname :type "o" :defaults tmp-file))
	 (tmp-lsp-file (make-pathname :type "lsp" :defaults tmp-file)))
    (unless (probe-file tmp-o-file)
      (unless (probe-file tmp-lsp-file)
	(with-open-file (s tmp-lsp-file :direction :output)
	  (print '(in-package "USER") s)))
      (compile-file tmp-lsp-file :output-file tmp-o-file))
    (system:faslink tmp-o-file (format nil "~a" binary-pathname)))
  #+(and Allegro unix)
  (load binary-pathname)
  #+(and Lucid unix)
  (progn
    (lcl:load-foreign-files (list binary-pathname))
    (lcl:load-foreign-files nil)
    )
  #-(or ibcl (and Allegro unix) (or Lucid unix))
  (error "Don't know how to load C code"))

(pushnew '(:c "c" "o" compile-c-file load-c-file)
	 *language-descriptions* :test #'equal)

;;;
;;; Systems and modules
;;;

(defstruct (system (:print-function print-system))
  ;; "Public" slots that can be initialized from (defsystem ...)
  (name)
  (default-pathname (pathname "") :type pathname)
  (default-binary-pathname nil)
  (default-package nil :type symbol)
  (needed-systems nil :type list)
  (load-before-compile nil)
  (default-optimizations nil :type list)
  (patch-file-pattern nil)	        ;Not used by DEFSYSTEM; just for Makefile facility
  ;; Internal slots
  (loaded-p nil)			;Has (a version of) the system been loaded?
  (declared-defining-file nil)		;Declared location of the file that
					; contains the system definition.  This
					; is different from the defining-file
					; slot that follows, since that slot
					; holds the actual pathname of the file
					; that really did define the system, and
					; this holds a higher truth.
  (defining-file nil)			;What file contains the system definition?
  (definition-loaded-p nil)		;Has the system definition been loaded?
  (defining-file-write-date nil)	;What is the write-date of the file that
					; contained the current system definition
  (module-list nil :type list))		;List of modules that make up the system

(defun print-system (system stream depth)
  (declare (ignore depth))
  (format stream "#<System ~a>" (system-name system)))


(defstruct (module (:print-function print-module))
  ;; "Public" slots that can be set directly from (defsystem ...)
  (name)
  (compile-satisfies-load nil)		;Does compiling the module have the
					; side-effect of loading it?
  (load-after nil)
  (load-before-compile nil)
  (optimizations nil :type list)
  (language :lisp :type symbol)
  (features t)
  (eval-after nil)
  (pathname nil)
  (binary-pathname nil)
  (package nil)
  (binary-only nil)
  ;; Internal slots
  (system)
  (load-after-list nil :type list)	;Expanded version of the load-after slot
  (load-date 0)
  (loaded-p nil)
  (loaded-from-file nil))

(defun print-module  (module stream level)
  (declare (ignore level))
  (format stream "#<Module ~a>" (module-name module)))


;; Some "virtual" read-only slots of the structs just defined



;; Does the module apply to the current environment, as determined by the value
;; of *features* and the features slot of the module?
(defun module-applicable-p (module)
  (labels ((feature-equal-p (f1 f2)
	     (or (equal f1 f2)
		 (and (or (stringp f1) (symbolp f1))
		      (or (stringp f2) (symbolp f2))
		      (string-equal f1 f2))))
	   (featurep (f)
	     (etypecase f
	       (symbol (member f *features* :test #'feature-equal-p))
	       (cons (ecase (car f)
		       (and (every #'featurep (cdr f)))
		       (or (some #'featurep (cdr f)))
		       (not (not (featurep (cadr f)))))))))
    (or (eq t (module-features module))
	(featurep (module-features module)))))


;; Adds a type onto the pathname.  If there are many possibilities for the type,
;; tries to be smart about picking one.
(defun add-pathname-type (pathname types)
  (flet ((make-path (type defaults)
	   (lisp:make-pathname :type (pretty-pathname-component type)
			       :defaults defaults)))
    (if (consp types)
	(let (path)
	  (dolist (type types)
	    (setq path (make-path type pathname))
	    (when (probe-file path)
	      (return-from add-pathname-type path)))
	  (make-path (first types) pathname))
	(make-path types pathname))))

;; Return the pathname of the source version of the module
(defun module-src-path (module)
  (let ((pathname (module-pathname module))
	(types (language-source-types (find-language (module-language module))))
	(defaults (or (system-default-pathname (module-system module))
		      *default-pathname-defaults*)))
    (cond ((null pathname)
	   (add-pathname-type (lisp:make-pathname :name (string (module-name module))
						  :defaults defaults)
			      types))
	  ((null (pathname-type pathname))
	   (add-pathname-type (merge-pathnames pathname defaults)
			      types))
	  (t (merge-pathnames pathname defaults)))))


;;; Bind true if you want output directories auto-created if needed.

(defvar *auto-create-output-directories* nil)

;;; Defaults to always returning true if an implementation hasn't
;;; said how to check, or never wants to auto-create.

(defun directory-exists-p (pathname)
  pathname
  #+CCL-2 (ccl::directory-exists-p pathname))

(defun create-directory (pathname)
  #+CCL-2 (let* ((path (translate-logical-pathname pathname))
                 (dir (pathname-directory path))
                 (dirname (first (last dir)))
                 (ndir (butlast dir))
                 (dirpath (lisp:make-pathname :name dirname :type :unspecific :directory ndir :defaults path)))
            (ccl:create-directory dirpath))
  pathname)

;; Return the pathname of the binary version of the module
(defun module-bin-path (module)
  (let* ((pathname (module-binary-pathname module))
         (src-pathname (module-pathname module))
         (types (language-binary-types (find-language (module-language module))))
         (system (module-system module))
         (sys-default-1 (system-default-pathname system))
         (sys-default (if sys-default-1
			  (merge-pathnames sys-default-1)
			  *default-pathname-defaults*))
         (sys-bin-default (system-default-binary-pathname system))
         (defaults (if sys-bin-default
		       (merge-pathnames sys-bin-default sys-default)
		       sys-default))
         (result (cond ((and (null pathname) (null src-pathname))
                        (add-pathname-type (lisp:make-pathname :name (string (module-name module))
                                                               :defaults defaults)
                                           types))
                       ((null pathname)
                        (add-pathname-type (merge-pathnames src-pathname defaults)
                                           types))
                       ((null (pathname-type pathname))
                        (add-pathname-type (merge-pathnames pathname defaults)
                                           types))
                       (t (merge-pathnames pathname defaults)))))
    (when *auto-create-output-directories*
      (unless (directory-exists-p result)
        (let ((dir-path (lisp:make-pathname :name nil :type nil :defaults result)))
          (format t "~&Creating directory ~S~:[~*~; (~A)~]"
                  dir-path
		  ;; --- Allegro 4.0 nonconformant
		  #+(and ANSI-90 (not Allegro) (not Genera) (not Cloe))
		  (typep result 'logical-pathname)
		  #-(and ANSI-90 (not Allegro) (not Genera) (not Cloe))
		  nil
                  (translate-logical-pathname dir-path))
          (create-directory result))))
    result))

;; Return the pathname of the newest loadable version of the module
(defun module-loadable-path (module &optional source-if-newer)
  (let ((bname (probe-file (module-bin-path module))))
    (if (and bname (not source-if-newer))
	bname
	(let ((sname (probe-file (module-src-path module))))
	  (cond ((and (null bname) (null sname))
		 (error "Can't find source or binary for ~s" module))
		((null bname) (if (eq (module-language module) :lisp)
				  sname
				  (error "Can't find loadable version of ~s" module)))
		((null sname) bname)
		((< (file-write-date sname) (file-write-date bname)) bname)
		(t sname))))))

;; Is the most recent version of the module loaded?
(defun module-up-to-date-p (module)
  (cond ((not (module-applicable-p module)) t)
	((not (module-loaded-p module)) nil)
	(t (let ((file (probe-file (module-loadable-path module))))
	     (if file
		 (eql (module-load-date module) (file-write-date file))
		 (error "The module ~S has disappeared." (module-name module)))))))

;;;
;;; Database of systems, indexed by name
;;;

(defvar *systems* (list) "List of all defined systems")

(defun lookup-system (name)
  (find name *systems* :key #'system-name :test #'string-equal))

(defun intern-system (system)
  (pushnew system *systems*))

(defun unintern-system (system)
  (setq *systems* (remove system *systems*))
  system)



;;;
;;; Loading system definitions on demand.  There are two ways to find the sysdcl
;;; file for a system.  It might be explicitly defined by the function
;;; SET-SYSTEM-SOURCE-FILE, or it my be implicitly defined by storing in the the
;;; file <system-name>-sysdcl in one of the sysdcl directories.
;;;

(defvar *sysdcl-pathname-defaults* (lisp:pathname "/import/lisp/sysdcl/")
  "List of default pathnames used to locate system declaration files")

(defparameter *sysdcl-name-format-string* "~a-sysdcl")

(defun system-source-file (name)
  "Return the pathname of the file that contains the definition of the system
named NAME."
  ;; First see if there is an explicitly provided defn file
  (let ((system (lookup-system name)))
    (when system
      (return-from system-source-file (system-declared-defining-file system))))
  ;;
  (let ((defaults-list (if (listp *sysdcl-pathname-defaults*)
			   *sysdcl-pathname-defaults*
			   (list *sysdcl-pathname-defaults*))))
    ;; Then see if there is a defn file in the default location
    (dolist (defaults defaults-list)
      (let ((file (lisp:make-pathname :name (format nil *sysdcl-name-format-string* name)
				      :defaults defaults)))
	;; Don't return the result of the PROBE-FILE, since that might contain
	;; dereferenced directory stuff, instead of the "real" name.
	(when (probe-file file)
	  (return-from system-source-file file))))
    ;; But because of UN*X, also check the lower-cased version of the name
    (dolist (defaults defaults-list)
      (let ((file (lisp:make-pathname :name (let ((*print-case* :downcase))
					      (format nil *sysdcl-name-format-string* name))
				      :defaults defaults)))
	(when (probe-file file)
	  (return-from system-source-file file))))
    ;; If still no result, enumerate the default directory and look for a file
    ;; whose name matches.  This is a last resort since it is bound to be slow.
    (dolist (defaults defaults-list)
      (let* ((sysdcl-name (format nil *sysdcl-name-format-string* name))
	     (files (remove-if #'(lambda (p)
				   (not (equalp (pathname-name p) sysdcl-name)))
			       (directory defaults))))
	(when files
	  (return-from system-source-file (lisp:make-pathname :name (pathname-name (car files))
							      :defaults defaults)))))))

(defun set-system-source-file (name pathname)
  "Declare that the file named PATHNAME contains the defintion of the system
named NAME."
  (let ((system (lookup-system name)))
    (when (null system)
      (setq system (make-system :name name))
      (intern-system system))
    (setf (system-declared-defining-file system) pathname))
  pathname)

(defsetf system-source-file set-system-source-file)


;; Load the system defintion from a file.  If the file doesn't define the
;; system, signal an error.
(defun load-sysdcl-file (file system-name)
  (let ((old-system (lookup-system system-name))
	(ok nil))
    (unwind-protect
	 (progn
	   (when old-system
	     (unintern-system old-system))
	   (load file)
	   (setq ok t))
      (unless ok
	(when old-system
	  (intern-system old-system)))))
  (let ((new-system (lookup-system system-name)))
    (if new-system
	new-system
	(error "The file ~A doesn't define the system ~S" file system-name))))

;; Given a system name, return the system definition.  If the definition needs
;; to be loaded, load it.  If the def is out-of-date, load the new definition.
(defun find-system (name)
  (let ((system (lookup-system name))
	(sysdcl-file (system-source-file name)))
    (cond ((and (null system) (null sysdcl-file))
	   (error "No system description named ~a found." name))
	  ((null sysdcl-file)
	   system)
	  ((or (null system)
	       (not (system-definition-loaded-p system)))
	   (when *load-verbose*
	     (format t "~&;;; Autoloading definition of system ~A" name))
	   (load-sysdcl-file sysdcl-file name)
	   (lookup-system name))
	  ((or (null (system-defining-file-write-date system))
	       (null (probe-file sysdcl-file))
	       (null (file-write-date sysdcl-file)))
	   system)
	  ((= (system-defining-file-write-date system)
	      (file-write-date sysdcl-file))
	   system)
	  (t (when *load-verbose*
	       (format t "~&;;; System definition file changed - ~
                              ~<~%;;;~10T~:;reloading definiton of system ~A~>"
		       name))
	     (load-sysdcl-file sysdcl-file name)
	     (lookup-system name)))))


(defun load-system-def (name &optional (reload nil))
  "Load the system definition for the system NAME, and return the system.
If the system is already defined, don't load the (possibly new) definition
unless RELOAD is true.  Returns NIL if the system definition can't be found."
  (let ((system (lookup-system name)))
    (if (and system (system-definition-loaded-p system) (not reload))
	system
	(let ((sysdcl-file (system-source-file name)))
	  (if (null sysdcl-file)
	      nil
	      (progn
		(load-sysdcl-file sysdcl-file name)
		(lookup-system name)))))))


;;;
;;; Computing list of all required subsystems of a system
;;;


;; List of all subsystems (including these systems) in load order.
(defun expand-subsystems (systems &optional
				  (subsystems-function #'(lambda (s)
							   (mapcar #'find-system
								   (system-needed-systems s)))))
  (labels ((merge-subsystems (systems pending result)
	     (cond ((null systems) result)
		   (t (merge-subsystems (cdr systems)
					pending
					(merge-system (car systems)
						      pending
						      result)))))
	   (merge-system (system pending result)
	     (cond ((member system result) result)
		   ((member system pending)
		    (recursive-system-error system))
		   (t (cons system
			    (merge-subsystems (funcall subsystems-function system)
					      (cons system pending)
					      result))))))
    (reverse (merge-subsystems systems nil nil))))

;;;
;;; Defining systems
;;;

(defun define-system (system module-list)
  ;; Fill in the modules, resolving module-names
  (labels ((find-module-named (name)
	     (or (find (string name)
		       module-list :key #'module-name :test #'path-equal)
		 (error "No module named ~a in system ~s" name system)))
	   (canonicalize-module-list (module mlist)
	     (if (eq t mlist)
		 (subseq module-list 0 (position module module-list))
		 (mapcar #'find-module-named (module-load-before-compile module)))))
    (dolist (module module-list)
      (setf (module-system module)
	    system
	    (module-load-after-list module)
	    (canonicalize-module-list module (module-load-after module))
	    (module-load-before-compile module)
	    (canonicalize-module-list module (module-load-before-compile module)))))
  (setf (system-module-list system) module-list)
  ;; Install this system
  (let* ((old-system (lookup-system (system-name system)))
	 (old-module-list (and old-system (system-module-list old-system)))
	 (source-file (current-source-file)))
    (flet ((retain-module-loaded-information (module)
	     (let ((old-module (find (module-name module) old-module-list
				     :key #'module-name :test #'path-equal)))
	       (when old-module
		 (setf (module-load-date module) (module-load-date old-module)
		       (module-loaded-from-file module) (module-loaded-from-file old-module)
		       (module-loaded-p module) (module-loaded-p old-module))))))
      (when old-module-list
	(mapc #'retain-module-loaded-information module-list)))
    (when old-system (unintern-system old-system))
    (setf (system-definition-loaded-p system) t)
    (when source-file
      (setf (system-defining-file system)
	    source-file
	    (system-defining-file-write-date system)
	    (file-write-date source-file))))
  (intern-system system)
  system)


(defmacro defsystem (name (&key default-pathname default-binary-pathname
				default-package needed-systems
				load-before-compile default-optimizations
				patch-file-pattern)
			  &rest module-list)
  "Define the system NAME.  Only the default-pathname option and the module
pathname fields are evaluated."
  (flet ((expand-module-descr (name &key compile-satisfies-load (load-after t)
				    load-before-compile optimizations (language :lisp)
				    (features t) eval-after pathname binary-pathname
				    package binary-only)
	   `(make-module :name ',(pretty-pathname-component (string name))
			 :pathname ,(pretty-pathname-component pathname)
			 :binary-pathname ,(pretty-pathname-component binary-pathname)
			 :compile-satisfies-load ',compile-satisfies-load
			 :load-after ',load-after
			 :load-before-compile ',load-before-compile
			 :optimizations ',optimizations
			 :language ',language
			 :features ',features
			 :eval-after ',eval-after
			 :package ',package
			 :binary-only ',binary-only)))
    `(define-system (make-system :name ',name
				 :default-pathname ,default-pathname
				 :default-binary-pathname ,default-binary-pathname
				 :default-package ',default-package
				 :needed-systems ',needed-systems
				 :load-before-compile ',load-before-compile
				 :default-optimizations ',default-optimizations
				 :patch-file-pattern ',patch-file-pattern)
	 (list
	  ,@(mapcar #'(lambda (descr)
			(apply #'expand-module-descr descr)) module-list)))))


(defun undefsystem (system-name)
  "Undefine the system with the specified name"
  (unintern-system (find-system system-name)))


(defun recursive-system-error (system)
  (error "System ~a recursivly depends on itself!" (system-name system)))

;;;
;;; Load-system
;;;


;; The following are used to prevent multiple compilations/loads from occuring
;; during a single call to LOAD-SYSTEM or COMPILE-SYSTEM.
(eval-when (eval compile)
  (proclaim '(special *loaded-systems* *loaded-modules* *compiled-systems*
	      *compiled-modules* *tracep*)))

#-(and ANSI-90 (not Allegro) (not Cloe))
(#+Allegro excl::without-package-locks #-Allegro progn
	   (defun translate-logical-pathname (path) path))

;; Evaluate the BODY with the reader context and default pathname set up as
;; defined by the module.
(defmacro in-module-env ((module) &body body)
  (let ((mod (gensym))
	(path (gensym)))
    `(let* ((,mod ,module)
	    (,path (module-src-path ,mod))
	    (*package* (find-package (or (module-package ,mod)
					 (system-default-package (module-system ,mod))
					 (package-name *package*))))
	    #-Allegro
	    (*default-pathname-defaults*
             ;; Some systems may not like a logical pathname as a default.
             ;; Specifically, Macintosh CL 2.0a3.
             (translate-logical-pathname
              (lisp:make-pathname
               :host (pathname-host ,path)
               :device (pathname-device ,path)
               :directory (lisp:pathname-directory ,path)
               :name "foo"
               :type "fasl"
               :defaults *default-pathname-defaults*))))
       ,@body)))


(defun load-system (name &key (reload nil)
			      (trace nil)
			      (recurse nil)
			      (source-if-newer nil))
  "Load the system NAME.  If RELOAD is true, load the modules in the system even
if the in-core version appears to be up-to-date.  If RECURSE is true, recursively
verify that each already loaded subsystem is up-to-date, reloading it if need be."
  (let ((*loaded-systems* nil)
	(*tracep* trace))
    (load-subsystems (list (find-system name)) reload recurse source-if-newer)
    name))

(defun load-subsystems (systems reload recurse source-if-newer)
  (labels ((subsystems (s)
	     (mapcar #'find-system (system-needed-systems s)))
	   (loadablep (s)
	     (or recurse
		 (not (system-loaded-p s))
		 (member s systems)))
	   (loadable-subsystems (s)
	     (remove-if-not #'loadablep (subsystems s))))
    (dolist (sys (expand-subsystems systems #'loadable-subsystems))
      (load-system-internal sys reload source-if-newer))))

(defun load-system-internal (system reload source-if-newer)
  (unless (member system *loaded-systems* :test #'eq)
    (let* ((name (system-name system))
	   (*loaded-modules* nil)
	   (*current-system* name))
      #+CCL-2 (ccl:set-mini-buffer ccl:*top-listener* "Loading system ~A."
                                   *current-system*)
      (if *tracep*
	  (format t "~&;;; -- Would load system ~a" name)
	  (when *load-verbose*
	    (format t "~&;;; Loading system ~A" name)))
      ;; Load the modules that make up the system
      (dolist (module (system-module-list system))
	(load-module module reload source-if-newer))
      ;; Update indicators to show the system has been loaded
      (unless *tracep* (setf (system-loaded-p system) t))
      (push system *loaded-systems*)
      (when (and (not *tracep*) *load-verbose*)
	(format t "~&;;; done loading system ~A" name)))))


;; Returns TRUE iff it actually loaded the module
(defun load-module (module reloadp source-if-newer)
  (cond ((not (module-applicable-p module))
	 nil)
	((member module *loaded-modules* :test #'eq)
	 nil)
	((or reloadp (not (module-up-to-date-p module)))
	 ;; Load the modules that must preceed this one
	 (dolist (m (module-load-after-list module))
	   (load-module m reloadp source-if-newer))
	 ;;
	 (let ((pathname (module-loadable-path module source-if-newer)))
	   (in-module-env (module)
	     (cond (*tracep* (format t "~&;;; -- Would load ~A" pathname))
		   (t (funcall (language-load-fn (find-language (module-language module)))
			pathname)
		      (setf (module-loaded-p module) t
			    (module-load-date module) (file-write-date pathname)
			    (module-loaded-from-file module) pathname)))
	     (push module *loaded-modules*)
	     (execute-module-hooks module)))
	 t)
	(t (push module *loaded-modules*) ;Cache the info that it's up-to-date
	   nil)))

(defun execute-module-hooks (module)
  (when (module-eval-after module)
    (in-module-env (module)
      (if *tracep*
	  (format t "~&;;; -- Would eval ~S"
		  (module-eval-after module))
	  (eval (module-eval-after module))))))

;;;
;;; compile-system
;;;

(defun compile-system (name &key (reload nil)
			         (recompile nil)
				 (include-components t)
				 (propagate nil)
				 (trace nil))
  (let ((*compiled-systems* nil)
	(*loaded-systems* nil)
	(*tracep* trace))
    (with-delayed-compiler-warnings
	(compile-subsystems (list (find-system name))
			    reload recompile propagate include-components))
    name))

(defun compile-subsystems (systems reload recompile propagate include-components)
  (labels ((subsystems (s)
	     (mapcar #'find-system
		     (append (if (eq t (system-load-before-compile s))
				 nil	;Just the needed-systems, thank you
				 (system-load-before-compile s))
			     (system-needed-systems s))))
	   (compilablep (s)
	     (or propagate
		 (member s systems)))
	   (compilable-subsystems (s)
	     (remove-if-not #'compilablep (subsystems s))))
    (dolist (sys (expand-subsystems systems #'compilable-subsystems))
      (compile-system-internal sys reload recompile include-components))))

(defun compile-system-internal (system reload recompile include-components)
  (let ((name (system-name system))
	(pre-compile-fn-run-p nil))
    (flet ((pre-compile-fn ()
	     (unless pre-compile-fn-run-p
	       ;; Make sure required subsystems are loaded
	       (when include-components
		 (let ((required-subsystems (system-load-before-compile system)))
		   (when (eq required-subsystems t)
		     (setq required-subsystems (system-needed-systems system)))
		   (load-subsystems (mapcar #'find-system required-subsystems)
				    reload nil nil)))
	       (format t "~&;;; Compiling system ~a" name)
	       (setq pre-compile-fn-run-p t)))
	   (post-compile-fn ()
	     (when pre-compile-fn-run-p
	       (format t "~&;;; done compiling system ~a" name))))
      (unless (member system *compiled-systems* :test #'eq)
	;; Compile the modules that make up the system
	(let ((*compiled-modules* nil)
	      (*loaded-modules* nil)
	      (*current-system* name))
          #+CCL-2 (ccl:set-mini-buffer ccl:*top-listener* "Compiling system ~A."
                                       *current-system*)
	  (dolist (module (system-module-list system))
	    (compile-module module reload recompile #'pre-compile-fn)))
	;; Update info about what systems have been compiled
	(push system *compiled-systems*)
	(setf (system-loaded-p system) nil)
	(setq *loaded-systems* (remove system *loaded-systems* :test #'eq))
	(post-compile-fn)))))

(defvar *load-all-before-compile* nil
  "If true, all previous modules will be loaded before a module is compiled.")

(defun compile-module (module reload recompile pre-compile-fn)
  (flet ((file-write-date-or-nil (file)
	   (if (probe-file file) (file-write-date file) nil)))
    (unless (or (not (module-applicable-p module))
		(module-binary-only module)
		(member module *compiled-modules* :test #'eq))
      (let* ((s-pathname (module-src-path module))
	     (b-pathname (module-bin-path module))
	     (s-date (file-write-date-or-nil s-pathname))
	     (b-date (file-write-date-or-nil b-pathname)))
	(when (and (null s-date)
		   (null b-date))
	  (error "No source or binary for ~s" module))
	(when (and (null s-date)
		   (not (module-binary-only module)))
	  (error "No source for ~s" module))
	(when (or recompile
		  (null b-date)
		  (< b-date s-date)
		  (find-if #'(lambda (m)
			       (let ((s (file-write-date-or-nil (module-src-path m))))
				 (or (null s)
				     (< b-date s))))
			   (module-load-before-compile module)))
	  ;; This module needs to be recompiled
	  (funcall pre-compile-fn)
	  (if *load-all-before-compile*
	      (dolist (dep-module (module-load-after-list module))
		(compile-module dep-module reload recompile #'values)
		(load-module dep-module reload nil))
	      (dolist (dep-module (module-load-before-compile module))
		(compile-module dep-module reload recompile #'values)
		(load-module dep-module reload nil)))
	  (in-module-env (module)
	    (cond (*tracep* (format t "~&;;; -- Would compile ~A" s-pathname))
		  (t (funcall (language-compile-fn (find-language (module-language module)))
		       s-pathname b-pathname
		       (or (module-optimizations module)
			   (system-default-optimizations (module-system module))))
		     (setf (module-loaded-p module) (module-compile-satisfies-load module)))))
	  (if (module-compile-satisfies-load module)
	      (push module *loaded-modules*)
	      (setq *loaded-modules* (remove module *loaded-modules* :test #'eq)))
	  (push module *compiled-modules*)
	  ;; return file-write-date for binary
	  (file-write-date-or-nil b-pathname))))))

;;; Describe and show

(defun format-time (time &optional (stream *standard-output*))
  (let (second minute hour date month year day daylight-savings-p time-zone)
    (multiple-value-setq
	(second minute hour date month year day daylight-savings-p time-zone)
      (get-decoded-time))
    (multiple-value-setq
	(second minute hour date month year day daylight-savings-p time-zone)
      (if (<= 5 time-zone 8)		;US-centric, to be sure
	  (decode-universal-time time)
	  (decode-universal-time time 0)))
    (princ (nth day
		'("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
	   stream)
    (princ " " stream)
    (princ (nth (1- month)
		'("Jan" "Feb" "Mar" "Apr" "May" "June"
		  "July" "Aug" "Sept" "Oct" "Nov" "Dec"))
	   stream)
    (format stream " ~2D ~2D:~2,'0D:~2,'0D " date hour minute second)
    (cond ((= time-zone 0)
	   (princ "GMT" stream))
	  (t (princ (nth (- time-zone 5)
			 '("E" "C" "M" "P"))
		    stream)
	     (princ (if daylight-savings-p "D" "S") stream)
	     (princ "T")))
    (format stream " ~4D" year)))

(defun describe-system (system)
  (format t "~&; system: ~a" (system-name system))
  (cond ((system-definition-loaded-p system)
	 (when (system-needed-systems system)
	   (format t "~&; needed systems:~{ ~A~}"
		   (system-needed-systems system)))
	 (format t "~&; default package: ~a" (or (system-default-package system)
						 "None"))
	 (format t "~&; default pathname: ~a" (or (system-default-pathname system)
						  "None"))
	 (when (system-default-binary-pathname system)
	   (format t "~&; default binary pathname: ~a"
		   (system-default-binary-pathname system)))
	 (when (system-load-before-compile system)
	   (format t "~&; Load systems before compile:~{ ~a~}"
		   (system-load-before-compile system)))
	 (when (system-default-optimizations system)
	   (format t "~&; default optimizations ~s" (system-default-optimizations system)))
	 (when (system-declared-defining-file system)
	   (format t "~&; definition declared to reside in file ~a"
		   (system-declared-defining-file system)))
	 (when (system-defining-file system)
	   (format t "~&; definition loaded from ~A"
		   (system-defining-file system)))
	 (when (system-defining-file-write-date system)
	   (format t "~&; definition loaded ")
	   (format-time (system-defining-file-write-date system)))
	 (format t "~&;")
	 (dolist (module (system-module-list system))
	   (describe-module module)))
	(t
	 (format t "~&; definition declared to reside on file ~A"
		 (system-declared-defining-file system))
	 (format t "~&; System defintion is not currently loaded"))))

(defun describe-module (module)
  (format t "~&; module: ~a" (module-name module))
  (when (module-package module)
    (format t "~&;~5Tpackage: ~s" (module-package module)))
  (when (module-loaded-p module)
    (format t "~&;~5Tloaded from file ~a~%;~16Tdated " (module-loaded-from-file module))
    (format-time (module-load-date module)))
  (when (module-pathname module)
    (format t "~&;~5Tpathname: ~a" (module-pathname module)))
  (when (module-binary-pathname module)
    (format t "~&;~5Tbinary pathname: ~a" (module-binary-pathname module)))
  (when (module-compile-satisfies-load module)
    (format t "~&;~5TCompile satisfies load"))
  (unless (eq (module-language module) :lisp)
    (format t "~&;~5Tlanguage: ~a" (module-language module)))
  (when (module-optimizations module)
    (format t "~&;~5Toptimizations: ~s" (module-optimizations module)))
  (when (module-binary-only module)
    (format t "~&;~5Tdeclared binary only"))
  (when (module-load-before-compile module)
    (format t "~&;~5Tload-before-compile:")
    (dolist (m (module-load-before-compile module))
      (format t " ~a" (module-name m))))
  (when (consp (module-load-after module))
    (format t "~&;~5Tload-after:")
    (dolist (m (module-load-after module))
      (format t " ~a" m)))
  (when (module-eval-after module)
      (format t "~&;~5Teval-after: ~s" (module-eval-after module)))
  (unless (eq t (module-features module))
    (format t "~&;~5Tapplies only to lisps with one of features: ~s"
	    (module-features module))))

(defun show-system (name)
  "show a system definition in detail"
  (describe-system (or (lookup-system name)
		       (error "No system description named ~a loaded." name))))

#+(or Genera Minima-Developer)
(export '(import-into-sct))

#+(or Genera Minima-Developer)
(defun import-into-sct (system &key (sct-name system) (subsystem nil)
				    (default-pathname nil) (default-destination-pathname nil)
				    (pretty-name nil))
  (setf sct-name (zl:::sct:canonicalize-system-name sct-name))
  (setf system (lookup-system system))
  (when (null default-pathname)
    (setf default-pathname (system-default-pathname system)))
  (when (null default-destination-pathname)
    (setf default-destination-pathname (system-default-binary-pathname system)))
  (when #+Minima t #-Minima (sys:record-source-file-name sct-name 'zl:::sct:defsystem)
    (zl:::sct:define-system-internal
      sct-name
      (if subsystem 'zl:::sct:subsystem 'zl:::sct:system)
      `(:default-pathname ,default-pathname
	,@(when default-destination-pathname
	    `(:default-destination-pathname ,default-destination-pathname))
	,@(when (system-default-package system)
	    `(:default-package ,(system-default-package system)))
	,@(when pretty-name
	    `(:pretty-name ,pretty-name))
	)
      (mapcar #'(lambda (module)
		  (flet ((make-name (module)
			   (intern (string-upcase (module-name module)))))
		    `(:module ,(make-name module)
		      (,(module-name module))
		      (:type ,(ecase (module-language module)
				(:lisp
				  (if (module-applicable-p module)
				      #+Minima :minima-lisp #-Minima :lisp
				      :lisp-example))))
		      ,@(unless (eq (module-language module) :lisp)
			  `((:type ,(module-language module))))
		      ,@(when (module-load-after-list module)
			  `((:in-order-to (:compile :load)
			     (:load ,@(mapcar #'make-name (module-load-after-list module))))))
		      ,@(when (module-load-before-compile module)
			  `((:uses-definitions-from
			      ,@(mapcar #'make-name (module-load-before-compile module)))))
		      )))
	      (system-module-list system)))))

(defun get-compiler-speed-and-safety ()
  #+Allegro (values comp::.speed. comp::.safety.)
  #+Genera (values (lt:optimize-state 'speed) (lt:optimize-state 'safety))
  #+Lucid (values (cdr (assoc 'speed lucid::*compiler-optimizations*))
		  (cdr (assoc 'safety lucid::*compiler-optimizations*)))
  ;; Allegro 4.0 doesn't comply with this.  Maybe 4.1
  #+CCL-2
  (let ((opts (declaration-information 'optimize)))
    (values (second (assoc 'speed opts))
	    (second (assoc 'safety opts))))
  #-(or Allegro Genera Lucid CCL-2) (values nil nil))

(defmacro with-compiler-options ((&key speed safety) &body body)
  `(multiple-value-bind (old-speed old-safety) (get-compiler-speed-and-safety)
     (unless (and old-speed old-safety)
       (warn "You have not provided a version of ~S for this~%implementation; ~
	      the ~:[speed ~;~]~:[and ~;~]~:[safety ~;~]optimization declaration~:[s~;~] ~
	      will not be changed."
	     'get-compiler-speed-and-safety
	     old-speed (or old-speed old-safety)
	     old-safety (or old-speed old-safety)))
     (unwind-protect
	 (progn (proclaim `(optimize ,@(and old-speed ,speed `((speed ,,speed)))
				     ,@(and old-safety ,safety `((safety ,,safety)))))
		,@body)
       (proclaim `(optimize ,@(and old-speed ,speed `((speed ,old-speed)))
			    ,@(and old-safety ,safety `((safety ,old-safety))))))))


;;; Write (Franz only?) Makefile description for constructiong big FASL
;;; from system definition.  20-21 August 1990 by Richard Lamson

(defun write-Makefile-for-system (system)
  (unless (system-p system)
    (setf system (find-system system)))
  (let* ((up-pathname (make-pathname :directory '(:relative :up)))
	 (default-pathname
	   ;; The DEFSYSTEM:: version of MAKE-PATHNAME is broken!
	   (lisp:make-pathname :defaults (merge-pathnames up-pathname
							  (or (system-default-pathname system)
							      (system-defining-file system)))
			       :name nil :type nil :version nil)))
    (with-open-file (stream (lisp:make-pathname :defaults default-pathname
						:name "Makefile"
						;; --- Unclear this will work on Unix.
						:type "")
			    :direction :output)
      (write-Makefile-for-system-internal stream system default-pathname)
      (pathname stream))))

(defun write-Makefile-for-system-internal (stream system default-pathname)
  (multiple-value-bind (sec min hr day mon yr) (get-decoded-time)
    (format stream "#-*- Mode: Text; Nofill: t~%#~%# Makefile for system ~A; ~
			 created ~2,'0D/~2,'0D/~2,'0D ~2,'0D:~2,'0D:~2,'0D by a program.~%~
		       #  --- Please do not edit by hand.  ~%#~%"
	    (system-name system) mon day yr hr min sec))
  (let* ((bin-type (let ((type (second lisp-file-types)))
		     (if (consp type) (first type) type)))
	 (fasl-file-name (format nil "~(~A~).~A" (system-name system) bin-type))
	 (system-lists (write-pathname-list-for-system stream system default-pathname)))
    (format stream "~2%doit:~%~8@Trm -f ~A~%~8@T@echo Finding binary files ...~%"
	    fasl-file-name)
    (format stream "~8@T@(X=; for file in \\~{~%~8@T$(~A)~^ \\~};~
		    do if test -f $$file; then X=\"$$X $$file\"; ~
		       else echo No match for: $$file; fi; done;\\~%~
		~8@Techo making ~A... ; cat $$X > ~A; echo \" done.\"~2%"
	    system-lists fasl-file-name fasl-file-name)))

(defun write-pathname-list-for-system (stream system default-pathname)
  (let ((*done-systems* nil))
    (declare (special *done-systems*))
    (write-pathname-list-for-system-internal stream system default-pathname)))

(defun write-pathname-list-for-system-internal (stream system default-pathname)
  (declare (special *done-systems*))
  (when (member system *done-systems*)
    (return-from write-pathname-list-for-system-internal nil))
  (push system *done-systems*)
  (append (mapcan #'(lambda (system) (write-pathname-list-for-system-internal
				       stream system default-pathname))
		  (mapcar #'find-system (system-needed-systems system)))
	  (and (not (and (null (system-module-list system))
			 (null (system-patch-file-pattern system))))
	       (let* ((file-list-name (format nil "~(~A-files~)" (system-name system)))
		      (indentation (+ (length file-list-name) 3)))
		 (format stream "~2%~A = ~?~@[ \\~%~A~]"
			 file-list-name (format nil "~~@{~~A~~^ \\~%~V@T~~}" indentation)
			 (mapcar #'(lambda (module)
				     (enough-namestring (module-bin-path module)
							default-pathname))
				 (remove-if-not
				   ;; Only those modules which actually
				   ;; apply and are Lisp files.
				   #'(lambda (module)
				       (and (eq (module-language module) :lisp)
					    (module-applicable-p module)))
				   (system-module-list system)))
			 (let ((pfp (system-patch-file-pattern system))
			       (bin-type (let ((type (second lisp-file-types)))
					   (if (consp type) (first type) type))))
			   (and pfp
				(format nil "~VT~A.~A" indentation pfp bin-type))))
		 (list file-list-name)))))

