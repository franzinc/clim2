;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10; Package: CL-USER; Lowercase: Yes -*-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca.  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, Ca.  All rights reserved.
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
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;

;; $fiHeader: packages.lisp,v 1.1 91/08/30 13:57:48 cer Exp Locker: cer $

(in-package #-ansi-90 :user #+ansi-90 :common-lisp-user)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved.
Copyright (c) 1991, Franz Inc. All rights reserved
 Portions copyright (c) 1990 International Lisp Associates."

;;; define the clim-lisp package, a package designed to mimic ansi common lisp
;;; as closely as possible (including clos).

(#-ansi-90 clim-lisp::defpackage #+ansi-90 defpackage :clim-lisp

 (:use #-ansi-90 :lisp #+ansi-90 :common-lisp
       #+excl :clos
       #+(and clim-uses-lisp-streams excl) :stream)
 
 #-ansi-90
 (:shadowing-import-from #+pcl :pcl
			 #+(and (not pcl) (or genera cloe-runtime)) :clos-internals
			 #+(and (not pcl) (not (or genera cloe-runtime))) :clos
   defstruct
   documentation
   setf)

 #-ansi-90
 (:import-from #+pcl :pcl #-pcl :clos
   add-method
   allocate-instance
   built-in-class
   call-method
   call-next-method
   change-class
   class
   class-direct-subclasses
   class-direct-superclasses
   class-name
   class-of
   class-precedence-list
   class-prototype
   #+pcl classp
   compute-applicable-methods
   defclass
   defgeneric
   define-method-combination
   defmethod
   ensure-generic-function
   find-class
   find-method
   funcallable-standard-class
   function-keywords
   generic-flet
   generic-function
   generic-labels
   initialize-instance
   invalid-method-error
   make-instance
   make-instances-obsolete
   make-load-form
   make-method
   method
   method-combination
   method-combination-error
   method-qualifiers
   next-method-p
   no-applicable-method
   no-next-method
   print-object
   reinitialize-instance
   remove-method
   shared-initialize
   slot-boundp
   slot-exists-p
   slot-makunbound
   slot-missing
   slot-unbound
   slot-value
   standard
   standard-class
   standard-generic-function
   standard-method
   standard-object
   structure-class
   structure-object
   symbol-macrolet
   update-instance-for-different-class
   update-instance-for-redefined-class
   with-accessors
   with-slots)

 #+(and ansi-90 (not ccl-2))
 (:import-from #+pcl pcl #-pcl clos
   class-direct-subclasses 
   class-direct-superclasses 
   class-precedence-list 
   class-prototype
   funcallable-standard-class)

 #+ccl-2
 (:import-from ccl
   class-direct-subclasses
   class-direct-superclasses
   class-precedence-list
   class-prototype
   class)

 #-ansi-90
 (:shadowing-import-from #+lcl4.0 lucid-common-lisp 
			 #+excl common-lisp
			 #-(or excl lcl4.0) conditions
   *break-on-signals*				;conditions v18
   *debugger-hook*				;conditions v18
   abort					;conditions v18
   arithmetic-error-operands			;conditions v18
   arithmetic-error-operation			;conditions v18
   arithmetic-error				;conditions v18
   assert					;conditions v18 (supersedes cltl)
   break					;conditions v18 (supersedes cltl)
   ccase					;conditions v18 (supersedes cltl)
   cell-error					;conditions v18
   cell-error-name				;conditions v18
   cerror					;conditions v18 (supersedes cltl)
   check-type					;conditions v18 (supersedes cltl)
   compute-restarts				;conditions v18
   condition					;conditions v18
   continue					;conditions v18
   control-error				;conditions v18
   ctypecase					;conditions v18 (supersedes cltl)
   #-lucid define-condition			;conditions v18
   division-by-zero				;conditions v18
   ecase					;conditions v18 (supersedes cltl)
   end-of-file					;conditions v18
   error					;conditions v18 (supersedes cltl)
   etypecase					;conditions v18 (supersedes cltl)
   file-error-pathname				;conditions v18
   file-error					;conditions v18
   find-restart					;conditions v18
   floating-point-inexact			;issue floating-point-condition-names
   floating-point-invalid-operation		;issue floating-point-condition-names
   floating-point-overflow			;conditions v18
   floating-point-underflow			;conditions v18
   handler-bind					;conditions v18
   handler-case					;conditions v18
   ignore-errors				;conditions v18
   invoke-debugger				;conditions v18
   invoke-restart-interactively			;conditions v18
   invoke-restart				;conditions v18
   make-condition				;conditions v18
   muffle-warning				;conditions v18
   package-error-package			;conditions v18
   package-error				;conditions v18
   parse-error					;issue reader-error
   print-not-readable-object			;issue data-io
   print-not-readable				;issue data-io
   program-error				;conditions v18
   restart-bind					;conditions v18
   restart-case					;conditions v18
   restart-name					;conditions v18
   restart					;conditions v18
   serious-condition				;conditions v18
   signal					;conditions v18
   simple-condition-format-arguments		;conditions v18
   #-ansi-90 simple-condition-format-string
   #+ansi-90 simple-condition-format-control
   simple-condition				;conditions v18
   simple-error					;conditions v18
   simple-type-error				;conditions v18
   simple-warning				;conditions v18
   storage-condition				;conditions v18
   store-value					;conditions v18
   stream-error-stream				;conditions v18
   stream-error					;conditions v18
   style-warning				;issue compiler-diagnostics
   type-error-datum				;conditions v18
   type-error-expected-type			;conditions v18
   type-error					;conditions v18
   unbound-slot-instance			;issue undefined-variables-and-functions
   unbound-slot					;issue undefined-variables-and-functions
   unbound-variable				;conditions v18
   undefined-function				;conditions v18
   use-value					;conditions v18
   warning					;conditions v18
   warn						;conditions v18 (supersedes cltl)
   with-simple-restart)				;conditions v18

 #+lucid (:shadow define-condition)

 #+(and genera (not ansi-90))
 (:import-from future-common-lisp
   declaim
   define-compiler-macro
   defpackage
   dynamic-extent
   print-unreadable-object
   *print-readably*
   *read-eval*
   real
   with-standard-io-syntax)

 #+(or genera cloe-runtime)
 (:import-from clos-internals
   compile-file-environment-p)

 #+cloe-runtime
 (:import-from cloe
   with-standard-io-syntax)

 #+lcl4.0
 (:import-from lucid-common-lisp
   dynamic-extent)

 #+excl
 (:import-from excl
   dynamic-extent)

 ;; stream proposal
 #+(and (not cloe-runtime) (not clim-uses-lisp-streams))
 (:shadow
   input-stream-p
   output-stream-p
   open-stream-p
   streamp
   stream-element-type
   close
   pathname
   truename)

 ;; stream proposal
 #+excl
 (:shadow
   pathname
   truename)

 #+(and clim-uses-lisp-streams ccl-2)
 (:shadow
   input-stream-p
   output-stream-p
   open-stream-p
   streamp
   ;; stream-element-type
   close
   pathname
   truename)

 ;; in genera and cloe the i/o functions are integrated, don't shadow
 #-(or genera cloe-runtime clim-uses-lisp-streams)
 (:shadow
   ;; cl stream input functions
   read-byte
   read-char
   unread-char
   read-char-no-hang
   peek-char
   listen
   read-line
   clear-input
   ;; cl stream output functions
   write-byte
   write-char
   write-string
   terpri
   fresh-line
   force-output
   finish-output
   clear-output
   format
   with-open-stream)

 ;; import these symbols so that we can define methods for them.
 #+(and clim-uses-lisp-streams ccl-2)
 (:import-from ccl
  stream-force-output
  stream-clear-input
  stream-fresh-line
  stream-listen)

 #+cloe-runtime
 (:import-from system 
  stream-read-char
  stream-unread-char
  stream-read-char-no-hang
  stream-peek-char
  stream-listen
  stream-read-line
  stream-clear-input

  stream-write-char
  stream-line-column
  stream-start-line-p
  stream-write-string
  stream-terpri
  stream-fresh-line
  stream-finish-output
  stream-force-output
  stream-clear-output
  stream-advance-to-column

  stream-read-byte
  stream-write-byte)

 (:export   
   ;; ansi common lisp
   *print-readably*
   *read-eval*
   define-compiler-macro
   defpackage
   dynamic-extent
   print-unreadable-object
   real
   with-standard-io-syntax

   ;; condition system
   *break-on-signals*				;conditions v18
   *debugger-hook*				;conditions v18
   abort					;conditions v18
   arithmetic-error-operands			;conditions v18
   arithmetic-error-operation			;conditions v18
   arithmetic-error				;conditions v18
   cell-error-name				;conditions v18
   cell-error					;conditions v18
   compute-restarts				;conditions v18
   condition					;conditions v18
   continue					;conditions v18
   control-error				;conditions v18
   define-condition				;conditions v18
   division-by-zero				;conditions v18
   end-of-file					;conditions v18
   file-error-pathname				;conditions v18
   file-error					;conditions v18
   find-restart					;conditions v18
   floating-point-inexact			;issue floating-point-condition-names
   floating-point-invalid-operation		;issue floating-point-condition-names
   floating-point-overflow			;conditions v18
   floating-point-underflow			;conditions v18
   handler-bind					;conditions v18
   handler-case					;conditions v18
   ignore-errors				;conditions v18
   invoke-debugger				;conditions v18
   invoke-restart-interactively			;conditions v18
   invoke-restart				;conditions v18
   make-condition				;conditions v18
   muffle-warning				;conditions v18
   package-error-package			;conditions v18
   package-error				;conditions v18
   parse-error					;issue reader-error
   print-not-readable-object			;issue data-io
   print-not-readable				;issue data-io
   program-error				;conditions v18
   restart-bind					;conditions v18
   restart-case					;conditions v18
   restart-name					;conditions v18
   restart					;conditions v18
   serious-condition				;conditions v18
   signal					;conditions v18
   simple-condition-format-arguments		;conditions v18
   #-ansi-90 simple-condition-format-string
   #+ansi-90 simple-condition-format-control
   simple-condition				;conditions v18
   simple-error					;conditions v18
   simple-type-error				;conditions v18
   simple-warning				;conditions v18
   storage-condition				;conditions v18
   store-value					;conditions v18
   stream-error-stream				;conditions v18
   stream-error					;conditions v18
   style-warning				;issue compiler-diagnostics
   type-error-datum				;conditions v18
   type-error-expected-type			;conditions v18
   type-error					;conditions v18
   unbound-slot-instance			;issue undefined-variables-and-functions
   unbound-slot					;issue undefined-variables-and-functions
   unbound-variable				;conditions v18
   undefined-function				;conditions v18
   use-value					;conditions v18
   warning					;conditions v18
   with-simple-restart				;conditions v18

   ;; stream proposal
   fundamental-stream
   fundamental-input-stream
   fundamental-output-stream
   fundamental-character-stream
   fundamental-binary-stream
   fundamental-character-input-stream
   fundamental-character-output-stream
   fundamental-binary-input-stream
   fundamental-binary-output-stream

   stream-read-char
   stream-unread-char
   stream-read-char-no-hang
   stream-peek-char
   stream-listen
   stream-read-line
   stream-clear-input

   stream-write-char
   stream-line-column
   stream-start-line-p
   stream-write-string
   stream-terpri
   stream-fresh-line
   stream-finish-output
   stream-force-output
   stream-clear-output
   stream-advance-to-column

   stream-read-byte
   stream-write-byte
   
   ;; clos
   ;; symbols from chapter 2
   add-method
   allocate-instance
   built-in-class
   call-method
   call-next-method
   change-class
   class
   class-name
   class-of
   compute-applicable-methods
   defclass
   defgeneric
   define-method-combination
   defmethod
   documentation
   ensure-generic-function
   find-class
   find-method
   function-keywords
   generic-flet
   generic-function
   generic-labels
   initialize-instance
   invalid-method-error
   make-instance
   make-instances-obsolete
   make-method
   method-combination-error
   method-qualifiers
   next-method-p
   no-applicable-method
   no-next-method
   print-object
   reinitialize-instance
   remove-method
   shared-initialize
   slot-boundp
   slot-exists-p
   slot-makunbound
   slot-missing
   slot-unbound
   slot-value
   standard-class
   standard-generic-function
   symbol-macrolet
   update-instance-for-different-class
   update-instance-for-redefined-class
   with-accessors
   with-slots
   ;; post-88-002r additions that would have been in chapter 2
   describe-object
   make-load-form
   make-load-form-saving-slots
   ;; not in hornig's chapter 2 list (?) but in both pcl and genera clos, and
   ;; used by silica
   class-direct-subclasses
   class-direct-superclasses
   class-precedence-list
   class-prototype
   funcallable-standard-class
   ;; somehow missing from the above
   standard-object
   compile-file-environment-p

   ;; cltl
   &allow-other-keys
   &aux
   &body
   &environment
   &key
   &optional
   &rest
   &whole
   *
   **
   ***
   #-ansi-90 *applyhook*
   #-ansi-90 *break-on-warnings*
   *debug-io*
   *default-pathname-defaults*
   *error-output*
   #-ansi-90 *evalhook*
   *features*
   *load-verbose*
   *macroexpand-hook*
   #-ansi-90 *modules*
   *package*
   *print-array*
   *print-base*
   *print-case*
   *print-circle*
   *print-escape*
   *print-gensym*
   *print-length*
   *print-level*
   *print-pretty*
   *print-radix*
   *query-io*
   *random-state*
   *read-base*
   *read-default-float-format*
   *read-suppress*
   *readtable*
   *standard-input*
   *standard-output*
   *terminal-io*
   *trace-output*
   +
   ++
   +++
   -
   //
   ///
   /=
   /
   1+
   1-
   <
   <=
   =
   >
   >=
   abs
   acons
   acos
   acosh
   adjoin
   adjust-array
   adjustable-array-p
   alpha-char-p
   alphanumericp
   and
   append
   #-ansi-90 applyhook
   apply
   apropos
   apropos-list
   aref
   array-dimension
   array-dimension-limit
   array-dimensions
   array-element-type
   array-has-fill-pointer-p
   array-in-bounds-p
   array-rank-limit
   array-rank
   array-row-major-index
   array-total-size-limit
   array-total-size
   arrayp
   array
   ash 
   asin
   asinh
   assert
   assoc-if
   assoc-if-not
   assoc
   atanh
   atan
   atom
   bignum
   bit-and bit-ior bit-xor bit-eqv bit-nand bit-nor
   bit-andc1 bit-andc2 bit-orc1 bit-orc2 bit-not
   bit-vector-p
   bit-vector
   bit
   block
   boole-clr boole-set boole-1 boole-2 boole-c1 boole-c2 boole-and boole-ior
   boole-xor boole-eqv boole-nand boole-nor boole-andc1 boole-andc2 boole-orc1 boole-orc2
   boole
   both-case-p
   boundp
   break
   butlast
   byte
   byte-position
   byte-size
   caaaar
   caaadr
   caaar
   caadar
   caaddr
   caadr
   caar
   cadaar
   cadadr
   cadar
   caddar
   cadddr
   caddr
   cadr
   call-arguments-limit
   car
   case
   catch
   ccase
   cdaaar
   cdaadr
   cdaar
   cdadar
   cdaddr
   cdadr
   cdar
   cddaar
   cddadr
   cddar
   cdddar
   cddddr
   cdddr
   cddr
   cdr
   ceiling
   cerror
   #-ansi-90 char-bit
   #-ansi-90 char-bits
   #-ansi-90 char-bits-limit
   char-code
   char-code-limit
   #-ansi-90 char-control-bit
   char-downcase
   char-equal
   #-ansi-90 char-font
   #-ansi-90 char-font-limit
   char-greaterp
   #-ansi-90 char-hyper-bit
   char-int
   char-lessp
   #-ansi-90 char-meta-bit
   char-name
   char-not-equal
   char-not-greaterp
   char-not-lessp
   #-ansi-90 char-super-bit
   char-upcase
   char/=
   char<
   char<=
   char=
   char>
   char>=
   character
   characterp
   char
   check-type
   cis
   clear-input
   clear-output
   close
   clrhash
   code-char
   coerce
   #-ansi-90 commonp
   #-ansi-90 common
   compilation-speed
   compile
   compile-file
   compiled-function-p
   compiled-function
   #-ansi-90 compiler-let
   complex
   complexp
   concatenate
   cond
   conjugate
   cons
   consp
   constantp
   copy-alist
   copy-list
   copy-readtable
   copy-seq
   copy-symbol
   copy-tree
   cos
   cosh
   count
   count-if
   count-if-not
   ctypecase
   decf
   declaim
   declaration
   declare
   decode-float
   decode-universal-time
   defconstant
   define-modify-macro
   define-setf-method
   defmacro
   defparameter
   defsetf
   defstruct
   deftype
   defun
   defvar
   delete-duplicates
   delete-file
   delete-if
   delete-if-not
   delete
   denominator
   deposit-field
   describe
   digit-char
   digit-char-p
   directory-namestring
   directory
   disassemble
   do
   do*
   do-all-symbols
   do-external-symbols
   do-symbols
   documentation
   dolist
   dotimes
   double-float-epsilon
   double-float-negative-epsilon
   double-float
   dpb
   dribble
   ecase
   ed
   eighth 
   elt
   encode-universal-time
   endp
   enough-namestring
   eq
   eql
   equalp
   equal
   error
   etypecase
   eval
   eval-when
   #-ansi-90 evalhook
   evenp
   every
   exp
   export
   expt
   fboundp
   fceiling
   ffloor
   fifth
   file-author
   file-length
   file-namestring
   file-position
   file-write-date
   fill
   fill-pointer
   find
   find-all-symbols
   find-if
   find-if-not
   find-package
   find-symbol
   finish-output
   first
   fixnum
   flet
   float-digits
   float-precision
   float-radix
   float-sign
   floatp
   float
   floor
   fmakunbound
   force-output
   format
   fourth
   fresh-line
   fround
   ftruncate
   ftype
   funcall
   function
   functionp
   gcd
   gensym
   gentemp
   get-decoded-time 
   get-dispatch-macro-character
   get-internal-real-time
   get-internal-run-time
   get-macro-character
   get-output-stream-string
   get-properties
   get-setf-method
   get-setf-method-multiple-value
   get-universal-time
   getf
   gethash
   get
   go
   graphic-char-p
   hash-table-count
   hash-table-p
   hash-table
   host-namestring
   identity
   if
   ignore
   imagpart
   import
   in-package
   incf
   inline
   input-stream-p
   inspect
   #-ansi-90 int-char
   integer-decode-float
   integer-length
   integerp
   integer
   internal-time-units-per-second
   intern
   intersection
   isqrt
   keywordp
   keyword
   labels
   lambda
   lambda-list-keywords
   lambda-parameters-limit
   last
   lcm
   ldb
   ldb-test
   ldiff
   least-negative-double-float
   least-negative-long-float
   least-negative-short-float
   least-negative-single-float
   least-positive-double-float
   least-positive-long-float
   least-positive-short-float
   least-positive-single-float
   length
   let
   let*
   lisp-implementation-type
   lisp-implementation-version
   list
   list*
   list-all-packages
   list-length
   listen
   listp
   load
   locally
   logandc1
   logandc2
   logand
   logbitp
   logcount
   logeqv
   logior
   lognand
   lognor
   lognot
   logorc1
   logorc2
   logtest
   logxor
   log
   long-float-epsilon
   long-float-negative-epsilon
   long-float
   long-site-name
   loop
   lower-case-p
   machine-instance
   machine-type
   machine-version
   macro-function
   macroexpand-1
   macroexpand
   macrolet
   make-array
   make-broadcast-stream
   #-ansi-90 make-char
   make-concatenated-stream
   make-dispatch-macro-character
   make-echo-stream
   make-hash-table
   make-list
   make-package
   make-pathname
   make-random-state
   make-sequence
   make-string
   make-string-input-stream
   make-string-output-stream
   make-symbol
   make-synonym-stream
   make-two-way-stream
   makunbound
   mapc
   mapcan
   mapcar
   mapcon
   maphash
   maplist
   mapl
   map
   mask-field
   max
   member-if
   member-if-not
   member
   merge
   merge-pathnames
   min
   minusp
   mismatch
   mod
   most-negative-double-float
   most-negative-fixnum
   most-negative-long-float
   most-negative-short-float
   most-negative-single-float
   most-positive-double-float
   most-positive-fixnum
   most-positive-long-float
   most-positive-short-float
   most-positive-single-float
   multiple-value-bind
   multiple-value-call
   multiple-value-list
   multiple-value-prog1
   multiple-value-setq
   multiple-values-limit
   name-char
   namestring
   nbutlast
   nconc
   nil
   nintersection
   ninth
   not
   notany
   notevery
   notinline
   nreconc
   nreverse
   nset-difference
   nset-exclusive-or
   nstring-capitalize
   nstring-downcase
   nstring-upcase
   nsublis
   nsubst-if
   nsubst-if-not
   nsubstitute
   nsubstitute-if
   nsubstitute-if-not
   nsubst
   nth
   nthcdr
   null
   numberp
   number
   numerator
   nunion
   oddp
   open
   optimize
   or
   otherwise
   output-stream-p
   package-name
   package-nicknames
   package-shadowing-symbols
   package-use-list
   package-used-by-list
   packagep
   package
   pairlis
   parse-integer
   parse-namestring
   pathname-device
   pathname-directory
   pathname-host
   pathname-name
   pathname-type
   pathname-version
   pathnamep
   pathname
   peek-char
   phase
   pi
   plusp
   pop
   position
   position-if
   position-if-not
   pprint
   prin1-to-string
   prin1
   princ-to-string
   princ
   print
   probe-file
   proclaim
   prog
   prog*
   prog1
   prog2
   progn
   progv
   #-ansi-90 provide
   psetf
   psetq
   push
   pushnew
   quote
   random
   random-state-p
   random-state
   rassoc-if
   rassoc-if-not
   rassoc
   rationalize
   rationalp
   rational
   ratio
   read-byte
   read-char
   read-char-no-hang
   read-delimited-list
   read-from-string
   read-line
   read-preserving-whitespace
   readtablep
   readtable
   read
   realpart
   reduce
   remf
   remhash
   remove-duplicates
   remove-if 
   remove-if-not
   remove
   remprop
   rem
   rename-file
   rename-package
   replace
   #-ansi-90 require
   rest
   return-from
   return
   revappend
   reverse
   room
   rotatef
   round
   rplaca
   rplacd
   safety
   satisfies
   sbit
   scale-float
   schar
   search
   second 
   sequence
   set
   #-ansi-90 set-char-bit
   set-difference
   set-dispatch-macro-character
   set-exclusive-or
   set-macro-character
   set-syntax-from-char
   setf
   setq
   seventh
   shadow
   shadowing-import
   shiftf
   short-float-epsilon
   short-float-negative-epsilon
   short-float
   short-site-name
   signed-byte
   signum
   simple-array
   simple-bit-vector-p
   simple-bit-vector
   simple-string-p
   simple-string
   simple-vector-p
   simple-vector
   sin
   single-float-epsilon
   single-float-negative-epsilon
   single-float
   sinh
   sixth
   sleep
   software-type
   software-version
   some
   sort
   space
   special-form-p
   special
   speed
   sqrt
   stable-sort
   standard-char-p
   standard-char
   step
   stream-element-type
   streamp
   stream
   string
   string-capitalize
   #-ansi-90 string-char
   #-ansi-90 string-char-p
   string-downcase
   string-equal
   string-greaterp
   string-left-trim
   string-lessp
   string-not-equal
   string-not-greaterp
   string-not-lessp
   string-right-trim
   string-trim
   string-upcase
   string/=
   string<
   string<=
   string=
   string>
   string>=
   stringp
   structure
   sublis
   subseq
   subsetp
   subst-if
   subst-if-not
   substitute
   substitute-if
   substitute-if-not
   subst
   subtypep
   svref
   sxhash
   symbol-function
   symbol-name
   symbol-package
   symbol-plist
   symbol-value
   symbolp
   symbol
   t
   tagbody
   tailp
   tan
   tanh
   tenth
   terpri
   the
   third
   throw
   time
   trace
   tree-equal
   truename
   truncate
   type-of
   typecase
   typep
   type
   unexport
   unintern
   union
   unless
   unread-char
   unsigned-byte
   untrace
   unuse-package
   unwind-protect
   upper-case-p
   use-package
   user-homedir-pathname
   values
   values-list
   variable
   vector
   vector-pop
   vector-push-extend
   vector-push
   vectorp
   warn
   when
   with-input-from-string
   with-open-file
   with-open-stream
   with-output-to-string
   write
   write-byte
   write-char
   write-line
   write-string
   write-to-string
   y-or-n-p
   yes-or-no-p
   zerop)

 #+genera
 (:import-from future-common-lisp
   base-string
   broadcast-stream
   broadcast-stream-streams
   *compile-file-pathname*
   compile-file-pathname
   *compile-print*
   compiler-macro-function
   *compile-verbose*
   concatenated-stream
   concatenated-stream-streams
   copy-pprint-dispatch
   debug
   destructuring-bind
   echo-stream
   echo-stream-input-stream
   echo-stream-output-stream
   ;encapsulated
   fdefinition
   file-stream
   file-string-length
   formatter
   *gensym-counter*
   load-logical-pathname-translations
   *load-pathname*
   *load-print*
   load-time-value
   *load-truename*
   logical-pathname
   logical-pathname-translations
   loop-finish 
   nth-value
   pathname-match-p
   pprint-dispatch
   pprint-exit-if-list-exhausted
   pprint-fill
   pprint-indent
   pprint-linear
   pprint-logical-block
   pprint-newline
   pprint-pop
   pprint-tab
   pprint-tabular
   *print-lines*
   *print-miser-width*
   *print-pprint-dispatch*
   *print-right-margin*
   readtable-case
   row-major-aref
   set-pprint-dispatch
   ;simple-array-base-character
   simple-base-string
   stream-external-format
   string-stream
   synonym-stream
   synonym-stream-symbol
   translate-logical-pathname
   translate-pathname
   two-way-stream
   two-way-stream-input-stream
   two-way-stream-output-stream
   upgraded-array-element-type
   upgraded-complex-part-type
   wild-pathname-p
   with-compilation-unit
   with-condition-restarts
   with-hash-table-iterator
   with-package-iterator)

 ;; more ansi exports.  why were these not exported?
 (:export 
   ;; why not exported here?
   open-stream-p
   ;; not supported in genera yet, but not used by clim either!
   ;with-added-methods
   structure-class
   standard-method 
   method-combination
   method
   #+pcl classp
   augment-environment
   base-string
   broadcast-stream
   broadcast-stream-streams
   *compile-file-pathname*
   compile-file-pathname
   *compile-print*
   compiler-macroexpand
   compiler-macroexpand-1
   compiler-macro-function
   *compile-verbose*
   concatenated-stream
   concatenated-stream-streams
   copy-pprint-dispatch
   debug
   destructuring-bind
   echo-stream
   echo-stream-input-stream
   echo-stream-output-stream
   ;encapsulated
   fdefinition
   file-stream
   file-string-length
   formatter
   *gensym-counter*
   load-logical-pathname-translations
   *load-pathname*
   *load-print*
   load-time-value
   *load-truename*
   logical-pathname
   logical-pathname-translations
   loop-finish 
   nth-value
   pathname-match-p
   pprint-dispatch
   pprint-exit-if-list-exhausted
   pprint-fill
   pprint-indent
   pprint-linear
   pprint-logical-block
   pprint-newline
   pprint-pop
   pprint-tab
   pprint-tabular
   *print-lines*
   *print-miser-width*
   *print-pprint-dispatch*
   *print-right-margin*
   readtable-case
   row-major-aref
   set-pprint-dispatch
   simple-array-base-character
   simple-base-string
   stream-external-format
   string-stream
   synonym-stream
   synonym-stream-symbol
   translate-logical-pathname
   translate-pathname
   two-way-stream
   two-way-stream-input-stream
   two-way-stream-output-stream
   upgraded-array-element-type
   upgraded-complex-part-type
   wild-pathname-p
   with-compilation-unit
   with-condition-restarts
   with-hash-table-iterator
   with-package-iterator))

;;; get around a ccl-2 bug with defpackage
#+ccl-2
(eval-when (eval compile load)
  (export '(()) :clim-lisp))


#+genera
(pushnew (find-package :clim-lisp) si:*reasonable-packages*)

(clim-lisp::defpackage :clim-utils
  (:use :clim-lisp)

  #+genera (:import-from :system
	     arglist)
  #+genera (:import-from :zwei
	     indentation)
  #+cloe-runtime (:import-from :cloe
		   arglist)

  #+lcl3.0 (:import-from :lcl
	     arglist)
  #+(and lucid (not :lcl3.0)) (:import-from :system
			       arglist)

  #+excl (:import-from :excl arglist)

  #+(or genera (not ansi-90))
  (:shadow
    defun
    flet labels
    defgeneric defmethod)

  ;; lucid and franz don't hack declarations properly for with-slots
  #+(or lucid (and excl (or :rs6000 (not (version>= 4 1)))))
  (:shadow
    with-slots)
  #+(or lucid (and excl (or :rs6000 (not (version>= 4 1)))))
  (:export
    with-slots)

  ;; fix char-bits and friends as best we can
  #+ccl-2
  (:shadow
    char=
    standard-char-p
    graphic-char-p
    alpha-char-p)
  #+ccl-2
  (:export
    char=
    standard-char-p
    graphic-char-p
    alpha-char-p)

  #+(and ansi-90 genera)
  (:import-from :lisp
    string-char
    char-bits)
  #+excl
  (:shadowing-import-from :cltl1
    string-char
    char-bits)
  #+(or excl (and ansi-90 genera))
  (:export
    string-char
    char-bits)

  #+excl
  (:import-from :cltl1
    *applyhook*
    *break-on-warnings*
    *evalhook*
    *modules*
    applyhook
    char-bit
    char-bits-limit
    char-control-bit
    char-font
    char-font-limit
    char-hyper-bit
    char-meta-bit
    char-super-bit
    ;common   ; ^%$!@# franz
    ;commonp
    compiler-let
    evalhook
    int-char
    make-char
    provide
    require
    set-char-bit
    string-char-p)
  
  #+excl
  (:export
    *applyhook*
    *break-on-warnings*
    *evalhook*
    *modules*
    applyhook
    char-bits
    char-bits-limit
    char-control-bit
    char-font
    char-font-limit
    char-hyper-bit
    char-meta-bit
    char-super-bit
    ;common   ; ^%$!@# franz
    ;commonp
    compiler-let
    evalhook
    int-char
    make-char
    provide
    require
    set-char-bit
    string-char-p)

  (:export
    arglist

    ;; from defun
    *keyword-package*
    extract-declarations

    ;; from utilities
    boolean
    with-collection collect
    with-gensyms
    integerize-coordinate
    with-fast-vector-references
    ignore-errors
    def-property-slot-macros
    def-property-slot-accessors
    define-unimplemented-protocol-method
    unimplemented warn-obsolete
    with-lockf initial-lock-value
    with-recursive-lockf initial-recursive-lock-value
    without-scheduling 
    *multiprocessing-p*
    make-process destroy-process
    processp
    current-process all-processes
    process-yield process-wait process-wait-with-timeout
    show-processes
    make-setf-function-name

    ;; from lisp-utilities
    without-interrupts
    funcallable-p
    standard-io-environment-vars-and-vals 
    with-standard-io-environment 
    follow-synonym-stream
    fintern
    gensymbol 
    extended-char
    remove-word-from-string
    push-unique
    letf-globally
    letf-globally-if
    time-elapsed-p
    ;; ignore
    with-stack-list
    with-stack-list*
    evacuate-list
    with-stack-array
    with-rem-keywords
    rem-keywords
    with-stack-copy-of-list
    define-keywords
    dovector
    doseq
    dorest
    define-group
    with-warnings-for-definition
    defun-property
    defun-inline
    writing-clauses
    clause
    flatten-arglist
    make-pass-on-arglist
    ignore-arglist
    canonicalize-and-match-lambda-lists	 
    define-class-mixture
    do-delimited-substrings
    with-compile-time-local-property
    get-compile-time-local-property
    compile-time-property
    define-constructor
    with-lock-held
    make-lock
    once-only
    trap-on-error
    bind-to-list
    lambda-list-variables-used-in-body
    parameter-specifier-keyword
    find-class-that-works
    minf maxf minf-or maxf-or roundf
    simple-vector-push-extend
    simple-vector-insert-element

    ;; from clos
    find-dynamic-class

    ;; from queue
    locking-queue
    make-queue
    queue 
    queue-contents-list
    map-over-queue
    queue-contents-type
    queue-empty-p
    queue-flush
    queue-full-p
    queue-get
    queue-last
    queue-length
    queue-next
    queue-pop
    queue-push
    queue-put
    queue-size
    queue-unget
    with-queue-locked

    ;; from protocols
    protocol role operation find-protocol find-role 
    protocol-name protocol-roles protocol-operations
    role-slots
    operation-name 
    operation-required-args operation-specs operation-extra-args
    defprotocol defrole defoperation
    *outer-self*
    generate-trampolines
    define-trampoline-template define-slot-trampoline-template

    ;; from autoconstructor
    make-instance-with-constructor
    defautoconstructor
    autoconstructor
    disable-autoconstructors
    enable-autoconstructors

    ;; from transformations
    +identity-transformation+
    compose-rotation-transformation
    compose-scaling-transformation
    compose-transformations
    compose-translation-transformation
    even-scaling-transformation-p
    identity-transformation-p
    invert-transformation
    invertible-transformation-p
    make-3-point-transformation
    make-3-point-transformation*
    make-reflection-transformation
    make-reflection-transformation*
    make-rotation-transformation
    make-rotation-transformation*
    make-scaling-transformation
    make-scaling-transformation*
    make-transformation
    make-translation-transformation
    rectilinear-transformation-p
    reflection-transformation-p
    rigid-transformation-p
    scaling-transformation-p
    transform-distance
    transform-point*
    transformation
    standard-transformation
    identity-transformation
    translation-transformation
    transformation-equal
    translation-transformation-p
    untransform-distance
    untransform-point*
    singular-transformation
    transformation-underspecified

    ;; from regions
    fix-rectangle
    +everywhere+
    +nowhere+
    area
    design
    ellipse
    ellipse-center-point
    ellipse-center-point*
    ellipse-end-angle
    ellipse-radii
    ellipse-start-angle
    elliptical-arc
    line
    line-end-point
    line-end-point*
    line-start-point
    line-start-point*
    make-ellipse
    make-ellipse*
    make-elliptical-arc
    make-elliptical-arc*
    make-line
    make-line*
    make-point
    make-polygon
    make-polygon*
    make-polyline
    make-polyline*
    make-rectangle
    make-rectangle*
    map-over-polygon-coordinates
    map-over-polygon-segments
    map-over-region-set-regions
    opacity
    opacity-value
    standard-opacity
    path
    point
    point-position*
    point-x
    point-y
    polygon
    polygon-points
    polyline
    polyline-closed
    rectangle
    rectangle-edges*
    rectangle-height
    rectangle-max-point
    rectangle-max-x
    rectangle-max-y
    rectangle-min-point
    rectangle-min-x
    rectangle-min-y
    rectangle-size
    rectangle-width
    region
    region-contains-point*-p
    region-contains-region-p
    region-difference
    region-equal
    region-intersection
    region-intersects-region-p
    region-set
    region-set-function
    region-set-regions
    region-union
    standard-ellipse
    standard-elliptical-arc
    standard-line
    standard-point
    standard-polygon
    standard-polyline
    standard-rectangle
    standard-rectangle-set
    transform-region
    untransform-region

    ;; bounding rectangle protocols
    left top right bottom
    make-bounding-rectangle
    bounding-rectangle
    bounding-rectangle*
    bounding-rectangle-set-edges
    with-bounding-rectangle*
    bounding-rectangle-min-x
    bounding-rectangle-min-y
    bounding-rectangle-max-x
    bounding-rectangle-max-y
    bounding-rectangle-position
    bounding-rectangle-position*
    bounding-rectangle-set-position*
    bounding-rectangle-shift-position
    bounding-rectangle-position-difference
    bounding-rectangle-position-equal
    bounding-rectangle-edges-equal
    bounding-rectangle-width
    bounding-rectangle-height
    bounding-rectangle-size
    bounding-rectangle-set-size
    bounding-rectangle-size-equal
    bounding-rectangle-center
    bounding-rectangle-center*
    bounding-rectangle-ltrb
    with-bounding-rectangle-ltrb
    bounding-rectangle-left
    bounding-rectangle-top
    bounding-rectangle-right
    bounding-rectangle-bottom
    position-difference*

    ;; ltrbs
    ltrb-well-formed-p
    ltrb-equals-ltrb-p
    ltrb-size-equal
    ltrb-contains-point*-p
    ltrb-contains-ltrb-p
    ltrb-overlaps-ltrb-p
    ltrb-union
    ltrb-intersection
    ltrb-difference

    ;; some random geometry
    pi-single-float
    2pi
    pi/2
    radians->degrees
    degrees->radians
    point-close-to-line-p
    point-inside-ellipse-p
    point-on-thick-ellipse-p
    elliptical-arc-box
    angle-between-angles-p
    2x2-singular-value-decomposition

    ;; from designs
    +foreground+
    +background+
    +flipping-ink+
    +black+
    +white+
    +red+
    +blue+
    +green+
    +cyan+
    +magenta+
    +yellow+
    color
    gray-color
    rgb-color
    color-ihs
    color-rgb
    compose-in
    compose-out
    compose-over
    composite-in
    composite-out
    composite-over
    contrasting-ink
    contrasting-ink-index
    make-color-for-contrasting-ink
    make-gray-color-for-contrasting-ink
    decode-flipping-ink
    decode-pattern
    pattern-width
    pattern-height
    decode-rectangular-tile
    decode-tile-as-stipple
    flipping-ink
    make-color-ihs
    make-color-rgb
    make-contrasting-inks
    make-contrasting-dash-patterns
    make-design-from-output-record
    make-flipping-ink
    make-gray-color
    make-opacity
    make-pattern
    make-rectangular-tile
    make-stencil
    opacity-value
    pattern
    rectangular-tile
    stencil
    stencil-array

    ;; stupid colors
    +alice-blue+
    +antique-white+
    +aquamarine+
    +azure+
    +beige+
    +bisque+
    +black+
    +blanched-almond+
    +blue-violet+
    +brown+
    +burlywood+
    +cadet-blue+
    +chartreuse+
    +chocolate+
    +coral+
    +cornflower-blue+
    +cornsilk+
    +dark-goldenrod+
    +dark-green+
    +dark-khaki+
    +dark-olive-green+
    +dark-orange+
    +dark-orchid+
    +dark-salmon+
    +dark-sea-green+
    +dark-slate-blue+
    +dark-slate-gray+
    +dark-turquoise+
    +dark-violet+
    +deep-pink+
    +deep-sky-blue+
    +dim-gray+
    +dodger-blue+
    +firebrick+
    +floral-white+
    +forest-green+
    +gainsboro+
    +ghost-white+
    +gold+
    +goldenrod+
    +gray+
    +green-yellow+
    +honeydew+
    +hot-pink+
    +indian-red+
    +ivory+
    +khaki+
    +lavender+
    +lavender-blush+
    +lawn-green+
    +lemon-chiffon+
    +light-blue+
    +light-coral+
    +light-cyan+
    +light-goldenrod+
    +light-goldenrod-yellow+
    +light-gray+
    +light-pink+
    +light-salmon+
    +light-sea-green+
    +light-sky-blue+
    +light-slate-blue+
    +light-slate-gray+
    +light-steel-blue+
    +light-yellow+
    +lime-green+
    +linen+
    +maroon+
    +medium-aquamarine+
    +medium-blue+
    +medium-orchid+
    +medium-purple+
    +medium-sea-green+
    +medium-slate-blue+
    +medium-spring-green+
    +medium-turquoise+
    +medium-violet-red+
    +midnight-blue+
    +mint-cream+
    +misty-rose+
    +moccasin+
    +navajo-white+
    +navy-blue+
    +old-lace+
    +olive-drab+
    +orange+
    +orange-red+
    +orchid+
    +pale-goldenrod+
    +pale-green+
    +pale-turquoise+
    +pale-violet-red+
    +papaya-whip+
    +peach-puff+
    +peru+
    +pink+
    +plum+
    +powder-blue+
    +purple+
    +rosy-brown+
    +royal-blue+
    +saddle-brown+
    +salmon+
    +sandy-brown+
    +sea-green+
    +seashell+
    +sienna+
    +sky-blue+
    +slate-blue+
    +slate-gray+
    +snow+
    +spring-green+
    +steel-blue+
    +tan+
    +thistle+
    +tomato+
    +turquoise+
    +violet+
    +violet-red+
    +wheat+
    +white+
    +white-smoke+
    +yellow-green+))


;;; now we define the clim package.

#+ignore-this-for-now
(clim-lisp::defpackage :clim
  (:use	:clim-lisp :clim-utils
   #+silica :silica
   #+silica :windshield)

  #+ccl-2
  (:shadowing-import-from :clim-utils
    char=
    standard-char-p
    graphic-char-p
    alpha-char-p)

  #-ccl-2
  (:shadowing-import-from :clim-utils
    defun
    flet labels
    defgeneric defmethod
    dynamic-extent
    #-excl non-dynamic-extent)

  #+(or lucid (and excl (or :rs6000 (not (version>= 4 1)))))
  (:shadowing-import-from :clim-utils
    with-slots)

  (:export
    ;; stream proposal
    input-stream-p
    output-stream-p
    open-stream-p
    streamp
    stream-element-type
    close
    pathname
    truename
    format
    ;; cl stream input functions
    read-byte
    read-char
    unread-char
    read-char-no-hang
    peek-char
    listen
    read-line
    clear-input
    ;; cl stream output functions
    write-byte
    write-char
    write-string
    terpri
    fresh-line
    force-output
    finish-output
    clear-output
    ;; stream generics of the same
    stream-read-char
    stream-unread-char
    stream-read-char-no-hang
    stream-peek-char
    stream-listen
    stream-read-line
    stream-clear-input
    stream-write-char
    stream-line-column
    stream-start-line-p
    stream-write-string
    stream-terpri
    stream-fresh-line
    stream-finish-output
    stream-force-output
    stream-clear-output
    stream-advance-to-column
    stream-read-byte
    stream-write-byte

    ;; geometry
    ;; from transformations
    +identity-transformation+
    compose-rotation-transformation
    compose-scaling-transformation
    compose-transformations
    compose-translation-transformation
    even-scaling-transformation-p
    identity-transformation-p
    invert-transformation
    invertible-transformation-p
    make-3-point-transformation
    make-3-point-transformation*
    make-reflection-transformation
    make-reflection-transformation*
    make-rotation-transformation
    make-rotation-transformation*
    make-scaling-transformation
    make-scaling-transformation*
    make-transformation
    make-translation-transformation
    rectilinear-transformation-p
    reflection-transformation-p
    rigid-transformation-p
    scaling-transformation-p
    transform-distance
    transform-point*
    transformation
    standard-transformation
    transformation-equal
    translation-transformation-p
    untransform-distance
    untransform-point*
    singular-transformation
    transformation-underspecified

    ;; from regions
    +everywhere+
    +nowhere+
    area
    design
    ellipse
    ellipse-center-point
    ellipse-center-point*
    ellipse-end-angle
    ellipse-radii
    ellipse-start-angle
    elliptical-arc
    line
    line-end-point
    line-end-point*
    line-start-point
    line-start-point*
    make-ellipse
    make-ellipse*
    make-elliptical-arc
    make-elliptical-arc*
    make-line
    make-line*
    make-point
    make-polygon
    make-polygon*
    make-polyline
    make-polyline*
    make-rectangle
    make-rectangle*
    map-over-polygon-coordinates
    map-over-polygon-segments
    map-over-region-set-regions
    opacity
    opacity-value
    path
    point
    point-position*
    point-x
    point-y
    polygon
    polygon-points
    polyline
    polyline-closed
    rectangle
    rectangle-edges*
    rectangle-height
    rectangle-max-point
    rectangle-max-x
    rectangle-max-y
    rectangle-min-point
    rectangle-min-x
    rectangle-min-y
    rectangle-size
    rectangle-width
    region
    region-contains-point*-p
    region-contains-region-p
    region-difference
    region-equal
    region-intersection
    region-intersects-region-p
    region-set
    region-set-function
    region-set-regions
    region-union
    standard-ellipse
    standard-elliptical-arc
    standard-line
    standard-point
    standard-polygon
    standard-polyline
    standard-rectangle
    transform-region
    untransform-region

    ;; from designs
    +foreground+
    +background+
    +flipping-ink+
    +black+
    +white+
    +red+
    +blue+
    +green+
    +cyan+
    +magenta+
    +yellow+
    color
    color-ihs
    color-rgb
    compose-in
    compose-out
    compose-over
    make-color-ihs
    make-color-rgb
    make-contrasting-inks
    make-contrasting-dash-patterns
    make-design-from-output-record
    make-flipping-ink
    make-gray-color
    make-opacity
    make-pattern
    make-rectangular-tile
    make-stencil

    ;; stupid colors
    +alice-blue+
    +antique-white+
    +aquamarine+
    +azure+
    +beige+
    +bisque+
    +black+
    +blanched-almond+
    +blue-violet+
    +brown+
    +burlywood+
    +cadet-blue+
    +chartreuse+
    +chocolate+
    +coral+
    +cornflower-blue+
    +cornsilk+
    +dark-goldenrod+
    +dark-green+
    +dark-khaki+
    +dark-olive-green+
    +dark-orange+
    +dark-orchid+
    +dark-salmon+
    +dark-sea-green+
    +dark-slate-blue+
    +dark-slate-gray+
    +dark-turquoise+
    +dark-violet+
    +deep-pink+
    +deep-sky-blue+
    +dim-gray+
    +dodger-blue+
    +firebrick+
    +floral-white+
    +forest-green+
    +gainsboro+
    +ghost-white+
    +gold+
    +goldenrod+
    +gray+
    +green-yellow+
    +honeydew+
    +hot-pink+
    +indian-red+
    +ivory+
    +khaki+
    +lavender+
    +lavender-blush+
    +lawn-green+
    +lemon-chiffon+
    +light-blue+
    +light-coral+
    +light-cyan+
    +light-goldenrod+
    +light-goldenrod-yellow+
    +light-gray+
    +light-pink+
    +light-salmon+
    +light-sea-green+
    +light-sky-blue+
    +light-slate-blue+
    +light-slate-gray+
    +light-steel-blue+
    +light-yellow+
    +lime-green+
    +linen+
    +maroon+
    +medium-aquamarine+
    +medium-blue+
    +medium-orchid+
    +medium-purple+
    +medium-sea-green+
    +medium-slate-blue+
    +medium-spring-green+
    +medium-turquoise+
    +medium-violet-red+
    +midnight-blue+
    +mint-cream+
    +misty-rose+
    +moccasin+
    +navajo-white+
    +navy-blue+
    +old-lace+
    +olive-drab+
    +orange+
    +orange-red+
    +orchid+
    +pale-goldenrod+
    +pale-green+
    +pale-turquoise+
    +pale-violet-red+
    +papaya-whip+
    +peach-puff+
    +peru+
    +pink+
    +plum+
    +powder-blue+
    +purple+
    +rosy-brown+
    +royal-blue+
    +saddle-brown+
    +salmon+
    +sandy-brown+
    +sea-green+
    +seashell+
    +sienna+
    +sky-blue+
    +slate-blue+
    +slate-gray+
    +snow+
    +spring-green+
    +steel-blue+
    +tan+
    +thistle+
    +tomato+
    +turquoise+
    +violet+
    +violet-red+
    +wheat+
    +white+
    +white-smoke+
    +yellow-green+

    ;; drawing options
    line-style
    line-style-cap-shape
    line-style-dashes
    line-style-initial-dash-phase
    line-style-joint-shape
    line-style-thickness
    line-style-unit
    make-line-style
    medium-background
    medium-clipping-region
    medium-foreground
    medium-ink
    medium-line-style
    medium-transformation
    with-drawing-options
    with-clipping-region
    with-translation
    with-scaling
    with-rotation
    with-local-coordinates
    with-first-quadrant-coordinates
    with-room-for-graphics

    ;; bounding rectangle protocol
    make-bounding-rectangle
    bounding-rectangle
    bounding-rectangle*
    with-bounding-rectangle*
    bounding-rectangle-min-x
    bounding-rectangle-min-y
    bounding-rectangle-max-x
    bounding-rectangle-max-y
    bounding-rectangle-position
    bounding-rectangle-position*
    bounding-rectangle-width
    bounding-rectangle-height
    bounding-rectangle-size
    bounding-rectangle-center
    bounding-rectangle-center*

    ;; extended bounding rectangle protocol
    bounding-rectangle-ltrb
    with-bounding-rectangle-ltrb
    bounding-rectangle-left
    bounding-rectangle-top
    bounding-rectangle-right
    bounding-rectangle-bottom

    ;; protocol stuff
    window-stream-p				;???
    encapsulating-stream-p
    extended-input-stream-p
    extended-output-stream-p
    graphics-stream-p
    interactive-stream-p
    output-recording-stream-p
    windowp

    ;; events
    device-event
    event-window
    keyboard-event
    keyboard-event-char
    key-press-event
    key-release-event
    pointer-event
    pointer-event-x
    pointer-event-y
    pointer-button-event
    pointer-event-button
    pointer-event-shift-mask
    pointer-button-press-event
    pointer-button-release-event
    pointer-click-hold-event
    pointer-double-click-event
    pointer-motion-event
    pointer-enter-event
    pointer-exit-event
    window-event
    window-size-or-position-change-event

    ;; window stuff.
    open-root-window
    open-window-stream
    window-name
    window-parent
    window-children
    window-label
    window-label-size
    window-inside-width
    window-inside-height
    window-inside-size
    window-inside-left
    window-inside-top
    window-inside-right
    window-inside-bottom
    window-inside-edges
    window-clear
    window-expose
    window-visibility
    window-stack-on-top
    window-stack-on-bottom
    window-refresh
    window-margins
    stream-set-input-focus			;???
    stream-restore-input-focus			;???
    window-viewport
    window-erase-viewport
    window-viewport-position*
    window-set-viewport-position*
    window-root
    window-top-level-window

    ;; window-stream-clear-history

    ;; window-stream-pointers ; not yet implemented

    stream-record-p
    stream-draw-p
    output-recording-stream-output-record
    output-recording-stream-replay
    with-output-to-output-record
    with-new-output-record
    with-output-recording-options
    surrounding-output-with-border
    define-border-type

    with-end-of-page-action with-end-of-line-action 
    medium-default-text-style medium-text-style
    stream-merged-text-style 
    with-text-style 
    with-text-family with-text-face with-text-size
    text-style-family text-style-face text-style-size
    text-style-height text-style-width
    text-style-ascent text-style-descent
    text-style-fixed-width-p
    text-size

    ;; extended input protocol
    basic-extended-input-protocol
    stream-input-wait
    stream-input-buffer
    stream-pointers
    stream-primary-pointer
    stream-pointer-position*
    stream-set-pointer-position*
    stream-read-gesture
    stream-unread-gesture
    pointer-input-rectangle
    pointer-input-rectangle*

    ;; extended output protocol
    stream-cursor-position*
    stream-set-cursor-position*
    stream-increment-cursor-position*
    stream-string-width
    stream-character-width
    stream-line-height
    stream-text-margin
    stream-default-view

    stream-end-of-line-action
    stream-end-of-page-action

    draw-point
    draw-point*
    draw-points
    draw-points*
    draw-line
    draw-line*
    draw-lines
    draw-lines*
    draw-polygon
    draw-polygon*
    draw-regular-polygon
    draw-regular-polygon*
    draw-rectangle
    draw-rectangle*
    draw-triangle
    draw-triangle*
    draw-ellipse
    draw-ellipse*
    draw-circle
    draw-circle*
    draw-string
    draw-string*
    draw-character
    draw-character*
    draw-text
    draw-text*
    draw-vertical-string
    draw-vertical-string*
    draw-icon
    draw-design
    make-design-from-output-record

    beep

    parse-text-style make-text-style
    merge-text-styles define-character-face-class
    define-character-face-added-mappings define-character-face
    merged-text-style text-style-components
    display-device define-display-device
    define-text-style-mappings add-text-style-mapping
    text-style-mapping
    diacritic-char-p
    *standard-character-set* *null-text-style*
    *undefined-text-style* *default-text-style*

    ;; output records
    fundamental-output-recording
    output-record
    output-record-p
    output-record-element
    output-record-element-p
    displayed-output-record-element
    displayed-output-record-element-p
    replay
    replay-1
    output-record-position*
    output-record-set-position*
    output-record-start-position*
    output-record-end-position*
    output-record-parent
    output-record-refined-sensitivity-test
    add-output-record
    add-output-record-element
    delete-output-record-element
    clear-output-record
    map-over-output-record-elements
    map-over-output-record-elements-overlapping-region
    map-over-output-record-elements-containing-point*
    highlight-output-record
    highlight-output-record-1
    tree-recompute-extent
    tree-recompute-extent-1
    convert-from-relative-to-absolute-coordinates
    convert-from-absolute-to-relative-coordinates
    convert-from-ancestor-to-descendant-coordinates
    convert-from-descendant-to-ancestor-coordinates
    copy-textual-output-history

    defstipple
    make-stipple
    *hearts-stipple*
    *parquet-stipple*
    *bricks-stipple*

    ;; uims
    read-gesture
    unread-gesture
    read-token
    write-token
    abort-gesture
    parse-error
    simple-parse-error
    input-not-of-required-type
    with-blip-characters
    *blip-characters*
    with-activation-characters
    *activation-characters*
    *standard-activation-characters*
    complete-input
    completing-from-suggestions
    complete-from-possibilities
    complete-from-generator
    suggest
    *complete-characters*
    *possibilities-characters*
    *help-characters*
    accept
    accept-from-string
    present
    present-to-string
    replace-input
    presentation-replace-input
    with-output-as-presentation
    with-input-editing
    with-input-editor-typeout
    with-input-focus
    with-input-context
    *input-context*
    with-accept-help

    ;; avvs (dialogs)
    accepting-values
    accept-values
    accept-values-command-button
    accept-values-pane
    accept-values-pane-displayer

    presentation
    presentationp
    presentation-object
    presentation-type
    presentation-single-box
    standard-presentation

    accept-present-default
    class-presentation-type-name
    default-describe-presentation-type
    define-presentation-type
    define-presentation-generic-function
    call-presentation-generic-function
    define-presentation-method
    define-default-presentation-method
    describe-presentation-type
    define-presentation-type-abbreviation
    expand-presentation-type-abbreviation
    expand-presentation-type-abbreviation-1
    find-presentation-type-class
    highlight-presentation
    make-presentation-type-specifier
    map-over-presentation-type-supertypes
    presentation-subtypep
    presentation-type-direct-supertypes
    presentation-type-history
    presentation-type-name
    presentation-type-options
    presentation-type-parameters
    presentation-type-specifier-p
    presentation-typep
    with-presentation-type-decoded
    with-presentation-type-options
    with-presentation-type-parameters

    textual-view
    dialog-view
    menu-view
    iconic-view
    +textual-view+
    +dialog-view+
    +menu-view+
    +iconic-view+

    blank-area
    *null-presentation*

    define-presentation-translator
    define-presentation-to-command-translator
    define-presentation-action
    define-click-and-drag-translator
    find-presentation-translators
    test-presentation-translator
    presentation-matches-context-type
    find-applicable-translators
    call-presentation-translator
    document-presentation-translator
    call-presentation-menu

    find-innermost-applicable-presentation
    throw-highlighted-presentation
    highlight-applicable-presentation
    set-highlighted-presentation
    unhighlight-highlighted-presentation

    add-pointer-gesture-name
    remove-pointer-gesture-name
    define-gesture-name

    formatting-table
    formatting-row
    formatting-column
    formatting-cell
    formatting-item-list
    format-items
    format-graph-from-root
    *default-generation-separation*
    *default-within-generation-separation*
    cell-output-record
    row-output-record
    table-output-record
    item-list-output-record
    adjust-table-cells
    filling-output
    indenting-output
    format-textual-list

    ;; presentation type names
    t
    null
    boolean
    symbol
    keyword
    number
    complex
    real
    rational
    integer
    ratio
    float
    character
    string
    pathname
    completion
    member
    member-sequence
    member-alist
    subset-completion
    subset
    subset-sequence
    subset-alist
    sequence
    sequence-enumerated
    or
    and
    not
    satisfies
    token-or-type
    null-or-type
    type-or-string
    expression
    form

    ;; incremental redisplay
    redisplay
    redisplay-1
    updating-output
    inferior-output-record-changed
    compute-differences
    output-record-fixed-position
    output-record-unique-id
    match-output-records
    find-inferior-output-record
    recompute-contents-ok
    cache-output-record
    find-cached-output-record
    new-output-records
    stream-redisplayable-p
    incremental-redisplay
    erase-output-record

    *abort-menus-when-buried*
    menu-choose
    hierarchical-menu-choose
    menu-choose-from-drawer
    draw-standard-menu
    define-static-menu
    with-menu
    menu-item
    menu-item-value
    menu-item-display
    menu-item-style
    menu-item-item-list
    print-menu-item

    tracking-pointer
    dragging-output
    dragging-output-record

    ;; the command processor and its friends
    define-command
    ;; command tables
    command-table
    command-table-name
    command-table-inherit-from
    command-table-not-found
    command-table-already-exists
    define-command-table
    find-command-table
    make-command-table
    global-command-table
    user-command-table
    ;; commands
    command-not-present
    command-already-present
    command-not-accessible
    command-accessible-in-command-table-p
    command-present-in-command-table-p
    add-command-to-command-table
    remove-command-from-command-table
    do-command-table-inheritance
    map-over-command-table-commands
    map-over-command-table-names
    find-command-from-command-line-name
    command-line-name-for-command
    ;; command menus
    add-menu-item-to-command-table
    remove-menu-item-from-command-table    
    map-over-command-table-menu-items
    find-menu-item
    command-menu-item-type
    command-menu-item-value
    command-menu-item-options
    menu-choose-command-from-command-table
    display-command-table-menu
    display-command-menu
    ;; keystroke accelerators
    add-keystroke-to-command-table
    remove-keystroke-from-command-table
    map-over-command-table-keystrokes
    find-keystroke-item
    lookup-keystroke-item
    lookup-keystroke-command-item
    ;; presentation translators
    add-presentation-translator-to-command-table
    remove-presentation-translator-from-command-table
    map-over-command-table-translators
    find-presentation-translator

    ;; the command processor itself
    command
    command-name
    command-or-form
    command-arguments
    *unsupplied-argument*
    *command-name-delimiters*
    *command-argument-delimiters*
    *command-dispatchers*
    *command-previewers*
    read-command
    with-command-table-keystrokes
    read-command-using-keystrokes
    *command-parser*
    *command-unparser*
    *partial-command-parser*
    command-line-parser
    command-line-unparser
    command-line-read-remaining-arguments-for-partial-command
    accept-values-command-parser
    menu-command-parser
    menu-read-remaining-arguments-for-partial-command
    invoke-command-parser-and-collect
    partial-command-p

    ;; application frames
    *application-frame*
    application-frame
    define-application-frame
    #+genera define-genera-application
    make-application-frame
    with-frame-state-variables
    frame-command-table
    run-frame-top-level
    default-frame-top-level
    panes-need-redisplay
    pane-needs-redisplay
    redisplay-frame-pane
    redisplay-frame-panes
    redisplay-frame-command-menu
    read-frame-command
    execute-frame-command
    enable-command
    disable-command
    command-enabled-p
    frame-name
    frame-pretty-name
    frame-panes
    frame-current-panes
    frame-top-level-window
    frame-current-layout
    frame-standard-input
    frame-standard-output
    frame-query-io
    frame-maintain-presentation-histories
    frame-input-context-button-press-handler
    frame-find-innermost-applicable-presentation
    frame-replay
    frame-exit
    get-frame-pane
    set-frame-layout
    size-frame-pane
    layout-frame-panes
    notify-user
    *pointer-documentation-output*

    ;; postscript support
    with-output-to-postscript-stream

    #||
    ;;--- these are the new silica-ish guys
    incremental-redisplay-display-function
    pane-display-function
    pane-display-time
    clim-top-level

    ;; frame manager stuff
    frame-manager *frame-manager* frame-manager-p 
    find-frame-manager with-frame-manager
    frame framep frame-state frame-pane pane-frame
    adopt-frame disown-frame
    enable-frame disable-frame shrink-frame
    frame-properties-mixin frame-properties

    clim-stream-pane
    ||#
    ))

;; any symbol exported by the clim package should have a home package of clim
;; in preference to clim-lisp or clim-utils, so it will print nicely, and to
;; aid in preparing documentation.
;; astoundingly, common lisp doesn't allow setf of symbol-package, so this has
;; to be conditionalized to the systems that support the extension.
#+(or genera lucid)
(funcall #'(lambda ()
	     (let ((clim (find-package :clim))
		   (clim-lisp (find-package :clim-lisp))
		   (clim-utils (find-package :clim-utils)))
	       (do-external-symbols (sym clim)
		 (let ((pkg (symbol-package sym)))
		   (when (or (eq pkg clim-lisp) (eq pkg clim-utils))
		     (setf (symbol-package sym) clim)))))))
