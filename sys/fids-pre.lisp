;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; $Id: fids-pre.lisp,v 1.2.36.1 2002/02/08 19:11:24 layer Exp $


(defpackage "SYSTEM"
   (:export "PUSH-PATCH")
   )

(defmacro sys:push-patch (&rest x)
 (declare (ignore x))
 nil)

(defpackage "ALLEGRO"
   (:nicknames "FRANZ" "EXCL" "CLOS")
   (:export "MEMQ" "*FASL-DEFAULT-TYPE*" "INTERN-EQL-SPECIALIZER" "NAMED-FUNCTION"
    "PURE-STRCAT")
   )
 
#+acl86win32
(defvar excl:*FASL-DEFAULT-TYPE* "fasl")

#-acl86win32
(defvar excl:*FASL-DEFAULT-TYPE* "fsl")

(defvar SYS::*SOURCE-FILE-TYPES* '("lsp"))

(defun franz:memq (x l)
   (member x l :test #'eq))

(defmacro excl::.error (&rest x)
   `(error ,@x))

(defpackage "DEFSYS"
   (:import-from "ACL" "DYNAMIC-EXTENT"))

(declaim (declaration excl::IGNORE-IF-UNUSED KEYS))

(defun clos:INTERN-EQL-SPECIALIZER (x) (error))

(defmacro excl:NAMED-FUNCTION (&whole form) `(error ',form))

(defun franz:PURE-STRCAT (&rest s)
   (apply #'concateneate 'string s))
