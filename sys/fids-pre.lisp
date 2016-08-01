;; See the file LICENSE for the full license governing this code.
;;


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
