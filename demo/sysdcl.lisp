;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-1998 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; $Id: sysdcl.lisp,v 1.31.22.1 2000/06/08 19:16:03 cley Exp $

(in-package #-ansi-90 :user #+ansi-90 :common-lisp-user)

;;;"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

;; see sys/sysdcl.lisp for the package-module defn (cim 2/28/96)

(defsystem clim-demo
    (:default-pathname "clim2:;demo;")
  (:serial
;;;   #+acl86win32 aclnt-clim
   postscript-clim-stubs
   ("packages" (:module-class compile-always))
   ("demo-driver"     (:load-before-compile "packages"))
   ("listener"        (:load-before-compile "demo-driver" "packages"))
   #+acl86win32
   ("winlisp"         (:load-before-compile "demo-driver" "packages"))
   ("graphics-demos"  (:load-before-compile "demo-driver" "packages"))
   ("cad-demo"	     (:load-before-compile "demo-driver" "packages"))
   ("navdata"	     (:load-before-compile "packages"))
   ("navfun"          (:load-before-compile "demo-driver" "packages" "navdata"))
   ("puzzle"          (:load-before-compile "demo-driver" "packages"))
   ("address-book"    (:load-before-compile "demo-driver" "packages"))
   ("thinkadot"       (:load-before-compile "demo-driver" "packages"))
   ("plot"	     (:load-before-compile "demo-driver" "packages"))
   ("color-editor"    (:load-before-compile "demo-driver" "packages"))
   ("graphics-editor" (:load-before-compile "demo-driver" "packages"))

   ;; only compile with non-ICS if no fasl file exist
   ;; always compile with ICS in case it was previously compiled by
   ;; non-ICS
   #-acl86win32
   ("japanese-graphics-editor" (:module-class #-ics compile-once
					      #+ics compile-always)
			       (:load-before-compile "demo-driver" "packages"))

   ;;#-acl86win32
   ("bitmap-editor"   (:load-before-compile "demo-driver" "packages"))
   ("ico"	     (:load-before-compile "demo-driver" "packages"))
   ("browser"	     (:load-before-compile "demo-driver" "packages"))
   ("peek-frame"      (:load-before-compile "demo-driver" "packages"))
   ("process-browser" (:load-before-compile "demo-driver" "packages"))
   ("custom-records"  (:load-before-compile "demo-driver" "packages"))
   ("demo-activity"   (:load-before-compile "demo-driver" "packages"))
   ("demo-last")
   #+(or Genera Cloe-Runtime) ("demo-prefill")
   ))

#+Genera
(clim-defsys:import-into-sct 'clim-demo
  :pretty-name "CLIM Demo"
  :default-pathname "SYS:CLIM;REL-2;DEMO;"
  :required-systems '(clim))

#+Minima-Developer
(clim-defsys:import-into-sct 'clim-demo :subsystem t
  :sct-name :minima-clim-demo-standalone
  :pretty-name "Minima CLIM Demo Standalone"
  :default-pathname "SYS:CLIM;REL-2;DEMO;")

#+Minima-Developer
(zl:::sct:defsystem minima-clim-demo
    (:pretty-name "Minima CLIM Demo"
     :default-pathname "SYS:CLIM;REL-2;DEMO;"
     :maintain-journals nil
     :default-module-type :system
     :patches-reviewed "Bug-CLIM"
     :source-category :optional)
  (:serial "minima-clim-demo-standalone"))
