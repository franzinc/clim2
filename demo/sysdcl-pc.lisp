;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package #-ansi-90 :user #+ansi-90 :common-lisp-user)

;;;"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

#+aclpc
(clim-defsys:defsystem clim-demo
  (:default-pathname #+Genera "SYS:CLIM;REL-2;DEMO;"
		     #-Genera (frob-pathname "demo")
   :default-binary-pathname #+Genera "SYS:CLIM;REL-2;DEMO;"
			    #-Genera (frob-pathname "demo"))

  ("packages")
  ("demo-driver"     :load-before-compile ("packages"))
  ("listener"        :load-before-compile ("demo-driver" "packages"))
  ("winlisp"         :load-before-compile ("demo-driver" "packages"))
  ("graphics-demos"         :load-before-compile ("demo-driver" "packages"))
  ("cad-demo"	     :load-before-compile ("demo-driver" "packages"))
  ("navdata"	     :load-before-compile ("packages"))
  ("navfun"          :load-before-compile ("demo-driver" "packages" "navdata"))
  ("puzzle"          :load-before-compile ("demo-driver" "packages"))
  ("address-book"    :load-before-compile ("demo-driver" "packages"))
  ("thinkadot"       :load-before-compile ("demo-driver" "packages"))
;  ("plot-pc"	     :load-before-compile ("demo-driver" "packages"))
  ("plot"	     :load-before-compile ("demo-driver" "packages"))
  ("color-editor"    :load-before-compile ("demo-driver" "packages"))
;  ("palette"	     :load-before-compile ("demo-driver" "packages"))
  ("graphics-editor"	     :load-before-compile ("demo-driver" "packages"))
;  ("bitmap-editor"   :load-before-compile ("demo-driver" "packages"))
#-aclpc  ("ico"	     :load-before-compile ("demo-driver" "packages"))
  ("browser"	     :load-before-compile ("demo-driver" "packages"))
  ("peek-frame"      :load-before-compile ("demo-driver" "packages"))
  ("process-browser" :features allegro :load-before-compile ("demo-driver" "packages"))
  ("custom-records"  :load-before-compile ("demo-driver" "packages"))
#+allegro ("demo-activity"   :load-before-compile ("demo-driver" "packages"))
  ("demo-last"       :features allegro))

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
