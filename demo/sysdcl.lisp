;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: sysdcl.lisp,v 1.12 92/07/06 18:52:14 cer Exp $

(in-package #-ansi-90 :user #+ansi-90 :common-lisp-user)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

(clim-defsys:defsystem clim-demo
  (:default-pathname #+Genera "SYS:CLIM;REL-2;DEMO;"
		     #+Minima "SYS:CLIM;REL-2;DEMO;"
		     #+Cloe-Runtime "\\clim\\rel-2\\demo\\"
		     #+allegro (frob-pathname "demo")
		     #+Lucid (frob-pathname "demo")
		     #+CMU "/home/hornig/clim/rel-2/demo/"
		     #+CCL-2 "ccl;clim-2.0:demo:"
   :default-binary-pathname #+Genera "SYS:CLIM;REL-2;DEMO;"
			    #+Minima "SYS:CLIM;REL-2;DEMO;"
			    #+Cloe-Runtime "\\clim\\rel-2\\bin\\"
			    #+allegro (frob-pathname "demo")
			    #+Lucid (frob-pathname "demo")
			    #+CMU "/home/hornig/clim/rel-2/cmu/"
			    #+CCL-2 "ccl;clim-2.0:fasls:"
   ;;--- :needed-systems (clim-standalone)
   )

  ("packages")
  ("demo-driver"     :load-before-compile ("packages"))
  ("listener"        :load-before-compile ("demo-driver" "packages"))
  ("graphics-demos"  :load-before-compile ("demo-driver" "packages")
		     :features (not Minima))
  ("cad-demo"	     :load-before-compile ("demo-driver" "packages")
		     :features (not Minima))
  ("navdata"	     :load-before-compile ("packages")
		     :features (not Minima))
  ("navfun"          :load-before-compile ("demo-driver" "navdata" "packages")
		     :features (not Minima))
  ("puzzle"          :load-before-compile ("demo-driver" "packages"))
  ("address-book"    :load-before-compile ("demo-driver" "packages"))
  ("thinkadot"       :load-before-compile ("demo-driver" "packages"))
  ("plot"	     :load-before-compile ("demo-driver" "packages"))
  ("graphics-editor" :load-before-compile ("demo-driver" "packages"))
  ("ico"	     :load-before-compile ("demo-driver" "packages"))
  ("browser"	     :load-before-compile ("demo-driver" "packages"))
  ("demo-prefill"    :features (or Genera Cloe-Runtime)))

#+Genera
(clim-defsys:import-into-sct 'clim-demo 
			     :pretty-name "CLIM Demo"
			     :default-pathname "SYS:CLIM;REL-2;DEMO;"
			     :default-destination-pathname "SYS:CLIM;REL-2;DEMO;")

#+Minima
(clim-defsys:import-into-sct 'clim-demo :subsystem t
			     :sct-name :minima-clim-demo-standalone
			     :pretty-name "Minima CLIM Demo Standalone"
			     :default-pathname "SYS:CLIM;REL-2;DEMO;"
			     :default-destination-pathname "SYS:CLIM;REL-2;DEMO;")

#+Minima
(zl:::sct:defsystem minima-clim-demo
    (:pretty-name "Minima CLIM Demo"
     :default-pathname "SYS:CLIM;REL-2;DEMO;"
     :maintain-journals nil
     :default-module-type :system
     :patches-reviewed "Bug-CLIM"
     :source-category :optional)
  (:parallel "minima-clim-demo-standalone"))
