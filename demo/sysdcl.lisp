;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: sysdcl.lisp,v 1.9 92/05/07 13:13:39 cer Exp $

(in-package #-ANSI-90 :user #+ANSI-90 :common-lisp-user)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

(clim-defsys:defsystem clim-demo
  (:default-pathname #+Genera "SYS:CLIM;REL-2;DEMO;"
		     #+Minima "SYS:CLIM;REL-2;DEMO;"
		     #+Cloe-Runtime "\\clim\\rel-2\\demo\\"
		     #+Lucid "/home/hornig/clim/rel-2/demo/"
		     #+Allegro (frob-pathname "demo")
		     #+CMU "/home/hornig/clim/rel-2/demo/"
		     #+CCL-2 "ccl;clim-2.0:demo:"
   :default-binary-pathname #+Genera "SYS:CLIM;REL-2;DEMO;"
			    #+Minima "SYS:CLIM;REL-2;DEMO;"
			    #+Cloe-Runtime "\\clim\\rel-2\\bin\\"
			    #+Lucid "/home/hornig/clim/rel-2/lcl4/"
			    #+Allegro (frob-pathname "demo")
			    #+CMU "/home/hornig/clim/rel-2/cmu/"
			    #+CCL-2 "ccl;clim-2.0:fasls:"
   ;;--- :needed-systems (clim-standalone)
   )

  ("packages")
  ("aaai-demo-driver" :load-before-compile ("packages"))
  ("listener"       :load-before-compile ("aaai-demo-driver" "packages"))
  ("graphics-demos" :load-before-compile ("aaai-demo-driver" "packages")
		    :features (not Minima))
  ("cad-demo"	    :load-before-compile ("aaai-demo-driver" "packages")
		    :features (not Minima))
  ("navdata"	    :load-before-compile ("packages")
		    :features (not Minima))
  ("navfun"         :load-before-compile ("aaai-demo-driver" "navdata" "packages")
		    :features (not Minima))
  ("puzzle"         :load-before-compile ("aaai-demo-driver" "packages"))
  ("address-book"   :load-before-compile ("aaai-demo-driver" "packages"))
  ("thinkadot"      :load-before-compile ("aaai-demo-driver" "packages"))
  ("demo-prefill" :features (or Genera Cloe-Runtime)))

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
