;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: sysdcl.lisp,v 1.6 91/03/26 13:00:01 cer Exp $

(in-package #-ANSI-90 "USER" #+ANSI-90 :cl-user)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;; Tell the world that we're here
(pushnew :clim *features*)
(pushnew :clim-2 *features*)
(pushnew :silica *features*)

#+Genera
(when (eql (sct:get-release-version) 8)
  (pushnew :Genera-Release-8 *features*)
  (multiple-value-bind (major minor) (sct:get-system-version)
    (declare (ignore minor))
    (cond ((= major 425)
	   (pushnew :Genera-Release-8-0 *features*))
	  ((= major 436)
	   (pushnew :Genera-Release-8-1 *features*))
	  ((>= major 437)
	   (pushnew :Genera-Release-8-2 *features*)))))

#+Genera
(when (null (find-package "DEFSYS"))
  (load "SYS:CLIM;REL-2;SYS;DEFSYSTEM"))


;;; CLIM is implemented using the "Gray Stream Proposal" (STREAM-DEFINITION-BY-USER)
;;; a proposal to X3J13 in March, 1989 by David Gray of Texas Instruments.  In that
;;; proposal, stream objects are built on certain CLOS classes, and stream functions
;;; (e.g., WRITE-CHAR) are non-generic interfaces to generic functions (e.g.,
;;; STREAM-WRITE-CHAR).  These "trampoline" functions are required because their
;;; STREAM argument is often optional, which means it cannot be used to dispatch to
;;; different methods.

;;; Various Lisp vendors have their own stream implementations, some of which are
;;; identical to the Gray proposal, some of which implement just the trampoline
;;; functions and not the classes, etc.  If the Lisp vendor has not implemented the
;;; classes, we will shadow those class names (and the predicate functions for them)
;;; in the CLIM-LISP package, and define the classes ourselves.  If the vendor has
;;; not implemented the trampoline functions, we will shadow their names, and write
;;; our own trampolines which will call our generic function, and then write default
;;; methods which will invoke the COMMON-LISP package equivalents.

(eval-when (compile load eval)

#+(or Allegro 
      Minima)
(pushnew :clim-uses-lisp-stream-classes *features*)

#+(or Allegro
      Genera				;Except for STREAM-ELEMENT-TYPE
      Minima
      Cloe-Runtime
      CCL-2)				;Except for CLOSE (and WITH-OPEN-STREAM)
(pushnew :clim-uses-lisp-stream-functions *features*)

;;; CLIM-ANSI-Conditions means this lisp truly supports the ANSI CL condition system
;;; CLIM-Conditions      means that it has a macro called DEFINE-CONDITION but that it works
;;;                      like Allegro 3.1.13 or Lucid.
(pushnew #+Lucid :CLIM-Conditions
	 #+(or ANSI-90 Symbolics CMU) :CLIM-ANSI-Conditions
	 #-(or ANSI-90 Symbolics Lucid CMU)
	 (error "Figure out what condition system for this Lisp")
	 *features*)

#+Allegro
(pushnew :allegro-v4.0-constructors *features*)

;; The presentation type system massages presentation type parameters
;; and options during method inheritance.
#+(or Genera Cloe-Runtime Minima)
(pushnew :CLIM-extends-CLOS *features*)

)	;eval-when


(setq defsys:*load-all-before-compile* t)

#+Allegro
(defun frob-pathname (subdir &optional (dir excl::*source-pathname*))
    (namestring
     (make-pathname
       :directory (append (butlast (pathname-directory dir)) (list subdir)))))


(defsys:defsystem clim-utils
  (:default-pathname #+Genera "SYS:CLIM;REL-2;UTILS;"
		     #+Minima "SYS:CLIM;REL-2;UTILS;"
		     #+Cloe-Runtime "\\clim\\rel-2\\utils\\"
		     #+Lucid "/home/hornig/clim/rel-2/utils/"
		     #+Allegro (frob-pathname "utils")
		     #+CMU "/home/hornig/clim/rel-2/utils/"
		     #+CCL-2 "ccl;clim-2.0:utils:"
   :default-binary-pathname #+Genera "SYS:CLIM;REL-2;UTILS;"
			    #+Minima "SYS:CLIM;REL-2;UTILS;"
			    #+Cloe-Runtime "\\clim\\rel-2\\bin\\"
			    #+Lucid "/home/hornig/clim/rel-2/lcl4/"
			    #+Allegro (frob-pathname "utils")
			    #+CMU "/home/hornig/clim/rel-2/cmu/"
			    #+CCL-2 "ccl;clim-2.0:fasls:")

  ("excl-verification" :features Allegro)
  ("lisp-package-fixups")
  ("defpackage" :features (or Allegro (not ANSI-90)))
  ("packages")
  ("coral-char-bits" :features CCL-2)
  ("defun-utilities") ;; extract-declarations and friends
  ("defun" :features (or Genera (not ANSI-90)))
  ("reader")
  ("clos-patches")
  ("clos")
  ("condpat" :features CLIM-conditions)  ;get the define-condition macro
  ("utilities")
  ("lisp-utilities")
  ("processes")
  ("queue")
  ("protocols")
  ("autoconstructor")
  ("clim-streams")
  ("cl-stream-classes" :features (not clim-uses-lisp-stream-classes))
  ("minima-stream-classes" :features Minima)
  ("cl-stream-functions" :features (not clim-uses-lisp-stream-functions))
  ("genera-streams" :features Genera)
  ("excl-streams" :features Allegro)
  ("ccl-streams" :features CCL-2)
  ("clim-macros")
  ("transformations" :load-before-compile ("condpat"))
  ("regions")
  ("region-arithmetic")
  ("designs"))

(defsys::defsystem clim-silica
    (:default-pathname #+Genera "SYS:CLIM;REL-2;SILICA;"
      		       #+Minima "SYS:CLIM;REL-2;SILICA;"
		       #+Cloe-Runtime "\\clim\\rel-2\\silica\\"
		       #+Lucid "/home/hornig/clim/rel-2/silica/"
		       #+Allegro (frob-pathname "silica")
		       #+CMU "/home/hornig/clim/rel-2/silica/"
		       #+CCL-2 "ccl;clim-2.0:silica:"
      :default-binary-pathname #+Genera "SYS:CLIM;REL-2;CLIM;"
      			       #+Minima "SYS:CLIM;REL-2;SILICA;"
			       #+Cloe-Runtime "\\clim\\rel-2\\bin\\"
			       #+Lucid "/home/hornig/clim/rel-2/lcl4/"
			       #+Allegro (frob-pathname "silica")
			       #+CMU "/home/hornig/clim/rel-2/cmu/"
			       #+CCL-2 "ccl;clim-2.0:fasls:"
      :needed-systems (clim-utils)
      :load-before-compile (clim-utils))
  ("classes")
  ("text-style")
  ("macros")
  ("sheet")
  ("mirror")
  ("event")
  ("port")
  ("medium")
  ("graphics")
  ("std-sheet")

  ;; "Windshield"
  ("layout")
  ("db-layout")
  ("db-box")
  ("db-table")
  ("gadgets")
  ("db-scroll"))

(defsys:defsystem clim-standalone
  (:default-pathname #+Genera "SYS:CLIM;REL-2;CLIM;"
		     #+Minima "SYS:CLIM;REL-2;CLIM;"
		     #+Cloe-Runtime "\\clim\\rel-2\\clim\\rel-2\\"
		     #+Lucid "/home/hornig/clim/rel-2/clim/"
		     #+Allegro (frob-pathname "clim")
		     #+CMU "/home/hornig/clim/rel-2/clim/"
		     #+CCL-2 "ccl;clim-2.0:clim:"
   :default-binary-pathname #+Genera "SYS:CLIM;REL-2;CLIM;"
			    #+Minima "SYS:CLIM;REL-2;CLIM;"
			    #+Cloe-Runtime "\\clim\\rel-2\\bin\\"
			    #+Lucid "/home/hornig/clim/rel-2/lcl4/"
			    #+Allegro (frob-pathname "clim")
			    #+CMU "/home/hornig/clim/rel-2/cmu/"
			    #+CCL-2 "ccl;clim-2.0:fasls:"
   :needed-systems (clim-utils clim-silica)
   :load-before-compile (clim-utils clim-silica))

  ("shift-mask")
  ("defprotocol")
  ("stream-defprotocols")
  ("defresource")
  ("temp-strings")
  ("coral-defs" :features CCL-2)
  ("clim-defs")
  ("stipples")
  ("stream-class-defs")
  (#-Cloe-Runtime "interactive-defs" #+Cloe-Runtime "int-defs")
  ("cursor")
  ("view-defs")
  ("input-defs")
  ("input-protocol")
  ("output-protocol")
  ("drawing-state-mixin" :features (not Silica))
  ("window-protocol")

  ("output-recording-protocol"
   :load-before-compile ("clim-defs"))
  ("output-recording-defs"
   :load-before-compile ("output-recording-protocol") :features Silica)
  (#-Cloe-Runtime "interactive-protocol" #+Cloe-Runtime "int-prot"
   :load-before-compile ("clim-defs"))
  ("input-editor-commands")			; :load-before-compile ("lisp-utilities")
  (#-Cloe-Runtime "protocol-intermediaries" #+Cloe-Runtime "prot-int"
   :features (not Silica))

  ("formatted-output-defs")
  ("incremental-redisplay"
   :load-before-compile ("clim-defs" "output-recording-protocol"))

  ("defs-graphics-generics" :features (not Silica))
  (#-Cloe-Runtime "graphics-generics" #+Cloe-Runtime "gph-gene" :features (not Silica))

  ("coordinate-sorted-set")
  ("window-stream")
  ("completer")
  ("ptypes1"
   :load-before-compile ("clim-defs"))
  ("presentations"
   :load-before-compile ("clim-defs" "ptypes1"))
  ("translators"
   :load-before-compile ("presentations"))
  ("histories"
   :load-before-compile ("presentations"))
  ("ptypes2"
   :load-before-compile ("translators"))
  (#-Cloe-Runtime "standard-types" #+Cloe-Runtime "std-typs"
   :load-before-compile ("ptypes2"))

  ;; because table-formatting defines methods on PRESENTATION
  ("table-formatting"
   :load-before-compile ("clim-defs" "incremental-redisplay"))
  ("graph-formatting"
   :load-before-compile ("clim-defs" "incremental-redisplay"))
  ("surround-output" 
   :load-before-compile ("clim-defs" "incremental-redisplay"))
  ("text-formatting"
   :load-before-compile ("clim-defs" "incremental-redisplay"))

  (#-Cloe-Runtime "graphics-internals" #+Cloe-Runtime "gph-inte" :features (not Silica))

  ("tracking-pointer")
  ("dragging-output"
   :load-before-compile ("tracking-pointer"))

  ("vertical-string" :features (not Silica))

  ("clx-implementation" :features (and (not Silica) Xlib))

  ("genera-implementation" :features (and (not Silica) Genera))

  ("coral-scroll" :features (and (not Silica) CCL-2))
  ("coral-implementation" :features (and (not Silica) CCL-2))
  ("coral-fonts" :features  (and (not Silica) CCL-2))
  ("coral-font-stuff" :features  (and (not Silica) CCL-2))
  ("coral-events" :features (and (not Silica) CCL-2))
  ("coral-streams" :features (and (not Silica) CCL-2))

  ("wheader" :features (and (not Silica) Cloe-Runtime))
  ("windows" :features (and (not Silica) Cloe-Runtime)
   :load-before-compile ("wheader"))
  ("cloe-implementation" :features (and (not Silica) Cloe-Runtime)
   :load-before-compile ("windows"))
  ("cloe-events" :features (and (not Silica) Cloe-Runtime)
   :load-before-compile ("windows" "cloe-implementation"))
  ("cloe-applications" :features (and (not Silica) Cloe-Runtime))

  ("postscript-implementation" :features (not Silica))
  ("laserwriter-metrics" :features (not Silica)
   :load-before-compile ("postscript-implementation"))

  ;; have to duplicate this entry for each "port" that uses X
  ;; The port mechansim can't deal with the fact that Genera might or might
  ;; not have CLX loaded
  ("db-stream" :features Silica)
  ("gadget-output" :features Silica)
  ("mac-menus" :features (and (not Silica) CCL-2) :load-before-compile ("menus"))
  ("accept"
   :load-before-compile ("clim-defs" "ptypes2"))
  ("present"
   :load-before-compile ("clim-defs" "ptypes2"))
  ("command"
   :load-before-compile ("clim-defs"))
  ("command-processor"
   :load-before-compile ("clim-defs" "command"))
  ("basic-translators"
   :load-before-compile ("ptypes2" "command"))
  ("define-application"
   :load-before-compile ("clim-defs") :features (not Silica))
  ("default-application"
      :load-before-compile ("define-application") :features (not Silica))
  ("frames" :features Silica)
  ("genera-activities" :features Genera
   :load-before-compile ("default-application"))
  ("menus"
   :load-before-compile ("defresource" "clim-defs"))
  ("accept-values"
   :load-before-compile ("clim-defs" "incremental-redisplay"))
  ("item-list-manager")
  ("pixmap-streams"
   :load-before-compile ("clim-defs"))
  ("stream-trampolines"
   :load-before-compile ("defprotocol" "stream-defprotocols"))
  ("lucid-patches" :features Lucid)
  ("prefill" :features (or Genera Cloe-Runtime)))


#+Allegro
(defsys::defsystem xlib-clim
    (:default-pathname (frob-pathname "xlib")
     :default-binary-pathname (frob-pathname "xlib")
     :needed-systems (clim-standalone)
     :load-before-compile (clim-standalone))
  ("pkg")
  ("ffi")
  ("xlib")
  ("x11-keysyms")
  ("last"))

#+Allegro
(defsys::defsystem tk-clim
    (:default-pathname (frob-pathname "tk")
     :default-binary-pathname (frob-pathname "tk")
     :needed-systems (xlib-clim)
     :load-before-compile (xlib-clim))
  ;; General stuff
  ("pkg")
  ("foreign-obj")
  ("macros")
  
  ;; Xlib stuff
  ("xlib")
  ("font")
  ("gcontext")
  ("graphics")
  
  ;; Toolkit stuff
  ("xtk")
  ("meta-tk")
  ("make-classes")
  ("foreign")
  ("widget")
  ("resources")
  ("event")
  ("callbacks")

  ;; Motif specific stuff
  ("load-xm")
  ("xm-init")
  ("xm-widgets")
  ("xm-font-list")
  ("xm-protocols")
  ("convenience")
  #+ignore ("examples")

  ;; More General stuff
  ("make-widget"))

#+Allegro
(defsys::defsystem ol-clim
    (:default-pathname (frob-pathname "tk")
	:default-binary-pathname (frob-pathname "tk")
	:needed-systems (xlib-clim)
	:load-before-compile (xlib-clim))
  (|pkg|)
  (|foreign-obj|)
  (|macros|)
  
  ;; Xlib stuff
  
  (|xlib|)
  (|font|)
  (|gcontext|)
  (|graphics|)
  
  
  ;; Toolkit stuff
  
  (|xtk|)
  (|meta-tk|)
  (|make-classes|)
  
  (|foreign|)
  (|widget|)
  (|resources|)
  (|event|)
  (|callbacks|)
  (|load-ol|)
  (|ol-init|)
  (|ol-callbacks|)
  #+ignore(|ol-examples|)
  (|make-widget|)
  )

#+Allegro
(defsys::defsystem motif-clim
    (:default-pathname (frob-pathname "xm-silica")
	:default-binary-pathname (frob-pathname "xm-silica")
	:needed-systems (tk-clim)
	:load-before-compile (tk-clim))
  ("pkg")
  ("xt-silica")
  ("xm-silica")
  ("xt-graphics")
  ("xm-graphics")
  ("image")
  ("xt-frames")
  ("xm-frames")
  ("xt-gadgets")
  ("xm-gadgets")
  ("xm-menus")
  ("xt-pixmaps"))

#+Allegro
(defsys::defsystem openlook-clim
    (:default-pathname (frob-pathname "xm-silica")
	:default-binary-pathname (frob-pathname "xm-silica")
	:needed-systems (ol-clim)
	:load-before-compile (ol-clim))
  ("pkg")
  ("xt-silica")
  ("ol-silica")
  ("xt-graphics")
  ("ol-graphics")
  ("image")
  ("xt-frames")
  ("ol-frames")
  ("xt-gadgets")
  ("ol-gadgets")
  ("xt-pixmaps"))




#+Genera (progn

(defsys:import-into-sct 'clim-utils :subsystem t
			:pretty-name "CLIM Utilities"
			:default-pathname "SYS:CLIM;REL-2;UTILS;"
			:default-destination-pathname
			"SYS:CLIM;REL-2;UTILS;")

(defsys:import-into-sct 'clim-silica :subsystem t
			:pretty-name "CLIM Silica"
			:default-pathname "SYS:CLIM;REL-2;SILICA;"
			:default-destination-pathname "SYS:CLIM;REL-2;SILICA;")

(defsys:import-into-sct 'clim-standalone :subsystem t
			:pretty-name "CLIM Standalone"
			:default-pathname "SYS:CLIM;REL-2;CLIM;"
			:default-destination-pathname "SYS:CLIM;REL-2;CLIM;")

(sct:defsystem clim
    (:pretty-name "CLIM"
     :default-pathname "SYS:CLIM;REL-2;CLIM;"
     :journal-directory "SYS:CLIM;REL-2;PATCH;"
     :default-module-type :system
     :patches-reviewed "Bug-CLIM-Doc"
     :source-category :optional)
  (:module defsystem "sys:clim;rel-2;sys;defsystem"
	   (:type :lisp) (:root-module nil))
  (:serial "clim-utils"
	   "clim-silica"
	   "clim-standalone"))

)

#+Minima-Developer (progn

(defsys:import-into-sct 'clim-utils :subsystem t
			:sct-name :minima-clim-utils :pretty-name "Minima CLIM Utilities"
			:default-pathname "SYS:CLIM;REL-2;UTILS;"
			:default-destination-pathname
			"SYS:CLIM;REL-2;UTILS;")

(defsys:import-into-sct 'clim-silica :subsystem t
			:sct-name :minima-clim-silica :pretty-name "Minima CLIM Silica"
			:default-pathname "SYS:CLIM;REL-2;SILICA;"
			:default-destination-pathname "SYS:CLIM;REL-2;SILICA;")

(defsys:import-into-sct 'clim-standalone :subsystem t
			:sct-name :minima-clim-standalone :pretty-name "Minima CLIM Standalone"
			:default-pathname "SYS:CLIM;REL-2;CLIM;"
			:default-destination-pathname "SYS:CLIM;REL-2;CLIM;")

(zl:::sct:defsystem minima-clim
    (:pretty-name "Minima CLIM"
     :default-pathname "SYS:CLIM;REL-2;CLIM;"
     :journal-directory "SYS:CLIM;REL-2;PATCH;"
     :maintain-journals nil
     :default-module-type :system
     :patches-reviewed "Bug-CLIM-Doc"
     :source-category :optional)
  (:module defsystem "sys:clim;rel-2;sys;defsystem"
	   (:type :minima-lisp) (:root-module nil))
  (:serial "minima-clim-utils"
	   "minima-clim-silica"
	   "minima-clim-standalone"))

)


#||
;; You get the general idea...
(defun clone-CLIM ()
  (sct:copy-system 'clim
    :copy-sources t :copy-binaries nil
    :destination '((#p"S:>sys>clim>sys>*.*.*" #p"S:>rel-8-0>sys>clim>sys>*.*.*")
		   (#p"S:>sys>clim>utils>*.*.*" #p"S:>rel-8-0>sys>clim>utils>*.*.*")
		   (#p"S:>sys>clim>clim>*.*.*" #p"S:>rel-8-0>sys>clim>clim>*.*.*")))
  (sct:copy-system 'clim-demo
    :copy-sources t :copy-binaries nil
    :destination '((#p"S:>sys>clim>demo>*.*.*" #p"S:>rel-8-0>sys>clim>demo>*.*.*"))))
|#
