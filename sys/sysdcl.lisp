;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: sysdcl.lisp,v 1.8 92/03/10 10:13:03 cer Exp Locker: cer $

(in-package #-ANSI-90 "USER" #+ANSI-90 :cl-user)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

(eval-when (compile load eval)

;;; Tell the world that we're here
  ;;------------- This needs to be in the CLIM.fasl also.
  
(pushnew :clim *features*)
(pushnew :clim-2 *features*)
(pushnew :clim-2.0 *features*)
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
(pushnew :use-CLX *features*)

)	;eval-when

#+Genera
(when (null (find-package :defsys))
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

#+Genera
(defun frob-pathname (subdir &optional (dir sys:fdefine-file-pathname))
  (namestring
    (make-pathname
      :defaults sys:fdefine-file-pathname
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
  ;; These files establish a uniform Lisp environment
  ("excl-verification" :features Allegro)
  ("lucid-before" :features lucid)
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

  ;; General Lisp extensions
  ("utilities")
  ("lisp-utilities")
  ("processes")
  ("queue")
  ("protocols")
  ("autoconstructor")

  ;; Establish a uniform stream model
  ("clim-streams")
  ("cl-stream-classes" :features (not clim-uses-lisp-stream-classes))
  ("minima-stream-classes" :features Minima)
  ("cl-stream-functions" 
   :features (and (not clim-uses-lisp-stream-functions) (not Lucid)))
  ("lucid-stream-functions" :features Lucid)
  ("genera-streams" :features Genera)
  ("excl-streams" :features Allegro)
  ("ccl-streams" :features CCL-2)

  ;; Basic utilities for Silica and CLIM
  ("clim-macros")
  ("transformations" :load-before-compile ("condpat"))
  ("regions")
  ("region-arithmetic")
  ("extended-regions")
  ("designs"))

(defsys:defsystem clim-silica
    (:default-pathname #+Genera "SYS:CLIM;REL-2;SILICA;"
      		       #+Minima "SYS:CLIM;REL-2;SILICA;"
		       #+Cloe-Runtime "\\clim\\rel-2\\silica\\"
		       #+Lucid "/home/hornig/clim/rel-2/silica/"
		       #+Allegro (frob-pathname "silica")
		       #+CMU "/home/hornig/clim/rel-2/silica/"
		       #+CCL-2 "ccl;clim-2.0:silica:"
      :default-binary-pathname #+Genera "SYS:CLIM;REL-2;SILICA;"
      			       #+Minima "SYS:CLIM;REL-2;SILICA;"
			       #+Cloe-Runtime "\\clim\\rel-2\\bin\\"
			       #+Lucid "/home/hornig/clim/rel-2/lcl4/"
			       #+Allegro (frob-pathname "silica")
			       #+CMU "/home/hornig/clim/rel-2/cmu/"
			       #+CCL-2 "ccl;clim-2.0:fasls:"
      :needed-systems (clim-utils)
      :load-before-compile (clim-utils))
  ;; "Silica"
  ("classes")
  ("text-style")
  ("macros")
  ("sheet")
  ("mirror")
  ("event")
  ("port")
  ("medium")
  ("framem")
  ("graphics")
  ("std-sheet")

  ;; "Windshield", aka "DashBoard"
  ;; First the layout gadgets
  ("layout")
  ("db-layout")
  ("db-box")
  ("db-table")

  ;; Then the "physical" gadgets
  ("gadgets")
  ("db-border")
  ("db-scroll")
  ("db-button")
  ("db-slider"))

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

  ;; Basic tools
  ("gestures")
  ("defprotocol")
  ("stream-defprotocols")
  ("defresource")
  ("temp-strings")
  ("coral-defs" :features CCL-2)
  ("clim-defs")
  ("stipples")
  
  ;; Definitions and protocols
  ("stream-class-defs")
  (#-Cloe-Runtime "interactive-defs" #+Cloe-Runtime "int-defs")
  ("cursor")
  ("view-defs")
  ("input-defs")
  ("input-protocol")
  ("output-protocol")
  ("drawing-state-mixin" :features (not Silica))
  ("window-protocol" :features (not Silica))

  ;; Output recording
  ("recording-defs"
   :load-before-compile ("clim-defs"))
  ("recording-protocol"
   :load-before-compile ("recording-defs"))
  ("text-recording"
   :load-before-compile ("recording-protocol"))
  ("graphics-recording"
   :load-before-compile ("recording-protocol") :features Silica)

  ;; Input editing
  (#-Cloe-Runtime "interactive-protocol" #+Cloe-Runtime "int-prot"
   :load-before-compile ("clim-defs"))
  ("input-editor-commands")

  (#-Cloe-Runtime "protocol-intermediaries" #+Cloe-Runtime "prot-int"
   :features (not Silica))

  ;; Formatted output definitions
  ("formatted-output-defs")
  ("incremental-redisplay"
   :load-before-compile ("clim-defs" "recording-protocol"))

  ;; Graphics
  ("defs-graphics-generics" :features (not Silica))
  (#-Cloe-Runtime "graphics-generics" #+Cloe-Runtime "gph-gene" :features (not Silica))

  ;; Windows
  ("coordinate-sorted-set")
  ("window-stream")

  ;; Presentation types
  ("completer")
  ("ptypes1"
   :load-before-compile ("clim-defs"))
  ("presentations"
   :load-before-compile ("ptypes1"))
  ("translators"
   :load-before-compile ("presentations"))
  ("histories"
   :load-before-compile ("presentations"))
  ("ptypes2"
   :load-before-compile ("translators"))
  (#-Cloe-Runtime "standard-types" #+Cloe-Runtime "std-typs"
   :load-before-compile ("ptypes2"))
  ("excl-presentations"
   :load-before-compile ("presentations")
   :features Allegro)

  ;; Formatted output
  ("table-formatting"
   :load-before-compile ("clim-defs" "incremental-redisplay"))
  ("graph-formatting"
   :load-before-compile ("clim-defs" "incremental-redisplay"))
  ("surround-output" 
   :load-before-compile ("clim-defs" "incremental-redisplay"))
  ("text-formatting"
   :load-before-compile ("clim-defs" "incremental-redisplay"))

  (#-Cloe-Runtime "graphics-internals" #+Cloe-Runtime "gph-inte" :features (not Silica))

  ;; Pointer tracking
  ("tracking-pointer")
  ("dragging-output"
   :load-before-compile ("tracking-pointer"))

  ("vertical-string" :features (not Silica))

  ;; CLX
  ("clx-implementation" :features (and (not Silica) Xlib))

  ;; Genera
  ("genera-implementation" :features (and (not Silica) Genera))

  ;; MCL
  ;("coral-scroll" :features (and (not Silica) CCL-2))
  ;("coral-implementation" :features (and (not Silica) CCL-2))
  ;("coral-fonts" :features  (and (not Silica) CCL-2))
  ;("coral-font-stuff" :features  (and (not Silica) CCL-2))
  ;("coral-events" :features (and (not Silica) CCL-2))
  ;("coral-streams" :features (and (not Silica) CCL-2))

  ;; CLOE
  ;("wheader" :features (and (not Silica) Cloe-Runtime))
  ;("windows" :features (and (not Silica) Cloe-Runtime)
  ; :load-before-compile ("wheader"))
  ;("cloe-implementation" :features (and (not Silica) Cloe-Runtime)
  ; :load-before-compile ("windows"))
  ;("cloe-events" :features (and (not Silica) Cloe-Runtime)
  ; :load-before-compile ("windows" "cloe-implementation"))
  ;("cloe-applications" :features (and (not Silica) Cloe-Runtime))

  ;; Postscript
  ;("postscript-implementation" :features (not Silica))
  ;("laserwriter-metrics" :features (not Silica)
  ; :load-before-compile ("postscript-implementation"))

  ;; Gadgets
  ("db-stream" :features Silica)
  ("gadget-output" :features Silica)

  ;; Application building substrate
  ("accept"
   :load-before-compile ("clim-defs" "ptypes2"))
  ("present"
   :load-before-compile ("clim-defs" "ptypes2"))
  ("command"
   :load-before-compile ("clim-defs" "ptypes2"))
  ("command-processor"
   :load-before-compile ("clim-defs" "command"))
  ("basic-translators"
   :load-before-compile ("ptypes2" "command"))
  ("define-application"
   :load-before-compile ("clim-defs" "command-processor") :features (not Silica))
  ("default-application"
   :load-before-compile ("define-application") :features (not Silica))
  ("frames" 
   :load-before-compile ("clim-defs" "command-processor") :features Silica)
  ("menus"
   :load-before-compile ("defresource" "clim-defs"))
  ;("mac-menus"
  ; :features (and (not Silica) CCL-2) :load-before-compile ("menus"))
  ("accept-values"
   :load-before-compile ("clim-defs" "incremental-redisplay"
			 #-Silica "define-application" #+Silica "frames"))
  ("pixmap-streams"
   :load-before-compile ("clim-defs"))
  ("item-list-manager")

  ;; Bootstrap everything
  ("stream-trampolines"
   :load-before-compile ("defprotocol" "stream-defprotocols"))
  ("lucid-after" :features lucid)
  ("prefill" :features (and (not Silica) (or Genera Cloe-Runtime))))


#+(and Silica Genera)
(defsys:defsystem genera-clim
    (:default-pathname "SYS:CLIM;REL-2;GENERA;"
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;GENERA;"
     :needed-systems (clim-standalone)
     :load-before-compile (clim-standalone))
  ("pkgdcl")
  ("genera-port")
  ("genera-mirror")
  ("genera-medium")
  ("genera-frames")
  ("genera-gadgets")  
  ("genera-activities"))

#+(and Silica CLX use-CLX)
(defsys:defsystem clx-clim
    (:default-pathname "SYS:CLIM;REL-2;CLX;"
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;CLX;"
     :needed-systems (clim-standalone)
     :load-before-compile (clim-standalone))
  ("pkgdcl")
  ("clx-port")
  ("clx-mirror")
  ("clx-medium")
  ("clx-frames")
  ("clx-gadgets"))


#+(and Silica Allegro)
(defsys:defsystem xlib
    (:default-pathname (frob-pathname "xlib")
	:default-binary-pathname (frob-pathname "xlib")
	:needed-systems (clim-standalone)
	:load-before-compile (clim-standalone))
  ("pkg")
  #+++ignore ("ffi" :eval-after (mapc #'load '("xlib/xlib.lisp" "xlib/x11-keysyms.lisp"
					       "xlib/last.lisp")))
  ("ffi")
  ("xlib-defs" #|:load-before-compile ("ffi") |#) ; Takes forever to compile...
  ("xlib-funs" :load-before-compile ("ffi"))
  ("x11-keysyms" :load-before-compile ("ffi"))
  ("last" :load-before-compile ("xlib-funs")))


#+(and Silica Allegro)
(defsys:defsystem xt-tk
    (:default-pathname (frob-pathname "tk")
     :default-binary-pathname (frob-pathname "tk")
     :needed-systems (xlib)
     :load-before-compile (xlib))
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
  ("xt-classes")
  ("xt-init"))

#+(and Silica Allegro)
(defsys:defsystem xm-tk
    (:default-pathname (frob-pathname "tk")
     :default-binary-pathname (frob-pathname "tk")
     :needed-systems (xt-tk)
     :load-before-compile (xt-tk))
  ;; Motif specific stuff
  ("xm-classes")
  ("xm-init")
  ("xm-widgets")
  ("xm-font-list")
  ("xm-protocols")
  ("convenience")
  #+ignore ("examples")

  ;; More General stuff
  ("make-widget"))

#+(and Silica Allegro)
(defsys:defsystem ol-tk
    (:default-pathname (frob-pathname "tk")
     :default-binary-pathname (frob-pathname "tk")
     :needed-systems (xt-tk)
     :load-before-compile (xt-tk))
  ;; OpenLook specific stuff
  ("ol-classes")
  ("ol-init")
  ("ol-callbacks")
  #+ignore("ol-examples")
  ("make-widget"))

#+(and Silica Allegro)
(defsys:defsystem motif-clim
    (:default-pathname (frob-pathname "xm-silica")
     :default-binary-pathname (frob-pathname "xm-silica")
     :needed-systems (clim-standalone xm-tk)
     :load-before-compile (clim-standalone xm-tk))
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
  ("xm-cursor")
  ("xt-pixmaps"))

#+(and Silica Allegro)
(defsys:defsystem openlook-clim
    (:default-pathname (frob-pathname "xm-silica")
     :default-binary-pathname (frob-pathname "xm-silica")
     :needed-systems (clim-standalone ol-tk)
     :load-before-compile (clim-standalone ol-tk))
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


#+(and Silica CCL-2)
(defsys:defsystem ccl-clim
    (:default-pathname "SYS:CLIM;REL-2;CCL;"
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;CCL;"
     :needed-systems (clim-standalone)
     :load-before-compile (clim-standalone))
  ("pkgdcl")
  ("ccl-port")
  ("ccl-mirror")
  ("ccl-medium")
  ("ccl-frames")
  ("ccl-gadgets")
  ("ccl-menus"))


#+(and Silica Cloe-Runtime)
(defsys:defsystem cloe-clim
    (:default-pathname "SYS:CLIM;REL-2;CLOE;"
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;CLOE;"
     :needed-systems (clim-standalone)
     :load-before-compile (clim-standalone))
  ("pkgdcl")
  ("wheader")
  ("windows")
  ("cloe-port")
  ("cloe-mirror")
  ("cloe-medium")
  ("cloe-frames")
  ("cloe-gadgets")
  ("cloe-menus"))


#+(and Silica use-PostScript)
(defsys:defsystem postscript-clim
    (:default-pathname "SYS:CLIM;REL-2;POSTSCRIPT;"
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;POSTSCRIPT;"
     :needed-systems (clim-standalone)
     :load-before-compile (clim-standalone))
  ("pkgdcl")
  ("postscript-port")
  ("postscript-mirror")
  ("postscript-medium")
  ("postscript-frames")
  ("postscript-gadgets")
  ("laserwriter-metrics"))


#+Genera (progn

(defsys:import-into-sct 'clim-utils :subsystem t
			:pretty-name "CLIM Utilities"
			:default-pathname "SYS:CLIM;REL-2;UTILS;")

(defsys:import-into-sct 'clim-silica :subsystem t
			:pretty-name "CLIM Silica"
			:default-pathname "SYS:CLIM;REL-2;SILICA;")

(defsys:import-into-sct 'clim-standalone :subsystem t
			:pretty-name "CLIM Standalone"
			:default-pathname "SYS:CLIM;REL-2;CLIM;")

#+Silica
(defsys:import-into-sct 'genera-clim :subsystem t
			:pretty-name "Genera CLIM"
			:default-pathname "SYS:CLIM;REL-2;GENERA;")

#+Silica
(defsys:import-into-sct 'clx-clim :subsystem t
			:pretty-name "CLX CLIM"
			:default-pathname "SYS:CLIM;REL-2;CLX;")

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
	   "clim-standalone"
	   #+Silica "genera-clim"
	   #+Silica "clx-clim"))

#+(and Silica ignore)
(defsys:import-into-sct 'motif-clim :subsystem t
			:pretty-name "Motif CLIM"
			:default-pathname "SYS:CLIM;REL-2;XM-SILICA;")

#+(and Silica ignore)
(defsys:import-into-sct 'openlook-clim :subsystem t
			:pretty-name "OpenLook CLIM"
			:default-pathname "SYS:CLIM;REL-2;XM-SILICA;")

#+(and Silica ignore)
(sct:defsystem clim-tags-table
    (:pretty-name "CLIM Tags Table"
     :default-pathname "SYS:CLIM;REL-2;CLIM;"
     :maintain-journals nil
     :default-module-type :system)
  (:module defsystem "sys:clim;rel-2;sys;defsystem"
	   (:type :lisp) (:root-module nil))
  (:serial "clim"
	   "motif-clim"
	   "openlook-clim"))

)

#+Minima-Developer (progn

(defsys:import-into-sct 'clim-utils :subsystem t
			:sct-name :minima-clim-utils :pretty-name "Minima CLIM Utilities"
			:default-pathname "SYS:CLIM;REL-2;UTILS;")

(defsys:import-into-sct 'clim-silica :subsystem t
			:sct-name :minima-clim-silica :pretty-name "Minima CLIM Silica"
			:default-pathname "SYS:CLIM;REL-2;SILICA;")

(defsys:import-into-sct 'clim-standalone :subsystem t
			:sct-name :minima-clim-standalone :pretty-name "Minima CLIM Standalone"
			:default-pathname "SYS:CLIM;REL-2;CLIM;")

#+Silica
(defsys:import-into-sct 'clx-clim :subsystem t
			:sct-name :minima-clx-clim :pretty-name "CLX CLIM"
			:default-pathname "SYS:CLIM;REL-2;CLX;")

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
	   "minima-clim-standalone"
	   #+Silica "minima-clx-clim"))

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
||#
