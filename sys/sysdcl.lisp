;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: sysdcl.lisp,v 1.19 92/07/08 16:31:13 cer Exp $

(in-package #-ANSI-90 :user #+ANSI-90 :cl-user)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

(eval-when (compile load eval)

;;; Tell the world that we're here
;;;--- These need to be in the CLIM.fasl also.
;;;--- Currently they're in EXCL-VERIFICATION but that does not seem the best place.
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

#+(or Genera Lucid)
(pushnew :use-CLX *features*)

)	;eval-when

#+Genera
(when (null (find-package :clim-defsys))
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


(setq clim-defsys:*load-all-before-compile* t)

#-Genera
(defun frob-pathname (subdir
		      &optional (dir #+Allegro excl::*source-pathname*
				     #+Lucid lcl::*source-pathname*
				     #-(or Allegro Lucid) (or *compile-file-pathname*
							      *load-pathname*)))
  (namestring
    (make-pathname
      :directory (append (butlast (pathname-directory dir)) (list subdir)))))

#+Genera
(defun frob-pathname (subdir &optional (dir sys:fdefine-file-pathname))
  (namestring
    (make-pathname
      :defaults dir
      :directory (append (butlast (pathname-directory dir)) 
			 (mapcar #'string-upcase (list subdir))))))


(clim-defsys:defsystem clim-utils
    (:default-pathname #+Genera "SYS:CLIM;REL-2;UTILS;"
		       #-Genera (frob-pathname "utils")
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;UTILS;"
			      #-Genera (frob-pathname "utils"))
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
  (#-Cloe-Runtime "cl-stream-classes" #+Cloe-Runtime "clstrcla"
   :features (not clim-uses-lisp-stream-classes))
  ("minima-stream-classes" :features Minima)
  (#-Cloe-Runtime "cl-stream-functions" #+Cloe-Runtime "clstrfun"
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

(clim-defsys:defsystem clim-silica
    (:default-pathname #+Genera "SYS:CLIM;REL-2;SILICA;"
		       #-Genera (frob-pathname "silica")
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;SILICA;"
			      #-Genera (frob-pathname "silica")
     :needed-systems (clim-utils)
     :load-before-compile (clim-utils))
  ;; "Silica"
  ("macros")
  ("classes")
  ("text-style")
  ("sheet")
  ("mirror")
  ("event")
  ("port")
  ("medium")
  ("framem")
  ("graphics")
  ("pixmaps")
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

(clim-defsys:defsystem clim-standalone
    (:default-pathname #+Genera "SYS:CLIM;REL-2;CLIM;"
		       #-Genera (frob-pathname "clim")
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;CLIM;"
			      #-Genera (frob-pathname "clim")
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

  ;; Output recording
  ("recording-defs"
   :load-before-compile ("clim-defs"))
  ("recording-protocol"
   :load-before-compile ("recording-defs"))
  ("text-recording"
   :load-before-compile ("recording-protocol"))
  ("graphics-recording"
   :load-before-compile ("recording-protocol"))
  ("design-recording"
   :load-before-compile ("graphics-recording"))

  ;; Input editing
  (#-Cloe-Runtime "interactive-protocol" #+Cloe-Runtime "int-prot"
   :load-before-compile ("clim-defs"))
  ("input-editor-commands")

  ;; Formatted output definitions
  ("formatted-output-defs")
  ("incremental-redisplay"
   :load-before-compile ("clim-defs" "recording-protocol"))

  ;; Windows
  ("coordinate-sorted-set")
  ("window-stream")
  ("pixmap-streams")

  ;; Presentation types
  ("ptypes1"
   :load-before-compile ("clim-defs"))
  ("completer"
   :load-before-compile ("ptypes1"))
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

  ;; Pointer tracking
  ("tracking-pointer")
  ("dragging-output"
   :load-before-compile ("tracking-pointer"))

  ;; Gadgets
  ("db-stream")
  ("gadget-output")

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
  ("frames" 
   :load-before-compile ("clim-defs" "command-processor"))
  ("default-frame" 
   :load-before-compile ("frames"))
  ("noting-progress"
   :load-before-compile ("frames"))
  ("menus"
   :load-before-compile ("defresource" "clim-defs"))
  ("accept-values"
   :load-before-compile ("clim-defs" "incremental-redisplay" "frames"))
  ("drag-and-drop" 
   :load-before-compile ("frames"))
  ("item-list-manager")

  ;; Bootstrap everything
  ("stream-trampolines"
   :load-before-compile ("defprotocol" "stream-defprotocols"))
  ("lucid-after" :features lucid)
  ("prefill" :features (or Genera Cloe-Runtime)))


#+Genera
(clim-defsys:defsystem genera-clim
    (:default-pathname #+Genera "SYS:CLIM;REL-2;GENERA;"
		       #-Genera (frob-pathname "genera")
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;GENERA;"
			      #-Genera (frob-pathname "genera")
     :needed-systems (clim-standalone)
     :load-before-compile (clim-standalone))
  ("pkgdcl")
  ("genera-port")
  ("genera-mirror")
  ("genera-medium")
  ("genera-pixmaps")
  ("genera-frames")
  ("genera-activities")
  ("genera-prefill"))

#+(and CLX use-CLX)
(clim-defsys:defsystem clx-clim
    (:default-pathname #+Genera "SYS:CLIM;REL-2;CLX;"
		       #-Genera (frob-pathname "clx")
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;CLX;"
			      #-Genera (frob-pathname "clx")
     :needed-systems (clim-standalone)
     :load-before-compile (clim-standalone))
  ("pkgdcl")
  ("clx-port")
  ("clx-mirror")
  ("clx-medium")
  ("clx-pixmaps")
  ("clx-frames")
  ("clx-prefill" :features (or Genera Cloe-Runtime)))


#+Lucid
(clim-defsys:defsystem lucid-clim
    (:default-pathname nil
     :default-binary-pathname nil
     :needed-systems (clx-clim)
     :load-before-compile (clx-clim)))


#+Allegro
(clim-defsys:defsystem xlib
    (:default-pathname #+Genera "SYS:CLIM;REL-2;XLIB;"
		       #-Genera (frob-pathname "xlib")
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;XLIB;"
			      #-Genera (frob-pathname "xlib")
     :needed-systems (clim-standalone)
     :load-before-compile (clim-standalone))
  ("pkg")
  #+++ignore ("ffi" :eval-after (mapc #'load '("xlib/xlib.lisp" "xlib/x11-keysyms.lisp"
					       "xlib/last.lisp")))
  ("ffi")
  ("xlib-defs" #|:load-before-compile ("ffi") |#) ; Takes forever to ; compile...
  ("load-xlib")
  ("xlib-funs" :load-before-compile ("ffi"))
  ("x11-keysyms" :load-before-compile ("ffi"))
  ("last" :load-before-compile ("load-xlib" "xlib-funs")))

#+Allegro
(clim-defsys:defsystem xt-tk
    (:default-pathname #+Genera "SYS:CLIM;REL-2;TK;"
		       #-Genera (frob-pathname "tk")
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;TK;"
			      #-Genera (frob-pathname "tk")
     :needed-systems (xlib)
     :load-before-compile (xlib))
  ;; General stuff
  ("pkg")
  ("macros")
  ("xt-defs")				; Used to be 'xtk'.
  ("foreign-obj")
  ;; Xlib stuff
  ("xlib")
  ("font")
  ("gcontext")
  ("graphics")
  
  ;; Toolkit stuff
  ("meta-tk")
  ("make-classes")
  ("foreign")
  ("widget")
  ("resources")
  ("event")
  ("callbacks")
  ("xt-classes")
  ("xt-init"))

#+Allegro
(clim-defsys:defsystem xm-tk
    (:default-pathname #+Genera "SYS:CLIM;REL-2;TK;"
		       #-Genera (frob-pathname "tk")
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;TK;"
			      #-Genera (frob-pathname "tk")
     :needed-systems (xt-tk)
     :load-before-compile (xt-tk))
  ;; Motif specific stuff
  ("load-xm")
  ("xm-defs")
  ("xm-classes")
  ("xm-callbacks")
  ("xt-funs") ;;--- This is really in tk but because of the loading
  ("xm-funs")
  ("xm-classes")
  ("xm-init")
  ("xm-widgets")
  ("xm-font-list")
  ("xm-protocols")
  ("convenience")
  ("make-widget"))

#+Allegro
(clim-defsys:defsystem ol-tk
    (:default-pathname #+Genera "SYS:CLIM;REL-2;TK;"
		       #-Genera (frob-pathname "tk")
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;TK;"
			      #-Genera (frob-pathname "tk")
     :needed-systems (xt-tk)
     :load-before-compile (xt-tk))
  ;; OpenLook specific stuff
  ("ol-defs")
  ("load-ol")
  ("xt-funs") ;;--- This is really in tk but because of the loading
  ("ol-funs")
  ("ol-classes")
  ("ol-init")
  ("ol-widgets")
  ("ol-callbacks")
  ("make-widget"))

#+Allegro
(clim-defsys:defsystem motif-clim
    (:default-pathname #+Genera "SYS:CLIM;REL-2;TK-SILICA;"
		       #-Genera (frob-pathname "tk-silica")
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;TK-SILICA;"
			      #-Genera (frob-pathname "tk-silica")
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
  ("xt-cursor")
  ("xt-pixmaps"))

#+Allegro
(clim-defsys:defsystem openlook-clim
    (:default-pathname #+Genera "SYS:CLIM;REL-2;TK-SILICA;"
		       #-Genera (frob-pathname "tk-silica")
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;TK-SILICA;"
			      #-Genera (frob-pathname "tk-silica")
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


#+CCL-2
(clim-defsys:defsystem ccl-clim
    (:default-pathname #+Genera "SYS:CLIM;REL-2;CCL;"
		       #-Genera (frob-pathname "ccl")
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;CCL;"
			      #-Genera (frob-pathname "ccl")
     :needed-systems (clim-standalone)
     :load-before-compile (clim-standalone))
  ("pkgdcl")
  ("ccl-port")
  ("ccl-mirror")
  ("ccl-medium")
  ("ccl-frames")
  ("ccl-gadgets")
  ("ccl-menus"))


#+Cloe-Runtime
(clim-defsys:defsystem cloe-clim
    (:default-pathname #+Genera "SYS:CLIM;REL-2;CLOE;"
		       #-Genera (frob-pathname "cloe")
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;CLOE;"
			      #-Genera (frob-pathname "cloe")
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


(clim-defsys:defsystem postscript-clim
    (:default-pathname #+Genera "SYS:CLIM;REL-2;POSTSCRIPT;"
		       #-Genera (frob-pathname "postscript")
     :default-binary-pathname #+Genera "SYS:CLIM;REL-2;POSTSCRIPT;"
			      #-Genera (frob-pathname "postscript")
     :needed-systems (clim-standalone)
     :load-before-compile (clim-standalone))
  ("pkgdcl")
  ("postscript-port")
  ("postscript-medium")
  ("laserwriter-metrics"))


#+Genera (progn

(clim-defsys:import-into-sct 'clim-utils :subsystem t
			     :pretty-name "CLIM Utilities"
			     :default-pathname "SYS:CLIM;REL-2;UTILS;")

(clim-defsys:import-into-sct 'clim-silica :subsystem t
			     :pretty-name "CLIM Silica"
			     :default-pathname "SYS:CLIM;REL-2;SILICA;")

(clim-defsys:import-into-sct 'clim-standalone :subsystem t
			     :pretty-name "CLIM Standalone"
			     :default-pathname "SYS:CLIM;REL-2;CLIM;")

(clim-defsys:import-into-sct 'genera-clim :subsystem t
			     :pretty-name "Genera CLIM"
			     :default-pathname "SYS:CLIM;REL-2;GENERA;")

(clim-defsys:import-into-sct 'clx-clim :subsystem t
			     :pretty-name "CLX CLIM"
			     :default-pathname "SYS:CLIM;REL-2;CLX;")

(clim-defsys:import-into-sct 'postscript-clim :subsystem t
			     :pretty-name "PostScript CLIM"
			     :default-pathname "SYS:CLIM;REL-2;POSTSCRIPT;")

(sct:defsystem clim
    (:pretty-name "CLIM"
     :default-pathname "SYS:CLIM;REL-2;"
     :journal-directory "SYS:CLIM;REL-2;PATCH;"
     :default-module-type :system
     :patches-reviewed "Bug-CLIM-Doc"
     :source-category :optional)
  (:module defsystem "sys:clim;rel-2;sys;defsystem"
	   (:type :lisp) (:root-module nil))
  (:serial "clim-utils"
	   "clim-silica"
	   "clim-standalone"
	   "genera-clim"
	   "clx-clim"
	   "postscript-clim"))

#+++ignore
(progn
(clim-defsys:import-into-sct 'motif-clim :subsystem t
			     :pretty-name "Motif CLIM"
			     :default-pathname "SYS:CLIM;REL-2;TK-SILICA;")

(clim-defsys:import-into-sct 'openlook-clim :subsystem t
			     :pretty-name "OpenLook CLIM"
			     :default-pathname "SYS:CLIM;REL-2;TK-SILICA;")

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

)	;#+Genera

#+Minima-Developer (progn

(clim-defsys:import-into-sct 'clim-utils :subsystem t
			     :sct-name :minima-clim-utils :pretty-name "Minima CLIM Utilities"
			     :default-pathname "SYS:CLIM;REL-2;UTILS;")

(clim-defsys:import-into-sct 'clim-silica :subsystem t
			     :sct-name :minima-clim-silica :pretty-name "Minima CLIM Silica"
			     :default-pathname "SYS:CLIM;REL-2;SILICA;")

(clim-defsys:import-into-sct 'clim-standalone :subsystem t
			     :sct-name :minima-clim-standalone :pretty-name "Minima CLIM Standalone"
			     :default-pathname "SYS:CLIM;REL-2;CLIM;")

(clim-defsys:import-into-sct 'clx-clim :subsystem t
			     :sct-name :minima-clx-clim :pretty-name "CLX CLIM"
			     :default-pathname "SYS:CLIM;REL-2;CLX;")

(zl:::sct:defsystem minima-clim
    (:pretty-name "Minima CLIM"
     :default-pathname "SYS:CLIM;REL-2;"
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
	   "minima-clx-clim"))

)	;#+Minima-Developer


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
