;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10; Lowercase: Yes -*-
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

;; $fiHeader: sysdcl.lisp,v 1.15 91/08/05 14:36:27 cer Exp $

(in-package #-ansi-90 :user #+ansi-90 :cl-user)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved."
"Copyright (c) 1991, Franz Inc. All rights reserved"

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
(when (null (find-package :defsys))
  (load "SYS:CLIM;SYS;DEFSYSTEM"))

#+(or excl ccl-2 Minima)
(pushnew :clim-uses-lisp-streams *features*)

#+(and excl (version>= 4 1))
(pushnew :allegro-v4.0-constructors *features*)

;;; CLIM-ANSI-Conditions means this lisp truly supports the ANSI CL condition system
;;; CLIM-Conditions      means that it has a macro called DEFINE-CONDITION but that it works
;;;                      like Allegro 3.1.13 or Lucid.
(eval-when (compile load eval)
  (pushnew #+Lucid :CLIM-Conditions
           #+(or ansi-90 Symbolics) :clim-ansi-Conditions
	   #-(or ansi-90 Symbolics Lucid)
	   (error "Figure out what condition system for this Lisp")
	   *features*)
  )	;eval-when

(setq defsys:*load-all-before-compile* t)

#+excl
(defun frob-pathname (subdir &optional (dir excl::*source-pathname*))
    (namestring
     (make-pathname
      :directory (append (butlast (pathname-directory dir))
			 (list subdir)))))

(defsys:defsystem clim-utils
  (:default-pathname #+Genera "SYS:CLIM;UTILS;"
   #+Minima "MINIMA:CLIM;UTILS;"
   #+Cloe-Runtime "\\clim\\utils\\"
   #+lucid "/home/hornig/clim/utils/"
   #+excl (frob-pathname "utils")
   #+ccl-2 "ccl;clim-1.0:utils:"
   :default-binary-pathname #+Genera "SYS:CLIM;CLIM;"
   #+Minima "MINIMA:CLIM;UTILS;"
   #+Cloe-Runtime "\\clim\\bin\\"
   #+lucid "/home/hornig/clim/lcl4/"
   #+excl (frob-pathname "utils")
   #+ccl-2 "ccl;clim-1.0:fasls:")

  ("excl-verification" :features excl)
  ("defpackage" :features (or excl (not ansi-90)))
  ("packages")
  #+ccl-2 ("coral-char-bits" :features ccl-2)
  ("defun-utilities") ;; extract-declarations and friends
  ("defun" :features (or Genera (not ansi-90)))
  ("reader")
  ("clos-patches")
  ("utilities")
  ("lisp-utilities")
  ("condpat" :features CLIM-conditions)  ;get the define-condition macro
  ("clos")
  ("queue")
  ("protocols")
  ("autoconstructor")
  ("cl-streams" :features (not CLIM-uses-lisp-streams))
  ("excl-streams" :features excl)
  ("transformations")
  ("regions")
  ("region-arithmetic")
  ("designs"))

(defsys:defsystem clim-standalone
    (:default-pathname #+Genera "SYS:CLIM;CLIM;"
     #+Minima "MINIMA:CLIM;CLIM;"
     #+Cloe-Runtime "\\clim\\clim\\"
     #+lucid "/home/hornig/clim/clim/"
     #+excl (frob-pathname "clim")
     #+ccl-2 "ccl;clim-1.0:clim:"
     :default-binary-pathname #+Genera "SYS:CLIM;CLIM;"
     #+Minima "MINIMA:CLIM;CLIM;"
     #+Cloe-Runtime "\\clim\\bin\\"
     #+lucid "/home/hornig/clim/lcl4/"
     #+excl (frob-pathname "clim")
     #+ccl-2 "ccl;clim-1.0:fasls:"
     :needed-systems (clim-utils)
     :load-before-compile (clim-utils))

  ("pkgdcl")
  ("shift-mask")
  ("defprotocol")
  ("stream-defprotocols")
  ("defresource")
  ("temp-strings")

  #+ccl-2 ("coral-defs" :features Coral)

  ("clim-defs")

  ("stipples")

  #+Silica ("clim-sheet" :features Silica)
  #+Silica ("clim-silica-glue" :features Silica)

  ("stream-class-defs")
  (#-CLOE-Runtime "interactive-defs" #+CLOE-Runtime "int-defs")

  ("text-style" :features (not Silica))
   
  ("cursor")

  ("view-defs")

  ("input-defs")
  ("input-protocol")
  ("output-protocol")

  #+Silica ("clim-x-stuff" :features (and Silica xlib))
  #+Silica ("clim-genera-stuff" :features (and Silica Genera))
  #+Silica ("clim-coral-stuff" :features (and Silica Coral))

  ("drawing-state-mixin" :features (not Silica))
  ("window-protocol" :features (not Silica))

  ("output-recording-protocol"
   :load-before-compile ("clim-defs"))
  (#-CLOE-Runtime "interactive-protocol" #+CLOE-Runtime "int-prot"
		  :load-before-compile ("clim-defs"))
  ("input-editor-commands")		; :load-before-compile ("lisp-utilities")
  (#-CLOE-Runtime "protocol-intermediaries" #+CLOE-Runtime "prot-int")

  ("formatted-output-defs")
  ("incremental-redisplay"
   :load-before-compile ("clim-defs" "output-recording-protocol"))

  ("defs-graphics-generics" :features (not Silica))
  (#-CLOE-Runtime "graphics-generics" #+CLOE-Runtime "gph-gene" :features (not Silica))

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
  (#-CLOE-Runtime "standard-types" #+CLOE-Runtime "std-typs"
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

  (#-CLOE-Runtime "graphics-internals" #+CLOE-Runtime "gph-inte" :features (not Silica))

  ("tracking-pointer")
  ("dragging-output"
   :load-before-compile ("tracking-pointer"))

  ("vertical-string" :features (not Silica))

  ("clx-implementation" :features (and (not Silica) Xlib))

  ("genera-implementation" :features (and (not Silica) Genera))

  ("wheader" :features (and (not Silica) Cloe-Runtime))
  ("windows" :features (and (not Silica) Cloe-Runtime)
	     :load-before-compile ("wheader"))
  ("cloe-implementation" :features (and (not Silica) Cloe-Runtime)
			 :load-before-compile ("windows"))
  ("cloe-events" :features (and (not Silica) Cloe-Runtime)
		 :load-before-compile ("windows" "cloe-implementation"))
  ("cloe-applications" :features Cloe-Runtime)

  ("postscript-implementation" :features (not Silica))
  ("laserwriter-metrics" :features (not Silica)
			 :load-before-compile ("postscript-implementation"))

  ;; have to duplicate this entry for each "port" that uses X
  ;; The port mechansim can't deal with the fact that Genera might or might
  ;; not have CLX loaded
  ("menus"
   :load-before-compile ("defresource" "clim-defs"))
  #+ccl-2 ("mac-menus" :features ccl-2
		       :load-before-compile ("menus"))
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
   :load-before-compile ("clim-defs"))
  ("default-application"
   :load-before-compile ("define-application"))
  ("genera-activities" :features Genera
		       :load-before-compile ("default-application"))
  ("accept-values"
   :load-before-compile ("clim-defs" "incremental-redisplay"))
  ("item-list-manager")
  ("stream-trampolines"
   :load-before-compile ("defprotocol" "stream-defprotocols"))
  ("prefill" :features (or Genera Cloe-Runtime)))



#+Genera (progn

(defsys:import-into-sct 'clim-utils :subsystem t
			:pretty-name "CLIM Utilities"
			:default-pathname "SYS:CLIM;UTILS;"
			:default-destination-pathname "SYS:CLIM;UTILS;")

(defsys:import-into-sct 'clim-standalone :subsystem t
			:pretty-name "CLIM Standalone"
			:default-pathname "SYS:CLIM;CLIM;"
			:default-destination-pathname "SYS:CLIM;CLIM;")

(sct:defsystem clim
    (:pretty-name "CLIM"
     :default-pathname "SYS:CLIM;CLIM;"
     :journal-directory "SYS:CLIM;PATCH;"
     :default-module-type :system
     :patches-reviewed "Bug-CLIM-Doc"
     :source-category :optional)
  (:module defsystem "sys:clim;sys;defsystem"
	   (:type :lisp) (:root-module nil))
  (:serial "clim-utils"
	   "clim-standalone"))

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
