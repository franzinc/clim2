;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: distribution-script.lisp,v 1.4 92/07/27 11:03:09 cer Exp $

#|
:Distribute Systems CLIM-BINARIES Latest :Distribute Patch Sources No :Include Patches Yes :Query Confirm-Only :Machine Types Imach :File Types Binaries 
|#
(sct:define-distribution-system clim-binaries
    (:default-pathname "SYS:CLIM;REL-2;"
     :distribute-sources nil
     :distribute-binaries t
     :source-category :restricted)
  (:module clim
    ("CLIM" "Genera-CLIM" "CLX-CLIM" "PostScript-CLIM" "CLIM-Demo")
    (:type :system) (:version :latest))
  (:serial 
    ";SYS;LOAD-CLIM2.LISP"
    clim
    ";TEST;TEST-SUITE.IBIN"
    ";TEST;TEST.IBIN"
    ";TEST;TEST-BUTTONS.IBIN"
    ";TEST;TEST-SLIDERS.IBIN"))

#|
:Distribute Systems CLIM-SOURCES Newest :Distribute Patch Sources Yes :Include Patches Yes :Query Confirm-Only :Machine Types Imach :File Types Sources
|#
(sct:define-distribution-system clim-sources
    (:default-pathname "SYS:CLIM;REL-2;"
     :distribute-sources t
     :distribute-binaries nil
     :source-category :restricted)
  (:module clim
    ("CLIM" "Genera-CLIM" "CLX-CLIM" "PostScript-CLIM" "CLIM-Demo" "CLIM-Compatibility")
    (:type :system) (:version :newest))
  (:serial 
    ";SYS;LOAD-CLIM2.LISP"
    ";TEST;TEST-SUITE.LISP"
    ";TEST;TEST.LISP"
    ";TEST;TEST-BUTTONS.LISP"
    ";TEST;TEST-SLIDERS.LISP"
    ";SPECS;CLIM.PS"
    clim))

