;; -*- mode: common-lisp; package: user -*-
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
;; $fiHeader$


(setq excl::*record-source-file-info* t
      excl::*load-source-file-info* t)

(without-package-locks
 (excl::compile-file-if-needed "sys/defsystem")
 (load "sys/defsystem")
 (load "sys/sysdcl"))

(without-package-locks
 (defsys::compile-system 'clim-utils)
 (defsys::load-system 'clim-utils))


(load "silica/sysdcl")
(defsys::compile-system :silica)
(defsys::load-system :silica)

(load "clim/sysdcl")
(defsys::compile-system :clim)
(defsys::load-system :clim)



(load "xlib/sysdcl")
(defsys::compile-system :xlib)
(tenuring (defsys::load-system :xlib))


(load "tk/defsys.cl")
(defsys::compile-system :basic-tk)
(defsys::load-system :basic-tk)

(let ((which (or (and (boundp 'which)
		      which)
		 :motif)))
  (let ((sys
	 (ecase which
	   (:motif :xm-tk)
	   (:openlook :ol-tk))))
    (defsys::compile-system sys)
    (defsys::load-system sys))

  (defsys::compile-system :hi-tk)
  (defsys::load-system :hi-tk)

  (load "xm-silica/sysdcl.cl")
  
  (let ((sys (ecase which
	       (:motif :xm-silica)
	       (:openlook :ol-silica))))
    (defsys::compile-system sys)
    (defsys::load-system sys))

  
  (let ((sys (ecase which
	       (:motif :xm-ws)
	       (:openlook :ol-ws))))
    (defsys::compile-system sys)
    (defsys::load-system sys)))

#+:composer
(progn
  (excl::compile-file-if-needed "draw-sheets.cl")
  (load "draw-sheets")
  )

(excl::compile-file-if-needed "test/test")
(load "test/test")

(excl::compile-file-if-needed "test/test-suite")
(load "test/test-suite")

