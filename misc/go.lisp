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

(setq excl::*record-source-file-info* t
      excl::*load-source-file-info* t)

(without-package-locks
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
(defsys::compile-system :xm-tk)
(defsys::load-system :xm-tk)

(load "xm-silica/sysdcl.cl")
(defsys::compile-system :xm-silica)
(defsys::load-system :xm-silica)

(defsys::compile-system :xm-ws)
(defsys::load-system :xm-ws)

#+:composer
(progn
  (excl::compile-file-if-needed "draw-sheets.cl")
  (load "draw-sheets")
  )

(load "test/test")

(excl::compile-file-if-needed "test/test-suite")
(load "test/test-suite")

