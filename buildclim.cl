;; Load this file to make a clim image.
;; $Id: buildclim.cl,v 1.5.34.1 2000/09/05 19:06:37 layer Exp $

(excl:generate-application
 #+ics "clim" #-ics "clim8"
 "./"
 '(
   #-mswindows :climxm
   #+mswindows :climnt
   :climg ;; ...require'd by the above, but listed for completeness.
;;;; The rest of the modules are optional.  Include them or not according
;;;; to your needs:
;;;   #-mswindows :clim-debug
;;;   #-mswindows :clim-debugxm
;;;   :climps
;;;   #-mswindows :climhpgl
;;;   :climdemo
   )
 :autoload-warning nil
 :image-only t
 :runtime nil
 :purify t
 :debug-on-error t
 :internal-debug "buildclim.out"
 :shlib-warning nil
 :libfasl-warning nil
 :discard-local-name-info t
 :discard-source-file-info t
 :discard-xref-info t)

(sys:copy-file
 (excl::curpgmname)

 #+(and mswindows ics) "clim.exe"
 #+(and mswindows (not ics)) "clim8.exe"
 #+(and (not mswindows) ics) "clim"
 #+(and (not mswindows) (not ics)) "clim8")
