;; Load this file to make a clim image.
;; $Id: buildclim.cl,v 1.2 1998/03/30 16:37:51 layer Exp $

(excl:generate-application
 #+ics "climi" #-ics "clim"
 "./"
 '(
   :climg
   :climxm
;;;; The rest of the modules are optional.  Include them or not according
;;;; to your needs:
   :clim-debug
   :clim-debugxm
   :climps
   :climhpgl
   #+ignore :climdemo
   )
 :autoload-warning nil
 :image-only t
 :print-startup-message :default
 :purify t
 :debug-on-error t
 :internal-debug "buildclim.out"
 :shlib-warning nil
 :libfasl-warning nil
 :discard-local-name-info t
 :discard-source-file-info t
 :discard-xref-info t)

(sys:copy-file
 #+(and mswindows ics) "lispi.exe"
 #+(and mswindows (not ics)) "lisp.exe"
 #+(and (not mswindows) ics) "lispi"
 #+(and (not mswindows) (not ics)) "lisp"

 #+(and mswindows ics) "climi.exe"
 #+(and mswindows (not ics)) "clim.exe"
 #+(and (not mswindows) ics) "climi"
 #+(and (not mswindows) (not ics)) "clim"

 :link-ok t)
