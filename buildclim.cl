;; Load this file to make a clim image.
;; $Id: buildclim.cl,v 1.4 1998/08/06 23:15:36 layer Exp $

(excl:generate-application
 #+ics "climi" #-ics "clim"
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
