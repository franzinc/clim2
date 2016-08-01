;; Load this file to make a clim image.

(excl:build-lisp-image
 #+ics "sys:clim.dxl" #-ics "sys:clim8.dxl"
 :lisp-files
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
 :runtime nil
 :build-input (namestring (translate-logical-pathname "sys:buildclim.out"))
 :discard-local-name-info t
 :discard-source-file-info t
 :discard-xref-info t)

(let ((dest
       #+(and mswindows ics) "sys:clim.exe"
       #+(and mswindows (not ics)) "sys:clim8.exe"
       #+(and (not mswindows) ics) "sys:clim"
       #+(and (not mswindows) (not ics)) "sys:clim8"))
  (ignore-errors (delete-file dest))
  (sys:copy-file (merge-pathnames (excl::curpgmname) "sys:") dest))

