
;;; LOAD the file PATH.LSP first!

;;; use the following to load clim 2.0 WITHOUT demos

#+aclpc
(load (climpath "sys\\defsyste.fsl"))
#+allegro
(load (climpath "sys\\defsystem.fasl"))

(load (climpath "sys\\sysdcl-pc.lisp"))

(load (climpath "aclpc\\sysdcl.lisp"))

;(clim-defsystem:load-system "clim-standalone")
; uncomment above and comment below to make
; a clim standalone
#+aclpc
(clim-defsystem:load-system "aclpc-clim")
#+allegro
(clim-defsystem:load-system "aclnt-clim")
