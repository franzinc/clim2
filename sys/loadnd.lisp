;; See the file LICENSE for the full license governing this code.
;;

(in-package :user)

#+aclpc (load (climpath "sys\\defsyste.fsl"))
#+acl86win32 (load (climpath "sys\\defsystem.fasl"))

#+aclpc (load (climpath "sys\\sysdcl-pc.lisp"))
#+acl86win32 (load (climpath "sys\\sysdcl.lisp"))

(load (climpath "aclpc\\sysdcl.lisp"))

;#+aclpc (clim-defsystem:load-system "clim-standalone")
;#+acl86win32 (load-system 'clim-standalone)
; uncomment above and comment below to make
; a clim standalone
#+aclpc (clim-defsystem:load-system "aclpc-clim")
#+allegro (load-system 'aclnt-clim)
