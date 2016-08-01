;; See the file LICENSE for the full license governing this code.
;;

(in-package :user)

#+aclpc (load (climpath "demo\\test-suite.fsl"))
#+acl86win32 (load (climpath "demo\\test-suite.fasl"))

#+aclpc (load (climpath "demo\\sysdcl-pc.lisp"))
#+acl86win32 (load (climpath "demo\\sysdcl.lisp"))

#+aclpc (clim-defsystem:load-system "clim-demo")
#+acl86win32 (clim-defsystem:load-system 'clim-demo)
