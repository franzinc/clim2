
#+aclpc
(load (climpath "demo\\test-suite.fsl"))
#+allegro
(load (climpath "demo\\test-suite.fasl"))

(load (climpath "demo\\sysdcl-pc.lisp"))

(clim-defsystem:load-system "clim-demo")
