;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: load-clim2.lisp,v 1.2 92/09/08 15:18:41 cer Exp $
 
(defun flush-clim ()
  (when (find-package 'clim-defsys)
    (set (intern "*SYSTEMS*" 'clim-defsys) nil))
  (dolist (pkg '(clim-lisp clim-utils clim clim-user clim-defsys defsys
		 clim-demo clim-graphics-editor clim-browser
		 ;; CLIM 2.0
		 clim-sys clim-silica clim-internals clx-clim genera-clim
		 ;; CLIM 0.9
		 clim-conditions clim-shared clim-stream
		 silica windshield on-x on-genera))
    (when (find-package pkg)
      (delete-package pkg)))
  (dolist (sys '(clim-utils clim-standalone clim clim-demo
		 ;; CLIM 2.0
		 clim-internals clim-silica genera-clim clx-clim postscript-clim
		 ;; CLIM 0.9
		 silica silica-low silica-core ws ws-core ws-panes
		 silica-x silica-genera silica-coral-2 silica-postscript
		 clim-stream ws-demo clim-test ptk))
    (sct:undefsystem sys)))

(defun rename-clim1 ()
  (loop for (old new) in '((clim-lisp clim1-lisp)
			   (clim-utils clim1-utils)
			   (clim clim1)
			   (clim-user clim1-user)
			   (clim-demo clim1-demo)
			   (defsystem clim1-defsystem))
	do (rename-package old new)))

(defun compile-clim2 ()
  (compile-file #p"sys:clim;rel-2;sys;defsystem.lisp")
  (load #p"sys:clim;rel-2;sys;defsystem")
  (load #p"sys:clim;rel-2;sys;sysdcl")
  (load #p"sys:clim;rel-2;genera;sysdcl")
  (load #p"sys:clim;rel-2;clx;sysdcl")
  (load #p"sys:clim;rel-2;postscript;sysdcl")
  (let ((other-keys '(:query nil :recompile t :reload t)))
    (apply #'sct:compile-system 'clim
				:batch #p"sys:clim;rel-2;sys;clim.cwarns"
				other-keys)
    (apply #'sct:compile-system 'genera-clim :include-components nil
				:batch #p"sys:clim;rel-2;sys;genera-clim.cwarns"
				other-keys)
    (apply #'sct:compile-system 'clx-clim :include-components nil
				:batch #p"sys:clim;rel-2;sys;clx-clim.cwarns"
				other-keys)
    (apply #'sct:compile-system 'postscript-clim :include-components nil
				:batch #p"sys:clim;rel-2;sys;postscript-clim.cwarns"
				other-keys)))

(defun load-clim2 ()
  (unless (find-package 'clim-defsys)
    (load #p"sys:clim;rel-2;sys;defsystem"))
  (load #p"sys:clim;rel-2;sys;sysdcl")
  (load #p"sys:clim;rel-2;genera;sysdcl")
  (load #p"sys:clim;rel-2;clx;sysdcl")
  (load #p"sys:clim;rel-2;postscript;sysdcl")
  (let ((other-keys '(:query nil :reload t)))
    (apply #'sct:load-system 'clim
			     other-keys)
    (apply #'sct:load-system 'genera-clim :include-components nil
			     other-keys)
    (apply #'sct:load-system 'clx-clim :include-components nil
			     other-keys)
    (apply #'sct:load-system 'postscript-clim :include-components nil
			     other-keys)))

(defun compile-clim2-demo ()
  (load #p"sys:clim;rel-2;demo;sysdcl")
  (let ((other-keys '(:query nil :recompile t :reload t)))
    (apply #'sct:compile-system 'clim-demo
				:batch #p"sys:clim;rel-2;sys;clim-demo.cwarns"
				other-keys)))

(defun load-clim2-demo ()
  (load #p"sys:clim;rel-2;demo;sysdcl")
  (let ((other-keys '(:query nil :reload t)))
    (apply #'sct:load-system 'clim-demo
			     other-keys)))

(defun compile-clim2-tests ()
  (compile-file #p"sys:clim;rel-2;test;test-suite.lisp")
  (compile-file #p"sys:clim;rel-2;test;test.lisp")
  (compile-file #p"sys:clim;rel-2;test;test-sliders.lisp")
  (compile-file #p"sys:clim;rel-2;test;test-buttons.lisp"))

(defun load-clim2-tests ()
  (load #p"sys:clim;rel-2;test;test-suite")
  (load #p"sys:clim;rel-2;test;test")
  (load #p"sys:clim;rel-2;test;test-sliders")
  (load #p"sys:clim;rel-2;test;test-buttons"))
