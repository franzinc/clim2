;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: load-clim2.lisp,v 1.1 92/08/19 10:25:18 cer Exp $
 
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
      (pkg-kill pkg)))
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
  (sct:compile-system 'clim
		      :query nil :recompile t :reload t
		      :batch #p"sys:clim;rel-2;sys;clim.cwarns")
  (sct:compile-system 'genera-clim :include-components nil
		      :query nil :recompile t :reload t
		      :batch #p"sys:clim;rel-2;sys;genera-clim.cwarns")
  (sct:compile-system 'clx-clim :include-components nil
		      :query nil :recompile t :reload t
		      :batch #p"sys:clim;rel-2;sys;clx-clim.cwarns")
  (sct:compile-system 'postscript-clim :include-components nil
		      :query nil :recompile t :reload t
		      :batch #p"sys:clim;rel-2;sys;postscript-clim.cwarns"))

(defun load-clim2 ()
  (unless (find-package 'clim-defsys)
    (load #p"sys:clim;rel-2;sys;defsystem"))
  (load #p"sys:clim;rel-2;sys;sysdcl")
  (load #p"sys:clim;rel-2;genera;sysdcl")
  (load #p"sys:clim;rel-2;clx;sysdcl")
  (load #p"sys:clim;rel-2;postscript;sysdcl")
  (sct:load-system 'clim
		   :query nil :reload t)
  (sct:load-system 'genera-clim :include-components nil
		   :query nil :reload t)
  (sct:load-system 'clx-clim :include-components nil
		   :query nil :reload t)
  (sct:load-system 'postscript-clim :include-components nil
		   :query nil :reload t))

(defun compile-clim2-demo ()
  (load #p"sys:clim;rel-2;demo;sysdcl")
  (sct:compile-system 'clim-demo
		      :query nil :recompile t :reload t
		      :batch #p"sys:clim;rel-2;sys;clim-demo.cwarns"))

(defun load-clim2-demo ()
  (load #p"sys:clim;rel-2;demo;sysdcl")
  (sct:load-system 'clim-demo
		   :query nil :reload t))

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
