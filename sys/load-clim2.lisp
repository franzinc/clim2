;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-1998 Franz Inc, Berkeley, CA  - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: load-clim2.lisp,v 1.7 1998/08/06 23:17:06 layer Exp $
 
(defun flush-clim ()
  (when (find-package 'clim-defsys)
    (set (intern "*SYSTEMS*" 'clim-defsys) nil))
  (dolist (pkg '(clim-demo clim-graphics-editor clim-browser
		 clim-user clim clim-utils clim-lisp clim-defsys defsys
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
    (sct:undefsystem sys))
  (setq *features* (delete :clim   *features*))
  (setq *features* (delete :clim-1 *features*))
  (setq *features* (delete :clim-2 *features*))
  t)

(defun rename-clim1 ()
  (loop for (old new) in '((clim-lisp clim1-lisp)
			   (clim-utils clim1-utils)
			   (clim clim1)
			   (clim-user clim1-user)
			   (clim-demo clim1-demo)
			   (defsystem clim1-defsystem))
	do (rename-package old new))
  t)

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
			     other-keys))
  t)

(defun compile-clim2-demo ()
  (load #p"sys:clim;rel-2;demo;sysdcl")
  (let ((other-keys '(:query nil :recompile t :reload t)))
    (apply #'sct:compile-system 'clim-demo
				:batch #p"sys:clim;rel-2;sys;clim-demo.cwarns"
				other-keys))
  t)

(defun load-clim2-demo ()
  (load #p"sys:clim;rel-2;demo;sysdcl")
  (let ((other-keys '(:query nil :reload t)))
    (apply #'sct:load-system 'clim-demo
			     other-keys))
  t)

(defun compile-clim2-tests ()
  (compile-file #p"sys:clim;rel-2;test;test-suite.lisp")
  (compile-file #p"sys:clim;rel-2;test;test.lisp")
  (compile-file #p"sys:clim;rel-2;test;test-sliders.lisp")
  (compile-file #p"sys:clim;rel-2;test;test-buttons.lisp")
  t)

(defun load-clim2-tests (&optional load-all)
  (load #p"sys:clim;rel-2;test;test-suite")
  (when load-all
    (load #p"sys:clim;rel-2;test;test")
    (load #p"sys:clim;rel-2;test;test-sliders")
    (load #p"sys:clim;rel-2;test;test-buttons"))
  t)
