
(in-package "USER")

(defun system::rcsnote (&rest args) (warn "RCSNOTE call with ~a~%" args) nil)

#+(and Allegro microsoft-32)
(eval-when (compile load eval) (push :acl86win32 *features*))

#+acl86win32
(setq comp:trust-dynamic-extent-declarations-switch nil)

;;; LOAD the file PATH.LSP first!

;;; to build a version that doesn't require fixnum coordinates,
;;; turn off the feature: USE-FIXNUM-COORDINATES in the
;;; sys\sysdcl-pc.lisp file.

;;;+++
#+acl86win32
(setq *climpath* "u:\\customer\\franz\\clim22m\\clim2\\")
#+acl86win32
(defun climpath (sub) (merge-pathnames sub *climpath*))
;;;+++

#+acl86win32
(compile-file-if-needed (climpath "sys\\defsystem.lisp"))
#+aclpc
(let* ((defsys-path (climpath "sys\\defsystem.lisp"))
       (defsys-fsl-path (climpath "sys\\defsystem.fsl"))
       (fsl-file-date (if (probe-file defsys-fsl-path)
			  (file-write-date defsys-fsl-path))))
  (unless (and fsl-file-date
	       (>= fsl-file-date (file-write-date defsys-path)))
    (compile-file defsys-path)))

#+aclpc
(load (climpath "sys\\defsystem.fsl"))
#+acl86win32
(load (climpath "sys\\defsystem.fasl"))

(load (climpath "sys\\sysdcl-pc.lisp"))

(let (#+acl86win32
      (excl::*enable-package-locked-errors* nil))

(clim-defsystem:compile-system "clim-utils")
(clim-defsystem:load-system "clim-utils")

(clim-defsystem:compile-system "clim-silica")
(clim-defsystem:load-system "clim-silica")

(clim-defsystem:compile-system "clim-standalone")

(clim-defsystem:load-system "clim-standalone")

(load (climpath "aclpc\\sysdcl.lisp"))

;;; to compile aclpc-clim, uncomment the next line
#+aclpc
(clim-defsystem:compile-system "aclpc-clim")

#+acl86win32
(clim-defsystem:compile-system "aclnt-clim")

#+aclpc
(clim-defsystem:load-system "aclpc-clim")

#+acl86win32
(clim-defsystem:load-system "aclnt-clim")

;;; to make a demo-loaded version, uncomment the following
#+acl86win32
(compile-file-if-needed (climpath "test\\test-suite.lisp"))

#+aclpc (compile-file (climpath "test\\test-suite.lisp"))

#+aclpc
(load (climpath "test\\test-suite.fsl"))

#+acl86win32
(load (climpath "test\\test-suite.fasl"))

(load (climpath "demo\\sysdcl-pc.lisp"))

(clim-defsystem:compile-system "clim-demo")
(clim-defsystem:load-system "clim-demo")

)

;;;+++ why must we do this?  Without this, for some reason,
;;; create-overlapped-window fails on second invocations!

#+acl86win32
(load (climpath "aclpc\\acl-class.fasl"))
#+aclpc
(load (climpath "aclpc\\acl-class.fsl"))

;; postscipt backend 

(load (climpath "postscript\\sysdcl-pc.lisp"))

(clim-defsystem:compile-system "postscript-clim")

(clim-defsystem:load-system "postscript-clim")


