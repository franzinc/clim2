(in-package :user)

#+(and Allegro microsoft-32)
(eval-when (compile load eval) (push :acl86win32 *features*))

#+acl86win32
(let ((excl::*enable-package-locked-errors* nil))
  (defun system::rcsnote (&rest args) (warn "RCSNOTE call with ~a~%" args) nil))

#+acl86win32
(setq comp:trust-dynamic-extent-declarations-switch nil)

#+acl86win32
(defvar *clim-root* (make-pathname 
                       :device (pathname-device *load-pathname*)
                       :directory (butlast (pathname-directory *load-pathname*))))

#+acl86win32 ;; aclpc gets climpath in do.lisp
(defun climpath (sub) (merge-pathnames sub *clim-root*))

#+aclpc
(let* ((defsys-path (climpath "sys\\defsystem.lisp"))
       (defsys-fsl-path (climpath "sys\\defsystem.fsl"))
       (fsl-file-date (if (probe-file defsys-fsl-path)
			  (file-write-date defsys-fsl-path))))
  (unless (and fsl-file-date
	       (>= fsl-file-date (file-write-date defsys-path)))
    (compile-file defsys-path))
  (load (climpath "sys\\defsystem.fsl")))

;; if you turn on this feature be sure to add appropriate forms below
#+acl86win32-uses-clim-defsystem (compile-file-if-needed (climpath "sys\\defsystem.lisp"))
#+acl86win32-uses-clim-defsystem (load (climpath "sys\\defsystem.fasl"))

;; should probably change ANSI-90 to ANSI-CL throughout the CLIM code but
;; until then... (aclpc gets this feature in defsystem)
#+acl86win32 (pushnew :ansi-90 *features*)
#+acl86win32
(setf (logical-pathname-translations "clim2")
  `((";**;*.*" ,*clim-root*)))

#+aclpc (load (climpath "sys\\sysdcl-pc.lisp"))
#+acl86win32 (load (climpath "sys\\sysdcl.lisp"))

(let (#+acl86win32 (excl::*enable-package-locked-errors* nil))

#+aclpc (progn
(clim-defsystem:compile-system "clim-utils")
(clim-defsystem:load-system "clim-utils")
(clim-defsystem:compile-system "clim-silica")
(clim-defsystem:load-system "clim-silica")
(clim-defsystem:compile-system "clim-standalone")
(clim-defsystem:load-system "clim-standalone")
) ;; #+aclpc progn

#+acl86win32
(let ((excl::*update-entry-points* nil))
  (compile-system 'clim-standalone :include-components t))

(load (climpath "aclpc\\sysdcl.lisp"))

#+aclpc (clim-defsystem:compile-system "aclpc-clim")
#+aclpc (clim-defsystem:load-system "aclpc-clim")
#+acl86win32 (compile-system 'aclnt-clim)
#+acl86win32 (load-system 'aclnt-clim)

;;; to make a non-demo-loaded version, comment the following
#+aclpc (compile-file (climpath "test\\test-suite.lisp"))
#+aclpc (load (climpath "test\\test-suite.fsl"))
#+acl86win32 (compile-file-if-needed (climpath "test\\test-suite.lisp"))
#+acl86win32 (load (climpath "test\\test-suite.fasl"))

#+aclpc (load (climpath "demo\\sysdcl-pc.lisp"))
#+acl86win32 (load (climpath "demo\\sysdcl.lisp"))

#+aclpc (clim-defsystem:compile-system "clim-demo")
#+aclpc (clim-defsystem:load-system "clim-demo")
#+acl86win32 (compile-system 'clim-demo)
#+acl86win32 (load-system 'clim-demo)

) ;; end let #+acl86win32 (excl::*enable-package-locked-errors* nil)

;;;+++ why must we do this?  Without this, for some reason,
;;; create-overlapped-window fails on second invocations!

#+aclpc (load (climpath "aclpc\\acl-class.fsl"))
#+acl86win32 (load (climpath "aclpc\\acl-class.fasl"))

;; postscript backend 

#+aclpc (load (climpath "postscript\\sysdcl-pc.lisp"))
#+acl86win32 (load (climpath "postscript\\sysdcl.lisp"))

#+aclpc (clim-defsystem:compile-system "postscript-clim")
#+aclpc (clim-defsystem:load-system "postscript-clim")
#+acl86win32 (compile-system 'postscript-clim)
#+acl86win32 (load-system 'postscript-clim)

(in-package :clim-user)

#+acl86win32x (setq mp::*default-process-quantum* 0.6)

(defun run-tests ()
  (let ((frame (make-application-frame 'clim-tests)))
    (raise-frame frame)
    (run-frame-top-level frame)))
