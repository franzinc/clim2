#+acl86win32
(eval-when (compile load eval) (push :acl86win32 *features*))

#+acl86win32
(setq comp:trust-dynamic-extent-declarations-switch nil)

;;; LOAD the file PATH.LSP first!

;;; use the following to load clim 2.0

(defun system::rcsnote (&rest args) (warn "RCSNOTE call with ~a~%" args) nil)

;;;+++
#+acl86win32
(setq *climpath* "u:\\customer\\franz\\clim22m\\clim2\\")
#+acl86win32
(defun climpath (sub) (merge-pathnames sub *climpath*))
;;;+++
;;; If loading into a bare ACL for Windows,
;;; uncomment the next four loads
#+aclpc
(load (climpath "sys\\defsystem.fsl"))
#+acl86win32
(load (climpath "sys\\defsystem.fasl"))

(load (climpath "sys\\sysdcl.lisp"))

(load (climpath "aclpc\\sysdcl.lisp"))

(let (#+acl86win32 (excl::*enable-package-locked-errors* nil))

#+aclpc
(clim-defsystem:load-system "aclpc-clim")

#+acl86win32
(clim-defsystem:load-system "aclnt-clim")

;;; to make a demo-loaded version, uncomment the following
#+aclpc
(load (climpath "test\\test-suite.fsl"))
#+acl86win32
(load (climpath "test\\test-suite.fasl"))

(load (climpath "demo\\sysdcl.lisp"))

)

#-acl86win32
(clim-defsystem:load-system "clim-demo")

;;; Remove this feature for the final world.

#+acl86win32
(setq *features* (delete :defsystem *features*))

;;;+++ why must we do this?  Without this, for some reason,
;;; create-overlapped-window fails on second invocations!

#+acl86win32
(load (climpath "aclpc\\acl-class.fasl"))
#+aclpc
(load (climpath "aclpc\\acl-class.fsl"))

;; postscipt backend 

(load (climpath "postscript\\sysdcl.lisp"))

(clim-defsystem:load-system "postscript-clim")


(in-package "CLIM-USER")

#+acl86win32
(setq mp::*default-process-quantum* 0.6)

(defun run-tests ()
  (let ((frame (make-application-frame 'clim-tests)))
    (raise-frame frame)
    (run-frame-top-level frame)))

