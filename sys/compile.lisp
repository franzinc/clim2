(in-package :user)

#+allegro
(eval-when (compile eval load)
  (when (eq :case-sensitive-lower excl:*current-case-mode*)
    (setq *print-case* :downcase)
    (set-case-mode :case-insensitive-lower)))

#+(and Allegro (or microsoft-32 mswindows))
(eval-when (compile load eval) (push :acl86win32 *features*))

#+acl86win32
(let ((excl::*enable-package-locked-errors* nil))
  (when (not (fboundp 'system::rcsnote))
    (defun system::rcsnote (&rest args)
      nil)))

#+acl86win32
(setq comp:trust-dynamic-extent-declarations-switch nil)

#+acl86win32
(defvar *clim-root* (make-pathname 
                       :device (pathname-device *load-pathname*)
                       :directory (butlast (pathname-directory *load-pathname*))))

#+acl86win32 ;; aclpc gets climpath in do.lisp
(defun climpath (sub) (merge-pathnames sub *clim-root*))

;; if you turn on this feature be sure to add appropriate forms below
#+acl86win32-uses-clim-defsystem (compile-file-if-needed (climpath "sys\\defsystem.lisp"))
#+acl86win32-uses-clim-defsystem (load (climpath "sys\\defsystem.fasl"))

;; should probably change ANSI-90 to ANSI-CL throughout the CLIM code but
;; until then... (aclpc gets this feature in defsystem)
#+acl86win32 (pushnew :ansi-90 *features*)
#+acl86win32
(setf (logical-pathname-translations "clim2")
  `((";**;*.*" ,*clim-root*)))

#+acl86win32 (load (climpath "sys\\sysdcl.lisp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-compilation-unit ()
(let ((excl::*enable-package-locked-errors* nil))

(load (climpath "aclpc\\sysdcl.lisp"))

(let ((excl::*update-entry-points* nil))
  (compile-system 'aclnt-clim :include-components t)
  (let ((excl:*redefinition-warnings* nil)
	(excl::*defconstant-redefinition-check* nil))
    (load-system 'aclnt-clim)))

;;;+++ why must we do this?  Without this, for some reason,
;;; create-overlapped-window fails on second invocations!
(load (climpath "aclpc\\acl-class.fasl"))

;; postscript backend 
(load (climpath "postscript\\sysdcl.lisp"))
(compile-system 'postscript-clim)
(let ((excl:*redefinition-warnings* nil)
      (excl::*defconstant-redefinition-check* nil))
  (load-system 'postscript-clim))

;;; to make a non-demo-loaded version, comment the following
(compile-file-if-needed (climpath "test\\test-suite.lisp"))
(load (climpath "test\\test-suite.fasl"))

(let ((excl:*redefinition-warnings* nil)
;;;      (*record-source-file-info* nil) ;; too many damn warnings
;;;      (*load-source-file-info* nil)
      )
  (load (climpath "demo\\sysdcl.lisp"))
  (compile-system 'clim-demo))

) ;; end let #+acl86win32 (excl::*enable-package-locked-errors* nil)

) ;; with-compilation-unit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :clim-user)

#+acl86win32x (setq mp::*default-process-quantum* 0.6)

(defun run-tests ()
  (let ((frame (make-application-frame 'clim-tests)))
    (raise-frame frame)
    (run-frame-top-level frame)))

#+acl86win32
(flet ((append-files (input-files output-file)
	 (format t "~%;; Making ~a...~%" output-file)
	 (with-open-file (s output-file
			  :element-type '(unsigned-byte 8)
			  :direction :output :if-exists :supersede
			  :if-does-not-exist :create)
	   (dolist (input-file input-files)
	     (sys:copy-file input-file s)))
	 t))
  (append-files '("aclpc/pkgdcl.fasl"
		  "aclpc/winwidgh.fasl"
		  "aclpc/climpat.fasl"
		  "aclpc/acl-prel.fasl"
		  "aclpc/acl-class.fasl"
		  "aclpc/acl-dc.fasl"
		  "aclpc/acl-port.fasl"
		  "aclpc/acl-mirror.fasl"
		  "aclpc/acl-medium.fasl"
		  "aclpc/acl-pixmaps.fasl"
		  "aclpc/acl-frames.fasl"
		  "aclpc/acl-widget.fasl"
		  "aclpc/acl-scroll.fasl"
		  "utils/last.fasl")
		"climnt.fasl")
  (append-files '("postscript/postscript-port.fasl"
		  "postscript/postscript-medium.fasl"
		  "postscript/laserwriter-metrics.fasl")
		"climps.fasl")
  (append-files '("demo/packages.fasl"
		  "demo/demo-driver.fasl"
		  "demo/listener.fasl"
		  "demo/graphics-demos.fasl"
		  "demo/cad-demo.fasl"
		  "demo/navdata.fasl"
		  "demo/navfun.fasl"
		  "demo/puzzle.fasl"
		  "demo/address-book.fasl"
		  "demo/thinkadot.fasl"
		  "demo/plot.fasl"
		  "demo/color-editor.fasl"
		  "demo/graphics-editor.fasl"
		  "demo/ico.fasl"
		  "demo/browser.fasl"
		  "demo/peek-frame.fasl"
		  "demo/process-browser.fasl"
		  "demo/custom-records.fasl"
		  "demo/demo-activity.fasl"
		  "test/test-suite.fasl"
		  "demo/demo-last.fasl")
		"climdemo.fasl")
  (append-files '("utils/excl-verification.fasl"
		  "utils/packages.fasl"
		  "utils/defun-utilities.fasl"
		  "utils/reader.fasl"
		  "utils/clos-patches.fasl"
		  "utils/clos.fasl"
		  "utils/utilities.fasl"
		  "utils/lisp-utilities.fasl"
		  "utils/processes.fasl"
		  "utils/queue.fasl"
		  "utils/timers.fasl"
		  "utils/protocols.fasl"
		  "utils/clim-streams.fasl"
		  "utils/excl-streams.fasl"
		  "utils/clim-macros.fasl"
		  "utils/transformations.fasl"
		  "utils/regions.fasl"
		  "utils/region-arithmetic.fasl"
		  "utils/extended-regions.fasl"
		  "utils/base-designs.fasl"
		  "utils/designs.fasl"
		  "silica/classes.fasl"
		  "silica/text-style.fasl"
		  "silica/macros.fasl"
		  "silica/sheet.fasl"
		  "silica/mirror.fasl"
		  "silica/event.fasl"
		  "silica/port.fasl"
		  "silica/medium.fasl"
		  "silica/framem.fasl"
		  "silica/graphics.fasl"
		  "silica/pixmaps.fasl"
		  "silica/std-sheet.fasl"
		  "silica/layout.fasl"
		  "silica/db-layout.fasl"
		  "silica/db-box.fasl"
		  "silica/db-table.fasl"
		  "silica/gadgets.fasl"
		  "silica/db-scroll.fasl"
		  "silica/db-border.fasl"
		  "silica/db-button.fasl"
		  "silica/db-slider.fasl"
		  "silica/db-label.fasl"
		  "clim/gestures.fasl"
		  "clim/defprotocol.fasl"
		  "clim/stream-defprotocols.fasl"
		  "clim/defresource.fasl"
		  "clim/temp-strings.fasl"
		  "clim/clim-defs.fasl"
		  "clim/stream-class-defs.fasl"
		  "clim/interactive-defs.fasl"
		  "clim/cursor.fasl"
		  "clim/view-defs.fasl"
		  "clim/input-defs.fasl"
		  "clim/input-protocol.fasl"
		  "clim/output-protocol.fasl"
		  "clim/recording-protocol.fasl"
		  "clim/recording-defs.fasl"
		  "clim/text-recording.fasl"
		  "clim/graphics-recording.fasl"
		  "clim/design-recording.fasl"
		  "clim/interactive-protocol.fasl"
		  "clim/input-editor-commands.fasl"
		  "clim/formatted-output-defs.fasl"
		  "clim/incremental-redisplay.fasl"
		  "clim/coordinate-sorted-set.fasl"
		  "clim/r-tree.fasl"
		  "clim/window-stream.fasl"
		  "clim/pixmap-streams.fasl"
		  "clim/ptypes1.fasl"
		  "clim/completer.fasl"
		  "clim/presentations.fasl"
		  "clim/translators.fasl"
		  "clim/histories.fasl"
		  "clim/ptypes2.fasl"
		  "clim/excl-presentations.fasl"
		  "clim/standard-types.fasl"
		  "clim/table-formatting.fasl"
		  "clim/graph-formatting.fasl"
		  "clim/surround-output.fasl"
		  "clim/text-formatting.fasl"
		  "clim/tracking-pointer.fasl"
		  "clim/dragging-output.fasl"
		  "clim/db-stream.fasl"
		  "clim/gadget-output.fasl"
		  "clim/accept.fasl"
		  "clim/present.fasl"
		  "clim/command.fasl"
		  "clim/command-processor.fasl"
		  "clim/basic-translators.fasl"
		  "clim/frames.fasl"
		  "clim/panes.fasl"
		  "clim/default-frame.fasl"
		  "clim/activities.fasl"
		  "clim/db-menu.fasl"
		  "clim/db-list.fasl"
		  "clim/db-text.fasl"
		  "clim/noting-progress.fasl"
		  "clim/menus.fasl"
		  "clim/accept-values.fasl"
		  "clim/drag-and-drop.fasl"
		  "clim/item-list-manager.fasl"
		  "postscript/pkgdcl.fasl"
		  "clim/stream-trampolines.fasl")
		"climg.fasl"))
