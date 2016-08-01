;; See the file LICENSE for the full license governing this code.
;;

(in-package :tk)

(defvar *xm-done* nil)

(unless *xm-done*
  (xt-initialize)
  (define-toolkit-classes
      *intrinsic-classes*
      *motif-classes*)
  (setq *xm-done* t))


;; We only want to reinitialize if the toolkit was linked in shared.
;; Unfortunately there doesn't appear to be anyway of reliably
;; finding this out. Experience would indicate that the default is
;; to always link in the shared libraries except on rs6k (cim 3/14/96)


#+(version>= 5 0)
(defparameter *toolkit-static*
    #+rs6000 t
    #-rs6000 nil)


#+(and (not (version>= 5 0)) dlfcn)
(defparameter *toolkit-static*
    #+rs6000 t
    #-rs6000 nil)

#+(version>= 5 0)
(defun reinitialize-toolkit ()
  (unless *toolkit-static*

    (setq *callback-handler-address*
      (register-foreign-callable 'callback-handler :reuse))
    (setq *match-event-sequence-and-types-address*
      (register-foreign-callable 'match-event-sequence-and-types-using-structure :reuse))
    (setq *event-handler-address*
      (register-foreign-callable 'event-handler :reuse))
    (setq *error-handler-function-address*
      (register-foreign-callable 'toolkit-error-handler :reuse))
    (setq *warning-handler-function-address*
      (register-foreign-callable 'toolkit-warning-handler :reuse))
    (setq *x-error-handler-address*
      (register-foreign-callable 'x-error-handler :reuse))
    (setq *x-io-error-handler-address*
      (register-foreign-callable 'x-io-error-handler :reuse))
    (reinitialize-silica-callbacks)

    (xt_toolkit_initialize)
    (setup-error-handlers)
    (fixup-class-entry-points)))

#+(and (not (version>= 5 0)) dlfcn)
(defun reinitialize-toolkit ()
  (unless *toolkit-static*
    (xt_toolkit_initialize)
    (setup-error-handlers)
    (fixup-class-entry-points)))

#+(version>= 5 0)
(push 'reinitialize-toolkit excl::*restart-actions*)

#+(and (not (version>= 5 0)) dlfcn)
(push 'reinitialize-toolkit excl::*restart-actions*)

