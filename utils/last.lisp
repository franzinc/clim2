;; See the file LICENSE for the full license governing this code.
;;

;;; All this is allegro-sepcific.

(in-package :system)

;;; This is, perhaps, a temporary hack to get the EUC stuff loaded at
;;; a non-bad time.
;;
;; This is no longer needed, since we're using native-to-* interface now
;; The euc module may be autoloaded due to process-code usage, but the
;; autoload no longer produces a mesage at inopportune times.
#+ignore
(eval-when (:load-toplevel :execute)
  (require :euc)
  (find-external-format :euc))

(provide
 #+mswindows :climnt
 #-mswindows
 (cond ((excl::featurep :clim-motif) :climxm)
       ((excl::featurep :clim-openlook) :climol)
       (t (error "Unknown Xt backend"))))
