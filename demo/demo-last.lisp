;; -*- mode: common-lisp; package: user -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :user)

(provide :climdemo)

(cond ((excl::featurep :clim-motif)
       (provide :climdemoxm)
       (load "clim2demoxm-preload.fasl" :if-does-not-exist nil))
      ((excl::featurep :clim-openlook)
       (provide :climdemool)
       (load "clim2demool-preload.fasl" :if-does-not-exist nil)))
