;; -*- mode: common-lisp; package: user -*-
;; See the file LICENSE for the full license governing this code.
;;
;; $Id: demo-last.lisp,v 2.7 2007/04/17 21:45:51 layer Exp $

(in-package :user)

(provide :climdemo)

(cond ((excl::featurep :clim-motif)
       (provide :climdemoxm)
       (load "clim2demoxm-preload.fasl" :if-does-not-exist nil))
      ((excl::featurep :clim-openlook)
       (provide :climdemool)
       (load "clim2demool-preload.fasl" :if-does-not-exist nil)))
