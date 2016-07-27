;; -*- mode: common-lisp; package: system -*-
;;
;;
;; See the file LICENSE for the full license governing this code.
;;

;; $Id: instclimxm.lisp,v 2.7 2007/04/17 21:45:48 layer Exp $

;;
;; Load the Motif version of CLIM
;;

(in-package :system)

(load-application (require :climxm) :devel system::*devel*)

#+ics
(load-application (require :climwnn) :devel system::*devel*)

(format t "~&; Finished loading CLIM XM~%")
(force-output)
