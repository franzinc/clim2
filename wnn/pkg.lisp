;; -*- mode: common-lisp; package: cl-user -*-
;;
;; 
;; See the file LICENSE for the full license governing this code.
;;
;; $Id: pkg.lisp,v 2.7 2007/04/17 21:45:54 layer Exp $

(defpackage :wnn
  (:use :clim-lisp :clim-utils :clim :silica :ff)
  (:import-from :excl #:if*)
  (:export
   #:jserver
   #:jserver-login
   #:jserver-host
   #:jserver-lang
   #:*jserver-timeout*
   #:open-jserver
   #:bunsetu-suu
   #:get-kanji
   #:get-yomi
   #:*wnn-unique*
   #:select-bunsetu
   #:bunsetu-kouho-suu
   #:get-kouho-kanji
   #:get-zenkouho-kanji
   #:henkan-begin
   #:henkan-end
   ))

(setf (package-definition-lock (find-package :wnn)) t)

(in-package :wnn)
