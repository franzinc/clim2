;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LUCID; Base: 10 -*-
;;;
;;;; Lucid-after-patches, Module CLIM
;;;
;;; ***************************************************************************
;;;
;;;        Copyright (c) 1991, 1992 by Lucid, Inc.  All Rights Reserved
;;;
;;; ***************************************************************************
;;;
;;; Lucid specific hacks which need to be loaded after CLIM gets loaded.
;;;
;;;
;;; Edit-History:
;;;
;;; Created: PW  2-Nov-91
;;;
;;;
;;; End-of-Edit-History


(in-package :lucid)


;;; Workaround for a disksave/hash bug.  
;;;
(defun rehash-ptypes ()
  (dolist (ht (list clim-internals::*presentation-type-description-table*
                    clim-internals::*presentation-type-history-table*
                    clim-internals::*presentation-type-class-table*
                    clim-internals::*presentation-type-inheritance-table*
                    clim-internals::*presentation-type-parameters-table*
                    clim-internals::*presentation-type-options-table*
                    clim-internals::*presentation-type-abbreviation-table*
                    clim-internals::*presentation-generic-function-table*))
    (rehash ht)))

;;; Do this when we load CLIM
(rehash-ptypes)

;;; Add this for disksaving
(when (boundp '*restart-cleanup-functions*)
  (pushnew 'rehash-ptypes *restart-cleanup-functions*))


(eval-when (load)
  (pushnew :clim-2-0 *features*)
  (defparameter *clim-repacking-date* (universal-time-string (get-universal-time)))
  #+ignore                                ;breaks in 2.0
  (unless (member :application *features*)
    (precompile-generic-functions))
  ;;(clos-system:revalidate-all-mki-optimizations)
  )

(lucid::add-patchable-product :clim 20000)
