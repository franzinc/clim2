;;; -*- Syntax: Zetalisp; Package: SCT; Base: 10; Mode: LISP; Lowercase: Yes -*-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca.  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, Ca.  All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader$


;;; This distribution script is for the distribution tape for the
;;; release copy of CLIM 25.  It assumes that the system in which
;;; it is to be loaded is at least Genera 8.1

;; The newest legal notice
(define-distribution-system legal-notice
    (:default-pathname "sys:site;")
  (:serial "notice.text"))

(define-distribution-system clim-test-suite
    (:default-pathname "sys:clim;tests;")
  (:serial "test-suite.lisp" "test-suite.bin" "test-suite.ibin"))

(defconst clim-25-release-tape
	  '((legal-notice :newest (:distribute-sources t :distribute-binaries t))
	    ;; Patch Binaries (.BINs and .IBINs) for loadable systems
	    (clim 25      (:distribute-sources nil
			   :distribute-binaries t
			   :included-files-checkpoint :none))
	    (clim-demo 25 (:distribute-sources t
			   :distribute-binaries t
			   :included-files-checkpoint :none))
	    (clim-doc 2   (:distribute-sources t
			   :distribute-binaries t
			   :included-files-checkpoint :none))
	    (clim-test-suite :newest (:distribute-sources t :distribute-binaries t))))

(defmacro collect-systems-list (system-list)
  `(loop for (system version args) in ,system-list
	 collecting (list (find-system-named system) version args)))

;; CP command to write CLIM 25 tape
(define-cp-command (com-write-CLIM-25-tape :name "Write CLIM 25 Tape"
					   :command-table "USER")
    ()
   (dis:write-distribution-on-tape (collect-systems-list clim-25-release-tape)
				   :tape
				   :machine-types :all
				   :distribute-sources nil
				   :distribute-binaries nil
				   :source-category :basic
				   :include-journals t
				   :include-patches t
				   :distribute-patch-sources nil
				   :flatten-files t
				   :compress-files nil
				   :full-length-tapes t
				   :included-files-checkpoint "8.1"
				   :use-cached-checkpoint t
				   :query nil))
