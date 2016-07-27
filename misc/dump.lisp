;; -*- mode: common-lisp; package: user -*-
;;
;;
;; See the file LICENSE for the full license governing this code.
;;
;; $Id: dump.lisp,v 2.7 2007/04/17 21:45:51 layer Exp $

;; Assuming CLIM is loaded, dump it into /usr/tmp/clim.temp_$USER.
(room t)
(sys:resize-areas :global-gc t :old 1000000 :new 500000)
(room t)

#+ignore (setq tpl::*user-top-level* nil) ;; workaround for bug3225

(gc :tenure)
(gc :tenure)
(gc :tenure)
(gc t)

(dumplisp :name sys::*clim-dump-name* :shlib-warning nil)
