;; -*- mode: common-lisp; package: user -*-
;;
;;				-[Mon Jul  6 15:36:40 1998 by layer]-
;;
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, CA  All rights reserved.
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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: dump.lisp,v 1.11 1998/08/06 23:16:43 layer Exp $

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
