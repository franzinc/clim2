;; -*- mode: common-lisp; package: user -*-
;;
;;				-[]-
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
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: dump.lisp,v 1.3 92/03/04 16:23:13 cer Exp Locker: cer $

;; Assuming CLIM is loaded, dump it into /usr/tmp/clim.temp_$USER.
(room t)
(sys:resize-areas :global-gc t :old 1000000 :new 500000)
(room t)

(dumplisp :name (concatenate 'simple-string "/tmp/clim.temp_"
			     (sys:getenv "USER"))
	  :checkpoint nil)

