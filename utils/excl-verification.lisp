#+(version= 8 2)
(sys:defpatch
    #+mswindows "climnt" #-mswindows "climxm" 1
    "v1: Make CLIM loadable from clinit.cl again."
    :type :system
    :post-loadable t)

#+(version= 8 1)
(sys:defpatch
    #+mswindows "climnt" #-mswindows "climxm" 5
    "v1: full international font support;
v2: fix v1 for 8-bit Lisps;
v3: styled text drawing in 8-bit images, switching table orientation
    when redisplaying;  
v4: drawing rotated text in i18n, accepting pathnames containing delimiter
    chars, completion of logical pathnames;
v5: Fix postscript, GC cursor, Windows text field bugs, speed up
    internationalized text drawing on Motif."
  :type :system
  :post-loadable t)

;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2007 Franz Inc, Oakland, CA - All rights reserved.
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
;; $Id: excl-verification.lisp,v 2.12 2008/07/22 16:51:19 layer Exp $

(in-package :sys)

#+ignore
(eval-when (compile)
  (assert (member excl::*current-case-mode*
		  '(:case-insensitive-lower :case-insensitive-upper))))

(eval-when (compile load eval)
  (pushnew :clim *features*)
  (pushnew :clim-2 *features*)
  (pushnew :clim-2.1 *features*)
  (pushnew :silica *features*)
  (pushnew :ansi-90 *features*))

(provide :clim)
(provide :climg)
