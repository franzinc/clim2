#+(version= 8 2)
(sys:defpatch
    #+mswindows "climnt" #-mswindows "climxm" 2
    "v1: Make CLIM loadable from clinit.cl again;
v2: Mac OS X specific fixes."
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

;; See the file LICENSE for the full license governing this code.
;;

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
