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
