;; -*- mode: common-lisp; package: tk -*-
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
;; $fiHeader: load-xm.cl,v 1.2 92/01/02 15:08:49 cer Exp Locker: cer $

(in-package :tk)

#|
make ucl_xtras='/usr/tech/cer/stuff/clim-2.0/xm-classes.o /usr/motif/usr/lib/libXm.a /usr/motif/usr/lib/libXt.a /usr/motif/usr/lib/libX11.a' ucl
|#

;;; This has to kept consistent with the Makefile

(eval-when (compile load eval)
  (defparameter *motif-classes* '(
				  "_constraintWidgetClass"
				 "_objectClass"
				 "_xmGadgetClass"
				 "_wmShellWidgetClass"
				 "_vendorShellWidgetClass"
				     "_coreWidgetClass"
				     "_shellWidgetClass"
				     "_compositeWidgetClass"
				     "_xmPrimitiveWidgetClass"
				     "_xmManagerWidgetClass"
				     "_applicationShellWidgetClass"
				     "_xmArrowButtonWidgetClass"
				     "_xmArrowButtonGadgetClass"
				     "_xmBulletinBoardWidgetClass"
				     "_xmCascadeButtonWidgetClass"
				     "_xmCascadeButtonGadgetClass"
				     "_xmCommandWidgetClass"
				     "_xmDialogShellWidgetClass"
				     "_xmDrawingAreaWidgetClass"
				     "_xmDrawnButtonWidgetClass"
				     "_xmFileSelectionBoxWidgetClass"
				     "_xmFormWidgetClass"
				     "_xmFrameWidgetClass"
				     "_xmLabelWidgetClass"
				     "_xmLabelGadgetClass"
				     "_xmListWidgetClass"
				     "_xmMainWindowWidgetClass"
				     "_xmMenuShellWidgetClass"
				     "_xmMessageBoxWidgetClass"
				     "_overrideShellWidgetClass"
				     "_xmPanedWindowWidgetClass"
				     "_xmPushButtonWidgetClass"
				     "_xmPushButtonGadgetClass"
				     "_xmRowColumnWidgetClass"
				     "_xmScaleWidgetClass"
				     "_xmScrollBarWidgetClass"
				     "_xmScrolledWindowWidgetClass"
				     "_xmSelectionBoxWidgetClass"
				     "_xmSeparatorWidgetClass"
				     "_xmSeparatorGadgetClass"
				     "_xmTextWidgetClass"
				     "_xmTextFieldWidgetClass"
				     "_xmToggleButtonWidgetClass"
				     "_xmToggleButtonGadgetClass"
				     "_topLevelShellWidgetClass"
				     "_transientShellWidgetClass"
				     )))

(defun load-from-xm (&optional (what *motif-classes*))
  (setq what (remove-if #'ff::get-entry-point
			`(
			  "_XCopyGC"
			  ,@what
			  )))
  (when what
    #+ignore
    (mapc #'foreign-functions:remove-entry-point 
	  '("__unpack_quadruple" 
	    "__prod_b10000" 
	    "__carry_out_b10000" 
	    "__prod_65536_b10000"))
    (load "" 
	  :unreferenced-lib-names what
	  :foreign-files 
	  '("/usr/motif/usr/lib/libXm.a"
	    "/usr/motif/usr/lib/libXt.a"
	    "/usr/motif/usr/lib/libX11.a") 
	  :print t)))

(load-from-xm)

