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
;; $fiHeader: xm-classes.lisp,v 1.3 92/03/30 17:51:54 cer Exp $


(provide :climxm)
(require :climg)

(in-package :tk)

;;; This has to kept consistent with the Makefile

(defparameter *motif-classes* '("_applicationShellWidgetClass"
				"_compositeWidgetClass"
				"_constraintWidgetClass"
				"_coreWidgetClass"
				"_objectClass"
				"_overrideShellWidgetClass"
				"_shellWidgetClass"
				"_topLevelShellWidgetClass"
				"_transientShellWidgetClass"
				"_vendorShellWidgetClass"
				"_wmShellWidgetClass"
				
				"_xmArrowButtonGadgetClass"
				"_xmArrowButtonWidgetClass"
				"_xmBulletinBoardWidgetClass"
				"_xmCascadeButtonGadgetClass"
				"_xmCascadeButtonWidgetClass"
				"_xmCommandWidgetClass"
				"_xmDialogShellWidgetClass"
				"_xmDrawingAreaWidgetClass"
				"_xmDrawnButtonWidgetClass"
				"_xmFileSelectionBoxWidgetClass"
				"_xmFormWidgetClass"
				"_xmFrameWidgetClass"
				"_xmGadgetClass"
				"_xmLabelGadgetClass"
				"_xmLabelWidgetClass"
				"_xmListWidgetClass"
				"_xmMainWindowWidgetClass"
				"_xmManagerWidgetClass"
				"_xmMenuShellWidgetClass"
				"_xmMessageBoxWidgetClass"
				"_xmPanedWindowWidgetClass"
				"_xmPrimitiveWidgetClass"
				"_xmPushButtonGadgetClass"
				"_xmPushButtonWidgetClass"
				"_xmRowColumnWidgetClass"
				"_xmScaleWidgetClass"
				"_xmScrollBarWidgetClass"
				"_xmScrolledWindowWidgetClass"
				"_xmSelectionBoxWidgetClass"
				"_xmSeparatorGadgetClass"
				"_xmSeparatorWidgetClass"
				"_xmTextFieldWidgetClass"
				"_xmTextWidgetClass"
				"_xmToggleButtonGadgetClass"
				"_xmToggleButtonWidgetClass"))
