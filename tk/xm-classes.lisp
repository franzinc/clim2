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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Header: /repo/cvs.copy/clim2/tk/xm-classes.lisp,v 1.11 1997/02/05 01:53:16 tomj Exp $


(in-package :tk)

;;; This has to kept consistent with the Makefile

(defparameter *motif-classes* '("applicationShellWidgetClass"
				"compositeWidgetClass"
				"constraintWidgetClass"
				"coreWidgetClass"
				"objectClass"
				"overrideShellWidgetClass"
				"shellWidgetClass"
				"topLevelShellWidgetClass"
				"transientShellWidgetClass"
				"vendorShellWidgetClass"
				"wmShellWidgetClass"

				"xmArrowButtonGadgetClass"
				"xmArrowButtonWidgetClass"
				"xmBulletinBoardWidgetClass"
				"xmCascadeButtonGadgetClass"
				"xmCascadeButtonWidgetClass"
				"xmCommandWidgetClass"
				"xmDialogShellWidgetClass"
				"xmDrawingAreaWidgetClass"
				"xmDrawnButtonWidgetClass"
				"xmFileSelectionBoxWidgetClass"
				"xmFormWidgetClass"
				"xmFrameWidgetClass"
				"xmGadgetClass"
				"xmLabelGadgetClass"
				"xmLabelWidgetClass"
				"xmListWidgetClass"
				"xmMainWindowWidgetClass"
				"xmManagerWidgetClass"
				"xmMenuShellWidgetClass"
				"xmMessageBoxWidgetClass"
				"xmPanedWindowWidgetClass"
				"xmPrimitiveWidgetClass"
				"xmPushButtonGadgetClass"
				"xmPushButtonWidgetClass"
				"xmRowColumnWidgetClass"
				"xmScaleWidgetClass"
				"xmScrollBarWidgetClass"
				"xmScrolledWindowWidgetClass"
				"xmSelectionBoxWidgetClass"
				"xmSeparatorGadgetClass"
				"xmSeparatorWidgetClass"
				"xmTextFieldWidgetClass"
				"xmTextWidgetClass"
				"xmToggleButtonGadgetClass"
				"xmToggleButtonWidgetClass"
				"xmMyDrawingAreaWidgetClass"
				))
