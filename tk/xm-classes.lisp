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
;; $Id: xm-classes.lisp,v 2.7 2007/04/17 21:45:53 layer Exp $

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
