;; -*- mode: common-lisp; package: tk -*-
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
;; $Id: ol-classes.lisp,v 2.7 2007/04/17 21:45:53 layer Exp $

(in-package :tk)

(defparameter *openlook-classes* '(
				   ;; Base classes
				   "applicationShellWidgetClass"
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

				   ;; OpenLook specific classes
				   ;; Some of these are both gadgets
				   ;; and widget and I think the
				   ;; names are the same!

				   "abbrevMenuButtonWidgetClass"
				   "abbrevStackWidgetClass"
				   "arrowWidgetClass"
				   "baseWindowShellWidgetClass"
				   "bulletinBoardWidgetClass"
				   "buttonWidgetClass"
				   "buttonGadgetClass"
				   "buttonStackWidgetClass"

				   ;;"buttonStackGadgetClass"

				   "captionWidgetClass"
				   "checkBoxWidgetClass"
				   "controlAreaWidgetClass"
				   "eventObjClass"
				   "exclusivesWidgetClass"
				   "flatCheckBoxWidgetClass"
				   "flatExclusivesWidgetClass"
				   "flatNonexclusivesWidgetClass"
				   "flatWidgetClass"
				   "footerPanelWidgetClass"
				   "formWidgetClass"
				   "gaugeWidgetClass"
				   "helpWidgetClass"
				   "listPaneWidgetClass"
				   "magWidgetClass"
				   "managerWidgetClass"
				   "menuShellWidgetClass"
				   "menuButtonWidgetClass"

				   ;;"menuButtonGadgetClass"

				   "nonexclusivesWidgetClass"
				   "noticeShellWidgetClass"
				   "oblongButtonWidgetClass"
				   ;; "oblongButtonGadgetClass"
				   "popupWindowShellWidgetClass"
				   "primitiveWidgetClass"
				   "pushpinWidgetClass"
				   "rectButtonWidgetClass"
				   "scrollbarWidgetClass"
				   "scrolledWindowWidgetClass"
				   "scrollingListWidgetClass"
				   "sliderWidgetClass"
				   "staticTextWidgetClass"
				   "stubWidgetClass"

				   ;;"textWidgetClass"
				   ;;; This two seem to be broken!
				   ;;"textPaneWidgetClass"

				   ;; Try this again
				   "textEditWidgetClass"
				   "textFieldWidgetClass"

				   "compositeWidgetClass"
				   "overrideShellWidgetClass"
				   "shellWidgetClass"
				   "topLevelShellWidgetClass"
				   "widgetClass"
				   "widgetClassRec"
				   "drawAreaWidgetClass"
				   ))
