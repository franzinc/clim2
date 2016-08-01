;; -*- mode: common-lisp; package: tk -*-
;; See the file LICENSE for the full license governing this code.
;;

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
