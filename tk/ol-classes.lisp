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
;; $fiHeader: ol-classes.lisp,v 1.4 92/03/30 17:51:46 cer Exp $

(provide :climol)
(require :climg)

(in-package :tk)

(defparameter *openlook-classes* '(
				   ;; Base classes
				   "_applicationShellWidgetClass"
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

				   ;; OpenLook specific classes
				   ;; Some of these are both gadgets
				   ;; and widget and I think the
				   ;; names are the same!
				     
				   "_abbrevMenuButtonWidgetClass"
				   "_abbrevStackWidgetClass"
				   "_arrowWidgetClass"
				   "_baseWindowShellWidgetClass"
				   "_bulletinBoardWidgetClass"
				   "_buttonWidgetClass"
				   "_buttonGadgetClass"
				   "_buttonStackWidgetClass"

				   ;;"_buttonStackGadgetClass"

				   "_captionWidgetClass"
				   "_checkBoxWidgetClass"
				   "_controlAreaWidgetClass"
				   "_eventObjClass"
				   "_exclusivesWidgetClass"
				   "_flatCheckBoxWidgetClass"
				   "_flatExclusivesWidgetClass"
				   "_flatNonexclusivesWidgetClass"
				   "_flatWidgetClass"
				   "_footerPanelWidgetClass"
				   "_formWidgetClass"
				   "_helpWidgetClass"
				   "_listPaneWidgetClass"
				   "_magWidgetClass"
				   "_managerWidgetClass"
				   "_menuShellWidgetClass"
				   "_menuButtonWidgetClass"

				   ;;"_menuButtonGadgetClass"

				   "_nonexclusivesWidgetClass"
				   "_noticeShellWidgetClass"
				   "_oblongButtonWidgetClass"
				   ;; "_oblongButtonGadgetClass"
				   "_popupWindowShellWidgetClass"
				   "_primitiveWidgetClass"
				   "_pushpinWidgetClass"
				   "_rectButtonWidgetClass"
				   "_scrollbarWidgetClass"
				   "_scrolledWindowWidgetClass"
				   "_scrollingListWidgetClass"
				   "_sliderWidgetClass"
				   "_staticTextWidgetClass"
				   "_stubWidgetClass"
				   "_textWidgetClass"
				   
				   ;;; This two seem to be broken!
				   ;; "_textEditWidgetClass"
				   ;;"_textPaneWidgetClass"
				   
				   "_compositeWidgetClass"
				   "_overrideShellWidgetClass"
				   "_shellWidgetClass"
				   "_topLevelShellWidgetClass"
				   "_widgetClass"
				   "_widgetClassRec"
				   "_drawAreaWidgetClass"
				   ))
