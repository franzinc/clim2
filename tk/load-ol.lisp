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
;; $fiHeader: load-ol.cl,v 1.3 92/01/17 17:49:11 cer Exp $

(in-package :tk)

#|
make ucl_xtras='/usr/tech/cer/stuff/clim-2.0/ol-classes.o /usr/tech/cer/stuff/clim-2.0/lib/libXol.a /usr/tech/cer/stuff/clim-2.0/lib/libXt.a /usr/tech/cer/stuff/clim-2.0/lib/libX11.a' ucl
|#

(eval-when (compile load eval)
  (defparameter *openlook-classes* '(
				     ;; Base classes
				     "_constraintWidgetClass"
				     "_objectClass"
				     "_wmShellWidgetClass"
				     "_vendorShellWidgetClass"
				     "_coreWidgetClass"
				     "_shellWidgetClass"
				     "_compositeWidgetClass"
				     "_applicationShellWidgetClass"
				     "_overrideShellWidgetClass"
				     "_topLevelShellWidgetClass"
				     "_transientShellWidgetClass"

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
				     )))

(defun load-ol (&optional (what *openlook-classes*))
  (setq what (remove-if #'ff::get-entry-point `(,@what)))
  (when what
    (mapc #'foreign-functions:remove-entry-point 
	  '("__unpack_quadruple" 
	    "__prod_b10000" 
	    "__unpacked_to_decimal"
	    "__carry_out_b10000" 
	    "__prod_65536_b10000"))
    (load ""
	  :unreferenced-lib-names 
	  what
	  :foreign-files 
	  '("/usr/openwin-3.0/lib/libXol.a"
	    "/usr/motif/usr/lib/libXt.a"
	    "/usr/motif/usr/lib/libX11.a"
	    ;; Hopefully
	    ;;"/usr/openwin-3.0/lib/libXt.a"
	    ;; "/usr/openwin-3.0/lib/libX11.a"
	    )
	  :print t)))

(load-ol)
