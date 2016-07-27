;; See the file LICENSE for the full license governing this code.
;;
;; $Id: xt-classes.lisp,v 2.7 2007/04/17 21:45:53 layer Exp $

(in-package :tk)

;;; This has to kept consistent with the Makefile

(defparameter *intrinsic-classes* '(
				    "constraintWidgetClass"
				    "objectClass"
				    "wmShellWidgetClass"
				    "vendorShellWidgetClass"
				    "coreWidgetClass"
				    "shellWidgetClass"
				    "compositeWidgetClass"
				    "applicationShellWidgetClass"
				    "overrideShellWidgetClass"
				    "topLevelShellWidgetClass"
				    "transientShellWidgetClass"))
