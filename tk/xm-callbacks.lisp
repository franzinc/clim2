;; See the file LICENSE for the full license governing this code.
;;
;; $Id: xm-callbacks.lisp,v 2.7 2007/04/17 21:45:53 layer Exp $

(in-package :tk)

(defmethod spread-callback-data (widget call-data (type (eql :single-selection)))
  (declare (ignore widget))
  (xm-list-callback-struct-item-position call-data))

(defmethod spread-callback-data (widget call-data (type (eql :multiple-selection)))
  (declare (ignore widget))
  (let ((si (xm-list-callback-struct-selected-item-positions call-data))
	(r nil))
    (dotimes (i (xm-list-callback-struct-selected-item-count call-data) (nreverse r))
      (push (xm-selected-position-array si i) r))))



