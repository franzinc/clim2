;; See the file LICENSE for the full license governing this code.
;;

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



