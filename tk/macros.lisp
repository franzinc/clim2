(in-package :tk)

(defmacro with-ref-par (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
    (destructuring-bind
     ((var value) &rest more-bindings) bindings
     `(let ((,var (make-array 1 
			      :element-type '(unsigned-byte 32)
			      :initial-contents (list ,value))))
	(with-ref-par ,more-bindings ,@body)))))

