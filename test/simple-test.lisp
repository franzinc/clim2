(in-package :silica)

(setq port (find-port))
(setq graft (find-graft))

(setq sheet (make-instance 'standard-sheet
			   :parent graft
			   :region (make-bounding-rectangle 0 0 300 300)))


(setf (sheet-enabled-p sheet) t)

(setq sheet1 (make-instance 'simple-sheet
			    :parent sheet
			    :region (make-bounding-rectangle 0 0 100 100)
			    :transformation
			    (make-translation-transformation 75 75)))


(setf (sheet-enabled-p sheet1) t)
