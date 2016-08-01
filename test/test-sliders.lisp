;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-USER; Base: 10; Lowercase: Yes -*-


(in-package :clim-user)

(define-application-frame slider-test-frame () (radio-box)
  (:command-table test-frame)
  (:pane
    (horizontally ()
      (vertically ()
	(outlining ()
	  (spacing ()
	    (make-pane 'slider
		       :value-changed-callback 'slider-changed-callback
		       :drag-callback 'slider-dragged-callback)))
	(outlining ()
	  (spacing ()
	    (make-pane 'slider
		       :label "Slider"
		       :value-changed-callback 'slider-changed-callback
		       :drag-callback 'slider-dragged-callback)))
	(outlining ()
	  (spacing ()
	    (make-pane 'slider
		       :label "with 5 Tick Marks"
		       :number-of-tick-marks 5
		       :value-changed-callback 'slider-changed-callback
		       :drag-callback 'slider-dragged-callback)))
	(outlining ()
	  (spacing ()
	    (make-pane 'slider
		       :label "with 20 Tick Marks"
		       :number-of-tick-marks 20
		       :value-changed-callback 'slider-changed-callback
		       :drag-callback 'slider-dragged-callback)))
	(outlining ()
	  (spacing ()
	    (make-pane 'slider
		       :label "with Ticks + Range Labels"
		       :number-of-tick-marks 20
		       :min-label "0"
		       :max-label "20"
		       :value-changed-callback 'slider-changed-callback
		       :drag-callback 'slider-dragged-callback)))
	(outlining ()
	  (spacing ()
	    (make-pane 'slider
		       :label "with Range Labels"
		       :number-of-tick-marks 20
		       :min-label "Min"
		       :max-label "Max"
		       :value-changed-callback 'slider-changed-callback
		       :drag-callback 'slider-dragged-callback)))
	(outlining ()
	  (spacing ()
	    (make-pane 'slider
		       :label "with visible-value"
		       :show-value-p t
		       :min-value 0
		       :max-value 20
		       :number-of-tick-marks 20
		       :number-of-quanta 20
		       :value-changed-callback 'slider-changed-callback
		       :drag-callback 'slider-dragged-callback)))
	(outlining ()
	  (scrolling ()
	    (make-pane 'interactor-pane))))
      (vertically ()
	(outlining ()
	  (spacing ()
	    (make-pane 'slider
		       :orientation :vertical
		       :value-changed-callback 'slider-changed-callback
		       :drag-callback 'slider-dragged-callback)))
	(outlining ()
	  (spacing ()
	    (make-pane 'slider
		       :orientation :vertical
		       :number-of-tick-marks 10
		       :value-changed-callback 'slider-changed-callback
		       :drag-callback 'slider-dragged-callback))))
      (outlining ()
	(spacing ()
	  (make-pane 'slider
		     :orientation :vertical
		     :number-of-tick-marks 20
		     :min-label "Min"
		     :max-label "Max"
		     :value-changed-callback 'slider-changed-callback
		     :drag-callback 'slider-dragged-callback)))
      (outlining ()
	(spacing ()
	  (make-pane 'slider
		     :orientation :vertical
		     :number-of-tick-marks 20
		     :label "Vertical Label"
		     :min-label "Min"
		     :max-label "Max"
		     :value-changed-callback 'slider-changed-callback
		     :drag-callback 'slider-dragged-callback)))
      (outlining ()
	(spacing ()
	  (make-pane 'slider
		     :orientation :vertical
		     :number-of-tick-marks 20
		     :number-of-quanta 20
		     :label "Visi."
		     :show-value-p t
		     :min-value 0
		     :max-value 20
		     :min-label "Min"
		     :max-label "Max"
		     :value-changed-callback 'slider-changed-callback
		     :drag-callback 'slider-dragged-callback))))))

(defvar *stf* nil)

(defun test-sliders (&optional reinitialize (port (port (find-port))))
  (when (or reinitialize (null *stf*))
    (setq *stf* (make-application-frame 'slider-test-frame
					:width 800 :height 600
					:frame-manager (find-frame-manager :port port))))
  (run-frame-top-level *stf*))
