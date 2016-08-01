;; -*- mode: common-lisp; package: silica -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :silica)

(defclass clim-sheet (sheet

		      ;; mirrored-sheet-mixin

		      sheet-multiple-child-mixin
		      sheet-transformation-mixin

		      standard-repainting-medium
		      standard-sheet-input-mixin

		      permanent-medium-sheet-output-mixin
		      )
	  ())

(defmethod stream-event-handler ((stream clim-sheet) &key timeout input-wait-test original-stream)
  (let ((pointer (stream-primary-pointer stream))
	(*original-stream* nil)
	(done-time (and timeout (+ timeout (get-internal-real-time))))
	ws)

    (loop

     (when (and input-wait-test (funcall input-wait-test original-stream))
       (return :input-wait-test))

     (when timeout
       (setq timeout (- done-time (get-internal-real-time))))

     (let ((event (and (not (and timeout (minusp timeout)))
		       (event-read-with-timeout stream timeout))))
       (cond ((null event)
	      (return :timeout))

	     ((typep event 'motion-event)
	      (return :mouse-motion))

	     ((typep event 'device-event)
	      (unread-event stream event)
	      (return :input-buffer))

	     (t
	      (handle-event (event-sheet event) event)))))))


(defmethod handle-repaint ((stream clim-sheet) nil region)
  )

;; window-beep
;; notify-user-1
;; (setf window-label)



