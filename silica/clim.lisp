;; -*- mode: common-lisp; package: silica -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-1998 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: clim.lisp,v 1.6.22.2 1998/07/06 23:09:53 layer Exp $

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



