;; -*- mode: common-lisp; package: xm-silica -*-
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
;; $fiHeader: xt-frames.lisp,v 1.14 92/07/20 16:01:55 cer Exp Locker: cer $


(in-package :xm-silica)

;; Basic intrinsics frame-manager

(defclass xt-frame-manager (standard-frame-manager) 
    ((menu-cache
       :initform nil
       :accessor frame-manager-menu-cache)
     ))

(defmethod frame-wrapper ((framem xt-frame-manager) 
			  (frame standard-application-frame) pane)
  (declare (ignore pane))
  (with-look-and-feel-realization (framem frame)
    (let* ((menu-bar (slot-value frame 'menu-bar))
	   (menu-bar-pane
	     (and menu-bar
		  (make-pane 'menu-bar
			     :command-table (if (eq menu-bar t)
						(frame-command-table frame)
						(find-command-table menu-bar)))))
	   (pointer-doc-pane
	     ;;--- Don't like these forward references
	     (and (clim-internals::frame-pointer-documentation-p frame)
		  (make-pane
		   'clim-internals::pointer-documentation-pane
		   :max-width +fill+
		   ;;--- This should be one line height in some text style
		   :height 15)))
	   (application-panes (call-next-method)))
      (cond ((and menu-bar-pane pointer-doc-pane)
	     (vertically () menu-bar-pane application-panes pointer-doc-pane))
	    (menu-bar-pane
	     (vertically () menu-bar-pane application-panes))
	    (pointer-doc-pane
	     (vertically () application-panes pointer-doc-pane))
	    (t
	     application-panes)))))


;;;

(defun command-button-callback (button dunno frame command-table item)
  (distribute-event
    (port frame)
    (allocate-event 'presentation-event
      :frame frame
      :sheet (frame-top-level-sheet frame)
      :presentation-type `(command :command-table ,command-table)
      :value (second item))))


(defun frame-wm-protocol-callback (widget frame)
  ;; Invoked when the Wm close function has been selected
  ;; We want to queue an "event" somewhere so that we can
  ;; synchronously quit from the frame
  (distribute-event
   (port frame)
   (allocate-event 'window-manager-delete-event
     :sheet (frame-top-level-sheet frame))))

(defmethod handle-event (sheet (event window-manager-delete-event))
  (frame-exit *application-frame*))

;;; Menu code

(defmethod frame-manager-menu-choose
	   ((framem xt-frame-manager) items &rest keys
	    &key printer 
		 presentation-type 
		 (associated-window (frame-top-level-sheet *application-frame*))
		 default-style label
		 cache
		 (unique-id items)
		 (id-test 'equal)
		 (cache-value items)
		 (cache-test #'equal))
  (declare (ignore keys))      
  (declare (values value chosen-item gesture))
  (let ((port (port framem))
	menu closure)
    (when cache
      (let ((x (assoc unique-id
		      (frame-manager-menu-cache framem)
		      :test id-test)))
	(when x
	  (destructuring-bind
	      (menu-cache-value amenu aclosure) x
	    (if (funcall cache-test cache-value menu-cache-value)
		(setq menu amenu closure aclosure)
	      (progn
		(setf (frame-manager-menu-cache framem)
		  (delete x (frame-manager-menu-cache framem)))
		(framem-destroy-menu framem amenu)
		#+ignore
		(tk::destroy-widget (tk::widget-parent amenu))))))))
      
    (unless menu
      (multiple-value-setq
	  (menu closure)
	(frame-manager-construct-menu framem 
				      items 
				      printer 
				      presentation-type 
				      associated-window
				      default-style
				      label))
      (when cache
	(push (list unique-id menu closure) 
	      (frame-manager-menu-cache framem))))
	  
    ;; initialize the closure
    (funcall closure t)
    ;;
    (multiple-value-bind
	(ignore win1 win2 x y root-x root-y)
	(tk::query-pointer (tk::display-root-window
			    (port-display port)))
      (declare (ignore ignore win1 win2 x y))
      (tk::set-values menu :x root-x :y root-y)
      (loop
	(when (funcall closure) (return nil))
	(framem-enable-menu framem menu)
	;; Now we have to wait
	(port-force-output port)
	(wait-for-callback-invocation
	 port
	 #'(lambda () 
	     ;;-- This is to deal
	     ;;-- with the race
	     ;;-- condition where
	     ;;-- the menu go down
	     ;;-- to quick
	     (or (funcall closure)
		 (not (framem-menu-active-p framem menu)))) 
	 "Returned value"))

      (framem-popdown-menu framem menu)
      
      (unless cache
	(framem-destroy-menu framem menu))
      
      (values-list (nth-value 1 (funcall closure))))))


(defun pixmap-from-menu-item (associated-window menu-item printer presentation-type)
  (with-menu (menu associated-window)
    (setf (stream-text-margin menu) 1000)
    (let ((rec (with-output-recording-options (menu :draw nil :record t)
		 (with-output-to-output-record (menu)
		   (handler-case
		       (if presentation-type
			   (present menu-item presentation-type :stream menu)
			 (funcall printer menu-item menu))
		     (error (c)
		       (write-string "Error in printer" menu)))))))
      (multiple-value-bind
	  (width height)
	  (bounding-rectangle-size rec)
	(with-output-to-pixmap (s associated-window :width width :height height)
	  (multiple-value-call #'draw-rectangle* 
	    s 0 0 (bounding-rectangle-size s) :ink +background-ink+)
	  (replay-output-record 
	    rec s +everywhere+
	    (- (bounding-rectangle-min-x rec))
	    (- (bounding-rectangle-min-y rec))))))))


;;;

(defmethod frame-manager-exit-box-labels ((framem xt-frame-manager) frame view)
  (declare (ignore frame view))
  '((:exit  "Ok")
    (:abort  "Cancel")))

(defmethod frame-manager-default-exit-boxes ((framem xt-frame-manager))
  '((:exit) (:abort)))
