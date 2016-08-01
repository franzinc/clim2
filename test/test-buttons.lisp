;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-USER; Base: 10; Lowercase: Yes -*-


(in-package :clim-user)

(defparameter *silly-button*
	      (make-pattern #2a(		; width = 21  height = 21 
				( 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ) 
				( 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 0 ) 
				( 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 ) 
				( 0 1 0 0 0 1 1 1 1 0 0 0 1 1 1 1 0 0 0 1 0 ) 
				( 0 1 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 1 0 ) 
				( 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 ) 
				( 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 ) 
				( 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 ) 
				( 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 ) 
				( 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 ) 
				( 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 ) 
				( 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 ) 
				( 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 ) 
				( 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 ) 
				( 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 ) 
				( 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 ) 
				( 0 1 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 1 0 ) 
				( 0 1 0 0 0 1 1 1 1 0 0 0 1 1 1 1 0 0 0 1 0 ) 
				( 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 ) 
				( 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 0 ) 
				( 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ))
			    (list +background-ink+ +foreground-ink+)))

(define-application-frame button-test-frame () (radio-box)
  (:command-table test-frame)
  (:pane 
    (vertically ()
      (outlining ()
	(vertically ()
	  (make-pane 'push-button 
		     :activate-callback 'push-button-callback)
	  (make-pane 'push-button 
		     :external-label "Default Square Button with External Label"
		     :activate-callback 'push-button-callback)
	  (make-pane 'push-button 
		     :external-label "Circular Button"
		     :pattern silica::*circle-button-pattern*
		     :activate-callback 'push-button-callback)
	  (make-pane 'push-button 
		     :external-label "Rounded Button"
		     :pattern silica::*rounded-button-pattern*
		     :activate-callback 'push-button-callback)))
      (outlining ()
	(horizontally ()
	  (make-pane 'push-button 
		     :label "Normal"
		     :pattern silica::*rounded-button-pattern*
		     :activate-callback 'push-button-callback)
	  (make-pane 'push-button 
		     :label "Big"
		     :text-style (make-text-style :sans-serif :bold :very-large)
		     :pattern silica::*rounded-button-pattern*
		     :activate-callback 'push-button-callback)))
      (outlining ()
	(horizontally ()
	  (make-pane 'push-button 
		     :external-label "Triangular Button"
		     :pattern silica::*right-triangle-button-pattern*
		     :activate-callback 'push-button-callback)
	  (make-pane 'push-button 
		     :pattern *silly-button*
		     :label "Silly Shaped Buttons"
		     :activate-callback 'push-button-callback)
	  (make-pane 'push-button 
		     :text-style (make-text-style :sans-serif :bold :very-large)
		     :pattern silica::*rounded-button-pattern*
		     :icon-pattern *silly-button*
		     :activate-callback 'push-button-callback)))
      #+ignore
      (outlining ()
	(horizontally ()
	  (make-pane 'push-button 
		     :label "B1"
		     :activate-callback 'push-button-callback)
	  (make-pane 'push-button 
		     :label "B2"
		     :activate-callback 'push-button-callback)
	  (make-pane 'push-button 
		     :external-label "B3"
		     :activate-callback 'push-button-callback)
	  (make-pane 'push-button 
		     :external-label "Push Button with External Label"
		     :activate-callback 'push-button-callback)
	  (make-pane 'push-button 
		     :external-label "Circular Button"
		     :pattern silica::*circle-button-pattern*
		     :activate-callback 'push-button-callback)))
      #+ignore
      (horizontally ()
	(vertically ()
	  (outlining ()
	    (horizontally ()
	      (make-pane 'toggle-button
			 :label "T1" :width 80 
			 :value-changed-callback 'toggle-button-callback)
	      (make-pane 'toggle-button 
			 :label "T2" :width 80
			 :value-changed-callback 'toggle-button-callback)))
	  (outlining ()
	    (with-radio-box (:value-changed-callback 'radio-value-changed-callback)
	      (make-pane 'toggle-button
			 :label "RT1" :width 80)
	      (radio-box-current-selection
		(make-pane 'toggle-button
			   :label "RT2" :width 80))
	      (make-pane 'toggle-button
			 :label "RT3" :width 80)))
	  (outlining ()
	    (with-radio-box (:type :some-of
			     :value-changed-callback 'radio-value-changed-callback)
	      (make-pane 'toggle-button
			 :label "CT1" :width 80)
	      (radio-box-current-selection
		(make-pane 'toggle-button
			   :label "CT2" :width 80))
	      (make-pane 'toggle-button
			 :label "CT3" :width 80))))
	(outlining ()
	  (with-radio-box (:value-changed-callback 'radio-value-changed-callback)
	    (make-pane 'toggle-button
		       :label "RT1" :width 80)
	    (radio-box-current-selection
	      (make-pane 'toggle-button
			 :label "RT2" :width 80))
	    (make-pane 'toggle-button
		       :label "RT3" :width 80))))
      (outlining ()
	(scrolling ()
	  (make-pane 'interactor-pane))))))


(defvar *btf* nil)

(defun test-buttons (&optional reinitialize (port (port (find-port))))
  (when (or reinitialize (null *btf*))
    (setq *btf* (make-application-frame 'button-test-frame 
					:width 800 :height 700
					:frame-manager (find-frame-manager :port port))))
  (run-frame-top-level *btf*))
