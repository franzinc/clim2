;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca.  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, Ca.  All rights reserved.
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
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: test.lisp,v 1.13 92/02/24 13:09:12 cer Exp $

(in-package :clim-internals)

(define-application-frame test-frame () ()
  (:pane 
    (vertically ()
      (scrolling ()
	(realize-pane 'interactor-pane
		      :foreground +green+
		      :background +red+))
      (realize-pane 'push-button
		    :label "press me"
		    :background (make-pattern #2A((0 0 0 1 1 0 0 0)
						  (0 0 1 1 1 1 0 0)
						  (0 1 1 1 1 1 1 0)
						  (1 1 1 1 1 1 1 1)
						  (1 1 1 1 1 1 1 1)
						  (0 1 1 1 1 1 1 0)
						  (0 0 1 1 1 1 0 0)
						  (0 0 0 1 1 0 0 0))
					      (list +red+ +green+))
		    :foreground +purple+
		    :text-style (make-text-style :serif :roman 20)))))


(define-application-frame test-frame0 () ()
  (:command-table test-frame)
  (:pane 
    (scrolling ()
      (realize-pane 'interactor-pane))))


(define-application-frame test-frame2 () ()
  (:command-table test-frame)
  (:pane 
    (vertically ()
      (tabling ()
	((horizontally ()
	   (realize-pane 'toggle-button)
	   (realize-pane 'toggle-button)
	   (realize-pane 'toggle-button))
	 (realize-pane 'text-field))
	((realize-pane 'push-button :label "hello")
	 (realize-pane 'slider)))
      (scrolling ()
	(realize-pane 'interactor-pane
		      :display-function 'moby-display-function))
      (scrolling ()
	(realize-pane 'interactor-pane)))))

(defun moby-display-function (frame stream)
  (window-clear stream)
  (display-command-table-menu 
    (frame-command-table frame) stream
    :max-width (bounding-rectangle-width stream)))


(define-application-frame test-frame3 () ()
  (:command-table test-frame)
  (:pane 
    (scrolling ()
      (realize-pane 'interactor-pane
		    :width 300 :height 300))))


(define-test-frame-command (com-square-it :name t :menu t)
    ((x 'integer :gesture :select))
  (present (* x x) 'integer :stream *query-io*))

(define-test-frame-command (com-double-it :name t :menu t)
    ((x 'integer :gesture :select))
  (present (+ x x) 'integer :stream *query-io*))


(define-test-frame-command (com-clear :name t :menu t)
    ()
  (window-clear *query-io*))

(define-test-frame-command (com-quit :name t :menu t)
    ()
  (frame-exit *application-frame*))

(define-test-frame-command (com-make-table :name t :menu t)
    ()
  (formatting-table (*query-io*)
    (dotimes (i 10)
      (formatting-row (*query-io*)
	(dotimes (j 10)
	  (formatting-cell (*query-io*)
	    (present (* i j) 'integer :stream *query-io*)))))))

(define-test-frame-command (com-show :name t :menu t)
    ()
  (terpri *query-io*)
  (display-command-table-menu 
    (frame-command-table *application-frame*) *query-io*
    :move-cursor t)
  (terpri *query-io*))


(define-application-frame test-frame4 () ()
  (:command-table test-frame)
  (:pane 
    (vertically ()
      (realize-pane 'push-button :label "Press me")
      (realize-pane 'toggle-button)
      (realize-pane 'slider)
      (realize-pane 'text-field)
      (scrolling ()
	(realize-pane 'interactor-pane
		      :width 300 :max-width +fill+
		      :height 300 :max-height +fill+)))))


(define-application-frame test-frame5 () ()
  (:command-table test-frame)
  (:panes
    (a (horizontally ()
	 (realize-pane 'push-button :label "Press me")
	 (realize-pane 'push-button :label "Squeeze me")))
    (b (realize-pane 'toggle-button))
    (c (realize-pane 'slider))
    (d (realize-pane 'text-field))
    (e (realize-pane 'interactor-pane
		     :width 300 :max-width +fill+
		     :height 300 :max-height +fill+)))
  (:layout
    (:default 
      (vertically ()
	a b c (scrolling () e)))
    (:more
      (vertically ()
	a (scrolling () e) b  d))))


(define-test-frame-command (com-switch :name t :menu t)
    ()
  (setf (frame-current-layout *application-frame*)
	(ecase (frame-current-layout *application-frame*)
	  (:default :more)
	  (:more :default))))

(define-test-frame-command (com-number-please :name t) 
    ((num 'integer :provide-default t))
  (print num *query-io*))

(define-test-frame-command (com-accept :name t :menu t)
    (&key (own-window 'boolean :default nil)
	  (textual-view 'boolean :default nil))
  (let ((stream *query-io*)
	(a t)
	(b nil)
	(c :normal)
	(d 10))
    (accepting-values (stream :own-window own-window)
      (letf-globally (((stream-default-view stream)
		       (if textual-view 
			   +textual-dialog-view+
			   (stream-default-view stream))))
	(setq a (accept 'boolean  
			:stream stream
			:default a :prompt "a"))
	(terpri stream)
	(unless a
	  (setq b (accept 'boolean  
			  :stream stream
			  :default b :prompt "b"))
	  (terpri stream))
	(when a
	  (setq c (accept '(member :normal :point) :stream stream
			  :prompt "Line style units" :default c))
	  (terpri stream))
	(setq d (accept '(integer 0 100) 
			:stream stream
			:prompt "d" :default d))
	(terpri stream)
	(accept-values-command-button (stream)
	    "Press me"
	  (setq a t
		b nil
		c :normal))))
    (format *query-io* "~&Values are ~S ~S ~S ~D" a b c d)))


(define-presentation-type some-kinda-gadget ())

(define-presentation-method present (object (type some-kinda-gadget) stream (view t) &key)
  ;; Kludge!
  (write-string "gadget" stream))

(define-test-frame-command (com-make-one :name t :menu t)
    ()
  (let* ((stream *query-io*))
    (let ((weird (cons nil nil)))
      (setf (car weird)
	    (with-output-as-presentation (stream weird 'some-kinda-gadget)
	      (surrounding-output-with-border (stream)
		(with-output-as-gadget (stream)
		  (realize-pane 'slider))))))
    (let ((weird (cons nil nil)))
      (setf (car weird)
	    (with-output-as-presentation (stream weird 'some-kinda-gadget)
	      (surrounding-output-with-border (stream)
		(with-output-as-gadget (stream)
		  (realize-pane 'push-button
				:label "Amazing"))))))
    (let ((weird (cons nil nil)))
      (setf (car weird)
	    (with-output-as-presentation (stream weird 'some-kinda-gadget)
	      (surrounding-output-with-border (stream)
		(with-output-as-gadget (stream)
		  (scrolling ()
		    (realize-pane 'interactor-pane)))))))))

(define-test-frame-command (com-move-gadget :name t :menu t)
    ((weird 'some-kinda-gadget))
  (drag-output-record *query-io* (car weird)
    :repaint nil
    :erase #'(lambda (gadget stream)
	       (with-bounding-rectangle* (left top right bottom) gadget
		 (draw-rectangle* stream
		   (1- left) (1- top)
		   (1+ right) (1+ bottom)
		   :ink +background-ink+)))))

(define-presentation-to-command-translator move-gadget-translator
    (some-kinda-gadget com-move-gadget test-frame)
    (object)
  (list object))

(define-test-frame-command (com-track :name t :menu t)
    ()
  (let* ((stream *query-io*))
    (tracking-pointer-test stream)))

(define-test-frame-command (com-make-button :name t :menu t)
    ()
  (let* ((stream *query-io*))
    (flet ((make-it (i)
	     (with-output-as-gadget (stream)
	       (realize-pane 
		 'push-button
		 :label (format nil "Amazing ~D" i)))))
      (format-graph-from-root
	'(a (c (x) (y)))
	#'(lambda (node s)
	    (declare (ignore s))
	    (make-it (if (consp node) (car node) node)))
	#'cdr
	:stream stream))))

(define-test-frame-command (com-make-radio-box :name t :menu t)
    ()
  (let* ((stream *query-io*))
    (with-output-as-gadget (stream)
      (let* (#+ignore (frame-pane (realize-pane 'frame-pane))
	     (gadget
	       (realize-pane 'radio-box
			     #+ignore :parent #+ignore frame-pane)))
	(realize-pane 'toggle-button :label "a" :parent gadget)
	(realize-pane 'toggle-button :label "b" :parent gadget)
	(realize-pane 'toggle-button :label "c" :parent gadget)
	gadget))))


(define-application-frame tf0 () (radio-box)
  (:command-table test-frame)
  (:pane 
    (vertically ()
      (outlining ()
	(horizontally ()
	  (realize-pane 'push-button 
			:label "B1"
			:activate-callback 'push-button-callback)
	  (realize-pane 'push-button 
			:label "B2"
			:activate-callback 'push-button-callback)))
      (outlining ()
	(horizontally ()
	  (realize-pane 'toggle-button
			:label "T1" 
			:value-changed-callback 'toggle-button-callback)
	  (realize-pane 'toggle-button 
			:label "T2"
			:value-changed-callback 'toggle-button-callback)))
      (outlining ()
	(with-radio-box ()
	  (realize-pane 'toggle-button
			:label "RT1"
			:value-changed-callback 'toggle-button-callback)
	  (radio-box-current-selection
	    (realize-pane 'toggle-button
			  :label "RT2"
			  :value-changed-callback 'toggle-button-callback))
	  (realize-pane 'toggle-button
			:label "RT3"
			:value-changed-callback 'toggle-button-callback)))
      (outlining ()
	(spacing ()
	  (realize-pane 'slider
			:label "Slider"
			:value-changed-callback 'slider-changed-callback
			:drag-callback 'slider-dragged-callback)))
      (outlining ()
	(scrolling ()
	  (realize-pane 'interactor-pane))))))

(defun push-button-callback (button)
  (format t "~&Button ~A pushed" (gadget-label button)))

(defun toggle-button-callback (button value)
  (format t "~&Button ~A toggled to ~S" (gadget-label button) value))

(defun slider-changed-callback (slider value)
  (format t "~&Slider ~A changed to ~S" (gadget-label slider) value))

(defun slider-dragged-callback (slider value)
  (format t "~&Slider ~A dragged to ~S" (gadget-label slider) value))
