;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-USER; Base: 10; Lowercase: Yes -*-

;;
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-user)

;;; Simple little frame

(defun make-graphical-label ()
  (with-output-to-pixmap (stream (graft *application-frame*) :width 100 :height 100)
    (draw-rectangle* stream 0 0 100 100 :ink +background-ink+)
    (draw-rectangle* stream 10 10 90 90 :ink +red+)))

(define-application-frame test-frame () ()
  (:pane
   (vertically ()
     (make-clim-interactor-pane
      :foreground +green+
      :background +red+)
     (make-pane 'push-button
		:label (make-graphical-label))
     (make-pane 'push-button
		:label "press me"
		:activate-callback (command-callback #'(lambda (gadget) (print :hello)))
		:background +black+
		:foreground +cyan+
		:text-style (make-text-style :serif :roman 20)))))


;;; Some commands

(define-test-frame-command (com-make-table :name t :menu t) ()
  (formatting-table (*query-io*)
    (dotimes (i 10)
      (formatting-row (*query-io*)
	(dotimes (j 10)
	  (formatting-cell (*query-io*)
	    (present (* i j) 'integer :stream *query-io*)))))))

(define-test-frame-command (com-square-it :name t :menu t)
    ((x 'integer :gesture :select))
  (present (* x x) 'integer :stream *query-io*))

(define-test-frame-command (com-double-it :name t :menu t)
    ((x 'integer :gesture :select))
  (present (+ x x) 'integer :stream *query-io*))

(define-test-frame-command (com-clear :name t :menu t) ()
  (window-clear *query-io*))

(define-test-frame-command (com-quit :name t :menu ("Quit" :documentation "Word")) ()
  (frame-exit *application-frame*))

(define-test-frame-command (com-show :name t :menu t) ()
  (terpri *query-io*)
  (display-command-table-menu
    (frame-command-table *application-frame*) *query-io*
    :move-cursor t)
  (terpri *query-io*))


;;; Simple little frame that specifies an icon

(define-application-frame test-frame0 () ()
  (:command-table test-frame)
  (:pane (make-clim-interactor-pane))
  (:icon :name "foo"
	 :pixmap (make-pattern-from-bitmap-file
		  "/usr/include/X11/bitmaps/terminal"
		  :designs (list +red+ +green+)))
  (:pointer-documentation t)
  (:geometry :width 800 :height 800))


;;; More frames

(define-application-frame test-frame4 () ()
  (:command-table test-frame)
  (:pane
    (vertically ()
      (make-pane 'push-button :label "Press me")
      (make-pane 'toggle-button)
      (make-pane 'slider)
      (outlining ()
	(make-pane 'text-field))
      (make-clim-interactor-pane
	:width 300 :max-width +fill+
	:height 300 :max-height +fill+))))

(define-application-frame test-frame5 () ()
  (:command-table test-frame)
  (:panes
    (a (horizontally ()
	 (make-pane 'push-button :label "Press me")
	 (make-pane 'push-button :label "Squeeze me")))
    (b toggle-button)
    (c slider)
    (d text-field)
    (e :interactor
       :width 300 :max-width +fill+
       :height 300 :max-height +fill+))
  (:layouts
    (:default
      (vertically ()
	a b c e))
    (:more
      (vertically ()
 	a e b d))))

(define-test-frame-command (com-switch :name t :menu t) ()
  (setf (frame-current-layout *application-frame*)
	(ecase (frame-current-layout *application-frame*)
	  (:default :more)
	  (:more :default))))

(define-test-frame-command (com-number-please :name t)
    ((num 'integer :provide-default t))
  (print num *query-io*))

(define-test-frame-command (com-accept :name t :menu t)
    (&key (own-window 'boolean :default nil)
	  (view '(member :textual :gadget :default) :default ':default))
  (let* ((stream *query-io*)
	 (a t)
	 (b nil)
	 (c :normal)
	 (d 10)
	 (e 40.0)
	 (framem (frame-manager stream)))
    (clim-utils:letf-globally (((frame-manager-dialog-view framem)
				(case view
				  (:textual +textual-dialog-view+)
				  (:gadget  +gadget-dialog-view+)
				  (:default (frame-manager-dialog-view framem)))))
      (accepting-values (stream :own-window own-window)
	(setq a (accept 'boolean
			:stream stream
			:default a :prompt "Boolean"))
	(terpri stream)
	(unless a
	  (setq b (accept 'boolean
			  :stream stream
			  :default b :prompt "Another boolean"))
	  (terpri stream))
	(when a
	  (setq c (accept '(member :normal :point) :stream stream
			  :prompt "Line style units" :default c
			  :view '(radio-box-view :label "Line style units")))
	  (terpri stream))
	(setq d (accept '(integer 0 100)
			:stream stream
			:prompt "Integer" :default d))
	(terpri stream)
	(setq e (accept '(real 0 100)
			:stream stream
			:prompt "Real" :default e
			:view 'slider-view))
	(terpri stream)
	(accept-values-command-button (stream)
	    "Press me"
	  (setq a t
		b nil
		c :normal))))
    (format *query-io* "~&Values are ~S ~S ~S ~D ~D" a b c d e)))


(define-presentation-type some-kinda-gadget ())

(define-presentation-method present (object (type some-kinda-gadget) stream (view t) &key)
  ;; Kludge!
  (write-string "gadget" stream))

(define-test-frame-command (com-make-one :name t :menu t)
    ((what '(subset :slider :push-button :interactor :radio-box)
	   :default '(:slider :push-button :interactor)))
  (let* ((stream *query-io*))
    (when (member :slider what)
      (let ((weird (cons nil nil)))
	(setf (car weird)
	      (with-output-as-presentation (stream weird 'some-kinda-gadget)
		(surrounding-output-with-border (stream)
		  (with-output-as-gadget (stream)
		    (make-pane 'slider :width 100 :min-height 20)))))))
    (when (member :push-button what)
      (let ((weird (cons nil nil)))
	(setf (car weird)
	      (with-output-as-presentation (stream weird 'some-kinda-gadget)
		(surrounding-output-with-border (stream)
		  (with-output-as-gadget (stream)
		    (make-pane 'push-button
			       :label "Amazing")))))))
    (when (member :interactor what)
      (let ((weird (cons nil nil)))
	(setf (car weird)
	      (with-output-as-presentation (stream weird 'some-kinda-gadget)
		(surrounding-output-with-border (stream)
		  (with-output-as-gadget (stream)
		    (scrolling ()
		      (make-pane 'interactor-pane :width 150 :height 60))))))))
    (when (member :radio-box what)
      (let ((weird (cons nil nil)))
	(setf (car weird)
	      (with-output-as-presentation (stream weird 'some-kinda-gadget)
		(surrounding-output-with-border (stream)
		  (with-output-as-gadget (stream)
		    (with-radio-box ()
		      (make-pane 'toggle-button :label "A")
		      (radio-box-current-selection
			(make-pane 'toggle-button :label "B"))
		      (make-pane 'toggle-button :label "C"))))))))))

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
	       (make-pane 'push-button
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
      (make-pane 'radio-box
		 :choices
		   (list
		     (make-pane 'toggle-button :label "a")
		     (make-pane 'toggle-button :label "b")
		     (make-pane 'toggle-button :label "c"))))))


(define-application-frame tf0 () (radio-box)
  (:command-table test-frame)
  (:pane
    (vertically ()
      (outlining ()
	(horizontally ()
	  (make-pane 'push-button
	    :label "B1"
	    :activate-callback 'push-button-callback)
	  (make-pane 'push-button
	    :label "B2"
	    :activate-callback 'push-button-callback)))
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
	    :label "CT3" :width 80)))
      (outlining ()
	(spacing ()
	  (make-pane 'slider
	    :label "Slider"
	    :value-changed-callback 'slider-changed-callback
	    :drag-callback 'slider-dragged-callback)))
      (outlining ()
	(scrolling ()
	  (make-pane 'interactor-pane))))))

(defun push-button-callback (button)
  (format t "~&Button ~A pushed" (gadget-label button)))

(defun toggle-button-callback (button value)
  (format t "~&Button ~A toggled to ~S" (gadget-label button) value))

(defun radio-value-changed-callback (radio-box value)
  (declare (ignore radio-box))
  (format t "~&Radio box toggled to ~S" value))

(defun slider-changed-callback (slider value)
  (format t "~&Slider ~A changed to ~S" (gadget-label slider) value))

(defun slider-dragged-callback (slider value)
  (format t "~&Slider ~A dragged to ~S" (gadget-label slider) value))

(defun option-pane-changed-callback (tf value)
  (format t "~&Option menu ~A changed to ~S"  tf value))

(defun list-pane-changed-callback (tf value)
  (format t "~&List pane ~A changed to ~S"  tf value))


(defun text-field-changed (tf value)
  (format t "~&Text field ~A changed to ~S" tf value))

(defclass insect () ())

(define-test-frame-command (com-make-insect :name t :menu t)
    ()
  (let ((i (make-instance 'insect)))
    (with-output-as-presentation (t i (presentation-type-of i))
      (print i))
    (terpri)))

(define-test-frame-command (com-describe-insect :name t :menu t)
    ((bug 'insect
	  :gesture (nil :menu t)
	  :prompt "Select an insect"))
  (describe bug))

(define-test-frame-command (com-disable-insect :name t :menu t) ()
  (setf (command-enabled 'com-describe-insect *application-frame*)
	(not (command-enabled 'com-describe-insect *application-frame*))))

(clim:define-application-frame tf100 () ()
  (:command-table test-frame)
  (:pane
   (vertically ()
     (outlining ()
       (scrolling ()
	 (make-pane 'text-editor
		    :word-wrap nil
		    :value "c sucks"
		    :width '(30 :character)
		    :height '(10 :line))))
     (outlining ()
       (scrolling (:scroll-bars :vertical)
	 (make-pane 'text-editor
		    :word-wrap t
		    :value "unix sucks more"
		    :width '(30 :character)
		    :height '(10 :line)))))))

(define-application-frame tf99 () ()
  (:command-table test-frame)
  (:pane
    (horizontally ()
      (make-pane 'slider
        :label "SliderH"
	:orientation :vertical
	:show-value-p t
	:value-changed-callback 'slider-changed-callback
	:drag-callback 'slider-dragged-callback)
      (vertically ()
        (outlining ()
	  (horizontally ()
	    (make-pane 'push-button
	      :label "B1"
	      :activate-callback 'push-button-callback)
	    (make-pane 'push-button
	      :label "B2"
	      :activate-callback 'push-button-callback)))
	(outlining ()
	  (horizontally ()
	    (make-pane 'toggle-button
	      :label "T1"
	      :value-changed-callback 'toggle-button-callback)
	    (make-pane 'toggle-button
	      :label "T2"
	      :value-changed-callback 'toggle-button-callback)))
	(horizontally ()
	  (outlining ()
	    (scrolling ()
	      (make-pane 'text-editor
		:value "c sucks"
		:value-changed-callback 'text-field-changed
		:width '(30 :character)
		:height '(10 :line))))
	  (outlining ()
	    (scrolling ()
	      (make-pane 'text-editor
		:value "unix sucks more"
		:value-changed-callback 'text-field-changed
		:width '(30 :character)
		:height '(10 :line)))))
	(outlining ()
	  (spacing ()
	    (make-pane 'slider
	      :label "Slider"
	      :show-value-p t
	      :value-changed-callback 'slider-changed-callback
	      :drag-callback 'slider-dragged-callback)))))))

(define-application-frame tf98 () ()
  (:command-table test-frame)
  (:pane
    (vertically ()
      (outlining ()
	(horizontally ()
	  (make-pane 'push-button
	    :label "B1"
	    :activate-callback 'push-button-callback)
	  (make-pane 'push-button
	    :label "B2"
	    :activate-callback 'push-button-callback)
	  :fill))
      (outlining ()
	(horizontally ()
	  (make-pane 'toggle-button
	    :label "T1"
	    :value-changed-callback 'toggle-button-callback)
	  (make-pane 'toggle-button
	    :label "T2"
	    :value-changed-callback 'toggle-button-callback)))
      (outlining ()
	(horizontally ()
	  (outlining ()
	    (scrolling ()
	      (make-pane 'text-editor
		:value "c sucks"
		:value-changed-callback 'text-field-changed
		:width '(30 :character)
		:height '(10 :line))))
	  (outlining ()
	    (scrolling ()
	      (make-pane 'text-editor
		:value "unix sucks more"
		:value-changed-callback 'text-field-changed
		:width '(30 :character)
		:height '(10 :line))))))
      (outlining ()
 	(spacing ()
 	  (make-pane 'slider
	    :label "Slider"
	    :show-value-p t
	    :value-changed-callback 'slider-changed-callback
	    :drag-callback 'slider-dragged-callback))))))

(define-application-frame tf988 () ()
  (:command-table test-frame)
  (:pane
    (outlining ()
      (outlining ()
	(horizontally ()
	  (outlining ()
	    (scrolling ()
	      (make-pane 'text-editor
		:value "lucid "
		:value-changed-callback 'text-field-changed
		:width '(30 :character)
		:height '(10 :line))))
	  (scrolling ()
	    (make-pane 'text-editor
	      :value "harlqn  more"
	      :value-changed-callback 'text-field-changed
		:width '(30 :character)
		:height '(10 :line))))))))

(define-application-frame tf97 () ()
  (:command-table test-frame)
  (:pane
    (vertically ()
      (horizontally ()
	(scrolling ()
	  (make-pane 'list-pane
	    :value "Franz"
	    :test 'string=
	    :value-changed-callback 'list-pane-changed-callback
	    :items '("Franz" "Lucid" "Harlqn" "Symbolics")))
	:fill
	(scrolling ()
	  (make-pane 'list-pane
	    :value '("Lisp" "C")
	    :test 'string=
	    :mode :nonexclusive
	    :value-changed-callback 'list-pane-changed-callback
	    :items '("C" "Cobol" "Lisp" "Ada"))))
      (make-pane 'option-pane
	:items '("eenie" "meanie" "minie")
	:value "minie"
	:value-changed-callback 'option-pane-changed-callback
	:test 'string=
	:label "moo")
      (outlining ()
	(outlining ()
	  (scrolling ()
	    (make-pane 'text-editor
	      :value "lucid are nice guys "
	      :value-changed-callback 'text-field-changed
	      		:width '(30 :character)
		:height '(10 :line)))))
      (make-clim-interactor-pane :scroll-bars :vertical))))

(define-application-frame tf96 () ()
  (:command-table test-frame)
  (:panes
    (a :application :width '(80 :character))
    (b :application :width '(50 :mm))
    (c :application :height '(10 :line))
    (d :application :height '(5 :line))
    (e :interactor :height '(50 :mm)))
  (:layouts
    (:default
      (vertically ()
	(horizontally () a b)
	(vertically () c d e)))))

(define-test-frame-command (com-frob-sizes :name t :menu t) ()
  (changing-space-requirements ()
    (change-space-requirements
      (get-frame-pane *application-frame* 'a)
      :resize-frame t
      :width `(,(random 60) :character))
    (change-space-requirements
      (get-frame-pane *application-frame* 'b)
      :resize-frame t
      :width `(,(random 60) :character))
    (change-space-requirements
      (get-frame-pane *application-frame* 'd)
      :resize-frame t
      :height `(,(random 10) :line))))

(define-application-frame tf95 () ()
  (:command-table test-frame)
  (:panes
    (a :application :width '(80 :character))
    (b :application :width '(50 :mm))
    (c :application :height '(10 :line))
    (d :application :height '(5 :line))
    (e :interactor :height '(50 :mm)))
  (:layouts
    (:default
      (vertically ()
	(1/3 (horizontally () (1/10 a) (9/10 b)))
	(2/3 (vertically () (1/4 c) (1/2 d) (1/4 e)))))))

(define-application-frame tf94 () ()
  (:command-table test-frame)
  (:panes
    (a :application :width '(80 :character))
    (b :application)
    (c :application :height '(10 :line))
    (d :application)
    (e :interactor :height '(50 :mm)))
  (:layouts
    (:default
      (vertically ()
	(1/3 (horizontally () (1/10 a) (:fill b)))
	(2/3 (vertically () (1/4 c) (:fill d) (1/4 e)))))))

(define-application-frame tf93 () ()
  (:command-table test-frame)
  (:panes
    (a :application)
    (b :application)
    (c :application)
    (d :application)
    (e :interactor))
  (:layouts
    (:default
      (vertically ()
	(1/3 (horizontally () (1/10 a) (9/10 b)))
	(2/3 (vertically () (1/4 c) (1/2 d) (1/4 e)))))))

(define-application-frame tf91 () ()
  (:command-table test-frame)
  (:panes
    (a :application)
    (b :application)
    (c :application)
    (d :application)
    (e :interactor))
  (:layouts
    (:default
      (vertically ()
	(1/3 (horizontally () (1/10 a) (:fill b)))
	(2/3 (vertically () (1/4 c) (:fill d) (1/4 e)))))))

(define-application-frame tf92 () ()
  (:pane
    (make-pane 'vbox-pane
      :contents
        (mapcar #'(lambda (x)
		    (destructuring-bind (min max &optional (decimal-places 0)) x
		      (make-pane 'slider
			:label (format nil "Slider ~D,~D,~D"
				 min max decimal-places)
			:min-value min
			:max-value max
			:decimal-places decimal-places
			:show-value-p t
			:value-changed-callback 'slider-changed-callback
			:drag-callback 'slider-dragged-callback)))
		'((0 100)
		  (50 60)
		  (50 60 2)
		  (0.0 1.0 2))))))

(define-test-frame-command (com-test-accepting-subset :menu t :name t) ()
  (let ((x '(a c))
	(stream *query-io*))
    (accepting-values (stream)
      (setq x (accept '(subset a b c)
		      :default x
		      :stream stream
		      :prompt "foo")))
    (print x)))

(define-test-frame-command (com-test-accepting-boolean :menu t :name t) ()
  (let ((x 0)
	(y nil)
	(stream *query-io*))
    (accepting-values (stream :resynchronize-every-pass t )
      (setq x (accept 'integer
		      :default x
		      :stream stream
		      :prompt "foo"))
      (terpri stream)
      (setq y (accept 'boolean
		      :default (oddp x)
		      :stream stream
		      :prompt "bar")))))

(define-test-frame-command (com-test-accepting-values :menu t :name t)
    (&key
     (own-window 'boolean :default t)
     (gadget-dialog-view 'boolean :default t))
  (let* ((stream *query-io*)
	 (ptypes-and-prompts `((boolean "Finished it yet")
			       ((member a b c) "3 Member test")
			       ((member a b c d e f g h) "8 Member test")
			       ((member a b c d e f g h i
					j k l m o p q r s t u v w
					x y z)  "26 Member test")
			       ((subset a b c) "3 Subset test")
			       ((subset a b c d e f g h) "8 Subset test")
			       ((subset a b c d e f g h i
					j k l m o p q r s t u v w
					x y z)  "26 Subset test")
			       ;; Some types of real
			       ;; A big blob of text
			       ;; token-or-type??.tim
			       ;; Funny ones
			       ((or integer (member :small :large)) "Size:")
			       ))
	 (n (length ptypes-and-prompts))
	 (values (make-array n :initial-element :none)))

    (accepting-values (stream :own-window own-window :label "foo")
      ;; Test of the member stuff
      (clim-utils:letf-globally (((stream-default-view stream)
				  (if gadget-dialog-view
				      (stream-default-view stream)
				      +textual-dialog-view+)))
	(dotimes (i n)
	  (if (eq (svref values i) :none)
	      (setf (svref values i)
		    (accept (first (nth i ptypes-and-prompts))
			    :stream stream
			    :prompt (second (nth i ptypes-and-prompts))))
	      (setf (svref values i)
		    (accept (first (nth i ptypes-and-prompts))
			    :default (svref values i)
			    :stream stream
			    :prompt (second (nth i ptypes-and-prompts)))))
	  (terpri stream))))
    values))


;; This seems to work ok

(define-test-frame-command (com-test-nested-accept :name t :menu t) ()
  (let ((stream *query-io*))
    (let ((a 10)
	  (b 20)
	  (c 110)
	  (d 120))
      (accepting-values (stream)
	(setq a (accept 'integer :stream stream :prompt "a" :default a))
	(terpri stream)
	(setq b (accept 'integer :stream stream :prompt "b" :default b))
	(terpri stream)
	(accept-values-command-button (stream)
	    "Press me"
	  (restart-case
	      (accepting-values (stream  :own-window t :label "inner accept")
		(setq c (accept 'integer :stream stream :prompt "c" :default c))
		(terpri stream)
		(setq d (accept 'integer :stream stream :prompt "d" :default d))
		(terpri stream))
	    (abort)
	    (frame-exit)))))))

(define-application-frame tf101 () ()
  (:command-table test-frame)
  (:panes
    (a :application :height '(10 :line))
    (b :application :height '(5 :line))
    (c :interactor))
  (:layouts
    (:default
      (vertically () a b c))
    (:more
      (vertically () a b c)
      (a :height '(5 :line))
      (b :height '(10 :line)))))

#+Allegro
(define-application-frame tf102 () ()
  (:command-table test-frame)
  (:panes
    (a :application :height '(10 :line))
    (b :application :height '(5 :line))
    (c :interactor)
    (d :application))
  (:layouts
    (:default
      (vertically ()
	d
	#-acl86win32
	(make-pane 'xm-silica::motif-paned-pane
		   :contents (list a b c))
	#+acl86win32
	(horizontally () a b c)))))


#+Allegro
(define-application-frame tf103 () ()
  (:command-table test-frame)
  (:panes
    (a :application :height '(10 :line))
    (b :application :height '(5 :line))
    (c :interactor)
    (d :application))
  (:layouts
    (:default
      (vertically ()
	d
	#-acl86win32
	(make-pane 'xm-silica::motif-rc-pane
		   :contents (list* a
				    b
				    c
				    (let ((r nil))
				      (dotimes (i 10 (nreverse r))
					(push
					  (make-pane 'push-button
						     :label (format nil "button ~D" i))
					  r)))))
	#+acl86win32
    (horizontally () a b c)))))

#+Allegro
(define-application-frame tf104 () ()
  (:command-table test-frame)
  (:panes
    (a :application :height '(10 :line))
    (b :application :height '(5 :line))
    (c :interactor)
    (d :application))
  (:layouts
    (:default
      #-acl86win32
      (make-pane 'xm-silica::motif-form-pane
		 :attachments '((0 :left-attachment :form
				   :right-attachment :position
				   :right-position 33
				   :top-attachment :form
				   :bottom-attachment :form)
				(1 :left-attachment :position
				   :left-position 33
				   :right-attachment :position
				   :right-position 66
				   :top-attachment :form
				   :bottom-attachment :form)
				(2 :left-attachment :position
				   :left-position 66 :right-attachment :form
				   :top-attachment :form
				   :bottom-attachment :form))
		 :contents (list a b c))
    #+acl86win32
    (horizontally () a b c))))


;; This was from a pkarp mail message
(defun shift-output-record (stream record dx dy)
  (let ((parent (output-record-parent record)))
    (multiple-value-bind (x-offset y-offset)
	(convert-from-relative-to-absolute-coordinates stream parent)
      (multiple-value-bind (x y)
	  (bounding-rectangle-position record)
	(erase-output-record record stream)
	(output-record-set-position record (+ x dx) (+ y dy))
	(replay-output-record record stream nil x-offset y-offset)
	(add-output-record record parent)))))

(define-test-frame-command (com-shift-gadget :name t :menu t)
    ((weird 'some-kinda-gadget)
     (dx 'integer)
     (dy 'integer))
  (shift-output-record *query-io* (car weird) dx dy))


(define-application-frame tf105 () ()
  (:command-table test-frame)
  (:panes
   (c :interactor :height 200 :width 200))
  (:layouts
   (:default c)))


(define-application-frame tf106 ()
			  ((s :initform "hello")
			   (r :initform "hello")
			   (w :initform :oval)
			   (v :initform nil)
			   (u :initform :xxx)
			   (square-dimension :initform 100)
			   (draw-circle :initform t)
			   (draw-square :initform t)
			   (draw-/-diagonal :initform t)
			   (draw-\\-diagonal :initform t)
			   (line-thickness :initform 1)
			   (line-thickness-units :initform :normal))
  (:command-table (tf106 :inherit-from (accept-values-pane)))
  (:panes
   (d :application
      :height :compute
      :display-function 'display-frame-b)
   (c :accept-values
      :height :compute :width :compute
      :display-function `(accept-values-pane-displayer
			  :resynchronize-every-pass t
			  :displayer display-frame-c))
   (e :accept-values
      :height :compute :width :compute
      :display-function `(accept-values-pane-displayer
			  :displayer display-frame-e)))
  (:layouts
   (:default (vertically () c d e))))

(defun display-frame-b (frame stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (window-clear stream)
  (with-slots (square-dimension
	       draw-circle
	       draw-square
	       draw-/-diagonal
	       draw-\\-diagonal
	       line-thickness
	       line-thickness-units) frame
    (with-room-for-graphics (stream)
      (let ((radius (/ square-dimension 2)))
	(with-drawing-options (stream :line-unit line-thickness-units
				      :line-thickness line-thickness)
	  (when draw-square
	    (draw-polygon* stream (list 0 0
					0 square-dimension
					square-dimension square-dimension
					square-dimension 0)
			   :line-joint-shape :miter
			   :filled nil))
	  (when draw-circle
	    (draw-circle* stream radius radius radius
			  :filled nil))
	  (when draw-/-diagonal
	    (draw-line* stream 0 square-dimension square-dimension 0
			:line-cap-shape :round))
	  (when draw-\\-diagonal
	    (draw-line* stream 0 0 square-dimension square-dimension
			:line-cap-shape :round)))))))

(defun display-frame-c (frame stream)
  (with-slots (square-dimension
	       draw-circle
	       draw-square
	       draw-/-diagonal
	       draw-\\-diagonal
	       line-thickness
	       line-thickness-units) frame
    (setq square-dimension
      (accept 'number :stream stream
	      :prompt "Size of square" :default square-dimension))
    (terpri stream)
    (setq draw-circle
      (accept 'boolean :stream stream
	      :prompt "Draw the circle" :default draw-circle))
    (terpri stream)
    (setq draw-square
      (accept 'boolean :stream stream
	      :prompt "Draw the square" :default draw-square))
    (terpri stream)
    (setq draw-/-diagonal
      (accept 'boolean :stream stream
	      :prompt "Draw / diagonal" :default draw-/-diagonal))
    (terpri stream)
    (setq draw-\\-diagonal
      (accept 'boolean :stream stream
	      :prompt "Draw \\ diagonal" :default draw-\\-diagonal))
    (terpri stream)
    (setq line-thickness
      (accept 'number :stream stream
	      :prompt "Line thickness" :default line-thickness))
    (terpri stream)
    (setq line-thickness-units
      (accept '(member :normal :point) :stream stream
	      :prompt "Line style units" :default line-thickness-units))
    (terpri stream)))

(defun display-frame-e (frame stream)
  (with-slots (s r w v u) frame
    (setq s (accept 'string :stream stream
		    :prompt "bar"
		    :default s))
    (terpri stream)
    (setq r  (accept 'string :stream stream
		     :prompt "foo"
		     :view '(text-editor-view
			     :width (30 :character)
			     :height (5 :line))
		     :default r))
    (terpri stream)
    (setq u (accept '(member :xxx :yyy :zyy)
		    :view +list-pane-view+
		    :stream stream
		    :prompt "baz"
		    :default u))
    (terpri stream)
    (setq w (accept '(member :oval :rectangle)
		    :view +option-pane-view+
		    :stream stream
		    :prompt "bazzy"
		    :default w))
    (terpri stream)
    (setq v (accept '(subset :x :y :z)
		    :view +list-pane-view+
		    :stream stream
		    :prompt "barf"
 		    :default v))))

(clim-demo::define-lisp-listener-command (com-test-active-p :name t)
    ()
  (let ((x 1)
	(y :a)
	(stream *query-io*))
    (accepting-values (stream :align-prompts t :resynchronize-every-pass t)
      (setq y (accept '(member :a :b :c)
		      :default y
		      :active-p (< x 5)
		      :stream stream
		      :prompt "YYY"))
      (setq x (accept '(integer 0 10)
		      :default x
		      :view +slider-view+
		      :stream stream
 		      :prompt "XXXX")))))


(define-application-frame tf107 () ()
  (:command-table test-frame)
  (:pane
   (bulletin-board ()
     ((10 10) (make-clim-interactor-pane
		  :height '(10 :line)
		  :width '(50 :character)
		  :foreground +green+
		  :background +red+))
     ((50 200) (make-pane 'push-button
			     :label (with-menu (menu (graft *application-frame*))
				      (with-output-to-pixmap (stream menu :width 100 :height 100)
					(draw-rectangle* stream 0 0 100 100 :ink +background-ink+)
					(draw-rectangle* stream 10 10 90 90 :ink +red+)))))
     ((90 300) (make-pane 'push-button
			     :label "press me"
			     :background +black+
			     :foreground +cyan+
			     :text-style (make-text-style :serif
							  :roman 20))))))

(define-application-frame tf108 ()
			   ()
   (:command-table test-frame)
   (:panes
    (z push-button :label "fart")
    (a text-field)
    (b label-pane :label "hello")
    (c text-field :width '(50 :character))
    (d text-editor :height '(10 :line))
    (e label-pane :label "goodbye" :width '(50 :character)  :max-width +fill+)
    (f push-button :label "goodbye" :width '(50 :character))
    (g text-field :width '(50 :character) :editable-p nil)
    (h text-editor :height '(10 :line)  :editable-p nil))
   (:layouts
    (default (scrolling (:max-height +fill+)
	       (vertically (:max-width +fill+ :y-spacing 20) z a b c d
			   e f g h)))))

(define-application-frame tf109 ()
   ()
   (:command-table test-frame)
   (:panes
    (a list-pane :items '(a b c) :visible-items 3)
    (b option-pane :items '(x y z))
    (z :interactor :height '(5 :line)))
   (:layouts
    (default (vertically () (scrolling () a) b z))))


(define-test-frame-command (com-change-set-gadget-items :name t :menu t)
    (&key
     (which 'boolean :default nil))
  (let ((a (find-pane-named *application-frame* 'a))
	(b (find-pane-named *application-frame* 'b)))
    (when which
      (setf (set-gadget-items a) '(1 2 3 4)
	    (set-gadget-items b) '(p q r z)))
    (unless which
      (setf (set-gadget-items a) '(p q r z)
	    (set-gadget-items b) '(1 2 3 4)))))

(define-test-frame-command (com-change-set-gadget-values :name t :menu t)
    (&key
     (which 'boolean :default nil))
  (let ((a (find-pane-named *application-frame* 'a))
	(b (find-pane-named *application-frame* 'b)))
    (when which
      (setf (gadget-value a) 3
	    (gadget-value b) 'q))
    (unless which
      (setf (gadget-value a) 'z
	    (gadget-value b) 2))))

(define-application-frame tf110 ()
  ()
  (:menu-bar nil)
  (:command-table test-frame)
  (:panes
   (a :application)
   (b :interactor :height '(5 :line) :max-height '(5 :line))
   (c :interactor :height '(5 :line)))
  (:layouts
   (default (vertically () (:fill a) b))
   (other (vertically () (3/4 a) (:fill c)))))


(define-application-frame tf111 ()
  ()
  (:menu-bar nil)
  (:command-table test-frame)
  (:pane
   (vertically ()
     (scrolling ()
       (vertically ()
	 (with-radio-box () "Common Lisp" "Smalltalk" "Fortran" "Cobol")
	 (with-radio-box (:orientation :vertical) "Common Lisp" "Smalltalk" "Fortran" "Cobol")
	 (with-radio-box (:orientation :vertical :columns 2) "Common Lisp" "Smalltalk" "Fortran" "Cobol")
	 (with-radio-box (:orientation :horizontal :rows 2) "Common Lisp" "Smalltalk" "Fortran" "Cobol")
	 (with-radio-box (:orientation :horizontal :columns 1) "Common Lisp" "Smalltalk" "Fortran" "Cobol")))
     (scrolling ()
       (vertically ()
	 (with-radio-box (:type :some-of) "Common Lisp" "Smalltalk" "Fortran" "Cobol")
	 (with-radio-box (:type :some-of :orientation :vertical) "Common Lisp" "Smalltalk" "Fortran" "Cobol")
	 (with-radio-box (:type :some-of :orientation :vertical :columns 2) "Common Lisp" "Smalltalk" "Fortran" "Cobol")
	 (with-radio-box (:type :some-of :orientation :horizontal :rows 2) "Common Lisp" "Smalltalk" "Fortran" "Cobol")
	 (with-radio-box (:type :some-of :orientation :vertical :rows 1) "Common Lisp" "Smalltalk" "Fortran" "Cobol"))))))


(define-application-frame tf112 ()
  ()
  (:menu-bar nil)
  (:command-table test-frame)
  (:pane
   (vertically ()
     (make-pane 'label-pane :label (make-graphical-label))
     (make-pane 'slider :label (make-graphical-label))
     (make-pane 'option-pane :label (make-graphical-label)))))



(define-application-frame tf113 ()
  ()
  (:menu-bar t)
  (:panes
   (a :application :scroll-bars nil)
   (b :application :scroll-bars nil)
   (i :interactor))
  (:layouts
   (default (vertically () a b i))))

(define-tf113-command (com-whacky-cursor-1 :name t :menu t)
    ()
  (dolist (pane-and-ink `((a ,+green+) (b ,+blue+) (i ,+red+)))
    (destructuring-bind (pane ink) pane-and-ink
      (setf (sheet-pointer-cursor
	     (get-frame-pane *application-frame* pane))
	(make-pattern-from-bitmap-file
	 "/usr/include/X11/bitmaps/tie_fighter"
	 :designs (list +nowhere+ ink))))))

(define-tf113-command (com-whacky-cursor-2 :name t :menu t)
    ()
  (dolist (pane-and-ink `((a ,+green+) (b ,+blue+) (i ,+red+)))
    (destructuring-bind (pane ink) pane-and-ink
      (setf (sheet-pointer-cursor
	     (get-frame-pane *application-frame* pane))
	(make-pattern-from-bitmap-file
	 "/usr/include/X11/bitmaps/tie_fighter"
	 :designs (list +black+ ink))))))

;;;

(define-tf108-command (com-test-text-selection :name t)
    ()
  (let ((text-field (find-pane-named *application-frame* 'c))
	(text-editor (find-pane-named *application-frame* 'd)))
    (print (gadget-current-selection text-field))
    (print (gadget-current-selection text-editor))))

(define-tf108-command (com-test-text-editable :name t)
    ()
  (let ((text-field (find-pane-named *application-frame* 'c))
	(text-editor (find-pane-named *application-frame* 'd)))
    (setf (gadget-editable-p text-field) nil
	  (gadget-editable-p text-editor) nil
	  (gadget-editable-p text-field) t
	  (gadget-editable-p text-editor) t)))
;;


(define-application-frame tf114 ()
  ()
  (:menu-bar t)
  (:panes
   (a (with-radio-box () "Common Lisp" "Smalltalk" "Fortran" "Cobol"))
   (b (with-radio-box (:type :some-of) "Common Lisp" "Smalltalk" "Fortran" "Cobol")))
  (:layouts
   (default (vertically () a b))))

(define-tf114-command com-update-boxes
    ()
  (let ((rb (find-pane-named *application-frame* 'a))
	(cb (find-pane-named *application-frame* 'b)))
    (let ((n1 (nth (random (length (radio-box-selections rb))) (radio-box-selections rb)))
	  (n2 (list (nth (random (length (check-box-selections cb))) (check-box-selections cb)))))
      (setf (gadget-value rb) n1)
      (assert (eq n1 (radio-box-current-selection rb)))
      (setf (gadget-value cb) n2)
      (assert (equal n2 (check-box-current-selection cb))))))


(define-application-frame tf115 ()
  ()
  (:menu-bar nil)
  (:command-table test-frame)
  (:panes
   (a :accept-values
      :display-function '(accept-values-pane-displayer :displayer display-tf115)
      :width :compute
      :height :compute))
  (:layouts
   (default a)))

(defun display-tf115 (frame stream &key &allow-other-keys)
  (formatting-table (stream)
    (dolist (view '(list-pane-view text-editor-view))
      (formatting-row (stream)
	(dolist (scroll-bars '(nil :both :vertical :horizontal))
	  (formatting-cell (stream)
	    (accept (ecase view
		      (list-pane-view '(member a b c d e f))
		      (text-editor-view 'string))
		    :stream stream
		    :prompt nil
		    :query-identifier (cons view scroll-bars)
		    :view (cons view (and scroll-bars `(:scroll-bars ,scroll-bars))))))))))

