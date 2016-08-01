(in-package :clim-user)




(clim-test:define-frame-test test-puzzle-demo (clim-demo::puzzle)
  (
   ;;-- Should verify which command gets executed
   (:presentation-click clim-demo::display clim-demo::puzzle-cell)
   (:presentation-click clim-demo::display clim-demo::puzzle-cell)
   (:presentation-click clim-demo::display clim-demo::puzzle-cell)
   (:presentation-click clim-demo::display clim-demo::puzzle-cell)
   ;;
   (clim-demo::com-scramble)
   (clim-demo::com-scramble)
   (clim-demo::com-scramble))
  (clim-demo::com-exit-puzzle)
  )



(clim-test:define-frame-test test-flight-planner (clim-demo::flight-planner :width 1000 :height 800)
  (
   (clim-demo::com-zoom-in)
   (clim-demo::com-zoom-out)
   (clim-demo::com-show-map)
   ;; Describe some things
   (:presentation-click clim-demo::display
			  clim-demo::concrete-object :gesture :describe)
   (:presentation-click clim-demo::display
			  clim-demo::concrete-object :gesture :describe)
   (:presentation-click clim-demo::display
			  clim-demo::concrete-object :gesture :describe)
   (:presentation-click clim-demo::display
			  clim-demo::concrete-object :gesture :describe)
   ;; Build a route
   (:presentation-click clim-demo::display
			clim-demo::named-position
			:gesture :select)
   (:presentation-click clim-demo::display
			clim-demo::named-position
			:gesture :select)
   (:presentation-click clim-demo::display
			clim-demo::named-position
			:gesture :select)
   (:presentation-click clim-demo::display
			clim-demo::named-position
			:gesture :select)
   #\return
   ;;
   (:presentation-click clim-demo::display
			clim-demo::route
			:gesture :delete)
   (:presentation-click clim-demo::display
			clim-demo::concrete-object
			:gesture :delete)
   ;;
   (:command clim-demo::display clim-demo::com-show-distance)
   ;; Missing the plan flight stuff
   )
  (clim-demo::com-exit-flight-planner)
  )


;;; Taking commands for a walk.


(clim-test:define-frame-test test-flight-planner-2 (clim-demo::flight-planner :width 1000 :height 800)
  (
   (:command clim-demo::display clim-demo::com-describe-object)
   )
  (clim-demo::com-exit-flight-planner)
  )


;; Listener

;;;--- The listener is using a nonstandard top level and does not
;;;---- seemed to be looking at the queue

(clim-test:define-frame-test test-lisp-listener (clim-demo::lisp-listener :width 1000 :height 800)
  (
   (:command clim-demo::interactor clim-demo::com-show-some-commands
	     :colon-prefix t)
   (:presentation-click clim-demo::interactor clim-demo::printer)

   (:presentation-click clim-demo::interactor clim-internals::accept-values-choice)
   :help
   ;;-- This only works because its bypassing the X event distribution mechanism
   #\newline
   "Santa Cruz Comic News"
   #\newline
   :abort
   (:sleep 4)
   "(cons nil nil)
"
   (:command clim-demo::interactor clim-demo::com-clear-output-history
	     :colon-prefix t)
   (:command clim-demo::interactor clim-demo::com-show-some-commands
	     :colon-prefix t)
   "(clim:draw-line* *standard-output* -51070 149 -51070 164)
"
   )
  (:command clim-demo::interactor clim-demo::com-quit-listener
	    :colon-prefix t)
  )


;; graphics-demos

(clim-test:define-frame-test test-graphics-demos (clim-demo::graphics-demo :width 840 :height 800)
  (
   (clim-demo::com-spin-graphics-demo)
   (clim-demo::com-big-spin-graphics-demo)
   (clim-demo::com-cbs-logo-graphics-demo)
   (clim-demo::com-polygons-graphics-demo)
   (:sleep 20)
   (clim-demo::com-circles-graphics-demo)
   (clim-demo::com-maze-graphics-demo)
   (:sleep 10)
   )
  (clim-demo::com-exit-graphics-demo)
  )


;;---- cad-demo

(clim-test:define-frame-test test-cad-demo (clim-demo::cad-demo :width 700 :height 600)
  (
   (clim-demo::com-setup)
   (:presentation-click clim-demo::design-area clim-demo::output :gesture :describe)
   (:presentation-click clim-demo::design-area clim-demo::output :gesture :describe)
   (:presentation-click clim-demo::design-area clim-demo::output :gesture :describe)
   (:presentation-click clim-demo::design-area clim-demo::output :gesture :describe)
   (clim-demo::com-refresh)
   (clim-demo::com-clear)
   #\s
   #\r
   #\l
   (:commands test-cad-demo-1)
   )
  (clim-demo::com-exit-cad-demo))

(defun test-cad-demo-1 (inv)
  (let ((frame (clim-test::invocation-frame inv)))
    (flet ((create-gadget (type x y)
	     (clim-test:execute-one-command inv '(clim-demo::com-create-component))
	     (clim-test:with-waiting (:timeout 30)
	       ((clim-test:find-menu frame) (menu-items)
				  menu-items
				  (clim-test:select-menu-item frame type)
				  (clim-test:click-on-window 'clim-demo::design-area x y)))))

      (create-gadget 'clim-demo::logic-zero 100 100)
      (create-gadget 'clim-demo::logic-one  100 200)
      (create-gadget 'clim-demo::and-gate  150 150)
      ;;
      (clim-test:click-on-presentation 'clim-demo::design-area 'clim-demo::component)
      (clim-test:click-on-window 'clim-demo::design-area (random 200) (random 200))
      ;;
      #+ignore
      (clim-test:click-on-presentation 'clim-demo::design-area 'clim-demo::output)
      #+ignore
      (clim-test:click-on-presentation 'clim-demo::design-area 'clim-demo::input))))


;;--- address-book

(clim-test:define-frame-test test-address-book (clim-demo::address-book :width 400 :height 400)
  (
   (:presentation-click clim-demo::names clim-demo::address)
   (:presentation-click clim-demo::names clim-demo::address)
   (:presentation-click clim-demo::names clim-demo::address)
   )
  (clim-demo::com-quit-address-book)
  )


;;--- Thinkadot

;;---  plot

(clim-test:define-frame-test test-plot-demo (clim-demo::plot-demo)
  (
   (clim-demo::com-redisplay)
   (:Commands test-plot-demo-1))
  (clim-demo::com-quit-plot-demo))

(defun test-plot-demo-1 (inv)
  inv
  (clim-test:execute-one-command inv '(:edit-avv  clim-demo::options "Graph type" :bar))
  (clim-test:execute-one-command inv '(:edit-avv  clim-demo::options "Graph type" :pie))

  ;;-- We cannot click on presentations in the above code because of
  ;;-- the single-box problem.

  (clim-test:execute-one-command inv '(:edit-avv  clim-demo::options "Graph type" :plot))

  (clim-test:click-on-presentation 'clim-demo::graph-window t :press nil :release nil :x-offset 5 :y-offset 5)

  (clim-test:execute-one-command inv "Describe region ")
  (clim-test:press-on-window 'clim-demo::graph-window 100 100)
  (clim-test:release-on-window 'clim-demo::graph-window 200 200)
  (clim-test:execute-one-command inv #\newline)

  (flet ((change-data-point ()
	   (clim-test:click-on-presentation 'clim-demo::data-window 'clim-demo::data-point)
	   (clim-test:execute-one-command inv (format nil "~D" (random 30)))
	   (clim-test:execute-one-command inv #\newline))
	 (change-y-label (label)
	   (clim-test:click-on-presentation 'clim-demo::data-window 'clim-demo::y-label)
	   (clim-test:execute-one-command inv label)
	   (clim-test:execute-one-command inv #\newline)))
    (change-y-label "Nowhere")
    (change-y-label "Somehere")
    (change-data-point)
    (change-data-point)
    (change-data-point))
  #+cannot-do-this
  (clim-test:execute-one-command inv '(clim-demo::com-add-new-column))
  (clim-test:execute-one-command inv '(clim-demo::com-random-update))
  (clim-test:execute-one-command inv '(clim-demo::com-add-new-row))
  )


;;---- color-editor

(clim-test:define-frame-test test-color-chooser (clim-demo::color-chooser)
  ((:commands test-color-chooser-1))
  (clim-demo::com-quit-color-chooser))

(defun test-color-chooser-1 (inv)
  (let ((frame (clim-test::invocation-frame inv)))
    (with-slots ((red clim-demo::red)
		 (green clim-demo::green)
		 (blue clim-demo::blue)
		 (intensity clim-demo::intensity)
		 (hue clim-demo::hue)
		 (saturation clim-demo::saturation)) frame
      (flet ((random-real (x)
	       (* (random 100) (/ x 100))))
	(declare (dynamic-extent #'random-real))
	(dotimes (i 20)
	  (clim-test:change-gadget-value red (random-real 1.0))
	  (clim-test:change-gadget-value green (random-real 1.0))
	  (clim-test:change-gadget-value blue (random-real 1.0))
	  (clim-test:change-gadget-value intensity (random-real (sqrt 3)))
	  (clim-test:change-gadget-value hue (random-real 1.0))
	  (clim-test:change-gadget-value saturation (random-real 1.0)))))))

  
;; graphics-editor

(clim-test:define-frame-test test-graphics-editor (clim-graphics-editor::graphics-editor :width 800 :height 600)
  (
   ;;-- These work only because of a timing error.
   ;;-- They should be press, move, release.

   (:press clim-graphics-editor::display 30 30)
   (:release clim-graphics-editor::display 70 70)

   (clim-graphics-editor::com-clear)

   (:press clim-graphics-editor::display 30 30)
   (:release clim-graphics-editor::display 70 70)


   (:press clim-graphics-editor::display 100 100)
   (:release clim-graphics-editor::display 150 200)

   (:press clim-graphics-editor::display 300 100)
   (:release clim-graphics-editor::display 400 200)

   (clim-graphics-editor::com-redisplay)

   (:presentation-click clim-graphics-editor::display
			clim-graphics-editor::box)
   (:presentation-click clim-graphics-editor::display
			clim-graphics-editor::box)
   (:presentation-click clim-graphics-editor::display
			clim-graphics-editor::box)

   ;; try moving one

   (:presentation-press clim-graphics-editor::display
			clim-graphics-editor::box
			:gesture :describe)

   (:release clim-graphics-editor::display 200 200)
   (clim-graphics-editor::com-deselect-object)

   (:presentation-click clim-graphics-editor::display
			clim-graphics-editor::box
			:gesture :delete)

   (:presentation-click clim-graphics-editor::display
			clim-graphics-editor::box
			:gesture :delete)

   )
  (clim-graphics-editor::com-quit)
  )

;; japanese-graphics-editor

(excl:ics-target-case
(:+ics

(clim-test:define-frame-test test-japanese-graphics-editor (japanese-graphics-editor::graphics-editor :width 800 :height 600)
  (
   ;;-- These work only because of a timing error.
   ;;-- They should be press, move, release.

   (:press japanese-graphics-editor::display 30 30)
   (:release japanese-graphics-editor::display 70 70)

   (japanese-graphics-editor::com-clear)

   (:press japanese-graphics-editor::display 30 30)
   (:release japanese-graphics-editor::display 70 70)


   (:press japanese-graphics-editor::display 100 100)
   (:release japanese-graphics-editor::display 150 200)

   (:press japanese-graphics-editor::display 300 100)
   (:release japanese-graphics-editor::display 400 200)

   (japanese-graphics-editor::com-redisplay)

   (:presentation-click japanese-graphics-editor::display
			japanese-graphics-editor::box)
   (:presentation-click japanese-graphics-editor::display
			japanese-graphics-editor::box)
   (:presentation-click japanese-graphics-editor::display
			japanese-graphics-editor::box)

   ;; try moving one

   (:presentation-press japanese-graphics-editor::display
			japanese-graphics-editor::box
			:gesture :describe)

   (:release japanese-graphics-editor::display 200 200)
   (japanese-graphics-editor::com-deselect-object)

   (:presentation-click japanese-graphics-editor::display
			japanese-graphics-editor::box
			:gesture :delete)

   (:presentation-click japanese-graphics-editor::display
			japanese-graphics-editor::box
			:gesture :delete)

   )
  (japanese-graphics-editor::com-quit)
  )

)) ;; ics-target-case


;; ico

(clim-test:define-frame-test test-ico (clim-demo::ico-frame :width 950 :height 400)
  (
   (clim-demo::com-ico-throw-ball)
   (:sleep 30)
   (clim-demo::com-ico-catch-ball)
   (:edit-avv clim-demo::options "Choose" (:faces))
   (clim-demo::com-ico-throw-ball)
   (:sleep 15)
   (clim-demo::com-ico-catch-ball)

   (:edit-avv clim-demo::options "Buffering" :double)
   (clim-demo::com-ico-throw-ball)
   (:sleep 15)
   (clim-demo::com-ico-catch-ball)
   )
  (clim-demo::com-ico-quit)
  )


;; browser

(clim-test:define-frame-test test-graphical-browser (clim-browser::browser :width 900 :height 900)
  ((:commands do-graphical-browser-test))
  (clim-browser::com-quit-browser))

(defun do-graphical-browser-test (inv)
  (clim-test:change-query-value 'clim-browser::type :class 'clim-browser::control-panel)
  (clim-test:change-query-value 'clim-browser::subtype :subclasses 'clim-browser::control-panel)
  (clim-test:change-query-value 'clim-browser::depth 2 'clim-browser::control-panel)
  (clim-test:execute-one-command inv `(clim-browser::com-show-graph ,(find-class 'sheet)))

  ;; Build the graph up
  (labels ((leaf-node-presentation-p (presentation)
	     (not
	      (clim-browser::node-inferiors
	       (presentation-object presentation))))
	   (root-node-presentation-p (presentation)
	     (not
	      (clim-browser::node-superiors
	       (presentation-object presentation))))
	   (interior-node-presentation-p (presentation)
	     (not (or (root-node-presentation-p presentation)
		      (leaf-node-presentation-p presentation)))))

    (dotimes (i 10)
      (clim-test:click-on-presentation 'clim-browser::graph 'clim-browser::call-node
			     :test #'leaf-node-presentation-p))

    (handler-case (dotimes (i 4)
		    (clim-test:click-on-presentation 'clim-browser::graph
					   'clim-browser::call-node :gesture :describe
					   :test #'leaf-node-presentation-p))
      (clim-test:cannot-find-presentation-error (c)
	c))

    (handler-case (dotimes (i 4)
		    (clim-test:click-on-presentation 'clim-browser::graph
					   'clim-browser::call-node
					   :test #'interior-node-presentation-p
					   :gesture :delete))
      (clim-test:cannot-find-presentation-error (c)
	c))

    (clim-test:execute-one-command inv `(clim-browser::com-redisplay))
    (clim-test:execute-one-command inv `(clim-browser::com-redisplay))))
  ;;


;; peek-frame

(clim-test:define-frame-test test-peek-frame (peek-frame :width 800 :height 400)
  (
   (:sleep 30)
   )
  (com-exit-peek)
  )

;; process-browser

(clim-test:define-frame-test test-process-browser (clim-demo::process-browser :width 800 :height 400)
  (
   (:sleep 30)
   )
  (clim-demo::com-quit-process-browser)
  )


;; customer-records

(clim-test:define-frame-test test-custom-records (scigraph :width 600 :height 500)
  ((com-example-1)
   (com-example-2)
   (com-example-3)
   (:sleep 5)
   (com-clear-display))
  (com-exit-scigraph))
;; demo-acivity


(clim-test:define-frame-test test-bitmap-editor (clim-demo::bitmap-editor :width 700 :height 500)
  (
   (:presentation-click clim-demo::edit-pane
			clim-demo::bitmap-editor-cell)
   (:edit-avv clim-demo::palette clim-demo::colors #.+white+)
   (:presentation-click clim-demo::edit-pane
			clim-demo::bitmap-editor-cell)
   (:edit-avv clim-demo::palette clim-demo::colors #.+black+)
   (:presentation-click clim-demo::edit-pane
			clim-demo::bitmap-editor-cell)

   )
  (clim-demo::com-bitmap-editor-quit))



(pushnew 'run-all-demos clim-test:*frame-tests*)


(defun run-all-demos (&optional (errorp clim-test:*catch-errors-in-tests*))
  (dolist (demo clim-demo::*demos*)
    (flet ((doit ()
	     (mp::with-timeout (20)
	       (clim-demo::run-demo demo :force t))))
      (if errorp
	  (handler-case (doit)
	    (error (c)
	      (clim-test:note-test-failed (clim-demo::demo-class demo) c))
	    (:no-error (&rest ignore)
	      (declare (ignore ignore))
	      (clim-test:note-test-succeeded (clim-demo::demo-class demo))))
	(doit)))))
