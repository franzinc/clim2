(in-package :clim-user)

;; $fiHeader: test-demos.lisp,v 1.1 93/03/19 09:46:37 cer Exp $

(define-frame-test test-puzzle-demo (clim-demo::puzzle)
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

(define-frame-test test-flight-planner (clim-demo::flight-planner :width 1000 :height 800)
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


(define-frame-test test-flight-planner-2 (clim-demo::flight-planner :width 1000 :height 800)
  (
   (:command clim-demo::display clim-demo::com-describe-object)
   )
  (clim-demo::com-exit-flight-planner)
  )

;; Listener

;;;--- The listener is using a nonstandard top level and does not
;;;---- seemed to be looking at the queue

(define-frame-test test-lisp-listener (clim-demo::lisp-listener :width 1000 :height 800)
  (
   "(cons nil nil)
"
   (:command clim-demo::interactor clim-demo::com-clear-output-history
	     :colon-prefix t)
   (:command clim-demo::interactor clim-demo::com-show-some-commands
	     :colon-prefix t)
   )
  (:command clim-demo::interactor clim-demo::com-quit-listener 
	    :colon-prefix t)
  )

;; graphics-demos

(define-frame-test test-graphics-demos (clim-demo::graphics-demo :width 840 :height 800)
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

;; cad-demo

;; address-book

(define-frame-test test-address-book (clim-demo::address-book :width 400 :height 400)
  (
   (:presentation-click clim-demo::names clim-demo::address)
   (:presentation-click clim-demo::names clim-demo::address)
   (:presentation-click clim-demo::names clim-demo::address)
   )
  (clim-demo::com-quit-address-book)
  )

;; Thinkadot
;; plot
;; color-editor
;; graphics-editor

(define-frame-test test-graphics-editor (clim-graphics-editor::graphics-editor :width 800 :height 600)
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

;; bitmap-editor
;; ico

(define-frame-test test-ico (clim-demo::ico-frame :width 400 :height 400)
  (
   (clim-demo::com-ico-throw-ball)
   (:sleep 30)
   (clim-demo::com-ico-catch-ball)
   )
  (clim-demo::com-ico-quit)
  )

;; browser
;; peek-frame
;; process-browser

(define-frame-test test-process-browser (clim-demo::process-browser :width 800 :height 400)
  (
   (:sleep 30)
   )
  (clim-demo::com-quit-process-browser)
  )

;; customer-records
;; demo-acivity

(define-frame-test test-bitmap-editor (clim-demo::bitmap-editor :width 700 :height 500)
  (
   (:presentation-click clim-demo::edit-pane
			clim-demo::bitmap-editor-cell)
   (:edit-avv clim-demo::palette "Colors" #.+white+)
   (:presentation-click clim-demo::edit-pane
			clim-demo::bitmap-editor-cell)
   (:edit-avv clim-demo::palette "Colors" #.+black+)
   (:presentation-click clim-demo::edit-pane
			clim-demo::bitmap-editor-cell)
   
   )
  (clim-demo::com-bitmap-editor-quit))

(pushnew 'run-all-demos *frame-tests*)

(defun run-all-demos ()
  (dolist (demo clim-demo::*demos*)
    (let ((fn (cdr demo)))
      (unless (functionp fn)
	(handler-case (mp::with-timeout (20)
			(funcall fn :force t))
	  (error (c)
	    (note-test-failed fn c))
	  (:no-error (&rest ignore)
	    (declare (ignore ignore))
	    (note-test-succeeded fn)))))))
