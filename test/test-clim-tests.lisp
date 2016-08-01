;; -*- mode: common-lisp; package: clim-user -*-
;;
;;
;; See the file LICENSE for the full license governing this code.
;;


(in-package :clim-user)

;;; Testing stuff

(clim-test:define-frame-test run-clim-tests (clim-tests :width 600 :height 600)
  (
   (:commands clim-tests-commands)
   )
   (exit-clim-tests))

(clim-test:define-frame-test run-clim-tests-with-r-tree (clim-tests
							 :width 600 :height 600
							 :history-class r-tree-output-history)
  (
   (:commands clim-tests-commands)
   )
  (exit-clim-tests))

(defun do-avv-test (inv)
  (clim-test:execute-one-command inv '(com-graphics-dialog))
  (clim-test:wait-for-clim-input-state inv)
  (clim-utils:letf-globally (((slot-value inv 'clim-test::avv-frame) (clim-test:get-avv-frame inv)))
    (dotimes (i 10)
      (dolist (cmd '(("Draw / diagonal" nil)
		     ("Draw / diagonal" t)
		     ("Color" :red)
		     ("Color" :green)))
	(apply 'clim-test:change-query-value cmd)))
    (clim-test:execute-one-command inv :abort)
    ))

(clim-test:define-command-sequence clim-tests-commands
    (com-draw-some-bezier-curves)
  (:commands do-avv-test)
  (com-input-editor-tests)
  "(cons"
  :ie-show-arglist
  :ie-show-documentation
  " *standard-input*"
  :ie-show-value
  :ie-show-documentation
  :ie-clear-input
  "This is (sexp) and a string"
  :ie-backward-character
  :ie-backward-character
  :ie-backward-word
  :ie-backward-word
  :ie-backward-word
  :ie-forward-word
  :ie-backward-word
  :ie-backward-sexp
  :ie-forward-sexp
  :ie-end-of-line
  :ie-beginning-of-line
  :ie-delete-word
  :ie-delete-character
  :ie-forward-word
  :ie-forward-character
  :ie-rubout-character
  :ie-rubout-character
  " love lisp"
  :ie-beginning-of-line
  :ie-upcase-word
  :ie-beginning-of-line
  :ie-downcase-word
  :ie-beginning-of-line
  :ie-kill-line
  :ie-make-|()|
  "cons (car x) (cdr x)"
  :help
  :abort

  ;;--- phew. write some more

  (com-simple-redisplay)
  (:presentation-click display-pane integer)
  (:presentation-click display-pane integer)
  (:presentation-click display-pane integer)
  (:presentation-click display-pane integer)
  :abort

  (com-draw-some-pixmap)
  )

(clim-test:define-frame-test more-simple-menu-test (clim-tests
				    :width 600 :height 600)
  ((com-more-simple-menus))
  (exit-clim-tests))


(clim-test:define-frame-test more-clim-tests (clim-tests
				    :width 600 :height 600)
  ((com-read-image-test)

  (com-filled-output)
  (:presentation-click display-pane string)

  ;; This would be nice but moving to the top right of the
  ;; presentation does not work when they are circular!
  ;; (com-ordering-test-1)
  ;; (:presentation-click display-pane integer)

  (com-ordering-test-2a)
  (:presentation-click display-pane integer)
  (com-ordering-test-2b)
  (:presentation-click display-pane integer)

  (com-text-formatting)

  (com-negative-extent)

  (com-readonly-gadget-dialog)
  :abort
  (com-ozone-dialog)
  :abort
  (com-gadgets-dialog)
  :abort
  (com-graphics-dialog)
  :abort
  (com-simple-spreadsheet)
  :abort
  (com-highlighting-tests)
  "123"
  #\return
  "56565"
  #\return
  :abort
  ;;  (com-graphics-dialog)
  ;;  (com-choose-compass-direction)
  ;;  (com-graphical-menu)
  ;;  (com-simple-menu)
  (com-string-stream-accept)
  (com-string-accept)
  (com-redisplay-graph)
  (com-redisplay-border)
  (com-redisplay-overlapping)
  (com-graphics-redisplay-2)
  (com-graphics-redisplay-1)
  ;; (com-simple-redisplay)
  ;; (com-comprehensive-table-tests)
  ;; (com-text-formatting)
  ;; (com-filled-output)
  (com-offset-graph)
  (com-clos-metaobjects-graph)
  (com-offset-table)
  (com-simple-graph)
  (com-hairy-graph-formatting)
  (com-simple-borders)
  (com-mixed-table)
  (com-checkerboard)
  (com-cell-coordinates)
  (com-nested-table)
  (com-equalized-multiple-columns-table)
  (com-multiple-columns-table)
  (com-equal-width-table)
  (com-column-table)
  (com-row-table)
  (com-cursorpos-table)
  ;; (com-ordering-test-2b)
  ;; (com-ordering-test-2a)
  ;; (com-ordering-test-1)
  (com-draw-bullseye)
  (com-basic-line-styles)
  (com-patterned-graphics-shapes)
  (com-colored-inks)
  (com-rotated-text)
  (com-pixmap-test)
  (com-points-and-lines)
  (com-basic-graphics-inks)
  (com-transformed-graphics-shapes)
  (com-basic-graphics-shapes)
  (com-region-intersects-region-tests)
  (com-region-contains-region-tests)
  (com-region-contains-point-tests)
  (com-region-equal-tests)
  (com-draw-enstyled-text)
  (com-blue-gettysburg)
  (com-styled-gettysburg)
  (com-gettysburg)
  (com-draw-some-arcs)
  (com-draw-some-points)
  (com-scaled-rotated-circles)
  (com-rotated-scaled-circles)
  (com-draw-some-circles)
  (com-scaled-rotated-rectangles)
  (com-rotated-scaled-rectangles)
  (com-draw-some-rectangles))

  (exit-clim-tests)
  )

(clim-test:define-frame-test test-drag-and-drop (clim-tests
					       :width 600 :height 600
					       :history-class r-tree-output-history
					       )
  ((com-drag-and-drop-tests)
   (:presentation-click display-pane drag-source)
   (:presentation-click display-pane drop-target)
   (:presentation-click display-pane drag-source)
   (:presentation-click display-pane drop-target)
   (:presentation-click display-pane drag-source)
   (:presentation-click display-pane drop-target)
   (:presentation-click display-pane drag-source)
   (:presentation-click display-pane drop-target)
   (:presentation-click display-pane drag-source)
   (:presentation-click display-pane drop-target)
   :abort)
  (exit-clim-tests)
  )


(defun profile-clim-tests ()
  (let ((errorp clim-test:*catch-errors-in-tests*))
    (clim-test:exercise-frame 'clim-test:test-it
		    'clim-tests
		    '(:width 600 :height 400)
		    (mapcan #'(lambda (benchmark-group)
				(mapcar #'list (cdr benchmark-group)))
			    *summary-contributions*)
		    `(exit-clim-tests)
		    :error errorp)))

(defun run-profile-clim-tests (&optional (prefix "notes/profiles"))
  (let ((prof::*hidden-packages* nil)
	(prof::*significance-threshold* 0.001)
	(prof::*fractional-significance-threshold* .002))
    (clim-test:with-test-success-expected ('run-profile-clim-tests-time)
      (clim-test:do-frame-test-with-profiling 'profile-clim-tests  :prefix prefix :type :time))
    (clim-test:with-test-success-expected ('run-profile-clim-tests-space)
      (clim-test:do-frame-test-with-profiling 'profile-clim-tests :prefix prefix
				    :type :space))))

