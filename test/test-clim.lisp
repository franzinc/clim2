;; -*- mode: common-lisp; package: clim-user -*-
;;
;;				-[]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1992 Franz Inc, Berkeley, CA  All rights reserved.
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
;; $fiHeader: test-clim.lisp,v 1.20 93/02/08 15:57:09 cer Exp $


(in-package :clim-user)

;;; Testing stuff

(define-frame-test run-clim-tests (clim-tests :width 600 :height 600)
  (
   (:commands clim-tests-commands)
   )
   (exit-clim-tests))

(define-frame-test run-clim-tests-with-r-tree (clim-tests 
					       :width 600 :height 600  
					       :history-class r-tree-output-history)
  (
   (:commands clim-tests-commands)
   )
  (exit-clim-tests))

(defun do-avv-test (inv)
  (execute-one-command inv '(com-graphics-dialog))
  (wait-for-clim-input-state inv)
  (clim-utils:letf-globally (((slot-value inv 'avv-frame) (get-avv-frame inv)))
    (dotimes (i 10)
      (dolist (cmd '(("Draw / diagonal" nil)
		     ("Draw / diagonal" t)
		     ("Color" :red)
		     ("Color" :green)))
	(apply 'change-query-value inv cmd)
	(wait-for-clim-input-state inv)))
    (execute-one-command inv :abort)
    ))

(define-command-sequence clim-tests-commands
    (com-draw-some-bezier-curves)
    (:commands do-avv-test)
  (com-input-editor-tests)
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
   
  (com-drag-and-drop-tests)
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
  :abort

  (com-simple-redisplay)   
  (:presentation-click display-pane integer)
  (:presentation-click display-pane integer)
  (:presentation-click display-pane integer)
  (:presentation-click display-pane integer)
  :abort
  
  (com-read-image-test)
   
  (com-filled-output)
   
  ;; This would be nice but moving to the top right of the
  ;; presentation does not work when they are circular!
  ;; (com-ordering-test-1)
  ;; (:presentation-click display-pane integer)


  (:presentation-click display-pane string)
   
  (com-ordering-test-2a)
  (:presentation-click display-pane integer)
  (com-ordering-test-2b)
  (:presentation-click display-pane integer)
   
  (com-text-formatting)

  (com-negative-extent)
  (com-more-simple-menus)
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

(define-frame-test test-test-frame (test-frame :width 600 :height 600)
  ((com-clear)
   (com-make-table) 
   (com-make-table) 
   (com-square-it 5)
   (com-make-one (:push-button)))
  (com-quit))

(define-frame-test test-test-frame0 (test-frame0)
  ((com-clear)
   (com-make-table) 
   (com-make-table) 
   (com-square-it 5)
   (com-make-one (:push-button)))
  (com-quit))

(define-frame-test test-test-frame5 (test-frame5)
  ((com-clear)
   (com-make-table) 
   (com-make-table) 
   (com-square-it 5)
   (com-make-one (:push-button))
   (com-switch)
   (com-switch))
  (com-quit))

(define-frame-test test-tf99 (tf99)
  ()
  (com-quit))

(define-frame-test test-tf98 (tf98 :width 600 :height 600)
  ()
  (com-quit))

(define-frame-test test-tf96 (tf96)
  ((com-clear)
   (com-make-table) 
   (com-make-table) 
   (com-square-it 5)
   (com-make-one (:push-button)))
  (com-quit))

(define-frame-test test-tf96-2 (tf96)
  ((com-clear)
   (com-make-table) 
   (com-make-table) 
   (com-square-it 5)
   (com-make-one (:push-button))
   (com-frob-sizes)
   (com-frob-sizes)
   (com-frob-sizes)
   (com-frob-sizes))
  (com-quit))

(define-frame-test test-tf95 (tf95)
  ((com-clear)
   (com-make-table) 
   (com-make-table) 
   (com-square-it 5)
   (com-make-one (:push-button)))
  (com-quit))

(define-frame-test test-tf94 (tf94)
  ((com-clear)
   (com-make-table) 
   (com-make-table) 
   (com-square-it 5)
   (com-make-one (:push-button)))
  (com-quit))

(define-frame-test test-tf93 (tf93)
  ((com-clear)
   (com-make-table) 
   (com-make-table) 
   (com-square-it 5)
   (com-make-one (:push-button)))
  (com-quit))

(define-frame-test test-tf91 (tf91)
  ((com-clear)
   (com-make-table) 
   (com-make-table) 
   (com-square-it 5)
   (com-make-one (:push-button)))
  (com-quit))

(define-frame-test test-tf92 (tf92)
  ()
  (com-quit))


(define-frame-test test-tf101 (tf101)
  ((com-make-table)
   (com-switch)
   (com-switch)
   (com-switch))
  (com-quit))

(define-frame-test test-tf100 (tf100)
  ()
  (com-quit))

(define-frame-test test-tf107 (tf107)
  ()
  (com-quit))

(define-frame-test test-tf107 (tf108)
  ()
  (com-quit))

(defun run-postscript-tests (&key (output :view))
  (exercise-frame 'clim-postscript-tests
		  '(:width 600 :height 600)
		  (mapcar #'(lambda (command)
			      (append command `(:output ,output)))
			  '((pcom-test-set-1)
			    (pcom-partial-circle 0 255)
			    (pcom-show-dash-patterns-some-more)
			    (pcom-show-dash-patterns)
			    (pcom-test-with-room-for-graphics)
			    (pcom-test-record-and-replay)
			    (pcom-test-transformations)
			    (pcom-test-text-alignment "Ignatz")
			    (pcom-test-text-baselines)
			    (pcom-test-text-sizes)
			    (pcom-test-text-size)
			    (pcom-test-table)
			    (pcom-test-text-vertical-alignment)
			    (pcom-test-character-positioning)
			    (pcom-write-multiple-strings)
			    (pcom-test-alignment)
			    (pcom-test-graphics)
			    (pcom-test-ellipse)
			    (pcom-draw-line-test)
			    (pcom-pattern-test)))
		  '(com-exit-clim-postscript-tests)))
;;;

(define-application-frame frame-test ()
			  ()
  (:panes
   (display :interactor))
  (:layouts
   (default display)))

(define-frame-test-command com-frame-test-hello
    ()
  (with-text-size (t :huge)
    (write-line "Hello")))

(define-frame-test-command com-frame-test-change-name
    ()
  (setf (frame-pretty-name *application-frame*) "Sonic Rules"))

(define-frame-test-command com-frame-test-display-dialogs
    ()
  (mp::with-timeout (3) (select-file *application-frame*))
  (dolist (style '(:inform :error :question :warning))
    (mp::with-timeout (3)
      (notify-user *application-frame* 
		   "Just say no to sega games"
		   :title (format nil "The style is ~A" style)
		   :style style))))

(define-frame-test-command com-frame-test-bye
    ()
  (with-text-size (t :huge)
    (write-line "Bye")))

(define-frame-test-command com-frame-test-quit
    ()
  (frame-exit *application-frame*))

(define-frame-test-command com-frame-test-raise
    ()
  (raise-frame *application-frame*))

(define-frame-test-command com-frame-test-bury
    ()
  (bury-frame *application-frame*))

(define-frame-test-command com-frame-test-iconify
    ()
  (shrink-frame *application-frame*))

(define-frame-test-command com-frame-test-deiconify
    ()
  (enable-frame *application-frame*))

(define-frame-test test-frame-test (frame-test :width 400 :height 400)
  ((com-frame-test-hello)
   (:sleep 2)
   (com-frame-test-change-name)
   (:sleep 2)
   (com-frame-test-display-dialogs)
   (:sleep 2)
   (com-frame-test-bury)
   (:sleep 3)
   (com-frame-test-raise)
   (:sleep 3)
   (com-frame-test-iconify)
   (:sleep 2)
   (com-frame-test-deiconify)
   (:sleep 2)
   (com-frame-test-bye)
   )
  (com-frame-test-quit))

;;; make the training selective.

(locally 
  (declare (special si::*clos-preload-packages*))
 (setq si::*clos-preload-packages* 
   (mapcar #'find-package '(:clim :clim-internals :silica :tk :xm-silica))))

;; This stops warnings happening asynchronously and causing confusion.

(setq excl:*global-gc-behavior* nil)



