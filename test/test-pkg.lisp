
(in-package :user)

(defpackage :clim-test
  (:use :clim-lisp :clim)
  (:export
   #:*frame-tests*
   #:*catch-errors-in-tests*
   #:abort-menu
   #:activate-push-button
   #:benchmark-clim
   #:cannot-find-presentation-error
   #:change-gadget-value
   #:change-query-value
   #:click-on-presentation
   #:click-on-window
   #:define-activity-test
   #:define-command-sequence
   #:define-frame-test
   #:do-frame-test-with-profiling
   #:do-frame-tests
   #:execute-one-command
   #:exercise-frame
   #:find-frame
   #:find-menu
   #:find-notify-user
   #:find-user-interface-components
   #:get-avv-frame
   #:invoke-accept-values-button
   #:note-test-failed 
   #:note-test-succeeded
   #:notify-user-ok
   #:press-on-window
   #:release-on-window
   #:select-menu-item
   #:test-it
   #:train-clim-2
   #:wait-for-clim-input-state
   #:wait-for-frame-to-exit
   #:with-frame-invocation
   #:with-test-reporting
   #:with-test-success-expected
   #:with-waiting
   ))

#+ignore
(defun fixit ()
  (interactive)
  (beginning-of-buffer)
  (query-replace "*frame-tests*" "clim-test:*frame-tests*")
  (beginning-of-buffer)
  (query-replace "abort-menu" "clim-test:abort-menu")
  (beginning-of-buffer)
  (query-replace "activate-push-button" "clim-test:activate-push-button")
  (beginning-of-buffer)
  (query-replace "benchmark-clim" "clim-test:benchmark-clim")
  (beginning-of-buffer)
  (query-replace "cannot-find-presentation-error" "clim-test:cannot-find-presentation-error")
  (beginning-of-buffer)
  (query-replace "change-gadget-value" "clim-test:change-gadget-value")
  (beginning-of-buffer)
  (query-replace "change-query-value" "clim-test:change-query-value")
  (beginning-of-buffer)
  (query-replace "click-on-presentation" "clim-test:click-on-presentation")
  (beginning-of-buffer)
  (query-replace "click-on-window" "clim-test:click-on-window")
  (beginning-of-buffer)
  (query-replace "define-activity-test" "clim-test:define-activity-test")
  (beginning-of-buffer)
  (query-replace "define-command-sequence" "clim-test:define-command-sequence")
  (beginning-of-buffer)
  (query-replace "define-frame-test" "clim-test:define-frame-test")
  (beginning-of-buffer)
  (query-replace "do-frame-test-with-profiling" "clim-test:do-frame-test-with-profiling")
  (beginning-of-buffer)
  (query-replace "do-frame-tests" "clim-test:do-frame-tests")
  (beginning-of-buffer)
  (query-replace "execute-one-command" "clim-test:execute-one-command")
  (beginning-of-buffer)
  (query-replace "exercise-frame" "clim-test:exercise-frame")
  (beginning-of-buffer)
  (query-replace "find-frame" "clim-test:find-frame")
  (beginning-of-buffer)
  (query-replace "find-menu" "clim-test:find-menu")
  (beginning-of-buffer)
  (query-replace "find-notify-user" "clim-test:find-notify-user")
  (beginning-of-buffer)
  (query-replace "find-user-interface-components" "clim-test:find-user-interface-components")
  (beginning-of-buffer)
  (query-replace "get-avv-frame" "clim-test:get-avv-frame")
  (beginning-of-buffer)
  (query-replace "invoke-accept-values-button" "clim-test:invoke-accept-values-button")
  (beginning-of-buffer)
  (query-replace "note-test-failed " "clim-test:note-test-failed ")
  (beginning-of-buffer)
  (query-replace "note-test-succeeded" "clim-test:note-test-succeeded")
  (beginning-of-buffer)
  (query-replace "notify-user-ok" "clim-test:notify-user-ok")
  (beginning-of-buffer)
  (query-replace "press-on-window" "clim-test:press-on-window")
  (beginning-of-buffer)
  (query-replace "release-on-window" "clim-test:release-on-window")
  (beginning-of-buffer)
  (query-replace "select-menu-item" "clim-test:select-menu-item")
  (beginning-of-buffer)
  (query-replace "test-it" "clim-test:test-it")
  (beginning-of-buffer)
  (query-replace "train-clim-2" "clim-test:train-clim-2")
  (beginning-of-buffer)
  (query-replace "wait-for-clim-input-state" "clim-test:wait-for-clim-input-state")
  (beginning-of-buffer)
  (query-replace "wait-for-frame-to-exit" "clim-test:wait-for-frame-to-exit")
  (beginning-of-buffer)
  (query-replace "with-frame-invocation" "clim-test:with-frame-invocation")
  (beginning-of-buffer)
  (query-replace "with-test-reporting" "clim-test:with-test-reporting")
  (beginning-of-buffer)
  (query-replace "with-test-success-expected" "clim-test:with-test-success-expected")
  (beginning-of-buffer)
  (query-replace "with-waiting" "clim-test:with-waiting")
  )
