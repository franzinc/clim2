
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

