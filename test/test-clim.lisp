;; -*- mode: common-lisp; package: clim-user -*-
;;
;;
;; See the file LICENSE for the full license governing this code.
;;


(in-package :clim-user)

(clim-test:define-frame-test test-test-frame (test-frame :width 600 :height 600)
  ((com-clear)
   (com-make-table)
   (com-make-table)
   (com-square-it 5)
   (com-make-one (:push-button)))
  (com-quit))

(clim-test:define-frame-test test-test-frame0 (test-frame0)
  ((com-clear)
   (com-make-table)
   (com-make-table)
   (com-square-it 5)
   (com-make-one (:push-button)))
  (com-quit))

(clim-test:define-frame-test test-test-frame5 (test-frame5)
  ((com-clear)
   (com-make-table)
   (com-make-table)
   (com-square-it 5)
   (com-make-one (:push-button))
   (com-switch)
   (com-switch))
  (com-quit))

(clim-test:define-frame-test test-tf0 (tf0)
  ()
  (com-quit))

(clim-test:define-frame-test test-tf99 (tf99)
  ()
  (com-quit))

(clim-test:define-frame-test test-tf98 (tf98 :width 600 :height 600)
  ()
  (com-quit))

(clim-test:define-frame-test test-tf96 (tf96)
  ((com-clear)
   (com-make-table)
   (com-make-table)
   (com-square-it 5)
   (com-make-one (:push-button)))
  (com-quit))

(clim-test:define-frame-test test-tf96-2 (tf96)
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

(clim-test:define-frame-test test-tf95 (tf95)
  ((com-clear)
   (com-make-table)
   (com-make-table)
   (com-square-it 5)
   (com-make-one (:push-button)))
  (com-quit))

(clim-test:define-frame-test test-tf94 (tf94)
  ((com-clear)
   (com-make-table)
   (com-make-table)
   (com-square-it 5)
   (com-make-one (:push-button)))
  (com-quit))

(clim-test:define-frame-test test-tf93 (tf93)
  ((com-clear)
   (com-make-table)
   (com-make-table)
   (com-square-it 5)
   (com-make-one (:push-button)))
  (com-quit))

(clim-test:define-frame-test test-tf91 (tf91)
  ((com-clear)
   (com-make-table)
   (com-make-table)
   (com-square-it 5)
   (com-make-one (:push-button)))
  (com-quit))

(clim-test:define-frame-test test-tf92 (tf92)
  ()
  (com-quit))


(clim-test:define-frame-test test-tf101 (tf101)
  ((com-make-table)
   (com-switch)
   (com-switch)
   (com-switch))
  (com-quit))

(clim-test:define-frame-test test-tf100 (tf100)
  ()
  (com-quit))

(clim-test:define-frame-test test-tf107 (tf107)
  ()
  (com-quit))

(clim-test:define-frame-test test-tf108 (tf108)
  ()
  (com-quit))

(defun run-postscript-tests (&key (output :view))
  (run-printer-tests output :postscript))

(defun run-printer-tests (output printer-type)
  (clim-test:exercise-frame (clim-utils:fintern "~A~A" 'printer-tests printer-type)
		  'clim-postscript-tests
		  `(:width 600 :height 600 :printer-type ,printer-type)
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
    (mp:with-timeout (3)
      (notify-user *application-frame*
		   "Just say no to sega games"
		   :title (format nil "The style is ~A" style)
		   :style style)))
  (mp:with-timeout (3)
    (select-file *application-frame*
		 :pattern "Makefile*"
		 :documentation "Find Makefiles"
		 :text-style '(:fix :roman :huge))))

(define-frame-test-command com-frame-test-display-progress-note
    ()
  (dotimes-noting-progress (i 10 "fred")
    (sleep 1)))

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

(define-frame-test-command com-frame-test-move
    ()
  (let ((x (random 200))
	(y (random 200)))
    (position-sheet-carefully (frame-top-level-sheet *application-frame*) x y)
    (multiple-value-bind (nx ny)
	(tk::get-values (silica:frame-shell *application-frame*) :x :y)
      ;; this tends to fail because the window decoration appears to
      ;; be taken into consideration in the position-sheet-carefully
      (assert (and (= x nx) (= y ny))))))

(define-frame-test-command com-frame-test-bury
    ()
  (bury-frame *application-frame*))

(define-frame-test-command com-frame-test-iconify
    ()
  (shrink-frame *application-frame*))

(define-frame-test-command com-frame-test-deiconify
    ()
  (enable-frame *application-frame*))

(clim-test:define-frame-test test-frame-test (frame-test :width 400 :height 400)
  ((com-frame-test-hello)
   (:sleep 2)
   (com-frame-test-change-name)
   (:sleep 2)
   (com-frame-test-display-dialogs)
   (:sleep 2)
   (com-frame-test-display-progress-note)
   (:sleep 2)
   (com-frame-test-bury)
   (:sleep 3)
   (com-frame-test-raise)
   (:sleep 3)
   (com-frame-test-move)
   (:sleep 1)
   (com-frame-test-move)
   (:sleep 1)
   (com-frame-test-move)
   (:sleep 1)
   (com-frame-test-iconify)
   (:sleep 2)
   (com-frame-test-deiconify)
   (:sleep 2)
   (com-frame-test-bye)
   )
  (com-frame-test-quit))

(clim-test:define-frame-test test-tf109 (tf109)
  ((com-change-set-gadget-items)
   (com-change-set-gadget-values)
   (com-change-set-gadget-items :which t)
   (com-change-set-gadget-values :which t)
   (com-change-set-gadget-items)
   (com-change-set-gadget-values))
  (com-quit))


(clim-test:define-frame-test test-tf111 (tf111)
  ()
  (com-quit))

(clim-test:define-frame-test test-tf112 (tf112)
  ()
  (com-quit))

;;; Disable/enable command stuff.

(define-application-frame enable-disable-frame ()

  ()
  (:panes
   (i :interactor))
  (:layouts
   (default i)))

(define-enable-disable-frame-command (com-enable-disable-foo :name nil :menu t)
    ((i 'integer :gesture :select))
  (present (* i i) 'integer))

(define-enable-disable-frame-command (com-enable-disable-bar :name nil :menu t)
    ((i 'integer :gesture :describe)
     (j 'integer))
  (present (* i j) 'integer))

(define-enable-disable-frame-command (com-enable-disable-quit :name nil :menu t)
    ()
  (frame-exit *application-frame*))

(define-enable-disable-frame-command (com-enable-disable-sensitive :name t)
    ((enabled 'boolean))
  (setf (command-enabled 'com-enable-disable-frame *application-frame*) enabled))

(clim-test:define-frame-test test-enable-disable-command (enable-disable-frame
						:disabled-commands '(com-enable-disable-frame)
						:width 400 :height 400)
  ((com-enable-disable-sensitive t)
   (:sleep 1)
   (com-enable-disable-sensitive nil)
   (:sleep 1))
  (com-enable-disable-quit))

(push 'find-frame-manager-test clim-test:*frame-tests*)

(defun find-frame-manager-test ()
  (clim-test:with-test-success-expected ('find-frame-manager-test)
    (let ((fm (find-frame-manager)))
      (with-frame-manager (fm)
	(let ((*default-server-path*
	       `(,(if (excl::featurep :clim-motif)
		      :motif :openlook)
		    :display "mysparc10:0")))
	  (assert (eq fm (find-frame-manager))))))))

;;

(push 'open-window-stream-test clim-test:*frame-tests*)

;; Stewart introduced bugs here......................

(defun open-window-stream-test ()
  (clim-test:with-test-success-expected ('open-window-stream-test)
    (let* (stream1 stream2 stream3)
      (setq stream1 (open-window-stream :label "Fred" :scroll-bars :vertical))
      (window-expose stream1)
      (setq stream2 (open-window-stream :label "Joh" :scroll-bars :vertical :parent stream1))
      (window-expose stream2)
      (setq stream3 (open-window-stream :label "Joh" :scroll-bars :vertical :left 500 :top 500))
      (window-expose stream3)
      (sleep 10)
      (setf (window-visibility stream1) nil
	    (window-visibility stream2) nil
	    (window-visibility stream3) nil))))


;;

(push 'multiple-value-setf-test clim-test:*frame-tests*)

(defun multiple-value-setf-test ()
  (clim-test:with-test-success-expected ('multiple-value-setf-test)
    (eval '(let ((x (make-bounding-rectangle 0 0 10 10)))
	    (setf (bounding-rectangle* x)
	      (values 12 12 13 14))))))

;;

(push 'define-presentation-type-with-history-test clim-test:*frame-tests*)

(defun define-presentation-type-with-history-test ()
  (clim-test:with-test-success-expected ('define-presentation-type-with-history-test)
    (eval `(define-presentation-type ,(gensym) nil :history t :inherit-from '((string))))))
;;


(push 'filling-output-on-plain-stream-test clim-test:*frame-tests*)

(defun filling-output-on-plain-stream-test ()
  (clim-test:with-test-success-expected ('filling-output-on-plain-stream-test)
    (filling-output (*standard-output* :fill-width '(20 :character))
      (write-string *gettysburg-address* *standard-output*))))

(push 'filling-output-on-string-stream-test clim-test:*frame-tests*)

(defun filling-output-on-string-stream-test ()
  (clim-test:with-test-success-expected ('filling-output-on-string-stream-test)
    (with-output-to-string (stream)
      (filling-output (stream :fill-width '(20 :character))
	(write-string *gettysburg-address* stream)))))

;;--- This would be nice but the problem occurs in the event handler process
;;----

#+ignore
(push 'create-multiple-ports clim-test:*frame-tests*)

#+ignore
(defun create-multiple-ports ()
  (clim-test:with-test-success-expected ('create-multiple-ports)
    (let (port2)
      (mp::with-timeout (30)
	(clim-demo::start-demo :port (setq port2
				       (find-port :server-path (list (car *default-server-path*) :application-name "climx")))))
      (mp::with-timeout (30)
	(clim-demo::start-demo))
      (mp::with-timeout (30)
	(clim-demo::start-demo :port port2)))))

;;

(define-frame-test-command com-multi-colored-button ()
  ()
  (let  ((gadget
	  (with-output-as-gadget (t)
	    (make-pane 'push-button :label "hello"))))
    (sleep 1)
    (setf (pane-background gadget) +red+)
    (sleep 1)
    (setf (pane-foreground gadget) +green+)
    (sleep 1)
    (setf (pane-text-style gadget) '(:fix :roman 10))
    (sleep 1)))

(clim-test:define-frame-test test-com-multi-colored-button (frame-test :width 400 :height 400)
  ((com-multi-colored-button))
  (com-frame-test-quit))

(clim-test:define-frame-test  test-tf108-selection (tf108)
  ((com-test-text-selection)
   (com-test-text-editable))
  (com-quit))

(clim-test:define-frame-test test-box-update (tf114)
  ((com-update-boxes)
   (com-update-boxes))
  (com-quit))


(push 'accept-from-string-tests clim-test:*frame-tests*)

(defun accept-from-string-tests ()
  (clim-test:with-test-success-expected ('accept-from-string-tests)

    (assert (eq nil (accept-from-string '(subset :a :b) "")))
    (assert (equal '(:a :b) (accept-from-string '(subset :a :b) "a,b")))
    (assert (eq :abc (accept-from-string 'keyword "abc")))
    (handler-case (accept-from-string '(member :a :b) "")
      (error (c) c)
      (:no-error (&rest ignore) ignore (error "member null string failed")))

    (handler-case (accept-from-string '(member :a :b) "z")
      (error (c) c)
      (:no-error (&rest ignore) ignore (error "member bogus failed")))

    (handler-case (accept-from-string '(subset :a :b) "a,c")
      (error (c) c)
      (:no-error (&rest ignore) ignore (error "subset bogus failed")))))


(clim-test:define-frame-test test-avv-text-fields-and-list-panes-with-scroll-bars (tf115)
  ()
  (com-quit))
