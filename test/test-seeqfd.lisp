(in-package :clim-user)

(defun test-seeqfd ()
  (mp::process-run-function "Startup" 'sq::seeqfd-startup)
  (test-seeqfd-1))

(defun test-seeqfd-1 ()
  (test-startup-frame
   (wait-for-frame :type 'sq::STARTUP-FRAME :timeout 180))
  (test-select-notebook 
   (wait-for-frame :type 'sq::DATABASE-CHOOSER :timeout 180)))

(defun test-startup-frame (frame)
  ;; Perhaps we need to wait for the process to rest
  ;; Should we verify that the user interface composition is correct
  ;; -- ie. an application pane and a button
  ;; Should we verify that the text is the same?
  ;; It would be good to press the button to make the frame go away.
  ;; frame-exit?
  (print (list frame (clim-internals::frame-top-level-process frame))
	 excl:*initial-terminal-io*)
  (with-frame-invocation (frame)
    (press-push-button (find-user-interface-components frame '(push-button)))
    (wait-for-frame-to-exit frame)))


(defun test-select-notebook (frame)
  (with-frame-invocation (frame)
    ;;-- Perhaps we should be waving the mouse around
    ;;-- Browsing around the directory hierarchy
    ;;-- Select a notebook
    ;;-- Hit the load button
    ;;-- Expect this frame to disappear and a notebook 
    ;;-- Wait for a product plan frame
    (let ((subdirectories (get-frame-pane frame 'sq::subdirectories))
	  (databases (get-frame-pane frame 'sq::databases)))
      (click-on-presentation subdirectories
			     'clim-lisp::pathname
			     :x-offset 15 :y-offset 5
			     :test #'(lambda (presentation)
				       (member  "example-dbs"
						(pathname-directory (presentation-object presentation))
						:test #'equal)))
      (click-on-presentation databases t :x-offset 15 :y-offset 5)
      (flet ((open-button-p  (gadget)
	       (and (typep gadget 'push-button)
		    (equal (gadget-label gadget) "Open"))))
	(press-push-button (find-user-interface-components frame
							   (list
							    #'open-button-p))))
      ;;-- We probably dont want to wait for an input-state after
      ;;-- pressing the push button
      
      ;; We might get some warning dialogs at this point
      ;; 
      
      (with-waiting ()
	((find-frame :type 'sq::PRODUCT-PLAN-FRAME) (frame) 
						    nil)
	((find-notify-user frame) (stuff)
				  (sleep 5)
				  (notify-user-ok frame)
				  (with-waiting ()
				    ((find-notify-user frame) (stuff)
							      (sleep 5)
							      (notify-user-ok frame)))))
      
      (test-product-plan-frame
       (wait-for-frame :type 'sq::PRODUCT-PLAN-FRAME :timeout 300)))))


(defun test-product-plan-frame (&optional 
				(frame (wait-for-frame :type 'sq::product-plan-frame :timeout 30)))
  (with-frame-invocation (frame)
    ;;-- wave the mouse around but dont click on anything
    (click-on-presentation 'sq::display-pane t  :press nil :release nil)
    ;; 
    (click-on-presentation 'sq::display-pane 'sq::house-map-grid :x-offset 20 :y-offset 20 :press t)
    ;; 
    ))
