;; -*- mode: common-lisp; package: clim-internals -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

(defmethod default-frame-top-level ((frame standard-application-frame)
				    &key command-parser command-unparser
					 partial-command-parser
					 (prompt "Command: "))
  ;; Enable the frame now
  (unless (eq (frame-state frame) :enabled)
    (enable-frame frame))
  (loop
    (let* ((*standard-output*
	    (or (frame-standard-output frame) *standard-output*))
	   (*standard-input*
	    (or (frame-standard-input frame) *standard-input*))
	   (*query-io*
	    (or (frame-query-io frame) *query-io*))
	   (*error-output*
	    (or (frame-error-output frame) *error-output*))
	   (*pointer-documentation-output*
	    (frame-pointer-documentation-output frame))
	   (interactor
	    (not (null (find-frame-pane-of-type frame 'interactor-pane))))
	   (*command-parser*
	    (or command-parser
		(if interactor
		    #'command-line-command-parser
		  #'menu-command-parser)))
	   (*command-unparser*
	    (or command-unparser
		#'command-line-command-unparser))
	   (*partial-command-parser*
	    (or partial-command-parser
		(if interactor
		    #'command-line-read-remaining-arguments-for-partial-command
		  #'menu-read-remaining-arguments-for-partial-command)))
	   (command-stream
	    ;;--- We have to ask the frame since we do not want to
	    ;;--- just pick up a stream from the dynamic environment
	    (let ((si (or (frame-standard-input frame)
			  (frame-standard-output frame))))
	      (typecase si
		(output-protocol-mixin si)
		(t (frame-top-level-sheet frame))))))
      ;; The read-eval-print loop for applications...
      (letf-globally (((frame-actual-pointer-documentation-pane frame)
		       *pointer-documentation-output*))
	(loop
	  ;; Redisplay all the panes
	  (catch-abort-gestures ("Return to ~A command level" (frame-pretty-name frame))
	    (redisplay-frame-panes frame)
	    (when interactor
	      (fresh-line *standard-input*)
	      (if (stringp prompt)
		  (write-string prompt *standard-input*)
		(funcall prompt *standard-input* frame)))
	    (let ((command (read-frame-command frame :stream command-stream)))
	      (when interactor
		(terpri *standard-input*))
	      ;; Need this check in case the user aborted out of a command menu
	      (when command
		(execute-frame-command frame command)))))))))
