(in-package :clim-user)

(defvar *catch-errors-in-tests* t)

(defclass invocation ()
	  ((process :initform nil :accessor invocation-process)
	   (frame :initform nil :accessor invocation-frame)
	   (condition :initform nil :accessor invocation-condition)
	   (state :initform nil :accessor invocation-state)
	   (avv-frame :initform nil :accessor invocation-avv-frame)
	   ))

(defun invocation-active-p (invocation)
  (not (eq (invocation-state invocation) :dead)))

(defmethod initialize-instance :after ((inv invocation) &key class initargs)
  (labels ((do-it (frame)
	     (unwind-protect
		 (loop
		   (let ((again (catch 'try-again
				  (setf (invocation-state inv) :running)
				  (run-frame-top-level frame)
				  nil)))
		     (unless again (return nil))))
	       (setf (invocation-state inv) :dead)))
	   (run-frame-top-level-almost (frame)
	     ;; Maybe this should be a handler bind so there is the
	     ;; option of trying again as well as aborting the process
	     (if *catch-errors-in-tests*
		 (handler-case
		     (do-it frame)
		   (error (condition)
		     (handler-invocation-debugger-hook inv condition)))
	       (do-it frame))))
    (let* ((frame (apply #'make-application-frame class initargs))
	   (process (mp:process-run-function
		     (format nil "~A Test process for" class)
		     #'run-frame-top-level-almost 
		     frame)))
      (setf (invocation-process inv) process
	    (invocation-frame inv) frame)
      (wait-for-clim-input-state inv))))

(defun handler-invocation-debugger-hook (invocation condition)
  (format excl:*initial-terminal-io* "The following error occurred: ~A~%" condition))

(defvar *default-input-state-timeout* 300)
(defun wait-for-clim-input-state (invocation &optional (timeout *default-input-state-timeout*))
  (let ((process (invocation-process invocation)))
    (mp:process-allow-schedule)
    (if timeout
	(let ((done nil))
	  (mp:process-wait-with-timeout 
	   "Waiting for process to sleep"
	   timeout
	   #'(lambda () 
	       (setq done
		 (or (not (mp::process-stack-group process))
		     (string-equal (mp::process-whostate process) "CLIM Input")))))
	  (unless done
	    (error "Timed out after ~D seconds" timeout)))
      (mp:process-wait 
       "Waiting for process to sleep"
       #'(lambda () 
	   (or (not (mp::process-stack-group process))
	       (string-equal (mp::process-whostate process) "CLIM Input")))))))

(defvar *death-timeout* 30)

(defun terminate-invocation (invocation exit-command)
  (with-slots (frame process) invocation
    (write-line "Terminating frame")
    (execute-one-command invocation exit-command)
    (unless (mp::process-stack-group process) (error "Frame terminated abnormally"))
    (unless (wait-for-death process)
      (warn "Process did not terminate"))))

(defun wait-for-death (process)
  (let (done)
    (mp:process-wait-with-timeout
     "Waiting for death" 
     *death-timeout*
     #'(lambda () (setq done (not (mp::process-stack-group process)))))
    done))


(defun execute-commands-in-invocation (invocation commands)
  (do ((commands commands (cdr commands)))
      ((or (not (invocation-active-p invocation))
	   (null commands)))
    (execute-one-command invocation (car commands))
    (wait-for-clim-input-state invocation)))

(defvar *command-sequence-table* (make-hash-table))

(defun execute-one-command (invocation command)
  (with-slots (frame process avv-frame) invocation
    (let ((sheet (frame-top-level-sheet (or avv-frame frame))))
      (unless (characterp command)
	(format t "Executing command ~S~%" command))
      (etypecase command
	(character
	 (let ((x (clim-internals::port-canonicalize-gesture-spec (port frame) command)))
	   (when x
	     (distribute-event (port frame)
			       (make-instance 'key-press-event
					      :sheet sheet
					      :character command
					      :key-name (car x)
					      :modifier-state (cdr x))))))
	(string
	 (dotimes (i (length command))
	   (execute-one-command invocation (char command i))))
	(keyword
	 (distribute-event (port frame)
			   (multiple-value-bind (keysym modifiers)
			       (clim-internals::gesture-name-keysym-and-modifiers command)
			     (make-instance 'key-press-event
					    :sheet sheet
					    :character nil
					    :key-name keysym 
					    :modifier-state modifiers))))

	(list
	 (case (car command)
	   (:sleep
	    (sleep (cadr command)))
	   (:presentation-click
	    (apply #'click-on-presentation invocation (cdr command)))
	   (:presentation-press
	    (apply #'click-on-presentation invocation 
		   (second command) (third command) :release nil (cdddr command)))
	   (:click
	    (apply #'click-on-window invocation (cdr command)))
	   (:press
	    (apply #'press-on-window invocation (cdr command)))
	   (:release
	    (apply #'release-on-window invocation (cdr command)))
	   (:command
	    (apply #'test-a-command invocation (cdr command)))
	   (:commands
	    (funcall (cadr command) invocation))
	   (:edit-avv
	    (destructuring-bind (pane prompt new-value)  (cdr command)
	      (change-query-value invocation prompt new-value pane)))
	   (t
	    (if (and (consp command) (consp (car command)))
		(destructuring-bind (command &key timeout) command
		  (execute-command-in-frame (or avv-frame frame) command)
		  (when timeout
		    (wait-for-clim-input-state invocation timeout)))
	      (execute-command-in-frame (or avv-frame frame) command)))))))))

(defun exercise-frame (test-name class initargs commands exit-command &optional (error *catch-errors-in-tests*))
  (flet ((doit ()
	   (let ((invocation (make-instance 'invocation :class class :initargs initargs)))
	     (unwind-protect
		 (if commands
		     (execute-commands-in-invocation invocation commands)
		   (sleep 5))
	       (terminate-invocation invocation exit-command)
	       (mp::process-kill (invocation-process invocation))
	       (unless (wait-for-death (invocation-process invocation))
		 (warn "Process would not die when killed"))
	       (destroy-frame (invocation-frame invocation))))))
    (if error
	(handler-case (doit)
	  (error (condition)
	    (note-test-failed test-name condition))
	  (:no-error (&rest ignore)
	    (declare (ignore ignore))
	    (note-test-succeeded test-name)))
      (doit))))

(defvar *test-successes* nil)
(defvar *test-failures* nil)

(defun note-test-failed (test reason)
  (warn "The following error occurred: ~A" reason)
  (when *test-failures*
    (push (cons test reason) (cdr *test-failures*))))

(defun note-test-succeeded (test)
  (when *test-successes*
    (push test (cdr *test-successes*))))

(defmacro with-test-reporting ((&rest options &key file) &body body)
  `(invoke-with-test-reporting #'(lambda () ,@body) ,@options))

(defun invoke-with-test-reporting (continuation &rest options)
  (let ((*test-successes* (list nil))
	(*test-failures* (list nil)))
    (funcall continuation)
    (pop *test-successes*)
    (pop *test-failures*)
    (apply #'generate-test-report options)))

(defun generate-test-report (&key file)
  (format t "~4%")
  (when *test-failures*
    (format t "The following tests failed:~%")
    (dolist (x *test-failures*)
      (format t "~10t~A : ~A~%" (car x) (cdr x))))
    
  (when (probe-file file)
    (let (old-successes old-failures)
      (with-open-file (*standard-input* file)
	(let ((*package* (find-package :clim-user)))
	  (setq old-successes (read) old-failures (read))))
	
      (let ((first t))
	(dolist (x *test-failures*)
	  (when (member (car x) old-successes)
	    (when first
	      (format t "The following tests have for failed the first time:~%")
	      (setq first nil))
	    (format t "~10t~A : ~A~%" (car x) (cdr x)))))
	
      (let ((first t))
	(dolist (x *test-failures*)
	  (when (member (car x) old-failures)
	    (when first
	      (format t "The following tests have failed again:~%")
	      (setq first nil))
	    (format t "~10t~A : ~A~%" (car x) (cdr x)))))
	
      (let ((first t))
	(dolist (x *test-successes*)
	  (when (member x old-failures)
	    (when first
	      (format t "The following tests have succeeded finally:~%")
	      (setq first nil))
	    (format t "~10t~A~%" x))))))
    
  (when *test-successes*
    (format t "The following tests succeeded:~%")
    (dolist (x *test-successes*)
      (format t "~10t~A ~%" x)))
    

  (with-open-file (*standard-output* file
		   :direction :output :if-exists :supersede)
    (let ((*package* (find-package :clim-user)))
      (print *test-successes*)
      (print (mapcar #'car *test-failures*)))))
	
    
	  
	  
(defun walk-over-presentations (function output-record)
  (labels ((find-object-1 (record x-offset y-offset)
	     (with-bounding-rectangle* (left top right bottom) record
	       (incf left x-offset)
	       (incf right x-offset)
	       (incf top y-offset)
	       (incf bottom y-offset)
	       (when (typep record 'standard-presentation)
		 (funcall function record left top right bottom))
	       (multiple-value-bind (xoff yoff) (output-record-start-cursor-position record)
		 (map-over-output-records
		  #'find-object-1 record
		  (- x-offset) (- y-offset)
		  (+ x-offset xoff) (+ y-offset yoff))))))
    (find-object-1 output-record 0 0)))

(setq *random-state* (make-random-state t))


(defun click-on-presentation (invocation pane-name
			      presentation-type 
			      &key gesture 
				   (release t)
				   (modifier 0) (button +pointer-left-button+))
  (with-slots (frame process) invocation
    (when gesture
      (multiple-value-setq (button modifier)
	(clim-internals::gesture-name-button-and-modifiers gesture))
      (assert button () "Not a valid gesture"))
    (let ((pane (get-frame-pane frame pane-name))
	  (presentations nil)
	  (expanded-presentation-type
	   (expand-presentation-type-abbreviation presentation-type)))
      (flet ((doit (record left top right bottom)
	       (when (presentation-subtypep 
		      (presentation-type record)
		      expanded-presentation-type)
		 (stream-set-pointer-position pane left top)
		 ;; At this point it would be nice to specify that a
		 ;; modifier key was pressed.
		 (wait-for-clim-input-state invocation)
		 (when (eq (clim-internals::stream-highlighted-presentation
			    pane) record)
		   (push (list record left top right bottom)
			 presentations)))))
	(walk-over-presentations #'doit (stream-output-history pane)))
      #+ignore
      (format excl:*initial-terminal-io* "~d presentations~%" (length presentations))
      (assert presentations ()
	"Did not find presentations to click on!")
      (when presentations
	(prog1 t
	  (let* ((len (length presentations))
		 (i (random len)))
	    (destructuring-bind
		(record left top right bottom) (nth i presentations)
	      (declare (ignore record right bottom))
	      #+ignore
	      (format excl:*initial-terminal-io*
		      "selecting ~d of ~d = ~s @ ~d,~d~%"
		      i len record left top)
	      (stream-set-pointer-position pane left top)
	      (wait-for-clim-input-state invocation)
	      (assert (clim-internals::stream-highlighted-presentation pane))
	      ;;-- this is bypassing the distribution mechanism
	      ;;-- Perhaps we should have a send-event that interfaces to
	      ;;-- the Xlib code. But can we send fill in all the detail
	      ;;-- fields of the event
	      ;;-- In order to get the modifiers to work I think we need
	      ;;-- to send Keypress/release events
	      (multiple-value-bind
		  (x y) (untransform-position (sheet-device-transformation pane) left top)
		(let ((ma (sheet-mirrored-ancestor pane))
		      (port (port pane)))
		  (distribute-event port
				    (make-instance 'pointer-button-press-event
						   :sheet ma
						   :pointer (port-pointer port)
						   :button button
						   :native-x x
						   :native-y y
						   :x :?? :y :??
						   :modifier-state
						   modifier))
		  (when release
		    (distribute-event port
				      (make-instance 'pointer-button-release-event
						     :sheet ma
						     :pointer (port-pointer port)
						     :button button
						     :native-x x
						     :native-y y
						     :x :?? :y :??
						     :modifier-state modifier))))))))))))


(defun click-on-window (invocation pane-name left top &rest args)
  (apply #'button-event-on-window invocation pane-name left top
	 :up t :down t args))

(defun press-on-window (invocation pane-name left top &rest args)
  (apply #'button-event-on-window invocation pane-name left top
	 :up nil :down t args))

(defun release-on-window (invocation pane-name left top &rest args)
  (apply #'button-event-on-window invocation pane-name left top
	 :up t :down nil args))
	 


(defun button-event-on-window (invocation pane-name left top 
			       &key gesture (modifier 0) (button +pointer-left-button+)
				    up down)
  (with-slots (frame process) invocation
    (when gesture
      (multiple-value-setq (button modifier)
	(clim-internals::gesture-name-button-and-modifiers gesture))
      (assert button () "Not a valid gesture"))
    (let ((pane (get-frame-pane frame pane-name)))
      (stream-set-pointer-position pane left top)
      (wait-for-clim-input-state invocation)
      (multiple-value-bind
	  (x y) (untransform-position (sheet-device-transformation pane) left top)
	(let ((ma (sheet-mirrored-ancestor pane))
	      (port (port pane)))
	  (when down
	    (distribute-event port
			      (make-instance 'pointer-button-press-event
					     :sheet ma
					     :pointer (port-pointer port)
					     :button button
					     :native-x x
					     :native-y y
					     ;;-- normally the distributor
					     ;;-- fills these in
					     :x :?? :y :??
					     :modifier-state modifier)))
	  (when up
	    (distribute-event port 
			      (make-instance 'pointer-button-release-event
					     :sheet ma
					     :pointer (port-pointer port)
					     :button button
					     :native-x x
					     :native-y y
					     ;;-- normally the distributor
					     ;;-- fills these in
					     :x :?? :y :??
					     :modifier-state modifier))))))))



(defun simulate-accept (invocation pane-name presentation-type &key
								  stream 
								  default provide-default &allow-other-keys)
  ;; We a choice.
  ;; 1. Send the characters or
  ;; 2. Click on a presentation
  (assert (click-on-presentation invocation pane-name presentation-type)
      () "Clicking failed"))

(defun send-it (invocation x &key (delim t))
  (execute-one-command invocation (string x))
  (when delim
    (execute-one-command invocation " ")))

(defun test-a-command (invocation pane-name command &key colon-prefix)
  (with-slots (process frame) invocation
    (let ((stream (get-frame-pane frame pane-name)))
      (flet ((accept-function (stream presentation-type &rest args)
	       (apply #'simulate-accept invocation pane-name stream
		      presentation-type args))
	     (send-it-function (x)
	       (send-it invocation x)))
	(when colon-prefix
	  (send-it invocation ":" :delim nil))
	(fill-in-partial-command-1
	 (if (atom command) command (car command))
	 (frame-command-table frame)
	 stream
	 command
	 #'accept-function
	 #'send-it-function)
	;;--- Need to send a newline now
	;;--- It looks like some clim code tries to eat following
	;;--- delimiters if they are there. Which code is this?
	(sleep 2)
	(send-it invocation #\newline :delim nil)
	))))



(defvar *frame-tests* nil)

(defmacro define-frame-test (name (class &rest initargs) commands exit-command)
  `(progn
     (pushnew ',name *frame-tests*)
     (defun ,name ()
       (exercise-frame ',name ',class ',initargs ',commands ',exit-command))))

(defmacro define-command-sequence (name &rest commands)
  `(defun ,name (invocation)
     (execute-commands-in-invocation invocation ',commands)))

(defun do-frame-tests (&optional (errorp *catch-errors-in-tests*))
  (dolist (test *frame-tests*)
    (format t "Doing test ~A~%" test)
    (if errorp
	(handler-case (funcall test)
	  (error (c) 
	    (format t "~&The following error occurred in ~S: ~A~%" test
		    c)))
      (funcall test))))


;;; Training stuff

(defun train-clim-2 (&optional (n 2))
  (dotimes (i n)
    (format t "Test ~D out of ~D~%" (1+ i) n)
    (test-it)))

(defun benchmark-clim (filename)
  (test-it (or filename
	       (multiple-value-bind (second minute hour date month year)
		   (get-decoded-time)
		 (declare (ignore second minute hour))
		 (format nil "notes/times/~D.~D.~D.n" month date year)))))

(defun test-it (&optional filename (errorp *catch-errors-in-tests*))
  (let* ((file (pathname (or filename (format nil "/tmp/foofile~A" (gensym))))))
    (exercise-frame 'test-it
		    'clim-tests
		    '(:width 600 :height 400 :left 0 :top 0)
		    `(((run-benchmarks-to-dummy-file :file ,file) :timeout 1800))
		    `(exit-clim-tests)
		    errorp)
    (unless filename (delete-file file))))

(defun fill-in-partial-command-1 (command-name command-table stream
				  partial-command accept-function send-it)
  (let ((*original-stream* nil)
	(copy-partial-command (if (atom partial-command)
				  (list partial-command)
				partial-command)))
    (labels ((arg-parser (stream presentation-type &rest args)
	       (declare (dynamic-extent args))
	       ;; This code is to handle the case where a partial command has been
	       ;; passed in.  PARSE-NORMAL-ARG needs to be called with a :DEFAULT of
	       ;; the appropriate element of the partial command structure.  
	       (let* ((default (if copy-partial-command
				   (pop copy-partial-command)
				 *unsupplied-argument-marker*)))
		 (with-presentation-type-decoded (type-name parameters) presentation-type
		   (when (eq type-name 'command-name)
		     (send-it (string-downcase(command-line-name-for-command
			       command-name
			       command-table)))
		     (return-from arg-parser (values command-name presentation-type)))
		   (cond ((not (clim-internals::unsupplied-argument-p default))
			  (cond ((eq type-name 'keyword-argument-name) 
				 default)
				(t (apply #'parse-normal-arg
					  stream presentation-type
					  :default default args))))
			 ((eq type-name 'keyword-argument-name)
			  (let ((name (intern (symbol-name (caar parameters)) clim-internals::*keyword-package*)))
			    (send-it name)
			    name))
			 (t 
			  (apply #'parse-normal-arg
				 stream presentation-type
				 :provide-default nil args))))))
	     (send-it (it)
	       (funcall send-it it))
	     (parse-normal-arg (stream arg-type &rest options)
	       (declare (dynamic-extent options))
	       (with-delimiter-gestures (*command-argument-delimiters*)
		 (apply accept-function arg-type :stream stream options)))
	     (separate-args (stream args-to-go)
	       (declare (ignore stream args-to-go))
	       #+ignore
	       (when (only-keyword-args-remain args-to-go)
		 (throw 'stop-reading-command-arguments nil))))
      (declare (dynamic-extent #'arg-parser #'parse-normal-arg #'separate-args))
      (clim-internals::invoke-command-parser-and-collect
       command-table #'arg-parser #'separate-args stream))))

;;; Need
;;; 1. Access to avv fields
;;; 2. Send mouse events clicks that are independent of presentations
;;; (eg tracking-pointer stuff).

;;; Testing avv

(defun get-avv-frame (invocation)
  (with-slots (process frame) invocation
    (let ((avv-frame nil))
      (loop
	(setf avv-frame nil)
	(mp::process-interrupt process #'(lambda () (setq avv-frame (list *application-frame*))))
	(mp:process-wait "Waiting for frame" #'(lambda () avv-frame))
	(unless (eq (car avv-frame) frame)
	  (return-from get-avv-frame (car avv-frame)))
	(sleep 1)))))

(defun find-avv-query (avv-stream prompt)
  (maphash #'(lambda (query-id query)
	       (when (and (consp query-id)
			  (eq (car query-id) :query-identifier)
			  (consp (cdr query-id))
			  (equal (cadr query-id) prompt))
		 (return-from find-avv-query query)))
	   (slot-value (slot-value avv-stream 'clim-internals::avv-record) 'clim-internals::query-table)))

(defun change-query-value (invocation prompt new-value &optional pane-name)
  (with-slots (process frame avv-frame) invocation
    (let ((query (find-avv-query (if pane-name
				     (car (gethash (get-frame-pane frame pane-name) 
						   (clim-internals::frame-pane-to-avv-stream-table frame)))
				   (slot-value avv-frame 'stream))
				 prompt)))
      (assert query () "Could not find the query")
      (execute-one-command invocation `(clim-internals::com-change-query ,query ,new-value)))))

;; How to invoke command-buttons
;; It would be nice if we could change gadget values directly.
;; Lets say we want to run the bitmap-editor-add-color frame, change
;; some values and then exit from the frame
;; How can we wait for a frame to be created. Grab hold of it and then
;; run some commands on it.
