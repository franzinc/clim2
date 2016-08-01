;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

(defmethod frame-manager-construct-avv-panes ((frame accept-values-own-window)
					      (framem xm-silica::motif-frame-manager))
  (let (exit-button-stream own-window pane)
    (declare (ignore pane))
    (values
     (with-look-and-feel-realization (framem frame)
       (with-slots (scroll-bars) frame
	 (vertically ()
	   (multiple-value-setq (pane own-window)
	     (make-clim-stream-pane
	      :output-record
	      (make-instance 'standard-sequence-output-history)
	      :initial-cursor-visibility :off
	      :scroll-bars scroll-bars
	      :end-of-page-action :allow))
	   (make-pane 'silica::separator :orientation :horizontal)
	   (setf exit-button-stream
	     (make-pane 'clim-stream-pane
			:initial-cursor-visibility nil)))))
     own-window
     exit-button-stream)))

;;;---- The rest of what in here is dreaming right now
;;;---- It would be nice to use more

;#+ignore
;(define-application-frame xm-silica::motif-accepting-values-frame (accept-values-own-window)
;			  ()
;  (:panes
;   (control-area :application :scroll-bars nil)
;   (action-area (make-pane 'xm-silica::motif-form-pane)))
;  (:menu-bar nil)
;  (:top-level (accept-values-top-level))
;  (:command-table accept-values)
;  (:command-definer nil)
;  (:layouts
;   (:default
;       (let ((frame *application-frame*)
;	     (view +gadget-dialog-view+))
;	 (let ((vp
;		(make-pane 'xm-silica::motif-paned-pane
;			   :children (list control-area action-area))))
;	   (let ((labels (frame-manager-exit-box-labels (frame-manager frame) frame view)))
;	     (with-slots (exit-boxes) frame
;	       (dolist (exit-box exit-boxes)
;		 (let* ((value (if (consp exit-box) (car exit-box) exit-box))
;			(label (or (if (consp exit-box) (second exit-box))
;				   (second (assoc value labels)))))
;		   (make-pane 'push-button
;			      :parent action-area
;			      :label label
;			      :client frame :id value
;			      :activate-callback
;			      #'handle-exit-box-callback))))
;	     (setf (slot-value action-area 'xm-silica::fraction-base)
;	       (1+ (* 2 (length labels))))
;	     (setf (slot-value action-area 'xm-silica::attachments)
;	       (let ((i 0))
;		 (mapcar #'(lambda (label)
;			     (declare (ignore label))
;			     (prog1
;			     `(,i :top-attachment :form
;				  :bottom-attachment :form
;				  :left-attachment :position
;				  :left-position ,(1+ (* 2 i))
;				  :right-attachment :position
;				  :right-position ,(+ 2 (* 2 i)))
;			     (incf i)))
;			 labels)))
;
;	     vp))))))
;
;
;
;
;(defmethod generate-panes :after ((framem standard-frame-manager)
;				  (frame xm-silica::motif-accepting-values-frame))
;  (setf (slot-value frame 'stream)
;    (make-instance 'accept-values-stream
;		   :stream (setf (slot-value frame 'own-window)
;			     (get-frame-pane frame 'control-area))))
;  (setf (slot-value frame 'exit-button-stream) nil))
;
;(defmethod display-exit-boxes ((frame xm-silica::motif-accepting-values-frame)
;			       stream
;			       (view view))
;  ;;-- Nothing should have changed
;  nil)
;
;;;; Need to hook up the constraint resources and set the min-max size
;;;; of the action-pane to be just large enough to contain the buttons.
;
;(defmethod accept-values-top-level ((frame xm-silica::motif-accepting-values-frame) &rest args)
;  (declare (ignore args))
;  ;; this might want to use table-formatting or equivalent
;  ;; to make sure that the rows line up properly.
;  ;; This requires formatting-table and friends to return their bodies' values properly.
;  (with-slots (stream continuation resynchronize-every-pass check-overlapping
;	       selected-item initially-select-query-identifier
;	       own-window own-window-properties exit-button-stream) frame
;
;    (unless own-window (return-from accept-values-top-level (call-next-method)))
;
;    (let* ((original-view (stream-default-view stream))
;	   (return-values nil)
;	   (initial-query nil)
;	   exit-button-record
;	   avv avv-record)
;      (letf-globally (((stream-default-view stream)
;		       (frame-manager-dialog-view (frame-manager frame)))
;		      ((cursor-state (stream-text-cursor stream)) nil))
;	(labels ((run-continuation (stream avv-record)
;		   (setf (slot-value stream 'avv-record) avv-record)
;		   (setf (slot-value stream 'avv-frame) frame)
;		   (with-output-recording-options (stream :draw nil :record t)
;		     (setq return-values (multiple-value-list
;					   (let ((*application-frame* *avv-calling-frame*))
;					     (funcall continuation stream))))
;		     (unless own-window
;		       (display-exit-boxes frame stream
;					   (stream-default-view stream)))))
;		 (run-avv ()
;		   (when (and initially-select-query-identifier
;			      (setq initial-query
;				    (find-query avv-record
;						(car initially-select-query-identifier))))
;		     (if (cdr initially-select-query-identifier)
;			 (com-modify-avv-choice initial-query)
;			 (com-edit-avv-choice initial-query))
;		     (redisplay avv stream :check-overlapping check-overlapping))
;		   (loop
;		     (let ((command
;			    (let ((command-stream (slot-value stream 'stream)))
;			      ;; While we're reading commands, restore the view
;			      ;; to what it was before we started.
;			      (letf-globally (((stream-default-view command-stream)
;					       original-view))
;				(read-frame-command frame :stream command-stream)))))
;		       (if (and command (not (keyboard-event-p command)))
;			   (execute-frame-command frame command)
;			 (beep stream)))
;		     (with-deferred-gadget-updates
;		       (when (or resynchronize-every-pass
;				 (slot-value avv-record 'resynchronize))
;			 ;; When the user has asked to resynchronize every pass, that
;			 ;; means we should run the continuation an extra time to see
;			 ;; that all visible stuff is up to date.  That's all!
;			 (with-output-recording-options (stream :draw nil)
;			   (redisplay avv stream :check-overlapping check-overlapping)))
;		     (setf (slot-value avv-record 'resynchronize) nil)
;
;;		     (when exit-button-record
;; 		       (redisplay exit-button-record exit-button-stream))
;
;		     (redisplay avv stream :check-overlapping check-overlapping)
;
;		     (when (ecase (frame-resizable frame)
; 			     ((nil) nil)
; 			     ((t) t)
; 			     (:grow
;			       (and own-window
;				    (multiple-value-bind (width height)
;					(bounding-rectangle-size
;					  (stream-output-history own-window))
;				      (multiple-value-bind (vwidth vheight)
;					  (bounding-rectangle-size
;					    (or (pane-viewport own-window) own-window))
;					(or (> width vwidth) (> height vheight)))))))
;		       (size-panes-appropriately)))))
;
;	       (size-panes-appropriately ()
;		 (changing-space-requirements ()
;		     ;; We really want to specify the min/max sizes of
;		     ;; the exit-button pane
;		     ;; also
;;		     (when exit-button-stream
;;		       (size-frame-from-contents exit-button-stream
;;						 :size-setter
;;						 #'(lambda (pane w h)
;;						     (change-space-requirements
;;						      pane
;;						      :width w :min-width w :max-width w
;;						      :height h :min-height h :max-height h))))
;		   (when own-window
;		     (size-frame-from-contents own-window
;					       :width own-window-width
;					       :height own-window-height
;					       :right-margin own-window-right-margin
;					       :bottom-margin own-window-bottom-margin)))))
;	  (declare (dynamic-extent #'run-continuation #'run-avv
;				   #'size-panes-appropriately))
;	  (with-simple-restart (frame-exit "Exit from the ACCEPT-VALUES dialog")
;	    (setq avv
;		  (updating-output (stream)
;		    (setq avv-record
;			  (with-end-of-line-action (stream :allow)
;			    (with-end-of-page-action (stream :allow)
;			      (with-new-output-record
;				  (stream 'accept-values-output-record avv-record)
;				(run-continuation stream avv-record)))))))
;	    ;; In own window dialogs the buttons are displayed separately
;;	    (when (and own-window exit-button-stream)
;;	      (setq exit-button-record
;;		    (updating-output (exit-button-stream)
;;		      (with-end-of-line-action (exit-button-stream :allow)
;;			(with-end-of-page-action (exit-button-stream :allow)
;;			  (display-exit-boxes frame exit-button-stream
;;					      (stream-default-view stream)))))))
;
;	    (unwind-protect
;		(cond (own-window
;		       (size-panes-appropriately)
;		       (multiple-value-bind (x y)
;			   #+++ignore (pointer-position (port-pointer (port own-window)))
;			   #---ignore (values 100 100)
;			 (when (and own-window-x-position own-window-y-position)
;			   (setq x own-window-x-position
;				 y own-window-y-position))
;			 (position-sheet-carefully
;			   (frame-top-level-sheet (pane-frame own-window)) x y))
;		       (window-expose own-window)
;		       (with-input-focus (own-window)
;			 (when exit-button-record
;			   (replay exit-button-record exit-button-stream))
;			 (replay avv stream)
;			 (run-avv)))
;		      (t
;		       ;; Ensure that bottom of the AVV is visible.  I think that
;		       ;; this is OK even if the AVV is bigger than the viewport.
;		       (move-cursor-beyond-output-record (slot-value stream 'stream) avv)
;		       (stream-ensure-cursor-visible stream)
;		       (replay avv stream)
;		       (run-avv)))
;	      (deactivate-all-gadgets avv-record)
;	      (unless own-window
;		(move-cursor-beyond-output-record (slot-value stream 'stream) avv))))
;	  (values-list return-values))))))


