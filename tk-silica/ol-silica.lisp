;; See the file LICENSE for the full license governing this code.
;;

(in-package :xm-silica)

(defclass openlook-port (xt-port) ()
  (:default-initargs :deep-mirroring t))

(defmethod initialize-instance :before ((port openlook-port) &key)
  (unless (sys:getenv "OPENWINHOME")
    (warn "OPENWINHOME environment variable is not set")))

(defmethod find-port-type ((type (eql :openlook)))
  'openlook-port)

(defmethod port-type ((port openlook-port))
  ':openlook)

(setq *default-server-path* '(:openlook))

(defmethod port-note-cursor-change :after
    ((port openlook-port) cursor stream (type (eql 'cursor-focus)) old new)
  (declare (ignore cursor old))
  (when new
    (let ((mirror (sheet-mirror stream)))
      (when mirror
	(let ((window (tk::widget-window mirror nil)))
	  (when window
	    (with-port-event-lock (port)
	      (excl:without-interrupts
		(let ((display (port-display port)))
		  (tk:with-server-grabbed (display)
		    (when (eq (tk::window-map-state window) :viewable)
		      ;;--- There could very well be a race condition involving
		      ;;--- a couple of processes.  Another process could have made
		      ;;--- this window go away at this point
		      (tk::ol_set_input_focus
		       mirror
		       2	; RevertToParent
		       ;;-- This is not exact but it might work better.
		       (xt::xt-last-timestamp-processed display)))))))))))))

(defmethod change-widget-geometry ((parent tk::draw-area) child
				   &rest args
				   &key x y width height)
  (declare (ignore x y width height))
  (apply #'tk::configure-widget child args))

(defmethod change-widget-geometry ((parent tk::shell) child
						      &rest args
						      &key x y width height)
  (declare (ignore x y args))
  ;;-- shells decide where windows are positioned!
  (unless (multiple-value-bind (w h)
	      (tk::get-values child :width :height)
	    (and (= w width)
		 (= h height)))
    (tk::set-values child :width width :height height)))

;;;--- Why have this class

(defclass openlook-geometry-manager (xt-geometry-manager) ())

;;--- This looks kinda like to motif-port method.
(defmethod find-shell-class-and-initargs ((port openlook-port) sheet)
  (cond ( ;;--- hack alert
	 (popup-frame-p sheet)
	 (values 'tk::transient-shell
		 (append
		  (let ((x (find-shell-of-calling-frame sheet)))
		    (and x `(:transient-for ,x)))
		  (and (typep (pane-frame sheet)
			   'clim-internals::menu-frame)
		    '(:override-redirect t)))))
	(t
	 (call-next-method))))

(defmethod enable-xt-widget ((parent tk::transient-shell) (mirror t))
  (manage-child mirror)
  (popup parent))

(defmethod disable-xt-mirror ((parent xt::transient-shell) (mirror t))
  (popdown parent))

(defmethod destroy-mirror ((port openlook-port) (sheet mirrored-sheet-mixin))
  ;; Only do this if its the top most widget being destroyed or we are
  ;; screwing around with the tree in someway
  (xt::unmanage-child (sheet-direct-mirror sheet)))

(defun ol-get-focus-widget (widget)
  (tk::intern-widget (tk::ol_get_current_focus_widget widget)))

(defun make-xevent ()
  (clim-utils::allocate-cstruct 'x11::xevent :initialize t))

(defmethod process-an-event ((port openlook-port) mask reason)
  (with-slots (context) port
    ;; Because of a feature in the OLIT toolkit we need to
    ;; give preference to events rather than timer events

    #+debug
    (when (logtest mask tk::*xt-im-xevent*)
      (let ((event (make-xevent)))
	(unless (zerop (tk::xt_app_peek_event context event))
	  (print (tk::event-type event) excl:*initial-terminal-io*))))

    (tk::process-one-event context
			   (if (logtest mask tk::*xt-im-xevent*)
			       tk::*xt-im-xevent*
			     mask)
			   reason)))



