;; -*- mode: common-lisp; package: xm-silica -*-
;;
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca.  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, Ca.  All rights reserved.
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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $fiHeader: xm-silica.lisp,v 1.44 1995/10/17 05:03:45 colin Exp $

(in-package :xm-silica)

;; Motif specific stuff

(defclass motif-port (xt-port) ()
  (:default-initargs :deep-mirroring t))

(defmethod find-port-type ((type (eql ':motif)))
  'motif-port)

(defmethod port-type ((port motif-port))
  ':motif)

(defmethod change-widget-geometry ((parent tk::xm-my-drawing-area) child
				   &rest args
				   &key x y width height)
  (declare (ignore x y width height))
  (apply #'tk::configure-widget child args))

(defmethod change-widget-geometry ((parent tk::xm-bulletin-board) child
				   &rest args
				   &key x y width height)
  (declare (ignore x y width height))
  (apply #'tk::configure-widget child args))

;;--- Why is this here???
;;--- It should be in xt-silica

(defmethod change-widget-geometry ((parent tk::shell) child
				   &rest args
				   &key x y width height)
  (declare (ignore x y args))
  ;;-- shells decide where windows are positioned!
  (tk::set-values child :width width :height height))

(defclass motif-geometry-manager (xt-geometry-manager) ())


(defmethod change-widget-geometry ((parent tk::xm-dialog-shell) child
				   &rest args
				   &key x y width height)
  (declare (ignore args))
  (tk::set-values child :width width :height height :x x :y y))

(defmethod find-shell-class-and-initargs ((port motif-port) sheet)
  (multiple-value-bind (class initargs)
      (if (popup-frame-p sheet)
	  (values 'tk::xm-dialog-shell
		  (append
		   (let ((x (find-shell-of-calling-frame sheet)))
		     (and x `(:transient-for ,x)))
		   (and (typep (pane-frame sheet)
			       'clim-internals::menu-frame)
			'(:override-redirect t))))
	(call-next-method))
    (values class `(:keyboard-focus-policy
		    ,(ecase (port-input-focus-selection port)
		       (:click-to-select :explicit)
		       (:sheet-under-pointer :pointer))
		    ,@initargs))))

(defmethod enable-xt-widget ((parent tk::xm-dialog-shell) (mirror t))
  ;; this is a nasty hack just to make sure that the child is managed.
  ;; top-level-sheets are created unmanaged because they are
  ;; disabled to we have to do not!
  (manage-child mirror)
  (popup (widget-parent mirror)))


(ff:defun-c-callable my-drawing-area-query-geometry-stub ((widget :unsigned-long)
							  (intended :unsigned-long)
							  (desired :unsigned-long))
  (my-drawing-area-query-geometry widget intended desired))

(defun setup-mda ()
  (tk::initializemydrawingareaquerygeometry
   (ff:register-function 'my-drawing-area-query-geometry-stub)))

(setup-mda)

(defun my-drawing-area-query-geometry (widget intended desired)
  (let* ((sheet (find-sheet-from-widget-address widget))
	 (sr (compose-space sheet))
	 (rm (tk::xt-widget-geometry-request-mode intended)))
    ;; If its asking and its out of range then say so
    (multiple-value-bind (width min-width max-width
			  height min-height max-height)
	(space-requirement-components sr)
      (when (or (and (logtest rm x11:cwwidth)
		     (not (<= min-width
			      (tk::xt-widget-geometry-width intended)
			      max-width)))
		(and (logtest rm x11:cwheight)
		     (not (<= min-height
			      (tk::xt-widget-geometry-height intended)
			      max-height))))
	(return-from my-drawing-area-query-geometry tk::xt-geometry-no))

      (when (and (logtest rm x11:cwheight) (logtest rm x11:cwwidth))
	(return-from my-drawing-area-query-geometry tk::xt-geometry-yes))

      (setf (tk::xt-widget-geometry-width desired) (fix-coordinate width)
	    (tk::xt-widget-geometry-height desired) (fix-coordinate height)
	    (tk::xt-widget-geometry-request-mode desired) (logior x11:cwwidth x11:cwheight)))


    (return-from my-drawing-area-query-geometry tk::xt-geometry-almost)))


;;; hacks for explicit focus

(defmethod note-sheet-tree-grafted :after ((port motif-port)
					   (sheet clim-stream-sheet))
  ;; this is a hack which enables this child widget to take input
  ;; events in the presence of explicit focus.
  (let ((cursor (stream-text-cursor sheet))
	(widget (sheet-mirror sheet)))
    (when cursor
      (let ((input-widget (make-instance 'tk::xm-my-drawing-area
			    :name :xm-cursor
			    :parent widget
			    :background (tk::get-values widget
							:background)
			    :width 1
			    :height 1
			    :managed (cursor-active cursor))))
	(with-slots (clim-internals::plist) cursor
	  (setf (getf clim-internals::plist :input-widget) input-widget)
	  (tk::add-callback input-widget
			    :input-callback
			    'sheet-mirror-input-callback
			    sheet)
	  (tk::add-event-handler input-widget
				 '(:focus-change)
				 1
				 'sheet-mirror-event-handler
				 sheet))))))

(defmethod note-sheet-degrafted :after ((sheet clim-stream-sheet))
  (let* ((cursor (stream-text-cursor sheet))
	 (plist (and cursor
		     (slot-value cursor 'clim-internals::plist)))
	 (input-widget (getf plist :input-widget)))
    (when input-widget
      (tk::destroy-widget input-widget))))

(defmethod port-note-cursor-change :after
	   ((port motif-port) cursor stream (type (eql 'cursor-active)) old new)
  (declare (ignore stream old))
  (let* ((plist (slot-value cursor 'clim-internals::plist))
	 (input-widget (getf plist :input-widget)))
    (when input-widget
      (if new
	  (tk::manage-child input-widget)
	(tk::unmanage-child input-widget)))))

(defmethod port-note-cursor-change :after
	   ((port motif-port) cursor stream (type (eql 'cursor-focus)) old new)
  (declare (ignore stream old))
  (when new
    (let* ((plist (slot-value cursor 'clim-internals::plist))
	   (input-widget (getf plist :input-widget)))
      (when input-widget
	(tk::xm_process_traversal input-widget 0)))))

(defun xm-get-focus-widget (widget)
  (tk::intern-widget (tk::xm_get_focus_widget widget)))

(defmethod port-move-focus-to-gadget ((port xm-silica::motif-port) gadget)
  ;;-- If this is composite then it should find the first child
  ;;-- that can take the focus
  (let ((m (sheet-direct-mirror gadget)))
    (when m (tk::xm_process_traversal m 0))))


;; why is this only defined in motif - shouldn't this be in
;; xt-silica.lisp (cim) 1/7/94

(defmethod destroy-mirror :around ((port xt-port) (sheet top-level-sheet))
  (let* ((m (sheet-direct-mirror sheet))
	 (shell (and m (tk::widget-parent m))))
    (call-next-method)
    (when (and shell
	       ;; Do it only if the shell has no popup children [clim2bug598]
	       (zerop (tk::xt_widget_num_popups shell)))
      (tk::destroy-widget shell))))

(defmethod port-move-frame :after ((port motif-port) frame x y)
  (fix-coordinates x y)
  (check-type x (signed-byte 16))
  (check-type y (signed-byte 16))
  (let ((m (sheet-direct-mirror (frame-top-level-sheet frame))))
    (when (typep m 'xt::xm-bulletin-board)
      (tk::set-values m :default-position nil :x x :y y))))

#+ics
(defmethod text-style-to-font-list ((port motif-port) text-style)
  (let ((result nil))
    (dotimes (i 4)
      (push (cons i (text-style-mapping port text-style i)) result))
    (nreverse result)))
