;; -*- mode: common-lisp; package: xm-silica -*-
;;
;;				-[]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, CA  All rights reserved.
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
;; $fiHeader: xt-frames.lisp,v 1.33 93/04/16 09:46:04 cer Exp $


(in-package :xm-silica)

;; Basic intrinsics frame-manager

(defclass xt-frame-manager (standard-frame-manager) 
    ((menu-cache
       :initform nil
       :accessor frame-manager-menu-cache)
     ))

(defmethod frame-wrapper ((framem xt-frame-manager) 
			  (frame standard-application-frame) pane)
  (with-look-and-feel-realization (framem frame)
    (let* ((menu-bar (slot-value frame 'menu-bar))
	   (menu-bar-pane
	    (and menu-bar
		 (apply #'make-pane 
			'menu-bar
			:command-table (cond ((eq t menu-bar)
					      (frame-command-table frame))
					     ((listp menu-bar)
					      (find-command-table (car menu-bar)))
					     (t (find-command-table menu-bar)))
			(and (listp menu-bar) (cdr menu-bar)))))
	   (pointer-doc-pane
	    ;;--- Don't like these forward references
	    (let ((options (clim-internals::frame-pointer-documentation-p frame)))
	      (and options
		   (apply #'make-pane
			  'clim-internals::pointer-documentation-pane
			  (append (and (listp options) options)
				  `(
				    :max-width ,+fill+
				    :max-height (1 :line)
				    :height (1 :line))))))))
      (cond ((and menu-bar-pane pointer-doc-pane)

	     (vertically () menu-bar-pane pane pointer-doc-pane))
	    (menu-bar-pane
	     (vertically () menu-bar-pane pane))
	    (pointer-doc-pane
	     (vertically () pane pointer-doc-pane))
	    (t pane)))))


;;;

(defun command-button-callback (button dunno frame command-table item)
  (declare (ignore dunno button))
  (execute-command-in-frame
   frame (substitute clim-internals::*application-frame-marker* frame
		     (command-menu-item-value item))
   :presentation-type `(command :command-table ,command-table)))


(defun frame-wm-protocol-callback (widget frame)
  (declare (ignore widget))
  ;; Invoked when the Wm close function has been selected
  ;; We want to queue an "event" somewhere so that we can
  ;; synchronously quit from the frame
  (distribute-event
   (port frame)
   (allocate-event 'window-manager-delete-event
     :sheet (frame-top-level-sheet frame))))

(defmethod handle-event (sheet (event window-manager-delete-event))
  (and (pane-frame sheet)
       (frame-exit (pane-frame sheet))))

;;; Menu code

(defmethod frame-manager-menu-choose
	   ((framem xt-frame-manager) items &rest keys
	    &key printer 
		 presentation-type 
		 (associated-window (frame-top-level-sheet *application-frame*))
		 text-style label
		 cache
		 (unique-id items)
		 (id-test 'equal)
		 (cache-value items)
		 (cache-test #'equal)
		 (gesture :select)
		 row-wise
		 n-columns
		 n-rows)
  (declare (ignore keys))      
  (declare (values value chosen-item gesture))
  (let ((port (port framem))
	menu closure)
    (setq cache (and cache (frame-manager-allows-menu-caching framem)))
    (when cache
      (let ((x (assoc unique-id
		      (frame-manager-menu-cache framem)
		      :test id-test)))
	(when x
	  (destructuring-bind
	      (menu-cache-value amenu aclosure) (cdr x)
	    (if (funcall cache-test cache-value menu-cache-value)
		(setq menu amenu closure aclosure)
	      (progn
		(setf (frame-manager-menu-cache framem)
		  (delete x (frame-manager-menu-cache framem)))
		(framem-destroy-menu framem amenu)))))))
      
    (unless menu
      (multiple-value-setq
	  (menu closure)
	(frame-manager-construct-menu framem 
				      items 
				      printer 
				      presentation-type 
				      associated-window
				      text-style
				      label
				      gesture
				      row-wise
				      n-columns
				      n-rows))
      (when cache
	(push (list unique-id cache-value menu closure) 
	      (frame-manager-menu-cache framem))))
	  
    ;; initialize the closure
    (funcall closure t)
    ;;
    (multiple-value-bind
	(ignore win1 win2 x y root-x root-y)
	(tk::query-pointer (tk::display-root-window
			    (port-display port)))
      (declare (ignore ignore win1 win2 x y))
      (tk::set-values menu :x root-x :y root-y)

      (unwind-protect
	  (progn
	    (loop
	      (when (funcall closure) (return nil))


	      (framem-enable-menu framem menu)
	      ;; Now we have to wait
	      (port-force-output port)
	      (wait-for-callback-invocation
	       port
	       #'(lambda () 
		   ;;-- This is to deal
		   ;;-- with the race
		   ;;-- condition where
		   ;;-- the menu go down
		   ;;-- to quick
		   (or (funcall closure)
		       (not (framem-menu-active-p framem menu)))) 
	       "Returned value"))
	    (framem-popdown-menu framem menu))
	(unless cache
	  (framem-destroy-menu framem menu)))
      (port-force-output port)
      (values-list (nth-value 1 (funcall closure))))))

(defmethod frame-manager-allows-menu-caching ((framem xt-frame-manager))
  t)


;;;

(defmethod frame-manager-exit-box-labels ((framem xt-frame-manager) frame view)
  (declare (ignore frame view))
  '((:exit  "Ok")
    (:abort  "Cancel")))

(defmethod frame-manager-default-exit-boxes ((framem xt-frame-manager))
  '((:exit) (:abort)))

(defmethod note-frame-iconified ((framem xt-frame-manager) frame)
  (let ((display (port-display (port framem))))
    (x11:xiconifywindow display (tk::widget-window (frame-shell frame))
		    (xt::display-screen-number display))))

(defmethod note-frame-deiconified ((framem xt-frame-manager) frame)
  (x11:xmapwindow (port-display (port framem)) 
		  (tk::widget-window (frame-shell frame))))

;;; 

(defmethod invoke-with-menu-as-popup ((framem xt-frame-manager) (window t) continuation)
  (funcall continuation))


(defmethod invoke-with-mouse-grabbed-in-window ((framem xt-frame-manager) (window t) continuation &key)
  (invoke-with-pointer-grabbed window continuation))



(defclass xt-menu-bar ()
  ((command-name-to-button-table 
    :accessor menu-bar-command-name-to-button-table
    :initform nil)))

(defmethod note-sheet-degrafted :after ((sheet xt-menu-bar))
  (setf (menu-bar-command-name-to-button-table sheet) nil))

(defmethod note-command-enabled :after ((framem xt-frame-manager) frame command)
  (update-command-button-status frame command t))

(defmethod note-command-disabled :after ((framem xt-frame-manager) frame command)
  (update-command-button-status frame command nil))

(defun update-command-button-status (frame command enabled)
  (let ((sheet (frame-top-level-sheet frame)))
    (when sheet
      (flet ((update-sheet (sheet)
	       (when (typep sheet 'xt-menu-bar)
		 (let ((button (cdr (assoc command (menu-bar-command-name-to-button-table sheet)))))
		   (when button
		     (tk::set-sensitive button enabled))))))
	(map-over-sheets #'update-sheet sheet)))))



(defmethod update-frame-settings ((framem xt-frame-manager) (frame t))
  ;;--- Lets see how this works out
  (let ((shell (sheet-shell (frame-top-level-sheet frame))))
    (let ((sr (compose-space (frame-top-level-sheet frame))))
      (tk::set-values shell
		      :min-width (fix-coordinate (space-requirement-min-width sr))
		      :min-height (fix-coordinate (space-requirement-min-height sr))))

    (let ((geo (clim-internals::frame-geometry frame)))
      (destructuring-bind
	  (&key left top width height &allow-other-keys) geo
	;;-- what about width and height
	(when (and width height)
	  (tk::set-values shell :width (fix-coordinate width) 
			  :height (fix-coordinate height)))
	(when (and left top)
	  (tk::set-values shell 
			  :x (fix-coordinate left)
			  :y (fix-coordinate top)))))
    
    (tk::set-values shell :title (frame-pretty-name frame))
    (let ((icon (clim-internals::frame-icon frame)))
      (flet ((decode-pixmap (x)
	       (etypecase x
		 (string x)
		 (pattern 
		  (let ((sheet (frame-top-level-sheet frame)))
		    (with-sheet-medium (medium sheet)
		      (destructuring-bind (&key background-pixmap)
			  (decode-gadget-background medium sheet x)
			background-pixmap)))))))
	(destructuring-bind
	    (&key (name (frame-pretty-name frame)) pixmap clipping-mask) icon
	  ;;-- Dialog shells do not have :icon-name resource
	  (when (typep shell 'tk::top-level-shell)
	    (tk::set-values shell :icon-name name))
	  (when pixmap
	    (tk::set-values shell :icon-pixmap (decode-pixmap pixmap)))
	  (when clipping-mask
	    (tk::set-values shell :clip-mask (decode-pixmap clipping-mask))))))))

(defmethod frame-manager-note-pretty-name-changed ((framem xt-frame-manager) 
						   (frame standard-application-frame))
  (let ((shell (frame-shell frame)))
    (tk::set-values shell :title (frame-pretty-name frame))
    (when (typep shell 'tk::top-level-shell)
      (destructuring-bind (&key name &allow-other-keys) (clim-internals::frame-icon frame)
	;;-- Dialog shells do not have :icon-name resource
	(tk::set-values shell :icon-name (or name (frame-pretty-name frame)))))))
