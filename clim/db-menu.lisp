;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $Header: /repo/cvs.copy/clim2/clim/db-menu.lisp,v 1.9.22.1 1998/05/19 01:04:28 layer Exp $

"Copyright (c) 1992 by Symbolics, Inc.  All rights reserved."

;;; This file also exists in clim2/homegrown. The homegrown directory
;;; contains files to implement CLIM's generic gadgets. Native backends
;;; should not require this code. However, the Windows port of CLIM does
;;; seem to require this file so it is duplicated here with
;;; modifications. At some point this file and the file in homegrown should
;;; be merged and one of the two removed from cvs control (cim/tjm 2/4/97)

(in-package :silica)        ;yes, this file is in the Silica package

;;; Menu bars and buttons

;;--- The main visual problem is that there is just too much whitespace...


;;; Menu bar buttons

;; Like push buttons, except that they activate when the pointer button is
;; down rather than when it is released
#+(or aclpc acl86win32)
(defclass menu-bar-button (push-button-pane)
    ((next-menu :initform nil :initarg :next-menu)))

;;;#+(or aclpc acl86win32)
;;;(eval-when (compile load eval)
;;;   ;;mm: 11Jan95 - this is defined later in  ???
;;;   (unless (ignore-errors (find-class 'pull-down-menu))
;;;      (defclass pull-down-menu () ())))

#+(or aclpc acl86win32)
(defmethod handle-event ((pane menu-bar-button) (event pointer-enter-event))
  (with-slots (armed next-menu) pane
    (unless armed
      (cond ((let ((pointer (pointer-event-pointer event)))
               (and (pointer-button-state pointer)
                    (not (zerop (pointer-button-state pointer)))))
             (with-sheet-medium (medium pane)
               (setf armed :active)
               (highlight-button pane medium)
               (if (typep next-menu 'pull-down-menu)
                   (choose-from-pull-down-menu next-menu pane)
                   (activate-callback pane (gadget-client pane) (gadget-id pane)))
               (setf armed t)
               (highlight-button pane medium)))
            (t (setf armed t)))
      (armed-callback pane (gadget-client pane) (gadget-id pane)))))

#+(or aclpc acl86win32)
(defmethod handle-event ((pane menu-bar-button) (event pointer-button-press-event))
  (with-slots (armed next-menu) pane
    (with-sheet-medium (medium pane)
      (when armed
        (setf armed :active)
        (highlight-button pane medium))
      (if (typep next-menu 'pull-down-menu)
          (choose-from-pull-down-menu next-menu pane)
          (activate-callback pane (gadget-client pane) (gadget-id pane)))
      (setf armed t)
      (highlight-button pane medium))))

#+(or aclpc acl86win32)
(defmethod handle-event ((pane menu-bar-button) (event pointer-button-release-event))
  (with-slots (armed) pane
    (when (and (eq armed :active))
      (with-sheet-medium (medium pane)
        (setf armed t)
        (highlight-button pane medium)))))


;;; Pull-down menu buttons
#+(or aclpc acl86win32)
(defparameter *cascade-button-pattern*
              (make-pattern #2a((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 
                                (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
                                (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1) 
                                (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 1) 
                                (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 1) 
                                (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 1) 
                                (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 1) 
                                (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 0 0 1) 
                                (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 1) 
                                (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 1) 
                                (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 0 0 1) 
                                (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 1) 
                                (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 1) 
                                (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 1) 
                                (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 1) 
                                (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1) 
                                (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
                                (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
                            (list +background-ink+ +foreground-ink+)))

;; Like push buttons except that the pointer button is assumed to be already down
#+(or aclpc acl86win32)
(defclass pull-down-menu-button (push-button-pane) 
    ((next-menu :initform nil :initarg :next-menu)))

#+(or aclpc acl86win32)
(defmethod initialize-instance :after ((pane pull-down-menu-button) &key)
  (with-slots (next-menu pattern) pane
    (when next-menu
      (setq pattern *cascade-button-pattern*))))

#+(or aclpc acl86win32)
(defmethod handle-event ((pane pull-down-menu-button) (event pointer-enter-event))
  (when (port pane)                                ;the menu is sometimes disabled...
    (let* ((pointer (port-pointer (port pane)))
           (pointer-button-state (pointer-button-state pointer)))
      (unless (= pointer-button-state 0)
        (with-slots (armed) pane
          (unless (eq armed :active)
            (with-sheet-medium (medium pane)
              (setq armed :active)
              (highlight-button pane medium))
            (armed-callback pane (gadget-client pane) (gadget-id pane))))))))

#+(or aclpc acl86win32)
(defmethod handle-event ((pane pull-down-menu-button) (event pointer-exit-event))
  (when (port pane)                                ;the menu is often disabled...
    (with-slots (armed) pane
      (when armed
        (with-sheet-medium (medium pane)
          (setq armed nil)
          (highlight-button pane medium))
        (disarmed-callback pane (gadget-client pane) (gadget-id pane))))))

#+(or aclpc acl86win32)
(defmethod handle-event ((pane pull-down-menu-button) (event pointer-motion-event))
  (with-slots (next-menu x-margin normal-pattern) pane
    (when next-menu
      (with-bounding-rectangle* (left top right bottom) (sheet-region pane)
        (declare (ignore top right bottom))
        (let* ((pattern-width (pattern-width normal-pattern))
               (sensitive-region 16)
               (x (pointer-event-x event)))
          (when (and next-menu (> x (- (+ left x-margin pattern-width) sensitive-region)))
            (if (typep next-menu 'pull-down-menu)
                (choose-from-pull-down-menu next-menu pane :cascade-p t)
                (funcall next-menu pane))))))))

;; We really shouldn't ever get one of these - the button must have been down
;; for us to get here.
#+(or aclpc acl86win32)
(defmethod handle-event ((pane pull-down-menu-button) (event pointer-button-press-event))
  )

#+(or aclpc acl86win32)
(defmethod handle-event ((pane pull-down-menu-button) (event pointer-button-release-event))
  (with-slots (armed) pane
    (when (eq armed :active)
      (setf armed t)
      (with-sheet-medium (medium pane)
        (highlight-button pane medium))
      (activate-callback pane (gadget-client pane) (gadget-id pane))
      ;;--- This modularity is a bit dubious.  Oh well.
      (throw 'exit-pull-down-menu (values)))))


;;; Pull-down menu sheets

#+(or aclpc acl86win32)
(defclass popup-menu-sheet 
          (sheet-permanently-enabled-mixin
           sheet-single-child-mixin
           wrapping-space-mixin
           space-requirement-mixin
           basic-pane)
    ()
  (:default-initargs :medium t))

#+(or aclpc acl86win32)
(defclass pull-down-menu (popup-menu-sheet)
    ((current-button :initform nil)
     (buttons :initarg :buttons :accessor pull-down-menu-buttons)
     (entered :initarg nil :accessor pull-down-menu-entered))
  (:default-initargs :buttons nil))

#+(or aclpc acl86win32)
(defmethod handle-repaint :after ((pane pull-down-menu) region)
  (dolist (button (slot-value pane 'buttons))
    (handle-repaint button region)))

#+(or aclpc acl86win32)
(defmethod handle-event ((pane pull-down-menu) (event pointer-motion-event))
  (when (and (plusp (pointer-event-x event))
             (plusp (pointer-event-y event)))
    (setf (pull-down-menu-entered pane) t)))

#+(or aclpc acl86win32)
(defmethod handle-event ((pane pull-down-menu) (event pointer-button-press-event))
  )

#+(or aclpc acl86win32)
(defmethod handle-event ((pane pull-down-menu) (event pointer-button-release-event))
  )

#+(or aclpc acl86win32)
(defmethod handle-event ((pane pull-down-menu) (event pointer-enter-event))
  (when (and (plusp (pointer-event-x event))
             (plusp (pointer-event-y event)))
    (setf (pull-down-menu-entered pane) t)))

#-(or aclpc acl86win32)
(defmethod handle-event ((pane pull-down-menu) (event pointer-exit-event))
  ;; Don't punt if we've never entered the menu, or if we are entering
  ;; one of the buttons within the menu
  (when (and (pull-down-menu-entered pane)
             (not (eq (pointer-boundary-event-kind event) :inferior)))
    (throw 'exit-pull-down-menu (values))))

#+(or aclpc acl86win32)
(defmethod handle-event :after ((pane pull-down-menu) (event pointer-event))
  (deallocate-event event))


;;; Interface to pull-down menus 

#+(or aclpc acl86win32)
(define-application-frame pull-down-menu-frame ()
    (menu)
  (:pane
    (with-slots (menu) *application-frame*
      (outlining ()
        (setq menu (make-pane 'pull-down-menu
                     :width 100 :height 100)))))
  (:menu-bar nil))

#+(or aclpc acl86win32)
(defun make-pull-down-menu (&key port)
  (let ((frame (make-application-frame 'pull-down-menu-frame
                                       :frame-manager (find-frame-manager :port port)
                                       :save-under t)))
    (values (slot-value frame 'menu) frame)))

;; Pull-down menus share their event queue with the owning application
#+(or aclpc acl86win32)
(defresource pull-down-menu (port)
  :constructor (make-pull-down-menu :port port)
  :matcher (eq (port pull-down-menu) port)
  :deinitializer 
    (progn
      (setf (pull-down-menu-buttons pull-down-menu) nil)
      (setf (pull-down-menu-entered pull-down-menu) nil)
      (dolist (child (sheet-children pull-down-menu))
        (sheet-disown-child pull-down-menu child))))

#+(or aclpc acl86win32)
(defmacro initialize-pull-down-menu (menu &body buttons)
  (assert (= (length buttons) 1))
  `(with-look-and-feel-realization ()
     (setf (pull-down-menu-buttons ,menu) ,@buttons)
     (sheet-adopt-child ,menu (make-pane 'vbox-pane
                                :contents (pull-down-menu-buttons ,menu)
                                :spacing 0))
     (layout-frame (pane-frame ,menu))
     ,menu))

#-(or aclpc acl86win32)
(defvar *subsidiary-pull-down-menu* nil)
#-(or aclpc acl86win32)
(defun choose-from-pull-down-menu (menu &optional button &key cascade-p)
  (let ((menu-frame (pane-frame menu))
        (event-queue (sheet-event-queue menu)))
    (when button
      (with-bounding-rectangle* (bleft btop bright bbottom)
          (sheet-device-region button)
        (declare (ignore bright))
        (multiple-value-bind (fleft ftop fright fbottom)
            (let ((tls (get-top-level-sheet button)))
              (mirror-region* (port tls) tls))
          (declare (ignore fright fbottom))
          (if cascade-p
              (let ((pattern-width (pattern-width (slot-value button 'normal-pattern)))
                    (button-x-margin (slot-value button 'x-margin)))
                (move-sheet (frame-top-level-sheet menu-frame)
                            (+ bleft fleft pattern-width button-x-margin -16)
                            (+ btop ftop)))
              (move-sheet (frame-top-level-sheet menu-frame)
                          (+ bleft fleft 10)
                          (+ bbottom ftop -10))))))
    (enable-frame menu-frame)
    ;; Share the event queue with the application frame
    (setf (sheet-event-queue (frame-top-level-sheet (pane-frame menu)))
          (sheet-event-queue (frame-top-level-sheet *application-frame*)))
    ;; Ensure no surprise exit events
    (setf (pull-down-menu-entered menu) nil)
    ;; Wait for an event and then handle it
    (unwind-protect
        (flet ((waiter ()
                 (not (queue-empty-p event-queue))))
          #-Allegro (declare (dynamic-extent #'waiter))
          (catch (if *subsidiary-pull-down-menu* 
                     '|Not exit-pull-down-menu|
                     'exit-pull-down-menu)
            (let ((*subsidiary-pull-down-menu* t))
              (loop
                (port-event-wait (port menu) #'waiter 
                            :wait-reason "Pull Down Menu" :timeout 2)
                (let ((event (queue-get event-queue)))
                  (handle-event (event-sheet event) event))))))
      (disable-frame menu-frame))))

;;--- Kludge alert
#+(or aclpc acl86win32)
(defun get-top-level-sheet (pane)
  (if (typep pane 'top-level-sheet)
      pane
      (get-top-level-sheet (sheet-parent pane))))


;;; Menu bars

#+(or aclpc acl86win32)
(defclass menu-bar-pane (menu-bar)
    ())


;;mm: add  this to allow multiple functions to value of command-table slot
#+(or aclpc acl86win32)
(defun default-command-table-p (command-table)
   (and command-table (not (or (listp command-table) 
                               (typep command-table 'command-table)))))

;;--- What about when the command menu tick changes?
;;--- What about graying of disabled commands?
#+(or aclpc acl86win32)
(defun compute-menu-bar-pane (frame command-table)
  (let* ((text-style (and (listp command-table)
			  (getf (cdr command-table) :text-style)
			  `(:text-style ,(getf (cdr command-table) :text-style))))
         (command-table (if (listp command-table) (car command-table) command-table)))
    (when ;;mm was:(eq command-table 't)
	;; command-table arg comes from menu-bar slot of frame
	;; and may be NIL T=menu-hbox-pane command-table-arg
        (default-command-table-p command-table)
      (setq command-table (frame-command-table frame)))
    (with-look-and-feel-realization ((frame-manager frame) frame)
      (labels
	  ((make-command-table-buttons (command-table top-level)
	     (let ((buttons nil))
	       (map-over-command-table-menu-items
		#'(lambda (menu keystroke item)
		    (declare (ignore keystroke))
		    (let ((type (command-menu-item-type item))
			  (value (command-menu-item-value item)))
		      (case type
			(:command 
                         (push
			  (apply
			   #'make-pane
			   (if top-level
			       'push-button
			     #+(or aclpc acl86win32) 'pull-down-button-logic
			     #-(or aclpc acl86win32) 'pull-down-menu-button)
			   :label menu
			   :activate-callback
			   #'(lambda (button)
			       (menu-bar-command-callback
				button command-table value))
			   text-style)
                               buttons))
			(:function
			 ;;--- Do something about this
                         )
			(:menu
                         (push (apply #'make-pane
                                      (if top-level
					  #-(or aclpc acl86win32) 'menu-bar-button
					  #+(or aclpc acl86win32) 'menu-bar-button-logic
					  #+(or aclpc acl86win32) 'pull-down-button-logic
					  #-(or aclpc acl86win32) 'pull-down-menu-button)
                                      :label menu
                                      :next-menu (make-command-table-menu value nil)
                                      :activate-callback nil
                                      text-style)
                               buttons))
			(:divider
                         (unless top-level
			   ;;--- Do something about this
                           )))))
		command-table)
	       (nreverse buttons)))
	   (make-command-table-menu (command-table top-level)
	     (let ((buttons (make-command-table-buttons command-table top-level)))
	       (if top-level
		   #-(or ACLPC acl86win32)
		   (outlining ()
		     (make-pane 'hbox-pane :contents buttons))
		   ;;mm: We need to remember the ultimate menu-bar pane so that
		   ;;    we can resize if all the buttons are dropped, or we go
		   ;;    from no buttons to some.
		   #+(or ACLPC acl86win32)
		   (outlining ()
		     (setf (slot-value frame 'menu-bar)
		       (make-pane 'hbox-pane :contents buttons 
				  :min-height 2 :height 2)))
		   (let ((menu (make-pull-down-menu :port (port frame))))
		     (initialize-pull-down-menu menu buttons)
		     menu)))))
        (declare (dynamic-extent #'make-command-table-buttons #'make-command-table-menu))
        (make-command-table-menu command-table t)))))


#+(or aclpc acl86win32)
(defun menu-bar-command-callback (button command-table command)
  (let ((frame (pane-frame button)))
    (distribute-event
      (port button)
      (allocate-event 'presentation-event
        :frame frame
        :sheet (frame-top-level-sheet frame)
        :presentation-type `(command :command-table ,command-table)
        :value command))))




