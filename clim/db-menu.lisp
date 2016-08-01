;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

;;;"Copyright (c) 1992 by Symbolics, Inc.  All rights reserved."

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

#+broken
(defmethod handle-event ((pane pull-down-menu) (event pointer-exit-event))
  ;; Don't punt if we've never entered the menu, or if we are entering
  ;; one of the buttons within the menu
  (when (and (pull-down-menu-entered pane)
             (not (eq (pointer-boundary-event-kind event) :inferior)))
    (throw 'exit-pull-down-menu (values))))

#+(or aclpc acl86win32)
(defmethod handle-event :after ((pane pull-down-menu) (event pointer-event))
  (deallocate-event event))

;;--- Kludge alert
#+(or aclpc acl86win32)
(defun get-top-level-sheet (pane)
  (if (typep pane 'top-level-sheet)
      pane
      (get-top-level-sheet (sheet-parent pane))))


;;; Menu bars

#+(or aclpc acl86win32)
(defclass menu-bar-pane (menu-bar) ())

;;mm: add  this to allow multiple functions to value of command-table slot
(defun default-command-table-p (command-table)
   (and command-table (not (or (listp command-table) 
                               (typep command-table 'command-table)))))

;;--- What about when the command menu tick changes?
;;--- What about graying of disabled commands?
(defun compute-menu-bar-pane (frame command-table)
  (outlining ()
    (setf (slot-value frame 'menu-bar)
      (compute-menu-bar-pane-1 frame command-table))))

(defun compute-menu-bar-pane-1 (frame command-table)
  ;; Returns an hbox-pane containing a row of buttons.
  (let* ((command-table 
	  (if (listp command-table) (car command-table) command-table)))
    (when (default-command-table-p command-table)
      ;;mm was:(eq command-table 't)
      ;; command-table arg comes from menu-bar slot of frame
      ;; and may be NIL T=menu-hbox-pane command-table-arg
      (setq command-table (frame-command-table frame)))
    (with-look-and-feel-realization ((frame-manager frame) frame)
      (labels
	  ((make-command-table-menu (command-table)
	     (let ((choices nil))
	       (map-over-command-table-menu-items
		#'(lambda (menu keystroke item)
		    (declare (ignore keystroke))
		    (let ((type (command-menu-item-type item))
			  (value (command-menu-item-value item)))
		      (case type
			(:command 
			 (push `(,menu :value 
				       (,value ,command-table))
			       choices))
			(:function	;--- Do something about this
                         )
			(:menu 
			 (push (list menu :items 
				     (make-command-table-menu value))
			       choices))
			(:divider 
			 (push (list nil :type :divider) choices)))))
		command-table)
	       (nreverse choices)))
	   (make-menubar-buttons (command-table)
	     (let ((buttons 
		    (mapcar #'(lambda (menu)
				(let ((name (first menu))
				      (kind (second menu))
				      (value (third menu)))
				  (if (eq kind :items)
				      (make-pane 'menu-bar-button-logic
						 :label name
						 :next-menu value)
				    (error "not yet implemented")
				    )))
			    (make-command-table-menu command-table))))
	       (make-pane 'hbox-pane :contents buttons 
			  :min-height 2 :height 2))))
        (declare (dynamic-extent #'make-command-table-menu
				 #'make-menubar-buttons))
        (make-menubar-buttons command-table)))))
