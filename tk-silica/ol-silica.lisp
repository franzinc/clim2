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
;; $fiHeader: ol-silica.lisp,v 1.12 92/07/20 16:01:50 cer Exp $

(in-package :xm-silica)

(defclass openlook-port (xt-port) ())

(defmethod find-port-type ((type (eql :openlook)))
  'openlook-port)

(defmethod port-type ((port openlook-port))
  ':openlook)

(warn "Changing the default server path to ~S"
      (setq *default-server-path* '(:openlook)))

(defmethod make-cursor-widget-for-port ((port openlook-port) parent)
  (make-instance 'tk::draw-area
		 :parent parent
		 :background (tk::get-values parent :foreground)
		 :width 2
		 :height 11
		 :managed t))

(defmethod port-note-cursor-change :after ((port openlook-port)
					   cursor stream type old new)
  (declare (ignore old type cursor))
  (when new 
    (let ((mirror (sheet-mirror stream)))
      (when mirror 
	(let ((window (tk::widget-window mirror)))
	  (with-port-event-lock (port)
	    (excl:without-interrupts
	      (tk:with-server-grabbed ((port-display port))
		(when (eq (tk::window-map-state window) :viewable)
		  ;;--- There could very well be a race condition involving
		  ;;--- a couple of processes.  Another process could have made
		  ;;--- this window go away at this point
		  (tk::ol_set_input_focus 
		   mirror
		   2			; RevertToParent
		   0))))))))))

(defmethod change-widget-geometry ((parent tk::draw-area) child
				   &rest args
				   &key x y width height)
  (declare (ignore x y width height))
  (apply #'tk::configure-widget child args))

;;;--- Why have this class

(defclass openlook-geometry-manager (xt-geometry-manager) ())

(defmethod find-shell-class-and-initargs ((port openlook-port) sheet)
  (declare (ignore port))
  (cond ( ;;--- hack alert
	 (popup-frame-p sheet)
	 (values 'tk::transient-shell
		 (append
		  (let ((x (find-shell-of-calling-frame sheet)))
		    (and x `(:transient-for ,x)))
		  '(:keyboard-focus-policy :pointer))))
	(t
	 (call-next-method))))

(defmethod enable-xt-widget ((parent tk::transient-shell) (mirror t))
  (manage-child mirror)
  (popup parent))

(defmethod disable-xt-mirror ((parent xt::transient-shell) (mirror t))
  (tk::popdown parent))

(defmethod destroy-mirror ((port openlook-port) (sheet mirrored-sheet-mixin))
  ;; Only do this if its the top most widget being destroyed or we are
  ;; screwing around with the tree in someway
  (xt::unmanage-child (sheet-direct-mirror sheet)))
