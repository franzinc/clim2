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
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: xm-silica.lisp,v 1.22 92/07/01 15:48:09 cer Exp $

(in-package :xm-silica)

;; Motif specific stuff

(defclass motif-port (xt-port) ())

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

(defmethod change-widget-geometry ((parent tk::shell) child
				   &rest args
				   &key x y width height)
  (declare (ignore x y args))
  ;;-- shells decide where windows are positioned!
  (tk::set-values child :width width :height height))

(defclass motif-geometry-manager (xt-geometry-manager) ())


(defmethod find-shell-class-and-initargs ((port motif-port) sheet)
  (declare (ignore port))
  (cond ( ;;--- hack alert
	 (popup-frame-p sheet)
	 (values 'tk::xm-dialog-shell
		 (append
		  (let ((x (find-shell-of-calling-frame sheet)))
		    (and x `(:transient-for ,x)))
		  '(:keyboard-focus-policy :pointer))))
	(t
	 (call-next-method))))

(defmethod make-cursor-widget-for-port ((port motif-port) parent)
  (make-instance 'tk::xm-my-drawing-area
		 :parent parent
		 :background (tk::get-values parent :foreground)
		 :width 2
		 :height 11
		 :managed t))

;(ff:defforeign 'xmprocesstraversal
;    :entry-point "_XmProcessTraversal")
;(defmethod port-note-cursor-change :after ((port motif-port)
;					   cursor stream type old new)
;  (declare (ignore old type cursor))
;  (when new
;    (xmprocesstraversal (sheet-mirror stream) 0)))

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

(ff::defforeign 'initializemydrawingareaquerygeometry
    :entry-point "_InitializeMyDrawingAreaQueryGeometry")

(initializemydrawingareaquerygeometry 
 (ff:register-function 'my-drawing-area-query-geometry-stub))

(defun my-drawing-area-query-geometry (widget intended desired)
  (let* ((sheet (find-sheet-from-widget-address widget))
	 (sr (compose-space sheet))
	 (rm (tk::xt-widget-geometry-request-mode intended)))
    ;; If its asking and its out of range then say so
    (when (or (and (logtest rm x11:cwwidth)
		   (not (<= (space-requirement-min-width sr)
			    (tk::xt-widget-geometry-width intended)
			    (space-requirement-max-width sr))))
	      (and (logtest rm x11:cwheight)
		   (not (<= (space-requirement-min-height sr)
			    (tk::xt-widget-geometry-height intended)
			    (space-requirement-max-height sr)))))
      (return-from my-drawing-area-query-geometry tk::xt-geometry-no))
      
    (when (and (logtest rm x11:cwheight) (logtest rm x11:cwwidth))
      (return-from my-drawing-area-query-geometry tk::xt-geometry-yes))
      
    (setf (tk::xt-widget-geometry-width desired) (fix-coordinate 
						  (space-requirement-width sr))
	  (tk::xt-widget-geometry-height desired) (fix-coordinate
						   (space-requirement-height sr))
	  (tk::xt-widget-geometry-request-mode desired) (logior x11:cwwidth x11:cwheight))


    (return-from my-drawing-area-query-geometry tk::xt-geometry-almost)))


