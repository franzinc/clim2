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
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader: xm-silica.lisp,v 1.16 92/04/21 20:28:33 cer Exp Locker: cer $

(in-package :xm-silica)

;; Motif specific stuff

(defclass motif-port (xt-port) 
  ((type :allocation :class 
	 :initform :motif :reader port-type)))

(defmethod find-port-type ((type (eql ':motif)))
  'motif-port)


(defmethod change-widget-geometry ((parent tk::xm-drawing-area) child
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

(defclass motif-geometry-manager ()
	  ;; --- This is probably all
	  ;; composites excepts drawing-area and shell
	  ()
  (:documentation "These are all parents that have strong feelings
about their children"))


(defmethod update-mirror-transformation-1 ((port port) sheet 
					   (parent motif-geometry-manager))
  nil)

(defmethod update-mirror-region-1 ((port port) sheet 
				   (parent motif-geometry-manager))
  nil)
	    

(defmethod update-mirror-transformation-1 :after ((port port)
						  (sheet motif-geometry-manager)
						  (parent t))
  (update-geo-manager-sheet-children sheet))


(defmethod update-mirror-region-1 :after ((port port)
					  (sheet motif-geometry-manager)
					  (parent t))
  (update-geo-manager-sheet-children sheet))

(defmethod update-geo-manager-sheet-children (geo-manager)
  (dolist (child (sheet-children geo-manager))
    ;;--- Yuck!
    (when (typep child 'mirrored-sheet-mixin)
      (mirror-region-updated (port geo-manager) child))))

(defmethod find-shell-class-and-initargs ((port motif-port) sheet)
  (declare (ignore port))
  (cond ( ;;--- hack alert
	 (popup-frame-p sheet)
	 (values 'xm-dialog-shell
		 (append
		  (let ((x (find-shell-of-calling-frame sheet)))
		    (and x `(:transient-for ,x)))
		  '(:keyboard-focus-policy :pointer))))
	(t
	 (call-next-method))))

(defmethod make-cursor-widget-for-port ((port motif-port) parent)
  (make-instance 'tk::xm-drawing-area
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
