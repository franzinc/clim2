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
;; $fiHeader: xm-silica.lisp,v 1.6 92/01/31 14:56:35 cer Exp Locker: cer $

(in-package :xm-silica)

;; Motif specific stuff

(defclass motif-port (xt-port) 
	  ())

(defmethod find-port-type ((type (eql ':motif)))
  'motif-port)

(ff:defforeign 'xmprocesstraversal
    :entry-point "_XmProcessTraversal")

(defmethod port-note-cursor-change ((port motif-port)
				    cursor
				    stream
				    type
				    old
				    new)
  (declare (ignore old type cursor))
  (call-next-method)
  (when new
    (xmprocesstraversal (tk::object-handle (sheet-mirror stream)) 0)
    #+ignore
    (xtsetkeyboardfocus #+ignroe(tk::object-handle (sheet-mirror (sheet-top-level-mirror stream)))
			(tk::object-handle (sheet-mirror stream))
			(tk::object-handle (sheet-mirror stream))))
  (setf (silica::port-keyboard-focus port) 
    (and new stream)))


(defmethod change-widget-geometry (parent child &rest args)
  (declare (ignore parent))
  ;; In this case let the parent deal with it
  (apply #'tk::set-values child args))
		 
(defmethod change-widget-geometry ((parent tk::xm-drawing-area) child
				   &rest args
				   &key x y width height)
  (declare (ignore x y width height))
  (apply #'tk::configure-widget child args))

(defclass motif-geometry-manager ()
	  ;; --- This is probably all
	  ;; composites excepts drawing-area and shell
	  ()
  (:documentation "These are all parents that have strong feelings
about their children"))


(defmethod silica::update-mirror-transformation-1 ((port port) sheet 
						   (parent
						    motif-geometry-manager))
  nil)

(defmethod silica::update-mirror-region-1 ((port port) sheet 
						   (parent
						    motif-geometry-manager))
  nil)
	    

(defmethod silica::update-mirror-transformation-1 :after  ((port port)
							   (sheet motif-geometry-manager)
							   (parent t))
  (update-geo-manager-sheet-children sheet))


(defmethod silica::update-mirror-region-1 :after  ((port port)
						   (sheet motif-geometry-manager)
						   (parent t))
  (update-geo-manager-sheet-children sheet))

(defmethod update-geo-manager-sheet-children (geo-manager)
  (dolist (child (sheet-children geo-manager))
    (mirror-region-updated (sheet-port geo-manager) child)))
