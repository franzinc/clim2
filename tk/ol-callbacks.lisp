;; -*- mode: common-lisp; package: tk -*-
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
;; $fiHeader: ol-callbacks.lisp,v 1.10 93/04/23 09:18:33 cer Exp $

(in-package :tk)

;; We have to dispatch on the callback reason

;; from OlReasons.h

(defvar *ol-callback-reasons* '(
				(:none                  0)
				(:expose                1)
				(:graphics_expose       2)
				(:resize                3)
				(:dnd_preview           4)
				(:dnd_trigger           5)
				(:dnd_ownselection      6)
				(:dnd_animate           7)
				))

(defun find-ol-callback-reason (call-data)
  (or (car (find (tk::ol-callback-struct-reason call-data)
		 *ol-callback-reasons*
		 :key #'second))
      (error "Cannot find ol reason: ~S" call-data)))

(defmethod spread-callback-data ((widget draw-area) 
				 call-data 
				 (type (eql 'drawing-area)))
  (ecase (find-ol-callback-reason call-data)
    (:expose 
     (values (widget-window widget)
	     (ol-expose-callback-struct-event call-data)))
    (:resize (values nil nil))))

(defmethod spread-callback-data (widget data (type (eql 'slider-moved)))
  (values (ol-scroll-bar-verify-new-location data)))

(defun add-ol-callback (widget name type function &rest args)
  (ol_add_callback widget name *callback-handler-address*
		   (caar (push
			  (list (new-callback-id) (cons function args) type)
			  (widget-callback-data widget)))))


(defmethod spread-callback-data ((widget shell) data (type (eql :wm-protocol)))
  ;;; I dunno why this works but it does
  (ecase (logand 15 (ol-wm-protocol-verify-message-type data))
	    (1 :wm-delete-window)	; 80000001
	    (2 :wm-save-yourself)	; 80000002
	    (4 :wm-take-focus)		; 80000004
	    ))

(defmethod spread-callback-data ((widget ol-list) data (type (eql 'ol-list-item-make-current)))
  data)

