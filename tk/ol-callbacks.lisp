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
;; $fiHeader: ol-callbacks.lisp,v 1.4 92/02/24 13:03:34 cer Exp Locker: cer $

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

(def-c-type (ol-callback-struct :in-foreign-space) :struct
	    (reason :int))

(def-c-type (ol-expose-callback-struct :in-foreign-space) :struct
	    (reason :int)
	    (event * x11:xevent))


(def-c-type (ol-resize-callback-struct :in-foreign-space) :struct
	    (reason :int)
	    (x xt-position)
	    (y xt-position)
	    (width xt-dimension)
	    (height xt-dimension))

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
  (values))

