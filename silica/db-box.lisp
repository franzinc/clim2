;;; -*- Mode: Lisp; Package: silica; Base: 10.; Syntax: Common-Lisp -*-
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
;;;
;;; Copyright (c) 1989, 1990 by Xerox Corporation.  All rights reserved. 
;;;

(in-package :silica)

;;;
;;; Boxing Panes
;;;

(defclass box-pane (mute-input-mixin
		    pane-background-mixin
	
		    composite-pane 
		    list-contents-mixin
		    space-req-cache-mixin)
    ((space :initform 1 :initarg :space)))

(defmethod initialize-instance :after 
	   ((pane box-pane) &key contents &allow-other-keys)
  (insert-panes pane contents))

(defmethod insert-pane ((lcm box-pane) pane
			&key position batch-p &allow-other-keys)

  (if (panep pane)
      (call-next-method)
      (progn
	(unless (or (eq pane :fill)
		    (numberp pane))
	  (error "Illegal element in a box's contents"))
	(with-slots (contents reverse-p) lcm
	  (unless batch-p
	    (setq position (check-position position reverse-p contents)))
	  (if (zerop position)
	      (push pane contents)
	      (let ((tail (nthcdr (1- position) contents)))
		(setf (cdr tail) 
		      (cons pane (cdr tail)))))
	  (unless batch-p
	    (space-req-changed (sheet-parent lcm) lcm))))))


(defmacro compose-box (contract (major major+ major- minor minor+ minor-) keys)
  `(with-slots (contents space) ,contract
     (if (null contents) (make-space-req)
	 (let ((major 0)
	       (major+ 0)
	       (major- 0)

	       (minor 0)  
	       minor+ minor-
	       (minor-min 0)
	       (minor-max most-positive-fixnum))
	   (dolist (entry contents)
	     (cond ((eq entry :fill) (incf major+ +fill+))
		   ((numberp entry) (incf major entry))
		   (t 
		    (let ((space-req (compose-space entry)))
		      (incf major (+ (,major space-req) space))
		      (incf major+ (,major+ space-req))
		      (incf major- (,major- space-req))
		      (setq minor (max minor (,minor space-req))) 
		      (setq minor-min
			    (max minor-min (- (,minor space-req)
					      (,minor- space-req))))
		      (setq minor-max
			    (min minor-max (+ (,minor space-req)
					      (,minor+ space-req))))))))
	   (decf major space)
	   ;; ?? These calcs lead to weirdness when fills are involved.
	   (setq minor- (max 0 (- minor minor-min)))
	   (setq minor+ (max 0 (- minor-max minor)))
	   (make-space-req
	    ,@(mapcan #'(lambda (key val) (list key val))
		      keys '(major major+ major- minor minor+ minor-)))))))

(defmacro allocate-box (contract alloc-major alloc-minor move-and-resize-entry
				 (major major+ major-))
  `(with-slots (contents space space-req) ,contract
     (unless space-req (compose-space ,contract))
     (let ((stretch-p (> ,alloc-major (,major space-req)))
	   (pos 0)
	   give extra used)
       (if stretch-p
	   (progn (setq give (,major+ space-req))
		  (setq extra (min (- ,alloc-major (,major space-req))
				   give)))
	   (progn (setq give (,major- space-req))
		  (setq extra (min (- (,major space-req) ,alloc-major)
				   give))))
	     
       ;; Allocate Space to the Children
       (dolist (entry contents)
	 (cond ((eq entry :fill) 
		(when stretch-p 
		  (setq used (/ (* +fill+ extra) give))
		  (incf pos used)
		  (decf give +fill+)
		  (decf extra used)))
	       ((numberp entry) (incf pos entry))
	       (t 
		(let* ((entry-space-req (compose-space entry))
		       (alloc (,major entry-space-req)))
		  (when (> give 0)
		    (if stretch-p
			(progn (setq used (/ (* (,major+ entry-space-req)
						extra)
					     give))
			       (incf alloc used)
			       (decf give (,major+ entry-space-req)))
			(progn (setq used (/ (* (,major- entry-space-req)
						extra)
					     give))
			       (decf alloc used)
			       (decf give (,major- entry-space-req))))
		    (decf extra used))
		  (,move-and-resize-entry entry pos alloc ,alloc-minor)
		  (incf pos (+ alloc space)))))))))

(defclass hbox-pane (box-pane)
    ())

(defmethod compose-space ((contract hbox-pane))
  (compose-box contract (space-req-width 
			 space-req-max-width
			 space-req-min-width
			 space-req-height
			 space-req-max-height
			 space-req-min-height)
	       (:width :max-width :min-width :height :max-height :min-height)))

(defmethod allocate-space ((contract hbox-pane) width height)
  (flet ((move-and-resize-entry (entry min-x width height)
	   (move-and-resize-sheet* entry min-x 0 width height)))
    (allocate-box contract width height move-and-resize-entry
		  (space-req-width 
		   space-req-max-width
		   space-req-min-width
		   #+ignore space-req-height
		   #+ignore space-req-max-height
		   #+ignore space-req-min-height))))

(defclass vbox-pane (box-pane)
    ()
  (:default-initargs :reverse-p t))

(defmethod compose-space ((contract vbox-pane))
  (compose-box contract (space-req-height
			 space-req-max-height
			 space-req-min-height
			 space-req-width 
			 space-req-max-width
			 space-req-min-width)
	       (:height :max-height :min-height :width :max-width :min-width)))

(defmethod allocate-space ((contract vbox-pane) width height &aux (oheight height))
  (flet ((move-and-resize-entry (entry min-y height width)
	   (move-and-resize-sheet* entry 0 min-y 
				   width ; DREADful kludge to make
					 ; sure that we do not
					 ; make the sheet to big and
					 ; cause the request to fail
				   (min (1- (- oheight min-y)) height))))
    (allocate-box contract height width move-and-resize-entry
		  (space-req-height
		   space-req-max-height
		   space-req-min-height
		   #+ignore space-req-width 
		   #+ignore space-req-max-width
		   #+ignore space-req-min-width))))
		   


