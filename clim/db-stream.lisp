;;; -*- Mode: Lisp; Package: CLIM-INTERNALS; Base: 10.; Syntax: Common-Lisp -*-
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
;;; Copyright (c) 1990 by Xerox Corporations.  All rights reserved.
;;;
;; $fiHeader$


(in-package :clim-internals)

"Improvements Copyright (c) 1990 by International Lisp Associates.  All rights reserved."

;;;
;;; New Stream Pane
;;;

(defclass clim-pane-stream-mixin (basic-output-recording window-stream)
    ()
  (:default-initargs :medium t))

;;; Do we still need this
(defmethod pane-stream ((pane clim-pane-stream-mixin))
  pane)

;;; Communicate with output-protocol what the new text constraints are.

(defmethod note-sheet-region-changed ((pane clim-pane-stream-mixin) &key &allow-other-keys)
  (let ((viewport (pane-viewport pane)))
    (when viewport
	  (setf (stream-default-text-margin pane)
	    (bounding-rectangle-width (sheet-region (pane-viewport pane)))))))

;;; Communicate with output-protocol what the new text constraints are.

(defmethod viewport-region-changed ((pane t) viewport)
  (declare (ignore viewport)))

(defmethod viewport-region-changed ((pane clim-pane-stream-mixin)
				    viewport)
  (setf (sheet-region pane)
    (make-bounding-rectangle
     0 0 
     (max (bounding-rectangle-width pane)
	      (bounding-rectangle-width viewport))
     (max (bounding-rectangle-height pane)
	  (bounding-rectangle-height viewport))))

  (setf (stream-default-text-margin pane)
     (bounding-rectangle-width (sheet-region viewport))))

(defclass extended-stream-sheet (clim-pane-stream-mixin 
				 silica::pane
				 sheet-permanently-enabled-mixin
				 sheet-mute-input-mixin

					;sheet
					;sheet-multiple-child-mixin
					;sheet-transformation-mixin
					;standard-repainting-medium
					;standard-sheet-input-mixin
					;permanent-medium-sheet-output-mixin
				 
				 mute-repainting-mixin
				 
				 silica::space-req-mixin
				 )
	  ()
  (:default-initargs :medium t 
    ;;:max-width +fill+ :min-width 0 :max-height +fill+ :min-height 0
    ))

(defmethod silica::default-space-requirements ((pane extended-stream-sheet)
					       &key
					       (width 0 widthp)
					       (min-width width)
					       (max-width
						(if widthp width
						  +fill+))
					       (height 0 heightp)
					       (min-height height)
					       (max-height
						(if heightp height +fill+)))

  (values width min-width max-width height min-height max-height))

;;; We should straighten this out so that this can be
;;; subclass of LEAF-PANE.  Then it would get
;;; providing-output-contract.

(defclass extended-stream-pane (extended-stream-sheet)
    ())
	   
(defmethod initialize-instance :after 
	   ((pane extended-stream-pane) &key &allow-other-keys)
  (setf (sheet-transformation pane) +identity-transformation+))

#+ignore
(defmethod note-sheet-grafted :after ((pane extended-stream-pane))
  (let ((xform (sheet-transformation pane)))
    (setq xform (make-scaling-transformation 1 -1))
    (setf (sheet-transformation pane) xform)))

;;; This is a soon-to-be-obsolete method, but we need it for now when the
;;; extended-stream-pane is a child of the old-style viewport.  It shouldn't
;;; get called under the new viewport scheme.

#+ignore
(defmethod allocate-space :after ((pane extended-stream-pane) width height)
  (declare (ignore width height))
  (ecase (graft-origin (graft pane))
    (:nw)
    (:sw
     (let ((xform (sheet-transformation pane)))
       (setq xform (make-scaling-transformation 1 -1))
       ;; --- stream-panes ALWAYS have to have a parent to manage the
       ;; viewport clipping, etc.
       (setq xform (compose-transformations
		    xform
		    (make-translation-transformation
		     0 (1- (bounding-rectangle-height
			    (sheet-parent pane))))))
       (setf (sheet-transformation pane) xform)))))

(defmethod pane-stream ((pane extended-stream-pane))
  (unless (port pane) 
    (error "Can't call pane-stream on ~a until it's been grafted!"
	   pane))
  pane)

#+ignore
(defmethod update-region ((pane extended-stream-pane) width height
			  &key no-repaint &allow-other-keys)
  (when (pane-scroller pane)
    (update-extent (pane-viewport pane) width height
		       :no-repaint no-repaint)))

;;; ---This assumes that the stream-pane is always inside a viewport, which
;;; actually defines its visible size.  The stream pane's size is supposed
;;; to represent the size of the contents, but may be stretched to fill the
;;; available viewport space.

(defmethod silica::change-space-req :around ((pane extended-stream-pane) &rest keys &key width height)
  (declare (dynamic-extent keys))
  ;; Assume always called with width height
  (multiple-value-bind (history-width history-height)
      (bounding-rectangle-size (output-recording-stream-output-record pane))
    ;; Don't ever shrink down smaller than our contents.
    (apply #'call-next-method pane :width (max width history-width) :height (max height history-height) keys)))

(defclass basic-clim-interactor (extended-stream-pane) 
	  ((incremental-redisplay-p
	    :initarg :incremental-redisplay
	    :initform nil)
	   (display-function :initarg :display-function
			     :initform nil
			     :reader pane-display-function)
	   (display-time :initarg :display-time
			 :reader pane-display-time
			 :initform :command-loop
			 :type (member nil :command-loop t))))
 
(defclass interactor-pane (basic-clim-interactor) ())
(defclass application-pane (basic-clim-interactor) ())


#+ignore
(defmacro make-clim-pane ((&optional slot-name
				     &rest parent-options
				     &key (type ''extended-stream-pane) 
				     label (scroll-bars ':vertical)
				     &allow-other-keys)
			  &rest pane-options)
  (with-rem-keywords (parent-options parent-options '(:type :label :scroll-bars))
    (let ((default '#:default)
	  (pane (gensymbol "SCROLLABLE-PANE")))
      (macrolet ((setf-unless (slot-keyword value)
		   `(when (eq (getf parent-options ',slot-keyword default) default)
		      (setf (getf parent-options ',slot-keyword) ,value))))
	(setf-unless :hs 100)
	(setf-unless :vs 100)
	(setf-unless :hs+ +fill+)
	(setf-unless :hs- +fill+)
	(setf-unless :vs+ +fill+)
	(setf-unless :vs- +fill+))
      `(bordering (:thickness 1)
		  (let ((,pane (make-pane ,type ,@pane-options)))
		    ,@(when slot-name
			`((setq ,slot-name ,pane)))
		    (vertically ()
				(,(ecase scroll-bars
				    (:both 'scrolling)
				    (:vertical 'vscrolling)
				    (:horizontal 'hscrolling)
				    ((nil) 'ws::viewing))
				 (:subtransformationp t
				  ,@(unless scroll-bars
				      `(:controller ,pane))
				  ,@(copy-list parent-options))
				 ,pane)
				,@(when label
				    `((make-pane 'ws::label-pane :text ,label
						 :hs+ +fill+)))))))))
  
(defmacro make-clim-interactor ((&optional slot-name &rest clim-pane-options)
				&rest pane-options)
  (declare #+Genera
	   (arglist (&optional slot-name
			       &key (hs 100) (vs 100) label (scroll-bars ':vertical))
		    &rest pane-options))
  `(make-clim-pane (,slot-name :type 'clim-interactor ,@clim-pane-options)
		   ,@pane-options))

