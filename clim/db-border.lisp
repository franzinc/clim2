;;; -*- Mode: Lisp; Package: WINDSHIELD; Base: 10.; Syntax: Common-Lisp -*-
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
;; $fiHeader: db-border.lisp,v 1.2 92/01/02 15:33:03 cer Exp $


(in-package :silica)


;;;
;;; Border Panes
;;;

(define-pane-class border-pane (composite-pane 
			       simple-wrapper-mixin
			       mute-input-mixin)
    ((thickness :initform 1 :initarg :thickness :accessor thickness)))

(defmethod compose-space ((pane border-pane))
  (with-slots (thickness) pane
    (unless (numberp thickness)
      (error "Non-number border size not yet supported."))
    (let ((child (sheet-child pane)))
      (space-req+* (compose-space child) 
		   :hs (* 2 thickness)
		   :vs (* 2 thickness)))))

(defmethod allocate-space ((pane border-pane) width height)
  (with-slots (thickness) pane
    (move-and-resize-sheet* (sheet-child pane)
			    thickness thickness
			    (- width (* 2 thickness))
			    (- height (* 2 thickness)))))
  
(define-pane-class outlined-pane (pane-background-mixin
				  layout-mixin
				  border-pane)
    ()
  (:default-initargs :background *black*))


(defvar *default-shadow-thickness* 2)

(defclass shadowed-style (repaint-handler)
    ((darker-color :initform *gray* :initarg :darker-color)
     (lighter-color :initform *white* :initarg :lighter-color)
     (popin-p :initform nil :initarg :popin-p))
  (:default-initargs :thickness *default-shadow-thickness*))

(define-pane-class shadowed-pane (shadowed-style
				 border-pane)
		       
    ())
  
(defmethod handle-repaint ((shadow shadowed-style) repaint-region
			   &key medium &allow-other-keys)
  #-PCL;; PCL uses this variable for method-table cache misses, unfortunately.
  (declare (ignore repaint-region))
  (with-slots (thickness popin-p darker-color lighter-color) shadow
    (let* ((width (bounding-rectangle-width shadow))
	   (height (bounding-rectangle-height shadow)))

      ;; left side and top
      (draw-polygon* 
       medium (list 0 0 0 height width height width (- height thickness)
		    thickness (- height thickness) thickness 0)
       :ink (if popin-p darker-color lighter-color)
       :filled t)

      ;; ???? disable this code on non-monochrome
      (unless popin-p
	(draw-line* medium thickness thickness thickness (- height thickness))
	(draw-line* medium thickness (- height thickness) 
		    (- width thickness) (- height thickness)))

      ;; right-side and bottom
      (draw-polygon*
       medium (list width 0 width height 
		    (- width thickness) (- height thickness)
		    (- width thickness) thickness thickness thickness 0 0)
       :ink (if popin-p lighter-color darker-color)
       :filled t)

      ;; disable this code on non-monochrome
      (when popin-p
	(draw-line* medium thickness thickness (- width thickness) thickness)
	(draw-line* medium (- width thickness) thickness
		    (- width thickness) (- height thickness))))))


;;;
;;;  Labelled Border
;;;

(define-pane-class labelled-border-pane (composite-pane 
					simple-wrapper-mixin
					mute-input-mixin
					text-part)
    ((thickness :initform 1 :initarg :thickness :accessor thickness)))

(defmethod compose-space ((pane labelled-border-pane))
  (with-slots (thickness) pane
    (unless (numberp thickness)
      (error "Non-number border size not yet supported."))
    (let ((child (sheet-child pane)))
      (space-req+* (compose-space child) 
		   :hs (* 2 thickness) 
		   :vs (+ (* 2 thickness) (text-height pane))))))

(defmethod allocate-space ((pane labelled-border-pane) width height)
  (with-slots (thickness) pane
    (let ((th (text-height pane)))
      (move-and-resize-sheet* (sheet-child pane)
			      thickness (+ th thickness)
			      (- width (* 2 thickness))
			      (- height (* 2 thickness) th)))))

(defmethod handle-repaint ((pane labelled-border-pane) repaint-region
			   &key medium &allow-other-keys)
  ;; Will we lose if we ignore the repaint region?
  (declare (ignore repaint-region))
  (with-slots (text-style background ink text halign valign) pane
    (with-bounding-rectangle* (minx miny maxx maxy) (sheet-region pane)
      (let* ((th   (text-height pane))
	     (th/2  miny #+ignore (floor th 2))
	     (tw   (text-width pane))
	     (lw   (floor (- maxx minx tw) 2)) 
	     )
	
	(draw-rectangle* medium minx miny maxx maxy
			 :filled t
			 :ink background)
	(let ((maxx (1- maxx))
	      (maxy (1- maxy)))
	  (draw-line* medium minx th/2 minx maxy) 
	  (draw-line* medium maxx th/2 maxx maxy)
	  (draw-line* medium minx maxy maxx maxy)
	  
	  (draw-line* medium minx th/2 (1- lw) th/2)
	  (draw-line* medium (- maxx (1- lw)) th/2 maxx th/2)
	  
	
	  )
    
	(draw-text-rectangle* medium text 
			      minx miny maxx th
			      :align-x :center
			      :align-y :center
			      :text-style text-style
			      :ink ink)))))  

;;;
;;; Spacer Pane
;;;

(define-pane-class spacer-pane (pane-background-mixin
				 mute-input-mixin
				 composite-pane 
				 simple-wrapper-mixin
				 client-space-space-req-mixin
				 space-req-cache-mixin)
    ((halign :initform :center :initarg :halign :accessor spacer-halign)
     (valign :initform :center :initarg :valign :accessor spacer-valign))
  #+ignore
  (:default-initargs :hs+ +fill+ :vs+ +fill+)
  (:default-initargs :halign :left :valign :bottom
		     :hs 2 :vs 2))


(defmethod compose-space ((pane spacer-pane))
  (let ((child (sheet-child pane)))
    (with-slots (client-space-req) pane
      (space-req+ (compose-space child) client-space-req))))

(defmacro allocate-amount (contract amount (space space+ space-))
  `(with-slots (space-req client-space-req) ,contract
     (let ((stretch-p (> ,amount (,space space-req)))
	   give extra used)
       (if stretch-p
	   (progn (setq give (,space+ space-req))
		  (setq extra (min (- ,amount (,space space-req))
				   give)))
	   (progn (setq give (,space- space-req))
		  (setq extra (min (- (,space space-req) ,amount)
				   give))))
       ;; Allocate Space to the Children
       (let* ((alloc (,space client-space-req)))
	 (when (> give 0)
	   (if stretch-p
	       (progn (setq used (/ (* (,space+ client-space-req) extra)
				    give))
		      (incf alloc used))
	       (progn (setq used (/ (* (,space- client-space-req) extra)
				    give))
		      (decf alloc used))))
	 alloc))))
		    
(defmethod allocate-space ((pane spacer-pane) width height)
  (with-slots (space-req halign valign) pane
    
    (unless space-req (compose-space pane)) ;; Needs the space-req-cache filled
    
    (let* ((child (sheet-child pane))
	   (child-space (compose-space child))
	   (child-width (space-req-hs child-space))
	   (child-height (space-req-vs child-space))
	   (hspace (allocate-amount pane width (space-req-hs 
						space-req-hs+
						space-req-hs-)))
	   (hoff (floor hspace 2))
	   (vspace (allocate-amount pane height (space-req-vs 
						 space-req-vs+
						 space-req-vs-)))
	   (voff (floor vspace 2))
	   new-x new-y new-w new-h)
      
      (setf new-w (- width hspace))
      
      ;; Note when given the space wanted, the align won't make a difference.
      (setf new-x (ecase halign 
		    (:left hoff)
		    (:center (max 0 (floor (- width child-width) 2)))
		    (:right (max 0 (- width hoff child-width)))))
      
      (setf new-h (- height vspace))
      (setf new-y (ecase valign
		    (:bottom voff)
		    (:center (max 0 (floor (- height child-height) 2)))
		    (:top (max 0 (- height voff child-height)))))
	
      (move-and-resize-sheet* child new-x new-y new-w new-h))))






