;;; -*- Mode: Lisp; Package: clim-user ; Base: 10.; Syntax: Common-Lisp -*-
;;;
;;; Copyright (c) 1989, 1990 by Xerox Corporation.  All rights reserved. 
;;; Portions (c) 1992 Franz Inc.
;;; $fiHeader: ico.lisp,v 1.1 92/07/01 16:07:07 cer Exp Locker: cer $
;;; Algorithm pretty much lifted from X11 demo program.
;;;

(in-package :clim-user)

;;;
;;; Entry 
;;;

(defvar *ico-mode* :clim)

(defun ico ()
  (run-frame-top-level (make-application-frame 'ico-frame)))

;;;
;;; ICO Frames
;;;

(define-application-frame ico-frame ()
			  ((ico-time-p :initform t)
			   (ico-line-style :initform :thin)
			   (draw-edges :initform t)
			   (draw-faces :initform t)
			   (ico-process :initform nil :accessor ico-process))
  (:command-table (ico-frame :inherit-from (clim-internals::accept-values-pane)))
  (:panes
   (target :application :width 400 :height 400)
   (options :application
    :width :compute :height :compute
    :display-function `(clim-internals::accept-values-pane-displayer
			:displayer display-options-pane)))
  (:layouts (:default (vertically () target options))))

(defmethod frame-target ((fr ico-frame))
  (get-frame-pane fr 'target))


(defmethod display-options-pane ((frame ico-frame) pane &key max-width max-height)
  (declare (ignore max-width max-height))
  (with-slots (ico-time-p ico-line-style draw-edges draw-faces) frame
    (setf ico-time-p (accept 'boolean 
			     :default ico-time-p
			     :stream pane
			     :prompt "Time"))
    (terpri pane)
    (let ((x (append (and draw-faces '(:faces))
		     (and draw-edges '(:edges)))))
      (setf x (accept '(subset :faces :edges) :default x :stream pane :prompt "Choose"))
      (terpri pane)
      (setf draw-edges (member :edges x)
	    draw-faces (member :faces x)))
    (setf ico-line-style (accept '(member :thin :thick)
				 :default ico-line-style
				 :stream pane
				 :prompt "Line Style"))
    (terpri pane)))

(define-ico-frame-command (com-ico-throw-ball :menu t)
    ()
  (with-application-frame (frame)
    (throw-ball frame)))

(define-ico-frame-command (com-ico-catch-ball :menu t)
    ()
  (with-application-frame (frame)
    (catch-ball frame)))



(defmethod disable-frame :after ((frame ico-frame))
  (catch-ball frame))

(defmethod throw-ball ((frame ico-frame))
  (unless (ico-process frame)
    (setf (ico-process frame)
	  (clim-sys:make-process #'(lambda ()
			    (if (slot-value frame 'ico-time-p)
				(time (run-ico frame))
				(run-ico frame))
			    (setf (ico-process frame) nil))
			:name "ICO ball process"))))

(defmethod catch-ball ((frame ico-frame))
  (when (ico-process frame)
    (clim-sys::destroy-process (ico-process frame))
    (setf (ico-process frame) nil)))

(defmethod repaint-frame ((frame ico-frame) pane region)
  (declare (ignore pane region))
  (throw-ball frame))

;;;
;;; Ico, the stuff
;;;

(defconstant ico-w   150)
(defconstant ico-h   150)
(defconstant ico-w/2 (floor ico-w 2))
(defconstant ico-h/2 (floor ico-h 2))

(defun run-ico (frame &optional (count 1000000))
  (let* ((pane  (frame-target frame))
	 (medium (sheet-medium pane))
	 (fg (medium-foreground medium))
	 (bg (medium-background medium))
	 (win-w (bounding-rectangle-width pane))
	 (win-h (bounding-rectangle-height pane))
	 (ico-x (floor (* (- win-w ico-w) 128)
		       256))
	 (ico-y (floor (* (- win-h ico-h) 128)
		       256))
	 (ico-dx 13)
	 (ico-dy 9)
	 (xtop (- win-w ico-w))
	 (ytop (- win-h ico-h))
	 (prev-x 0)
	 (prev-y 0)
	 edges
	 face-list
	 display black white drawable gcontext)
    (declare (integer win-w win-h)
	     (integer ico-x ico-y ico-dx ico-dy xtop ytop prev-x prev-y)
	     (special prev-x prev-y))
    
    (with-slots (draw-faces draw-edges) frame
      (ecase *ico-mode*
	#+ABC
	(:abc (clear-region medium +everywhere+))
	#+CLIM
	(:clim)
	(:dont)
	#+XLIB-IGNORE
	(:clx
	 (setq display (on-x::x-display (port medium)))
	 (setq black (xlib:screen-black-pixel (on-x::x-screen (port medium))))
	 (setq white (xlib:screen-white-pixel (on-x::x-screen (port medium))))
	 (setq drawable (slot-value medium 'on-x::drawable))
	 (setq gcontext (slot-value medium 'on-x::gcontext))
	 (setf (xlib:gcontext-fill-style gcontext) :solid))
	#+Genera
	(:genera
	 (setq drawable (slot-value medium 'on-genera::drawable))))

      (window-clear pane)
      (dotimes (i count)
      
	#+ignore
	(cond (draw-edges
	       (do* ((minx (car edges))
		     (miny (cadr edges))
		     (maxx minx)
		     (maxy miny)
		     (edges  (cddr edges)  (cddr edges)))
		   ((null edges)
		    (when (and minx miny maxx maxy)
		      (draw-rectangle* medium 
				       (1- minx)
				       (1- miny)
				       (1+ maxx)
				       (1+ maxy)
				       :ink bg
				       :filled t)))
		 (let ((x (car edges))
		       (y (second edges)))
		   (clim-utils::minf minx x)
		   (clim-utils::minf miny y)
		   (clim-utils::maxf maxx x)
		   (clim-utils::maxf maxy y)))))
      
	(multiple-value-setq (edges face-list)
	  (calculate-ico ico-x ico-y draw-edges draw-faces edges face-list))
      
      
	;; Draw ICO
	(ecase *ico-mode*
	  #+CLIM
	  (:clim

	   #-ignore
	   (draw-rectangle* medium 0 0 win-w win-h :ink bg :filled t)
	   
	   #+ignore
	   (draw-rectangle* medium 
			    (- prev-x (1+ ico-w) 30) 
			    (- prev-y (1+ ico-h) 30)
			    (+ (+ prev-x (1+ ico-w)) 30)
			    (+ (+ prev-y (1+ ico-h)) 30)
			    :ink bg 
			    :filled t)
	   (when draw-edges
	     (draw-lines* medium edges :ink +black+
			  :line-thickness 
			  (ecase (slot-value frame 'ico-line-style)
			    (:thick 5)
			    (:thin nil))))
	   (when draw-faces
	     (do ((f face-list (cdr (cdddr (cdddr f)))))
		 ((null f))
	       (draw-polygon* medium (subseq f 1 7)
			      :closed t
			      :filled t
			      :ink (if (oddp (car f)) +red+ +green+))))
	 
	   (silica::medium-force-output medium))
	
	  #+XLIB-IGNORE
	  (:clx 
	   (setf (xlib:gcontext-foreground gcontext) white)
	   (xlib:draw-rectangle drawable gcontext prev-x prev-y 
				(1+ ico-w) (1+ ico-h) t)
	   (setf (xlib:gcontext-foreground gcontext) black)
	   (xlib:draw-segments drawable gcontext
			       (mapcan
				#'(lambda (point)
				    (list (round (ico-point-x point)) 
					  (round (ico-point-y point))))
				edges))
	   (xlib:display-force-output display))
	  #+Genera
	  (:genera
	   (scl:send drawable :draw-rectangle
		     (1+ ico-w) (1+ ico-h)
		     prev-x prev-y
		     :erase)
	   (apply #'scl:send drawable :draw-lines :draw
		  (mapcan
		   #'(lambda (point)
		       (list (round (ico-point-x point)) 
			     (round (ico-point-y point))))
		   edges)))
	  (:dont))

	(setq prev-x ico-x
	      prev-y ico-y)
	(incf ico-x ico-dx)
	(when (or (< ico-x 0) (> ico-x xtop))
	  (decf ico-x (* ico-dx 2))
	  (setq ico-dx (- ico-dx)))
	(incf ico-y ico-dy)
	(when (or (< ico-y 0) (> ico-y ytop))
	  (decf ico-y (* ico-dy 2))
	  (setq ico-dy (- ico-dy)))))))




;;;
;;; DRAW ICO
;;;

(defconstant nv 12)
(defconstant nf 20)

(defparameter v3-seq 
    (let ((x
	   '(;; Initial Position
	     (0.0        0.0        -0.9510565)
	     (0.0        0.8506508  -0.42532536)
	     (0.809017   0.26286557 -0.42532536)
	     (0.5       -0.68819094 -0.42532536)
	     (-0.5      -0.68819094 -0.42532536)
	     (-0.809017  0.26286557 -0.42532536)
	     (0.5        0.68819094  0.42532536)
	     (0.809017  -0.26286557  0.42532536)
	     (0.0       -0.8506508   0.42532536)
	     (-0.809017 -0.26286557  0.42532536)
	     (-0.5       0.68819094  0.42532536)
	     (0.0        0.0         0.9510565))))
      (make-array (list (length x) 3) 
		  :element-type 'single-float
		  :initial-contents x)))


(defparameter faces '((0 2 1)  (0 3 2)  (0 4 3)  (0 5 4)  
		      (0 1 5)  (1 6 10) (1 2 6)  (2 7 6)
		      (2 3 7)  (3 8 7)  (3 4 8)  (4 9 8)
		      (4 5 9)  (5 10 9) (5 1 10) (10 6 11)
		      (6 7 11) (7 8 11) (8 9 11) (9 10 11)))

(defparameter xform nil) ; Initialized Below


(defmacro v2-aref (vertex-number field)
  (case field
    ((xy) `(aref v2 ,vertex-number 0))
    ((z) `(aref v2 ,vertex-number 1))	   
    (t (error "Bad array reference on v2"))))


(defun make-ico-point (x y)
  (excl::fast
   (let ((z (make-array 2 :element-type 'single-float)))
     (declare (type (simple-array single-float (2)) z))
     (setf (aref z 0) (float x 0s0)
	   (aref z 1) (float y 0s0))
     z)))

(defmacro ico-point-x (z) `(the single-float (aref (the (simple-array single-float (2)) ,z) 0)))
(defmacro ico-point-y (z) `(the single-float (aref (the (simple-array single-float (2)) ,z) 1)))

(defparameter v2 (make-array (list nv 2)
		       :initial-contents
		       (clim-utils::with-collection
			   (dotimes (i nv)
			     (clim-utils::collect (list (make-ico-point 0 0) 0))))))

(defparameter drawn      (make-array (list nv nv) :initial-element nil))
(defparameter drawn-fill (make-array (* nv nv) :displaced-to drawn))

(defmacro with-non-consing-collection ((list) &body body)
  `(let* (($with-collection-result$ (or ,list (list :hohoho-and-b-of-rum)))
	  ($with-collection-tail$ $with-collection-result$)
	  $with-collection-rest$)
     (macrolet
	 ((collect (form)
	    ;;  The FORM is evaluated first so that COLLECT nests
	    ;; properly, i.e., The test to determine if this is
	    ;; the first value collected should be done after the
	    ;; value itself is generated in case it does
	    ;; collection as well.
	    `(let (($collectable$ ,form))
	       (if $with-collection-tail$
		   (progn
		     (setf (car $with-collection-tail$) $collectable$)
		     (setq $with-collection-tail$ 
			   (cdr $with-collection-tail$)))
		   (push $collectable$ $with-collection-rest$))
	       $collectable$)))
       ,@body 
       (unless (eq (car $with-collection-result$) :hohoho-and-b-of-rum)
	 (if $with-collection-rest$
	     (nconc $with-collection-result$ (nreverse $with-collection-rest$))
	     $with-collection-result$)))))

(defun calculate-ico (ico-x ico-y do-edges do-faces edge-point-list face-point-list)
  (declare (integer ico-x ico-y)
	   (optimize (safety 0) (speed 3)))
  (let ((v2-fill (cdr (excl::ah_data v2)))
	(v3-seq v3-seq)
	(v2 v2)
	(drawn drawn)
	(fp 0)
	p0 p1 p2)
    (declare 
     (type (simple-array single-float (12 3)) v3-seq)
     (type (simple-array t (12 2)) v2)
     (type (simple-array t (12 12)) drawn)
     (simple-vector v2-fill))
    
    ;;Clear the drawn array
    (fill (cdr (excl::ah_data drawn-fill)) nil)
    

    ;; Rotate vertices
    (partial-nonhom-transformation xform v3-seq)
    
    ;; Convert 3d coordinates to 2D positions
    (dotimes (i (array-dimension v3-seq 0))
      (let ((z (aref v2-fill fp))
	    (x (+ ico-x (* ico-w/2 (+ (aref v3-seq i 0) 1.0))))
	    (y (+ ico-y (* ico-h/2 (+ (aref v3-seq i 1) 1.0)))))
	(declare (single-float x y))
	(if z
	    (setf (ico-point-x z) x (ico-point-y z) y)
	  (setf  (aref v2-fill fp)
	    (make-ico-point x y))))
      (incf fp)
      ;; Save the z for hidden line removal
      (setf (aref v2-fill fp) (aref v3-seq i 2))
      (incf fp))
            
    ;; Accummulate edges, w/o duplicates	    


    (values
     (when do-edges
       (with-non-consing-collection (edge-point-list)
	 (macrolet ((collect-point (p)
		      (let ((pp (gensym)))
			`(let ((,pp ,p))
			   (collect (ico-point-x ,pp))
			   (collect (ico-point-y ,pp))))))
	   (dolist (face faces)
	     (setq p0 (first face))
	     (setq p1 (second face))
	     (setq p2 (third face))
	      
	     ;; Unless hidden for some reason if the sum of the
	     ;; z-coordinates of the faces is < 0 then its hidden???

	     (unless (< (+ (v2-aref p0 z)
			   (v2-aref p1 z)
			   (v2-aref p2 z))
			0.0)
	       ;; At this point if we are drawing faces then we have
	       ;; something to do
	    
	       ;; Mark for Draw
	       (unless (aref drawn p0 p1)
		 (setf (aref drawn p0 p1) t)
		 (setf (aref drawn p1 p0) t)

		 (collect-point (v2-aref p0 xy))
		 (collect-point (v2-aref p1 xy)))
	

	       (unless (aref drawn p1 p2)
		 (setf (aref drawn p1 p2) t)
		 (setf (aref drawn p2 p1) t)
		 (collect-point (v2-aref p1 xy))
		 (collect-point (v2-aref p2 xy)))
	  

	       (unless (aref drawn p2 p0)
		 (setf (aref drawn p2 p0) t)
		 (setf (aref drawn p0 p2) t)
		 (collect-point (v2-aref p2 xy))
		 (collect-point (v2-aref p0 xy))))))))

     (when do-faces
       (with-non-consing-collection (face-point-list)
	 (macrolet ((collect-point (p)
		      (let ((pp (gensym)))
			`(let ((,pp ,p))
			   (collect (ico-point-x ,pp))
			   (collect (ico-point-y ,pp))))))
	   (let ((i 0))
	     (dolist (face faces)
	       (setq p0 (first face))
	       (setq p1 (second face))
	       (setq p2 (third face))
	      
	       ;; Unless hidden for some reason if the sum of the
	       ;; z-coordinates of the faces is < 0 then its hidden???

	       (unless (< (+ (v2-aref p0 z)
			     (v2-aref p1 z)
			     (v2-aref p2 z))
			  0.0)
		 (collect (incf i))
		 (collect-point (v2-aref p0 xy))
		 (collect-point (v2-aref p1 xy))
		 (collect-point (v2-aref p2 xy)))))))))))


;;;
;;; Matrix Operations
;;;

(deftype transformation-3d nil '(simple-array single-float (4 4)))

(defun concat-mat (l r m)
  (declare (type transformation-3d l r m))
  (dolist (i '(0 1 2 3))
    (dolist (j '(0 1 2 3))
      (setf (aref m i j)
	    (+ (* (aref l i 0)
		  (aref r 0 j))
	       (* (aref l i 1)
		  (aref r 1 j))
	       (* (aref l i 2)
		  (aref r 2 j))
	       (* (aref l i 3)
		  (aref r 3 j))))))
  m)

(defun format-rotate-mat (axis angle m)
  (declare (character axis)
	   (single-float angle)
	   (type transformation-3d m))
  (ident-mat m)
  (let ((s (sin angle))
	(c (cos angle)))
    (declare (single-float s c))
    (case axis
      (x (setf (aref m 1 1) c)
	 (setf (aref m 2 2) c)
	 (setf (aref m 1 2) s)
	 (setf (aref m 2 1) (- s)))
      (y (setf (aref m 0 0) c)
	 (setf (aref m 2 2) c)
	 (setf (aref m 2 0) s)
	 (setf (aref m 0 2) (- s)))
      (z (setf (aref m 0 0) c)
	 (setf (aref m 1 1) c)
	 (setf (aref m 0 1) s)
	 (setf (aref m 1 0) (- s))))))

(defun ident-mat (m)
  (declare (type transformation-3d m))
  (dolist (i '(0 1 2 3))
    (dolist (j '(0 1 2 3))
      (setf (aref m i j) 0.0))
    (setf (aref m i i) 1.0))
  m)

(defun partial-nonhom-transformation (m v3-seq)
  (declare (type transformation-3d m)
	   (type (simple-array single-float (12 3)) v3-seq)
	   (optimize (safety 0) (speed 3)))
  (let ((m00 (aref m 0 0))
	(m10 (aref m 1 0))
	(m20 (aref m 2 0))
	(m01 (aref m 0 1))
	(m11 (aref m 1 1))
	(m21 (aref m 2 1))
	(m02 (aref m 0 2))
	(m12 (aref m 1 2))
	(m22 (aref m 2 2)))
    (dotimes (i (array-dimension v3-seq 0))
      (let* ((in-x (aref v3-seq i 0))
	     (in-y (aref v3-seq i 1))
	     (in-z (aref v3-seq i 2)))
	(setf (aref v3-seq i 0) (+ (* in-x m00)
				   (* in-y m10)
				   (* in-z m20))
	      (aref v3-seq i 1) (+ (* in-x m01)
				   (* in-y m11)
				   (* in-z m21))
	      (aref v3-seq i 2) (+ (* in-x m02)
				   (* in-y m12)
				   (* in-z m22)))))))


(defun create-xform nil 
  (let ((r1 (create-transformation-3d))
	(r2 (create-transformation-3d))
	(r3 (create-transformation-3d)))
    (format-rotate-mat 'x (/ (* 5 (coerce pi 'single-float)) 180.0) r1)
    (format-rotate-mat 'y (/ (* 5 (coerce pi 'single-float)) 180.0) r2)
    (concat-mat r1 r2 r3)
    r3))

(defun create-transformation-3d nil 
  (make-array '(4 4) :element-type 'single-float))

(setq xform (create-xform))

(clim-demo::define-demo "Ico demo" (ico))
