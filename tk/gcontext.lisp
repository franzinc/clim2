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
;; $fiHeader: gcontext.lisp,v 1.20 92/12/02 13:31:15 colin Exp $

(in-package :tk)

(eval-when (compile load eval)
  #+ignore
  (defconstant *gcontext-components* 
      '((function :int)
	(plane-mask :unsigned-long)
	(foreground :unsigned-long)
	(background :unsigned-long)
	(line-width :int)
	(line-style :int)
	(cap-style :int)
	(join-style :int)
	(fill-style :int)
	(fill-rule :int)
	(arc-mode :int)
	(tile :pixmap)
	(stipple :pixmap)
	(ts-x-origin :int)
	(ts-y-origin :int)
	(font x-font)
	(subwindow-mode :int)
	(graphics-exposures :boolean)
	(clip-x-origin :int)
	(clip-y-origin :int)
	(clip-mask :pixmap)
	(dash-offset :int)
	(dashes :char)))

  (defconstant *gcontext-bit-mask*
      '(function plane-mask foreground background
	line-width line-style cap-style join-style fill-style
	fill-rule tile stipple ts-x-origin ts-y-origin font subwindow-mode
	graphics-exposures clip-x-origin clip-y-origin clip-mask dash-offset dashes
	arc-mode)))



(eval-when (compile eval)
  #+ignore
  (defun gcontext-component-to-slot-definition (x)
    (destructuring-bind
	(name c-type) x
      (declare (ignore c-type))
      `(,name :reader ,(intern (format nil "~A-~A" 'gcontext name)))))
  
  #+ignore
  (defun gcontext-component-to-writer  (x)
    (destructuring-bind
	(name c-type) x
      (declare (ignore c-type))
      `(defmethod (setf ,(intern (format nil "~A-~A" 'gcontext name))) 
	   (nv gc)
	 (set-gcontext-component 
	  gc 
	  ,(intern (symbol-name name) :keyword)
	  nv)
	 (setf (slot-value gc ',name) nv))))
  
  (defmacro define-gc-writer (name encoder &rest args)
    `(progn
       (defmethod (setf ,(intern (format nil "~A-~A" 'gcontext name)))
	   (nv (gc gcontext))
	 (let ((gc-values (x11::make-xgcvalues)))
	   (setf (,(intern (format nil "~A~A"
				   'xgcvalues-
				   name)
			   :x11)
		  gc-values)
	     (,encoder gc nv ,@args))
	 
	   (x11:xchangegc
	    (object-display gc)
	    gc
	    ,(ash 1 (or (position name *gcontext-bit-mask*)
			(error "Cannot find ~S in gcontext components" name)))
	    gc-values)
	   nv))))

  (defmacro define-gc-reader (name decoder &rest args)
    `(defmethod ,(intern (format nil "~A-~A" 'gcontext name)) ((gc gcontext))
       (,decoder 
	gc
	(,(intern (format nil "~A~A" '_xgc-values- name) :x11)
	 gc)
	,@args)))
  
  (defmacro define-gc-accessor (name (encoder decoder) &rest args)
    `(progn
       (define-gc-reader ,name ,decoder ,@args)
       (define-gc-writer ,name ,encoder ,@args)
       ',name)))

(defclass gcontext (display-object)
  ((cached-clip-mask :initform nil)))

(defmethod initialize-instance :after ((gcontext gcontext)
				       &key
				       foreign-address
				       drawable
				       function plane-mask foreground background
				       line-width line-style
				       cap-style join-style fill-style fill-rule 
				       arc-mode tile stipple ts-x ts-y
				       font subwindow-mode 
				       graphics-exposures clip-x-origin clip-y-origin
				       clip-mask clip-ordering 
				       dash-offset dashes)

  (unless foreign-address
    (unless drawable
      (error ":drawable must be specified when creating gcontext"))
    (let ((display (object-display drawable)))
      (setf foreign-address (x11::xcreategc display drawable 0 0)
	    (foreign-pointer-address gcontext) foreign-address
	    (slot-value gcontext 'display) display))
    (register-address gcontext foreign-address))
    
    ;;; Set the ones that are specified
    
  (when function (setf (gcontext-function gcontext) function))
  (when plane-mask (setf (gcontext-plane-mask gcontext) plane-mask))
  (when foreground (setf (gcontext-foreground gcontext) foreground))
  (when background (setf (gcontext-background gcontext) background))
  (when line-width (setf (gcontext-line-width gcontext) line-width))
  (when line-style (setf (gcontext-line-style gcontext) line-style))
  (when cap-style (setf (gcontext-cap-style gcontext) cap-style))
  (when join-style (setf (gcontext-join-style gcontext) join-style))
  (when fill-style (setf (gcontext-fill-style gcontext) fill-style))
  (when fill-rule (setf (gcontext-fill-rule gcontext) fill-rule))
  (when arc-mode (setf (gcontext-arc-mode gcontext) arc-mode))
  (when tile (setf (gcontext-tile gcontext) tile))
  (when stipple (setf (gcontext-stipple gcontext) stipple))
  (when ts-x (setf (gcontext-ts-x gcontext) ts-x))
  (when ts-y (setf (gcontext-ts-y gcontext) ts-y))
  (when font (setf (gcontext-font gcontext) font))
  (when subwindow-mode (setf (gcontext-subwindow-mode gcontext) subwindow-mode))
  (when graphics-exposures (setf (gcontext-graphics-exposures gcontext) graphics-exposures))
  (when clip-x-origin (setf (gcontext-clip-x-origin gcontext) clip-x-origin))
  (when clip-y-origin (setf (gcontext-clip-y-origin gcontext) clip-y-origin))
  (when clip-mask (setf (gcontext-clip-mask gcontext clip-ordering) clip-mask))
  (when dash-offset (setf (gcontext-dash-offset gcontext) dash-offset))
  (when dashes (setf (gcontext-dashes gcontext) dashes))
  gcontext)


(defun free-gcontext (gc)
  (x11:xfreegc 
   (object-display gc)
   gc)
  (unregister-address gc))

(defun lispify-function-name (name)
  (intern (substitute #\_ #\- (symbol-name (lispify-tk-name name)))))


;;; Accessors for the gc

(define-gc-accessor function (encode-function decode-function))
(define-gc-accessor plane-mask (encode-card32 decode-card32))

;(define-gc-accessor foreground (encode-pixel decode-pixel))
;(define-gc-accessor background  (encode-pixel decode-pixel))

(defmethod gcontext-foreground ((gc gcontext))
  (decode-pixel gc (x11::_xgc-values-foreground gc)))

(defmethod gcontext-background ((gc gcontext))
  (decode-pixel gc (x11::_xgc-values-background gc)))

(defmethod (setf gcontext-foreground) (nv (gc gcontext))
  (x11:xsetforeground 
   (object-display gc)
   gc
   (encode-pixel gc nv)))

(defmethod (setf gcontext-background) (nv (gc gcontext))
  (x11:xsetbackground 
   (object-display gc)
   gc
   (encode-pixel gc nv)))


(define-gc-accessor line-width  (encode-card16 decode-card16))
  
(defmethod gcontext-fill-style ((gc gcontext))
  (decode-fill-style
   gc (x11::_xgc-values-fill-style gc)))

(defun decode-fill-style (gc x)
  (declare (optimize (speed 3) (safety 0))
	   (ignore gc))
  (case x
    (0 :solid)
    (1 :tiled)
    (2 :stippled)
    (3 :opaque-stippled)))

(defun encode-fill-style (gc x)
  (declare (optimize (speed 3) (safety 0))
	   (ignore gc))
  (ecase x
    (:solid 0)
    (:tiled 1)
    (:stippled 2)
    (:opaque-stippled 3)))

(defmethod (setf gcontext-fill-style) (nv (gc gcontext))
  (x11:xsetfillstyle
   (object-display gc)
   gc
   (encode-fill-style gc nv)))
  

(define-gc-accessor fill-rule (encode-enum decode-enum) '(:even-odd :winding))
(define-gc-accessor tile  (encode-pixmap decode-pixmap))
(define-gc-accessor stipple  (encode-pixmap decode-pixmap))

(defun encode-pixmap (gc x)
  (declare (ignore gc))
  x)

(define-gc-accessor ts-x-origin (encode-int16 decode-int16))
(define-gc-accessor ts-y-origin (encode-int16  decode-int16))

   
(define-gc-accessor font (encode-font decode-font))
(define-gc-accessor cap-style (encode-cap-style decode-cap-style))

(defun encode-cap-style (gc x)
  (declare (optimize (speed 3) (safety 0))
	   (ignore gc))
  (ecase x
    (:not-last 0)
    (:butt 1)
    (:round 2)
    (:projecting 3)))

(define-gc-accessor join-style (encode-join-style decode-join-style))

(defun encode-join-style (gc x)
  (declare (optimize (speed 3) (safety 0))
	   (ignore gc))
  (ecase x
    (:miter 0)
    (:round 1)
    (:bevel 2)))

(define-gc-accessor subwindow-mode (encode-enum decode-enum)
		  '(:clip-by-children :include-inferiors))
(define-gc-accessor graphics-exposures (encode-enum decode-enum) '(:off :on))
(define-gc-accessor clip-x-origin (encode-int16 decode-int16))
(define-gc-accessor clip-y-origin (encode-int16 decode-int16))


(define-gc-accessor dash-offset (encode-card16 decode-card16))
(define-gc-accessor arc-mode (encode-enum decode-enum)  '(:chord :pie-slice))

;;; 


;;; Some encoding stuff

(defconstant *boole-vector*
	     '#(#.boole-clr #.boole-and #.boole-andc2 #.boole-1
		#.boole-andc1 #.boole-2 #.boole-xor #.boole-ior
		#.boole-nor #.boole-eqv #.boole-c2 #.boole-orc2
		#.boole-c1 #.boole-orc1 #.boole-nand #.boole-set))

(defun encode-function (gc x)
   (declare (ignore gc))
  (or (position x *boole-vector*)
      (error "Cannot encode gc function: ~S" x)))

(defun encode-card32 (gc x)
  (declare (ignore gc))
  x)
(defun encode-card16 (gc x)
  (declare (ignore gc))
  x)
(defun encode-enum (gc x y)
  (declare (ignore gc))
  (or (position x y)
      (error "cannot find ~s in ~s" x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-line-attributes (gc line-width line-style cap-style join-style)
  (x11:xsetlineattributes
   (object-display gc)
   gc
   line-width
   (encode-line-style gc line-style)
   (encode-cap-style gc cap-style)
   (encode-join-style gc join-style)))

(defmethod (setf gcontext-clip-mask) ((nv (eql :none)) (gc gcontext))
  (x11:xsetclipmask
   (object-display gc)
   gc
   x11::none))

(defmethod (setf gcontext-clip-mask) ((nv pixmap) (gc gcontext))
  (x11:xsetclipmask
   (object-display gc)
   gc
   nv))

(defmethod (setf gcontext-clip-mask) ((nv cons) (gc gcontext))
  (let ((r (x11:make-xrectangle)))
    (setf (x11:xrectangle-x r) (first nv)
	  (x11:xrectangle-y r) (second nv)
	  (x11:xrectangle-width r) (third nv)
	  (x11:xrectangle-height r) (fourth nv))
    (x11:xsetcliprectangles
     (object-display gc)
     gc
     0					; clip-x-origin
     0					; clip-y-origin
     r
     1
     x11:unsorted)))

;; this will come in useful when we can handle region sets for clip regions
#+ignore
(defmethod (setf gcontext-clip-mask) ((nv cons) (gc gcontext))
  (let ((n (length nv))
	(rs (x11:make-xrectangle-array :number n)))
    (dotimes (i n)
      (let ((r (pop nv)))
	(setf (x11:xrectangle-array-x rs i) (first r)
	      (x11:xrectangle-array-y rs i) (second r)
	      (x11:xrectangle-array-width rs i) (third r)
	      (x11:xrectangle-array-height rs i) (fourth r))))
    (x11:xsetcliprectangles
     (object-display gc)
     gc
     0					; clip-x-origin
     0					; clip-y-origin
     rs
     n
   x11:unsorted)))

(defmethod (setf gcontext-clip-mask) ((nv (eql :nowhere)) (gc gcontext))
  (x11:xsetcliprectangles
   (object-display gc)
   gc
   0					; clip-x-origin
   0					; clip-y-origin
   0
   0
   x11:unsorted))

(defmethod (setf gcontext-clip-mask) :around (nv (gc gcontext))
  (with-slots (cached-clip-mask) gc
    (unless (or (eq cached-clip-mask nv)
		(equal cached-clip-mask nv))
      (call-next-method)
      (setf cached-clip-mask nv))))
  
(define-gc-accessor line-style  (encode-line-style decode-style))

(defun encode-line-style (gc x)
  (declare (optimize (speed 3) (safety 0))
	   (ignore gc))
  (ecase x
    (:solid 0)
    (:dash 1)
    (:double-dash 2)))

(defmethod (setf gcontext-dashes) (nv (gc gcontext))
  (multiple-value-bind (n v)
      (encode-dashes gc nv)
    (x11:xsetdashes
     (object-display gc)
     gc
     0
     v
     n)))


(defun encode-dashes (gc nv)
  (declare (optimize (speed 3) (safety 0))
	   (ignore gc))
  (let* ((n (length nv))
	 (v (excl:make-static-array n :element-type '(unsigned-byte 8))))
    (declare (fixnum n)
	     (type (simple-array (unsigned-byte 8) (*)) v))
    (etypecase nv
      (list
       (let ((i 0))
	 (declare (fixnum i))
	 (dolist (x nv)
	   (declare (type (unsigned-byte 8) x))
	   (setf (aref v i) x)
	   (incf i))))
      (vector
       (dotimes (i n)
	 (setf (aref v i) (the (unsigned-byte 8) (aref nv i))))))
    (values n v)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun encode-int16 (gc x)
  (declare (ignore gc))
  x)


(defun encode-font (gc x)
  (declare (ignore gc))
  (x11:xfontstruct-fid x))



;;; This could be made more efficient by not have all the clos
;;; wrappers


(defun decode-card16 (gc x)
  (declare (ignore gc))
  x)

(defun decode-font (gc x)
  (if (= x #16rffffffff)
      (error "cannot decode font")
    (let* ((display (object-display gc))
	   (font (find-object-from-xid x display nil)))
      (or font
	  (query-font display x)))))

(defun decode-pixmap (gc x)
  (and (/= #16rffffffff x)
       (intern-object-xid
	x
	'pixmap
	(object-display gc))))

(defun decode-clip-mask (gc x)
  (declare (ignore gc))
  x)
(defun decode-enum (gc x y)
  (declare (ignore gc))
  (or (elt y x)
      (error "cannot find ~s in ~s" x y)))
(defun decode-card32 (gc x)
  (declare (ignore gc))
  x)
(defun decode-dashes (gc x)
  (declare (ignore gc))
  x)
(defun decode-int16 (gc x)
  (declare (ignore gc))
  x)


;;;;;;;;;;;;;;;;;;;
#|

(defparameter *temp-gc-stack* nil)

(defmacro with-gcontext ((gc &rest options) &body body)
  (let* ((tgc (gensym))
	 setfs
	 (bits (reduce #'logior
		       (do ((r nil)
			    (o options (cddr o)))
			   ((null o) r)
			 (let ((x (gensym)))
			   (push `(let ((,x ,(cadr o)))
				    (when ,x
				      (setf (,(intern (format nil "~A~A" 'gcontext-
							      (car o)))
					     gc)
					,x)))
				 setfs))
			 (push (ash 1 
				    (position (car o)
					      *gcontext-bit-mask*
					      :test #'string=))
			       r)))))
    `(let* ((,tgc (allocate-temp-gc gc))
	    (.gc. ,gc)
	    (.display. (object-display .gc.)))
       (x11:xcopygc .display. .gc. ,bits ,tgc)
       (unwind-protect
	   (progn ,@(nreverse setfs)
		  ,@body)
	 (x11:xcopygc .display. ,tgc ,bits .gc.)
	 (deallocate-temp-gc ,tgc)))))

(defun allocate-temp-gc (gc)
  (make-instance 'gcontext 
		 :drawable (display-root-window (object-display gc))))

(defun deallocate-temp-gc (gc)
  (x11:xfreegc (object-display gc) gc)
  (setf (ff:foreign-pointer-address gc) 0)
  nil)
|#


#|
(with-gcontext (gc :function foo :font bar)
	       (print gc))
|#

(defun decode-pixel (gc x)
  (declare (ignore gc))
  x)

(defmethod encode-pixel (gc (x integer))
  (declare (ignore gc))
  x)

(defmethod encode-pixel (gc (x color))
  (allocate-color (default-colormap (object-display gc) 0) x))
