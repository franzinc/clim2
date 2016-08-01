;; -*- mode: common-lisp; package: tk -*-
;; See the file LICENSE for the full license governing this code.
;;

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

(defun make-xgcvalues ()
  (clim-utils::allocate-cstruct 'x11::xgcvalues :initialize t))

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
	 (let ((gc-values (make-xgcvalues)))
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

  (defmacro define-gc-writer-function (name function encoder &rest args)
    `(progn
       (defmethod (setf ,(intern (format nil "~A-~A" 'gcontext name)))
	   (nv (gc gcontext))
	 (,function
	  (object-display gc)
	  gc
	  (,encoder gc nv ,@args)))))

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
  ((cached-clip-mask :initform nil excl::fixed-index 1)))

(defmethod initialize-instance :after ((gcontext gcontext)
				       &key
				       foreign-address
				       drawable
				       function plane-mask foreground background
				       line-width line-style
				       cap-style join-style fill-style fill-rule
				       arc-mode tile stipple
				       #+broken ts-x
				       #+broken ts-y
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
  #+broken
  (when ts-x (setf (gcontext-ts-x gcontext) ts-x))
  #+broken
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

(define-gc-reader function decode-function)

(define-gc-writer-function function x11:xsetfunction encode-function)

(define-gc-reader plane-mask decode-card32)
(define-gc-writer-function plane-mask x11:xsetplanemask encode-card32)

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


(define-gc-reader fill-rule decode-enum '(:even-odd :winding))
(define-gc-writer-function fill-rule x11:xsetfillrule encode-enum '(:even-odd :winding))

(define-gc-reader tile decode-pixmap)
(define-gc-writer-function tile x11:xsettile encode-pixmap)

(define-gc-reader stipple decode-pixmap)
(define-gc-writer-function stipple x11:xsetstipple encode-pixmap)

(defun encode-pixmap (gc x)
  (declare (ignore gc))
  x)

(define-gc-accessor ts-x-origin (encode-int16 decode-int16))
(define-gc-accessor ts-y-origin (encode-int16  decode-int16))


(define-gc-reader font decode-font)
(define-gc-writer-function font x11:xsetfont encode-font)

#+broken
(define-gc-accessor cap-style (encode-cap-style decode-cap-style))

#-broken
(define-gc-writer cap-style encode-cap-style)

(defun encode-cap-style (gc x)
  (declare (optimize (speed 3) (safety 0))
	   (ignore gc))
  (ecase x
    (:not-last 0)
    (:butt 1)
    (:round 2)
    (:projecting 3)))

#+broken
(define-gc-accessor join-style (encode-join-style decode-join-style))

#-broken
(define-gc-writer join-style encode-join-style)

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

(defun decode-function (gc x)
  (declare (ignore gc))
  (svref *boole-vector* x))

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

;; granted, this isn't the right thing to do, but it saves us from some
;; nasty consing -tjm 11Apr97
(defmethod (setf gcontext-clip-mask) ((nv clim-utils::standard-bounding-rectangle) (gc gcontext))
  (let ((rs (make-xrectangle-array :number 1)))
    (clim-utils::with-bounding-rectangle* (left top right bottom) nv
      (setf (x11:xrectangle-array-x rs 0) (clim-utils::fix-coordinate left)
	    (x11:xrectangle-array-y rs 0) (clim-utils::fix-coordinate top)
	    (x11:xrectangle-array-width rs 0) (clim-utils::fix-coordinate (- right left))
	    (x11:xrectangle-array-height rs 0) (clim-utils::fix-coordinate (- bottom top))))
    (x11:xsetcliprectangles
     (object-display gc)
     gc
     0
     0
     rs
     1
     x11:unsorted)))

(defmethod (setf gcontext-clip-mask) ((nv clim-utils::standard-rectangle-set) (gc gcontext))
  (let* ((rectangles (clim-utils::region-set-regions nv))
	 (n (length rectangles))
	 (rs (make-xrectangle-array :number n)))
    (dotimes (i n)
      (let ((r (nth i rectangles)))
	(clim-utils::with-bounding-rectangle* (left top right bottom) r
	  (setf (x11:xrectangle-array-x rs i) (clim-utils::fix-coordinate left)
		(x11:xrectangle-array-y rs i) (clim-utils::fix-coordinate top)
		(x11:xrectangle-array-width rs i) (clim-utils::fix-coordinate (- right left))
		(x11:xrectangle-array-height rs i) (clim-utils::fix-coordinate (- bottom top))))))
    (x11:xsetcliprectangles
     (object-display gc)
     gc
     0
     0
     rs
     n
     x11:unsorted)))

(defmethod (setf gcontext-clip-mask) ((nv list) (gc gcontext))
  (let* ((n (length nv))
	 (rs (make-xrectangle-array :number n)))
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

#+broken
(define-gc-accessor line-style  (encode-line-style decode-style))
#-broken
(define-gc-writer line-style encode-line-style)

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
	 (v (make-array n :allocation :static :element-type '(unsigned-byte 8))))
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
  (if (= x
         #+64bit #xffffffffffffffff
         #-64bit #xffffffff)
      (error "cannot decode font")
    (let* ((display (object-display gc))
           (font (find-object-from-xid x display nil)))
      (or font
          (query-font display x)))))

(defun decode-pixmap (gc x)
  (and (/= x
           #+64bit #xffffffffffffffff
           #-64bit #xffffffff)
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
