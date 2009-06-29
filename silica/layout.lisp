;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2007 Franz Inc, Oakland, CA - All rights reserved.
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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: layout.lisp,v 2.7 2007/04/17 21:45:52 layer Exp $

(in-package :silica)

;;;"Copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
;;; Portions copyright (c) 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1989, 1990 Xerox Corp.  All rights reserved."


(define-protocol-class space-requirement ())

(defmethod print-object ((object space-requirement) stream)
  (print-unreadable-object (object stream :type t)
    (multiple-value-bind (width min-width max-width height min-height max-height) 
        (space-requirement-components object)
      (format stream "~D<w=~D<~D ~D<h=~D<~D"
        min-width width max-width 
        min-height height max-height))))


(defclass null-space-requirement (space-requirement) ())

(defmethod space-requirement-components ((sr null-space-requirement))
  (values 0 0 0 0 0 0))

(defmethod space-requirement-width ((sr null-space-requirement)) 0)
(defmethod space-requirement-min-width ((sr null-space-requirement)) 0)
(defmethod space-requirement-max-width ((sr null-space-requirement)) 0)
(defmethod space-requirement-height ((sr null-space-requirement)) 0)
(defmethod space-requirement-min-height ((sr null-space-requirement)) 0)
(defmethod space-requirement-max-height ((sr null-space-requirement)) 0)

(defvar +null-space-requirement+ (make-instance 'null-space-requirement))


(defclass simple-space-requirement (space-requirement)
    ((width :initarg :width :accessor space-requirement-width)
     (height :initarg :height :accessor space-requirement-height)))

(defmethod space-requirement-components ((sr simple-space-requirement))
  (with-slots (width height) sr
    (values width width width
            height height height)))

(defmethod space-requirement-width ((sr simple-space-requirement)) 
  (slot-value sr 'width))
(defmethod space-requirement-min-width ((sr simple-space-requirement))
  (slot-value sr 'width))
(defmethod space-requirement-max-width ((sr simple-space-requirement))
  (slot-value sr 'width))
(defmethod space-requirement-height ((sr simple-space-requirement)) 
  (slot-value sr 'height))
(defmethod space-requirement-min-height ((sr simple-space-requirement)) 
  (slot-value sr 'height))
(defmethod space-requirement-max-height ((sr simple-space-requirement)) 
  (slot-value sr 'height))


(defclass general-space-requirement (space-requirement)
    ((width :initarg :width :reader space-requirement-width)
     (min-width :reader space-requirement-min-width)
     (max-width :reader space-requirement-max-width)
     (height :initarg :height :reader space-requirement-height)
     (min-height :reader space-requirement-min-height)
     (max-height :reader space-requirement-max-height)))

(defmethod space-requirement-components ((sr general-space-requirement))
  (with-slots (width min-width max-width
               height min-height max-height) sr
    (values width min-width max-width
            height min-height max-height)))

(defmethod initialize-instance :after ((sr general-space-requirement)
                                       &key (width (error "Width not specified"))
                                            (min-width width)
                                            (max-width width)
                                            (height (error "Height not specified"))
                                            (min-height height)
                                            (max-height height))
  (setf (slot-value sr 'min-height) min-height
        (slot-value sr 'max-height) max-height
        (slot-value sr 'min-width) min-width
        (slot-value sr 'max-width) max-width))

(defun make-space-requirement (&key (width 0) (min-width width) (max-width width)
                                    (height 0) (min-height height) (max-height height))
  (cond ((and (eq width  min-width)  (eq width  max-width)
              (eq height min-height) (eq height max-height))
         ;; Compare with EQ since width and height can be :COMPUTE
         (if (and (eq width 0) (eq height 0))
             +null-space-requirement+
             (make-instance 'simple-space-requirement 
               :width width :height height)))
        (t
         (make-instance 'general-space-requirement
           :width width :min-width min-width :max-width max-width
           :height height :min-height min-height :max-height max-height))))

(defconstant +fill+
             #+(or aclpc acl86win32) 2048 ; expression below gives 100 in aclpc - too small
             #-(or aclpc acl86win32)
             (floor (/ (expt 10 (floor (log most-positive-fixnum 10))) 100)))



;;; Layout protocol

;(defgeneric compose-space (sheet)
;  )
;
;(defmethod compose-space (sheet)
;  (multiple-value-bind (width height)
;      (bounding-rectangle-size sheet)
;    (make-space-requirement :width width :height height)))
;
;(defgeneric allocate-space (sheet width height))
;
;(defmethod allocate-space (sheet width height)
;  (declare (ignore sheet width height)))

(defun parse-box-contents (contents)
  (mapcar #'(lambda (x)
              ;; Handle top-down layout syntax
              (if (and (consp x)
                       (typep (first x) '(or (member :fill) number)))
                  `(list ,(first x) ,(second x))
                  x))
          contents))

(defmacro vertically ((&rest options &key spacing &allow-other-keys)
                      &body contents)
  (declare (ignore spacing))
  `(make-pane 'vbox-pane
     :contents (list ,@(parse-box-contents contents))
     ,@options))

(defmacro horizontally ((&rest options &key spacing &allow-other-keys)
                        &body contents)
  (declare (ignore spacing))
  `(make-pane 'hbox-pane
     :contents (list ,@(parse-box-contents contents))
     ,@options))



(defmethod resize-sheet ((sheet basic-sheet) width height)
  (unless (and (> width 0) (> height 0))
    (error "Trying to resize sheet ~S to be too small (~D x ~D)"
           sheet width height))
  (with-bounding-rectangle* (left top right bottom) sheet
    (let ((owidth (- right left))
          (oheight (- bottom top)))
      (if (or (/= owidth width)
              (/= oheight height)
              #+allegro
              (mirror-needs-changing-p sheet left top width height))
          ;; It should be safe to modify the sheet's region, since
          ;; each sheet gets a fresh region when it is created
          (let ((region (sheet-region sheet)))
            (setf (slot-value region 'left) left
                  (slot-value region 'top)  top
                  (slot-value region 'right) (+ left width)
                  (slot-value region 'bottom) (+ top height))
            (note-sheet-region-changed sheet))
        ;;--- Do this so that we relayout the rest of tree.
        ;;--- I guess we do not want to do this always but ...
        (allocate-space sheet owidth oheight)))))

;;;-- This sucks but it seems to get round the problem of the mirror
;;;-- geometry being completely wrong after the layout has been changed

(defmethod mirror-needs-changing-p ((sheet sheet) left top width height)
  (declare (ignore left top width height))
  nil)


(defmethod mirror-needs-changing-p ((sheet mirrored-sheet-mixin) 
				    left top width height)
  (and (sheet-direct-mirror sheet)
       (multiple-value-bind (mleft mtop mright mbottom)
           (mirror-region* (port sheet) sheet)
         (or (/= left mleft)
	     (/= top mtop)
	     (/= width (- mright mleft))
             (/= height (- mbottom mtop))))))


(defmethod move-sheet ((sheet basic-sheet) x y)
  (let ((transform (sheet-transformation sheet)))
    (multiple-value-bind (old-x old-y)
        (transform-position transform 0 0)
      (with-bounding-rectangle* (left top right bottom) sheet
	(when (or (and x (/= old-x x))
		  (and y (/= old-y y))
		  (mirror-needs-changing-p sheet x y (- right left) (- bottom top)))
	  ;; We would like to use volatile transformations here, but it's
	  ;; not really safe, since the current implementation of transformations
	  ;; is likely to cause them to be shared
	  (setf (sheet-transformation sheet)
	    (compose-translation-with-transformation
	     transform
	     (if x (- x old-x) 0) (if y (- y old-y) 0))))))))

(defmethod move-and-resize-sheet ((sheet basic-sheet) x y width height)
  (resize-sheet sheet width height)
  (move-sheet sheet x y))

(defgeneric sheet-flags (sheet)
  (:documentation "Returns a list of flags that the port may have set concerning the sheet.")
  (:method ((sheet basic-sheet))
    ()))

(defgeneric (setf sheet-flags) (new-value sheet)
  (:documentation "Set a list of flags, as supported by the port. Ignores any flags that the port doesn't understand.")
  (:method (new-value (sheet basic-sheet))
    (declare (ignore new-value))
    nil))


;;; Various

(define-protocol-class pane (sheet))

#+aclpc ;;; aclpc is anal about initialization args, so we must appease
(defmethod initialize-instance :around ((pane pane) &key background text-style)
  (apply #'call-next-method nil))

;;--- What about PANE-FOREGROUND/BACKGROUND vs. MEDIUM-FOREGROUND/BACKGROUND?
(defclass basic-pane 
          (sheet-transformation-mixin
           standard-sheet-input-mixin
           standard-repainting-mixin
           temporary-medium-sheet-output-mixin
           basic-sheet
           pane)
    ((frame :reader pane-frame :initarg :frame)
     (framem :reader frame-manager :initarg :frame-manager)
     (name :accessor pane-name :initform nil :initarg :name)))

(defmethod print-object ((object basic-pane) stream)
  (if (and (slot-boundp object 'name)
           (stringp (slot-value object 'name)))
      (print-unreadable-object (object stream :type t :identity t)
        (format stream "~A" (slot-value object 'name)))
      (call-next-method)))

(defmethod handle-repaint ((pane basic-pane) region)
  (declare (ignore region))
  nil)

;;--- This is suspicious - it should either be on a composite-pane or
;;--- on a top-level sheet
#+++ignore
(defmethod note-sheet-region-changed :after ((sheet basic-pane) &key port)
  (declare (ignore port))
  (multiple-value-bind (width height) (bounding-rectangle-size sheet)
    (allocate-space sheet width height)))

(defmethod pane-frame ((sheet basic-sheet)) nil)

;(defclass list-contents-mixin ()
;    ((contents :initform nil)
;     (nslots :initform nil :initarg :nslots)
;     ;; Reverse-p is provided because it makes sense to let users specify
;     ;; vertical lists from top to bottom, but internally it is easier to deal
;     ;; with bottom to top, because of the lower left coordinate system.
;     ;; For example, a user would like to list the vbox contents from top to
;     ;; bottom, while y coords increases from bottom to top. Whereas with
;     ;; a hbox, user specifys left to right just as x coord increases left
;     ;; to right.
;     ;; 
;     ;; A problem with reverse-p solution is that when new panes are inserted, 
;     ;; all panes before them in the user's view of contents changes coord,
;     ;; but the user may expect the one's later in the list to change coords.
;     ;; For example in a vertical list, adding a pane to the end will
;     ;; reposition all of the list items.
;     
;     ;; A possible solution is to user a upper left coordinate
;     ;; system so that added items have absolute y values that are increasing.
;     ;; Once upon a time Doug told me he prefered this, but it'll have to wait
;     ;; for now.   A deeper problem with this solution is that different panes
;     ;; would have different coordinate systems which maybe confusing if the
;     ;; coordinate system is exposed to the client in anyway.
;     (reverse-p :initform nil :initarg :reverse-p)))
;
;;;;
;;;; Insertion/Deletion Protocols
;;;;
;
;(defmethod contents ((lcm list-contents-mixin))
;  (with-slots (reverse-p contents) lcm
;    (if reverse-p (reverse contents)
;        contents)))
;    
;(defmethod (setf contents) (new-contents (lcm list-contents-mixin))
;  (with-slots (reverse-p contents children) lcm
;    (dolist (pane contents)
;      (sheet-disown-child lcm pane))
;    (dolist (pane new-contents)
;      (sheet-adopt-child lcm pane))
;    (setf contents (if reverse-p (reverse new-contents)
;                       new-contents)
;          ;; So that things repaint from top to bottom
;          ;; ??? However, it only work is you go through this method.
;          children (if reverse-p (nreverse children)
;                          children))
;    
;    (note-space-requirements-changed (sheet-parent lcm) lcm)))
;
;
;
;(defun check-position (position reverse-p contents)
;  (let ((len (length contents)))
;    (when reverse-p 
;      (setq position (if position (- len position) 0)))
;    (unless position (setq position len))
;    (check-type position number)
;    (assert (and (<= 0 position) (<= position len))
;            (position)
;            "Position argument out of bounds")
;    position))
;
;(defmethod insert-panes ((lcm list-contents-mixin) panes
;                         &key position &allow-other-keys)
;  (with-slots (reverse-p contents) lcm
;    (setq position (check-position position reverse-p contents))
;    (dolist (pane panes)
;      (insert-pane lcm pane :position position :batch-p t)
;      (unless reverse-p (incf position)))
;    (note-space-requirements-changed (sheet-parent lcm) lcm)))
;
;(defmethod insert-pane ((lcm list-contents-mixin) (pane basic-pane) 
;                        &key position batch-p &allow-other-keys)
;  (with-slots (contents reverse-p) lcm
;    (unless batch-p
;      (setq position (check-position position reverse-p contents)))
;    (if (zerop position)
;        (push pane contents)
;        (let ((tail (nthcdr (1- position) contents)))
;          (setf (cdr tail) 
;                (cons pane (cdr tail)))))
;    (sheet-adopt-child lcm pane)
;    (unless batch-p
;      (note-space-requirements-changed (sheet-parent lcm) lcm))))
;                              
;(defmethod remove-panes ((lcm list-contents-mixin) panes)
;  (dolist (pane panes)
;    (with-slots (contents) lcm
;      (setf contents (delete pane contents))
;      (sheet-disown-child lcm pane)))
;  (note-space-requirements-changed (sheet-parent lcm) lcm))
;
;(defmethod remove-pane ((lcm list-contents-mixin) (pane basic-pane)
;                        &key batch-p &allow-other-keys)
;  (with-slots (contents) lcm
;    (setf contents (delete pane contents :test #'eq))
;    (sheet-disown-child lcm pane)
;    (unless batch-p (note-space-requirements-changed (sheet-parent lcm) lcm))))
;
;(defmethod note-space-requirements-changed :before (parent (lcm list-contents-mixin))
;  #-PCL ;; PCL uses this variable for method-table cache misses, unfortunately.
;  (declare (ignore parent))
;  (with-slots (nslots contents) lcm
;    (setf nslots (length contents))))



;;--- CLIM 0.9 has some other methods on top-level sheets -- do we want them?
(defclass top-level-sheet 
          (;;--- Temporary kludge until we get the protocols correct
           ;;--- so that ACCEPT works correctly on a raw sheet
           clim-internals::window-stream
           wrapping-space-mixin
           sheet-multiple-child-mixin
           mirrored-sheet-mixin
           permanent-medium-sheet-output-mixin
           basic-pane)
    ((user-specified-size-p :initform :unspecified :initarg :user-specified-size-p)
     (user-specified-position-p :initform :unspecified  :initarg :user-specified-position-p))
  ;;--- More of same...
  (:default-initargs :text-cursor nil :text-margin most-positive-fixnum))

(defmethod window-refresh ((sheet top-level-sheet))
  nil)

;;; spr24242:
;;; Note:
;;; The following methods are specified in the Clim documentation 
;;; (at the end of Sec 19.5.2).
;;; 
;;; These are methods are basically copies of methods on the class 
;;; clim-stream-sheet (in clim/db-stream.lisp).
(defmethod window-expose ((stream top-level-sheet))
  (setf (window-visibility stream) t))

(defmethod (setf window-visibility) (visibility (stream top-level-sheet))
  (let ((frame (pane-frame stream)))
    (if frame
        (if visibility
            (enable-frame frame)
          (disable-frame frame))
      (setf (sheet-enabled-p stream) visibility))))

(defmethod window-visibility ((stream top-level-sheet))
  ;;mm: Is the Unix code more correct???
  #+(or aclpc acl86win32)
  (mirror-visible-p (port stream) stream)
  #-(or aclpc acl86win32)
  (let ((frame (pane-frame stream)))
    (and (if frame 
             (eq (frame-state frame) :enabled)
           (sheet-enabled-p stream))
         (mirror-visible-p (port stream) stream)))
  )

(defmethod window-stack-on-top ((stream top-level-sheet))
  (raise-sheet (window-top-level-window stream)))

(defmethod window-stack-on-bottom ((stream top-level-sheet))
  (bury-sheet (window-top-level-window stream)))

(defmethod window-inside-edges ((stream top-level-sheet))
  (bounding-rectangle* (sheet-region (or (pane-viewport stream) stream))))

(defmethod window-inside-size ((stream top-level-sheet))
  (bounding-rectangle-size (sheet-region (or (pane-viewport stream) stream))))

;;--- Needed methods include:
;;---   INVOKE-WITH-RECORDING-OPTIONS
;;---   DEFAULT-TEXT-MARGIN
;;---   STREAM-READ-GESTURE

(defmethod note-layout-mixin-region-changed ((pane top-level-sheet) &key port)
  (let ((frame (pane-frame pane)))
    (if (and port frame (frame-top-level-sheet frame))
	;; We do this because if the WM has resized us then we want to
	;; do the complete LAYOUT-FRAME thing including clearing caches
	;; etc etc.  Don't do this if the frame doesn't own this top-level
	;; sheet yet (still initializing).
	(multiple-value-call #'layout-frame
	  (pane-frame pane) 
	  (bounding-rectangle-size pane))
      ;; Otherwise just call ALLOCATE-SPACE etc
      (call-next-method))))

#+++ignore
(defmethod allocate-space ((sheet top-level-sheet) width height)
  (resize-sheet (first (sheet-children sheet)) width height))

#+++ignore
(defmethod compose-space ((sheet top-level-sheet) &key width height)
  (compose-space (first (sheet-children sheet)) :width width :height height))


(defclass composite-pane
          (sheet-multiple-child-mixin 
           basic-pane)
    ())

(defclass leaf-pane 
          (sheet-permanently-enabled-mixin
           client-overridability-mixin
           basic-pane)
    ((cursor :initarg :cursor :initform nil
             :accessor sheet-cursor)))

