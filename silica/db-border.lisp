;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

;; $Header: /repo/cvs.copy/clim2/silica/db-border.lisp,v 1.24 1997/02/14 23:57:06 tomj Exp $

"Copyright (c) 1989, 1990 by Xerox Corporation.  All rights reserved.
 Portions copyright (c) 1991, 1992 by Symbolics, Inc.  All rights reserved."

(in-package :silica)

;;; Border Panes

(defclass border-pane (layout-pane)
    ((thickness :initform 1 :initarg :thickness)))

(defmethod initialize-instance :after ((pane border-pane) &key contents)
  (sheet-adopt-child pane contents))

(defmethod handle-event :after ((pane border-pane) (event pointer-motion-event))
  (deallocate-event event))

(defmethod compose-space ((pane border-pane) &key width height)
  (let ((thickness (slot-value pane 'thickness))
        (child (sheet-child pane)))
    (space-requirement+
      (compose-space child :width width :height height)
      (make-space-requirement
        :width (* 2 thickness)
        :height (* 2 thickness)))))

(defmethod allocate-space ((pane border-pane) width height)
  (let ((thickness (slot-value pane 'thickness)))
    (move-and-resize-sheet
      (sheet-child pane)
      thickness thickness
      (- width (* 2 thickness)) (- height (* 2 thickness)))
    (repaint-border-pane pane)))
  
(defmethod handle-repaint ((pane border-pane) region)
  (declare (ignore region))             ;not worth checking
  (repaint-border-pane pane))

(defmethod border-color ((pane border-pane))
  (pane-background pane))

(defun repaint-border-pane (pane)
  (with-sheet-medium (medium pane)
    (with-bounding-rectangle* (left top right bottom) 
      (sheet-region pane)
      (let* ((thickness (slot-value pane 'thickness))
             ;; this used to be ceiling but it sometimes caused
             ;; borders to overwrite the child pane - which played
             ;; havoc with the flipping-ink mechanism used to draw the
             ;; cursor (cim 9/14/95)
             (ht (floor thickness 2)))
        (when #-(or aclpc acl86win32) t
              #+(or aclpc acl86win32) (> thickness 1)
          (incf left ht)
          (incf top ht)
          (decf right ht)
          (decf bottom ht))
        (draw-rectangle* medium left top right bottom
                         :line-thickness thickness :filled nil
                         :ink (border-color pane))))))

(defmacro bordering ((&rest options &key thickness &allow-other-keys)
                     &body contents)
  (declare (ignore thickness))
  (assert (null (cdr contents)) (contents)
          "The ~S layout macro can have only a single pane as its contents"
          'bordering)
  `(make-pane 'border-pane
     :contents ,@contents
     ,@options))

(defclass outlined-pane (border-pane) 
  ())

(defmethod border-color ((pane outlined-pane))
  (pane-foreground pane))

(defmacro outlining ((&rest options &key thickness &allow-other-keys)
                     &body contents)
  (declare (ignore thickness))
  (assert (null (cdr contents)) (contents)
          "The ~S layout macro can have only a single pane as its contents"
          'outlining)
  `(make-pane 'outlined-pane
     :contents ,@contents
     ,@options))


(defclass spacing-pane (border-pane) ()
  (:default-initargs :thickness 2))

(defmacro spacing ((&rest options &key thickness &allow-other-keys) &body contents)
  (declare (ignore thickness))
  (assert (null (cdr contents)) (contents)
          "The ~S layout macro can have only a single pane as its contents"
          'spacing)
  `(make-pane 'spacing-pane
     :contents ,@contents
     ,@options))


;;; Label panes

(defparameter *default-label-text-style*
              (make-text-style :sans-serif :italic :small))

(defclass label-pane
          (sheet-with-resources-mixin
           labelled-gadget-mixin
           pane)
    ()
  (:default-initargs :align-x :left
                     ;; Supplying a text style here defeats the resource
                     ;; mechanism for the Motif/OpenLook ports
                     :text-style nil))

(defmacro labelling ((&rest options
                      &key (label-alignment #+Genera :bottom #-Genera :top)
                      &allow-other-keys)
                     &body contents &environment env)
  #-(or Genera Minima) (declare (ignore env))
  (assert (null (cdr contents)) (contents)
          "The ~S layout macro can have only a single pane as its contents"
          'labelling)
  (setq options (remove-keywords options '(:label-alignment)))
  (let ((lvar '#:label)
        (cvar '#:contents))
    `(let ((,lvar (make-pane 'label-pane ,@options))
           (,cvar (progn ,@contents)))
       ,(if (constantp label-alignment #+(or Genera Minima) env)
            (ecase (eval label-alignment #+(or Genera Minima-Developer) env)
              (:bottom
                `(vertically () ,cvar #+acl86win32 :fill ,lvar))
              (:top
                `(vertically () ,lvar #+acl86win32 :fill ,cvar)))
            `(ecase ,label-alignment
               (:bottom
                 (vertically () ,cvar #+acl86win32 :fill ,lvar))
               (:top
                 (vertically () ,lvar #+acl86win32 :fill ,cvar)))))))




;;; Separator panes
#-(or aclpc acl86win32) ;; in mswin we do this in silica/gadgets.lisp (?) -tjm 12Feb97
(defclass separator (oriented-gadget-mixin pane) ())

