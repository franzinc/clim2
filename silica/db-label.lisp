;; -*- mode: common-lisp; package: silica -*-
;;
;;                                -[Thu Jul 22 17:16:08 1993 by colin]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1992 Franz Inc, Berkeley, CA  All rights reserved.
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
;; $Header: /repo/cvs.copy/clim2/silica/db-label.lisp,v 1.4 1997/02/05 01:50:46 tomj Exp $

;;; This file also exists in clim2/homegrown. The homegrown directory
;;; contains files to implement CLIM's generic gadgets. Native backends
;;; should not require this code. However, the Windows port of CLIM does
;;; seem to require this file so it is duplicated here with
;;; modifications. At some point this file and the file in homegrown should
;;; be merged and one of the two removed from cvs control (cim/tjm 2/4/97)

(in-package :silica)

(defclass generic-label-pane 
          (label-pane
           space-requirement-mixin
           leaf-pane)
    ()
  (:default-initargs :align-x :left
                     :text-style *default-label-text-style*))

(defmethod compose-space ((pane generic-label-pane) &key width height)
  (declare (ignore width height))
  (multiple-value-bind (width height)
      (compute-gadget-label-size pane)
    (make-space-requirement :width width :height height)))
  
(defmethod handle-repaint ((pane generic-label-pane) region)
  (declare (ignore region))                        ;not worth checking
  (with-sheet-medium (medium pane)
    (with-bounding-rectangle* (left top right bottom) (sheet-region pane)
      (declare (ignore bottom))
      ;; fixed bug when :align-x not :left (cim 10/14/96)
      (let* ((align-x (gadget-alignment pane))
             (x (ecase align-x
                  (:left left)
                  (:right right)
                  (:center (floor (+ left right) 2))))
             (y top))
        (draw-gadget-label pane medium x y :align-x align-x :align-y :top)))))

(defmethod draw-gadget-label ((pane labelled-gadget-mixin) medium x y
                              &key (align-x (gadget-alignment pane))
                                   (align-y :baseline))
  (let ((label (gadget-label pane)))
    (etypecase label
      (string
        (let ((text-style (slot-value pane 'text-style)))
          (draw-text* medium label x y
                      :text-style text-style
                      :align-x align-x :align-y align-y)))
      (null)      
      (pattern
        (let ((width (pattern-width label))
              (height (pattern-height label)))
          (ecase align-x
            (:left)
            (:right (decf x width))
            (:center (decf x (floor width 2))))
          (ecase align-y
            ((:top :baseline))
            (:bottom (decf x height))
            (:center (decf x (floor height 2))))
          (draw-pattern* medium label x y)))
      (pixmap
        (let ((width (pixmap-width label))
              (height (pixmap-height label)))
          (ecase align-x
            (:left)
            (:right (decf x width))
            (:center (decf x (floor width 2))))
          (ecase align-y
            ((:top :baseline))
            (:bottom (decf x height))
            (:center (decf x (floor height 2))))
          (copy-from-pixmap label 0 0 width height
                            medium x y))))))
