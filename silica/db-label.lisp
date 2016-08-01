;; -*- mode: common-lisp; package: silica -*-
;; See the file LICENSE for the full license governing this code.
;;

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
      (medium-clear-area medium left top right bottom)
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
