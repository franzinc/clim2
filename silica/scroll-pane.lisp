;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

;;;"Copyright (c) 1991, 1992 by Franz, Inc.  All rights reserved.
;;; Portions copyright(c) 1991, 1992 International Lisp Associates.
;;; Portions copyright (c) 1992 by Symbolics, Inc.  All rights reserved."

;;; This file also exists in clim2/homegrown. The homegrown directory
;;; contains files to implement CLIM's generic gadgets. Native backends
;;; should not require this code. However, the Windows port of CLIM does
;;; seem to require this file so it is duplicated here with
;;; modifications. At some point this file and the file in homegrown should
;;; be merged and one of the two removed from cvs control (cim/tjm 2/4/97)

(in-package :silica)


;; An implementation of a scroller pane
(defclass generic-scroller-pane ( 
                                 wrapping-space-mixin
                                 permanent-medium-sheet-output-mixin
                                 pane-repaint-background-mixin
                                 layout-pane
                                 scroller-pane)
    ())

(defmethod handle-event :after ((pane generic-scroller-pane) (event pointer-motion-event))
  (deallocate-event event))

(defmethod allocate-space :before ((scroller generic-scroller-pane) width height)
  #-(or aclpc acl86win32)
  (declare (ignore width height))
  ;; Adjust the status of the scrollbars
  #-(or aclpc acl86win32)
  (multiple-value-bind (changedp 
                        hscroll-bar hscroll-bar-enabled-p
                        vscroll-bar vscroll-bar-enabled-p)
      (compute-dynamic-scroll-bar-values scroller)
    (when changedp
      (update-dynamic-scroll-bars scroller changedp
                                  hscroll-bar hscroll-bar-enabled-p
                                  vscroll-bar vscroll-bar-enabled-p)))
  #+(or aclpc acl86win32)
  (let ((table (slot-value scroller 'viewport)))
         (multiple-value-bind (nwidth nheight)
             (bounding-rectangle-size table)
           (allocate-space table (max nwidth width) (max nheight height))))
  )

(defmethod compose-space ((scroller generic-scroller-pane) &key width height)
  (declare (ignore width height))
  (let ((sr (call-next-method)))
    (multiple-value-bind (width min-width max-width
                          height min-height max-height)
        (space-requirement-components sr)
      (if (or (< width 50) (< height 50))
          ;; Make sure the scroller pane is big enough to hold something
          (make-space-requirement
            :width (max width 50)
            :min-width (max min-width 50)
            :max-width (max max-width 50)
            :height (max height 50)
            :min-height (max min-height 50)
            :max-height (max max-height 50))
          sr))))

;;--- Ideally we should use a toolkit scrolling window. This will look
;;--- exactly right and will deal with user specified placement of scroll-bars.
;;--- However the geometry management problems are quite huge.
(defmethod initialize-instance :after ((pane generic-scroller-pane) 
                                       &key contents frame-manager frame
                                            scroll-bars)
  (let ((scroller (gadget-supplied-scrolling frame-manager frame contents
                                             :scroll-bars scroll-bars)))
    (if scroller
        (progn
          (sheet-adopt-child pane scroller)
          (setf (slot-value pane 'scrolling-supplied-by-gadget) t)
          scroller)
        (progn
          (check-type scroll-bars
                      (member t :both :dynamic :vertical :horizontal))
          (with-slots (vertical-scroll-bar horizontal-scroll-bar
                       (c contents) viewport foreground background) pane
            (with-look-and-feel-realization (frame-manager frame)
              (let ((verticalp
                      (member scroll-bars '(t :both :dynamic :vertical)))
                    (horizontalp
                      (member scroll-bars '(t :both :dynamic :horizontal))))
                (setf vertical-scroll-bar 
                      (and verticalp
                           (make-pane 'scroll-bar 
                             :orientation :vertical
                             :id :vertical
                             :client pane
                             :background background
                              :foreground foreground
                             :shared-medium-sheet pane))
                      horizontal-scroll-bar             
                      (and horizontalp
                           (make-pane 'scroll-bar 
                             :orientation :horizontal
                             :id :horizontal
                             :background background
                              :foreground foreground
                             :client pane
                             :shared-medium-sheet pane))
                      c contents
                      viewport (make-pane 'viewport :scroller-pane pane))
                (sheet-adopt-child
                  pane
                  (cond ((and horizontalp verticalp)
                         (tabling ()
                           (viewport vertical-scroll-bar)
                           (horizontal-scroll-bar nil)))
                        (verticalp
                         (horizontally ()
                           viewport
                           vertical-scroll-bar))
                        (horizontalp
                         (vertically ()
                           viewport
                           horizontal-scroll-bar))
                        (t (error "Internal error laying out scroll bars"))))
                (sheet-adopt-child viewport c)
                ;;--- Add callbacks
                )))))))


;;; Home-grown scroll bars

(defparameter *scroll-shaft-thickness-ink* 12)

;;--- unit-increment, page-increment.
(defclass scroll-bar-pane
          (scroll-bar
           sheet-permanently-enabled-mixin
           wrapping-space-mixin
           immediate-sheet-input-mixin
           sheet-multiple-child-mixin
           space-requirement-mixin
           shared-medium-sheet-output-mixin
           basic-pane)
    ((shaft-thickness :initarg :shaft-thickness)
     (min-target-pane :initform nil)
     (max-target-pane :initform nil)
     (shaft-pane :initform nil))
  (:default-initargs :value 0
                     :shaft-thickness *scroll-shaft-thickness-ink*))

(defmethod initialize-instance :after ((pane scroll-bar-pane)
                                       &key orientation shaft-thickness
                                            frame-manager frame)
  (with-slots (min-target-pane max-target-pane shaft-pane
               shared-medium-sheet foreground background) pane
    (with-look-and-feel-realization (frame-manager frame)
      (let ((inferiors
              (ecase orientation
                (:vertical
                  (spacing (:thickness 1)
                    (vertically ()
                      (setq min-target-pane
                            (make-pane 'scroll-bar-target-pane
                              :scroll-bar pane
                              :end :less-than
                              :width shaft-thickness
                              :height shaft-thickness
                              :background background
                              :foreground foreground
                              :shared-medium-sheet shared-medium-sheet))
                      (setq shaft-pane 
                            (make-pane 'scroll-bar-shaft-pane 
                              :scroll-bar pane
                              :width shaft-thickness
                              :height #-(or aclpc acl86win32) 0  #+(or aclpc acl86win32) shaft-thickness
                              :max-height +fill+
                              :background background
                              :foreground foreground
                              :shared-medium-sheet shared-medium-sheet))
                      (setq max-target-pane
                            (make-pane 'scroll-bar-target-pane
                              :scroll-bar pane
                              :end :greater-than
                              :width shaft-thickness
                              :height shaft-thickness
                              :background background
                              :foreground foreground
                              :shared-medium-sheet shared-medium-sheet)))))
                (:horizontal
                  (spacing (:thickness 1)
                    (horizontally ()
                      (setq min-target-pane
                            (make-pane 'scroll-bar-target-pane
                              :scroll-bar pane
                              :end :less-than
                              :width shaft-thickness
                              :height shaft-thickness
                              :background background
                              :foreground foreground
                              :shared-medium-sheet shared-medium-sheet))
                      (setq shaft-pane 
                            (make-pane 'scroll-bar-shaft-pane
                              :scroll-bar pane
                              :width #-(or aclpc acl86win32) 0 #+(or aclpc acl86win32) shaft-thickness
                              :max-width +fill+
                              :height shaft-thickness
                              :background background
                              :foreground foreground
                              :shared-medium-sheet shared-medium-sheet))
                      (setq max-target-pane
                            (make-pane 'scroll-bar-target-pane
                              :scroll-bar pane
                              :end :greater-than
                              :width shaft-thickness
                              :height shaft-thickness
                              :background background
                              :foreground foreground
                              :shared-medium-sheet shared-medium-sheet))))))))
        (sheet-adopt-child pane inferiors)))))

(defmethod handle-event :after ((pane scroll-bar-pane) (event pointer-event))
  (deallocate-event event))


(defmethod contents-range ((scroller generic-scroller-pane) orientation)
  (with-slots (viewport) scroller
    (with-bounding-rectangle* (left top right bottom) 
        (viewport-contents-extent viewport)
      (ecase orientation
        (:horizontal (- right left))
        (:vertical (- bottom top))))))

(defmethod viewport-range ((scroller generic-scroller-pane) orientation)
  (with-slots (viewport) scroller
    (with-bounding-rectangle* (left top right bottom) 
        (viewport-viewport-region viewport)
      (ecase orientation
        (:horizontal (- right left))
        (:vertical (- bottom top))))))

(defmethod scroll-up-line-callback ((scroll-bar scroll-bar-pane) scroller-pane orientation)
  (with-slots (current-size current-value port) scroll-bar
    (with-slots (viewport contents) scroller-pane
      (let* ((contents-range (contents-range scroller-pane orientation))
             (line-value (if (= contents-range 0)
                             0
                             (the single-float
                               (/ (line-scroll-amount scroller-pane orientation :up)
                                  (float contents-range  0.0s0)))))
             (new-value (max 0.0 (- current-value line-value))))        
        (scroll-bar-value-changed-callback
          scroll-bar scroller-pane orientation new-value current-size)))))

(defmethod scroll-down-line-callback ((scroll-bar scroll-bar-pane) scroller-pane orientation)
  (with-slots (current-size current-value port) scroll-bar
    (with-slots (viewport contents) scroller-pane
      (let* ((contents-range (contents-range scroller-pane orientation))
             (line-value (if (= contents-range 0)
                             0
                             (the single-float
                               (/ (line-scroll-amount scroller-pane orientation :down)
                                  (float contents-range  0.0s0)))))
             (new-value (+ current-value line-value)))
        (scroll-bar-value-changed-callback
          scroll-bar scroller-pane orientation new-value current-size)))))

(defmethod scroll-up-page-callback ((scroll-bar scroll-bar-pane) scroller-pane orientation)
  (with-slots (current-size current-value) scroll-bar
    (with-slots (viewport contents) scroller-pane
      (let* ((contents-range (contents-range scroller-pane orientation))
             (viewport-range (bounding-rectangle-max-y viewport))
             new-value)
        (if (zerop contents-range)
            (setq new-value current-value)
            (let ((page-value (the single-float 
                                (/ viewport-range (float contents-range 0.0s0)))))
              (setq new-value (max 0.0 (- current-value page-value)))))
        (scroll-bar-value-changed-callback
          scroll-bar scroller-pane orientation new-value current-size)))))

(defmethod scroll-down-page-callback ((scroll-bar scroll-bar-pane) scroller-pane orientation)
  (with-slots (current-size current-value) scroll-bar
    (with-slots (viewport contents) scroller-pane
      (let* ((contents-range (contents-range scroller-pane orientation))
             (viewport-range (bounding-rectangle-max-y viewport))
             new-value)
      (if (zerop contents-range)
          (setq new-value current-value)
          (let ((page-value (the single-float 
                              (/ viewport-range (float contents-range 0.0s0)))))
            (setq new-value (+ current-value page-value))))
      (scroll-bar-value-changed-callback
        scroll-bar scroller-pane orientation new-value current-size)))))

(defmethod scroll-to-top-callback ((scroll-bar scroll-bar-pane) client id)
  (with-slots (current-size current-value) scroll-bar
    (scroll-bar-value-changed-callback scroll-bar client id 0 current-size)))

(defmethod scroll-to-bottom-callback ((scroll-bar scroll-bar-pane) client id)
  (with-slots (current-size current-value) scroll-bar
    (scroll-bar-value-changed-callback
      scroll-bar client id 1.0 current-size)))

(defmethod scroll-line-to-top-callback ((scroll-bar scroll-bar-pane)
                                        scroller-pane id orientation x y)
  (with-slots (current-size current-value) scroll-bar
    (with-slots (viewport contents) scroller-pane
      ;; --- scroll-bar may not be the right thing if it's not the same
      ;; size as the contents pane - Davo 6/30/92.
      (with-bounding-rectangle* (left top right bottom)
          (sheet-region scroll-bar)
        (with-bounding-rectangle* (vleft vtop vright vbottom)
            (viewport-viewport-region viewport) 
          (declare (ignore vright vbottom))
          (with-bounding-rectangle* (cleft ctop cright cbottom)
              (viewport-contents-extent viewport) 
            (declare (ignore cright cbottom))
            (let ((contents-range (contents-range scroller-pane orientation)))
              (unless (zerop contents-range)
                (let* ((viewport-range (viewport-range scroller-pane orientation))
                       (contents-min
                         (ecase orientation (:horizontal cleft) (:vertical ctop)))
                       (viewport-min
                         (ecase orientation (:horizontal vleft) (:vertical vtop)))
                       (mouse-offset
                         (ecase orientation
                           (:vertical (/ (- y top) (float (- bottom top) 0.0s0)))
                           (:horizontal (/ (- x left) (float (- right left) 0.0s0)))))
                       (pos (/ (- (+ viewport-min (* viewport-range mouse-offset))
                                  contents-min)
                               contents-range)))
                  (scroll-bar-value-changed-callback
                    scroll-bar scroller-pane id
                    pos current-size))))))))))

(defmethod scroll-line-to-bottom-callback ((scroll-bar scroll-bar-pane)
                                           scroller-pane id orientation x y)
  (with-slots (current-size current-value) scroll-bar
    (with-slots (viewport contents) scroller-pane
      ;; --- scroll-bar may not be the right thing if its not the same
      ;; size as the contents pane - Davo 6/30/92.
      (with-bounding-rectangle* (left top right bottom)
          (sheet-region scroll-bar)
        (with-bounding-rectangle* (vleft vtop vright vbottom)
            (viewport-viewport-region viewport) 
          (declare (ignore vright vbottom))
          (with-bounding-rectangle* (cleft ctop cright cbottom)
              (viewport-contents-extent viewport) 
            (declare (ignore cright cbottom))
            (let ((contents-range (contents-range scroller-pane orientation)))
              (unless (zerop contents-range)
                (let* ((viewport-range (viewport-range scroller-pane orientation))
                       (contents-min
                         (ecase orientation (:horizontal cleft) (:vertical ctop)))
                       (viewport-min
                         (ecase orientation (:horizontal vleft) (:vertical vtop)))
                       (mouse-offset
                         (ecase orientation
                           (:vertical (/ (- y top) (float (- bottom top) 0.0s0)))
                           (:horizontal (/ (- x left) (float (- right left) 0.0s0)))))
                       (pos (/ (- (+ viewport-min (* viewport-range mouse-offset))
                                  viewport-range contents-min)
                               contents-range)))
                  (scroll-bar-value-changed-callback
                    scroll-bar scroller-pane id (max 0 pos) current-size))))))))))

(defmethod scroll-elevator-callback ((scroll-bar scroll-bar-pane)
                                     scroller-pane id orientation x y)
  (with-slots (current-size current-value) scroll-bar
    (with-slots (viewport contents) scroller-pane
      ;; --- scroll-bar may not be the right thing if its not the same
      ;; size as the contents pane - Davo 6/30/92.
      (with-bounding-rectangle* (left top right bottom)
          (sheet-region scroll-bar)
        (let* ((mouse-offset
                 (- (ecase orientation
                      (:vertical (/ (- y top) (float (- bottom top) 0.0s0)))
                      (:horizontal (/ (- x left) (float (- right left) 0.0s0))))
                    (/ current-size 2.0s0))))
          (scroll-bar-value-changed-callback
            scroll-bar scroller-pane id (min 1.0s0 (max 0.0s0 mouse-offset)) current-size))))))

;;; Set the indicator to the proper size and location (size and value are between 0 and 1)
(defmethod change-scroll-bar-values ((scroll-bar scroll-bar-pane)
                                     &key slider-size value line-increment)
  (declare (ignore line-increment slider-size))
  (setf (gadget-value scroll-bar :invoke-callback nil) value)
  ;;(setf (scroll-bar-current-size scroll-bar) slider-size)
  (let* ((scroller (gadget-client scroll-bar))
         (contents (pane-contents scroller)))
    ;;--- Hmm, if two "linked" panes have callbacks that cause the other
    ;;--- pane to be scrolled, this will go into a loop...
    (value-changed-callback scroll-bar contents (gadget-id scroll-bar) value)))


(defclass scroll-bar-target-pane 
          (immediate-sheet-input-mixin
           space-requirement-mixin
           sheet-single-child-mixin
           shared-medium-sheet-output-mixin
           sheet-with-resources-mixin
           pane-repaint-background-mixin
           leaf-pane)
    ((end :initarg :end)
     (scroll-bar :initarg :scroll-bar)
     (coord-cache :initform nil)))


(defmethod initialize-instance :after ((pane scroll-bar-target-pane) &key end)
  (let* ((scroll-bar (slot-value pane 'scroll-bar))
         (orientation (gadget-orientation scroll-bar)))
    (setf (slot-value pane 'pointer-cursor)
          (ecase end
            (:less-than 
              (ecase orientation
                (:horizontal :scroll-left)
                (:vertical :scroll-up)))
            (:greater-than
              (ecase orientation
                (:horizontal :scroll-right)
                (:vertical :scroll-down)))))))

(defmethod note-sheet-region-changed :after ((pane scroll-bar-target-pane)
                                             &key &allow-other-keys)
  (setf (slot-value pane 'coord-cache) nil))

(defmethod handle-repaint ((pane scroll-bar-target-pane) region)
  (declare (ignore region))
  (with-sheet-medium (medium pane)
    (with-bounding-rectangle* (left top right bottom) (sheet-region pane)
      (draw-rectangle* medium left top (1- right) (1- bottom)
                       :filled nil :ink (medium-foreground medium)))
    (draw-target pane medium)))

;;; You can pass :FILLED T to this in order to highlight the target when clicked on...
(defmethod draw-target ((pane scroll-bar-target-pane) medium
                        &key filled (ink +foreground-ink+))
  (let ((coord-cache (slot-value pane 'coord-cache)))
    (unless coord-cache
      (let ((identity (sheet-pointer-cursor pane)))
        (setq coord-cache
              (setf (slot-value pane 'coord-cache)
                    (with-bounding-rectangle* (left top right bottom) (sheet-region pane)
                      ;; This seems to be a kludge to deal with roundoff?
                      (decf right) (decf bottom)
                      ;; Indent the target arrows a little for aesthetic purposes
                      (incf left 2) (decf right 2)
                      (incf top 2)  (decf bottom 2)
                      (ecase identity
                        (:scroll-up
                          (list left bottom
                                (+ left (/ (- right left) 2)) top
                                right bottom))
                        (:scroll-down
                          (list left top
                                (+ left (/ (- right left) 2)) bottom
                                right top))
                        (:scroll-left
                          (list right top
                                left (/ (+ top bottom) 2)
                                right bottom))
                        (:scroll-right
                          (list left top
                                right (/ (+ top bottom) 2)
                                left bottom))))))))
    (draw-polygon* medium coord-cache :filled filled :ink ink)))

(defclass scroll-bar-shaft-pane
          (immediate-sheet-input-mixin
           space-requirement-mixin
           sheet-single-child-mixin
           shared-medium-sheet-output-mixin
           sheet-with-resources-mixin
           pane-repaint-background-mixin
           leaf-pane)
    ((scroll-bar :initarg :scroll-bar)
     (needs-erase :initform nil)))



(defmethod initialize-instance :after ((pane scroll-bar-shaft-pane) &key)
  (let* ((scroll-bar (slot-value pane 'scroll-bar))
         (orientation (gadget-orientation scroll-bar)))
    (setf (slot-value pane 'pointer-cursor)
          (ecase orientation
            (:horizontal :horizontal-scroll)
            (:vertical :vertical-scroll)))))

(defmethod handle-repaint ((pane scroll-bar-shaft-pane) region)
  (declare (ignore region))
  (with-sheet-medium (medium pane)
    (with-bounding-rectangle* (left top right bottom) (sheet-region pane)
      (draw-rectangle* medium left top (1- right) (1- bottom)
                       :filled nil :ink (medium-foreground medium)))
    (draw-thumb pane medium)))

(defparameter *scroll-bar-thumb-ink* (make-gray-color 2/3))

(defmethod draw-thumb ((pane scroll-bar-shaft-pane) medium
                       &key (ink +foreground-ink+))
  (let ((needs-erase (slot-value pane 'needs-erase)))
    (setf (slot-value pane 'needs-erase) (not (eq ink +background-ink+)))
    (when (and (not needs-erase)
               (eq ink +background-ink+))
      (return-from draw-thumb (values))))
  (let* ((scroll-bar (slot-value pane 'scroll-bar))
         (scroll-value (gadget-value scroll-bar))
         (min-value (gadget-min-value scroll-bar))
         (max-value (gadget-max-value scroll-bar))
         (gadget-size 0 #+broken (or (scroll-bar-current-size scroll-bar) 0))
         (gadget-range (abs (- max-value min-value)))
         (identity (sheet-pointer-cursor pane)))
    (flet ((draw-car (medium left top right bottom which)
             (decf right) (decf bottom)
             (draw-rectangle* medium left top right bottom
                              :filled t
                              :ink (if (eq ink +foreground-ink+) *scroll-bar-thumb-ink* ink))
             (case which
               (:vertical-scroll
                 (draw-line* medium left top right top :ink ink)
                 (draw-line* medium left bottom right bottom :ink ink))
               (:horizontal-scroll
                 (draw-line* medium left bottom left top :ink ink)
                 (draw-line* medium right bottom right top :ink ink)))))
      (declare (dynamic-extent #'draw-car))
      (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
        (let* ((height (- y2 y1))
               (width (- x2 x1))
               (scroll-range (ecase identity
                               (:vertical-scroll height)
                               (:horizontal-scroll width)))
               (car-size (max 10 (* scroll-range (/ gadget-size gadget-range))))
               (elevator-top (compute-symmetric-value 
                               min-value max-value scroll-value 0
                               scroll-range))
               (elevator-bottom (+ elevator-top car-size)))
          (ecase identity
            (:vertical-scroll
              (draw-car medium
                        (+ x1 1)           (+ y1 elevator-top)
                        (+ x1 width -1) (+ y1 elevator-bottom)
                        identity))
            (:horizontal-scroll
              (draw-car medium
                        elevator-top      (+ y1 1)
                        elevator-bottom   (+ y1 height -1)
                        identity))))))))


(defmethod handle-event :around ((pane scroll-bar-target-pane) 
                                 (event pointer-button-press-event))
  (with-sheet-medium (medium pane)
    (draw-target pane medium :filled t :ink +foreground-ink+)
    (call-next-method)
    (draw-target pane medium :filled t :ink +background-ink+)
    (draw-target pane medium :filled nil)))

;;; This implements genera style scroll bars.
;;; Vertical Scroll bars
;;;    Top Target
;;;       Left: Next Line   Middle: First Page   Right: Previous Line
;;;    Bottom Target
;;;       Left: Next Page   Middle: Last Page    Right: Previous Page
(defmethod handle-event ((pane scroll-bar-target-pane) 
                         (event pointer-button-press-event))
  (let* ((scroll-bar (slot-value pane 'scroll-bar))
         (client (gadget-client scroll-bar))
         (id (gadget-id scroll-bar)))
    (case (pointer-event-button event)
      (#.+pointer-left-button+
       (ecase (slot-value pane 'end)
         (:greater-than
           (scroll-down-page-callback scroll-bar client id))
         (:less-than
           (scroll-down-line-callback scroll-bar client id))))
      (#.+pointer-middle-button+
       (ecase (slot-value pane 'end)
         (:greater-than
           (scroll-to-bottom-callback scroll-bar client id))
         (:less-than
           (scroll-to-top-callback scroll-bar client id))))
      (#.+pointer-right-button+
       (ecase (slot-value pane 'end)
         (:greater-than
           (scroll-up-page-callback scroll-bar client id))
         (:less-than
           (scroll-up-line-callback scroll-bar client id)))))))

(defmethod handle-event ((pane scroll-bar-shaft-pane) 
                         (event pointer-button-press-event))
  (let* ((x (pointer-event-x event))
         (y (pointer-event-y event))
         (scroll-bar (slot-value pane 'scroll-bar))
         (client (gadget-client scroll-bar))
         (id (gadget-id scroll-bar))
         (orientation (gadget-orientation scroll-bar)))
    (case (pointer-event-button event)
      (#.+pointer-left-button+
       (scroll-line-to-top-callback
         scroll-bar client id orientation x y))
      (#.+pointer-right-button+
       (scroll-line-to-bottom-callback
         scroll-bar client id orientation x (- (bounding-rectangle-height scroll-bar) y)))
      (#.+pointer-middle-button+
       (scroll-elevator-callback
         scroll-bar client id orientation x y)))))

(defmethod handle-event :after ((pane scroll-bar-shaft-pane) (event pointer-enter-event))
  (declare (special *pointer-documentation-output*))
  (frame-manager-display-pointer-documentation-string
    (frame-manager pane) (pane-frame pane) *pointer-documentation-output*
    (ecase (gadget-orientation (slot-value pane 'scroll-bar))
      (:vertical
        "L: This line to top; M: Proportional scrolling; R: Top line to here.")
      (:horizontal
        "L: This column to left edge; M: Proportional scrolling; R: Left edge to here."))))

(defmethod handle-event :after ((pane scroll-bar-target-pane) (event pointer-enter-event))
  (declare (special *pointer-documentation-output*))
  (frame-manager-display-pointer-documentation-string
    (frame-manager pane) (pane-frame pane) *pointer-documentation-output*
    (ecase (gadget-orientation (slot-value pane 'scroll-bar))
      (:vertical
        (ecase (slot-value pane 'end)
          (:greater-than
            "L: Next screenful; M: Last screenful; R: previous screenful.")
          (:less-than
            "L: Next line; M: First screenful; R: Previous line.")))
      (:horizontal
        (ecase (slot-value pane 'end)
          (:greater-than
            "L: Next screenful; M: Last screenful; R: previous screenful.")
          (:less-than
            "L: Next column; M: First screenful; R: Previous column."))))))

(defmethod handle-event :after ((pane scroll-bar-shaft-pane) (event pointer-exit-event))
  (declare (special *pointer-documentation-output*))
  (frame-manager-display-pointer-documentation-string 
    (frame-manager pane) (pane-frame pane) *pointer-documentation-output* nil))

(defmethod handle-event :after ((pane scroll-bar-target-pane) (event pointer-exit-event))
  (declare (special *pointer-documentation-output*))
  (frame-manager-display-pointer-documentation-string 
    (frame-manager pane) (pane-frame pane) *pointer-documentation-output* nil))

(defmethod handle-event :after ((pane scroll-bar-shaft-pane) (event pointer-event))
  (deallocate-event event))

(defmethod handle-event :after ((pane scroll-bar-target-pane) (event pointer-event))
  (deallocate-event event))

(defmethod update-scroll-bar-value ((pane scroll-bar-shaft-pane) scroll-bar coord min max)
  (let* ((min-value (gadget-min-value scroll-bar))
         (max-value (gadget-max-value scroll-bar))
         (value (compute-symmetric-value min max coord min-value max-value)))
    (setf (gadget-value scroll-bar) value)))

(defmethod (setf gadget-value) (value (pane scroll-bar-pane) &key invoke-callback)
  (declare (ignore invoke-callback))
  (setf (slot-value pane 'value) value))

(defmethod (setf gadget-value) :around (value (pane scroll-bar-pane) &key invoke-callback)
  #-aclpc (declare (ignore value invoke-callback))
  (if (port pane)
      (let ((shaft-pane (slot-value pane 'shaft-pane)))
        (with-sheet-medium (medium shaft-pane)
          (draw-thumb shaft-pane medium :ink +background-ink+)
          (call-next-method)
          (draw-thumb shaft-pane medium :ink +foreground-ink+)))
      (call-next-method)))

