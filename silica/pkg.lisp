;; -*- mode: common-lisp; package: user -*-
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
;; $fiHeader$

(defpackage :silica
  (:use clim-lisp clim-utils)
  (:export
   #:*default-text-style*
   #:*null-text-style*
   #:*standard-character-set* 
   #:*undefined-text-style* 
   #:+fill+
   #:action-gadget
   #:activate-gadget-event
   #:add-text-style-mapping
   #:adopt-child
   #:adopt-frame
   #:allocate-space
   #:application-frame
   #:beep
   #:canvas
   #:char-character-set-and-index
   #:char-width
   #:click-event
   #:compose-space
   #:copy-area
   #:copy-area*
   #:define-character-face
   #:define-character-face-added-mappings
   #:define-character-face-class
   #:define-display-device
   #:define-text-style-mappings 
   #:diacritic-char-p
   #:disable-mirror
   #:dispatch-event
   #:dispatch-repaint
   #:display-device 
   #:distribute-event
   #:draw-circle
   #:draw-circle*
   #:draw-ellipse
   #:draw-ellipse*
   #:draw-image
   #:draw-image*
   #:draw-line
   #:draw-line* 
   #:draw-lines
   #:draw-lines*
   #:draw-point 
   #:draw-point* 
   #:draw-points 
   #:draw-points*
   #:draw-polygon
   #:draw-polygon*
   #:draw-rectangle
   #:draw-rectangle*
   #:draw-spline
   #:draw-spline*
   #:draw-text
   #:draw-text*
   #:draw-text-rectangle
   #:draw-text-rectangle*
   #:enable-mirror
   #:engraft-medium
   #:event-modifier-key-state
   #:event-sheet
   #:find-graft
   #:find-port
   #:find-port-type
   #:frame-manager
   #:frame-name
   #:frame-panes
   #:frame-shell
   #:frame-top-level-sheet
   #:gadget
   #:get-port-canonical-gesture-spec
   #:graft
   #:graftp
   #:handle-repaint
   #:intern-text-style
   #:key-press-event
   #:key-release-event
   #:keyboard-event-character
   #:keyboard-event-key-name
   #:make-frame-manager
   #:make-medium
   #:make-rectangle*
   #:medium
   #:medium-background
   #:medium-clipping-region
   #:medium-default-text-style
   #:medium-draw-text
   #:medium-foreground
   #:medium-ink
   #:medium-line-style
   #:medium-sheet
   #:medium-text-style
   #:medium-transformation
   #:merge-text-styles 
   #:merged-text-style 
   #:mirror-inside-edges*
   #:mirror-native-edges*
   #:mirror-region
   #:mirror-region*
   #:mirror-region-updated
   #:mirrored-sheet-mixin
   #:modifier-keysym
   #:mute-repainting-mixin
   #:note-frame-disabled
   #:note-frame-enabled
   #:note-sheet-grafted
   #:note-sheet-region-changed
   #:parse-gesture-spec
   #:parse-text-style 
   #:permanent-medium-sheet-output-mixin
   #:pointer-button-event
   #:pointer-enter-event
   #:pointer-event-native-x
   #:pointer-event-native-y
   #:pointer-event-x
   #:pointer-event-y
   #:pointer-exit-event
   #:pointer-motion-event
   #:pointer-native-event-x
   #:pointer-native-event-y
   #:pointer-press-event
   #:pointer-release-event
   #:port
   #:port-color-cache
   #:port-draw-line*
   #:port-draw-rectangle*
   #:port-draw-text*
   #:port-event-wait
   #:port-force-output
   #:port-glyph-for-character
   #:port-pointer
   #:process-next-event
   #:push-button
   #:queue-event
   #:realize-graft
   #:realize-mirror
   #:realize-pane-internal
   #:repaint-sheet
   #:set-sheet-mirror-edges*
   #:sheet
   #:sheet-actual-native-edges
   #:sheet-children
   #:sheet-device-region
   #:sheet-device-transformation
   #:sheet-direct-mirror
   #:sheet-enabled-p
   #:sheet-height
   #:sheet-leaf-mixin
   #:sheet-medium
   #:sheet-mirror
   #:sheet-mirrored-ancestor
   #:sheet-multiple-child-mixin
   #:sheet-mute-input-mixin
   #:sheet-native-transformation
   #:sheet-parent
   #:sheet-permanently-enabled-mixin
   #:sheet-region
   #:sheet-single-child-mixin
   #:sheet-transformation
   #:sheet-transformation-mixin
   #:sheet-width
   #:slider
   #:standard-repainting-medium
   #:standard-sheet
   #:standard-sheet-input-mixin
   #:standarize-style
   #:stream-glyph-for-character
   #:string-height 
   #:string-width 
   #:text-field
   #:text-style
   #:text-style-ascent 
   #:text-style-components
   #:text-style-descent 
   #:text-style-face
   #:text-style-family 
   #:text-style-height
   #:text-style-mapping 
   #:text-style-scale 
   #:text-style-size
   #:toggle-button
   #:transform-point*
   #:value-changed-gadget-event
   #:value-gadget
   #:window-configuration-event
   #:window-repaint-event
   #:with-drawing-options
   #:with-rotation with-scaling
   #:with-sheet-medium
   #:with-text-face
   #:with-text-family
   #:with-text-size
   #:with-text-style 
   #:with-translation
   #:stream-write-string-internal
   #:make-application-frame
   #:enable-frame
   #:sheet-event-queue
   #:port-note-cursor-change
   #:update-scrollbars
   #:event
   #:make-text-style
   #:pointer-event-button
   #:medium-+y-upward-p
   #:define-application-frame
   #:run-frame-top-level
   #:vertically
   #:realize-pane
   #:space-req-height
   #:space-req-width
   #:scroller-pane
   
   #:medium-merged-text-style
   #:medium-default-text-style
   #:medium-text-style
   #:medium-merged-text-style-valid
   #:beep
   #:port-beep
   #:port-server-path
   #:change-space-req
   ))




