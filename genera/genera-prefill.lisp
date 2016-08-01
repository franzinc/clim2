;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-


(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."


;;; (generate-prefill-dispatch-caches 'design)

(prefill-dispatch-caches
  (genera-clim::genera-decode-color
    (gray-color t)
    (flipping-ink t)
    (rgb-color t)
    (standard-opacity t))
  (genera-clim::genera-decode-ink
    (gray-color t)
    (flipping-ink t)
    (rgb-color t)
    (contrasting-ink t)
    (rectangular-tile t)
    (pattern t))
  (genera-clim::genera-decode-pattern
    (pattern t)))


;;; (generate-prefill-dispatch-caches 'medium)

(prefill-dispatch-caches
  (genera-clim::medium-drawing-possible)
  (genera-clim::invoke-with-appropriate-drawing-state))


;;; (generate-prefill-dispatch-caches 'port)

(prefill-dispatch-caches
  (genera-clim::genera-mirror-native-edges*)
  (genera-clim::initialize-genera-port))


;;; (generate-prefill-dispatch-caches 'application-frame)

(prefill-dispatch-caches
  (frame-wrapper
    (genera-clim::genera-frame-manager menu-frame t)
    (genera-clim::genera-frame-manager accept-values-own-window t))
  (generate-panes
    (genera-clim::genera-frame-manager menu-frame)
    (genera-clim::genera-frame-manager accept-values)
    (genera-clim::genera-frame-manager accept-values-own-window))
  (note-frame-disabled
    (genera-clim::genera-frame-manager menu-frame)
    (genera-clim::genera-frame-manager accept-values-own-window))
  (note-frame-enabled
    (genera-clim::genera-frame-manager menu-frame)
    (genera-clim::genera-frame-manager accept-values-own-window)))


;;; (generate-prefill-dispatch-caches 'frame-manager)

(prefill-dispatch-caches
  (adopt-frame
    (genera-clim::genera-frame-manager t))
  (frame-manager-dialog-view
    (genera-clim::genera-frame-manager))
  (frame-manager-display-pointer-documentation
    (genera-clim::genera-frame-manager t t t t t t t))
  ((setf frame-manager-frames)
   (t genera-clim::genera-frame-manager))
  (frame-manager-menu-choose
    (genera-clim::genera-frame-manager t))
  (graft
    (genera-clim::genera-frame-manager))
  (make-pane-1
    (genera-clim::genera-frame-manager t t))
  (make-pane-class
    (genera-clim::genera-frame-manager t))
  (make-pane-arglist
    (genera-clim::genera-frame-manager t))
  (port
    (genera-clim::genera-frame-manager))
  (print-object
    (genera-clim::genera-frame-manager t))
  (clos-internals::print-self
    (genera-clim::genera-frame-manager t t t))
  (update-frame-settings
    (genera-clim::genera-frame-manager t))
  (clos-internals::which-operations
    (genera-clim::genera-frame-manager)))


;;; (generate-prefill-dispatch-caches 'genera-clim::genera-medium)

(prefill-dispatch-caches
  (degraft-medium
    (genera-clim::genera-medium genera-clim::genera-port t))
  (engraft-medium
    (genera-clim::genera-medium genera-clim::genera-port vbox-pane)
    (genera-clim::genera-medium genera-clim::genera-port outlined-pane)
    (genera-clim::genera-medium genera-clim::genera-port scroller-pane)
    (genera-clim::genera-medium genera-clim::genera-port table-pane)
    (genera-clim::genera-medium genera-clim::genera-port scroll-bar-pane)
    (genera-clim::genera-medium genera-clim::genera-port spacing-pane)
    (genera-clim::genera-medium genera-clim::genera-port hbox-pane)
    (genera-clim::genera-medium genera-clim::genera-port scroll-bar-target-pane)
    (genera-clim::genera-medium genera-clim::genera-port scroll-bar-shaft-pane)
    (genera-clim::genera-medium genera-clim::genera-port viewport)
    (genera-clim::genera-medium genera-clim::genera-port interactor-pane)
    (genera-clim::genera-medium genera-clim::genera-port slider-pane)
    (genera-clim::genera-medium genera-clim::genera-port radio-box-pane)
    (genera-clim::genera-medium genera-clim::genera-port toggle-button-pane)
    (genera-clim::genera-medium genera-clim::genera-port push-button-pane)
    (genera-clim::genera-medium genera-clim::genera-port command-menu-pane)
    (genera-clim::genera-medium genera-clim::genera-port application-pane)
    (genera-clim::genera-medium genera-clim::genera-port clim-stream-pane)
    (genera-clim::genera-medium genera-clim::genera-port top-level-sheet))
  (invoke-with-drawing-options
    (genera-clim::genera-medium t))
  (invoke-with-text-style
    (genera-clim::genera-medium t t t))
  (medium-force-output
    (genera-clim::genera-medium))
  (medium-beep
    (genera-clim::genera-medium))
  (medium-+y-upward-p
    (genera-clim::genera-medium))
  ((setf medium-+y-upward-p)
    (t genera-clim::genera-medium))
  (medium-background
    (genera-clim::genera-medium))
  (medium-default-text-style
    (genera-clim::genera-medium))
  (medium-draw-ellipse*
    (genera-clim::genera-medium t t t t t t t t t))
  (medium-draw-line*
    (genera-clim::genera-medium t t t t))
  (medium-draw-polygon*
    (genera-clim::genera-medium t t t))
  (medium-draw-rectangle*
    (genera-clim::genera-medium t t t t t))
  (medium-draw-text*
    (genera-clim::genera-medium t t t t t t t t t t))
  (medium-copy-area
    (genera-clim::genera-medium t t t t genera-clim::genera-medium t t))
  (genera-clim::medium-drawing-possible
    (genera-clim::genera-medium))
  (medium-foreground
    (genera-clim::genera-medium))
  (medium-ink
    (genera-clim::genera-medium))
  ((setf medium-ink)
    (t genera-clim::genera-medium))
  (medium-line-style
    (genera-clim::genera-medium))
  ((setf medium-line-style)
    (t genera-clim::genera-medium))
  (medium-merged-text-style
    (genera-clim::genera-medium))
  (medium-text-style
    (genera-clim::genera-medium))
  (medium-transformation
    (genera-clim::genera-medium))
  ((setf medium-transformation)
    (t genera-clim::genera-medium))
  (clos-internals:operation-handled-p
    (genera-clim::genera-medium t))
  (port
    (genera-clim::genera-medium))
  (print-object
    (genera-clim::genera-medium t))
  (clos-internals::print-self
    (genera-clim::genera-medium t t t))
  (clos-internals::send-if-handles
    (genera-clim::genera-medium t))
  (stream-string-output-size
    (genera-clim::genera-medium t))
  (stream-string-width
    (genera-clim::genera-medium t))
  (text-size
    (genera-clim::genera-medium t))
  (text-style-ascent
    (standard-text-style genera-clim::genera-medium))
  (text-style-descent
    (standard-text-style genera-clim::genera-medium))
  (text-style-height
    (standard-text-style genera-clim::genera-medium))
  (text-style-width
    (standard-text-style genera-clim::genera-medium))
  (clos-internals::which-operations
    (genera-clim::genera-medium))
  (genera-clim::invoke-with-appropriate-drawing-state
    (genera-clim::genera-medium t t t t)))


;;; (generate-prefill-dispatch-caches 'genera-clim::genera-port)

(prefill-dispatch-caches
  (degraft-medium)
  (disable-mirror
    (genera-clim::genera-port t))
  (distribute-event
    (genera-clim::genera-port t))
  (distribute-event-1
    (genera-clim::genera-port pointer-button-release-event)
    (genera-clim::genera-port pointer-button-press-event)
    (genera-clim::genera-port key-press-event)
    (genera-clim::genera-port key-release-event)
    (genera-clim::genera-port pointer-motion-event))
  (enable-mirror
    (genera-clim::genera-port t))
  (engraft-medium)
  (genera-clim::genera-mirror-native-edges*
    (genera-clim::genera-port t))
  (genera-clim::initialize-genera-port
    (genera-clim::genera-port))
  (initialize-menu
    (genera-clim::genera-port clim-stream-pane))
  (make-frame-manager
    (genera-clim::genera-port))
  (make-medium
    (genera-clim::genera-port t))
  (mirror-inside-edges*
    (genera-clim::genera-port t))
  (mirror-native-edges*
    (genera-clim::genera-port t))
  (mirror-region
    (genera-clim::genera-port t))
  (mirror-region*
    (genera-clim::genera-port t))
  (mirror-region-updated
    (genera-clim::genera-port top-level-sheet))
  (clos-internals:operation-handled-p
    (genera-clim::genera-port t))
  ((setf port)
    (genera-clim::genera-port t))
  (port-draw-cursor
    (genera-clim::genera-port standard-text-cursor t t t t))
  (port-event-loop
    (genera-clim::genera-port))
  (port-glyph-for-character
    (genera-clim::genera-port t t))
  (port-graft-class
    (genera-clim::genera-port))
  (port-grafts
    (genera-clim::genera-port))
  ((setf port-grafts)
    (t genera-clim::genera-port))
  ((setf port-keyboard-input-focus)
    (t genera-clim::genera-port))
  (port-note-cursor-change
    (genera-clim::genera-port t t (eql 'cursor-focus) t t)
    (genera-clim::genera-port t t (eql 'cursor-active) t t)
    (genera-clim::genera-port t t (eql 'cursor-state) t t))
  (port-pointer
    (genera-clim::genera-port))
  ((setf port-pointer)
    (t genera-clim::genera-port))
  ((setf port-process)
    (t genera-clim::genera-port))
  (port-server-path
    (genera-clim::genera-port))
  (medium-text-bounding-box
    (genera-clim::genera-medium t t t t t t t t t t t t))
  (port-trace-thing
    (genera-clim::genera-port))
  (port-type
    (genera-clim::genera-port))
  (print-object
    (genera-clim::genera-port t))
  (clos-internals::print-self
    (genera-clim::genera-port t t t))
  (process-next-event
    (genera-clim::genera-port))
  (realize-graft
    (genera-clim::genera-port t))
  (realize-mirror
    (genera-clim::genera-port top-level-sheet))
  (restart-port
    (genera-clim::genera-port))
  (clos-internals::send-if-handles
    (genera-clim::genera-port t))
  (set-sheet-mirror-edges*
    (genera-clim::genera-port t t t t t))
  (shared-initialize
    (genera-clim::genera-port t))
  (standardize-text-style
    (genera-clim::genera-port t))
  (text-style-mapping
    (genera-clim::genera-port standard-text-style))
  ((setf text-style-mapping)
    (t genera-clim::genera-port t))
  (text-style-mapping*
    (genera-clim::genera-port t))
  (update-mirror-region
    (genera-clim::genera-port top-level-sheet))
  (update-mirror-region-1
    (genera-clim::genera-port t t))
  (update-mirror-transformation
    (genera-clim::genera-port top-level-sheet)
    (genera-clim::genera-port standard-graft))
  (update-mirror-transformation-1
    (genera-clim::genera-port t t))
  (clos-internals::which-operations
    (genera-clim::genera-port)))


;;; Compile constructors

#+++ignore	;this just doesn't seem to do any good
(ensure-constructors-compiled
  genera-clim::genera-frame-manager
  genera-clim::genera-medium
  genera-clim::genera-pixmap
  genera-clim::genera-pixmap-medium
  genera-clim::genera-port)

