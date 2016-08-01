;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."


;;; (generate-prefill-dispatch-caches 'design)

(prefill-dispatch-caches
  (clx-clim::clx-decode-color
    (gray-color t)
    (flipping-ink t)
    (rgb-color t)
    (standard-opacity t))
  (clx-clim::clx-decode-ink
    (gray-color t)
    (flipping-ink t)
    (rgb-color t)
    (contrasting-ink t)
    (rectangular-tile t)
    (pattern t))
  (clx-clim::clx-decode-pattern
    (pattern t)))


;;; (generate-prefill-dispatch-caches 'medium)

(prefill-dispatch-caches)


;;; (generate-prefill-dispatch-caches 'port)

(prefill-dispatch-caches
  (clx-clim::initialize-clx-port))


;;; (generate-prefill-dispatch-caches 'application-frame)

(prefill-dispatch-caches
  (frame-wrapper
    (clx-clim::clx-frame-manager menu-frame t)
    (clx-clim::clx-frame-manager accept-values-own-window t))
  (generate-panes
    (clx-clim::clx-frame-manager menu-frame)
    (clx-clim::clx-frame-manager accept-values)
    (clx-clim::clx-frame-manager accept-values-own-window))
  (note-frame-disabled
    (clx-clim::clx-frame-manager menu-frame)
    (clx-clim::clx-frame-manager accept-values-own-window))
  (note-frame-enabled
    (clx-clim::clx-frame-manager menu-frame)
    (clx-clim::clx-frame-manager accept-values-own-window)))


;;; (generate-prefill-dispatch-caches 'frame-manager)

(prefill-dispatch-caches
  (adopt-frame
    (clx-clim::clx-frame-manager t))
  (frame-manager-dialog-view
    (clx-clim::clx-frame-manager))
  (frame-manager-display-pointer-documentation
    (clx-clim::clx-frame-manager t t t t t t t))
  ((setf frame-manager-frames)
   (t clx-clim::clx-frame-manager))
  (frame-manager-menu-choose
    (clx-clim::clx-frame-manager t))
  (graft
    (clx-clim::clx-frame-manager))
  (make-pane-1
    (clx-clim::clx-frame-manager t t))
  (make-pane-class
    (clx-clim::clx-frame-manager t))
  (make-pane-arglist
    (clx-clim::clx-frame-manager t))
  (port
    (clx-clim::clx-frame-manager))
  (print-object
    (clx-clim::clx-frame-manager t))
  #+Genera (clos-internals::print-self
	     (clx-clim::clx-frame-manager t t t))
  (update-frame-settings
    (clx-clim::clx-frame-manager t))
  #+Genera (clos-internals::which-operations
	     (clx-clim::clx-frame-manager)))


;;; (generate-prefill-dispatch-caches 'clx-clim::clx-medium)

(prefill-dispatch-caches
  (degraft-medium
    (clx-clim::clx-medium clx-clim::clx-port t))
  (engraft-medium
    (clx-clim::clx-medium clx-clim::clx-port vbox-pane)
    (clx-clim::clx-medium clx-clim::clx-port outlined-pane)
    (clx-clim::clx-medium clx-clim::clx-port scroller-pane)
    (clx-clim::clx-medium clx-clim::clx-port table-pane)
    (clx-clim::clx-medium clx-clim::clx-port scroll-bar-pane)
    (clx-clim::clx-medium clx-clim::clx-port spacing-pane)
    (clx-clim::clx-medium clx-clim::clx-port hbox-pane)
    (clx-clim::clx-medium clx-clim::clx-port scroll-bar-target-pane)
    (clx-clim::clx-medium clx-clim::clx-port scroll-bar-shaft-pane)
    (clx-clim::clx-medium clx-clim::clx-port viewport)
    (clx-clim::clx-medium clx-clim::clx-port interactor-pane)
    (clx-clim::clx-medium clx-clim::clx-port slider-pane)
    (clx-clim::clx-medium clx-clim::clx-port radio-box-pane)
    (clx-clim::clx-medium clx-clim::clx-port toggle-button-pane)
    (clx-clim::clx-medium clx-clim::clx-port push-button-pane)
    (clx-clim::clx-medium clx-clim::clx-port command-menu-pane)
    (clx-clim::clx-medium clx-clim::clx-port application-pane)
    (clx-clim::clx-medium clx-clim::clx-port clim-stream-pane)
    (clx-clim::clx-medium clx-clim::clx-port top-level-sheet))
  (invoke-with-drawing-options
    (clx-clim::clx-medium t))
  (invoke-with-text-style
    (clx-clim::clx-medium t t t))
  (medium-force-output
    (clx-clim::clx-medium))
  (medium-beep
    (clx-clim::clx-medium))
  (medium-+y-upward-p
    (clx-clim::clx-medium))
  ((setf medium-+y-upward-p)
    (t clx-clim::clx-medium))
  (medium-background
    (clx-clim::clx-medium))
  (medium-default-text-style
    (clx-clim::clx-medium))
  (medium-draw-ellipse*
    (clx-clim::clx-medium t t t t t t t t t))
  (medium-draw-line*
    (clx-clim::clx-medium t t t t))
  (medium-draw-polygon*
    (clx-clim::clx-medium t t t))
  (medium-draw-rectangle*
    (clx-clim::clx-medium t t t t t))
  (medium-draw-text*
    (clx-clim::clx-medium t t t t t t t t t t))
  (medium-copy-area
    (clx-clim::clx-medium t t t t clx-clim::clx-medium t t))
  (medium-foreground
    (clx-clim::clx-medium))
  (medium-ink
    (clx-clim::clx-medium))
  ((setf medium-ink)
    (t clx-clim::clx-medium))
  (medium-line-style
    (clx-clim::clx-medium))
  ((setf medium-line-style)
    (t clx-clim::clx-medium))
  (medium-merged-text-style
    (clx-clim::clx-medium))
  (medium-text-style
    (clx-clim::clx-medium))
  (medium-transformation
    (clx-clim::clx-medium))
  ((setf medium-transformation)
    (t clx-clim::clx-medium))
  #+Genera (clos-internals:operation-handled-p
	     (clx-clim::clx-medium t))
  (port
    (clx-clim::clx-medium))
  (print-object
    (clx-clim::clx-medium t))
  #+Genera (clos-internals::print-self
	     (clx-clim::clx-medium t t t))
  #+Genera (clos-internals::send-if-handles
	     (clx-clim::clx-medium t))
  (stream-string-output-size
    (clx-clim::clx-medium t))
  (stream-string-width
    (clx-clim::clx-medium t))
  (text-style-ascent
    (standard-text-style clx-clim::clx-medium))
  (text-style-descent
    (standard-text-style clx-clim::clx-medium))
  (text-style-height
    (standard-text-style clx-clim::clx-medium))
  (text-style-width
    (standard-text-style clx-clim::clx-medium))
  (text-size
    (clx-clim::clx-medium t))
  #+Genera (clos-internals::which-operations
	     (clx-clim::clx-medium)))


;;; (generate-prefill-dispatch-caches 'clx-clim::clx-port)

(prefill-dispatch-caches
  (degraft-medium)
  (disable-mirror
    (clx-clim::clx-port t))
  (distribute-event
    (clx-clim::clx-port t))
  (distribute-event-1
    (clx-clim::clx-port pointer-button-release-event)
    (clx-clim::clx-port pointer-button-press-event)
    (clx-clim::clx-port key-press-event)
    (clx-clim::clx-port key-release-event)
    (clx-clim::clx-port pointer-motion-event))
  (enable-mirror
    (clx-clim::clx-port t))
  (engraft-medium)
  (clx-clim::initialize-clx-port
    (clx-clim::clx-port))
  (initialize-menu
    (clx-clim::clx-port clim-stream-pane))
  (make-frame-manager
    (clx-clim::clx-port))
  (make-medium
    (clx-clim::clx-port t))
  (mirror-inside-edges*
    (clx-clim::clx-port t))
  (mirror-native-edges*
    (clx-clim::clx-port t))
  (mirror-region
    (clx-clim::clx-port t))
  (mirror-region*
    (clx-clim::clx-port t))
  (mirror-region-updated
    (clx-clim::clx-port top-level-sheet))
  #+Genera (clos-internals:operation-handled-p
	     (clx-clim::clx-port t))
  ((setf port)
    (clx-clim::clx-port t))
  (port-draw-cursor
    (clx-clim::clx-port standard-text-cursor t t t t))
  (port-event-loop
    (clx-clim::clx-port))
  (port-glyph-for-character
    (clx-clim::clx-port t t))
  (port-graft-class
    (clx-clim::clx-port))
  (port-grafts
    (clx-clim::clx-port))
  ((setf port-grafts)
    (t clx-clim::clx-port))
  ((setf port-keyboard-input-focus)
    (t clx-clim::clx-port))
  (port-note-cursor-change
    (clx-clim::clx-port t t (eql 'cursor-focus) t t)
    (clx-clim::clx-port t t (eql 'cursor-active) t t)
    (clx-clim::clx-port t t (eql 'cursor-state) t t))
  (port-pointer
    (clx-clim::clx-port))
  ((setf port-pointer)
    (t clx-clim::clx-port))
  ((setf port-process)
    (t clx-clim::clx-port))
  (port-server-path
    (clx-clim::clx-port))
  (medium-text-bounding-box
    (clx-clim::clx-medium t t t t t t t t t t t t))
  (port-trace-thing
    (clx-clim::clx-port))
  (port-type
    (clx-clim::clx-port))
  (print-object
    (clx-clim::clx-port t))
  #+Genera (clos-internals::print-self
	     (clx-clim::clx-port t t t))
  (process-next-event
    (clx-clim::clx-port))
  (realize-graft
    (clx-clim::clx-port t))
  (realize-mirror
    (clx-clim::clx-port top-level-sheet))
  (restart-port
    (clx-clim::clx-port))
  #+Genera (clos-internals::send-if-handles
	     (clx-clim::clx-port t))
  (set-sheet-mirror-edges*
    (clx-clim::clx-port t t t t t))
  (shared-initialize
    (clx-clim::clx-port t))
  (standardize-text-style
    (clx-clim::clx-port t))
  (text-style-mapping
    (clx-clim::clx-port standard-text-style))
  ((setf text-style-mapping)
    (t clx-clim::clx-port t))
  (text-style-mapping*
    (clx-clim::clx-port t))
  (update-mirror-region
    (clx-clim::clx-port top-level-sheet))
  (update-mirror-region-1
    (clx-clim::clx-port t t))
  (update-mirror-transformation
    (clx-clim::clx-port top-level-sheet)
    (clx-clim::clx-port standard-graft))
  (update-mirror-transformation-1
    (clx-clim::clx-port t t))
  #+Genera (clos-internals::which-operations
	     (clx-clim::clx-port)))


;;; Compile constructors

#+++ignore	;this just doesn't seem to do any good
(ensure-constructors-compiled
  clx-clim::clx-frame-manager
  clx-clim::clx-medium
  clx-clim::clx-pixmap
  clx-clim::clx-pixmap-medium
  clx-clim::clx-port)

