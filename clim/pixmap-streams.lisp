;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1992 Franz, Inc.  All rights reserved."


;;; CLIM pixmap streams

(defclass pixmap-stream (output-recording-mixin
                         input-protocol-mixin
                         output-protocol-mixin
                         pixmap-sheet)
    ()
  (:default-initargs :text-cursor nil))

;; We can be a bit smarter than the usual method, in that if the width
;; and height are not supplied, we can deduce them.  Also, this will
;; support text output, formatted output, etc.
;;--- Note that, since this calls the continuation on the original stream
;;--- instead of the pixmap stream, things like WINDOW-CLEAR will affect
;;--- the wrong stream
#-(or allegro aclpc) ;;--- SWM is willing to live with this, but not CER
(defmethod invoke-with-output-to-pixmap ((stream output-protocol-mixin) continuation
                                         &key width height)
  (let ((record
          (with-output-to-output-record (stream)
            (funcall continuation stream))))
    (unless (and width height)
      (output-record-set-position record 0 0)
      (multiple-value-setq (width height) (bounding-rectangle-size record)))
    (let* ((pixmap-medium (make-pixmap-medium (port stream) stream
                                              :width width :height height))
           (pixmap-stream (make-instance 'pixmap-stream
                            :default-text-margin width
                            :port (port stream)
                            :medium pixmap-medium
                            :width width :height height)))
      (setf (medium-foreground pixmap-medium) (medium-foreground stream)
            (medium-background pixmap-medium) (medium-background stream))
      (replay record pixmap-stream)
      (slot-value pixmap-medium 'silica::pixmap))))

#+(or allegro aclpc)
(defmethod invoke-with-output-to-pixmap ((stream output-protocol-mixin) continuation
                                                                        &key width height)
  (let (record)
    (unless (and width height)
      (setq record (with-output-to-output-record (stream)
                     (funcall continuation stream)))
      (output-record-set-position record 0 0)
      (multiple-value-setq (width height) (bounding-rectangle-size record)))
    (let* ((pixmap-medium (make-pixmap-medium (port stream) stream
                                              :width width :height height))
           (pixmap-stream (make-instance 'pixmap-stream
                                         :default-text-margin width
                                         :port (port stream)
                                         :medium pixmap-medium
                                         :width width :height height)))
      (setf (medium-foreground pixmap-medium) (medium-foreground stream)
            (medium-background pixmap-medium) (medium-background stream)
            (medium-default-text-style pixmap-medium) (pane-text-style stream)
            (medium-text-style pixmap-medium) (pane-text-style stream))
      ;; this is probably OK on UNIX ports as well - check at merge
      ;; time (cim 10/4/96)
      #+(or aclpc acl86win32)
      (draw-rectangle* pixmap-stream 0 0 width height
                       :ink +background-ink+ :filled t)
      (if record
          (replay record pixmap-stream)
        (funcall continuation pixmap-stream))
      (slot-value pixmap-medium 'silica::pixmap))))

(defconstant +gray-out-ink+
    (compose-in (make-opacity .5) +background-ink+))

(defun pixmap-from-menu-item (associated-window menu-item printer presentation-type
                              &key text-style background foreground gray-out
                              &allow-other-keys)
  (with-menu (menu associated-window)
    (let ((record (with-output-recording-options (menu :draw nil :record t)
                    (with-output-to-output-record (menu)
                      (handler-case
                          (with-text-style (menu text-style)
                            (if presentation-type
                                (present menu-item presentation-type :stream menu)
                              (funcall printer menu-item menu)))
                        (error ()
                          (write-string "Error in printer" menu)))))))
      (multiple-value-bind (width height)
          (bounding-rectangle-size record)
        (assert (and (plusp width) (plusp height))
            () "Width and height of output must be greater than zero")
        (with-output-to-pixmap (stream associated-window :width width :height height)
          (when text-style
            (setf (medium-default-text-style stream) text-style))
          (when background
            (setf (medium-background stream) background))
          (when foreground
            (setf (medium-foreground stream) foreground))
          (draw-rectangle* stream 0 0 width height
                           :ink +background-ink+ :filled t)
          (replay-output-record
           record stream +everywhere+
           (- (bounding-rectangle-left record))
           (- (bounding-rectangle-top record)))
	  ;;--- on windoze, ideally we would create two output records, one
	  ;;--- in light 3d-color and one in dark 3d-color, replay the light
	  ;;--- one and then draw the dark one over it offset up and to the
	  ;;--- left by one pixel.  Need to associate a palette with the
	  ;;--- button in order to do this, though, and I haven't yet figured
	  ;;--- out how to do that...
          (when gray-out
            (draw-rectangle* stream 0 0 width height
                             :ink #-(or aclpc acl86win32) +gray-out-ink+
			     ;;--- no stipples on windoze, but
			     ;;--- unfortunately this solution just
			     ;;--- paints an ugly white box rather than
			     ;;--- doing the gray-out thang. -tjm
			     #+(or aclpc acl86win32) +white+
			     :filled t)))))))

;;; This code would be nice to use since it will eliminate the creation of
;;; a whole bunch of pixmaps. However, quite often you get multiple text
;;; output records and presentations. I suppose in theory this could be
;;; handled by making sure that the text-output-records line up nicely
;;; but........ This is tricky and will be more efficient than creating a
;;; pixmap?

#+ignore
(defun find-single-text-output-record (record)
  ;; If the output history contains just one text output record
  ;; nested inside presentations and boring types of composite output
  ;; records then return it
  (loop
    (let ((class (class-of record)))
      (cond ((or (eq class (find-class 'standard-presentation))
                 (eq class (find-class 'standard-sequence-output-record)))
             (if (= (output-record-count record) 1)
                 (setq record (output-record-element record 0))
               (return nil)))
            ((eq class (find-class 'standard-text-output-record))
             (return record))
            (t (return nil))))))

