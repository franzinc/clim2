;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1989, 1990 International Lisp Associates."

(defparameter *dragging-output-finish-on-release* nil)

;;; Yay!
(defmacro dragging-output ((&optional stream 
                            &key (repaint t) multiple-window
                                 (finish-on-release *dragging-output-finish-on-release*))
                           &body body)
  (default-output-stream stream dragging-output)
  (let ((output-record '#:output-record))
    `(let ((,output-record
            (with-output-to-output-record (,stream) ,@body)))
       (drag-output-record ,stream ,output-record
                           :repaint ,repaint :multiple-window ,multiple-window
                           :finish-on-release ,finish-on-release))))

(defun drag-output-record (stream output-record
                           &key (repaint t) multiple-window
                                (erase #'erase-output-record erase-sp) feedback
                                (finish-on-release *dragging-output-finish-on-release*))
  (declare (values final-x final-y delta-x delta-y))
  (let (last-x last-y
        (delta-x 0)
        (delta-y 0)
        (parent (output-record-parent output-record))
        ;; Clipping region for repainting the damaged region
        (region (bounding-rectangle output-record)))

    ;; If the default erase-output-record is used then it is not
    ;; necessary to repaint because erase-output-record does a replay
    (unless erase-sp
      (setq repaint nil))
    
    (multiple-value-bind (initial-x initial-y)
        (stream-pointer-position stream)
      (declare (type coordinate initial-x initial-y))
      (multiple-value-bind (x-offset y-offset)
          (convert-from-relative-to-absolute-coordinates
            stream (output-record-parent output-record))
        (declare (type coordinate x-offset y-offset))
        (multiple-value-bind (record-x record-y)
            (bounding-rectangle-position output-record)
          ;; Deltas are the position of the mouse in the local coordinate
          ;; system of the record
          (setq delta-x (- initial-x record-x)
                delta-y (- initial-y record-y)))
        (incf initial-x x-offset)
        (incf initial-y y-offset)
        (flet ((finish (window x y)
                 (when last-x
                   (if feedback
                       (funcall feedback output-record stream
                                initial-x initial-y last-x last-y :erase)
                       (funcall erase output-record stream))
                   ;; Note the asymmetry in the we call the feedback function
                   ;; with LAST-X/Y, but set the position to X/Y.  This is 
                   ;; because the user can be moving the pointer really fast,
                   ;; causing FINISH to be called on coordinates which are
                   ;; "before" the values of LAST-X/Y.
                   (unless (or multiple-window
                               (eq window stream))
                     ;; If the user didn't ask for multiple windows, and 
                     ;; he moved into another window, set things back
                     (setq x (- initial-x x-offset)
                           y (- initial-y y-offset)
                           delta-x 0
                           delta-y 0))
                   (output-record-set-position 
                     output-record (- x delta-x) (- y delta-y))
                   (when parent
                     (add-output-record output-record parent)
                     (tree-recompute-extent output-record)
                     (replay-output-record output-record stream nil x-offset y-offset)))
                 (return-from drag-output-record
                   (values x y delta-x delta-y))))
          (declare (dynamic-extent #'finish))
          (with-output-recording-options (stream :record nil :draw t)
            (when feedback
              (funcall erase output-record stream)
              (funcall feedback output-record stream
                       initial-x initial-y initial-x initial-y :draw)
              (setq last-x initial-x last-y initial-y))
            (tracking-pointer (stream :multiple-window multiple-window)
              (:pointer-motion (window x y)
               (when (eq window stream)
                 (when (or (not (eq x last-x))
                           (not (eq y last-y)))
                   (when last-x
                     (if feedback
                         (funcall feedback output-record stream
                                  initial-x initial-y last-x last-y :erase)
                         (funcall erase output-record stream)))
                   (setq last-x x last-y y)
                   ;; Remember the space the record used to occupy
                   (with-bounding-rectangle* (rl rt rr rb) output-record
                     (translate-coordinates x-offset y-offset rl rt rr rb)
                     (bounding-rectangle-set-edges region rl rt rr rb))
                   ;; Move the record
                   (output-record-set-position output-record (- x delta-x) (- y delta-y))
                   ;; Replay the region it used to live in, and then replay
                   ;; the record in its new home.
                   (when repaint
                     (frame-replay *application-frame* stream region))
                   (if feedback
                       (funcall feedback output-record stream
                                initial-x initial-y x y :draw)
                       (replay-output-record output-record stream nil x-offset y-offset)))))
              (:pointer-button-press (event x y)
               (unless finish-on-release
                 (finish (event-sheet event) x y)))
              (:pointer-button-release (event x y)
               (when finish-on-release
                 (finish (event-sheet event) x y))))))))))
