;; See the file LICENSE for the full license governing this code.
;;

(in-package :xm-silica)

(defvar *use-clim-gc-cursor* nil)
(defvar *gc-before* nil)
(defvar *gc-after*  nil)

(defun init-gc-cursor (frame &optional force)
  
  (let ((pointer-type (first
                       (list #+(and 64bit (not alpha)) '(unsigned-byte 64)
                             ;; otherwise, use 32 bits:
                             '(unsigned-byte 32)))))
    (when *use-clim-gc-cursor*
      (when (or force (null *gc-before*))
        (let ((vec (make-array 2 :element-type pointer-type)))
          (tk::init_clim_gc_cursor_stuff vec)
          (setq *gc-before* (aref vec 0)
                *gc-after*  (aref vec 1))
          (pushnew (make-array 1 :element-type pointer-type
                               :initial-element *gc-before*)
                   (excl:gc-before-c-hooks))
	
          (pushnew (make-array 1 :element-type pointer-type
                               :initial-element *gc-after*)
                   (excl:gc-after-c-hooks))))
      (let* ((sheet (frame-top-level-sheet frame))
             (mirror (and sheet (sheet-direct-mirror sheet))))
        (if mirror
            (tk::set_clim_gc_cursor_widget
             mirror
             (realize-cursor (port sheet)
                             sheet
                             (sheet-pointer-cursor sheet)))
            (tk::set_clim_gc_cursor_widget 0 0))))))

(defun reinitialize-gc-cursor ()
  ;; Force init-gc-cursor to go through its initialization
  ;; code the next time it is called.
  (setq *gc-before* nil)
  (setq *gc-after* nil))

(pushnew 'reinitialize-gc-cursor excl::*restart-actions*)

(defmethod (setf sheet-pointer-cursor) :after (cursor (sheet xt-top-level-sheet))
  (declare (ignore cursor))
  (init-gc-cursor (pane-frame sheet)))

(defmethod clim-internals::receive-gesture :after ((stream xt-top-level-sheet)
						   (gesture pointer-enter-event))
  ;; If the top level window has a cursor we need to pass that in somehow
  ;; so that it gets restored appropriately.
  (unless (eq (pointer-boundary-event-kind gesture) :inferior)
    (init-gc-cursor (pane-frame stream))))


