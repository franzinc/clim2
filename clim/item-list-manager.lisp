;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1989, 1990 International Lisp Associates.  All rights reserved."

(defclass item-list-manager ()
     ((item-list :initarg :item-list :accessor ilm-item-list)
      (display-record :initform nil))
  (:default-initargs :item-list nil))

(defmethod clear-items ((ilm item-list-manager))
  (setf (slot-value ilm 'item-list) nil))

(defmethod add-item ((ilm item-list-manager) new-item
                     &key before-item after-item before-index after-index)
  ;; Default is before 1st item
  (unless (or before-item after-item before-index after-index)
    (setq before-index 0))
  ;;--- make sure that only one argument is supplied
  (with-slots (item-list) ilm
    (when before-item
      (setq before-index (position before-item item-list))
      (when (null before-index)
        (error "Could not find :before-item ~S in item-list" before-item)))
    (when after-item
      (setq after-index (position after-item item-list))
      (when (null after-index)
        (error "Could not find :after-item ~S in item-list" after-item)))
    (cond (before-index
           ;; Check bounds, but allow for empty list (add first item either :before- or :after-index 0
           #+++ignore
           (assert (< -1 before-index (max 1 (length item-list))) ()
                   "The :before-index was ~D which is outside the bounds of the item-list"
                   before-index)
           (if (= before-index 0)
               (push new-item item-list)
               (push new-item (cdr (nthcdr (1- before-index) item-list)))))
          (after-index
           ;; Check bounds, but allow for empty list (add first item either :before- or :after-index 0
           #+++ignore
           (assert (< -1 after-index (max 1 (length item-list))) ()
                   "The :after-index was ~D which is outside the bounds of the item-list"
                   after-index)
           (push new-item (cdr (nthcdr after-index item-list))))
          (t (error "Could not add item ~S to the item list." new-item)))))

(defmethod delete-item ((ilm item-list-manager) item)
  (with-slots (item-list) ilm
    (let ((pos (position item item-list)))
      (if pos
          (setf item-list (delete item item-list))
          (error "Could not find item ~S in item list" item)))))

(defmethod append-item ((ilm item-list-manager) new-item)
  (with-slots (item-list) ilm
    (add-item ilm new-item :before-index (length item-list))))

(defmethod delete-item-at-index ((ilm item-list-manager) index)
  (with-slots (item-list) ilm
    (assert (< -1 index (length item-list)) () 
                   "The index was ~D which is outside the bounds of the item-list"
                   index)
    (setf item-list (append (subseq item-list 0 index)
                            (nthcdr (1+ index) item-list)))))

(defmethod map-over-items ((ilm item-list-manager) function)
  (dolist (item (slot-value ilm 'item-list))
    (funcall function item)))

;;; This is sort of kludgy.  We really need to keep a per-stream cache of display-records.
(defmethod display-items ((ilm item-list-manager) stream
                          &key incrementally (printer #'write-string))
  (if incrementally
      (display-items-incrementally ilm stream :printer printer)
      (with-slots (display-record item-list) ilm
        (dolist (item item-list)
          (funcall printer item stream)
          (terpri stream)))))

(defmethod display-items-incrementally ((ilm item-list-manager) stream
                                        &key (printer #'write-string))
  (with-slots (display-record item-list) ilm
    (setq display-record (updating-output (stream)
                           (dolist (item item-list)
                             (updating-output (stream :unique-id item :cache-value t)
                               (funcall printer item stream)
                               (terpri stream)))))))

(defmethod redisplay-items ((ilm item-list-manager) stream)
  (with-slots (display-record) ilm
    (unless display-record
      (error "You must use display-items with the :incrementally option to use redisplay"))
    (redisplay display-record stream)))
