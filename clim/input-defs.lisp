;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM-INTERNALS; Base: 10 -*-
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


(in-package :clim-internals)

"Copyright (c) 1988, 1989, 1990 International Lisp Associates.  All rights reserved."

;;; I/O Buffers

;;;--- The io-buffer class should take the :SIZE init keywords and the
;;; rest of this this should be folded into the initialize-instance method.
(defun make-io-buffer (&optional (size 100))
  (let ((iob (make-instance 'io-buffer)))
    (setf (io-buffer-size iob) size)
    (setf (io-buffer-buffer iob) (make-array size))
    (setf (io-buffer-input-pointer iob) 0)
    (setf (io-buffer-output-pointer iob) 0)
    iob))

(defun-inline pointer-increment (N dN limit)
  (mod (+ N dN) limit))

(defmethod io-buffer-empty-p ((iob io-buffer))
  (with-slots (input-pointer output-pointer) iob
    (= input-pointer output-pointer)))



(defmethod io-buffer-full-p ((iob io-buffer))
  (with-slots (input-pointer output-pointer size) iob
    ;; Always leave room for at least one unget to be done
    (= (pointer-increment input-pointer 2 size)
       output-pointer)))

(defmethod io-buffer-put ((iob io-buffer) element)
  (when (io-buffer-full-p iob)
    (error "IO buffer ~A is full, can't add ~A" iob element))
  (with-slots (buffer input-pointer size) iob
    (setf (aref buffer input-pointer) element)
    (setf input-pointer (pointer-increment input-pointer 1 size)))
  element)



(defmethod io-buffer-get ((iob io-buffer))
  ;; No hang-p option, that is the responsibility of the caller.
  (when (io-buffer-empty-p iob)
    (return-from io-buffer-get nil))
  (with-slots (buffer output-pointer size) iob
    (prog1 (aref buffer output-pointer)
	   (setf output-pointer (pointer-increment output-pointer 1 size)))))



(defmethod io-buffer-unget ((iob io-buffer) elt)
  (with-slots (buffer output-pointer size) iob 
    (let ((new-output-pointer (pointer-increment output-pointer -1 size)))
      (unless (eql elt (aref buffer new-output-pointer))
	(error "Attempt to unget ~A, which was not the last thing read from the I/O buffer"
	       elt))
      (setf output-pointer new-output-pointer)))
  (values))
    


(defmethod io-buffer-clear-input ((iob io-buffer))
  (with-slots (input-pointer output-pointer) iob
    (setf output-pointer 0
	  input-pointer 0)))




;;; Pointers and pointer actions

(defclass pointer
	  ()
     ((root :accessor pointer-root :initform nil :initarg :root)
      (window :accessor pointer-window :initform nil)
      ;; Position in root coordinates 
      (x-position :accessor pointer-x-position :initform 0)
      (y-position :accessor pointer-y-position :initform 0)
      (native-x-position :accessor pointer-native-x-position :initform 0)
      (native-y-position :accessor pointer-native-y-position :initform 0)
      (button-state :accessor pointer-button-state :initform 0)
      (position-changed :accessor pointer-position-changed)
      (cursor-pattern :accessor pointer-cursor-pattern)
      (cursor-width :accessor pointer-cursor-width)
      (cursor-height :accessor pointer-cursor-height)
      (cursor-x-offset :accessor pointer-cursor-x-offset)
      (cursor-y-offset :accessor pointer-cursor-y-offset)))



;;; Should have just made pointers obey region protocol

(defun pointer-position (pointer)
  (values (pointer-x-position pointer)
	  (pointer-y-position pointer)))


(defun pointer-set-position (pointer new-x new-y)
  (setf (pointer-x-position pointer) new-x)
  (setf (pointer-y-position pointer) new-y))


(defun pointer-set-native-position (pointer new-x new-y)
  (setf (pointer-native-x-position pointer) new-x)
  (setf (pointer-native-y-position pointer) new-y))

(defmethod pointer-decache ((pointer pointer))
  (with-slots (window) pointer
    ;; Beware of the cached stream pane becoming ungrafted.
    (unless (and window (port window))
      (setf window nil))
    (when window
      (let ((native-x-position (pointer-native-x-position pointer))
	    (native-y-position (pointer-native-y-position pointer)))
	(multiple-value-setq (native-x-position native-y-position)
	  (untransform-point* (fetch-native-transformation window) 
			      native-x-position 
			      native-y-position))
	(setf (pointer-x-position pointer) native-x-position
	      (pointer-y-position pointer) native-y-position)))))

(defmethod (setf pointer-window) :before (new-value (pointer pointer))
   (with-slots (window) pointer
     (unless (eql new-value window)
       (when window
	 ;; --- horrible cross-protocol modularity violation here
	 ;; but it's hours before AAAI
	 (when (output-recording-stream-p window)
	   (set-highlighted-presentation window nil))))))

(defmethod query-pointer ((pointer pointer))
  #+Genera
  (declare (values window x y))
  (with-slots (window x-position y-position) pointer
    (values window x-position y-position)))

(defun pointer-state-changed (pointer old-window old-x old-y)
  (multiple-value-bind (window x-position y-position) (query-pointer pointer)
    (values
      (or (not (eql window old-window))
	  (not (eql old-x x-position))
	  (not (eql old-y y-position)))
      window x-position y-position)))

