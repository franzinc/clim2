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

;;;--- better term than console?
;;; --- not used yet
(defclass console
	  ()
     ((key-table :accessor console-key-table)
      (pointer-list :initform nil :accessor console-pointer-list)))

;;; Table to associate host windows with their CLIM window "owners".
(defvar *host-window-to-clim-window-mapping* (make-hash-table))

(defun associate-clim-window-with-host-window (host-window clim-window)
  (setf (gethash host-window *host-window-to-clim-window-mapping*)
	clim-window))

(defun clim-window-for-host-window (host-window &key (error-if-no-match t))
  (let ((clim-window (gethash host-window *host-window-to-clim-window-mapping*)))
    (if (null clim-window)
	(if error-if-no-match
	    (error "Could not find CLIM window associated with ~S" host-window)
	    nil)
	clim-window)))

;;; Anything to be gained by the CLX drawable/window distinction?

(defclass window-stream
	  ;; The ordering of these two groups of classes matters if the
	  ;; method combinations are going to come out right.  However
	  ;; the ordering of the classes withing the two levels should not
	  ;; matter as each class defines its own stand-alone protocol.
	  (graphics-output-recording
	   
	   ;; sheet-output-recording
	   ;; basic-output-recording	;better name?
	   
	   input-protocol-mixin 
	   output-protocol-mixin
	   )
     ()
  )

(defmethod window-stream-class-name ((window-stream window-stream))
  (class-name (class-of window-stream)))

;;; --- maybe move this somewhere?

(defun erase-viewport (stream)
  ;; Just repaint the viewport, which has been set up with a
  ;; background color.
  (let ((viewport (pane-viewport stream)))
    ;; --- what if no viewport?  Just revert to draw-rectangle?  No,
    ;; there has to be a viewport to insulate the ancestors from the
    ;; size changes of the stream pane's output history.
    (when viewport
      (repaint-sheet viewport (sheet-region viewport))))
  ;; It is bogus to just draw a rectangle in some random color.  Where
  ;; did we expect the medium's background to get set?
  #+ignore
  (with-output-recording-options (stream :record-p nil)
    (with-bounding-rectangle* (minx miny maxx maxy) stream
      (draw-rectangle* stream minx miny maxx maxy
		       :filled t :ink +background+))))

;;; Temporary replacement for "window-clear".

(defmethod window-clear ((stream window-stream))
  (with-sheet-medium (medium stream)
    (letf-globally (((medium-transformation medium) +identity-transformation+))
		   (clear-output-history stream)
		   (repaint-sheet stream +everywhere+)
		   ;;(erase-viewport stream)
		   (stream-set-cursor-position* stream 0 0)
		   ;; Flush the old mouse position relative to this window
		   ;; so that we don't get bogus highlighted presentations
		   ;; when menus first pop up.
		   (let ((pointer (stream-primary-pointer stream)))
		     (when pointer
		       (setf (pointer-window pointer) nil)))
      ;;; doesn't really need to do force-output.
		   (force-output stream)
		   (values))))


(defmethod window-shift-mask ((window window-stream))
  (let ((pointer (stream-primary-pointer window)))
    (pointer-button-state pointer)))

;;; Creation functions





#||
;;;--- Move this into the various port-defining files, revamp define-implementation

(defvar *port-types*
	`(,@(when (member :xlib *features*)
	      `((:clx ,(intern "X-PORT" "ON-X"))))
	  ,@(when (find-package "ON-GENERA")
	      `((:genera ,(intern "GENERA-PORT" "ON-GENERA"))))))

;;; These aren't really tested since they aren't the primary way to create sheets.

#+Silica
(defun open-root-window (port-type &rest creation-args)
  (assert (member port-type *port-types* :key #'car) (port-type)
	  "The implementation type supplied, ~S, is not one of~{ ~S~}"
	  port-type *port-types*)
  (let ((port-type (second (assoc port-type *port-types*))))
    ;;; This will break but I don't think it is used -- RR
    (let ((port (apply #'find-port :port-type port-type creation-args)))
      (values (find-graft :port port :origin :nw) port))))

#+Silica
(defun open-window-stream (&key parent left top right bottom width height
			   &allow-other-keys)
  ;; --- incorporate size-hacking stuff from old definition below
  (assert (not (null parent)) (parent)
	  "You must supply the ~S option to ~S" ':parent 'open-window-stream)
  (when width
    (when right
      (error "Can't supply both :RIGHT and :WIDTH."))
    (setq right (+ left width)))
  (when height
    (when bottom
      (error "Can't supply both :BOTTOM and :HEIGHT."))
    (setq bottom (+ top height)))
  (make-instance 'window-stream :parent parent :min-x left :min-y top :max-x right :max-y bottom))
||#



;;; Text output methods.

;;; Must everything come in both string and char versions?
;;; Or should we put the char in a length 1 string buffer?

;(defmethod write-char-method ((stream window-stream) char)
;  (with-slots (cursor-x cursor-y) stream
;    ;;--- kludge Return specially for now
;    (case char
;      (#\newline
;       (stream-advance-cursor-line stream))
;      (otherwise
;	(let ((width (window-stream-character-width stream char))
;	      (right (window-stream-inside-width stream)))
;	  (when (> (+ cursor-x width 2) right)
;	    (window-stream-end-of-line-wrap stream right cursor-y))
;	  (draw-character char cursor-x cursor-y :stream stream
;			  ;; we want (set-cursorpos 0 0) (write-string "foo") to be visible
;			  :attachment-y :top))
;	(window-stream-advance-cursor-x
;	  stream (window-stream-character-width stream char))))))
;
;(defmethod window-stream-end-of-line-wrap ((stream window-stream) right top)
;  (break)
;  (with-output-recording-options (stream :draw-p t :record-p nil)
;    (draw-rectangle-method stream (- right 3) top right (+ top 10)))
;  (window-stream-advance-line stream))
;    
;;;; Try to be more efficient than a loop of WRITE-CHARs
;(defmethod write-string-method ((stream window-stream) string &key start end)
;  (with-slots (cursor-x cursor-y) stream
;    (multiple-value-bind (right) (window-stream-inside-width stream)
;      (labels ((write-substring (string start end)
;		 ;; no Returns
;		 (let ((width (window-stream-string-width stream string :start start :end end)))
;		   (cond ((or ;; end-of-line-action not wrap
;			    (<= (+ cursor-x width) right))
;			  (draw-string string cursor-x cursor-y :stream stream :start start :end end
;				       ;; we want (set-cursorpos 0 0) (write-string "foo") to be visible
;				       :attachment-y :top)
;			  ;; we know that this won't advance the cursor past the "right" edge
;			  (setf cursor-x (+ cursor-x width)))
;			 (t (slow-write-substring string start end)))))
;	       (slow-write-substring (string start end)
;		 (do ((i start (1+ i)))
;		     ((>= i end) nil)
;		   (let ((ch (aref string i)))
;		     (write-char-method stream ch)))))
;	;; Need do-delimited-substrings
;	(when (null end)
;	  (setq end (length string)))
;	(let ((start-pos start)
;	      (finished nil))
;	  ;; Loop over #\Return-separated substrings
;	  (loop
;	    (let ((end-pos (position #\newline string :start start-pos)))
;	      (cond ((null end-pos)
;		     (setq end-pos end)
;		     (setq finished t))
;		    ((> end-pos end)
;		     (setq end-pos end)
;		     (setq finished t)))
;	      (write-substring string start-pos end-pos)
;	      (when finished
;		(return))
;	      ;; advance cursor and write next chunk
;	      (setq start-pos (1+ end-pos))
;	      (window-stream-advance-line stream))))))))

