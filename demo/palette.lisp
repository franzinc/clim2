;; -*- mode: common-lisp; package: clim-demo -*-
;;
;;				-[]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1992 Franz Inc, Berkeley, CA  All rights reserved.
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
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
;; $fiHeader$


(in-package :clim-demo)

(define-application-frame gadget-demo ()
			  ()
  (:panes
   (slider1 slider :min-value 0 :max-value 10 :orientation :horizontal)
   (slider2 slider :min-value 0 :max-value 10 :orientation :vertical :height 100)
   (push-button1 push-button :label "Press Me")
   (toggle-button1 toggle-button :label "Toggle Me")
   (radio-box radio-box :choices '("Lisp" "Haskell" "Smalltalk"))
   (check-box check-box :choices '("Top-down" "Bottom-up"))
   (text-field text-field :height 20 :value "This is a text field")
   (text-editor (scrolling () 
			   (make-pane 'text-editor 
				      :value "This is a scrolling
text field gadget"
				      :ncolumns 20 :nlines 4)))
   (list-pane list-pane :value "Franz" :items '("Franz" "L" "S") :test 'string=)
   (option-pane option-pane
		:items '("eenie" "meanie" "minie")
		:value "minie"
		:value-changed-callback 'option-pane-changed-callback
		:test 'string=
		:label "moo"))
  (:layouts
   (:default
       (vertically ()
	   slider1 
	 (horizontally ()
	     slider2 
	   (vertically () push-button1 toggle-button1 radio-box check-box))
	 text-field text-editor list-pane option-pane))))

(defvar *gadgets* nil)

(defun do-gadget (&key reinit (root (find-frame-manager)))
  (let* ((entry (assoc root *gadgets*))
	 (p (cdr entry)))
    (when (or (null p) reinit)
      (setq p (make-application-frame 'gadget-demo :parent root))
      (if entry
	  (setf (cdr entry) p)
	  (push (cons root p) *gadgets*)))
    (run-frame-top-level p)))

(define-demo "Gadget demo" (do-gadget))

