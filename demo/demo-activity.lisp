;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: demo-activity.lisp,v 2.4 2003/12/15 18:35:13 layer Exp $

(in-package :clim-demo)

;;;"Copyright (c) 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1992 Franz, Inc.  All rights reserved."


(defclass demo-activity (activity) ())

(define-application-frame demo-app (activity-frame)
    ()
  (:panes
    (interactor :interactor)
    (display :application))
  (:pointer-documentation t)
  (:layouts
    (default 
      (vertically () (1/2 interactor) (:fill display)))))

(define-demo-app-command (com-activity-exit :menu t :name "Exit")
    ()
  (activity-frame-window-close *application-frame*))

(defvar *untitled-count* 1
  "Counter for naming new, untitled document windows")

;;; This method must be implemented by subclasses of activity
(defmethod start-initial-application-frame ((activity demo-activity))
  (start-application-frame activity
                           'demo-app 
                           :pretty-name "Initial"))

(define-demo-app-command (com-new :menu t :name t)
    ((name 'string 
	   :prompt "Name of Window"
	   :default (format nil "Untitled-~A" *untitled-count*)
	   :display-default t
	   :documentation "A Title String"))
  (start-application-frame (frame-activity *application-frame*)
                           'demo-app
                           :pretty-name name)
  (incf *untitled-count*))

(define-demo-app-command (com-select-frame :name t :menu t) 
    ((frame `((member ,@(frame-manager-frames *activity*)) 
	      ;; After a frame is deleted it ends not being a member of
	      ;; the presentation type so you get bogus presentations
	      ;; and the find in the present method returns NIL
	      :name-key ,#'(lambda (x) (and x (frame-pretty-name x))))))
  (select-activity-active-frame *activity* frame))

(define-demo-app-command (com-funky-add :name t :menu t) 
    ((n 'number))
  (flet ((print-random-sum (stream n)
           (let ((sum (+ n (random 20) -10)))
             (with-output-as-presentation (stream sum 'number)
               (format stream "~%~A plus ~A gives ~A" n (- sum n) sum)))))
    (print-random-sum (get-frame-pane *application-frame* 'display) n)))

;; This one gives a nicer pointer doc than the default one by  the
;; :gesture option of define-command
(define-presentation-to-command-translator add-number
    (number com-funky-add demo-app
     :pointer-documentation ((stream object)
			     (format stream "Add something to ~A" object)))
    (object)
  `(,object))



(define-demo "Activity Demo" demo-activity)
