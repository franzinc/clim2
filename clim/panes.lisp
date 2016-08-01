;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
;;; Portions copyright (c) 1989, 1990 International Lisp Associates."

(defun compute-pane-constructor-code (panes)
  (check-type panes list)
  `(list ,@(mapcar #'(lambda (pane-spec)
                       (destructuring-bind (name code &rest options) pane-spec
                         (setq code (canonicalize-pane-spec name code options))
                         `(list ',name
                                #'(lambda (frame framem)
                                    (with-look-and-feel-realization (framem frame)
                                      ,code)))))
                   panes)))

(defun canonicalize-pane-spec (name code rest)
  (cond ((symbolp code)
         (unless (getf rest :name)
           (setf (getf rest :name) `',name))
         (apply #'find-pane-class-constructor code rest))
        ((null rest) code)
        (t
         (error "Invalid pane specification: ~S"
                (list* name code rest)))))

(defmethod find-pane-class-constructor ((type t) &rest options)
  (declare (dynamic-extent options))
  (error "Unknown pane type ~S with options ~S" type options))


;;--- Extend this to default the options from the lambda list
(defmacro define-pane-type (type lambda-list &body body)
  `(defmethod find-pane-class-constructor ((type (eql ',type)) ,@lambda-list)
     ,@body))

(define-pane-type :title (&rest options)
  #+allegro (declare (non-dynamic-extent options))
  `(make-clim-stream-pane
     :type 'title-pane
     ,@options
     :display-function 'display-title
     :display-after-commands nil
     :text-style (parse-text-style '(:sans-serif :bold :large))
     :scroll-bars nil
     :width :compute :height :compute
     :max-height :compute
     :end-of-page-action :allow
     :end-of-line-action :allow))

(define-pane-type :command-menu (&rest options)
  #+allegro (declare (non-dynamic-extent options))
  `(make-clim-stream-pane
     :type 'command-menu-pane
     ,@options
     :display-function `display-command-menu
     :incremental-redisplay t
     :display-after-commands t
     :text-style *command-table-menu-text-style*
     :scroll-bars nil
     :width :compute :height :compute
     :end-of-page-action :allow
     :end-of-line-action :allow))


(define-pane-type :interactor (&rest options &key (scroll-bars :vertical))
  #+allegro (declare (non-dynamic-extent options))
  `(make-clim-interactor-pane
     ,@options
     :scroll-bars ,scroll-bars))

(define-pane-type :application (&rest options &key (scroll-bars :both))
  #+allegro (declare (non-dynamic-extent options))
  `(make-clim-application-pane
     ,@options
     :scroll-bars ,scroll-bars))

(define-pane-type :accept-values (&rest options &key (scroll-bars :vertical))
  #+allegro (declare (non-dynamic-extent options))
  `(make-clim-stream-pane
     :type 'accept-values-pane
     ,@options
     :display-after-commands :no-clear
     :scroll-bars ,scroll-bars
     :width :compute :height :compute
     :end-of-page-action :allow
     :end-of-line-action :allow))

(define-pane-type :pointer-documentation (&rest options)
  #+allegro (declare (non-dynamic-extent options))
  `(make-clim-stream-pane
     :type 'pointer-documentation-pane
     ,@options
     :display-after-commands nil
     :text-style (parse-text-style '(:sans-serif :bold :normal))
     :scroll-bars nil
     :end-of-page-action :allow
     :end-of-line-action :allow))

(define-pane-type scroll-bar (&rest options)
  #+allegro (declare (non-dynamic-extent options))
  `(make-pane 'scroll-bar ,@options))

(define-pane-type slider (&rest options)
  #+allegro (declare (non-dynamic-extent options))
  `(make-pane 'slider ,@options))

(define-pane-type push-button (&rest options)
  #+allegro (declare (non-dynamic-extent options))
  `(make-pane 'push-button ,@options))

(define-pane-type label-pane (&rest options)
  #+allegro (declare (non-dynamic-extent options))
  `(make-pane 'label-pane ,@options))

(define-pane-type text-field (&rest options)
  #+allegro (declare (non-dynamic-extent options))
  `(make-pane 'text-field ,@options))

(define-pane-type text-editor (&rest options)
  #+allegro (declare (non-dynamic-extent options))
  `(make-pane 'text-editor ,@options))

(define-pane-type toggle-button (&rest options)
  #+allegro (declare (non-dynamic-extent options))
  `(make-pane 'toggle-button ,@options))

(define-pane-type radio-box (&rest options)
  #+allegro (declare (non-dynamic-extent options))
  `(make-pane 'radio-box ,@options))

(define-pane-type check-box (&rest options)
  #+allegro (declare (non-dynamic-extent options))
  `(make-pane 'check-box ,@options))

(define-pane-type list-pane (&rest options)
  #+allegro (declare (non-dynamic-extent options))
  `(make-pane 'list-pane ,@options))

(define-pane-type option-pane (&rest options)
  #+allegro (declare (non-dynamic-extent options))
  `(make-pane 'option-pane ,@options))

(define-pane-type :menu-bar (&rest options)
  #+allegro (declare (non-dynamic-extent options))
  `(make-pane 'menu-bar ,@options))
