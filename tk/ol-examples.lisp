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
;; $Id: ol-examples.lisp,v 1.12 2002/07/09 20:57:18 layer Exp $

(in-package :tk)

(defun simple-ol-example ()
  (setq shell (make-instance 'base-window-shell :parent app))
  (setq control (make-instance 'control :parent shell
			       :layout-type :fixedcols
			       ))
  (setq button (make-instance 'menu-button :parent control
			      :label "foo"
			      :font
			      "-adobe-courier-medium-r-normal--12-120-75-75-m-70-iso8859-1"))

  (setq menu (tk::get-values button :menu-pane))

  (setq b1 (make-instance 'oblong-button :parent menu
			  :label "bar"
			  :font
			  "-adobe-courier-medium-r-normal--12-120-75-75-m-70-iso8859-1"))


  (setq b2 (make-instance 'oblong-button :parent menu
			  :label "baz"
			  :font
			  "-adobe-courier-medium-r-normal--12-120-75-75-m-70-iso8859-1"))

  (setq slider (make-instance 'slider
			      :orientation :horizontal
			      :height 100
			      :parent control))

  (setq scroll-bar (make-instance 'scroll-bar
				  :orientation :horizontal
				  :height 100
				  :parent control))

  (setq list-pane (make-instance 'list-pane
				 :parent control
				 ))

  (setq ab-menu (make-instance 'abbrev-menu-button
			       :parent control
			       ))

  (setq menu1 (tk::get-values ab-menu :menu-pane))

  (setq b3 (make-instance 'oblong-button :parent menu1
			  :label "Lisp"
			  :font
			  "-adobe-courier-medium-r-normal--12-120-75-75-m-70-iso8859-1"))


  (setq b4 (make-instance 'oblong-button :parent menu1
			  :label "Fortran"
			  :font
			  "-adobe-courier-medium-r-normal--12-120-75-75-m-70-iso8859-1"))

  (setq stext (make-instance 'static-text
			     :string "heooo"
			     :parent control))

  (set-values ab-menu :preview-widget stext)


  )





(defun simple-ol-example-2 ()
  (setq shell (make-instance 'base-window-shell :parent app))
  (setq da (make-instance 'draw-area :parent shell)))
