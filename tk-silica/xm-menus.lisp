;; -*- mode: common-lisp; package: xm-silica -*-
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
;; $fiHeader: xm-menus.lisp,v 1.5 92/03/04 16:20:38 cer Exp Locker: cer $


(in-package :xm-silica)

(defmethod port-menu-choose ((port motif-port) items &rest keys
			     &key printer 
				  presentation-type 
				  cache
				  associated-window
				  default-style label)
  (declare (values value chosen-item gesture))
  (let ((simplep (and (null printer)
		      (null presentation-type))))
    (unless (and (not cache))
      (return-from port-menu-choose
	(call-next-method))) 
    ;; We can use labels for the menu items, so use a Motif menu
    (let ((menu (make-instance 'tk::xm-popup-menu 
			       :parent (or (and associated-window
						(sheet-mirror associated-window))
					   (port-application-shell
					    port))
			       :managed nil))
	  (return-value nil)
	  (value-returned nil))
      (when label
	(make-instance 'tk::xm-label
		       :parent menu
		       :managed nil
		       :label-string label)
	(make-instance 'tk::xm-separator
		       :managed nil
		       :parent menu))
      (map nil #'(lambda (item)
		   (let ((menu-button
			  (if simplep
			      (make-instance 'tk::xm-push-button
					 :parent menu
					 :managed nil
					 :label-string (string
							(menu-item-display item)))
			    (make-instance 'tk::xm-push-button
					 :parent menu
					 :managed nil
					 :label-type :pixmap
					 :label-pixmap
					 (pixmap-from-menu-item
					  associated-window 
					  item
					  printer
					  presentation-type))))
			 (value (menu-item-value item)))
		     (tk::add-callback
		      menu-button
		      :activate-callback
		      #'(lambda (&rest args)
			  (declare (ignore args))
			  (setq return-value (list value item)
				value-returned t)))))
	   items)
      ;; 
      (tk::add-callback (widget-parent menu)
			:popdown-callback
			#'(lambda (&rest ignore) 
			    (setq value-returned t)))
      ;;
      (tk::manage-children (tk::widget-children menu))
      ;;
      (multiple-value-bind
	  (ignore win1 win2 x y root-x root-y)
	  (tk::query-pointer (tk::display-root-window
			       (port-display port)))
	(declare (ignore ignore win1 win2 x y))
	(tk::set-values menu :x root-x :y root-y)
	(tk::manage-child menu)
	;; Now we have to wait
	(port-force-output port)
	(mp::process-wait "Returned value" #'(lambda () 
					       ;;-- This is to deal
					       ;;-- with the race
					       ;;-- condition where
					       ;;-- the menu go down
					       ;;-- to quick
					       (or (not (tk::is-managed-p menu))
						   value-returned)))
	;; destroy the menu
	(tk::unmanage-child menu)
	(values-list return-value)))))

(defun pixmap-from-menu-item (associated-window menu-item printer presentation-type)
  (with-menu (menu associated-window)
    (setf (stream-text-margin menu) 1000)
    (let ((rec (with-output-recording-options (menu :draw nil :record t)
		 (with-output-to-output-record (menu)
		   (if presentation-type
		       (present menu-item presentation-type :stream menu)
		   (funcall printer menu-item menu))))))
      (multiple-value-bind
	  (width height)
	  (bounding-rectangle-size rec)
	(with-output-to-pixmap (s associated-window :width width :height height)
	  (multiple-value-call #'draw-rectangle* 
	    s 0 0 (bounding-rectangle-size s) :ink +background-ink+)
	  (replay-output-record 
	    rec s +everywhere+
	    (- (bounding-rectangle-min-x rec))
	    (- (bounding-rectangle-min-y rec))))))))
  
