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
;; $fiHeader: xm-menus.lisp,v 1.10 92/04/21 16:13:29 cer Exp Locker: cer $


(in-package :xm-silica)


;;-- This is generic and should be specialized on xt-port

(defmethod port-menu-choose ((port motif-port) items &rest keys
			     &key printer 
				  presentation-type 
				  associated-window
				  default-style label
				  cache
				  (unique-id items)
				  (id-test 'equal)
				  (cache-value items)
				  (cache-test #'equal))
  (declare (ignore keys))      
  (declare (values value chosen-item gesture))
  (let (menu closure)
    (when cache
      (let ((x (assoc unique-id
		      (port-menu-cache port)
		      :test id-test)))
	(when x
	  (destructuring-bind
	      (menu-cache-value amenu aclosure) x
	    (if (funcall cache-test cache-value menu-cache-value)
		(setq menu amenu closure aclosure)
	      (progn
		(setf (port-menu-cache port)
		  (delete x (port-menu-cache port)))
		(tk::destroy-widget (tk::widget-parent amenu))))))))
      
    (unless menu
      (multiple-value-setq
	  (menu closure)
	(port-construct-menu port 
			     items 
			     printer 
			     presentation-type 
			     associated-window
			     default-style
			     label))
      (when cache
	(push (list unique-id menu closure) 
	      (port-menu-cache port))))
	  
    ;; initialize the closure
    (funcall closure t)
    ;;
    (multiple-value-bind
	(ignore win1 win2 x y root-x root-y)
	(tk::query-pointer (tk::display-root-window
			    (port-display port)))
      (declare (ignore ignore win1 win2 x y))
      (tk::set-values menu :x root-x :y root-y)
      (loop
	(when (funcall closure) (return nil))
	(tk::manage-child menu)
	;; Now we have to wait
	(port-force-output port)
	(wait-for-callback-invocation
	 port
	 #'(lambda () 
	     ;;-- This is to deal
	     ;;-- with the race
	     ;;-- condition where
	     ;;-- the menu go down
	     ;;-- to quick
	     (or (funcall closure)
		 (not (tk::is-managed-p menu)))) 
	 "Returned value"))
      (unless cache
	(tk::destroy-widget (tk::widget-parent menu)))
      
      (values-list (nth-value 1 (funcall closure))))))

(defmethod port-construct-menu ((port motif-port) 
				items 
				printer 
				presentation-type 
				associated-window
				default-style
				label)
  (declare (ignore default-style))
  (let (value-returned 
	return-value
	(simplep (and (null printer)
		      (null presentation-type)))
	(menu (make-instance 'tk::xm-popup-menu 
			     :parent (or (and associated-window
					      (sheet-mirror associated-window))
					 (port-application-shell
					  port))
			     :managed nil)))
    (when label
      (make-instance 'tk::xm-label
		     :parent menu
		     :managed nil
		     :label-string label)
      (make-instance 'tk::xm-separator
		     :managed nil
		     :separator-type :double-line
		     :parent menu))
    (labels ((make-menu-button (item class parent &rest options)
	       (let ((button
		      (if simplep
			  (apply #'make-instance
				 class 
				 :sensitive (clim-internals::menu-item-active item)
				 :parent parent 
				 :managed nil
				 :label-string (string (menu-item-display item))
				 options) 
			(let* ((pixmap (pixmap-from-menu-item
					associated-window 
					item
					printer
					presentation-type))
			       (button
				(apply #'make-instance
				       class 
				       :sensitive (clim-internals::menu-item-active item)
				       :parent parent 
				       :label-type :pixmap
				       :label-pixmap pixmap
				       options)))
			  (xt::add-widget-cleanup-function
			   button
			   #'tk::destroy-pixmap pixmap)
			  button))))
		 (when (clim-internals::menu-item-documentation item)
		   (tk::add-callback button 
				 :help-callback 
				 'display-motif-help
				 port
				 (clim-internals::menu-item-documentation item)))
		 button))
	     (construct-menu-from-items (menu items)
	       (map nil #'(lambda (item)
			    (ecase (clim-internals::menu-item-type item)
			      (:separator
			       (make-instance 'tk::xm-separator
					      :managed nil
					      :parent menu))
			      (:label
				  (make-instance 'tk::xm-label
						 :parent menu
						 :managed nil
						 :label-string (string
								(menu-item-display item))))
			      (:item
			       (if (clim-internals::menu-item-item-list item)
				   (let* ((submenu (make-instance
						    'tk::xm-pulldown-menu
						    :managed nil
						    :parent menu))
					  (menu-button
					   (make-menu-button item 
							     'tk::xm-cascade-button
							     menu
							     :sub-menu-id submenu)))
				     (declare (ignore menu-button))
				     (construct-menu-from-items 
				      submenu 
				      (clim-internals::menu-item-item-list item)))
				 (let ((menu-button
					(make-menu-button item 
							  'tk::xm-push-button
							  menu))
				       (value (menu-item-value item)))
				   (tk::add-callback
				    menu-button
				    :activate-callback
				    #'(lambda (&rest args)
					(declare (ignore args))
					(setq return-value (list value item)
					      value-returned t))))))))
		    items)
	       ;;
	       (tk::manage-children (tk::widget-children menu))))
	
      (construct-menu-from-items menu items))
    (tk::add-callback (widget-parent menu)
		      :popdown-callback
		      #'(lambda (&rest ignore) 
			  (declare (ignore ignore))
			  (setq value-returned t)))
    (values menu
	    #'(lambda (&optional init)
		(if init
		    (setf value-returned nil return-value nil)
		  (values value-returned return-value))))))
		

(defun pixmap-from-menu-item (associated-window menu-item printer presentation-type)
  (with-menu (menu associated-window)
    (setf (stream-text-margin menu) 1000)
    (let ((rec (with-output-recording-options (menu :draw nil :record t)
		 (with-output-to-output-record (menu)
		   (handler-case
		       (if presentation-type
			   (present menu-item presentation-type :stream menu)
			 (funcall printer menu-item menu))
		     (error (c)
		       (write-string "Error in printer" menu)))))))
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
  
