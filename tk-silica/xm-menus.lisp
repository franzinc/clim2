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

(in-package :xm-silica)

(defmethod port-menu-choose ((port motif-port) items &rest keys
			     &key printer 
				  presentation-type 
				  cache
				  associated-window
				  default-style label)
  (declare (values value chosen-item gesture))
  (unless (and (null printer)
	       (null presentation-type)
	       (not cache))
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
			(make-instance 'tk::xm-push-button
				       :parent menu
				       :managed nil
				       :label-string (string
						      (menu-item-display item))))
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
    (tk::manage-children (tk::widget-children menu))
    ;;
    (multiple-value-bind
	(ignore win1 win2 x y root-x root-y)
	(tk::query-pointer (tk::display-root-window
			    (xm-silica::port-display port)))
      (declare (ignore ignore win1 win2 x y))
      (tk::set-values menu :x root-x :y root-y)
      (tk::manage-child menu)
      ;; Now we have to wait
      (mp::process-wait "Returned value" #'(lambda () value-returned))
      ;; destroy the menu
      (tk::unmanage-child menu)
      (values-list return-value))))
    
    
    
