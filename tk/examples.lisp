;; -*- mode: common-lisp; package: tk -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :tk)

;;; CLM bitmap code

(defun make-drawing-area ()
  (setq app4 (make-instance  'top-level-shell :parent app)
	sw2 (make-instance
	     'xm-scrolled-window :parent
	     app4
	     :width 500 :height 500
	     :scrolling-policy :application-defined
	     ;:visual-policy :constant
	     :scroll-bar-display-policy :static)
	da (make-instance
	    'xm-drawing-area :parent
	    sw2))
  (setq sb1 (make-instance 'xm-scroll-bar :parent sw2 :orientation :horizontal)
	sb2 (make-instance 'xm-scroll-bar :parent sw2 :orientation
			   :vertical))
  (set-values sw2 :horizontal-scroll-bar sb1 :vertical-scroll-bar sb2)

  (set-values sw2 :work-window da)
  (add-callback da :input-callback 'da-button-press-handler :input)
  (popup app4))

(defvar *bitmaps* (quote ("background" "25_foreground" "50_foreground" "75_foreground"
			  "horizontal" "vertical" "slant_right" "slant_left"
			  "1x1" "2x2" "black" "boxes" "cntr_ptr" "cntr_ptrmsk"
			  "cross_weave" "dimple1" "dimple3" "dot" "flagdown"
			  "flagup"
			  "flipped_gray" "flipped_gray" "gray" "gray1" "gray3"
			  "icon" "left_ptr" "left_ptrmsk" "light_gray"
			  "opendot" "right_ptr" "right_ptrmsk"
			  "root_weave" "scales" "sipb" "star" "stipple" "target"
			  "tie_fighter" "weird_size" "wide_weave" "wingdogs"
			  "woman"
			  "xfd_icon" "xlogo16")))


(defun make-some-bitmaps ()
  (setq app3 (make-instance 'top-level-shell
			    :name "foo"
			    :parent app)
	sw (make-instance
	    'xm-scrolled-window
	    :name "foo"
	    :parent app3
	    :width 500 :height 500
	    :visual-policy :constant
	    :scrolling-policy :automatic
	    :scroll-bar-display-policy :static)
	app-rc (make-instance
		'xm-row-column
		:name "foo"
		:parent sw
		:packing :pack-column
		:num-columns 5))
  (dolist (bitmap *bitmaps*)
    (setf rc (make-instance
	      'xm-row-column
	      :name "foo"
	      :parent app-rc))
    (make-instance
     'xm-label :parent
     rc
     :label-type :pixmap
     :alignment :center
     :label-pixmap bitmap)
    (make-instance
     'xm-label :parent
     rc
     :alignment :center
     :label-string bitmap))
  (popup app3))


(defun make-profiler-dialog ()


  (setq shell (make-instance 'xm-dialog-shell
			     :parent app
			     :title "Profiler Options"))


  (setq form (make-instance
	      'xm-bulletin-board
	      :name "foo"
	      :parent shell
	      :managed nil
	      :auto-unmanage nil
	      :dialog-style :full-application-modal))

  (setq controls (make-instance 'xm-row-column
				:name "foo"
				:parent form))

  (setq frame (make-instance 'xm-frame :parent controls))

  (setq collect-c (make-instance 'xm-row-column
				 :parent frame
				 :orientation :vertical))

  (setq collection-caption
    (make-instance 'xm-label
		   :parent collect-c
		   :alignment :beginning
		   :label-string "Data Collection"))

  (setq time-space (let ((rb
			  (make-instance 'xm-radio-box
					 :parent collect-c
					 :name "timespace"
					 :orientation :horizontal)))
		     (manage-child rb)
		     (mapc #'(lambda (x)
			       (make-instance
				'xm-toggle-button
				:parent rb
				:label-string x
				:set (oddp (random 100))))
			   (list "Time" "Space" "Both"))
		     rb))

  (setq collect-buttons
    (make-instance  'xm-row-column
		    :parent collect-c
		    :orientation :horizontal
		    :packing  :pack-column))

  (setq start (make-instance
	       'xm-push-button
	       :parent collect-buttons
	       :label-string "Start"))

  (setq stop (make-instance
	      'xm-push-button
	      :parent collect-buttons
	      :label-string "Stop"))


  (setq percent-frame
    (make-instance 'xm-frame :parent controls))

  (setq percent-row
    (make-instance 'xm-row-column
		   :parent percent-frame
		   :name "percentrowcolumn"
		   :orientation :vertical))

  (setq display-caption
    (make-instance 'xm-label
		   :parent  percent-row
		   :alignment :beginning
		   :label-string "Data Display"))


  (setq kid-threshhold
    (let* ((x (make-instance 'xm-row-column :parent percent-row))
	   (y (make-instance 'xm-label :parent x
			     :label-string "Child % Threshhold"))
	   (z (make-instance 'xm-text :parent x
			     :value "foooxxx")))
      (list x y z)))

  (setq per-cent-threshhold
    (let* ((x (make-instance 'xm-row-column :parent percent-row))
	   (y (make-instance 'xm-label :parent x
			     :label-string "Overall % Threshhold"))
	   (z (make-instance 'xm-text :parent x
			     :value "barxxx")))
      (list x y z)))

  (setq hrf (make-instance 'xm-toggle-button :parent
			   percent-row
			   :label-string
			   "Hide recursion"))

  (setq hide-chain (make-instance 'xm-toggle-button :parent
				  percent-row
				  :label-string "Hide = % Chains"))

  ;;

  (setq buttons (make-instance
		 'xm-row-column :parent
		 percent-row
		 :orientation :horizontal
		 :packing  :pack-column))

  (setq display-time (make-instance
		      'xm-push-button :parent
		      buttons
		      :label-string "Display time"))

  (setq display-space (make-instance
		       'xm-push-button :parent buttons
		       :label-string "Display space"))

  (setq sep (make-instance 'xm-separator :parent controls :orientation :horizontal))
  (setq buttons (make-instance 'xm-row-column :parent controls :orientation
			       :horizontal
			       :packing  :pack-column))
  (setq ok (make-instance 'xm-push-button :parent buttons :label-string "Ok"))
  (setq apply (make-instance 'xm-push-button :parent buttons :label-string "Apply"))
  (setq cancel (make-instance 'xm-push-button :parent buttons :label-string "Cancel"))
  (manage-child form))



(defun make-mouse-line ()

  (setq app2 (make-instance 'top-level-shell :name "foo" :parent app))
  (setq main-form (make-instance
		   'xm-row-column :parent app2 :orientation :vertical))
  (setq form (make-instance
	      'xm-row-column :parent main-form :orientation :horizontal))
  (setq ignore (make-instance 'xm-menu-bar :parent form))

  (setq ignore2 (create-top-level-menu ignore))

  (setq run (make-instance
	     'xm-label :parent
	     form
	     :label-string "Running"
	     :recompute-size nil))
  (setq sep1 (make-instance
	      'xm-separator :parent
	      form
	      :orientation :vertical
	      :separator-type :single-line))


  (setq sep3 (make-instance
	      'xm-separator :parent
	      main-form
	      :orientation :horizontal
	      :separator-type :single-line))


  (setq mouse-doc
	(make-instance 'xm-label :parent
			       main-form
			       :label-string
			       (make-string 50 :initial-element #\space)))
  (popup app2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun try-it ()
  (setq app (app-create-shell :display display :widget-class 'application-shell))
  (setq main (create-widget 'xm-main-window "foo" app))
  (setq label (create-widget 'xm-push-button "foo" main))
  (add-callback label :activate-callback 'push-button-activate)
  (manage-child label)
  (manage-child main)
  (realize-widget app))

(defun push-button-activate (&rest x) (format t "callback invoked with ~S~%" x))

;;;; The mouse line code


;;

#+composer
(defun find-menu-group (&rest args)
  (apply #'wt::find-menu-group args))

(defun create-top-level-menu (menu-bar)
  "Create the contents of the menu-bar"
  (create-menu-contents-from-menu-group menu-bar (find-menu-group :main-menu)))

(defun sub-menu-spec-p (spec)
  (eq (car spec) :menu))

(defun sub-menu-spec-name (menu-spec)
  ;; Return the name component of a menu item spec
  (cond ((atom (second menu-spec))
	 (second menu-spec))
	((getf (second menu-spec) :name))
	(t (error "Menu spec has no name component: ~S" menu-spec))))


(defun sub-menu-spec-tester (menu-spec)
  (if (consp (second menu-spec))
      (getf (second menu-spec) :tester)))


(defun menu-item-spec-name (menu-spec)
  ;; Return the name component of a menu item spec
  (cond ((atom (first menu-spec))
	 (first menu-spec))
	((getf (first menu-spec) :name))
	(t (error "Menu spec has no name component: ~S" menu-spec))))


(defun menu-item-spec-tester (menu-spec)
  (if (consp (first menu-spec))
      (getf (first menu-spec) :tester)))

(defun sub-menu-item-spec-name (menu-spec)
  ;; Return the name component of a menu item spec
  (cond ((atom menu-spec)
	 (second menu-spec))
	((getf menu-spec :name))
	(t (error "Menu spec has no name component: ~S" menu-spec))))


#+composer
(defun create-menu-contents-from-menu-group (menu mg &optional values)
  (let (testers)
    (dolist (spec (wt::menu-group-components mg) testers)
      (let ((maybe-tester (if (sub-menu-spec-p spec)
			      (create-submenu menu spec values)
			    (create-menu-item menu spec values))))
	(when  maybe-tester (push maybe-tester testers))))))

(defun create-menu-item (menu spec values)
  (let ((button (make-instance
		 'xm-push-button :parent
		 menu
		 :label-string (menu-item-spec-name spec)))
	(cb (cdr spec)))
    (when cb
      (add-callback button
		    :activate-callback
		    #'(lambda (&rest args)
			(ignore-errors
			 (apply (car cb) args)))
		    (append values (cdr cb))))
    (look-for-tester button spec #'menu-item-spec-tester)))

(defun create-submenu (menu1 spec &optional values)
  (let* ((menu (make-instance 'xm-pulldown-menu
			      :parent menu1
			      :managed nil
			      :name (sub-menu-spec-name spec)))
	 (button (make-instance
		  'xm-cascade-button :parent
		  menu1
		  :sub-menu-id menu
		  :label-string (sub-menu-spec-name spec))))
    (let ((testers
	   (create-menu-contents-from-menu-group
	    menu
	    (find-menu-group (third spec)) values)))
      (when testers
	(add-callback menu
		      :map-callback
		      #'(lambda (&rest ignore)
			  (declare (ignore ignore))
			  (handler-case
			      (mapc #'funcall testers)
			    (error (c)
			      (format t "~A~%" c)))
			  nil)
		      nil)))
    (look-for-tester button spec #'sub-menu-spec-tester)))

(defun look-for-tester (button spec accessor)
  (let ((tester (funcall accessor spec)))
    (and tester
	 #'(lambda ()
	     (set-values button
			 :sensitive
			 (not (not (if (atom tester)
				       (funcall tester)
				     (apply (car tester)
					    (cdr tester))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; The rest of this is obsolete

(defun make-error ()
  (setq app4 (create-popup-shell "foo" 'top-level-shell app))
  (popup app4))

#+xlib
(defun da-button-press-handler (widget window event ignore)
  (let ((e event))
    (format t "type =d ~d x = ~d, y = ~d, state = ~D, keycode = ~D~%"
	    (svref xlib::*event-key-vector* (x-event-type e))
	    (x-event-x e)
	    (x-event-y e)
	    (x-event-state e)
	    (x-event-keycode e))
    (print (multiple-value-list
	    (lookup-string event)))))


#+xlib
xlib:(defun try-drawing (display window string)
  (let* ((screen (x11:xdefaultscreenofdisplay display))
	 (black (screen-black-pixel screen))
	 (white (screen-white-pixel screen))
	 (font (open-font display "fixed"))
	 (border 1)			; Minimum margin around the text
	 (width (+ (text-width font string) (* 2 border)))
	 (height (+ (max-char-ascent font) (max-char-descent font) (* 2 border)))
	 (x (truncate (- (screen-width screen) width) 2))
	 (y (truncate (- (screen-height screen) height) 2))
	 (gcontext (create-gcontext :drawable window
				    :background black
				    :foreground white
				    :font font)))
    (draw-glyphs window gcontext 10 10 string)))







