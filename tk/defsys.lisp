
(defsys::defsystem :xm-tk
  (:default-pathname (frob-pathname "tk"))

  (|pkg|)
  (|foreign-obj|)
  (|macros|)
  
  ;;; Do the foreign loads
  
  (|load-xm|)
  
  ;; Clos<->xlib interface
  
  (|xlib|)
  (|font|)
  (|gcontext|)
  (|graphics|)
  
  
  ;; Toolkit stuff
  
  (|xtk|)
  (|meta-tk|)
  (|make-classes|)
  
  (|foreign|)
  (|widget|)
  (|resources|)
  (|event|)
  (|callbacks|)
  (|convenience|)

  (|xm-init|)
  (|xm-widgets|)
  
  (|xm-protocols|)
  
  (|examples|)
  )

#+this-is-totally-out-of-date
(defsys::defsystem :ol-tk
    (:default-pathname (frob-pathname "tk"))
  (|pkg|)
  (|foreign-obj|)
  (|load-ol|)
  (|xlib|)
  (|xtk|)
  (|make-classes|)
  (|foreign|)
  (|widget|)
  (|resources|)
  (|event|)
  (|callbacks|)
  (|ol-init|)
  (|graphics|)
  (|font|)
  (|gcontext|)
  (|ol-examples|)
  )
