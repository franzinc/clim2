;; -*- mode: common-lisp; package: user -*-
;;
;;				-[]-
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, CA  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, CA  All rights reserved.
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
;; $fiHeader: defsys.cl,v 1.2 92/01/02 15:08:37 cer Exp Locker: cer $


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
  (|xm-font-list|)
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
