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
;; $fiHeader: defsys.lisp,v 1.5 92/01/31 14:54:19 cer Exp $

(defsys::defsystem :basic-tk
    (:default-pathname (frob-pathname "tk"))

  (|pkg|)
  (|foreign-obj|)
  (|macros|)
  
  ;; Xlib stuff
  
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
  )

(defsys::defsystem :xm-tk
    (:default-pathname (frob-pathname "tk"))
  (|load-xm|)
  (|xm-init|)
  (|xm-widgets|)
  (|xm-font-list|)
  (|xm-protocols|)
  (|convenience|)
  (|examples|)
  )


(defsys::defsystem :hi-tk
    (:default-pathname (frob-pathname "tk"))
  (|make-widget|))

(defsys::defsystem :ol-tk
    (:default-pathname (frob-pathname "tk"))
  (|load-ol|)
  (|ol-init|)
  (|ol-callbacks|)
  (|ol-examples|))
