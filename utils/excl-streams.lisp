;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM-LISP; Base: 10; Lowercase: Yes -*-
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

;; $fiHeader: excl-streams.lisp,v 1.1 91/08/30 13:57:47 cer Exp Locker: cer $

(in-package :clim-lisp)

"Copyright (c) 1991 Symbolics, Inc.  All rights reserved."
"Copyright (c) 1991, Franz Inc. All rights reserved"

#-PCL
(defgeneric pathname (stream))

(defmethod pathname (stream)
  (lisp:pathname stream))

(deftype pathname () 'lisp:pathname)

;;;

#-PCL
(defgeneric truename (stream))

(defmethod truename (stream)
  (lisp:truename stream))

