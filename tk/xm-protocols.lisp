;; -*- mode: common-lisp; package: tk -*-
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
;; $fiHeader: xm-protocols.lisp,v 1.4 92/02/24 13:04:05 cer Exp Locker: cer $

(in-package :tk)

(defforeign 'xm_add_protocol_callback
    :entry-point "_XmAddProtocolCallback")

(defforeign 'xm_intern_atom 
    :entry-point "_XmInternAtom")

(def-c-type (proto-callback-info :in-foreign-space) :struct
	     (handle :int)
	     (data :int))

(def-c-type (protocol :in-foreign-space) :struct
  (object * :char)
  (ext * :char)
  (protocol * :char))

(defun-c-callable protocol-callback-handler ((something-weird-widget :unsigned-long)
					     (x :unsigned-long)
					     (call-data
					      :unsigned-long))

  ;; Seems that the first argument is not a widget but a pointer to
  ;; one of the above, and that the protocol component is the widget
  
  #+ignore
  (print (list something-weird-widget
	       (protocol-object something-weird-widget)
	       (protocol-ext something-weird-widget)
	       (protocol-protocol something-weird-widget)
	       (proto-callback-info-handle x)))
  
  (callback-handler-1 (proto-callback-info-handle x)
		      (proto-callback-info-data x)
		      call-data))

(defvar *protocol-callback-handler-address* (register-function 'protocol-callback-handler))

(defmethod spread-callback-data (widget call-data (type (eql :protocol-callback)))
  (declare (ignore widget call-data))
 (values))

(defun add-protocol-callback (shell property protocol function &rest args)
  (let ((type :protocol-callback))
    (xm_add_protocol_callback
     shell
     (if (integerp property) property (xm-intern-atom shell property))
     (if (integerp protocol) protocol (xm-intern-atom shell protocol))
     *protocol-callback-handler-address*
     (let ((x (make-proto-callback-info)))
       (setf (proto-callback-info-handle x) shell
	     (proto-callback-info-data x)
	     (caar (push
		    (list (new-callback-id) (cons function args) type)
		    (widget-callback-data shell))))
       x))))

(defun add-wm-protocol-callback (shell protocol fn &rest args)
  (apply 
   #'add-protocol-callback
   shell
   "WM_PROTOCOLS"
   (case protocol
     (:wm-delete-window "WM_DELETE_WINDOW")
     (:wm-save-your-self "WM_SAVE_YOUR_SELF")
     (t protocol))
   fn
   args))

(defun xm-intern-atom (shell name &optional only-if-exists)
  (xm_intern_atom
   (object-display shell)
   (string-to-char* (string name))
   (if only-if-exists 1 0)))




   

