;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2004 Franz Inc, Oakland, CA - All rights reserved.
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
;; $Id: printer.lisp,v 2.5 2004/01/16 19:15:42 layer Exp $

(in-package :clim-user)

(defun get-printer-options (&key (printer :lw)
				 (copies 1)
				 (orrientation :porta)
				 (multi-page nil)
				 (send-mail nil)
				 (stream *standard-output*))
  (accepting-values (stream :own-window t :label "Printer options"
			    :align-prompts t)
    (setq printer (accept '(member :lw :lw2 :lw3)
			  :default printer 
			  :prompt "Printer" :stream stream))
    (setq copies (accept '(integer 0 10)
			 :default copies
			 :prompt "Copies"
			 :stream stream))
    (setq orrientation (accept '(member :portrait :landscape)
			       :default orrientation
			       :prompt "Orrientation"
			       :stream stream))
    (setq multi-page (accept 'boolean
			     :default multi-page
			     :prompt "Multi-page"
			     :stream stream))
    (setq send-mail (accept 'boolean
			    :default send-mail
			    :prompt "Notify"
			    :stream stream))))

(defmacro with-output-to-printer ((stream &rest options) &body body)
  (invoke-with-output-to-printer #'(lambda (,stream) ,@body) ,@options))

(defun invoke-with-output-to-printer (continuation &rest options)
  #+mswindows (declare (ignore continuation options))
  #+mswindows
  (notify-user *application-frame* "Not implemented on this platform")
  #+unix
  (multiple-value-bind (printer copies orrientation multi-page send-mail)
      (apply #'get-printer-options options)
    (with-open-stream 
	(pipe (excl:run-shell-command  (format nil "lpr -P~A ~[~*;-#~D~] ~[-n~]"
					       printer
					       (= 1 copies)
					       copies
					       send-mail)
				       :input :stream :wait nil))
      (with-output-to-postscript-stream (stream pipe 
						:orrientation orrientation
						:multi-page multi-page)
	(funcall continuation stream)))))
