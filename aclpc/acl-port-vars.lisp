;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: ACL-CLIM; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

#|****************************************************************************
*                                                                            *
*                                                                            *
*  This file implements the CLIM Port protocol.  Also handles processing     *
*  events, and text and keyboard support.                                    *
*                                                                            *
*                                                                            *
****************************************************************************|#

(in-package :acl-clim)

(defparameter *keysym-alist*
	      `((#\Return . :return)
		(#\Newline . :newline)
		(#\Tab . :tab)
		(#\Rubout . :rubout)
		(#\Backspace . :backspace)
		(#\Page . :page)
		(#\Linefeed . :linefeed)
		(#\Escape . :escape)))


;;; MSWindows Virtual-Key Codes Win32 PR, V2p872
(defparameter *vk->keysym*
	      `(
		;; The semi-standard characters
		(#x0d #\newline :enter :newline #\return :return)
		(#x20 #\space :space)
		(#x09 #\tab :tab)
		(#x2e :delete)
		(#x08 #\backspace :backspace :rubout)
		;;(???? :page)
		;;(???? :linefeed)
		(#x1b #\escape :escape :abort)
		;; The shifts
		(#x10 :left-shift)
		(#x11 :left-control)
		(#x14 :caps-lock)
		(#x12 :left-meta)
		(#x90 :num-lock)
		;; Non-standard keys
		(#x03 :cancel)
		(#x0c :clear :clear-input)
		(#x13 :pause)
		(#x21 :page-up :scroll-up)
		(#x22 :page-down :scroll)
		(#x23 :end)
		(#x24 :home)
		(#x25 :left-arrow)
		(#x26 :up-arrow)
		(#x27 :right-arrow)
		(#x28 :down-arrow)
		(#x29 :select)
		(#x2b :execute)
		(#x2c :print-screen)
		(#x2d :insert)
		(#x2f :help)
		(#x60 :keypad-0)
		(#x61 :keypad-1)
		(#x62 :keypad-2)
		(#x63 :keypad-3)
		(#x64 :keypad-4)
		(#x65 :keypad-5)
		(#x66 :keypad-6)
		(#x67 :keypad-7)
		(#x68 :keypad-8)
		(#x69 :keypad-9)
		(#x6a :keypad-multiply)
		(#x6b :keypad-add)
		(#x6c :keypad-separator)
		(#x6d :keypad-subtract)
		(#x6e :keypad-decimal)
		(#x6f :keypad-divide)
		(#x70 :f1)
		(#x71 :f2)
		(#x72 :f3)
		(#x73 :f4)
		(#x74 :f5)
		(#x75 :f6)
		(#x76 :f7)
		(#x77 :f8)
		(#x78 :f9)
		(#x79 :f10)
		(#x7a :f11)
		(#x7b :f12)
		(#x7c :f13)
		(#x7d :f14)
		(#x7e :f15)
		(#x7f :f16)
		(#x80 :f17)
		(#x81 :f18)
		(#x82 :f19)
		(#x83 :f20)
		(#x84 :f21)
		(#x85 :f22)
		(#x86 :f23)
		(#x87 :f24)
		(#x91 :scroll-lock)
		;;(???? :complete)
		;;(???? :refresh)
		))

(defparameter *char->keysym*
  (let ((array (make-array 256 :initial-element nil)))
      (flet ((cstring (x)
	       (ecase excl:*current-case-mode*
		 ((:case-insensitive-upper)
		  (string-upcase (string x)))
		 ((:case-sensitive-lower)
		  (string x))
		 ((:case-insensitive-lower)
		  (string-downcase (string x))))))
	(dolist (char '(#\newline #\escape #\backspace #\tab #\space #\return))
	  (setf (svref array (char-code char))
	    (intern (cstring (char-name char))
		    (find-package :keyword))))
	(loop for code from (char-code #\!) to (char-code #\~)
	    do (setf (svref array code)
		 (intern (cstring (code-char code))
			 (find-package :keyword))))
	array)))

