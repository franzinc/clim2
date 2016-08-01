;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLOE-CLIM; Base: 10; Lowercase: Yes -*-


(in-package :cloe-clim)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."


;;; These should really be defined in CL-STREAM, but doing it causes CLOE to
;;; go south during the build, my guess is the regular streams get trashed.
;;; So anyway we now do it here.

(defmethod stream-read-byte ((stream t)) 
  (read-byte stream nil :eof))

(defmethod stream-read-char ((stream t)) 
  (read-char stream nil :eof))

(defmethod stream-unread-char ((stream t) character) 
  (unread-char character stream))

(defmethod stream-read-char-no-hang ((stream t)) 
  (read-char-no-hang stream nil :eof))

(defmethod stream-peek-char ((stream t)) 
  (peek-char nil stream nil :eof))

(defmethod stream-listen ((stream t)) 
  (listen stream))

(defmethod stream-read-line ((stream t)) 
  (read-line stream nil :eof))

(defmethod stream-clear-input ((stream t)) 
  (clear-input stream))

(defmethod stream-write-byte ((stream t) integer) 
  (write-byte integer stream))

(defmethod stream-write-char ((stream t) character) 
  (write-char character stream))

(defmethod stream-write-string ((stream t) string &optional (start 0) end) 
  (write-string string stream :start start :end end))

(defmethod stream-terpri ((stream t)) (terpri stream))


(defmethod stream-fresh-line ((stream t)) 
  (fresh-line stream))

(defmethod stream-force-output ((stream t)) 
  (force-output stream))

(defmethod stream-finish-output ((stream t)) 
  (finish-output stream))

(defmethod stream-clear-output ((stream t)) 
  (clear-output stream))
