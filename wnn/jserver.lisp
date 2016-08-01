;; -*- mode: common-lisp; package: wnn -*-
;;
;; See the file LICENSE for the full license governing this code.
;;

(in-package :wnn)

(defparameter *jserver-area-size* 1024)

(defclass jserver (basic-kanji-server)
  ((login :reader jserver-login)
   (host  :reader jserver-host)
   (lang  :reader jserver-lang)
   (buf :reader jserver-buf)
   (env :reader jserver-env)
   (area :initform (excl::malloc *jserver-area-size*)
	 :reader jserver-area)))

(defmethod print-object ((js jserver) stream)
  (print-unreadable-object (js stream :type t :identity t)
    (with-slots (login host) js
      (format stream "~A@~A" login host))))

(defparameter *jserver-timeout* 5)

(defmethod initialize-instance :after
	   ((js jserver) &key server-path)
  (destructuring-bind (&key (login (system:getenv "LOGNAME"))
			    (host (or (system:getenv "JSERVER")
				      (short-site-name)))
			    (lang (or (system:getenv "LANG") "ja_JP")))
      (cdr server-path)
    (setf (slot-value js 'login) login
	  (slot-value js 'host) host
	  (slot-value js 'lang) lang)
    (let* ((buf
	    (let ((temp (mp:process-quantum mp:*current-process*)))
	      (unwind-protect
		  (progn (setf (mp:process-quantum mp:*current-process*)
			   *jserver-timeout*)
			 (mp:process-allow-schedule)
			 ;; it's important to specify a timeout of -1
			 ;; in the call to jl_open_lang otherwise wnn
			 ;; messes with sigalrm
			 (jl_open_lang (excl:string-to-native login)
				       (excl:string-to-native host)
				       (excl:string-to-native lang)
				       0 0 0 -1))
		(setf (mp:process-quantum mp:*current-process*) temp)
		(mp:process-allow-schedule))))
	   (env (and buf
		     (not (zerop buf))
		     (wnn-buf-env buf))))
      (unless (and env
		   (not (zerop env))
		   (jl_isconnect_e env))
	(error "Failed to connect to jserver ~A as ~A"
	       host login))
      (setf (slot-value js 'env) env
	    (slot-value js 'buf) buf))))

(defun bunsetu-suu (js)
  (wnn-buf-bun-suu (jserver-buf js)))

(defun get-kanji (js bunsetu &optional (bunsetu-end (1+ bunsetu)))
  (with-slots (buf area) js
    (unless (zerop (wnn_get_area buf bunsetu bunsetu-end
				 area wnn_kanji))
      (excl:native-to-string area :external-format :16-bit))))

(defun get-yomi (js bunsetu &optional (bunsetu-end (1+ bunsetu)))
  (with-slots (buf area) js
    (unless (zerop (wnn_get_area buf bunsetu bunsetu-end
				 area wnn_yomi))
      (excl:native-to-string area :external-format :16-bit))))

(defparameter *wnn-unique* :unique-kanji)

(defmacro select-bunsetu (buf bunsetu)
  `(unless (eq ,bunsetu (wnn-buf-zenkouho-bun ,buf))
     (jl_zenkouho ,buf ,bunsetu wnn_use_mae
		  (case *wnn-unique*
		    (nil wnn_no_uniq)
		    (:unique wnn_uniq)
		    (:unique-kanji wnn_uniq_knj)))))


(defun bunsetu-kouho-suu (js bunsetu)
  (with-slots (buf) js
    (select-bunsetu buf bunsetu)
    (wnn-buf-zenkouho-suu buf)))

(defun get-kouho-kanji (js bunsetu kouho &optional set-jikouho)
  (with-slots (buf area) js
    (select-bunsetu buf bunsetu)
    (let* ((suu (wnn-buf-zenkouho-suu buf))
	   (kouho (case kouho
		    (:hiragana (- suu 1))
		    (:katakana (- suu 2))
		    (t kouho))))
      (when (< kouho suu)
	(when set-jikouho
	  (jl_set_jikouho buf kouho))
	(jl_get_zenkouho_kanji buf kouho area)
	(excl:native-to-string area :external-format :16-bit)))))

(defun get-zenkouho-kanji (js bunsetu)
  (let ((zen nil))
    (dotimes (i (bunsetu-kouho-suu js bunsetu))
      (push (get-kouho-kanji js bunsetu i) zen))
    (nreverse zen)))

(defun henkan-begin (js yomi)
  ;; these two hard-wired consants were copied from
  ;; <mule>/src/wnn4fns.c
  (with-slots (buf) js
    (jl_ren_conv buf yomi 0 -1 wnn_use_mae)))

(defun henkan-end (js)
  (jl_update_hindo (jserver-buf js) 0 -1)
  nil)

;;; the clim input editor protocol

(defmethod jie-begin-kanji-conversion ((js jserver) yomi)
  (let ((r nil))
    (dotimes (i (henkan-begin js yomi))
      (push (bunsetu-kouho-suu js i) r))
    (nreverse r)))

(defmethod jie-get-kanji ((js jserver) bunsetu kouho)
  (get-kouho-kanji js bunsetu kouho))

(defmethod jie-end-kanji-conversion ((js jserver) kouhos)
  (let ((bunsetu 0)
	(r ""))
    (dolist (kouho kouhos)
      (setq r
	(concatenate 'string r (get-kouho-kanji bunsetu kouho t)))
      (incf bunsetu))
    r))

(defmethod find-kanji-server-type ((type (eql ':jserver)))
  'jserver)

(defmethod kanji-server-type ((jserver jserver))
  ':jserver)
