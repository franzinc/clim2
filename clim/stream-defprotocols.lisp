;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM; Base: 10; Lowercase: Yes -*-
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

;; $fiHeader: stream-defprotocols.lisp,v 1.7 91/08/05 14:35:33 cer Exp $

(in-package :clim)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved.
Copyright (c) 1991, Franz Inc. All rights reserved
 Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

;;--- What about the "original self" (delegation) problem??

;;; Fundamental input --- These should be on fundamental-input-character-stream??
(define-stream-protocol fundamental-input-stream)

(defoperation stream-read-char fundamental-input-stream
  ((stream fundamental-input-stream)))

(defoperation stream-unread-char fundamental-input-stream
  ((stream fundamental-input-stream) character)
  #+Genera (:selector :untyi))

(defoperation stream-read-char-no-hang fundamental-input-stream
  ((stream fundamental-input-stream)))

(defoperation stream-peek-char fundamental-input-stream
  ((stream fundamental-input-stream)))

(defoperation stream-listen fundamental-input-stream
  ((stream fundamental-input-stream))
  #+Genera (:selector :listen))

(defoperation stream-read-line fundamental-input-stream
  ((stream fundamental-input-stream))
  #+Genera (:selector :line-in))		;the optional leader argument is not supported

(defoperation stream-clear-input fundamental-input-stream
  ((stream fundamental-input-stream))
  #+Genera (:selector :clear-input))

;;; The next three Genera-only generic functions allow CLIM streams to accept these
;;; Genera messages that do not have arguments compatible with the Gray functions.

#+Genera
(defoperation stream-compatible-read-char fundamental-input-stream
  ((stream fundamental-input-stream) &optional eof)
  (:selector :tyi))

#+Genera
(defoperation stream-compatible-read-char-no-hang fundamental-input-stream
  ((stream fundamental-input-stream) &optional eof)
  (:selector :tyi-no-hang))

#+Genera
(defoperation stream-compatible-peek-char fundamental-input-stream
  ((stream fundamental-input-stream) &optional eof)
  (:selector :tyipeek))

#+Genera
;; Called from si:with-clim-compatible-input-editing
(defoperation si:stream-compatible-input-editing fundamental-input-stream
  ((stream fundamental-input-stream)
   continuation activation-character-p blip-character-p))

#+Genera
;; READ calls this directly
(defoperation stream-compatible-any-tyi fundamental-input-stream
  ((stream fundamental-input-stream) &optional eof)
  (:selector :any-tyi))

#+Genera
;; READ calls this directly
(defoperation stream-compatible-any-tyi-no-hang fundamental-input-stream
  ((stream fundamental-input-stream) &optional eof)
  (:selector :any-tyi-no-hang))

#+Genera
(defoperation stream-compatible-interactive fundamental-input-stream
  ((stream fundamental-input-stream))
  (:selector :interactive))

#+Genera
(defoperation stream-compatible-input-wait fundamental-input-stream
  ((stream fundamental-input-stream) whostate function &rest arguments)
  (:selector :input-wait))


;;; Extended input
(define-stream-protocol basic-extended-input-protocol
  stream-input-buffer
  stream-pointers
  stream-primary-pointer)

(defoperation stream-read-gesture basic-extended-input-protocol
  ((stream basic-extended-input-protocol)
   &key timeout peek-p
	(input-wait-test *input-wait-test*)
	(input-wait-handler *input-wait-handler*)
	(pointer-button-press-handler *pointer-button-press-handler*)))

(defoperation stream-unread-gesture basic-extended-input-protocol
  ((stream basic-extended-input-protocol) gesture))

;;; Extended Input
(defoperation stream-input-wait basic-extended-input-protocol 
  ((stream basic-extended-input-protocol) &key timeout input-wait-test))

(defoperation stream-pointer-position* basic-extended-input-protocol
  ((stream basic-extended-input-protocol) &key (timeout 0) pointer)
  (declare (values x y)))

(defoperation stream-set-pointer-position* basic-extended-input-protocol
  ((stream basic-extended-input-protocol) x y &key pointer))

(defoperation stream-note-pointer-button-press basic-extended-input-protocol
  ((stream basic-extended-input-protocol) pointer button shift-mask x y))

(defoperation stream-pointer-input-rectangle* basic-extended-input-protocol
  ((stream basic-extended-input-protocol) pointer &key left top right bottom)
  (declare (values left top right bottom)))

(defoperation accept-1 basic-extended-input-protocol
  ((stream basic-extended-input-protocol) presentation-type &rest accept-args)
  (declare (values object type)))

(defoperation prompt-for-accept basic-extended-input-protocol
  ((stream basic-extended-input-protocol) type &rest accept-args))


;;; Output

;;; Fundamental output
(define-stream-protocol fundamental-output-stream)
(define-stream-protocol fundamental-character-output-stream)

;;; This is not responsible for wrapping text.
(defoperation stream-write-char fundamental-character-output-stream 
  ((stream fundamental-character-output-stream) char)
  #+Genera (:selector :tyo))

(defoperation stream-line-column fundamental-character-output-stream 
	      ((stream fundamental-character-output-stream)))

#+excl
(defoperation excl::stream-interactive-force-output fundamental-character-output-stream 
	      ((stream fundamental-character-output-stream)))


(defoperation stream-write-string fundamental-character-output-stream
  ((stream fundamental-character-output-stream) string &optional (start 0) end)
  #+Genera (:selector :string-out))

(defoperation stream-scan-string-for-writing fundamental-character-output-stream
  ((stream fundamental-character-output-stream)
   string medium start end style cursor-x max-x &optional glyph-buffer))

(defoperation stream-scan-character-for-writing fundamental-character-output-stream
  ((stream fundamental-character-output-stream) character style cursor-x max-x))

(defoperation stream-write-string-internal fundamental-character-output-stream
  ((stream fundamental-character-output-stream) glyph-buffer start end font color x y))

(defoperation stream-terpri fundamental-character-output-stream
  ((stream fundamental-character-output-stream)))

(defoperation stream-fresh-line fundamental-character-output-stream
  ((stream fundamental-character-output-stream))
  #+Genera (:selector :fresh-line))

(defoperation stream-force-output fundamental-output-stream
  ((stream fundamental-output-stream))
  #+Genera (:selector :force-output))

(defoperation stream-finish-output fundamental-output-stream
  ((stream fundamental-output-stream))
  #+Genera (:selector :finish))

(defoperation stream-clear-output fundamental-output-stream
  ((stream fundamental-output-stream))
  #+Genera (:selector :clear-output))

#+Genera
;; FORMAT calls this directly
(defoperation stream-compatible-output-as-presentation-1 fundamental-character-output-stream
  ((stream fundamental-character-output-stream)
   continuation continuation-args &rest object-options)
  (:selector :output-as-presentation-1))

#+Genera
(defoperation stream-compatible-output-as-presentation fundamental-character-output-stream
  ((stream fundamental-character-output-stream)
   continuation xstream &rest object-options)
  (:selector :output-as-presentation))

#+Genera
(defoperation stream-compatible-line-out fundamental-character-output-stream
  ((stream fundamental-character-output-stream) string &optional (start 0) end)
  (:selector :line-out))

#+Genera
(defoperation stream-compatible-with-character-style fundamental-character-output-stream
  ((stream fundamental-character-output-stream)
   new-style continuation xstream &optional bind-line-height)
  (:selector :with-character-style))

(define-stream-protocol basic-extended-output-protocol
  medium-foreground
  medium-background
  medium-text-style
  medium-merged-text-style-valid
  medium-merged-text-style
  medium-default-text-style
  
  stream-merged-text-style
  stream-baseline
  stream-current-line-height
  stream-vsp
  stream-end-of-line-action
  stream-end-of-page-action
  stream-text-margin
  stream-default-view
  stream-display-device-type
  stream-output-glyph-buffer)

(defoperation stream-cursor-position* basic-extended-output-protocol
  ((stream basic-extended-output-protocol))
  (declare (values x y)))

(defoperation stream-set-cursor-position* basic-extended-output-protocol
  ((stream basic-extended-output-protocol) x y))

;; Like STREAM-SET-CURSOR-POSITION*, but is more conservative about closing
;; the current text output record.
(defoperation stream-set-cursor-position*-internal basic-extended-output-protocol
  ((stream basic-extended-output-protocol) x y))

(defoperation stream-ensure-cursor-visible basic-extended-output-protocol
  ((stream basic-extended-output-protocol) &optional x y))

(defoperation stream-advance-cursor-x basic-extended-output-protocol
  ((stream basic-extended-output-protocol) amount))

(defoperation stream-advance-cursor-line basic-extended-output-protocol
  ((stream basic-extended-output-protocol)))

(defoperation stream-string-width basic-extended-output-protocol
  ((stream basic-extended-output-protocol) string &key (start 0) end text-style))

(defoperation stream-character-width basic-extended-output-protocol
  ((stream basic-extended-output-protocol) character &optional text-style))

(defoperation stream-line-height basic-extended-output-protocol
  ((stream basic-extended-output-protocol) &optional text-style))

(defoperation formatting-cell-internal basic-extended-output-protocol
  ((stream basic-extended-output-protocol) continuation
   &key (align-x :left) (align-y :top) (record-type 'cell-output-record)
	minimum-width minimum-height))

;; not sure this is the right place...
(defoperation incremental-redisplay basic-extended-output-protocol
  ((stream basic-extended-output-protocol)
   position erases moves draws erase-overlapping move-overlapping))

(defoperation decode-stream-for-writing basic-extended-output-protocol
  ((stream basic-extended-output-protocol) &optional brief-p))

#+Genera
(defoperation stream-compatible-cursor-position* basic-extended-output-protocol
  ((stream basic-extended-output-protocol) &optional unit)
  #+Genera (:selector :read-cursorpos))

#+Genera
(defoperation stream-compatible-set-cursor-position* basic-extended-output-protocol
  ((stream basic-extended-output-protocol) x y &optional unit)
  #+Genera (:selector :set-cursorpos))

#+Genera
(defoperation stream-compatible-increment-cursor-position* basic-extended-output-protocol
  ((stream basic-extended-output-protocol) x y &optional unit)
  #+Genera (:selector :increment-cursorpos))

(define-stream-protocol drawing-state-mixin
  medium-ink
  medium-transformation
  medium-line-style
  medium-+y-upward-p)

(defoperation silica::invoke-with-drawing-options drawing-state-mixin
	      ((stream drawing-state-mixin) function &rest x))

(defoperation silica::medium-draw-line* drawing-state-mixin
	      ((stream drawing-state-mixin) from-x from-y to-x to-y))



;;; Window protocol

#+Silica
(define-stream-protocol window-mixin)

#-Silica
(define-stream-protocol window-mixin
  window-parent
  window-children
  window-console
  window-name
  window-depth
  window-viewport
  window-update-region
  window-visibility
  window-label)

(defoperation window-clear window-mixin
  ((window window-mixin)))

(defoperation window-with-zero-viewport-position window-mixin
  ((window window-mixin) continuation))

#-Silica (progn
(defoperation window-erase-viewport window-mixin
  ((window window-mixin)))

(defoperation window-stack-on-top window-mixin
  ((window window-mixin)))

(defoperation window-stack-on-bottom window-mixin
  ((window window-mixin)))

(defoperation copy-area-internal window-mixin
  ((window window-mixin)
   from-left from-top from-right from-bottom to-left to-top))

(defoperation window-refresh window-mixin
  ((window window-mixin)))

(defoperation window-expose window-mixin
  ((window window-mixin)))

(defoperation window-drawing-possible window-mixin
  ((window window-mixin)))

(defoperation window-viewport-position* window-mixin
  ((window window-mixin)))

(defoperation window-set-viewport-position* window-mixin
  ((window window-mixin) x y))

(defoperation redisplay-decorations window-mixin
  ((window window-mixin)) )

(defoperation window-to-screen-coordinates window-mixin
  ((window window-mixin) x y))

(defoperation screen-to-window-coordinates window-mixin
  ((window window-mixin) x y))

(defoperation window-inside-edges window-mixin
  ((window window-mixin))
  (declare (values left top right bottom)))

(defoperation window-inside-size window-mixin
  ((window window-mixin))
  (declare (values width height)))

(defoperation window-set-inside-edges window-mixin
  ((window window-mixin) new-left new-top new-right new-bottom))

(defoperation window-set-inside-size window-mixin
  ((window window-mixin) new-width new-height))

(defoperation window-inside-left window-mixin
  ((window window-mixin)))

(defoperation window-inside-top window-mixin
  ((window window-mixin)))

(defoperation window-inside-right window-mixin
  ((window window-mixin)))

(defoperation window-inside-bottom window-mixin
  ((window window-mixin)))

(defoperation window-inside-width window-mixin
  ((window window-mixin)))

(defoperation window-inside-height window-mixin
  ((window window-mixin)))

(defoperation window-label-size window-mixin
  ((window-mixin window-mixin) &optional (label (window-label window-mixin))))

(defoperation window-note-size-or-position-change window-mixin
  ((window window-mixin) new-left new-top new-right new-bottom))

(defoperation window-shift-visible-region window-mixin
  ((window window-mixin) 
   old-left old-top old-right old-bottom
   new-left new-top new-right new-bottom))

(defoperation window-flush-update-region window-mixin
  ((window window-mixin)))

(defoperation window-process-update-region window-mixin
  ((window window-mixin)))

(defoperation window-beep window-mixin
  ((window window-mixin)))

;;; What shift keys are presently down?
(defoperation window-shift-mask window-mixin
  ((window window-mixin)))

)	;#-Silica


;;; Output recording.
(define-stream-protocol basic-output-recording
  stream-draw-p
  stream-record-p
  stream-redisplaying-p
  output-recording-stream-output-record
  output-recording-stream-text-output-record
  output-recording-stream-highlighted-presentation
  output-recording-stream-current-output-record-stack
  output-recording-stream-output-record-absolute-position
  output-recording-stream-redisplay-output-record)

(defoperation add-output-record basic-output-recording
  ((stream basic-output-recording) element))

(defoperation output-recording-stream-replay basic-output-recording
  ((stream basic-output-recording) &optional region))

(defoperation with-output-recording-options-internal basic-output-recording
  ((stream basic-output-recording) draw-p record-p continuation))

(defoperation close-current-text-output-record basic-output-recording
  ((stream basic-output-recording) &optional wrapped))

;;; Graphics protocol is in defs-graphics-generics
;;; Interactive protocol doesn't need to be encapsulated, for obvious reasons.

;;; "Implementation" protocol
(define-stream-protocol implementation-protocol)

(defoperation implementation-pixels-per-point implementation-protocol
  ((stream implementation-protocol)))


#+Silica (progn
(define-stream-protocol pane-protocol)

(defoperation pane-display-function pane-protocol
  ((pane pane-protocol)))

(defoperation pane-display-time pane-protocol
  ((pane pane-protocol)))

(defoperation pane-needs-redisplay pane-protocol
  ((pane pane-protocol)))

(defoperation pane-frame pane-protocol
  ((pane pane-protocol)))

)	;#+Silica


;; Some silica stuff

(define-stream-protocol port-etc-protocol
			sheet-medium)

(defoperation port port-etc-protocol
	      ((stream port-etc-protocol)))

#+ignore
(generate-trampolines drawing-state-mixin drawing-state-mixin  
		      encapsulating-stream-mixin
		      `(slot-value ,encapsulating-stream-mixin
				   'stream)
		      *original-stream*)




