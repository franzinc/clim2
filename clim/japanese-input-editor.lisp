;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

#-ics
(eval-when (compile)
  (warn "~S contains fat strings but is being compiled with a non-ICS lisp"
	excl:*source-pathname*))

(excl:ics-target-case
(:+ics

#-ics
(cerror "Continue with incorrect fat strings"
	"~S contains fat strings but was compiled with a non-ICS lisp"
	excl:*source-pathname*)

(defclass japanese-input-editing-stream
    (standard-input-editing-stream)
  (;; romaji-kana state transition
   (kana-state :initform nil)
   ;; kana->kanji convertor
   (kanji-server :initform nil :initarg :kanji-server)))

(defun make-kana-state ()
  (make-array 10 :fill-pointer 0 :adjustable t))

(defvar *initial-kana-state*
    (make-kana-state))

(define-input-editor-gestures
    (:ie-kana-mode :\\ :control))

(assign-input-editor-key-bindings
   com-ie-kana-mode :ie-kana-mode)

(define-input-editor-command (com-ie-kana-mode) (stream input-buffer)
  (with-slots (command-mode command-state kana-state) stream
    (setf command-mode *kana-input-editor-command-aarray*
	  command-state *kana-input-editor-command-aarray*
	  kana-state *initial-kana-state*)
    (let* ((point (stream-insertion-pointer stream))
	   (end (fill-pointer input-buffer)))
      (cond ((= point end)
	     (incf (fill-pointer input-buffer) 2))
	    (t
	     (erase-input-buffer stream point)
	     (shift-buffer-portion input-buffer point (+ point 2))))
      (setf (subseq input-buffer point) "||"
	    (stream-insertion-pointer stream) (1+ point))
      (redraw-input-buffer stream point))))

(defmethod find-kana-start ((stream input-editing-stream-mixin))
  (with-slots (input-buffer) stream
    (let ((i (stream-insertion-pointer stream))
	  (input-buffer (input-editor-buffer stream)))
      (loop
	(when (zerop i)
	  (return nil))
	(decf i)
	(when (eql (aref input-buffer i) #\|)
	  (return i))))))

(defmethod find-kana-end ((stream input-editing-stream-mixin))
  (with-slots (input-buffer) stream
    (let* ((i (stream-insertion-pointer stream))
	   (input-buffer (input-editor-buffer stream))
	   (fill-pointer (fill-pointer input-buffer)))
      (loop
	(when (eq (aref input-buffer i) #\|)
	  (return i))
	(when (eql i fill-pointer)
	  (return nil))
	(incf i)))))

(defmethod kana-end-input ((stream input-editing-stream-mixin))
  (with-slots (command-mode command-state kana-state input-buffer)
      stream
    (setf command-mode *input-editor-command-aarray*
	  command-state *input-editor-command-aarray*)
    (let ((start (find-kana-start stream))
	  (end (find-kana-end stream)))
      (erase-input-buffer stream start)
      (shift-buffer-portion input-buffer (1+ end) end)
      (shift-buffer-portion input-buffer (1+ start) start)
      (setf (stream-insertion-pointer stream) (1- end))
      (redraw-input-buffer stream start)
      (immediate-rescan stream))))

(defmethod input-editor-kanji-server ((stream input-editing-stream-mixin))
  (with-slots (kanji-server) stream
    (or kanji-server
	(setq kanji-server (find-kanji-server)))))

(defmethod kana-begin-henkan ((stream input-editing-stream-mixin))
  (with-slots (command-mode command-state kana-state input-buffer)
      stream
    (let* ((start (find-kana-start stream))
	   (end (find-kana-end stream))
	   (yomi (coerce (subseq input-buffer (1+ start) end)
			 'string))
	   (kanji-server (input-editor-kanji-server stream))
	   (candidates (jie-begin-kanji-conversion kanji-server yomi))
	   (kanji (let ((bunsetu 0)
			(r ""))
		    (dolist (candidate candidates)
		      (declare (ignore candidate))
		      (setq r
			(concatenate 'string
			  r (jie-get-kanji kanji-server bunsetu 0))))
		    r)))
      (erase-input-buffer stream start)
      (shift-buffer-portion input-buffer
			    end
			    (+ end (- (length kanji) (length yomi))))
      (setf (subseq input-buffer (1+ start)) kanji)
      (setf (stream-insertion-pointer stream) (1+ start))
      (redraw-input-buffer stream start)
      (immediate-rescan stream))))

(defmethod kana-process-gesture ((stream input-editing-stream-mixin)
				 (gesture character) type)
  (with-slots (kana-state input-buffer) stream
    (when (eq gesture #\newline)
      (kana-end-input stream))
    (when (eq gesture #\space)
      (kana-begin-henkan stream))
    (let ((entry (cdr (find gesture kana-state :key #'car))))
      (if entry
	  (if (arrayp entry)
	      (progn
		(setf kana-state entry)
		(return-from kana-process-gesture
		  (values gesture type)))
	    (destructuring-bind (hiragana katakana depth new) entry
	      (declare (ignore katakana))
	      #+debug
	      (format excl:*initial-terminal-io* "~%~S" entry)
	      (let* ((point (- (stream-insertion-pointer stream) depth))
		     (end (fill-pointer input-buffer))
		     (new-string (concatenate 'string hiragana new))
		     (length (length new-string)))
		(cond ((= point end)
		       (incf (fill-pointer input-buffer) length))
		      (t
		       (erase-input-buffer stream point)
		       (shift-buffer-portion input-buffer
					     (+ point depth)
					     (+ point length))))
		(setf (subseq input-buffer point) new-string
		      (stream-insertion-pointer stream) (+ point length))
		(redraw-input-buffer stream point))
	      (setf kana-state *initial-kana-state*)
	      (dovector (c new)
  		 (setf kana-state (cdr (find c kana-state :key #'car))))
	      (return-from kana-process-gesture
		nil)))
	(progn
	  (setf kana-state *initial-kana-state*)
	  (return-from kana-process-gesture
	    nil))))))

;;;;; default romaji-kana conversion

(defun add-romaji-kana (romaji hiragana katakana &optional new-romaji)
  (let ((state *initial-kana-state*)
	(depth 0))
    (dorest (r (map 'list #'identity romaji))
      (destructuring-bind (c . rest) r
	(if rest
	    (let ((new-state (cdr (find c state :key #'car))))
	      (if new-state
		  (if (arrayp new-state)
		      (setf state new-state)
		    (error "Trying to add a non-terminal romaji-kana ~A"
			   romaji))
		(let ((new-state (make-kana-state)))
		  (vector-push-extend (cons c new-state) state)
		  (setf state new-state))))
	  (let ((entry (find c state :key #'car))
		(terminal (list hiragana katakana depth new-romaji)))
	    (if entry
		(if (arrayp (cdr entry))
		    (error "Trying to add a terminal romaji-kana ~A"
			   romaji)
		  (setf (cdr entry) terminal))
	      (vector-push-extend (cons c terminal) state)))))
      (incf depth))))

(dolist (a '("k" "s" "t" "h" "y" "r" "w" "g" "z" "d" "b"
		 "p" "c" "f" "j" "v"))
  (add-romaji-kana (format nil "~A~A" a a) "っ" "ッ" a))

(add-romaji-kana "tch"  "っ" "ッ" "ch")

(add-romaji-kana "n'" "ん" "ン")
(add-romaji-kana "N" "ん" "ン")

(dolist (a '("b" "m" "p"))
  (add-romaji-kana (format nil "m~A" a) "ん" "ン" a))

(dolist (a '("k" "s" "t" "c" "h" "f" "m" "r" "l"
	      "w" "g" "z" "j" "d" "b" "v" "p" "x" "n"))
  (add-romaji-kana (format nil "n~A" a) "ん" "ン" a))

(add-romaji-kana "a" "あ" "ア")
(add-romaji-kana "i" "い" "イ")
(add-romaji-kana "u" "う" "ウ")
(add-romaji-kana "e" "え" "エ")
(add-romaji-kana "o" "お" "オ")
(add-romaji-kana "ka" "か" "カ")
(add-romaji-kana "ki" "き" "キ")
(add-romaji-kana "ku" "く" "ク")
(add-romaji-kana "ke" "け" "ケ")
(add-romaji-kana "ko" "こ" "コ")
(add-romaji-kana "kya" "きゃ" "キャ")
(add-romaji-kana "kyu" "きゅ" "キュ")
(add-romaji-kana "kye" "きぇ" "キェ")
(add-romaji-kana "kyo" "きょ" "キョ")
(add-romaji-kana "sa" "さ" "サ")
(add-romaji-kana "si" "し" "シ")
(add-romaji-kana "su" "す" "ス")
(add-romaji-kana "se" "せ" "セ")
(add-romaji-kana "so" "そ" "ソ")
(add-romaji-kana "sya" "しゃ" "シャ")
(add-romaji-kana "syu" "しゅ" "シュ")
(add-romaji-kana "sye" "しぇ" "シェ")
(add-romaji-kana "syo" "しょ" "ショ")
(add-romaji-kana "sha" "しゃ" "シャ")
(add-romaji-kana "shi" "し" "シ")
(add-romaji-kana "shu" "しゅ" "シュ")
(add-romaji-kana "she" "しぇ" "シェ")
(add-romaji-kana "sho" "しょ" "ショ")
(add-romaji-kana "ta" "た" "タ")
(add-romaji-kana "ti" "ち" "チ")
(add-romaji-kana "tu" "つ" "ツ")
(add-romaji-kana "te" "て" "テ")
(add-romaji-kana "to" "と" "ト")
(add-romaji-kana "tya" "ちゃ" "チャ")
(add-romaji-kana "tyi" "てぃ" "ティ")
(add-romaji-kana "tyu" "ちゅ" "チュ")
(add-romaji-kana "tye" "ちぇ" "チェ")
(add-romaji-kana "tyo" "ちょ" "チョ")
(add-romaji-kana "tsu" "つ" "ツ")
(add-romaji-kana "cha" "ちゃ" "チャ")
(add-romaji-kana "chi" "ち" "チ")
(add-romaji-kana "chu" "ちゅ" "チュ")
(add-romaji-kana "che" "ちぇ" "チェ")
(add-romaji-kana "cho" "ちょ" "チョ")
(add-romaji-kana "na" "な" "ナ")
(add-romaji-kana "ni" "に" "ニ")
(add-romaji-kana "nu" "ぬ" "ヌ")
(add-romaji-kana "ne" "ね" "ネ")
(add-romaji-kana "no" "の" "ノ")
(add-romaji-kana "nya" "にゃ" "ニャ")
(add-romaji-kana "nyu" "にゅ" "ニュ")
(add-romaji-kana "nye" "にぇ" "ニェ")
(add-romaji-kana "nyo" "にょ" "ニョ")
(add-romaji-kana "ha" "は" "ハ")
(add-romaji-kana "hi" "ひ" "ヒ")
(add-romaji-kana "hu" "ふ" "フ")
(add-romaji-kana "he" "へ" "ヘ")
(add-romaji-kana "ho" "ほ" "ホ")
(add-romaji-kana "hya" "ひゃ" "ヒャ")
(add-romaji-kana "hyu" "ひゅ" "ヒュ")
(add-romaji-kana "hye" "ひぇ" "ヒェ")
(add-romaji-kana "hyo" "ひょ" "ヒョ")
(add-romaji-kana "fa" "ふぁ" "ファ")
(add-romaji-kana "fi" "ふぃ" "フィ")
(add-romaji-kana "fu" "ふ" "フ")
(add-romaji-kana "fe" "ふぇ" "フェ")
(add-romaji-kana "fo" "ふぉ" "フォ")
(add-romaji-kana "ma" "ま" "マ")
(add-romaji-kana "mi" "み" "ミ")
(add-romaji-kana "mu" "む" "ム")
(add-romaji-kana "me" "め" "メ")
(add-romaji-kana "mo" "も" "モ")
(add-romaji-kana "mya" "みゃ" "ミャ")
(add-romaji-kana "myu" "みゅ" "ミュ")
(add-romaji-kana "mye" "みぇ" "ミェ")
(add-romaji-kana "myo" "みょ" "ミョ")
(add-romaji-kana "ya" "や" "ヤ")
(add-romaji-kana "yi" "い" "イ")
(add-romaji-kana "yu" "ゆ" "ユ")
(add-romaji-kana "ye" "いぇ" "イェ")
(add-romaji-kana "yo" "よ" "ヨ")
(add-romaji-kana "ra" "ら" "ラ")
(add-romaji-kana "ri" "り" "リ")
(add-romaji-kana "ru" "る" "ル")
(add-romaji-kana "re" "れ" "レ")
(add-romaji-kana "ro" "ろ" "ロ")
(add-romaji-kana "la" "ら" "ラ")
(add-romaji-kana "li" "り" "リ")
(add-romaji-kana "lu" "る" "ル")
(add-romaji-kana "le" "れ" "レ")
(add-romaji-kana "lo" "ろ" "ロ")
(add-romaji-kana "rya" "りゃ" "リャ")
(add-romaji-kana "ryu" "りゅ" "リュ")
(add-romaji-kana "rye" "りぇ" "リェ")
(add-romaji-kana "ryo" "りょ" "リョ")
(add-romaji-kana "lya" "りゃ" "リャ")
(add-romaji-kana "lyu" "りゅ" "リュ")
(add-romaji-kana "lye" "りぇ" "リェ")
(add-romaji-kana "lyo" "りょ" "リョ")
(add-romaji-kana "wa" "わ" "ワ")
(add-romaji-kana "wi" "ゐ" "ヰ")
(add-romaji-kana "wu" "う" "ウ")
(add-romaji-kana "we" "ゑ" "ヱ")
(add-romaji-kana "wo" "を" "ヲ")
(add-romaji-kana "ga" "が" "ガ")
(add-romaji-kana "gi" "ぎ" "ギ")
(add-romaji-kana "gu" "ぐ" "グ")
(add-romaji-kana "ge" "げ" "ゲ")
(add-romaji-kana "go" "ご" "ゴ")
(add-romaji-kana "gya" "ぎゃ" "ギャ")
(add-romaji-kana "gyu" "ぎゅ" "ギュ")
(add-romaji-kana "gye" "ぎぇ" "ギェ")
(add-romaji-kana "gyo" "ぎょ" "ギョ")
(add-romaji-kana "za" "ざ" "ザ")
(add-romaji-kana "zi" "じ" "ジ")
(add-romaji-kana "zu" "ず" "ズ")
(add-romaji-kana "ze" "ぜ" "ゼ")
(add-romaji-kana "zo" "ぞ" "ゾ")
(add-romaji-kana "zya" "じゃ" "ジャ")
(add-romaji-kana "zyu" "じゅ" "ジュ")
(add-romaji-kana "zye" "じぇ" "ジェ")
(add-romaji-kana "zyo" "じょ" "ジョ")
(add-romaji-kana "ja" "じゃ" "ジャ")
(add-romaji-kana "ji" "じ" "ジ")
(add-romaji-kana "ju" "じゅ" "ジュ")
(add-romaji-kana "je" "じぇ" "ジェ")
(add-romaji-kana "jo" "じょ" "ジョ")
(add-romaji-kana "jya" "じゃ" "ジャ")
(add-romaji-kana "jyu" "じゅ" "ジュ")
(add-romaji-kana "jye" "じぇ" "ジェ")
(add-romaji-kana "jyo" "じょ" "ジョ")
(add-romaji-kana "da" "だ" "ダ")
(add-romaji-kana "di" "ぢ" "ヂ")
(add-romaji-kana "du" "づ" "ヅ")
(add-romaji-kana "de" "で" "デ")
(add-romaji-kana "do" "ど" "ド")
(add-romaji-kana "dya" "ぢゃ" "ヂャ")
(add-romaji-kana "dyi" "でぃ" "ディ")
(add-romaji-kana "dyu" "ぢゅ" "ヂュ")
(add-romaji-kana "dye" "ぢぇ" "ヂェ")
(add-romaji-kana "dyo" "ぢょ" "ヂョ")
(add-romaji-kana "ba" "ば" "バ")
(add-romaji-kana "bi" "び" "ビ")
(add-romaji-kana "bu" "ぶ" "ブ")
(add-romaji-kana "be" "べ" "ベ")
(add-romaji-kana "bo" "ぼ" "ボ")
(add-romaji-kana "va" "ヴぁ" "ヴァ")
(add-romaji-kana "vi" "ヴぃ" "ヴィ")
(add-romaji-kana "vu" "ヴ" "ヴ")
(add-romaji-kana "ve" "ヴぇ" "ヴェ")
(add-romaji-kana "vo" "ヴぉ" "ヴォ")
(add-romaji-kana "bya" "びゃ" "ビャ")
(add-romaji-kana "byu" "びゅ" "ビュ")
(add-romaji-kana "bye" "びぇ" "ビェ")
(add-romaji-kana "byo" "びょ" "ビョ")
(add-romaji-kana "pa" "ぱ" "パ")
(add-romaji-kana "pi" "ぴ" "ピ")
(add-romaji-kana "pu" "ぷ" "プ")
(add-romaji-kana "pe" "ぺ" "ペ")
(add-romaji-kana "po" "ぽ" "ポ")
(add-romaji-kana "pya" "ぴゃ" "ピャ")
(add-romaji-kana "pyu" "ぴゅ" "ピュ")
(add-romaji-kana "pye" "ぴぇ" "ピェ")
(add-romaji-kana "pyo" "ぴょ" "ピョ")
(add-romaji-kana "kwa" "くゎ" "クヮ")
(add-romaji-kana "kwi" "くぃ" "クィ")
(add-romaji-kana "kwu" "く" "ク")
(add-romaji-kana "kwe" "くぇ" "クェ")
(add-romaji-kana "kwo" "くぉ" "クォ")
(add-romaji-kana "gwa" "ぐゎ" "グヮ")
(add-romaji-kana "gwi" "ぐぃ" "グィ")
(add-romaji-kana "gwu" "ぐ" "グ")
(add-romaji-kana "gwe" "ぐぇ" "グェ")
(add-romaji-kana "gwo" "ぐぉ" "グォ")
(add-romaji-kana "tsa" "つぁ" "ツァ")
(add-romaji-kana "tsi" "つぃ" "ツィ")
(add-romaji-kana "tse" "つぇ" "ツェ")
(add-romaji-kana "tso" "つぉ" "ツォ")

(add-romaji-kana "xa" "ぁ" "ァ")
(add-romaji-kana "xi" "ぃ" "ィ")
(add-romaji-kana "xu" "ぅ" "ゥ")
(add-romaji-kana "xe" "ぇ" "ェ")
(add-romaji-kana "xo" "ぉ" "ォ")
(add-romaji-kana "xya" "ゃ" "ャ")
(add-romaji-kana "xyu" "ゅ" "ュ")
(add-romaji-kana "xyo" "ょ" "ョ")
(add-romaji-kana "xtu" "っ" "ッ")
(add-romaji-kana "xtsu" "っ" "ッ")
(add-romaji-kana "xwa" "ゎ" "ヮ")

(add-romaji-kana "xka" "ヵ" "ヵ")
(add-romaji-kana "xke" "ヶ" "ヶ")
(add-romaji-kana "xti" "てぃ" "ティ")
(add-romaji-kana "xdi" "でぃ" "ディ")
(add-romaji-kana "xdu" "どぅ" "ドゥ")
(add-romaji-kana "xde" "でぇ" "デェ")
(add-romaji-kana "xdo" "どぉ" "ドォ")
(add-romaji-kana "xwi" "うぃ" "ウィ")
(add-romaji-kana "xwe" "うぇ" "ウェ")
(add-romaji-kana "xwo" "うぉ" "ウォ")

)) ;; ics-target-case
