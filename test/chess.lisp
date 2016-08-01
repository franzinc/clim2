;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-USER; Base: 10; Lowercase: Yes -*-

;;
;;
;; See the file LICENSE for the full license governing this code.
;;


(in-package :clim-user)

(define-application-frame chess-board ()
    ((board :initform (make-array '(8 8)
				  :initial-contents
				  (make-chess-board-initial-state)))
     (bitmaps :initform nil :allocation :class)
     (subprocess :initform (create-chess-subprocess)))
  (:command-table chess-commands)
  (:panes
    (board :application
	   :incremental-redisplay '(t :check-overlapping nil)
	   :scroll-bars :dynamic
	   :width :compute :height :compute
	   :max-width :compute :max-height :compute
	   :display-function 'draw-chess-board)
    (status :application
	    :scroll-bars nil :height '(1 :line)))
  (:layouts
    (:default (vertically () status board))))

(defmethod update-status (frame message)
  (let ((pane (get-frame-pane frame 'status)))
    (window-clear pane)
    (with-text-face (pane :bold)
      (write-string message pane))))

(defmethod read-frame-command :before  ((frame chess-board) &key)
  (update-status frame "Your move"))

(define-presentation-type chess-square ())

(define-presentation-method highlight-presentation ((type chess-square) record stream state)
  state
  (multiple-value-bind (xoff yoff)
      (convert-from-relative-to-absolute-coordinates
	stream (output-record-parent record))
    (with-bounding-rectangle* (left top right bottom) record
      (draw-rectangle* stream
		       (- (+ left xoff) 2) (- (+ top yoff) 2)
		       (+ right xoff 2) (+ bottom yoff 2)
		       :line-thickness 2
		       :filled nil
		       :ink +flipping-ink+))))

(define-chess-board-command (com-do-nothing :menu t) ()
  nil)

(define-chess-board-command (com-reset :menu t) ()
  (with-slots (board subprocess) *application-frame*
    (send-command subprocess "new")
    (setf board
	  (make-array '(8 8)
		      :initial-contents
		      (make-chess-board-initial-state)))
    (window-clear *standard-output*)))

(define-chess-board-command com-move-piece
    ((from 'chess-square)
     (to 'chess-square))
  (with-slots (subprocess board) *application-frame*
    (let ((res
	    (send-move-to-chess-subprocess
	      subprocess
	      (encode-move from to))))
      (unless res
	(beep *standard-output*)
	(return-from com-move-piece nil))
      (setf (apply #'aref board to)
	    (apply #'aref board from)
	    (apply #'aref board from) nil)
      (redisplay-frame-pane *application-frame* (frame-standard-output *application-frame*))
      (update-status *application-frame* "My Move......")
      (let ((res (read-chess-move-from-subprocess subprocess)))
	(multiple-value-bind
	  (from to)
	    (decode-move res)
	  (setf (apply #'aref board to)
		(apply #'aref board from)
		(apply #'aref board from)
		nil))))))


(defun decode-move (move)
  (assert (= (length move) 4))
  (values
    (decode-position (subseq move 0 2))
    (decode-position (subseq move 2))))


(defun encode-move (from to)
  (concatenate 'string
    (encode-position from)
    (encode-position to)))

(defun encode-position (x)
  (destructuring-bind (row col) x
    (make-array 2
		:element-type 'character
		:initial-contents
		(list
		 (code-char (+ (char-int #\a) col))
		 (digit-char (- 8 row))))))

(defun decode-position (position)
  (list (- 8 (digit-char-p (aref position 1))) ;; row
	(- (char-int (aref position 0)) (char-int #\a)))) ;; column

(define-presentation-to-command-translator move-a-piece
    (chess-square com-move-piece chess-commands)
    (object)
  (list object *unsupplied-argument-marker*))

(defmethod draw-chess-board (frame stream &key &allow-other-keys)
  (stream-set-cursor-position stream 0 0)
  (formatting-table (stream)
    (dotimes (row 8)
      (formatting-row (stream)
	(dotimes (column 8)
	  (formatting-cell (stream)
	    (let ((x (aref (slot-value frame 'board) row column)))
	      (updating-output (stream
				 :unique-id (list row column)
				 :id-test #'equal
				 :cache-value x
				 :cache-test #'equal)
		(with-output-as-presentation (stream (list row column) 'chess-square)
		  (draw-piece frame
			      stream
			      (second x)
			      (car x)
			      (oddp (+ row column))))))))))))

(defmethod draw-piece (frame stream (which (eql nil)) color square)
  (draw-rectangle* stream 0 0 80 80
		   :ink (if square +black+ +white+)))


(defmethod draw-piece (frame stream which color square)
  (let* ((key (list which color square))
	 (ink (second (assoc key (slot-value frame 'bitmaps) :test #'equal))))
    (unless ink
      (setq ink (second
		  (car
		    (push (list key
				(xm-silica::make-pattern-from-file
				  (format nil
				      "~/3rd/gnuchess/Xchess/~a.bitmap"
				    which)
				  (list (if square +black+ +white+)
					(ecase color
					  (:white +red+)
					  (:black +green+)))))
			  (slot-value frame 'bitmaps))))))
    (draw-rectangle* stream 0 0 80 80
		     :ink ink)))

(defun make-chess-board-initial-state ()
  (labels ((define-pieces (color)
	     (ecase color
	       (:white
		 (list (define-pawns color)
		       (define-others color)))
	       (:black
		 (list (define-others color)
		       (define-pawns color)))))
	   (define-pawns (color)
	     (make-list 8 :initial-element (list color :pawn)))
	   (define-others (color)
	     (mapcar #'(lambda (x)
			 (list color x))
		     '(:rook :knight :bishop
			     :queen :king
			     :bishop :knight :rook))))
    (append (define-pieces :black)
	    (make-list 4 :initial-element (make-list 8))
	    (define-pieces :white))))


(defun create-chess-subprocess ()
  (multiple-value-bind (stream something pid)
      (excl::run-shell-command
	"~/3rd/gnuchess/gnuchessr"
	:wait nil
	:error-output :output
	:input :stream :output :stream)
    ;; skip "Chess" message
    (assert (string= (read-line stream) "Chess"))
    (write-line "beep" stream)
    (force-output stream)
    stream))

(defun match-prefix (line prefix)
  (string= line prefix :end1 (length prefix)))

(defun send-move-to-chess-subprocess (stream move)
  (send-command stream move)
  (read-confirmation stream))

(defun read-confirmation (stream)
  (loop
    (let ((line (read-a-line stream)))
      (cond ((digit-char-p (aref line 0))
	     ;; Move confirmation
	     (return-from read-confirmation t))
	    ((match-prefix line "Illegal move")
	     (return nil))
	    ((match-prefix line "warning:")
	     (warn "got this from chess: ~a" line))
	    (t
	     (cerror "ignore it" "Dunno what to do with this: ~A"
		     line))))))

(defun send-command (stream move)
  (format excl::*initial-terminal-io* "sent: ~A~%" move)
  (write-line move stream)
  (force-output stream))

(defun read-a-line (stream)
  (let ((line (read-line stream)))
    (setq line (delete #\^g line))
    (format excl::*initial-terminal-io* "received: ~A~%" line)
    line))

(defun read-chess-move-from-subprocess (stream)
  (loop (let ((line (read-a-line stream)))
	  (cond
	    ((digit-char-p (aref line 0)))
	    ((match-prefix line "My move is: ")
	     (return (subseq line 12)))
	    ((match-prefix line "warning:")
	     (warn "got this from chess: ~a" line))
	    (t
	     (cerror "ignore it" "Dunno what to do with this: ~A" line))))))
