;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

;;; The implementation layer constructs a modifier key mask to go along with each
;;; pointer gesture, e.g., Shift-Left.

;;; For now, we are making a few assumptions.
;;; 1) The set of possible modifier bits is :CONTROL, :SHIFT, :META, :SUPER, :HYPER, :DOUBLE
;;;    as per CLtL.
;;; 2) The set of possible "mouse buttons" is :LEFT, :MIDDLE, and :RIGHT.

(eval-when (compile load eval)

;; Button indices are 0, 1, or 2
(defun-inline button-index (name)
  #+Genera (declare lt:(side-effects simple reducible))
  (position name *pointer-buttons*))
(defun-inline button-index-name (index)
  (aref *pointer-buttons* index))

;; Modifier key indices are 0, 1, 2, 3, or 4
(defun-inline modifier-key-index (name)
  #+Genera (declare lt:(side-effects simple reducible))
  (position name *modifier-keys*))
(defun-inline modifier-key-index-name (index)
  (aref *modifier-keys* index))

;; Modifier states can be compared with =
(defun make-modifier-state (&rest modifiers)
  (declare (dynamic-extent modifiers))
  (assert (every #'(lambda (x) (find x *modifier-keys*)) modifiers) (modifiers)
          "~S is not a subset of ~S" modifiers '(:shift :control :meta :super :hyper :double))
  (let ((state 0))
    (dolist (name modifiers)
      (let ((bit (modifier-key-index name)))
        (setf state (dpb 1 (byte 1 bit) state))))
    state))

(define-compiler-macro make-modifier-state (&whole form &rest modifiers)
  (if (every #'constantp modifiers)
      (apply #'make-modifier-state modifiers)
      form))

)        ;eval-when

;; A table indexed by mouse button and modifier state [now (3 x 32)].
;; Each bucket in the table contains a sequence of gesture names.
(defvar *button-and-modifier-key->gesture*
        (make-array `(,(length *pointer-buttons*)
                      ,(ash 1 (length *modifier-keys*)))
                    :initial-element nil))

;; A table indexed by the modifier state.
;; Each bucket in the table contains an alist of a keysym and 1 or more gesture names.
(defvar *keysym-and-modifier-key->gesture*
        (make-array (ash 1 (length *modifier-keys*)) :initial-element nil))

;; BUTTON is a button number (0, 1, or 2), and MODIFIER-STATE is a mask
(defun-inline button-and-modifier-state-gesture-names (button modifier-state)
  (declare (type fixnum button modifier-state))
  (aref *button-and-modifier-key->gesture* button modifier-state))

;; BUTTON is a button number (0, 1, or 2), and MODIFIER-STATE is a mask
(defun-inline set-button-and-modifier-state-gesture-names (button modifier-state new-gestures)
  (setf (aref *button-and-modifier-key->gesture* button modifier-state)
        new-gestures))

(defsetf button-and-modifier-state-gesture-names set-button-and-modifier-state-gesture-names)

(defmacro do-button-and-modifier-state ((button-var modifier-var bucket-var) &body body)
  (let ((n-buttons '#:n-buttons)
        (n-masks '#:n-masks))
    `(let ((,n-buttons (array-dimension *button-and-modifier-key->gesture* 0))
           (,n-masks (array-dimension *button-and-modifier-key->gesture* 1)))
       (dotimes (,button-var ,n-buttons)
         (dotimes (,modifier-var ,n-masks)
           (let ((,bucket-var
                  (aref *button-and-modifier-key->gesture* ,button-var ,modifier-var)))
             ,@body))))))

(defun gesture-name-button-and-modifiers (gesture-name)
  (declare (values button modifier-state))
  (do-button-and-modifier-state (button modifier-state bucket)
    (when (member gesture-name bucket)
      (return-from gesture-name-button-and-modifiers
        (values (ash 1 (+ (1- (integer-length +pointer-left-button+)) button))
                modifier-state))))
  nil)

(defun gesture-name-keysym-and-modifiers (gesture-name)
  (declare (values keysym modifier-state))
  (dotimes (index (ash 1 (length *modifier-keys*)))
    (let ((bucket (aref *keysym-and-modifier-key->gesture* index)))
      (dolist (entry bucket)
        (when (member gesture-name (cdr entry))
          (return-from gesture-name-keysym-and-modifiers
            (values (car entry) index))))))
  nil)

#||
;;; Returns an alist of (button . gestures)
(defun gestures-for-modifier-state (state)
  ;; Eventually, cache this result because it's a common question
  (let ((n-buttons (array-dimension *button-and-modifier-key->gesture* 0))
        (return-value nil))
    (dotimes (b n-buttons)
      (let ((bucket (aref *button-and-modifier-key->gesture* b state)))
        (when bucket
          (push (cons (button-index-name b) bucket) return-value))))
    (nreverse return-value)))

(defun gestures-for-shift-names (&rest shift-names)
  (declare (dynamic-extent shift-names))
  (let ((state (apply #'make-modifier-state shift-name)))
    (gestures-for-modifier-state state)))
||#

;; We typically have our hands on a button index and a modifier key state,
;; and we need to know if it matches a named gesture
(defun-inline button-and-modifier-state-matches-gesture-name-p (button state gesture-name)
  (or (eq gesture-name 't)
      (member gesture-name (button-and-modifier-state-gesture-names button state))))

(defun modifier-state-matches-gesture-name-p (state gesture-name)
  ;; This could obviously be cached as well.
  (dotimes (i (length *pointer-buttons*))
    (when (button-and-modifier-state-matches-gesture-name-p i state gesture-name)
      (return-from modifier-state-matches-gesture-name-p t))))

(defun event-matches-gesture-name-p (event gesture-name &optional port)
  (etypecase event
    (pointer-button-event                ;--- POINTER-BUTTON-PRESS-EVENT?
      (button-press-event-matches-gesture-name-p event gesture-name port))
    (keyboard-event                        ;--- KEY-PRESS-EVENT?
      (keyboard-event-matches-gesture-name-p event gesture-name port))))

(defun button-press-event-matches-gesture-name-p (event gesture-name &optional port)
  #---ignore (declare (ignore port))
  #+++ignore (unless port
               (setq port (port (event-sheet event))))
  (let ((button (pointer-event-button event))
        (modifier-state (event-modifier-state event)))
    (declare (type fixnum button modifier-state))
    (button-and-modifier-state-matches-gesture-name-p
      (- (integer-length button) #.(integer-length +pointer-left-button+))
      modifier-state gesture-name)))

;; GESTURE-NAME either names a gesture, or is a canonicalized gesture spec
(defun keyboard-event-matches-gesture-name-p (event gesture-name &optional port)
  (declare (special *application-frame*))
  (when (and (characterp event)
             (characterp gesture-name))
    ;; Speedy exit when they're both characters
    (when (eql event gesture-name)
      (return-from keyboard-event-matches-gesture-name-p t))
    (when (and (ordinary-char-p event)
               (ordinary-char-p gesture-name))
      (return-from keyboard-event-matches-gesture-name-p
        (eql event gesture-name))))
  (multiple-value-bind (keysym modifier-state)
      (etypecase event
        (character
          (unless port
            ;; This is the best we can do...
            (setq port (port *application-frame*)))
          (values event (if (upper-case-p event)
                            (make-modifier-state :shift)
                          0)))
        (keyboard-event                        ;--- KEY-PRESS-EVENT?
          (unless port
            (setq port (port (event-sheet event))))
          (values (keyboard-event-key-name event)
                  (event-modifier-state event)))
        (end-of-file-marker
          (return-from keyboard-event-matches-gesture-name-p nil)))
    (declare (type fixnum modifier-state))
    (when (consp gesture-name)
      ;; If GESTURE-NAME is a cons, then it might really be a gesture spec
      (when (or (multiple-value-bind (gkeysym gstate)
                    (parse-gesture-spec gesture-name)
                  (and (eq keysym gkeysym)
                       (= modifier-state gstate)))
                (and port
                     (let ((gesture-spec 
                             (port-canonicalize-gesture-spec port gesture-name)))
                       (and gesture-spec
                            (or (and (eq keysym (car gesture-spec))
                                     (= modifier-state (cdr gesture-spec)))
                                (equal gesture-spec
                                       (port-canonicalize-gesture-spec 
                                         port keysym modifier-state)))))))
        (return-from keyboard-event-matches-gesture-name-p t))
      (when (null (cdr gesture-name))
        ;; On the other hand, it might be the user messing with us and giving
        ;; us a bogus keysym disguised as a gesture spec
        (setq gesture-name (car gesture-name))))
    (unless (consp gesture-name)
      (or (let ((bucket (aref *keysym-and-modifier-key->gesture* modifier-state)))
            (member gesture-name (cdr (assoc keysym bucket))))
          (let ((gesture-spec 
                  (port-canonicalize-gesture-spec port keysym modifier-state)))
            (and gesture-spec
                 #+(or aclpc acl86win32)
                 (let ((keysym (car gesture-spec))
                       (modifier-state (cdr gesture-spec)))
;;;  problem with: 
;;;       destructuring-bind (keysym . modifier-state) gesture-spec
                   (let ((bucket (aref *keysym-and-modifier-key->gesture*
                                       modifier-state)))
		     (setq modifier-state (port-modifier-state (find-port)))
                     (member gesture-name (cdr (assoc keysym bucket)))))
                 #-(or aclpc acl86win32)
                 (destructuring-bind (keysym . modifier-state) gesture-spec
                   (let ((bucket (aref *keysym-and-modifier-key->gesture*
                                       modifier-state)))
                     (member gesture-name (cdr (assoc keysym bucket)))))))))))

(defun-inline keyboard-event-p (x)
  (or (characterp x)
      (typep x 'key-press-event)))

(defun keyboard-gesture-spec-p (x)
  (multiple-value-bind (keysym modifiers) (parse-gesture-spec x)
    (declare (ignore modifiers))
    (and keysym 
         (not (find keysym *pointer-buttons*)))))

(defun gesture-spec-eql (gesture1 gesture2)
  (multiple-value-bind (k1 m1) (parse-gesture-spec gesture1)
    (multiple-value-bind (k2 m2) (parse-gesture-spec gesture2)
      (and (or (eql k1 k2)
               (and (characterp k1)
                    (characterp k2)
                    (char-equal k1 k2)))
           (= m1 m2)))))

(defun parse-gesture-spec (gesture-spec)
  (declare (values keysym modifier-state))
  (when (atom gesture-spec)
    (return-from parse-gesture-spec
      (if (and (characterp gesture-spec)
               (upper-case-p gesture-spec))
          (values gesture-spec (make-modifier-state :shift))
        (values gesture-spec 0))))
  (when (and (consp gesture-spec)
             (integerp (cdr gesture-spec)))
    (return-from parse-gesture-spec
      (values (car gesture-spec) (cdr gesture-spec))))
  (let ((keysym nil)
        (modifier-state 0))
    (dolist (x gesture-spec)
      (if (find x *modifier-keys*)
          (let ((bit (modifier-key-index x)))
            (setf modifier-state (dpb 1 (byte 1 bit) modifier-state)))
        (setq keysym (or keysym x))))
    (when (and (characterp keysym)
               (upper-case-p keysym))
      (setq modifier-state (logior modifier-state (make-modifier-state :shift))))
    (values keysym modifier-state)))

;; A slower, more careful version of the above that gets used to
;; validate programmer input
(defun decode-gesture-spec (gesture-spec &key (errorp t))
  (declare (values keysym modifiers))
  (when (atom gesture-spec)
    (return-from decode-gesture-spec
      (values gesture-spec nil)))
  (let ((keysym nil)
        (modifiers nil))
    (dolist (x gesture-spec)
      (cond ((find x *modifier-keys*)
             (if (member x modifiers)
                 (if errorp
                     (cerror "Ignore the extra modifier"
                             "The modifier ~S appears more than once in the gesture spec ~S"
                             x gesture-spec)
                     (return-from decode-gesture-spec nil))
                 (push x modifiers)))
            (keysym
             (if errorp
                 (error "The gesture spec ~S is invalid" gesture-spec)
                 (return-from decode-gesture-spec nil)))
            (t
             (setq keysym x))))
    (values keysym (nreverse modifiers))))

#-(or aclpc acl86win32)
(defmethod port-canonicalize-gesture-spec :around 
           ((port basic-port) gesture-spec &optional modifier-state)
  (multiple-value-bind (keysym modifier-state) 
      (if modifier-state
          (values gesture-spec modifier-state)
          (parse-gesture-spec gesture-spec))
    (with-stack-list (key keysym modifier-state)
      (let ((table (port-canonical-gesture-specs port)))
        (when table
          (multiple-value-bind (value found-p) (gethash key table)
            (if found-p
                value
                (setf (gethash (evacuate-list key) table)
                      (call-next-method port keysym modifier-state)))))))))

;; Needed for (sigh) string streams.  Hardly ever used...
(defmethod port-canonicalize-gesture-spec 
           ((port t) gesture-spec &optional modifier-state)
  (multiple-value-bind (keysym shifts)
      (if modifier-state
          (values gesture-spec modifier-state)
          (parse-gesture-spec gesture-spec))
    (let ((entry (assoc keysym '((#\Return . :return)
                                 (#\Newline . :newline)
                                 (#\Tab . :tab)
                                 (#\Rubout . :rubout)
                                 (#\Backspace . :backspace)
                                 (#\Page . :page)
                                 #+Genera (#\Line . :linefeed)
                                 #-Genera (#\Linefeed . :linefeed)
                                 (#\Escape . :escape)))))
      (cond (entry
             (setq keysym (cdr entry)))
            ((and (characterp keysym)
                  (standard-char-p keysym)) 
             (setq keysym (svref #(:space :\! :\" :\# :\$ :\% :\& :\'
                                   :\( :\) :\* :\+ :\, :\- :\. :\/
                                   :\0 :\1 :\2 :\3 :\4 :\5 :\6 :\7 :\8 :\9
                                   :\: :\; :\< :\= :\> :\? :\@
                                   :a :b :c :d :e :f :g :h :i :j :k :l :m
                                   :n :o :p :q :r :s :t :u :v :w :x :y :z
                                   :\[ :\\ :\] :\^ :\_ :\`
                                   :a :b :c :d :e :f :g :h :i :j :k :l :m
                                   :n :o :p :q :r :s :t :u :v :w :x :y :z
                                   :\{ :\| :\} :\~)
                                 (- (char-code keysym) #.(char-code #\space)))))))
    (cons keysym shifts)))

#+(or aclpc acl86win32) ; unqualified first
(defmethod port-canonicalize-gesture-spec :around 
           ((port basic-port) gesture-spec &optional modifier-state)
  (multiple-value-bind (keysym modifier-state) 
      (if modifier-state
          (values gesture-spec modifier-state)
          (parse-gesture-spec gesture-spec))
    (with-stack-list (key keysym modifier-state)
      (let ((table (port-canonical-gesture-specs port)))
        (when table
          (multiple-value-bind (value found-p) (gethash key table)
            (if found-p
                value
                (setf (gethash (evacuate-list key) table)
                      (call-next-method port keysym modifier-state)))))))))


(defmethod port-canonical-gesture-specs ((port t))
  nil)

(defmethod port-invalidate-gesture-specs ((port basic-port))
  (let ((table (port-canonical-gesture-specs port)))
    (when table
      (clrhash table))))


(defvar *modifier-state-specifiers* nil)

(defun modifier-state-keys (state)
  (when (null *modifier-state-specifiers*)
    ;; Fill the cache if it hasn't been done yet
    (setq *modifier-state-specifiers* (make-array (ash 1 (length *modifier-keys*))))
    (dotimes (state (ash 1 (length *modifier-keys*)))
      (let ((shifts nil))
        (dotimes (bit (length *modifier-keys*))
          (let ((name (modifier-key-index-name bit)))
            (when (ldb-test (byte 1 bit) state)
              (push name shifts))))
        (setf (aref *modifier-state-specifiers* state) shifts))))
  (aref *modifier-state-specifiers* state))

(defun gesture-specs-from-gesture-name (gesture-name)
  (let ((gesture-specs nil))
    (do-button-and-modifier-state (button modifier-state bucket)
      (when (member gesture-name bucket)
        (push (list* (button-index-name button) (modifier-state-keys modifier-state))
              gesture-specs)))
    (dotimes (modifier-state (ash 1 (length *modifier-keys*)))
      (let ((bucket (aref *keysym-and-modifier-key->gesture* modifier-state)))
        (dolist (entry bucket)
          (when (member gesture-name (cdr entry))
            (push (list* (car entry) (modifier-state-keys modifier-state))
                  gesture-specs)))))
    (nreverse gesture-specs)))

(defun describe-gesture-spec (gesture-spec
                              &key (stream *standard-output*) (brief t))
  (let ((alist (if brief 
                   '((:hyper   "h-")
                     (:super   "s-")
                     (:meta    "m-")
                     (:control "c-")
                     (:shift   "sh-")
		     (:double  "dbl-"))
                   '((:hyper   "hyper-")
                     (:super   "super-")
                     (:meta    "meta-")
                     (:control "control-")
                     (:shift   "shift-")
		     (:double "double-")))))
    (dolist (shift alist)
      (when (member (first shift) (rest gesture-spec))
        (princ (second shift) stream)))
    (let ((keysym (first gesture-spec)))
      (when (and (characterp keysym)
                 (char<= #\A keysym #\A)
                 (not (member :shift (rest gesture-spec))))
        (princ (second (assoc :shift alist)) stream))
      (when (and (not brief)
                 (find keysym *pointer-buttons*))
        (princ "Mouse-" stream))
      (format stream "~:(~A~)" keysym))))

#+++ignore
(defun print-modifier-state (state stream &optional brief-p)
  (let ((n (length *modifier-keys*)))
    (dotimes (i n)
      (let ((bit (- n i 1)))
        (let ((name (modifier-key-index-name bit)))
          (when (ldb-test (byte 1 bit) state)
            (if brief-p
                (format stream "~A-"
                  (ecase name
                    (:shift "sh")
                    (:control "c")
                    (:meta "m")
                    (:super "s")
                    (:hyper "h")
		    (:double "dbl")))
              (format stream "~:(~A-~)" name))))))))


(defun add-gesture-name (name type gesture-spec &key (unique t))
  (check-type name symbol)
  (when unique
    (delete-gesture-name name))
  (ecase type
    (:keyboard
      (multiple-value-bind (key-name modifiers)
          (decode-gesture-spec gesture-spec)
        (check-type key-name (or symbol character))
        (let* ((index (apply #'make-modifier-state modifiers))
               (bucket (aref *keysym-and-modifier-key->gesture* index))
               (entry (assoc key-name bucket)))
          (cond (entry
                 (unless (member name (cdr entry))
                   (setf (aref *keysym-and-modifier-key->gesture* index)
                         (delete nil
                                 (nsubstitute (append entry (list name)) entry bucket)))))
                (t
                 (setq entry (list key-name name))
                 (setf (aref *keysym-and-modifier-key->gesture* index)
                       (delete nil
                               (append (aref *keysym-and-modifier-key->gesture* index)
                                       (list entry))))))
          bucket)))
    (:pointer-button
      (multiple-value-bind (button modifiers)
          (decode-gesture-spec gesture-spec)
        (check-type button (member :left :middle :right))
        (pushnew name (button-and-modifier-state-gesture-names
                        (button-index button)
                        (apply #'make-modifier-state modifiers)))))))

(defun delete-gesture-name (name)
  (do-button-and-modifier-state (button modifier-state bucket)
    (when (member name bucket)
      (setf (aref *button-and-modifier-key->gesture* button modifier-state)
            (delete name bucket))))
  (dotimes (index (ash 1 (length *modifier-keys*)))
    (let ((bucket (aref *keysym-and-modifier-key->gesture* index)))
      (do* ((entryl bucket (cdr entryl))
            (entry (first entryl) (first entryl)))
           ((null entryl))
        (when (member name (cdr entry))
          (setf (first entryl) (delete name entry)))))))

(defmacro define-gesture-name (name type gesture-spec &key (unique t))
  (setf (compile-time-property name 'gesture-name) t)
  `(add-gesture-name ',name ',type ',gesture-spec :unique ',unique))


;;; Canonical pointer gestures

(define-gesture-name :select   :pointer-button (:left))
(define-gesture-name :describe :pointer-button (:middle))
(define-gesture-name :menu     :pointer-button (:right))
(define-gesture-name :delete   :pointer-button (:middle :shift))
(define-gesture-name :edit     :pointer-button (:left :meta))


;;; Canonical keyboard gestures

(define-gesture-name :return    :keyboard (:return))
(define-gesture-name :newline   :keyboard (:newline))
(define-gesture-name :tab       :keyboard (:tab))
(define-gesture-name :rubout    :keyboard (:rubout))
(define-gesture-name :backspace :keyboard (:backspace))
(define-gesture-name :page      :keyboard (:page))
(define-gesture-name :line      :keyboard (:line))
(define-gesture-name :escape    :keyboard (:escape))

(define-gesture-name :select      :keyboard (:select) :unique nil)
(define-gesture-name :insert      :keyboard (:insert))
(define-gesture-name :home        :keyboard (:home))
(define-gesture-name :end         :keyboard (:end))
(define-gesture-name :abort       :keyboard #+Genera (:abort)
                                            #+Cloe-Runtime (:escape)
                                            #-(or Genera Cloe-Runtime) (:z :control))
(define-gesture-name :help        :keyboard (:help))
(define-gesture-name :complete    :keyboard (:complete)) ; not X
(define-gesture-name :scroll      :keyboard (:scroll)) ; not X
(define-gesture-name :scroll-up   :keyboard (:scroll-up)) ; aka prev, page up
(define-gesture-name :refresh     :keyboard (:refresh)) ; not X
(define-gesture-name :clear-input :keyboard (:clear-input)) ; not X

#-Genera
(define-gesture-name :asynchronous-abort :keyboard #+Cloe-Runtime (:escape :control)
                                                   #-Cloe-Runtime (:z :control :meta))


;;--- Both of these because of a bug in KEYBOARD-EVENT-MATCHES-GESTURE-NAME-P
;;--- that causes control-? not to match sometimes
(define-gesture-name :possibilities :keyboard (:? :control))
(define-gesture-name :possibilities :keyboard (:? :control :shift) :unique nil)
(define-gesture-name :apropos-possibilities :keyboard (:/ :control))

#+Genera (define-gesture-name :suspend :keyboard (:suspend))
#+Genera (define-gesture-name :resume  :keyboard (:resume))

;; As a convenience, all of the keysyms corresponding to ordinary characters
;; have themselves as gesture names
(define-gesture-name :space :keyboard :space)
(define-gesture-name :\! :keyboard :\!)
(define-gesture-name :\" :keyboard :\")
(define-gesture-name :\# :keyboard :\#)
(define-gesture-name :\$ :keyboard :\$)
(define-gesture-name :\% :keyboard :\%)
(define-gesture-name :\& :keyboard :\&)
(define-gesture-name :\' :keyboard :\')
(define-gesture-name :\( :keyboard :\()
(define-gesture-name :\) :keyboard :\))
(define-gesture-name :\* :keyboard :\*)
(define-gesture-name :\+ :keyboard :\+)
(define-gesture-name :\, :keyboard :\,)
(define-gesture-name :\- :keyboard :\-)
(define-gesture-name :\. :keyboard :\.)
(define-gesture-name :\/ :keyboard :\/)
(define-gesture-name :\0 :keyboard :\0)
(define-gesture-name :\1 :keyboard :\1)
(define-gesture-name :\2 :keyboard :\2)
(define-gesture-name :\3 :keyboard :\3)
(define-gesture-name :\4 :keyboard :\4)
(define-gesture-name :\5 :keyboard :\5)
(define-gesture-name :\6 :keyboard :\6)
(define-gesture-name :\7 :keyboard :\7)
(define-gesture-name :\8 :keyboard :\8)
(define-gesture-name :\9 :keyboard :\9)
(define-gesture-name :\: :keyboard :\:)
(define-gesture-name :\; :keyboard :\;)
(define-gesture-name :\< :keyboard :\<)
(define-gesture-name :\= :keyboard :\=)
(define-gesture-name :\> :keyboard :\>)
(define-gesture-name :\? :keyboard :\?)
(define-gesture-name :\@ :keyboard :\@)
(define-gesture-name :a  :keyboard :a)
(define-gesture-name :b  :keyboard :b)
(define-gesture-name :c  :keyboard :c)
(define-gesture-name :d  :keyboard :d)
(define-gesture-name :e  :keyboard :e)
(define-gesture-name :f  :keyboard :f)
(define-gesture-name :g  :keyboard :g)
(define-gesture-name :h  :keyboard :h)
(define-gesture-name :i  :keyboard :i)
(define-gesture-name :j  :keyboard :j)
(define-gesture-name :k  :keyboard :k)
(define-gesture-name :l  :keyboard :l)
(define-gesture-name :m  :keyboard :m)
(define-gesture-name :n  :keyboard :n)
(define-gesture-name :o  :keyboard :o)
(define-gesture-name :p  :keyboard :p)
(define-gesture-name :q  :keyboard :q)
(define-gesture-name :r  :keyboard :r)
(define-gesture-name :s  :keyboard :s)
(define-gesture-name :t  :keyboard :t)
(define-gesture-name :u  :keyboard :u)
(define-gesture-name :v  :keyboard :v)
(define-gesture-name :w  :keyboard :w)
(define-gesture-name :x  :keyboard :x)
(define-gesture-name :y  :keyboard :y)
(define-gesture-name :z  :keyboard :z)
(define-gesture-name :\[ :keyboard :\[)
(define-gesture-name :\\ :keyboard :\\)
(define-gesture-name :\] :keyboard :\])
(define-gesture-name :\^ :keyboard :\^)
(define-gesture-name :\_ :keyboard :\_)
(define-gesture-name :\` :keyboard :\`)
(define-gesture-name :\{ :keyboard :\{)
(define-gesture-name :\| :keyboard :\|)
(define-gesture-name :\} :keyboard :\})
(define-gesture-name :\~ :keyboard :\~)
