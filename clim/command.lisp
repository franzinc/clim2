;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;;; Command objects

(defun-inline command-name (command) (first command))

(defun-inline command-arguments (command) (rest command))

(defvar *unsupplied-argument-marker* '#:unsupplied-argument)
(defvar *numeric-argument-marker*    '#:numeric-argument)
(defvar *application-frame-marker* '#:application-frame)

(defun unsupplied-argument-p (arg)
  (eq arg *unsupplied-argument-marker*))

(defun partial-command-p (command)
  (and (listp command)
       (member *unsupplied-argument-marker* (command-arguments command))))

(defun substitute-numeric-argument-marker (command numeric-arg)
  (if (member *numeric-argument-marker* command)
      (substitute numeric-arg *numeric-argument-marker* command)
      command))


;;; Command tables

;; Registry of command tables
(defvar *all-command-tables* (make-hash-table :test #'eql))

(defvar *command-table-size* 25)

(defvar *completion-cache-tick* 0)

(define-protocol-class command-table ())

;; NAME is a symbol that names the command table.
;; INHERIT-FROM is a list of command tables from which this one inherits.
;; COMMANDS is a table of all the commands in this command table, which maps from the
;; command name to the command-line name.
;; COMMAND-LINE-NAMES is a sorted set that maps from command-line names to command names.
;; MENU is a mapping from menu names and accelerator gestures to menu items, stored
;; in the form (MENU-NAME KEYSTROKE COMMAND-MENU-ITEM), where the menu item is of the
;; form (TYPE VALUE . OPTIONS).  MENU-TICK is incremented when MENU changes.
;; TRANSLATORS is the set of presentation translators for this command table.
(defclass standard-command-table
          (command-table)
    ((name :reader command-table-name
           :initarg :name)
     (inherit-from :reader command-table-inherit-from
                   :initarg :inherit-from)
     (commands :initform (make-hash-table :size *command-table-size*))
     (command-line-names :initform nil)
     (completion-alist :initform nil)
     (completion-alist-tick :initform 0)
     (menu :initform nil)
     (menu-tick :initform 0)
     (keystrokes :initform nil)                        ;cache for keystroke accelerators
     (translators :initform nil)
     ;; For the time being, the translator cache is keyed on from-type->to-type.
     ;; Each bucket of the hash table contains a set of translators matching
     ;; matching from-type->to-type.  It may very well make sense to define a
     ;; different storage mechanism.
     (translators-cache :initform nil)))

(defmethod print-object ((command-table command-table) stream)
  (print-unreadable-object (command-table stream :type t :identity t)
    (write (command-table-name command-table) :stream stream :escape nil)))

(defmethod (setf command-table-inherit-from)
           (new-inherit-from (command-table standard-command-table))
  (with-slots (inherit-from completion-alist translators-cache) command-table
    (unless (eq new-inherit-from inherit-from)
      (setq inherit-from new-inherit-from)
      (setq completion-alist nil)
      (when translators-cache (clrhash translators-cache)))))

(define-condition command-table-error (error)
  ((format-string :reader command-table-error-format-string :initarg :format-string)
   (format-args :reader command-table-error-format-args :initarg :format-args))
  (:report (lambda (condition stream)
             (apply #'format stream (command-table-error-format-string condition)
                                    (command-table-error-format-args condition)))))

(define-condition command-table-not-found (command-table-error) ())

(define-condition command-table-already-exists (command-table-error) ())

(defun find-command-table (name &key (errorp t))
  (cond ((command-table-p name)
         name)
        ((symbolp name)
         (or (gethash name *all-command-tables*)
             (ecase errorp
               ((t)
                (error 'command-table-not-found
                       :format-string "Command table ~S not found"
                       :format-args (list name)))
               ((nil)
                nil))))
        (t
	 (error "~S is not a symbol or command table" name)))) ; spr25469


(defun make-command-table (name &key inherit-from menu inherit-menu (errorp t))
  (check-type name symbol)
  (check-type inherit-from list)
  (let ((command-table (find-command-table name :errorp nil)))
    (when command-table
      (ecase errorp
        ((t)
         (cerror "Remove the command table and proceed"
                 'command-table-already-exists
                 :format-string "Command table ~S already exists"
                 :format-args (list name))
         (remhash name *all-command-tables*))
        ((nil)
         (remhash name *all-command-tables*))))
    (setq command-table (make-instance 'standard-command-table
                          :name name
                          :inherit-from inherit-from))
    (process-command-table-menu command-table menu inherit-menu)
    (setf (gethash name *all-command-tables*) command-table)
    command-table))

(defun process-command-table-menu (command-table menu inherit-menu)
  (check-type menu list)
  (check-type inherit-menu (member nil t :menu :keystrokes))
  (when inherit-menu
    (dolist (comtab (command-table-inherit-from command-table))
      (let* ((comtab (find-command-table comtab :errorp nil))
             (menu (and comtab (slot-value comtab 'menu))))
        (when menu
          (dovector (element menu)
            (destructuring-bind (string keystroke (type value &rest options)) element
              (when (case inherit-menu
                      (:menu (setq keystroke nil)
                             t)
                      (:keystrokes (setq string nil)
                                   keystroke)
                      (otherwise t))
                (apply #'add-menu-item-to-command-table
                       command-table string type value
                       :keystroke keystroke :errorp nil
                       options))))))))
  (dolist (item menu)
    (let* ((string (pop item))
           (type (pop item))
           (value (pop item))
           (options item))
      (apply #'add-menu-item-to-command-table
             command-table string type value
             :errorp nil
             options))))

;; CLIM's global command table
;; Things like the Mouse-Right (menu) translator live here.
(defvar *global-command-table* (make-command-table 'global-command-table :inherit-from nil))

(defmacro define-command-table (name &key inherit-from (menu nil menu-p) inherit-menu)
  #+Genera (declare (zwei:indentation 1 1))
  (setf (compile-time-property name 'command-table-name) t)
  `(define-group ,name define-command-table
     (define-command-table-1 ',name
         :inherit-from ',inherit-from
         ,@(and menu-p `(:menu ',menu))
         :inherit-menu ,inherit-menu)))

(defun remove-command-table (name)
  ;; This could hack inheritance, too...
  (remhash name *all-command-tables*))

#+Genera
(progn
  (scl:defprop define-command-table "CLIM Command Table" si:definition-type-name)
  (scl:defprop define-command-table remove-command-table zwei:kill-definition))

(defun define-command-table-1 (name &key inherit-from (menu nil menu-p) inherit-menu)
  (check-type name symbol)
  (check-type inherit-from list)
  (when (null inherit-from)
    (setq inherit-from (list (command-table-name *global-command-table*))))
  (let ((old-command-table (find-command-table name :errorp nil)))
    (cond (old-command-table
           (setf (command-table-inherit-from old-command-table) inherit-from)
           (when menu-p
             ;; Only discard the old menu if a new one was explicitly
             ;; supplied.  This keeps us from throwing away menu items
             ;; that the user asked for via :MENU to DEFINE-COMMAND.
             (setf (slot-value old-command-table 'menu) nil))
           (process-command-table-menu old-command-table menu inherit-menu)
           (setf (slot-value old-command-table 'keystrokes) nil)
           old-command-table)
          (t
           (make-command-table name :inherit-from inherit-from
                                    :menu menu :inherit-menu inherit-menu)))))

;; CLIM's general "user" command table
(define-command-table user-command-table :inherit-from (global-command-table))


;;; Adding and removing commands, inheritance

(define-condition command-not-present (command-table-error) ())

(define-condition command-already-present (command-table-error) ())

(define-condition command-not-accessible (command-table-error) ())

;; SYMBOL better be a symbol, because we're gonna bash the string (twice)...
(defun command-name-from-symbol (symbol)
  (let ((name (remove-word-from-string "COM-" symbol t)))
    (nstring-capitalize (nsubstitute #\Space #\- name))))

(defun add-command-to-command-table (command-name command-table
                                     &key name menu keystroke (errorp t))
  (check-type command-name symbol)
  (setq command-table (find-command-table command-table))
  (when (eq name t)
    (setq name (command-name-from-symbol command-name)))
  (check-type name (or string null))
  (when keystroke
    (assert (keyboard-gesture-spec-p keystroke) (keystroke)
            "~S is not a keyboard gesture spec" keystroke))
  (when (command-present-in-command-table-p command-name command-table)
    (when errorp
      (cerror "Remove the command and proceed"
              'command-already-present
              :format-string "Command ~S already present in ~S"
              :format-args (list command-name command-table)))
    (remove-command-from-command-table command-name command-table))
  (let ((menu-name nil)
        (menu-options nil))
    (when menu
      (setq menu-name (if (consp menu) (first menu) menu))
      (when (eq menu-name t)
        (setq menu-name (or name (command-name-from-symbol command-name))))
      (check-type menu-name string)
      (setq menu-options (if (consp menu) (rest menu) nil)))
    (with-slots (commands) command-table
      (if name
          (add-command-line-name-to-command-table command-table name command-name)
          (setf (gethash command-name commands) t))
      (cond (menu
             (apply #'add-menu-item-to-command-table
                    command-table menu-name ':command command-name
                    :keystroke keystroke :errorp errorp
                    menu-options))
            (keystroke
             (add-keystroke-to-command-table command-table keystroke ':command command-name
                                             :errorp errorp)))))
  command-name)

(defun remove-command-from-command-table (command-name command-table &key (errorp t))
  (check-type command-name symbol)
  (setq command-table (find-command-table command-table))
  (unless (command-present-in-command-table-p command-name command-table)
    (when errorp
      (cerror "Proceed without any special action"
              'command-not-present
              :format-string "Command ~S not present in ~S"
              :format-args (list command-name command-table))
      (return-from remove-command-from-command-table)))
  ;; This knows too much about what COMMAND-LINE-NAMES looks like
  (let ((names (slot-value command-table 'command-line-names)))
    (loop
      (let ((index (position command-name names :key #'second)))
        (cond (index
               (unless (= (1+ index) (fill-pointer names))
                 (replace names names :start1 index :start2 (1+ index)))
               (decf (fill-pointer names))
               (incf *completion-cache-tick*))
              (t (return))))))
  ;; And this knows too much about what MENU looks like, but what the heck
  (let ((menu (slot-value command-table 'menu))
        (menu-items nil))
    (map nil #'(lambda (entry)
                 (let ((item (third entry)))
                   (when (and (eq (command-menu-item-type item) :command)
                              (eq (command-name (command-menu-item-value item)) command-name))
                     (push item menu-items))))
         menu)
    (dolist (item menu-items)
      (let ((index (position item menu :key #'third)))
        (unless (= (1+ index) (fill-pointer menu))
          (replace menu menu :start1 index :start2 (1+ index)))
        (decf (fill-pointer menu))
        (incf (slot-value command-table 'menu-tick)))))
  (remhash command-name (slot-value command-table 'commands)))

(defmacro do-command-table-inheritance ((command-table-var command-table) &body body)
  `(flet ((do-command-table-inheritance-body (,command-table-var)
            ,@body))
     (declare (dynamic-extent #'do-command-table-inheritance-body))
     (do-command-table-inheritance-1
       #'do-command-table-inheritance-body
       (find-command-table ,command-table))))

;;--- This should be careful to hit each command table only once
(defun do-command-table-inheritance-1 (function command-table)
  (funcall function command-table)
  (dolist (comtab (command-table-inherit-from command-table))
    (do-command-table-inheritance-1 function (find-command-table comtab))))

(defmethod command-present-in-command-table-p
           (command-name (command-table standard-command-table))
  (gethash command-name (slot-value command-table 'commands)))

(defun command-accessible-in-command-table-p (command-name command-table)
  (declare (values command-table))
  (do-command-table-inheritance (comtab command-table)
    (when (command-present-in-command-table-p command-name comtab)
      (return-from command-accessible-in-command-table-p comtab)))
  nil)

(defun map-over-command-table-commands (function command-table &key (inherited t))
  (declare (dynamic-extent function))
  (if inherited
      (do-command-table-inheritance (comtab command-table)
        (maphash #'(lambda (command-name command-line-name)
                     (declare (ignore command-line-name))
                     (funcall function command-name))
                 (slot-value comtab 'commands)))
    (maphash #'(lambda (command-name command-line-name)
                 (declare (ignore command-line-name))
                 (funcall function command-name))
             (slot-value (find-command-table command-table) 'commands))))

(defun add-command-line-name-to-command-table (command-table name command-name)
  (setq command-table (find-command-table command-table))
  (with-slots (command-line-names commands) command-table
    (when (null command-line-names)
      (setq command-line-names (make-array *command-table-size*
                                           :fill-pointer 0 :adjustable t)))
    ;; Make the command directly accessible in the command table, but
    ;; don't change the primary name if there already is one
    (unless (stringp (gethash command-name commands))
      (setf (gethash command-name commands) name))
    ;;--- Use binary insertion on COMMAND-LINE-NAMES (completion aarrays)
    (remove-command-line-name-from-command-table command-table name)
    (vector-push-extend (list name command-name) command-line-names)
    #-acl86win32
    (setq command-line-names (sort command-line-names #'string-lessp :key #'first))
    #+acl86win32
    (let ((newcts (sort command-line-names #'string-lessp :key #'first)))
      (setq command-line-names 
        (make-array (length newcts) 
                    :fill-pointer (fill-pointer command-line-names)
                    :adjustable t
                    :initial-contents newcts)))
    (incf *completion-cache-tick*)))

(defun remove-command-line-name-from-command-table (command-table name)
  (setq command-table (find-command-table command-table))
  (with-slots (command-line-names commands) command-table
    ;;--- Use binary deletion on COMMAND-LINE-NAMES (completion aarrays)
    (let ((index (position name command-line-names
                           :test #'string-equal :key #'first)))
      (when index
        (let ((command-name (second (aref command-line-names index))))
          (when (string-equal name (gethash command-name commands))
            ;; Remove the primary command-line name for this command,
            ;; but don't claim that the command does not exist
            (setf (gethash command-name commands) t)))
        (unless (= (1+ index) (fill-pointer command-line-names))
          (replace command-line-names command-line-names :start1 index :start2 (1+ index)))
        (decf (fill-pointer command-line-names))
        (incf *completion-cache-tick*)))))

(defun map-over-command-table-names (function command-table &key (inherited t))
  (declare (dynamic-extent function))
  (if inherited
      (do-command-table-inheritance (comtab command-table)
        (map nil #'(lambda (entry)
                     (funcall function (first entry) (second entry)))
             (slot-value comtab 'command-line-names)))
    (map nil #'(lambda (entry)
                 (funcall function (first entry) (second entry)))
         (slot-value (find-command-table command-table) 'command-line-names))))

(defun find-command-from-command-line-name (name command-table &key (errorp t))
  (declare (values command command-table))
  (do-command-table-inheritance (comtab command-table)
    ;;--- Use binary search on COMMAND-LINE-NAMES (completion aarrays)
    (let ((item (find name (slot-value comtab 'command-line-names)
                      :test #'string-equal :key #'first)))
      (when item
        (return-from find-command-from-command-line-name
          (values (second item) comtab)))))
  (when errorp
    (error 'command-not-accessible
           :format-string "Command-line name ~S has no command accessible in ~S"
           :format-args (list name command-table))))

(defun command-line-name-for-command (command-name command-table &key (errorp t))
  (do-command-table-inheritance (comtab command-table)
    (let ((command-line-name (gethash command-name (slot-value comtab 'commands))))
      (when (stringp command-line-name)
        (return-from command-line-name-for-command command-line-name))))
  (case errorp
    ((nil) nil)
    ((:create)
     (command-name-from-symbol command-name))
    (otherwise
      (error 'command-not-accessible
             :format-string "Command ~S has no command-line name accessible in ~S"
             :format-args (list command-name command-table)))))


;;; Command table menus

(defun-inline command-menu-item-type (item) (first item))

(defun-inline command-menu-item-value (item) (second item))

(defun-inline command-menu-item-options (item) (rest (rest item)))

;; Menu names can be NIL, but we don't want that to match the string "NIL"
(defun menu-name-equal (s1 s2)
  (and s1 s2 (string-equal s1 s2)))

(defun add-menu-item-to-command-table (command-table string type value
                                       &key documentation (after ':end)
                                            keystroke mnemonic accelerator-text
                                            text-style button-type (errorp t))
  (check-type string (or string null))
  (check-type type (member :command :function :menu :divider))
  (when keystroke
    (assert (keyboard-gesture-spec-p keystroke) (keystroke)
            "~S is not a keyboard gesture spec" keystroke)
    (multiple-value-bind (keysym modifiers)
        (decode-gesture-spec keystroke)
      (setq keystroke (cons keysym modifiers))))
  (check-type documentation (or string null))
  (setq command-table (find-command-table command-table))
  (let ((old-item (and string (find-menu-item string command-table :errorp nil))))
    (when old-item
      (when errorp
        (cerror "Remove the menu item and proceed"
                'command-already-present
                :format-string "Menu item ~S already present in ~S"
                :format-args (list string command-table)))
      (remove-menu-item-from-command-table command-table string)))
  (when (eq type ':command)
    ;; Canonicalize command name to a command with the right number of
    ;; unsupplied argument markers.
    (unless (listp value)
      (setq value (list value)))
    (let ((n-required (get (first value) 'n-required))
          (n-supplied (1- (length value))))
      (when (and n-required
                 (not (zerop n-required))
                 (< n-supplied n-required))
        (setq value (append value
                            (make-list (- n-required n-supplied)
                                       :initial-element *unsupplied-argument-marker*))))))
  (with-slots (menu menu-tick commands keystrokes) command-table
    (incf menu-tick)
    (setq keystrokes nil)
    (let* ((item `(,type ,value
                   ,@(and documentation `(:documentation ,documentation))
                   ,@(and text-style `(:text-style ,text-style))
                   ,@(and mnemonic `(:mnemonic ,mnemonic))
                   ,@(and accelerator-text `(:accelerator-text ,accelerator-text))
                   ,@(and button-type `(:button-type ,button-type))))
           ;; Entries are of the form (MENU-NAME KEYSTROKE MENU-ITEM)
           (entry (list string keystroke item)))
      (when (null menu)
        (setq menu (make-array *command-table-size*
                               :fill-pointer 0 :adjustable t)))
      (case after
        ((:start)
         (vector-push-extend nil menu)                ;extend the vector by 1
         (replace menu menu :start1 1 :start2 0)
         (setf (aref menu 0) entry))
        ((:end nil)
         (vector-push-extend entry menu))
        ((:sort)
         (vector-push-extend entry menu)
         (flet ((menu-name-lessp (x y)
                  (cond ((null x) t)
                        ((null y) nil)
                        (t (string-lessp x y)))))
		   #-acl86win32
           (setq menu (sort menu #'menu-name-lessp :key #'first))
           #+acl86win32
		   (let ((newcts (sort menu #'menu-name-lessp :key #'first)))
			 (setq menu
				   (make-array (length newcts) 
							   :fill-pointer (fill-pointer menu)
							   :adjustable t
							   :initial-contents newcts)))))
        (otherwise
          (if (stringp after)
              (let ((index (position after menu
                                     :test #'menu-name-equal :key #'first)))
                (if index
                    (cond ((= index (1- (fill-pointer menu)))
                           ;; Just add at end
                           (vector-push-extend entry menu))
                          (t (vector-push-extend nil menu)
                             (replace menu menu :start1 (+ index 2) :start2 (+ index 1))
                             (setf (aref menu (+ index 1)) entry)))
                  (error 'command-not-present
                         :format-string "Menu item ~S not present in ~S for :AFTER"
                         :format-args (list after command-table))))
            (error "The value for :AFTER is not a string, :START, :END, or :SORT"))))
      ;; Now that the command is accessible via a menu (or keystroke),
      ;; make sure that we've really imported it
      (when (eq type ':command)
        (let ((old-name (gethash (first value) commands)))
          (setf (gethash (first value) commands) (or old-name t))))
      entry)))

(defun remove-menu-item-from-command-table (command-table string &key (errorp t))
  (check-type string string)
  (setq command-table (find-command-table command-table))
  (with-slots (menu menu-tick keystrokes) command-table
    (let ((index (position string menu
                           :test #'menu-name-equal :key #'first)))
      (cond (index
             ;;--- Is it right for this to remove the whole item even
             ;;--- if there is still a keystroke accelerator?
             (unless (= (1+ index) (fill-pointer menu))
               (replace menu menu :start1 index :start2 (1+ index)))
             (decf (fill-pointer menu))
             (incf menu-tick)
             (setq keystrokes nil))
            (t
             (when errorp
               (cerror "Proceed without any special action"
                       'command-not-present
                       :format-string "Menu item ~S not present in ~S"
                       :format-args (list string command-table))))))))

(defun map-over-command-table-menu-items (function command-table)
  (declare (dynamic-extent function))
  (map nil #'(lambda (entry)
               (destructuring-bind (menu keystroke item) entry
                 (when (or menu
                           (eq (command-menu-item-type item) :divider))
                   (funcall function menu keystroke item))))
       (slot-value (find-command-table command-table) 'menu)))

(defun find-menu-item (menu-name command-table &key (errorp t))
  (declare (values menu-item command-table))
  (let* ((command-table (find-command-table command-table))
         (item (find menu-name (slot-value command-table 'menu)
                     :test #'menu-name-equal :key #'first)))
    (when item
      (return-from find-menu-item
        (values (third item) command-table))))
  (when errorp
    (error 'command-not-present
           :format-string "Menu name ~S has no menu item present in ~S"
           :format-args (list menu-name command-table))))

(defparameter *command-table-menu-text-style*
              (parse-text-style '(:sans-serif :roman :normal)))
(defparameter *command-table-menu-gray* (make-gray-color 1/2))

(define-presentation-type command-menu-element ())

;; This should never be called with :ACCEPTABLY T
(define-presentation-method present
                            (element (type command-menu-element) stream (view textual-view)
                             &key acceptably)
  (when acceptably
    (error "There is no way to print command-menu elements acceptably."))
  (let* ((menu (pop element))
         (keystroke (pop element))
         (item (pop element))
         (type (command-menu-item-type item))
         (command (and (or (eq type ':command)
                           (eq type ':function))
                       (extract-command-menu-item-value item t)))
         (text-style (getf (command-menu-item-options item) :text-style)))
    (flet ((body (stream)
             (format stream "~A" menu)
             (when keystroke
               (write-string " (" stream)
               (describe-gesture-spec keystroke :stream stream)
               (write-string ")" stream))
             (when (eq type ':menu)
               (write-string " >" stream))))
      (declare (dynamic-extent #'body))
      (with-text-style (stream text-style)
        (if (and command
                 (not (command-enabled (command-name command) *application-frame*)))
            (with-drawing-options (stream :ink *command-table-menu-gray*
                                          :text-face :bold)
              (body stream))
            (body stream))))))

(defun display-command-table-menu (command-table stream
                                   &key max-width max-height
                                        n-rows n-columns
                                        x-spacing y-spacing
                                        (cell-align-x ':left) (cell-align-y ':top)
                                        (initial-spacing t) (row-wise nil) move-cursor)
  (unless (or max-width max-height)
    (multiple-value-bind (width height)
        (bounding-rectangle-size (sheet-region stream))
      (unless max-width (setf max-width width))
      (unless max-height (setf max-height height))))
  (let ((menu (slot-value (find-command-table command-table) 'menu)))
    (if (zerop (count-if #'(lambda (x) (not (null (first x)))) menu))
        (with-text-face (stream :italic)
          (write-string "[No menu items]" stream))
        (formatting-item-list (stream :max-width max-width :max-height max-height
                                      :n-rows n-rows :n-columns n-columns
                                      :x-spacing x-spacing :y-spacing y-spacing
                                      :initial-spacing initial-spacing
                                      :row-wise row-wise :move-cursor move-cursor)
          (dovector (element menu)
            (cond ((eq (command-menu-item-type (third element)) :divider)
                   (typecase (first element)
                     (string
                       (let ((text-style
                               (getf (command-menu-item-options (third element)) :text-style)))
                         (with-text-style (stream text-style)
                           (formatting-cell (stream :align-x cell-align-x
                                                    :align-y cell-align-y)
                             (write-string (first element) stream)))))
                     (null
                       (let* ((options (command-menu-item-options (third element)))
                              (width (getf options :width 50))
                              (thickness (getf options :thickness 2))
                              (ink (getf options :ink *command-table-menu-gray*)))
                         (formatting-cell (stream :align-x cell-align-x
                                                  :align-y :center)
                           (with-local-coordinates (stream)
                             (draw-line* stream 0 0 width 0
                                         :line-thickness thickness :ink ink)))))))
                  ((first element)
                   (formatting-cell (stream :align-x cell-align-x :align-y cell-align-y)
                     (present element 'command-menu-element
                              :stream stream :single-box t)))))))))

;; This doesn't actually need to execute the command itself, because it
;; presents each of the menu items as COMMAND-MENU-ELEMENTs and establish
;; an input context of COMMAND.  Thus the COMMAND-MENU-ELEMENT-TO-COMMAND
;; and COMMAND-MENU-ELEMENT-TO-SUB-MENU translators will take care of
;; executing the command.
(defun menu-execute-command-from-command-table
       (command-table
        &key (associated-window (frame-top-level-sheet *application-frame*))
             (text-style *command-table-menu-text-style*) label
             cache (unique-id command-table) (id-test #'eql) cache-value (cache-test #'eql))
  (setq command-table (find-command-table command-table))
  (unless cache-value
    (setq cache-value (slot-value command-table 'menu-tick)))
  (let ((menu-items (slot-value command-table 'menu)))
    (with-menu (menu associated-window :label label)
      (with-text-style (menu text-style)
        (flet ((menu-choose-body (stream type)
                 (declare (ignore type))
                 (menu-choose-command-drawer stream menu-items nil)))
          (declare (dynamic-extent #'menu-choose-body))
          ;; The translators referred to above will ensure that we get only
          ;; valid, enabled commands
          (menu-choose-from-drawer
            menu `(command :command-table ,command-table) #'menu-choose-body
            :cache cache
            :unique-id unique-id :id-test id-test
            :cache-value cache-value :cache-test cache-test))))))

(defun menu-choose-command-from-command-table
       (command-table
        &key (associated-window (frame-top-level-sheet *application-frame*))
             (text-style *command-table-menu-text-style*) label
             cache (unique-id command-table) (id-test #'eql) cache-value (cache-test #'eql))
  (setq command-table (find-command-table command-table))
  (unless cache-value
    (setq cache-value (slot-value command-table 'menu-tick)))
  (let ((menu-items (slot-value command-table 'menu)))
    (with-menu (menu associated-window :label label)
      (with-text-style (menu text-style)
        (flet ((menu-choose-body (stream type)
                 (menu-choose-command-drawer stream menu-items type)))
          (declare (dynamic-extent #'menu-choose-body))
          (multiple-value-bind (item gesture)
              (menu-choose-from-drawer
                menu 'menu-item #'menu-choose-body
                :cache cache
                :unique-id unique-id :id-test id-test
                :cache-value cache-value :cache-test cache-test)
            (when item
              (extract-command-menu-item-value (third item) gesture))))))))

(defun menu-choose-command-drawer (stream items type)
  (formatting-item-list (stream :move-cursor nil)
    (dovector (item items)
      (cond ((eq (command-menu-item-type (third item)) :divider)
             (typecase (first item)
               (string
                 (let ((text-style
                         (getf (command-menu-item-options (third item)) :text-style)))
                   (with-text-style (stream text-style)
                     (formatting-cell (stream)
                       (write-string (first item) stream)))))
               (null
                 (let* ((options (command-menu-item-options (third item)))
                        (width (getf options :width 50))
                        (thickness (getf options :thickness 2))
                        (ink (getf options :ink *command-table-menu-gray*)))
                   (formatting-cell (stream :align-x :left
                                            :align-y :center)
                     (with-local-coordinates (stream)
                       (draw-line* stream 0 0 width 0
                                   :line-thickness thickness :ink ink)))))))
            ((first item)
             (formatting-cell (stream)
               (if type
                   (with-output-as-presentation (stream item type
                                                 :single-box t)
                     (present item 'command-menu-element :stream stream))
                   (present item 'command-menu-element :stream stream)))))))
  nil)

(defun extract-command-menu-item-value (menu-item gesture
                                        &optional (numeric-argument *numeric-argument*))
  (let ((type (command-menu-item-type menu-item))
        (value (command-menu-item-value menu-item)))
    (if (eq type ':function)
        (funcall value gesture numeric-argument)
        value)))


;;; Keystroke accelerators

(defun add-keystroke-to-command-table (command-table keystroke type value
                                       &key documentation (errorp t))
  (assert (keyboard-gesture-spec-p keystroke) (keystroke)
          "~S is not a keyboard gesture spec" keystroke)
  (check-type type (member :command :function :menu))
  (check-type documentation (or string null))
  (setq command-table (find-command-table command-table))
  (let ((old-item (find-keystroke-item keystroke command-table
                                       :test #'gesture-spec-eql :errorp nil)))
    (when old-item
      (when errorp
        (cerror "Remove the keystroke item and proceed"
                'command-already-present
                :format-string "Keystroke item ~S already present in ~S"
                :format-args (list keystroke command-table)))
        (remove-keystroke-from-command-table command-table keystroke)))
  (add-menu-item-to-command-table command-table nil type value
                                  :documentation documentation
                                  :keystroke keystroke :errorp nil))

(defun remove-keystroke-from-command-table (command-table keystroke
                                            &key (errorp t))
  (setq command-table (find-command-table command-table))
  (with-slots (menu keystrokes) command-table
    (let ((index (position keystroke menu :key #'second :test #'gesture-spec-eql)))
      (cond (index
             (let ((element (aref menu index)))
               ;; Don't remove the whole item if there's a menu-name,
               ;; just remove the accelerator
               (when (stringp (first element))
                 (setf (second element) nil)
                 (return-from remove-keystroke-from-command-table nil)))
             (unless (= (1+ index) (fill-pointer menu))
               (replace menu menu :start1 index :start2 (1+ index))
             (decf (fill-pointer menu))
             (setq keystrokes nil)))
            (t
             (when errorp
               (error 'command-not-present
                      :format-string "Keystroke item ~S not present in ~S"
                      :format-args (list keystroke command-table))))))))

(defun map-over-command-table-keystrokes (function command-table)
  (declare (dynamic-extent function))
  (map nil #'(lambda (entry)
               (when (second entry)
                 (apply function entry)))
       (slot-value (find-command-table command-table) 'menu)))

(defun find-keystroke-item (keystroke command-table
                            &key (test #'keyboard-event-matches-gesture-name-p) (errorp t))
  (declare (values menu-item command-table))
  (let* ((command-table (find-command-table command-table))
         (entry (block find-entry
                  ;; Do it the hard way so that GESTURE-SPEC-EQL doesn't see NILs
                  (map nil #'(lambda (entry)
                               (when (and (second entry)
                                          (funcall test keystroke (second entry)))
                                 (return-from find-entry entry)))
                       (slot-value command-table 'menu)))))
    (when entry
      (return-from find-keystroke-item
        (values (third entry) command-table))))
  (when errorp
    (error 'command-not-present
           :format-string "Keystroke ~S has no menu item present in ~S"
           :format-args (list keystroke command-table))))

(defun lookup-keystroke-item (keystroke command-table
                              &key (test #'keyboard-event-matches-gesture-name-p))
  (declare (values menu-item command-table))
  (labels ((map-menu (command-table)
             (map nil #'(lambda (entry)
                          (let ((item (third entry)))
                            (when (and (second entry)
                                       (funcall test keystroke (second entry)))
                              (return-from lookup-keystroke-item
                                (values item command-table)))
                            (when (and (eq (command-menu-item-type item) ':menu)
                                       (null (second entry)))
                              ;; When this entry points to a submenu, and this
                              ;; entry has no keystroke of its own, then map
                              ;; into the sub-menu.  This allows us to find
                              ;; single keystroke accelerators in sub-menus.
                              (map-menu (find-command-table
                                          (command-menu-item-value item))))))
                  (slot-value command-table 'menu))))
    (declare (dynamic-extent #'map-menu))
    (map-menu (find-command-table command-table))))

;; Return a command associated with a keystroke iff it is enabled.
;; Otherwise, return the keystroke keystroke.
(defun lookup-keystroke-command-item (keystroke command-table
                                      &key (test #'keyboard-event-matches-gesture-name-p)
                                           (numeric-argument 1))
  (let ((item (lookup-keystroke-item keystroke command-table :test test)))
    (when item
      (let* ((type (command-menu-item-type item))
             (command
               (and (or (eq type ':command)
                        (eq type ':function))
                    (extract-command-menu-item-value item t numeric-argument))))
        (when (command-enabled (command-name command) *application-frame*)
          (return-from lookup-keystroke-command-item
            (substitute-numeric-argument-marker command numeric-argument))))))
  keystroke)

(defmacro with-command-table-keystrokes ((keystrokes-var command-table) &body body)
  (let ((comtab '#:command-table))
    `(let ((,keystrokes-var
            (let ((,comtab (find-command-table ,command-table)))
              ;; Use the cache if it's around, otherwise fill it in
              (or (slot-value ,comtab 'keystrokes)
                  (let ((,keystrokes-var nil))
                    (map-over-command-table-keystrokes
                      #'(lambda (menu keystroke item)
                          (declare (ignore menu item))
                          (push keystroke ,keystrokes-var))
                      ,comtab)
                    (setf (slot-value ,comtab 'keystrokes) ,keystrokes-var)
                    ,keystrokes-var)))))
       ,@body)))


;;; Presentation translator utilities

(defun add-presentation-translator-to-command-table (command-table translator
                                                     &key (errorp t))
  (setq command-table (find-command-table command-table))
  (with-slots (translators translators-cache) command-table
    (let* ((translator-name (presentation-translator-name translator))
           (place (position translator-name translators
                            :key #'presentation-translator-name)))
      (cond (place
             (when errorp
               (cerror "Remove the translator and proceed"
                       'command-already-present
                       :format-string "Translator ~S already present in ~S"
                       :format-args (list (presentation-translator-name translator)
                                          command-table)))
             (setf (aref translators place) translator))
            (t
             (when (null translators)
               (setq translators (make-array *command-table-size*
                                             :adjustable t :fill-pointer 0)))
             (vector-push-extend translator translators)))))
  ;; Maybe one day we'll do incremental updating of the cache.  That day
  ;; is not today.
  (incf *translators-cache-tick*))

(defun remove-presentation-translator-from-command-table (command-table translator-name
                                                          &key (errorp t))
  (setq command-table (find-command-table command-table))
  (with-slots (translators) command-table
    (let* ((translator
            (if (symbolp translator-name)
                (find-presentation-translator translator-name command-table
                                              :errorp nil)
              translator-name))
           (place (position translator translators)))
      (cond (place
             (setf (aref translators place) (vector-pop translators))
             (incf *translators-cache-tick*))
            (t
             (when errorp
               (cerror "Proceed without any special action"
                       'command-not-present
                       :format-string "Translator ~S not present in ~S"
                       :format-args (list translator-name command-table))))))))

(defun map-over-command-table-translators (function command-table &key (inherited t))
  (declare (dynamic-extent function))
  (if inherited
      (do-command-table-inheritance (comtab command-table)
        (let ((translators (slot-value comtab 'translators)))
          (when translators
            (dovector (translator translators)
              (funcall function translator)))))
    (let ((translators (slot-value (find-command-table command-table) 'translators)))
      (when translators
        (dovector (translator translators)
          (funcall function translator))))))

(defun find-presentation-translator (translator-name command-table &key (errorp t))
  (declare (values command command-table))
  (when (presentation-translator-p translator-name)
    (setq translator-name (presentation-translator-name translator-name)))
  (do-command-table-inheritance (comtab command-table)
    (let ((translator (find translator-name (slot-value comtab 'translators)
                            :key #'presentation-translator-name)))
      (when translator
        (return-from find-presentation-translator
          (values translator comtab)))))
  (when errorp
    (error 'command-not-accessible
           :format-string "Translator ~S is not accessible in ~S"
           :format-args (list translator-name command-table))))

(defun remove-presentation-translator (name)
  (maphash #'(lambda (key comtab)
               (declare (ignore key))
               (remove-presentation-translator-from-command-table comtab name :errorp nil))
           *all-command-tables*))

(defun clear-presentation-translator-caches ()
  (maphash #'(lambda (key comtab)
               (declare (ignore key))
               (with-slots (translators-cache) comtab
                 (when translators-cache
                   (clrhash translators-cache))))
           *all-command-tables*)
  (incf *translators-cache-tick*)
  (values))


;;; Defining commands

(defvar *valid-cp-lambda-list-keywords* '(&key))

(defun valid-cp-lambda-list-keyword-p (argument)
  (cond ((member argument *valid-cp-lambda-list-keywords*) t)
        ((member argument lambda-list-keywords)
         (warn "~S is not allowed as a lambda-list keyword in a command" argument)
         nil)
        ((and (consp argument)
              (symbolp (car argument))
              (not (constantp (car argument)))
              (null (cdr (last argument)))
              (evenp (length argument)))
         nil)
        (t
         (warn "~S is not valid syntax in a command; ~{~S, ~}or a list of~@
                an argument name, a form that evaluates to a presentation type specifier,~@
                and alternating keyword options and values is expected."
               argument *valid-cp-lambda-list-keywords*)
         nil)))

(defun deduce-parser-let-bindings (arguments)
  (let ((required-bindings nil)
        (keyword-bindings nil)
        (mode nil)
        default)
    (dolist (argument arguments)
      (setq default nil)
      (cond ((valid-cp-lambda-list-keyword-p argument)
             (setq mode argument))
            ((and (eq mode '&key)
                  (do ((l (cddr argument) (cddr l)))
                      ((null l) nil)
                    (when (eq (first l) :default)
                      (setq default (second l))
                      (return t))))
             (push `(,(first argument) ,default) keyword-bindings))
            ;; Added extra check that the argument is a cons, because valid-cp-lambda-list-keyword-p
            ;; only warns (but does not signal an error) for invalid argument descriptions.
            ;; (alemmens, 2004-11-26)
            ((consp argument)
             (push `(,(first argument) ',*unsupplied-argument-marker*) required-bindings))
            (t (error "~S is not a valid argument description for define-command." argument))))
    (values (nreverse required-bindings) (nreverse keyword-bindings))))

(defun deduce-body-arglist (arguments)
  (let ((args nil)
        (required-p t))
    (dolist (argument arguments)
      (cond ((valid-cp-lambda-list-keyword-p argument)
             (push argument args)
             (setq required-p nil))
            (t (let ((arg-name (first argument))
                     (default (getf (cddr argument) :default)))
                 (cond ((and default (not required-p))
                        (push `(,arg-name ,default) args))
                       (t (push arg-name args)))))))
    (nreverse args)))

(defun do-nothing-parser (&rest args)
  #-aclpc (declare (ignore args))
  nil)

(defun process-keyword-args-expander (arguments env)
  (let ((clauses nil)
        (constant-keywords nil)
        (conditional-keywords nil)
        (keyword-documentation nil)
        (keyword-defaults nil))
    (dolist (keyword-clause arguments)
      (let ((keyword (intern (symbol-name (first keyword-clause)) *keyword-package*))
            (when nil)
            (default nil)
            (type (and (constantp (second keyword-clause))
                       (eval (second keyword-clause)))))
        (do ((l (cddr keyword-clause) (cddr l)))
            ((null l))
          (case (car l)
            (:default
              (when (and type (constantp (second l)))
                (setq default `(,keyword ,(eval (second l)) ,type))))
            (:mentioned-default
              ;; Here's how this works.  The value of the keyword is
              ;; initially bound to the specified default.  If we ever
              ;; read the keyword name, we call the parser with the
              ;; mentioned default used as the default passed to ACCEPT.
              ;; The most common idiom is for boolean arguments, where
              ;; :DEFAULT NIL :MENTIONED-DEFAULT T is used.
              (setq keyword-clause `(,(first keyword-clause) ,(second keyword-clause)
                                     :default ,(cadr l)
                                     ,@(remove-keywords (cddr keyword-clause)
                                         '(:default :mentioned-default))))
              (when (and type (constantp (second l)))
                (setq default `(,keyword ,(eval (second l)) ,type))))
            (:when
              (setq keyword-clause `(,(first keyword-clause) ,(second keyword-clause)
                                     ,@(remove-keywords (cddr keyword-clause) '(:when))))
              (setq when t)
              (push `(and ,(cadr l) '(,keyword)) conditional-keywords))
            (:documentation
              (push `(,keyword ,(second l)) keyword-documentation))))
        (when default
          (push default keyword-defaults))
        (unless when
          (push keyword constant-keywords))
        (push `(,keyword ,(generate-parse-and-assign-clause keyword-clause env))
              clauses)))
    `(flet ((read-keyword-value (keyword)
              (ecase keyword
                ,@clauses)))
       (process-keyword-args
         parsing-stream ,(if conditional-keywords
                             `(append ,@conditional-keywords ',constant-keywords)
                             `',constant-keywords)
         #'read-keyword-value delimiter-parser arg-parser
         ',keyword-documentation ',keyword-defaults))))

(defun generate-parse-and-assign-clause (argument env)
  (let ((arg-name (first argument))
        (prompt-p (not (eq (getf (cddr argument) ':prompt 'xyzzy) 'xyzzy)))
        ;; Documentation is ignored for required arguments, and is
        ;; handled elsewhere for keyword arguments
        (args (remove-keywords (cddr argument) '(:gesture :documentation))))
    (warn-if-presentation-type-specifier-invalid `(define-command) (second argument) env)
    (do* ((args args (cddr args))
          (keyword (car args) (car args)))
         ((null args))
      (unless (member keyword '(:view :default :default-type :history :provide-default
                                :prompt :prompt-mode :display-default
                                :activation-gestures :additional-activation-gestures
                                :delimiter-gestures :additional-delimiter-gestures))
        (warn "The option ~S in the argument description for ~S is unrecognized."
              keyword arg-name)))
    `(assign-argument-value ,arg-name
                            (funcall arg-parser parsing-stream
                                     ,(second argument)
                                     :query-identifier ',arg-name
                                     ,@(unless prompt-p
                                         `(:prompt ,(format nil "~(~A~)" arg-name)))
                                     ,@args))))

(defun write-command-parser (body-name arguments env)
  (let ((parser-name (gensymbol body-name 'command-parser))
        (required-clauses nil)
        (keyword-clauses nil))
    (multiple-value-bind (required-bindings keyword-bindings)
        (deduce-parser-let-bindings arguments)
      (dorest (arguments-to-go arguments)
        (let* ((argument (first arguments-to-go)))
          (when (valid-cp-lambda-list-keyword-p argument)
            ;; Emit keyword parser and (return)
            (push (process-keyword-args-expander (cdr arguments-to-go) env)
                  keyword-clauses)
            (return))
          (push (generate-parse-and-assign-clause argument env)
                required-clauses)
          (push `(funcall delimiter-parser parsing-stream ',(rest arguments-to-go))
                required-clauses)))
      (if (or required-clauses keyword-clauses)
          (values `(defun ,parser-name (arg-parser delimiter-parser parsing-stream)
                     arg-parser delimiter-parser parsing-stream
                     (macrolet ((assign-argument-value (arg-name to-what)
                                  ;; we use this PROG1 to avoid useless
                                  ;; "unused variable" warnings under Genera 7.4
                                  ;; for (let ((x nil)) (setq x ...)).
                                  `(setq ,arg-name (prog1 ,to-what ,arg-name))))
                       (let* ,required-bindings
                         ,@(nreverse required-clauses)
                         (let* ,keyword-bindings
                           ,@(nreverse keyword-clauses)))))
                  parser-name)
          (values nil 'do-nothing-parser)))))

(defun write-command-argument-translators (command-name arguments
                                           &key command-table pointer-documentation)
  (let ((definitions nil)
        (count -1)
        (n-required (or (position '&key arguments) (length arguments)))
        (found-key nil))
    (when (null pointer-documentation)
      (setq pointer-documentation (command-name-from-symbol command-name)))
    (dolist (argument arguments)
      (cond ((valid-cp-lambda-list-keyword-p argument)
             (when (eq argument '&key)
               (setq found-key t)))
            (t
             (incf count)
             (let* ((unsupplied '#:unsupplied)
                    (arg-name (first argument))
                    (gesture (getf (cddr argument) :gesture unsupplied)))
               (unless (eq gesture unsupplied)
                 (let ((arg-type (cadr argument))
                       (translator-options nil))
                   (when (consp gesture)
                     (dolist (indicator '(:tester :menu :priority :echo
                                          :documentation :pointer-documentation))
                       (let ((option (getf (rest gesture) indicator unsupplied)))
                         (unless (eq option unsupplied)
                           (setq translator-options
                                 (append translator-options `(,indicator ,option))))))
                     (setq gesture (first gesture)))
                   (unless (constantp arg-type)
                     (error "It is illegal to define a translator on a non-constant presentation type"))
                   (setq arg-type (eval arg-type))
                   (let ((translator-name (fintern "~A-~A-~A-~A"
                                                   arg-type 'to command-name 'translator)))
                     (flet ((unsupplied (n)
                              (make-list n :initial-element '*unsupplied-argument-marker*)))
                       (push `(define-presentation-to-command-translator ,translator-name
                                  (,arg-type ,command-name ,command-table
                                   :gesture ,gesture
                                   ,@translator-options
                                   :pointer-documentation ,pointer-documentation)
                                  (object)
                                ,(if found-key
                                     `(list ,@(unsupplied n-required)
                                            ,(intern (symbol-name arg-name) *keyword-package*)
                                            object)
                                     `(list ,@(unsupplied count)
                                            object
                                            ,@(unsupplied (- n-required count 1)))))
                             definitions)))))))))
    definitions))

(defvar *define-command-options* '(:command-table :name :menu :keystroke))

(defun decode-name-and-options (name-and-options &optional command-table-name)
  (declare (values command-name command-options))
  (if (symbolp name-and-options)
      (values name-and-options nil)
    (let ((name (first name-and-options))
          (options (rest name-and-options)))
      (with-warnings-for-definition name define-command
        (do* ((option options (cddr option))
              (keyword (car option) (car option)))
             ((null option))
          (unless (member keyword *define-command-options*)
            ;; The extraneous option will not cause an error later
            ;; since GETF, not &KEY, is used
            (warn "The ~S option to ~S is unrecognized and will be ignored."
                  keyword 'define-command))
          (when (and command-table-name (eq keyword :command-table))
            ;; The extraneous option will be ignored since the correct :command-table
            ;; option will get stuck in front of it
            (warn "The ~S option is invalid; ~:*~S ~S will be used."
                  keyword command-table-name)))
        (values name options)))))

(defmacro define-command (name arguments &body body &environment env)
  #+Genera (declare (zwei:indentation 1 3 2 1))
  (multiple-value-bind (command-name command-options)
      (decode-name-and-options name)
    (with-warnings-for-definition command-name define-command
      (multiple-value-bind (parser-function parser-name)
          (write-command-parser command-name arguments env)
        (let* ((command-table (getf command-options :command-table))
               (command-line-name (getf command-options :name))
               (menu-item (getf command-options :menu))
               (keystroke (getf command-options :keystroke))
               (translators
                 (write-command-argument-translators
                   command-name arguments
                   :command-table command-table
                   :pointer-documentation (if (eq command-line-name 't)
                                              (command-name-from-symbol command-name)
                                              command-line-name)))
               (n-required (or (position '&key arguments) (length arguments))))
          (setf (compile-time-property command-name 'command-name) t)
          `(define-group ,command-name define-command
             ,parser-function
             (defun ,command-name ,(deduce-body-arglist arguments)
               ,@body)
             (setf (get ',command-name 'n-required) ,n-required)
             (define-command-1 #',parser-name ',command-name)
             ,(when command-table
                `(ensure-command-in-command-table ',command-name ',command-table
                                                 :name ',command-line-name
                                                 :menu ',menu-item
                                                 :keystroke ',keystroke))
             ,@translators))))))


(defun ensure-command-in-command-table (command-name command-table &key name menu keystroke)

  (when menu
    (let ((menu-options (and (consp menu) (cdr menu)))
          (menu-name (if (consp menu) (car menu) menu)))

      (when (eq (getf menu-options :after :replace) :replace)
        ;; Find who to insert it after or whether we need to put it at
        ;; the beginning
        (with-slots ((menu-vector menu)) (find-command-table command-table)
          (let ((position (position (if (eq menu-name t)
                                        (or name (command-name-from-symbol command-name))
                                      menu-name)
                                    menu-vector :test #'menu-name-equal :key #'first)))
            (cond ((null position)
                   (setf (getf menu-options :after) :end
                         menu (cons menu-name menu-options)))
                  ((zerop position)
                   (setf (getf menu-options :after) :start
                         menu (cons menu-name menu-options)))
                  (t
                   (setf (getf menu-options :after) (first (aref menu-vector (1- position)))
                         menu (cons menu-name menu-options)))))))))

  (remove-command-from-command-table command-name command-table
                                     :errorp nil)

  (add-command-to-command-table command-name command-table
                                :name name
                                :menu menu
                                :keystroke keystroke
                                :errorp nil))

(defun remove-command (name)
  (maphash #'(lambda (key comtab)
               (declare (ignore key))
               (remove-command-from-command-table name comtab :errorp nil))
           *all-command-tables*))

#+Genera
(progn
  (scl:defprop define-command "CLIM Command" si:definition-type-name)
  (scl:defprop define-command remove-command zwei:kill-definition))

#+Genera
(scl:defprop define-command zwei:defselect-function-spec-finder
             zwei:definition-function-spec-finder)

(defun define-command-1 (parser-function body-function)
  (setf (get body-function 'command-parser) parser-function))

