;; See the file LICENSE for the full license governing this code.
;;

(in-package :xm-silica)

(defclass xt-port (basic-port)
    ((application-shell :reader port-application-shell)
     (display :reader port-display)
     (context :reader port-context)
     ;; access to port-copy-gc should be within a without-scheduling
     (copy-gc :initform nil)
     (copy-gc-depth-1 :initform nil)
     (stipples :initform nil :accessor port-stipples)
     ;; This next is true for servers like Suns, which (pretty much) always
     ;; can safely copy-area without generating graphics-exposures.
     (safe-backing-store :initform nil :accessor port-safe-backing-store)
     (event-lock :initform (clim-sys:make-lock "port event lock")
		 :reader port-event-lock)
     (rotated-font-cache :initform nil :accessor port-rotated-font-cache)
     (rotated-string-cache :initform nil :accessor port-rotated-string-cache)
     (depth :accessor port-depth)
     (visual-class :accessor port-visual-class)
     (cursor-font :initform nil)
     (cursor-cache :initform nil)
     (font-cache :initform (make-hash-table :test #'equal))
     (font-set-cache :initform (make-hash-table :test #'equal))
     (glyph-info-cache :initform (make-hash-table :test #'equal))
     (compose-status :initform (make-xcomposestatus)
		     :reader port-compose-status)
     #+ignore ;; figure out how to get this translation
     (fm-ornamentation-offset :initform nil
			      :accessor port-fm-ornamentation-offset))
  (:default-initargs :allow-loose-text-style-size-mapping t
		     :deep-mirroring t)
  (:documentation "The port for X intrinsics based ports"))

(defun make-xcomposestatus ()
  (clim-utils::allocate-cstruct 'x11::xcomposestatus :initialize t))

(defmethod port-type ((port xt-port))
  ':xt)

(defmethod port-name ((port xt-port))
  (values (excl:native-to-string
	   (x11:display-display-name (port-display port)))))

;; access to port-copy-gc should be within a without-scheduling

(defmethod port-copy-gc ((port xt-port))
  ;;-- Assume 1-1 port-graft mapping?
  (with-slots (copy-gc display) port
    (or copy-gc
	(setf copy-gc
	  (make-instance 'tk::gcontext
	    :display display
	    :graphics-exposures :on
	    :foreign-address (x11:screen-default-gc
 			      (x11:xdefaultscreenofdisplay display)))))))

;;--- I don't know of a better way of getting a depth 1 drawable
;;other than by making a dummy depth 1 pixmap
(defmethod port-copy-gc-depth-1 ((port xt-port))
  ;;-- Assume 1-1 port-graft mapping?
  (with-slots (copy-gc-depth-1 display) port
    (or copy-gc-depth-1
	(let ((pixmap (make-instance 'tk::pixmap
				     :drawable
				     (tk::display-root-window display)
				     :depth 1 :width 1 :height 1)))
	  (prog1
	      (setf copy-gc-depth-1
		(make-instance 'tk::gcontext :drawable pixmap))
	    (tk::destroy-pixmap pixmap))))))

(defmethod restart-port ((port xt-port))
  (let ((process (port-process port)))
    (when process
      (clim-sys:destroy-process process))
    (setq process
      (mp:process-run-function
       (list :name (format nil "CLIM Event Dispatcher for ~A"
			   (port-server-path port))
	     :priority 1000)
       #'port-event-loop port))
    (setf (getf (mp:process-property-list process) :no-interrupts) t)
    (setf (port-process port) process)
    ;; Find out the modifier->modbit mapping on the display.
    (setup-modifier-key-mapping port)))

(defparameter *use-color* t)		; For debugging monochrome

(defvar *unreliable-server-vendors*
    '("Solbourne Computer, Inc" "Network Computing Devices"
      "Tektronix"))

(defmethod initialize-instance :after ((port xt-port) &key server-path)
  (setq tk::*x-io-error-hook* #'xt-fatal-error-handler)
  (destructuring-bind
      (&key (display nil display-p)
	    (application-name nil application-name-p)
	    (application-class nil application-class-p))
      (cdr server-path)
    (let ((args nil))
      (when display-p (setf (getf args :host) display))
      (when application-name-p (setf (getf args :application-name) application-name))
      (when application-class-p (setf (getf args :application-class) application-class))
      (multiple-value-bind (context display application-shell)
	  (apply #'tk::initialize-toolkit args)
	(setf (slot-value port 'application-shell) application-shell
	      (slot-value port 'context) context
	      (slot-value port 'display) display
	      (port-depth port) (x11:xdefaultdepth display (tk::display-screen-number display))
	      (port-visual-class port) (tk::screen-root-visual-class (tk::default-screen display))
	      (slot-value port 'silica::default-palette)
	      (make-palette port :colormap
			    (tk::default-colormap (port-display port))))
	(let* ((screen (x11:xdefaultscreenofdisplay display))
	       (bs-p (not (zerop (x11::screen-backing-store screen))))
	       (su-p (not (zerop (x11::screen-save-unders screen))))
	       (vendor (excl:native-to-string (x11::display-vendor display))))
	  (if (and bs-p su-p
		   ;; An amazing crock.  XXX
		   (notany #'(lambda (x) (search x vendor)) *unreliable-server-vendors*))
	      (setf (slot-value port 'safe-backing-store) t)))
	(initialize-xlib-port port display)))))

(defun xt-fatal-error-handler (d)
  (excl:without-interrupts
    (let* ((display (tk::find-object-from-address d))
	   (context (tk::display-context display)))
      (block done
	(map-over-ports
	 #'(lambda (port)
	     (when (eq (port-context port) context)
	       (destroy-port port)
	       (return-from done))))))
      (when tk::*inside-event-wait-function*
	(throw tk::*inside-event-wait-function* :error))))

(defmethod port-color-p ((port xt-port))
  (and *use-color*
       (> (port-depth port) 2)
       (member (port-visual-class port)
	       '(:static-color :true-color :pseudo-color :direct-color))
       t))

;;; SPR30362:

(defvar *default-fallback-font* "fixed")

(defun list-fonts-by-registry (display)
  (let* ((fonts (tk::list-font-names display "-*-*-*-*-*-*-*-*-*-*-*-*-*-*"))
         (encoding-hash (make-hash-table :test #'equal)))
    (dolist (font fonts)
      (let ((font* (disassemble-x-font-name font)))
        (push font (gethash (last font* 2) encoding-hash))))
    encoding-hash))

(defun find-font-with-properties (fonts weight slant)
  (or (find (list weight slant) fonts
            :test #'equal
            :key (lambda (font)
                   (let ((font* (disassemble-x-font-name font)))
                     (list (nth 3 font*) (nth 4 font*)))))
      (first fonts)))

(defun font-name-of-aliased-font (display fontname)
  (excl:with-native-string (nfn fontname)
    (let ((font (x11:xloadqueryfont display nfn)))      
      (unless (zerop font)
        (unwind-protect
            (loop for i from 0 below (x11:xfontstruct-n-properties font)
                  for fontprop = (+ ;; this is horrible:
                                  (* i 2 #-64bit 4 #+64bit 8)
                                  (x11:xfontstruct-properties font))
                  when (eql x11:xa-font (x11:xfontprop-name fontprop))
                    do (return (values (excl:native-to-string 
                                        (x11:xgetatomname display
                                                          (x11:xfontprop-card32 fontprop))))))
          (x11:xfreefont display font))))))

;;; END SPR30362

(defparameter *xt-font-families*
    `(
      ;; ascii
      (0 "fixed"
	 (:fix "-*-courier-*-*-*-*-*-*-*-*-*-*-iso8859-1")
	 (:sans-serif "-*-helvetica-*-*-*-*-*-*-*-*-*-*-iso8859-1")
	 (:serif "-*-new century schoolbook-*-*-*-*-*-*-*-*-*-*-iso8859-1"
		 "-*-times-*-*-*-*-*-*-*-*-*-*-iso8859-1"))

      ,@(excl:ics-target-case
	 (:+ics
	  `(
	    ;; jis-x208 (kanji)
	    (1 "k14"
	       (:fix "-*-*-*-*-*-*-*-*-*-*-*-*-jisx0208.1983-*")
	       (:sans-serif "-*-*-*-*-*-*-*-*-*-*-*-*-jisx0208.1983-*")
	       (:serif "-*-*-*-*-*-*-*-*-*-*-*-*-jisx0208.1983-*"))

	    ;; jis-x201 (half width katakana)
	    (2 "rk14"
	       (:fix "-*-*-*-*-*-*-*-*-*-*-*-*-jisx0201.1976-*")
	       (:sans-serif "-*-*-*-*-*-*-*-*-*-*-*-*-jisx0201.1976-*")
	       (:serif "-*-*-*-*-*-*-*-*-*-*-*-*-jisx0201.1976-*")))))
      #+hpux
      ;; hp-roman8 encoding as used by HP (only!).
      (3 "fixed"
	 (:fix "-*-courier-*-*-*-*-*-*-*-*-*-*-hp-roman8")
	 (:sans-serif "-*-helvetica-*-*-*-*-*-*-*-*-*-*-hp-roman8")
	 (:serif "-*-new century schoolbook-*-*-*-*-*-*-*-*-*-*-hp-roman8"
		 "-*-times-*-*-*-*-*-*-*-*-*-*-hp-roman8"))))

(defun disassemble-x-font-name (name)
  (let ((cpos 0)
	(tokens nil))
    (loop
      (let ((dpos (position #\- name :start cpos)))
	(when (null dpos)
	  (push (subseq name cpos) tokens)
	  (return))
	(push (if (= cpos dpos) nil (subseq name cpos dpos))
	      tokens)
	(setf cpos (1+ dpos))))
    (reverse tokens)))

;; by default don't use them because most users complain because they
;; look ugly - at least on the HP that is.

(defvar *use-scalable-fonts* nil)

(defmethod initialize-xlib-port ((port xt-port) display)
  (let* ((screen (x11:xdefaultscreen display))
	 ;;-- This is a property of the graft
	 (screen-pixels-per-inch
	  (* 25.4 (/ (x11::xdisplayheight display screen)
		     (x11:xdisplayheightmm display screen)))))
    (labels ((font->text-style (font family)
               (flet ((parse-token (token)
                        (if token
                            (parse-integer token)
                            (return-from font->text-style nil))))
                 (let* ((tokens (disassemble-x-font-name font))
                        (italic (member (nth 4 tokens) '("i" "o") :test #'equalp))
                        (bold (equalp (nth 3 tokens) "bold"))
                        (face (if italic
                                  (if bold '(:bold :italic) :italic)
                                  (if bold :bold :roman)))
                        (pixel-size (parse-token (nth 7 tokens)))
                        (point-size (parse-token (nth 8 tokens)))
                        (y-resolution (parse-token (nth 10 tokens)))
                        (average-width (parse-token (nth 12 tokens)))
                        (corrected-point-size (* (float point-size)
                                                 (/ y-resolution
                                                    screen-pixels-per-inch))))
                   (unless (and (not *use-scalable-fonts*)
                                (or (eql pixel-size 0)
                                    (eql point-size 0)
                                    (eql average-width 0)))
                     (make-text-style family face (/ corrected-point-size 10))))))
             (load-1-charset (character-set fallback families &optional query-first-p)
               (let* ((matchesp nil) ;do any non-fallback fonts match?
                      (fallback-matches-p ;any fallback matches?
                       (not (null (tk::list-font-names display fallback))))
                      (fallback-loadable-p ;fallback actually loadable?
                       (and fallback-matches-p
                            (excl:with-native-string (nfn fallback)
                              (if query-first-p
                                  (let ((info (x11:xqueryfont display nfn)))
                                    (unless (zerop info)
                                      (let ((font (x11:xloadfont display nfn)))
                                        (unwind-protect
                                            (unless (zerop font)
                                              (x11:xfreefont display font)
                                              t)
                                          (x11:xfreefontinfo nil info 1)))))
                                  (let ((x (x11:xloadqueryfont display nfn)))
                                    (if (not (zerop x))
                                        (progn
                                          (x11:xfreefont display x)
                                          t)
                                        nil)))))))
                 (dolist (per-family families)
                   (destructuring-bind (family &rest patterns) per-family
                     (dolist (font-pattern patterns)
                       (dolist (xfont (tk::list-font-names display font-pattern))
                         ;; this hack overcomes a bug with hp's scalable fonts
                         (unless (find #\* xfont)
                           (setf matchesp t) ;there was at least one match
                           (let ((text-style (font->text-style xfont family)))
                             ;; prefer first font satisfying this text style, so
                             ;; don't override if we've already defined one.
                             (when text-style
                               (unless (text-style-mapping-exists-p
                                        port text-style character-set t)
                                 (setf (text-style-mapping port text-style
                                                           character-set)
                                       xfont)))))))))
                 ;; Set up the fallback if it looks like there is one, and
                 ;; complain if things look bad.  Things look bad if there were
                 ;; matches but the fallback is not loadable.  If there were
                 ;; no matches then don't complain even if there appears to be
                 ;; something wrong with the fallback, just silently don't load it
                 ;; (and thus define no mappings for the character set).
                 (cond
                   (fallback-loadable-p	;all is well
                    (setf (text-style-mapping port *undefined-text-style*
                                              character-set)
                          fallback))
                   ((and matchesp fallback-matches-p)
                    (excl:ics-target-case
                      ;; The +ics case tries to load several more
                      ;; character sets, for many of which lack of a
                      ;; fallback is inconsequential. Let's not annoy
                      ;; users with an error in this case.
                      (:-ics
                       (warn "Fallback font ~A, for character set ~A, matches with XListFonts,
but is not loadable by XLoadFont or XQueryFont.  Something may be wrong with the X font
setup."
                             fallback character-set))))
                   (matchesp
                    (warn "Fallback font ~A not loadable for character set ~A."
                          fallback character-set))))))
      ;; Setup font mappings.  This is made hairy by trying to deal
      ;; elegantly with possibly missing mappings and messed-up X font
      ;; setups.  It seems to be the case that XListFonts can return
      ;; things even when XLoadFont will fail to load the font: I
      ;; think XListFonts only probes the fonts.alias file (or
      ;; equivalent).  There seems to be no way to tell if a font is
      ;; loadable other than by loading it, which is far too expensive
      ;; to do for every font.  So what this does is check the
      ;; fallback font carefully, and complain if something is wrong
      ;; with it, but otherwise don't check.  This should mean that if
      ;; this doesn't warn then things will basically run, as the
      ;; fallback exists for each character set, at least.
      (let ((charset-number 0)
            (done-registries ()))
        (dolist (per-charset *xt-font-families*)
          (destructuring-bind (character-set fallback &rest families) per-charset
            (load-1-charset character-set fallback families)
            (setf charset-number (max charset-number character-set))
            (dolist (family families)
              (pushnew (last (disassemble-x-font-name (second family)) 2) done-registries
                       :test #'equal))))
        ;; Now setup font mappings of fonts that the user has
        ;; installed, but we don't know anything about (especially no
        ;; convenient font aliases).
        ;; Since we don't have any font alias names to rely on, we use
        ;; the "fixed" alias to find out at least a sensible default
        ;; weight and slant.
        (excl:ics-target-case
         (:+ics
          (let* ((default-fallback (disassemble-x-font-name (font-name-of-aliased-font display *default-fallback-font*)))
                 (weight (nth 3 default-fallback))
                 (slant (nth 4 default-fallback)))
            (loop for character-set from (1+ charset-number) 
                  for encoding being the hash-keys of (list-fonts-by-registry display) using (hash-value fonts)
                  for fallback-font = (find-font-with-properties fonts weight slant)
                  for default-font-match-string = (format nil "-*-*-*-*-*-*-*-*-*-*-*-*-~A-~A" (first encoding) (second encoding))
                  do (unless (member encoding done-registries :test #'equal)
                       (vector-push-extend (excl:string-to-native
                                            (format nil "~A-~A" (first encoding) (second encoding)))
                                           tk::*font-list-tags*)
                       (load-1-charset character-set fallback-font
                                       `((:fix ,default-font-match-string)
                                         (:sans-serif ,default-font-match-string)
                                         (:serif ,default-font-match-string))
                                       t))))))
        )))
  (setup-stipples port display))

(defparameter *xt-logical-size-alist*
	      '((:tiny       6)
		(:very-small 8)
		(:small	     10)
		(:normal     12)
		(:large	     14)
		(:very-large 18)
		(:huge	     24)))

(defmethod standardize-text-style ((port xt-port) style
				   &optional (character-set *standard-character-set*))
  (standardize-text-style-1
    port style character-set *xt-logical-size-alist*))



(defmethod destroy-mirror ((port xt-port) (sheet mirrored-sheet-mixin))
  ;; Only do this if its the top most widget being destroyed or we are
  ;; screwing around with the tree in someway
  (tk::destroy-widget (sheet-direct-mirror sheet)))

(defmethod destroy-mirror :after ((port xt-port) (sheet sheet-parent-mixin))
  (labels ((loose-em (sheet)
	     (dolist (child (sheet-children sheet))
	       (let ((m (sheet-direct-mirror child)))
		 (when m
		   (setf (silica::mirror->sheet port m) nil
			 (sheet-direct-mirror child) nil)))
	       (when (typep child 'sheet-parent-mixin)
		 (loose-em child)))))
    (declare (dynamic-extent #'loose-em))
    (loose-em sheet)))

(defmethod realize-widget-mirror ((port xt-port) (parent-sheet t) parent-widget sheet)
  (multiple-value-bind (class name)
      (find-widget-class-and-name-for-sheet port parent-widget sheet)
    (let ((initargs (find-widget-initargs-for-sheet port parent-widget sheet)))
      (apply #'make-instance class
	     :name name
	     :parent parent-widget
	     :managed nil		; See note below
	     initargs))))

(defmethod realize-mirror ((port xt-port) sheet)
  (let* ((parent-widget (find-widget-parent port sheet))
	 (parent (sheet-parent sheet))
	 (widget (realize-widget-mirror port parent parent-widget sheet)))
    (initialize-mirror port parent parent-widget sheet widget)
    widget))

;; You may wonder why this is being done in this perverse way
;; Well its because if you create a managed child of ScrollingWindow then the
;; scrolling window calls its query geometry method which can result
;; in compose-space being invoked and the mirror<->sheet mapping has
;; not been established nor has the rest of tree been mirrored.
;; So it seems to work out really well to do this bottom up.

(defmethod sheet-and-ancestors-enabled-p ((sheet basic-sheet))
  ;; If we have an non-mirrored ancestor that is between us and our
  ;; mirrored-ancestor that is disabled then we should not manage this gadget.
  ;; This should happen
  (and (sheet-enabled-p sheet)
       (do ((parent (sheet-parent sheet) (sheet-parent parent)))
	   (nil)
	 (when (or (null parent) (sheet-direct-mirror parent)) (return t))
	 (unless (sheet-enabled-p parent) (return nil)))))

(defmethod note-sheet-tree-grafted :after ((port xt-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (sheet-direct-mirror sheet)))
    (when (sheet-and-ancestors-enabled-p sheet)
      (tk::manage-child mirror))))

(defmethod initialize-mirror ((port xt-port) (parent basic-sheet) (parent-widget t)
			      (sheet basic-sheet) widget)
  (add-sheet-callbacks port sheet widget))

(defmethod add-sheet-callbacks ((port xt-port) sheet (widget t))
  (declare (ignore sheet)))

(defvar *last-click-time* 0)
(defvar *double-click-time* 300) ;; .3 seconds

(defmethod sheet-mirror-event-handler (widget event sheet)
  #+ignore
  (format excl:*initial-terminal-io* "Got event ~s~%" (tk::event-type event))
  (let ((port (port sheet))
	(clim-event nil))
    (when port
      (setq clim-event
	(case (tk::event-type event)
	  ((:reparent-notify :selection-clear :selection-request
	    :selection-notify :client-message :mapping-notify :no-expose)
	   nil)
	  ((:map-notify :unmap-notify)
	   (sheet-mirror-map-callback event sheet)
	   nil)
	  (:configure-notify
	   (sheet-mirror-resized-callback widget nil event sheet)
	   nil)
	  (:graphics-expose
	   ;; Tricky!
	   (setf (port-safe-backing-store port) nil)
	   nil)
	  (:expose
	   ;; This gets called only for an XmBulletinBoard widget.
           ;; -- erm, no, that's a lie.
	   (sheet-mirror-exposed-callback widget nil event sheet)
	   nil)
	  (:key-press
	   (multiple-value-bind (character keysym)
	       (lookup-character-and-keysym sheet widget event)
	     (let ((keysym-shift-mask
		    (if (typep keysym 'modifier-keysym)
			(make-modifier-state
			 (case keysym
			   ((:left-shift :right-shift) :shift)
			   ((:left-control :right-control) :control)
			   ((:left-meta :right-meta) :meta)
			   ((:left-super :right-super) :super)
			   ((:left-hyper :right-hyper) :hyper)))
		      0)))
	       (allocate-event 'key-press-event
			       :sheet sheet
			       :key-name keysym
			       :character character
			       :modifier-state
			       (logior
				(state->modifiers (x11::xkeyevent-state event))
				keysym-shift-mask)))))
	  #+nil ; INTERNATIONAL: we must ignore key release events
          (:key-release
	   (multiple-value-bind (character keysym)
               ;; TODO/international: don't call XmbLookupString for keyrelease events!
	       (lookup-character-and-keysym sheet widget event)
	     (let ((keysym-shift-mask
		    (if (typep keysym 'modifier-keysym)
			(make-modifier-state
			 (case keysym
			   ((:left-shift :right-shift) :shift)
			   ((:left-control :right-control) :control)
			   ((:left-meta :right-meta) :meta)
			   ((:left-super :right-super) :super)
			   ((:left-hyper :right-hyper) :hyper)))
                        0)))
	       (allocate-event 'key-release-event
			       :sheet sheet
			       :key-name keysym
			       :character character
			       :modifier-state
			       (logandc2
				(state->modifiers (x11::xkeyevent-state event))
				keysym-shift-mask)))))
	  (:button-press
	   ;; It would appear this code never runs.  See sheet-mirror-input-handler.
	   (let ((button (x-button->silica-button
			  (x11::xbuttonevent-button event)))
		 (pointer (port-pointer port))
		 (then *last-click-time*)
		 (now (get-internal-real-time))
		 (state (state->modifiers
			 (x11::xbuttonevent-state event))))
	     (setq *last-click-time* now)
	     (when (<= (- now then) *double-click-time*)
	       ;; A double-click is actually four events:
	       ;; press, release, press, release.  Unlike Windows,
	       ;; in X there is no explicit double-click event.
	       ;; So we do it ourselves, here.
	       (setq state (logior state (make-modifier-state :double))))
	     (allocate-event 'pointer-button-press-event
			     :sheet sheet
			     :pointer pointer
			     :button button
			     :native-x (x11::xbuttonevent-x event)
			     :native-y (x11::xbuttonevent-y event)
			     :x :?? :y :??
			     :modifier-state state)))
	  (:button-release
	   (let ((button (x-button->silica-button
			  (x11::xbuttonevent-button event)))
		 (pointer (port-pointer port)))
	     (allocate-event 'pointer-button-release-event
			     :sheet sheet
			     :pointer pointer
			     :button button
			     :native-x (x11::xbuttonevent-x event)
			     :native-y (x11::xbuttonevent-y event)
			     :x :?? :y :??
			     :modifier-state
			     (state->modifiers
			      (x11::xkeyevent-state event)))))
	  (:leave-notify
	   (allocate-event 'pointer-exit-event
			   :sheet sheet
			   :native-x (x11:xcrossingevent-x event)
			   :native-y (x11:xcrossingevent-y event)
			   :kind (boundary-detail->kind
				  (x11:xcrossingevent-detail event))
			   :pointer (port-pointer port)
			   :modifier-state
			   (state->modifiers (x11::xcrossingevent-state event))))
	  (:enter-notify
	   (allocate-event 'pointer-enter-event
			   :pointer (port-pointer port)
			   :sheet sheet
			   :native-x (x11:xcrossingevent-x event)
			   :native-y (x11:xcrossingevent-y event)
			   :kind (boundary-detail->kind
				  (x11:xcrossingevent-detail event))
			   :modifier-state
			   (state->modifiers (x11::xcrossingevent-state event))))
	  (:motion-notify
	   (multiple-value-bind (same-p root child root-x root-y
				 native-x native-y state)
	       (tk::query-pointer (tk::widget-window widget))
	     (declare (ignore same-p root child))

	     (allocate-event 'pointer-motion-event
			     :pointer (port-pointer port)
			     :sheet sheet
			     :x root-x	; These are actually unused...
			     :y root-y	; ""
			     :native-x native-x
			     :native-y native-y
			     :modifier-state
			     (state->modifiers state))))
	  (:focus-in
	   (allocate-event 'focus-in-event :sheet sheet))
	  (:focus-out
	   (allocate-event 'focus-out-event :sheet sheet))
	  (otherwise
	   ;; it seems better to warn about this rather than just dying,
	   ;; at least there might be some chance of continuing!
	   (warn "Unhandled X event ~S, type ~A"
		 event (tk::event-type event))
	   nil))))
    (when clim-event
      (distribute-event port clim-event))))

(defun x-button->silica-button (button)
  (case button
    (#.x11::button1 +pointer-left-button+)
    (#.x11::button2 +pointer-middle-button+)
    (#.x11::button3 +pointer-right-button+)
    (#.x11::button4 +pointer-left-button+) ; These two are arbitrary.
    (#.x11::button5 +pointer-left-button+)))

(defun boundary-detail->kind (detail)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum detail))
  (case detail
    (#.x11:notifyancestor :ancestor)
    (#.x11:notifyvirtual  :virtual)
    (#.x11:notifyinferior :inferior)
    (#.x11:notifynonlinear :nonlinear)
    (#.x11:notifynonlinearvirtual :nonlinear-virtual)))


(defmethod sheet-mirror-resized-callback (widget window event sheet)
  (declare (ignore widget window event))
  (dispatch-event
    sheet
    (let ((r (mirror-region (port sheet) sheet)))
      (allocate-event 'window-configuration-event
	:native-region r
	:region (untransform-region (sheet-native-transformation sheet) r)
	:sheet sheet))))

;;-- Perhaps this method should be for any mirrored composite
;;-- sheet?

(defmethod sheet-mirrored-ancestor-of-clim-stream-sheet-p ((sheet t))
  nil)

(defmethod sheet-mirrored-ancestor-of-clim-stream-sheet-p
    ((sheet sheet-parent-mixin))
  (labels ((walk-children (sheet)
	     (dolist (child (sheet-children sheet))
	       (typecase child
		 (clim-stream-sheet
		  (return-from sheet-mirrored-ancestor-of-clim-stream-sheet-p t))
		 (mirrored-sheet-mixin nil)
		 (sheet-parent-mixin
		  (walk-children child))))))
    (declare (dynamic-extent #'walk-children))
    (walk-children sheet)))

(defmethod sheet-mirror-exposed-callback (widget window event sheet)
  ;; This isn't really the right place to do this, but it's better than
  ;; in ensure-blinker-for-cursor.
  (declare (ignore window))
  (when (let ((port (port sheet)))
	  ;; why does this care about port-safe-backing-store?????
	  ;; (cim 10/19/94)
	  (and port (port-safe-backing-store port)))
    (let ((window (tk::widget-window widget)))
      (unless (getf (window-property-list window) 'backing-store-on)
	(let ((x (sheet-mirrored-ancestor-of-clim-stream-sheet-p sheet)))
	  (setf (getf (window-property-list window) 'backing-store-on)
	    (if x :yes :no))
	  (when x (setf (xt::window-backing-store window) :when-mapped))))))
  (let* ((minx (x11::xexposeevent-x event))
	 (miny (x11::xexposeevent-y event))
	 (width (x11::xexposeevent-width event))
	 (height (x11::xexposeevent-height event))
	 (maxx (+ minx width))
	 (maxy (+ miny height)))
    #+ignore
    (format excl:*initial-terminal-io* "Got expose event ~s~%"
	    (tk::event-type event))
    (queue-repaint
     sheet
     (allocate-event 'window-repaint-event
		     :native-region (make-bounding-rectangle minx miny maxx maxy)
		     :region (untransform-region
			      (sheet-native-transformation sheet)
			      (make-bounding-rectangle minx miny maxx maxy))
		     :sheet sheet))))

(defmethod sheet-mirror-input-callback (widget window event sheet)
  (declare (ignore window))
  (let ((port (port sheet))
	(event-key (tk::event-type event)))
    (ecase event-key
      (:key-press
       (distribute-event
	port
	(multiple-value-bind (character keysym)
	    (lookup-character-and-keysym sheet widget event)
	  (let* ((keysym-shift-mask
		  (if (typep keysym 'modifier-keysym)
		      (make-modifier-state
		       (case keysym
			 ((:left-shift :right-shift) :shift)
			 ((:left-control :right-control) :control)
			 ((:left-meta :right-meta) :meta)
			 ((:left-super :right-super) :super)
			 ((:left-hyper :right-hyper) :hyper)))
                      0))
		 (modifier-state
		  (logior (state->modifiers (x11::xkeyevent-state event))
                          keysym-shift-mask))
		 ;;-- Canonicalize the only interesting key right here.
		 ;;--  If we get a key labelled "Return", we canonicalize it
		 ;;-- into #\Newline.
		 ;;-- This may be misguided, but it'll almost certainly help us
		 ;;-- in the short run.
		 ;;-- This code copied from clx-mirror.
		 (char (cond ((and (eq keysym ':return)
				   (or (zerop modifier-state)
				       (= modifier-state
					  (make-modifier-state :shift))))
			      #\Newline)
			     (t character))))
	    (allocate-event 'key-press-event
                            :sheet sheet
                            :key-name keysym
                            :character char
                            :modifier-state modifier-state)))))
      (:key-release nil ; INTERNATIONAL: don't do anything for key releases
                  )
      (:button-press
       (let ((button (x-button->silica-button
		      (x11::xbuttonevent-button event)))
	     (pointer (port-pointer port))
	     (then *last-click-time*)
	     (now (get-internal-real-time))
	     (state (state->modifiers
		     (x11::xbuttonevent-state event))))
	 (setq *last-click-time* now)
	 (when (<= (- now then) *double-click-time*)
	   ;; A double-click is actually four events:
	   ;; press, release, press, release.  Unlike Windows,
	   ;; in X there is no explicit double-click event.
	   ;; So we do it ourselves, here.
	   (setq state (logior state (make-modifier-state :double))))

	 #+debug
	 (format excl:*initial-terminal-io*
		 "~%event ~s type: ~s button: ~s x: ~s y: ~s"
		 event
		 (x11::xevent-type event)
		 (x11::xbuttonevent-button event)
		 (x11::xbuttonevent-x event)
		 (x11::xbuttonevent-y event))
	 (distribute-event
	  port
	  (allocate-event 'pointer-button-press-event
			  :sheet sheet
			  :pointer pointer
			  :button button
			  :native-x (x11::xbuttonevent-x event)
			  :native-y (x11::xbuttonevent-y event)
			  ;;-- Filled in by distributor
			  :x :?? :y :??
			  :modifier-state state))))
      (:button-release
       (let ((button (x-button->silica-button
		      (x11::xbuttonevent-button event)))
	     (pointer (port-pointer port)))

	 (distribute-event
	  port
	  (allocate-event 'pointer-button-release-event
			  :sheet sheet
			  :pointer pointer
			  :button button
			  :native-x (x11::xbuttonevent-x event)
			  :native-y (x11::xbuttonevent-y event)
			  :x :?? :y :??
			  :modifier-state (state->modifiers
					   (x11::xbuttonevent-state event))))))
      )))

(defmethod sheet-mirror-map-callback (event sheet)
  (let ((frame (pane-frame sheet)))
    (when frame
      (let ((state (frame-state frame))
	    ;; Inform lisp but don't feed event back to X.
	    (*suppress-xevents* t))
	(declare (special *suppress-xevents*))
	;; spr24753
	;; Call note-frame-iconified and note-frame-deiconifed
	;; rather than simply setting the frame-state to :shrunk
	;; and :enabled respectively.  (This happens on the
	;; respective :after methods in silica/framem.lisp.)
	;; Note that according to the documenation, calling 
	;; these methods also directly iconify/deiconify the
	;; frame (by calling the functions x11:xmapwindow
	;; and x11:xiconifywindow and  --see tk-silica/xt-frames.lisp.)
	;; We are depending on the fact that the windows-system
	;; won't try to re-iconify an already iconfied window, etc.
	(case (tk::event-type event)
	  (:map-notify
	   (when (eq state :shrunk)
	     (note-frame-deiconified (frame-manager frame) frame) 
	     ))
	  (:unmap-notify
	   ;; spr17465
	   ;; On Sparc in CDE (common desktop environment),
	   ;; switching out of a workspace will send an UnmapNotify
	   ;; message.  Iconifying a window also sends an UnmapNotify
	   ;; message.  There is apparently no way to tell the difference.
	   ;; The right thing to do is to stop calling XIconifyWindow in
	   ;; response, since it is not only unnecessary, it is detrimental.
	   ;; That is the purpose of *suppress-xevents*.
	   (when (eq state :enabled)
	     (note-frame-iconified (frame-manager frame) frame) 
	     )))))))

(defmethod find-widget-class-and-name-for-sheet
    ((port xt-port) (parent t) (sheet basic-sheet))
  (error "we should not be here"))

(defmethod find-widget-initargs-for-sheet
    ((port xt-port) (parent t) (sheet basic-sheet))
  nil)

;; if no-one else names a widget use the class-name as the default
;; name

(defmethod find-widget-class-and-name-for-sheet :around
	   ((port xt-port) (parent t) (sheet basic-sheet))
  (multiple-value-bind
      (class name)
      (call-next-method)
    (values class
	    (or name class))))

(defmethod find-widget-initargs-for-sheet :around
	   ((port xt-port) (parent t) (sheet basic-sheet))
  (compute-initial-mirror-geometry parent sheet (call-next-method)))

(defmethod compute-initial-mirror-geometry (parent sheet initargs)
  ;;--- Should we pass in the size of the sheet even though it is
  ;; liable to be quite stupid
  ;; We really want to just create the gadgets and then let the layout
  ;; stuff do everything
  (unless (getf initargs :x)
    (multiple-value-bind (left top right bottom)
	(sheet-actual-native-edges* sheet)
      bottom right
      ;;--- We do not want to specify the x,y if this is a top-level
      ;;sheet.
      (unless (typep parent 'tk::shell)
	(setf (getf initargs :x) (fix-coordinate left)
	      (getf initargs :y) (fix-coordinate top)))
      ;;--- We should not do this, see realize-mirror :around in mirror
      #+ignore
      (setf (getf initargs :width)  (fix-coordinate (- right left))
	    (getf initargs :height) (fix-coordinate (- bottom top)))))
  initargs)

(defmethod initialize-shell ((port xt-port) sheet widget)
  (declare (ignore sheet widget))
  nil)

;; If we are creating a top level sheet then we have to create a shell for it
(defmethod find-widget-parent ((port xt-port) sheet)
  (let ((ma (sheet-mirrored-ancestor sheet)))
    (if (graftp ma)
	(multiple-value-bind (class initargs)
	    (find-shell-class-and-initargs port sheet)
	  (let ((frame (pane-frame sheet)))
	    (when frame
	      (setf (getf initargs :title) (frame-pretty-name frame)))
	    (let ((shell (apply #'make-instance class
				:name (or (frame-name frame) class)
				:parent (find-shell-parent port sheet) initargs)))
	      (initialize-shell port sheet shell)
	      shell)))
      (sheet-mirror-for-parenting ma))))

(defmethod sheet-mirror-for-parenting ((sheet basic-sheet))
  ;; There might be a situation where a sheet is mirrored by a tree of
  ;; children. Here you would want the mirror for parenting to be a leaf
  ;; rather than the root of the tree. For the moment a sheet is only
  ;; mirrored by one widget so the following is ok
  (sheet-mirror sheet))

(defmethod find-shell-of-calling-frame ((sheet basic-sheet))
  (let ((frame (pane-frame sheet)))
    (and frame
	 (find-shell-of-calling-frame frame))))

(defmethod find-shell-of-calling-frame ((frame application-frame))
  (let* ((calling-frame (frame-calling-frame frame))
	 (top-level-sheet (and calling-frame
			       (frame-top-level-sheet calling-frame))))
    (and top-level-sheet
	 (sheet-shell top-level-sheet))))

(defmethod find-shell-parent ((port xt-port) sheet)
  (let ((shell (find-shell-of-calling-frame sheet)))
    (if (and shell
	     (eq (xt::widget-display shell) (port-display port)))
	shell
      (port-application-shell port))))

(defmethod find-shell-class-and-initargs ((port xt-port) (sheet t))
  (values 'top-level-shell
	  '(:input t)))

(defmethod find-shell-class-and-initargs :around ((port xt-port) (sheet pane))
  (let* ((palette (frame-manager-palette (frame-manager (pane-frame sheet)))))
    (multiple-value-bind (class initargs)
	(call-next-method)
      (values class
	      `(:allow-shell-resize
		,(and (pane-frame sheet)
		      (clim-internals::frame-resizable (pane-frame sheet)))
		,@(and (not (eq (port-default-palette port) palette))
		       `(:colormap ,(palette-colormap palette)))
		,@initargs)))))


(defmethod enable-mirror ((port xt-port) (sheet t))
  (let ((mirror (sheet-mirror sheet)))
    (enable-xt-widget (widget-parent mirror) mirror)))

(defmethod enable-xt-widget ((parent t) mirror)
  (unless (xt::is-managed-p mirror)
    (manage-child mirror)))

(defmethod disable-mirror ((port xt-port) sheet)
  (let ((mirror (sheet-mirror sheet)))
    (when mirror
      (disable-xt-mirror (widget-parent mirror) mirror))))

(defmethod disable-xt-mirror ((parent t) mirror)
  (tk::unmanage-child mirror))

(defmethod disable-xt-mirror ((parent top-level-shell) mirror)
  (declare (ignore mirror))
  (popdown parent))

(defmethod realize-graft ((port xt-port) graft)
  ;; Set the width etc etc
  (setf (sheet-direct-mirror graft) (port-application-shell port))
  (let* ((display (port-display port))
	 (screen (x11:xdefaultscreen display)))
    (with-slots (silica::mm-height silica::mm-width
		 silica::pixel-height silica::pixel-width
		 silica::pixels-per-point) graft
      ;;-- If anyone cared we could just grab the screen and call the
      ;;-- accessors on that
      (setq silica::mm-width (x11:xdisplaywidthmm display screen)
	    silica::mm-height (x11:xdisplayheightmm display screen)
	    silica::pixel-width (x11::xdisplaywidth display screen)
	    silica::pixel-height (x11::xdisplayheight display screen)
	    silica::pixels-per-point (float (/ silica::pixel-width
					       (* 72 (/ silica::mm-width 25.4)))))
      ;;--- Mess with the region
      (setf (sheet-region graft)
	(ecase (graft-units graft)
	  ((:device :pixel)
	   (make-bounding-rectangle 0 0
				    silica::pixel-width silica::pixel-height))))
      ;;-- what about the transformation
      (setf (sheet-native-transformation graft) +identity-transformation+))))

(defmethod mirror-region* ((port xt-port) sheet)
  (when (sheet-mirror sheet)
    (multiple-value-bind (x y width height)
	(get-values (sheet-mirror sheet) :x :y :width :height)
      (values (coordinate x) (coordinate y)
	      (coordinate (+ x width)) (coordinate (+ y height))))))

(defmethod mirror-inside-region* ((port xt-port) sheet)
  (multiple-value-bind (minx miny maxx maxy)
      (mirror-region* port sheet)
    (values (coordinate 0) (coordinate 0)
	    (- maxx minx) (- maxy miny))))

(defmethod mirror-native-edges* ((port xt-port) sheet)
  (let* ((mirror (sheet-direct-mirror sheet)))
    (multiple-value-bind (x y width height)
	(get-values mirror :x :y :width :height)
      (values (coordinate x) (coordinate y)
	      (coordinate (+ x width)) (coordinate (+ y height))))))

(defmethod mirror-inside-edges* ((port xt-port) sheet)
  (multiple-value-bind (a b c d)
      (mirror-native-edges* port sheet)
    (values (coordinate 0) (coordinate 0)
	    (- c a) (- d b))))

(defparameter  *compare-widget-geometry-with-intention* nil)

(defmethod set-sheet-mirror-edges* ((port xt-port) sheet
						   target-left target-top
						   target-right target-bottom)
  (let ((w (- target-right  target-left))
	(h (- target-bottom target-top))
	(mirror (sheet-direct-mirror sheet)))
    (setf target-left (fix-coordinate target-left)
	  target-top  (fix-coordinate target-top)
	  w (fix-coordinate w)
	  h (fix-coordinate h))
    (change-widget-geometry
	 ;;--- For top level sheets the sheet-parent is the graft whose
	 ;; mirror is the application shell
	 (tk::widget-parent mirror)
	 mirror
	 :x target-left
	 :y target-top
	 :width w
	 :height h)
    (when *compare-widget-geometry-with-intention*
      (multiple-value-bind
	  (nx ny nw nh)
	  (tk::get-values mirror :x :y :width :height)
	(when (or (/= target-left nx)
		  (/= target-top ny)
		  (/= w nw)
		  (/= h nh))
	  (let ((*error-output* excl:*initial-terminal-io*))
	    (warn "Geo set fail, ~S, ~S,~S"
		  sheet
		  (list  target-left  target-top w h)
		  (list nx ny nw nh))))))))

(defmacro with-port-event-lock ((port) &body body)
  `(clim-sys:with-lock-held ((port-event-lock ,port))
     ,@body))

(defmethod process-next-event ((port xt-port) &key wait-function timeout)
  (with-slots (context event-lock) port
    (let ((context context))
      (multiple-value-bind (mask reason)
	  (tk::wait-for-event context
			      :wait-function wait-function
			      :timeout timeout)
	(clim-sys:with-lock-held (event-lock)
	  ;; Make sure there is still an event ready, so we don't block in C.
	  (multiple-value-setq (mask reason)
	    (tk::wait-for-event context
				:wait-function wait-function
				:timeout 0))
	  (process-an-event port mask reason))))))

(defmethod process-an-event ((port xt-port) mask reason)
  (with-slots (context) port
    (tk::process-one-event context mask reason)))

(defun xrect-to-list (xr)
  (list :x
        (x11:xrectangle-x xr)
        :y
        (x11:xrectangle-y xr)
        :width
        ;; bounding box extents:
        (x11:xrectangle-width xr)
        :height
        (x11:xrectangle-height xr)))

(excl:ics-target-case
  (:-ics
   (defmethod port-glyph-for-character ((port xt-port)
                                        character text-style
                                        &optional our-font)
     (let* ((index (char-int character))
            (x-font (or our-font
                        (text-style-mapping port text-style
                                            (char-character-set-and-index character)))))
       (when (null x-font)
         ;; Throw a more meaningful error-message
         ;; when the x-font is not found.
         ;; This will happen, for example, when
         ;; a Japanese/foreign is not set up correctly.
         (let ((character-set (char-character-set-and-index character)))
           (error "X-Font not found for text-style: ~S on port:~S~% (character-set: ~A, character:~C, char-index:~S)"
                  text-style
                  port
                  character-set
                  character
                  index)))
       (let* ((escapement-x (tk::char-width x-font index))
              (escapement-y 0)
              (origin-x 0)
              (origin-y (tk::font-ascent x-font))
              (bb-x escapement-x)
              (bb-y (+ origin-y (tk::font-descent x-font))))
         (when (zerop escapement-x)
           (setq escapement-x (tk::font-width x-font)))
         (values index x-font escapement-x escapement-y
                 origin-x origin-y bb-x bb-y)))))
  (:+ics
   (defmethod port-glyph-for-character ((port xt-port) character text-style &optional our-font)
     (declare (ignore our-font)) ; We need the font set, not the font.
     (let* ((font-set (text-style-font-set port text-style)))
       (if (characterp character)
           (with-slots (glyph-info-cache) port
              (values-list
               (or (gethash (cons character text-style) glyph-info-cache)
                   (setf (gethash (cons character text-style) glyph-info-cache)
                         (multiple-value-list
                          (port-glyph-for-character-from-font-set port character font-set))))))
           (port-glyph-for-character-from-font-set port character font-set))))
   
   (defmethod port-glyph-for-character-from-font-set ((port xt-port) character font-set)
     (multiple-value-bind (native-string length) (excl:string-to-native (string character))
       (unwind-protect
           (tk::with-xrectangle-array (ink-return 1)
             (tk::with-xrectangle-array (logical-return 1)
               (x11:xmbtextextents font-set native-string
                                   (1- length) ink-return
                                   logical-return)
               (values (when (characterp character) (char-code character))
                       font-set
                       ;; escapement:
                       (x11:xrectangle-width logical-return) 0
                       ;; origin:
                       0 (abs (x11:xrectangle-y logical-return))
                       ;; bounding box:
                       (x11:xrectangle-width logical-return)
                       (x11:xrectangle-height logical-return))))
         (excl:aclfree native-string))))))

(defvar *trying-fallback* nil)

(defmethod find-named-font ((port xt-port) name character-set)
  (with-slots (font-cache) port
    (or (gethash name font-cache)
	(setf (gethash name font-cache)
	  (handler-case
	      (make-instance 'tk::font
			     :display (port-display port)
			     :name name)
	    (error ()
	      (if *trying-fallback*
		  (error "Failed to open fallback ~S" name)
		(let ((*trying-fallback* t)
		      ;; rebinding *error-output* prevents recursive
		      ;; errors when *error-output* is a CLIM stream
		      (*error-output* excl:*initial-terminal-io*))
		  (warn "Failed to open font ~S, trying fallback" name)
		  (text-style-mapping
		   port *undefined-text-style* character-set)))))))))

(excl:ics-target-case
  (:+ics

 (defmethod text-style-mapping :around
   ((port xt-port) text-style
    &optional (character-set *standard-character-set*) window)
   (declare (ignore window))
   (if character-set ; anything but *all-character-sets*
       (let ((mapping (call-next-method)))
         (if (stringp mapping)
             (setf (text-style-mapping port text-style character-set)
                   (find-named-font port mapping character-set))
             mapping))
       (let ((mappings nil))
         (dotimes (c (length (slot-value port 'silica::mapping-table))) ; XXX: ugly. prettify.
           (let ((mapping (text-style-mapping port text-style c)))
             (when mapping
               (push (cons c mapping) mappings))))
         (reverse mappings))))

(defmethod text-style-font-set ((port xt-port) text-style)
  (with-slots (font-set-cache) port
     (or (gethash text-style font-set-cache)
         (setf (gethash text-style font-set-cache)
               (font-set-from-font-list
                port (text-style-mapping port text-style *all-character-sets*))))))

(defmethod font-set-from-font-list ((port xt-port) font-list)
  (let ((name (format nil "~{~A~^,~}"
                      (mapcar (lambda (font) (tk::font-name (cdr font)))
                              font-list))))
    (with-slots (font-cache) port
      (setq font-list
	(or (gethash name font-cache)
	    (setf (gethash name font-cache)
	      (make-instance 'tk::font-set
		:display (port-display port)
		:base-names name))))))))

(:-ics

(defmethod text-style-mapping :around
	   ((port xt-port) text-style
	    &optional (character-set *standard-character-set*) window)
  (declare (ignore window))
  (let ((mapping (call-next-method)))
    (if (stringp mapping)
	(setf (text-style-mapping port text-style character-set)
	  (find-named-font port mapping character-set))
      mapping)))

(defmethod font-set-from-font-list ((port xt-port) font-list)
  (declare (ignore font-list))
  (error "not yet implemented for non-ics lisp"))

) ;; :-ics

) ;; ics-target-case

(defmethod change-widget-geometry (parent child &rest args)
  (declare (ignore parent))
  ;; In this case let the parent deal with it
  (apply #'tk::set-values child args))

(defmethod change-widget-geometry :around (parent child &key x y width height)
  (declare (ignore parent))
  ;;-- Nasty use of (widget-callback-data child) field
  (unless (and (typep x '(signed-byte 16))
	       (typep (+ x width) '(signed-byte 16))
	       (typep y '(signed-byte 16))
	       (typep (+ y height) '(signed-byte 16)))
    (let ((window (tk::widget-window child nil)))
      (when (and window (not (assoc 'widget-is-cached (tk::widget-callback-data child))))
	(push (cons 'widget-is-cached t) (tk::widget-callback-data child))
	(x11::xunmapwindow (xt::widget-display child) window)))
    (return-from change-widget-geometry nil))
  ;; Make sure its mapped
  (let ((x (assoc 'widget-is-cached (tk::widget-callback-data child))))
    (when x
      (setf (tk::widget-callback-data child)  (delete x (tk::widget-callback-data child)))
      (let ((window (tk::widget-window child nil)))
	(when window (x11::xmapwindow (xt::widget-display child) window)))))
  ;;
  (call-next-method))

(defmethod popup-frame-p ((frame application-frame))
  (typep frame '(or clim-internals::menu-frame
		 clim-internals::accept-values-own-window)))

;;-- Is this the right thing to do?
;;-- In particular it causes the frame to have a dialog shell.
;;-- It should really be a secondary shell.
#+ignore
(defmethod popup-frame-p ((frame activity-frame))
  t)

(defmethod popup-frame-p ((sheet basic-sheet))
  (let ((frame (pane-frame sheet)))
    (and frame
	 (popup-frame-p frame))))

;;; Keysym stuff

(defvar *xt-keysym->clim-keysym-table* (make-hash-table))
(defvar *clim-keysym->xt-keysym-table* (make-hash-table))

(defmacro define-xt-keysym (xt-keysym clim-keysym)
  `(ensure-xt-keysym ,xt-keysym ,clim-keysym))

(defun ensure-xt-keysym (xt-keysym clim-keysym)
  (let ((x (gethash xt-keysym *xt-keysym->clim-keysym-table*)))
    (when (and x (not (eq x clim-keysym)))
      (warn "Redefining keysym ~S from ~S to ~S" xt-keysym x clim-keysym)))
  (setf (gethash xt-keysym *xt-keysym->clim-keysym-table*) clim-keysym)
  (unless (gethash clim-keysym *clim-keysym->xt-keysym-table*)
    (setf (gethash clim-keysym *clim-keysym->xt-keysym-table*) xt-keysym)))

(defun-inline xt-keysym->keysym (xt-keysym)
  (gethash xt-keysym *xt-keysym->clim-keysym-table*))

(defun-inline keysym->xt-keysym (keysym)
  (gethash keysym *clim-keysym->xt-keysym-table*))

;; The standard characters
(define-xt-keysym 032 :space)
(define-xt-keysym 033 :\!)
(define-xt-keysym 034 :\")
(define-xt-keysym 035 :\#)
(define-xt-keysym 036 :\$)
(define-xt-keysym 037 :\%)
(define-xt-keysym 038 :\&)
(define-xt-keysym 039 :\')
(define-xt-keysym 040 :\()
(define-xt-keysym 041 :\))
(define-xt-keysym 042 :\*)
(define-xt-keysym 043 :\+)
(define-xt-keysym 044 :\,)
(define-xt-keysym 045 :\-)
(define-xt-keysym 046 :\.)
(define-xt-keysym 047 :\/)
(define-xt-keysym 048 :\0)
(define-xt-keysym 049 :\1)
(define-xt-keysym 050 :\2)
(define-xt-keysym 051 :\3)
(define-xt-keysym 052 :\4)
(define-xt-keysym 053 :\5)
(define-xt-keysym 054 :\6)
(define-xt-keysym 055 :\7)
(define-xt-keysym 056 :\8)
(define-xt-keysym 057 :\9)
(define-xt-keysym 058 :\:)
(define-xt-keysym 059 :\;)
(define-xt-keysym 060 :\<)
(define-xt-keysym 061 :\=)
(define-xt-keysym 062 :\>)
(define-xt-keysym 063 :\?)
(define-xt-keysym 064 :\@)
(define-xt-keysym 065 :a)
(define-xt-keysym 097 :a)
(define-xt-keysym 066 :b)
(define-xt-keysym 098 :b)
(define-xt-keysym 067 :c)
(define-xt-keysym 099 :c)
(define-xt-keysym 068 :d)
(define-xt-keysym 100 :d)
(define-xt-keysym 069 :e)
(define-xt-keysym 101 :e)
(define-xt-keysym 070 :f)
(define-xt-keysym 102 :f)
(define-xt-keysym 071 :g)
(define-xt-keysym 103 :g)
(define-xt-keysym 072 :h)
(define-xt-keysym 104 :h)
(define-xt-keysym 073 :i)
(define-xt-keysym 105 :i)
(define-xt-keysym 074 :j)
(define-xt-keysym 106 :j)
(define-xt-keysym 075 :k)
(define-xt-keysym 107 :k)
(define-xt-keysym 076 :l)
(define-xt-keysym 108 :l)
(define-xt-keysym 077 :m)
(define-xt-keysym 109 :m)
(define-xt-keysym 078 :n)
(define-xt-keysym 110 :n)
(define-xt-keysym 079 :o)
(define-xt-keysym 111 :o)
(define-xt-keysym 080 :p)
(define-xt-keysym 112 :p)
(define-xt-keysym 081 :q)
(define-xt-keysym 113 :q)
(define-xt-keysym 082 :r)
(define-xt-keysym 114 :r)
(define-xt-keysym 083 :s)
(define-xt-keysym 115 :s)
(define-xt-keysym 084 :t)
(define-xt-keysym 116 :t)
(define-xt-keysym 085 :u)
(define-xt-keysym 117 :u)
(define-xt-keysym 086 :v)
(define-xt-keysym 118 :v)
(define-xt-keysym 087 :w)
(define-xt-keysym 119 :w)
(define-xt-keysym 088 :x)
(define-xt-keysym 120 :x)
(define-xt-keysym 089 :y)
(define-xt-keysym 121 :y)
(define-xt-keysym 090 :z)
(define-xt-keysym 122 :z)
(define-xt-keysym 091 :\[)
(define-xt-keysym 092 :\\)
(define-xt-keysym 093 :\])
(define-xt-keysym 094 :\^)
(define-xt-keysym 095 :\_)
(define-xt-keysym 096 :\`)
(define-xt-keysym 123 :\{)
(define-xt-keysym 124 :\|)
(define-xt-keysym 125 :\})
(define-xt-keysym 126 :\~)

;; The semi-standard characters
(defmacro keysym (b &rest more)
  (dolist (n more)
    (setq b (+ (ash b 8) n)))
  b)

(define-xt-keysym (keysym 255 013) :return)
(define-xt-keysym (keysym 255 009) :tab)
(define-xt-keysym (keysym 255 255) :rubout)
(define-xt-keysym (keysym 255 008) :backspace)
(define-xt-keysym (keysym 009 227) :page)

;;; this was being immediately redefined to be :newline causing
;;; redefinition warning. Why does clim have separate :linefeed and
;;; :newline keysyms? (cim)
#+ignore (define-xt-keysym (keysym 255 010) :linefeed)

(define-xt-keysym (keysym 255 027) :escape)
;;;---
(define-xt-keysym (keysym 255 010) :newline)

;; Other useful characters
(define-xt-keysym (keysym 255 #x6a) :help)

;; we have two keysyms for :end - XK_End and XK_R13 (which is marked
;; end on certain sun keyboards)

(define-xt-keysym (keysym 255 #xde) :end)
(define-xt-keysym (keysym 255 #x57) :end)
;;--- not on lisp machines?
(define-xt-keysym (keysym 255 #x50) :home)
(define-xt-keysym (keysym 255 #x63) :insert)
(define-xt-keysym (keysym 255 #x60) :select)

(define-xt-keysym (keysym 255 #x68) :complete) ; Not on my keyboard
(define-xt-keysym (keysym 255 #x69) :abort) ; Not on my keyboard
(define-xt-keysym (keysym 255 #x56) :scroll) ; not on my keyboard
(define-xt-keysym (keysym 255 #x55) :scroll-up) ; aka prev, page up
(define-xt-keysym (keysym 255 #x61) :refresh) ; ditto
(define-xt-keysym (keysym 255 #x0b) :clear-input) ; ditto

(define-xt-keysym (keysym 255 #x51) :left-arrow)
(define-xt-keysym (keysym 255 #x52) :up-arrow)
(define-xt-keysym (keysym 255 #x53) :right-arrow)
(define-xt-keysym (keysym 255 #x54) :down-arrow)


;;;


;;; Some nonstandard keys

(define-xt-keysym (keysym 255 #xbe) :f1)
(define-xt-keysym (keysym 255 #xbf) :f2)
(define-xt-keysym (keysym 255 #xc0) :f3)
(define-xt-keysym (keysym 255 #xc1) :f4)
(define-xt-keysym (keysym 255 #xc2) :f5)
(define-xt-keysym (keysym 255 #xc3) :f6)
(define-xt-keysym (keysym 255 #xc4) :f7)
(define-xt-keysym (keysym 255 #xc5) :f8)
(define-xt-keysym (keysym 255 #xc6) :f9)
(define-xt-keysym (keysym 255 #xc7) :f10)

(define-xt-keysym (keysym 255 #xc8) :l1)
(define-xt-keysym (keysym 255 #xc9) :l2)
(define-xt-keysym (keysym 255 #xca) :l3)
(define-xt-keysym (keysym 255 #xcb) :l4)
(define-xt-keysym (keysym 255 #xcc) :l5)
(define-xt-keysym (keysym 255 #xcd) :l6)
(define-xt-keysym (keysym 255 #xce) :l7)
(define-xt-keysym (keysym 255 #xcf) :l8)
(define-xt-keysym (keysym 255 #xd0) :l9)
(define-xt-keysym (keysym 255 #xd1) :l10)

(define-xt-keysym (keysym 255 #xd2) :r1)
(define-xt-keysym (keysym 255 #xd3) :r2)
(define-xt-keysym (keysym 255 #xd4) :r3)
(define-xt-keysym (keysym 255 #xd5) :r4)
(define-xt-keysym (keysym 255 #xd6) :r5)
(define-xt-keysym (keysym 255 #xd7) :r6)
(define-xt-keysym (keysym 255 #xd8) :r7)
(define-xt-keysym (keysym 255 #xd9) :r8)
(define-xt-keysym (keysym 255 #xda) :r9)
(define-xt-keysym (keysym 255 #xdb) :r10)
(define-xt-keysym (keysym 255 #xdc) :r11)
(define-xt-keysym (keysym 255 #xdd) :r12)
;; This is end
;; (define-xt-keysym (keysym 255 #xde) :r13)
(define-xt-keysym (keysym 255 #xdf) :r14)
(define-xt-keysym (keysym 255 #xe0) :r15)

;; Finally, the shifts
;; snarfed from translate.cl

(defconstant left-shift-keysym    (keysym 255 225))
(defconstant right-shift-keysym   (keysym 255 226))
(defconstant left-control-keysym  (keysym 255 227))
(defconstant right-control-keysym (keysym 255 228))
(defconstant caps-lock-keysym	  (keysym 255 229))
(defconstant shift-lock-keysym	  (keysym 255 230))
(defconstant left-meta-keysym	  (keysym 255 231))
(defconstant right-meta-keysym	  (keysym 255 232))
(defconstant left-alt-keysym	  (keysym 255 233))
(defconstant right-alt-keysym	  (keysym 255 234))
(defconstant left-super-keysym	  (keysym 255 235))
(defconstant right-super-keysym	  (keysym 255 236))
(defconstant left-hyper-keysym	  (keysym 255 237))
(defconstant right-hyper-keysym	  (keysym 255 238))

(define-xt-keysym left-shift-keysym    :left-shift)
(define-xt-keysym right-shift-keysym   :right-shift)
(define-xt-keysym left-control-keysym  :left-control)
(define-xt-keysym right-control-keysym :right-control)
(define-xt-keysym caps-lock-keysym     :caps-lock)
(define-xt-keysym shift-lock-keysym    :shift-lock)
(define-xt-keysym left-meta-keysym     :left-meta)
(define-xt-keysym right-meta-keysym    :right-meta)
(define-xt-keysym left-super-keysym    :left-super)
(define-xt-keysym right-super-keysym   :right-super)
(define-xt-keysym left-hyper-keysym    :left-hyper)
(define-xt-keysym right-hyper-keysym   :right-hyper)

;;;
(defun lookup-character-and-keysym (sheet mirror event)
  (declare (optimize (speed 3) (safety 0))
           (ignore sheet))
  (multiple-value-bind (character keysym)
      (if (eql :key-press (tk::event-type event))
          (tk::lookup-multibyte-string event mirror))
    (setq character (and (= (length (the simple-string character)) 1)
                         (aref (the simple-string character) 0)))
    ;; This gets stuff wrong because for example to type-< you have to
    ;; use the shift key and so instead for m-< you aget m-sh-<
    ;; Perhaps there is a way of checking to see whether shifting
    ;; makes sense given the keyboard layout!
    (when character
      (let ((x (state->modifiers
                (x11::xkeyevent-state event))))
        (setq character
              (if (and (<= (char-int character) 26)
                       (not (member character
                                    '(#\return
                                      #\tab
                                      #\page
                                      #\backspace
                                      #\linefeed
                                      #\escape)
                                    :test #'eq)))
                  (code-char
                   (+ (char-code character)
                      (1- (if (logtest x +shift-key+)
                              (char-int #\A)
                              (char-int #\a)))))
                  character))
        (setq character
              (clim-make-char
               character
               (logior
                (if (logtest x +control-key+) 1 0)
                (if (logtest x +meta-key+)    2 0)
                (if (logtest x +super-key+)   4 0)
                (if (logtest x +hyper-key+)   8 0))))))
    (values character (xt-keysym->keysym keysym))))

(defun clim-make-char (character &optional (bits 0))
  ;; Like cltl1:make-char but prevents the need to (require :cltl1)
  (if (zerop bits)
      character
    (excl::old-code-char (char-code character) bits)))

(defvar +super-modifier-mask+ 0)
(defvar +hyper-modifier-mask+ 0)
(defvar +meta-modifier-mask+ 0)

(defun setup-modifier-key-mapping (port)
  (let* ((display (slot-value port 'tk-silica::display))
         (modifier-map (x11:xgetmodifiermapping display))
         (mods (x11:xmodifierkeymap-modifiermap modifier-map))
         (max-keypermod (x11:xmodifierkeymap-max-keypermod modifier-map))
         (skip-modbits 3)     ; assuming that shift, lock, control are
                              ; always bound to sane values.
         )
    (labels ((translate-key (key &aux keysym)
               (loop for index from 0 below max-keypermod
                     do (setf keysym (x11:xkeycodetokeysym display key index))
                     while (zerop keysym)
                     
                     finally  (return keysym))))
      (unwind-protect
          ;; "The modifiermap member of the XModifierKeymap structure
          ;; contains 8 sets of max_keypermod KeyCodes" -- XGetModifierMap(3X11)
          (loop for i from skip-modbits to 8
                for mod-keyword in `(,x11:mod1mask ,x11:mod2mask ,x11:mod3mask
                                         ,x11:mod4mask ,x11:mod5mask)
                do (loop for j from 0 below max-keypermod
                         for key = (sys:memref-int mods
                                                   (+ (* i max-keypermod) j)
                                       0 :unsigned-byte)
                         unless (zerop key)
                           do (case (translate-key key)
                                ((#.x11:XK-Meta-L #.x11:XK-Meta-R)
                                 (setf +meta-modifier-mask+ mod-keyword))
                                ((#.x11:XK-Super-L #.x11:XK-Super-R)
                                 (setf +super-modifier-mask+ mod-keyword))
                                ((#.x11:XK-Hyper-L #.x11:XK-Hyper-R)
                                 (setf +hyper-modifier-mask+ mod-keyword)))))
        (x11:xfreemodifiermap modifier-map)))
    (values +meta-modifier-mask+ +super-modifier-mask+ +hyper-modifier-mask+)))

(defun state->modifiers (x)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum x))
  ;; 2007-05-09/asf This is not entirely optimal yet: In recent linux
  ;; distributions (Ubuntu, specifically), Hyper and Super cause the
  ;; same modifier bit to be set on x events. Maybe it would be better
  ;; to observe the press/release events of modifier keys (this could
  ;; also enable sticky ESC to be Meta, the way emacs has it).
  (logior
   (if (logtest x x11:shiftmask) +shift-key+ 0)
   (if (logtest x x11:controlmask) +control-key+ 0)
   (if (logtest x +meta-modifier-mask+) +meta-key+ 0)
   (if (logtest x +super-modifier-mask+) +super-key+ 0)
   (if (logtest x +hyper-modifier-mask+) +hyper-key+ 0)))

(defmethod get-application-resources ((port xt-port))
  (multiple-value-bind (names classes)
      (xt::widget-resource-name-and-class (port-application-shell port))
    (get-xt-resources port names classes)))

(defmethod get-sheet-resources ((port xt-port) sheet)
  ;; we have to treat top level sheets specially because when
  ;; get-sheet-resources is called typically they don't have a
  ;; parent-widget yet (This is done later when the mirror is
  ;; realized)
  (let* ((ma (sheet-mirrored-ancestor sheet))
	 (top-level (graftp ma))
	 (parent-widget (if top-level
			    (find-shell-parent port sheet)
			  (sheet-mirror-for-parenting ma))))
    (multiple-value-bind (names classes)
	(tk::widget-resource-name-and-class parent-widget)
      (flet ((extend (class name)
	       (setq names (nconc names
				  (list (tk::tkify-lisp-name (or name class))))
		     classes (nconc classes
				    (list (tk::tkify-lisp-name class :class t))))))
	(when top-level
	  (let ((shell-name (frame-name (pane-frame sheet)))
		(shell-class (find-shell-class-and-initargs port sheet)))
	    (extend shell-class shell-name)))
	(when (typep sheet 'mirrored-sheet-mixin)
	  (multiple-value-call #'extend
	    (find-widget-class-and-name-for-sheet port parent-widget sheet))))
      (get-xt-resources port names classes))))

;; --- these should be defresource'd otherwise there may be problems with
;; multiple concurrent queries (cim 8/94)

(defvar *resource-name* (make-string 40))
(defvar *resource-class* (make-string 40))

(defun get-xt-resources (port names classes)
  (let* ((display (silica::port-display port))
	 (db (tk::display-database display))
	 (name-index 0)
	 (class-index 0)
	 (resource-name *resource-name*)
	 (resource-class *resource-class*))
    (macrolet ((splice (item array index)
		 `(let ((index ,index)
			(item-length (length ,item))
			(array-length (length ,array)))
		    (when (>= (+ index item-length) array-length)
		      (let* ((new-array-length (+ array-length array-length
						  item-length))
			     (new-array (make-string new-array-length)))
			(dotimes (i array-length)
			  (setf (schar new-array i) (schar ,array i)))
			(setq ,array new-array)))
		    (dotimes (i item-length)
		      (setf (schar ,array index) (schar ,item i))
		      (incf index))
		    index)))
      (dolist (name names)
	(setq name-index (splice name resource-name name-index))
	(setf (schar resource-name name-index) #\.)
	(incf name-index))
      (dolist (class classes)
	(setq class-index (splice class resource-class class-index))
	(setf (schar resource-class class-index) #\.)
	(incf class-index))
      (flet ((get-resource (name class)
	       (let ((name-index (splice name resource-name
					 name-index)))
		 (setf (schar resource-name name-index) #\null))
	       (let ((class-index (splice class resource-class
					  class-index)))
		 (setf (schar resource-class class-index) #\null))
	       (xt::get-resource db resource-name resource-class)))
	(let ((background (get-resource "background" "Background"))
	      (foreground (get-resource "foreground" "Foreground"))
	      (text-style (get-resource "textStyle" "TextStyle")))
	  (setq *resource-name* resource-name
		*resource-class* resource-class)
	  `(,@(and background `(:background ,background))
	    ,@(and foreground `(:foreground ,foreground))
	    ,@(let ((style (convert-text-style port display text-style)))
		(and style `(:text-style ,style)))))))))

(defun convert-text-style (port display text-style)
  (declare (ignore display))
  (ignore-errors
   (let ((spec (read-from-string text-style)))
     (if (consp spec)
	 (parse-text-style spec)
       (let ((font-name (if (stringp spec)
			    spec
			  text-style)))
	 (make-device-font-text-style port font-name))))))

;;;--- Gadget activation deactivate

(defmethod realize-mirror :around ((port xt-port) (sheet basic-gadget))
  (let ((widget (call-next-method)))
    (unless (gadget-active-p sheet)
      (xt::set-sensitive widget nil))
    widget))

;; No longer a protocol function, but we need it internally
(defmethod port-force-output ((port xt-port))
  (x11:xflush (port-display port)))


;;; Tricky ground ahead!

(defclass xt-geometry-manager ()
	  ;; --- This is probably all
	  ;; composites excepts drawing-area and shell
	  ()
  (:documentation "These are all parents that have strong feelings
about their children. What this means is that CLIM does not control
the geometry of the children. Instead the parent has control. "))


;;; If you get the urge to change the geometry of the children dont.

(defmethod update-mirror-transformation-1 ((port port) sheet (parent xt-geometry-manager))
  (declare (ignore sheet))
  ;; This gets called by the mirror-region-updated code.
  ;; There is probably no harm in doing this anyway
  nil)

(defmethod update-mirror-region-1 ((port port) sheet (parent xt-geometry-manager))
  (declare (ignore sheet))
  ;;--- This gets called by the invalidate-cached .. code.
  ;;---  Surely if a sheet is mirrored then you do not need to
  ;;--- invalidate any caches below that point
  nil)

#+ignore
(defmethod initialize-mirror :after ((port xt-port)
				     (parent xt-geometry-manager)
				     (parent-widget t)
				     (sheet t)
				     (widget t))
  ;; A bogus attempt to get the initial sheet-region right.
  (sheet-mirror-resized-callback nil nil nil sheet))


;; Instead if the geometry of the parent has changed, I guess this
;; suggests that the children have changed shape and that we need to
;; update their geometry.
;;;--- This seems quite bogus and what we actually need to have a
;;;--- configure-notify event handlers that deal with this.

(defmethod update-mirror-transformation-1 :after ((port port) (sheet xt-geometry-manager) (parent t))
  (update-geo-manager-sheet-children sheet))


(defmethod update-mirror-region-1 :after ((port port)
					  (sheet xt-geometry-manager)
					  (parent t))
  (update-geo-manager-sheet-children sheet))

(defmethod update-geo-manager-sheet-children (geo-manager)
  (declare (ignore geo-manager))
  ;;--- Should this really do anything???????
  #+ignore
  (dolist (child (sheet-children geo-manager))
    ;;--- Yuck!
    (when (typep child 'mirrored-sheet-mixin)
      (mirror-region-updated (port geo-manager) child))))

#|
;;


;; note-space-space-requirements-changed seems to do absolutely
;; nothing except recurse all of the way to the top.

(defmethod note-space-requirements-changed ((parent xt-geometry-manager) child)
  ;; We should now ask the parent to relayout the children
  ;; (compose-space child)
  ;; (xt-make-geometry-request), or make-resize-request...
  )

;; We want XtQueryGeometry to call out to Lisp do a compose-space and
;; return something meaningful.

;; If the parents QueryGeometry does the right thing and asks the
;; child then we are winning because we get the right numbers, except
;; this does not take into account min/max stuff.

|#


(defvar *dont-invoke-callbacks* nil)

(defmacro with-no-value-changed-callbacks (&body body)
  `(let ((*dont-invoke-callbacks* t))
     ,@body))

(defmethod queue-value-changed-event (widget sheet &optional (value (gadget-value sheet)))
  (declare (ignore widget))
  (unless *dont-invoke-callbacks*
    (distribute-event
     (port sheet)
     (allocate-event 'value-changed-gadget-event
		     :gadget sheet
		     :value value))))

(defmethod queue-losing-focus-event (widget sheet)
  (declare (ignore widget))
  (unless *dont-invoke-callbacks*
    (distribute-event
     (port sheet)
     (allocate-event 'focus-out-gadget-event
		     :gadget sheet))))

(defmethod queue-gaining-focus-event (widget sheet)
  (declare (ignore widget))
  (unless *dont-invoke-callbacks*
    (distribute-event
     (port sheet)
     (allocate-event 'focus-in-gadget-event
		     :gadget sheet))))

(defmethod queue-armed-event (widget sheet)
  (declare (ignore widget))
  (unless *dont-invoke-callbacks*
    (distribute-event
     (port sheet)
     (allocate-event 'armed-gadget-event
		     :gadget sheet))))

(defmethod queue-disarmed-event (widget sheet)
  (declare (ignore widget))
  (unless *dont-invoke-callbacks*
    (distribute-event
     (port sheet)
     (allocate-event 'disarmed-gadget-event
		     :gadget sheet))))

(defmethod queue-drag-event (widget sheet &optional (value (gadget-value sheet)))
  (declare (ignore widget))
  (distribute-event
   (port sheet)
   (allocate-event 'drag-gadget-event
     :gadget sheet
     :value value)))

(defun find-sheet-from-widget-address (address)
  (let* ((widget (tk::find-object-from-address address))
	 (display (tk::widget-display widget))
	 (port (find-port-from-display display)))
    (find-widget-sheet port widget)))

(defun find-widget-sheet (port widget &optional (errorp t))
  (cond ((gethash widget (port-mirror->sheet-table port)))
	((not errorp))
	(t (error "Could not find sheet for widget ~S" widget))))

(defun find-port-from-display (display)
  (find-if #'(lambda (port)
               (and (typep port 'xt-port)
                    (eq (port-display port) display)))
           *ports*))

(defmethod port-canonicalize-gesture-spec
    ((port xt-port) gesture-spec &optional modifier-state)
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (keysym shifts)
      (if modifier-state
	  (values gesture-spec modifier-state)
	(parse-gesture-spec gesture-spec))
    ;; Here, we must take the gesture spec, turn it back into
    ;; a keycode, then see what the keysyms are for that keycode
    (let ((x-keysym
	   (if (characterp keysym)
	       (case keysym
		 (#\newline (keysym->xt-keysym :newline))
		 (#\tab (keysym->xt-keysym :tab))
		 (#\rubout (keysym->xt-keysym :rubout))
		 (#\return (keysym->xt-keysym :return))
		 (t (and (standard-char-p keysym)
			 (keysym->xt-keysym (xt-keysym->keysym (char-code keysym))))))
	     (keysym->xt-keysym keysym)))
	  (x-keycode nil))
      (declare (ignore x-keycode))
      ;;-- Is this correct????
      (unless x-keysym
	(return-from port-canonicalize-gesture-spec nil))
      (cons (xt-keysym->keysym x-keysym) shifts))))

;;; spr24597
;;; Change call from port-set-pointer-position-1
;;; to port-set-pointer-position-root so that we don't
;;; worry about which sheet we are over.

(defmethod port-set-pointer-position ((port xt-port) pointer x y)
  (let* ((sheet (pointer-sheet pointer)))
    (port-set-pointer-position-root port sheet x y)))

;;; New function.  Put near port-set-pointer-position-1 in tk-silica/xt-silica.lisp
;;; On motif, port-set-pointer-position used to
;;; call port-set-pointer-position-1, which sets the pointer
;;; based on the underlying sheet.  
;;; We need a function which sets it explicitly relatibe
;;; to the root.
(defun port-set-pointer-position-root (port sheet x y)
  (let ((m (and
	    sheet
	    (port sheet)
	    (sheet-mirror sheet))))
    (when m
      (let ((display (port-display port)))
	(x11:xwarppointer
	 display
	 0				; src
	 (tk::display-root-window display) ; dest
	 0				; src-x
	 0				; src-y
	 0				; src-width
	 0				; src-height
	 (fix-coordinate x)
	 (fix-coordinate y))))))

(defun port-set-pointer-position-1 (port sheet x y)
  (let ((m (and (port sheet) (sheet-mirror sheet))))
    (when m
      (let ((display (port-display port)))
	(x11:xwarppointer
	 display
	 0			; src
	 (if (graftp sheet)
	     (tk::display-root-window display)
	   (tk::widget-window m)) ; dest
	 0			; src-x
	 0			; src-y
	 0			; src-width
	 0			; src-height
	 (fix-coordinate x)
	 (fix-coordinate y))))))


(defmethod raise-mirror ((port xt-port) sheet)
  (x11:xraisewindow (port-display port) (tk::widget-window (sheet-mirror sheet))))

;;; From: /net/beast/within/home/pnc/clim-sparc/acl6.0/clim2/tk-silica/xt-silica.lisp
;;; bug11288
;;; There is an X-related race-condition here.  Specifically
;;; if raise-frame is called immediately after the frame is
;;; created, then the X-side might not be ready yet.
;;; In particular, the call to widget-window will fail.
;;; So, if the call fails, we now re-try a few times.
;;; Eventually we time-out so that we don't risk an 
;;; infinite-loop.
(defmethod raise-mirror ((port xt-port) (sheet top-level-sheet))
  ;; Compensate for the top-level-sheet's mirror not being the right window.
  (x11:xraisewindow (port-display port)
		    (tk::widget-window-with-retry
		     (tk::widget-parent (sheet-mirror sheet)))))

(defmethod bury-mirror ((port xt-port) sheet)
  (x11:xlowerwindow (port-display port) (tk::widget-window (sheet-mirror sheet))))

(defmethod bury-mirror ((port xt-port) (sheet top-level-sheet))
  ;; Compensate for the top-level-sheet's mirror not being the right window.
  (x11:xlowerwindow (port-display port)
		    (tk::widget-window (tk::widget-parent (sheet-mirror sheet)))))

(defmethod enable-mirror ((port xt-port) (sheet top-level-sheet))
  (let* ((mirror (sheet-mirror sheet))
 	 (parent (widget-parent mirror))
	 (display (tk::widget-display parent)))
    (manage-child mirror)
    (xt:realize-widget parent)		; Make sure widget is realized.
    (let ((window (tk::widget-window parent)))
      (x11:xdefinecursor display window
			 (realize-cursor port sheet
					 (or (sheet-pointer-cursor sheet)
					     :default)))
      (let ((ussp (slot-value sheet 'silica::user-specified-size-p))
	    (uspp (slot-value sheet 'silica::user-specified-position-p))
	    (size-hints (x11::xallocsizehints)))
	(tk::with-ref-par ((supplied 0 :unsigned-long))
	  (unless (zerop
		   (x11::xgetwmnormalhints display window size-hints &supplied))
	    (let ((flags (x11::xsizehints-flags size-hints)))
	      (if* (and uspp (not (eq uspp :unspecified)))
		 then (setf flags (logior flags x11::uspositionhint))
	       elseif (null uspp)
		 then (setf flags (logandc2 flags x11::uspositionhint)))
	      (if* (and ussp (not (eq ussp :unspecified)))
		 then (setf flags (logior flags x11::ussizehint))
	       elseif (null ussp)
		 then (setf flags (logandc2 flags x11::ussizehint)))
	      ;; always switch off program-specified position hint so
	      ;; that OpenWindows cascading works
	      (setf flags (logandc2 flags x11::ppositionhint))
	      (setf (x11::xsizehints-flags size-hints) flags))
	    (x11::xsetwmnormalhints display window size-hints)))))
    (popup parent)))

;;; so is this next method ever called???? let's comment it out and
;;; see... (cim 4/25/94)

#+ignore
(defmethod enable-xt-widget ((parent top-level-shell) (mirror t))
  (manage-child mirror)
  (popup (widget-parent mirror)))


(defmethod mirror-visible-p ((port xt-port) sheet)
  (let ((mirror (sheet-mirror sheet)))
    ;;--- This costs a round trip to the display server.  A better
    ;;--- scheme would be to have the map/unmap-notify set a flag
    ;;--- that we could simply read here, as in CLIM 1.1
    (eq (tk::window-map-state (tk::widget-window mirror))
	:viewable)))

;;;---- Cursor stuff

;;; if you're feeling bored extend and replace constants with
;;; x11:xc-xxx

(defparameter *xt-cursor-type-alist*
    `((:default 68)
      (:vertical-scroll 116)
      (:scroll-up 114)
      (:scroll-down 106)
      (:horizontal-scroll 108)
      (:scroll-left 110)
      (:scroll-right 112)
      (:busy 150)
      (:upper-left 134)
      (:upper-right 136)
      (:lower-left 12)
      (:lower-right 14)
      (:vertical-thumb 112)
      (:horizontal-thumb 114)
      (:button 68)
      (:prompt 68)
      (:move 52)
      (:position 34)
      (:menu ,x11:xc-arrow)))

(defmethod port-set-pointer-cursor ((port xt-port) pointer cursor)
  (unless (eq (pointer-cursor pointer) cursor)
    (let* ((cursor (realize-cursor port nil cursor))
	   (sheet (pointer-sheet pointer))
	   (widget (sheet-direct-mirror
		    (let ((frame (pane-frame sheet)))
		      (if frame
			  (frame-top-level-sheet frame)
			sheet))))
	   (window (and widget (tk::widget-window widget nil))))
      (when window
	(x11:xdefinecursor
	 (port-display port) window cursor)
	(port-force-output port))))
  cursor)


(defmethod port-set-sheet-pointer-cursor ((port xt-port) sheet cursor)
  (unless (eq (sheet-pointer-cursor sheet)  cursor)
    (let* ((cursor (realize-cursor port sheet cursor))
	   (widget (sheet-mirror sheet))
	   (window (tk::widget-window widget nil)))
      (when window
	(x11:xdefinecursor
	 (port-display port) window cursor)
	(port-force-output port))))
  cursor)

(defmethod realize-cursor :around ((port xt-port) sheet cursor)
  (declare (ignore sheet))
  (with-slots (cursor-cache) port
    (or (getf cursor-cache cursor)
	(setf (getf cursor-cache cursor)
	  (call-next-method)))))

(defmethod realize-cursor ((port xt-port) sheet (cursor null))
  (declare (ignore sheet))
  x11:none)

(defmethod realize-cursor ((port xt-port) sheet (cursor symbol))
  (let ((cursor (or (second (assoc cursor *xt-cursor-type-alist*))
		    (error "Unknown cursor name: ~S" cursor))))
    (realize-cursor port sheet cursor)))

(defmethod realize-cursor ((port xt-port) sheet (cursor number))
  (declare (ignore sheet))
  (x11:xcreatefontcursor (port-display port) cursor))

;;;--could this be made to use pixmap-from pattern in xt-graphics?

(defmethod realize-cursor ((port xt-port) sheet (cursor pattern))
  (multiple-value-bind (array designs)
      (decode-pattern cursor)
    ;; Three cases:
    ;; (ink1 ink2)
    ;; (nowhere ink1)
    ;; (nowhere ink1 ink2)
    (multiple-value-bind (offset maskp ink1 ink2)
	(cond
	 ((= (length designs) 3)
	  (assert (eq (elt designs 0) +nowhere+))
	  (values 0 t (elt designs 1) (elt designs 2)))
	 ((= (length designs) 2)
	  (if (eq (elt designs 0) +nowhere+)
	      (values 0 t (elt designs 1) (elt designs 1))
	    (values 1 nil (elt designs 0) (elt designs 1))))
	 (t (error "Cannot handle this cursor pattern ~S" cursor)))
      (let* ((height (array-dimension array 0))
	     (width (array-dimension array 1))
	     (pixmap-data (make-array (list height width)))
	     (mask-data (and maskp (make-array (list height width))))
	     (design-pixels (make-array (length designs))))

	(flet ((doit (sheet)
		 (with-sheet-medium (medium sheet)
		   (dotimes (n (length designs))
		     (let ((design (elt designs n)))
		       (setf (svref design-pixels n)
			 (and (not (eql design +nowhere+))
			      (decode-color design medium))))))))
	  (if sheet
	      (doit sheet)
	    (with-menu (sheet (find-graft :port port))
	      (doit sheet))))

	(dotimes (w width)
	  (dotimes (h height)
	    (let ((pixel (+ offset (aref array h w))))
	      (setf (aref pixmap-data h w)
		(ecase pixel
		  (0 0)
		  (1 0)
		  (2 1)))
	      (when maskp
		(setf (aref mask-data h w)
		  (if (eq pixel 0) 0 1))))))


	(let* ((display (port-display port))
	       (drawable (tk::display-root-window display))
	       (depth 1)
	       (pixmap-image (make-instance 'tk::image
			       :display display
			       :width width
			       :height height
			       :data pixmap-data
			       :depth depth))
	       (mask-image (and maskp
				(make-instance 'tk::image
				  :display display
				  :width width
				  :height height
				  :data mask-data
				  :depth depth)))
	       (pixmap
		(make-instance 'tk::pixmap
			       :drawable drawable
			       :width width
			       :height height
			       :depth depth))
	       (mask
		(and maskp
		     (make-instance 'tk::pixmap
				    :drawable drawable
				    :width width
				    :height height
				    :depth depth))))
	  (tk::put-image pixmap
			 (port-copy-gc-depth-1 port)
			 pixmap-image)
	  (tk::destroy-image pixmap-image)
	  (when maskp
	    (tk::put-image mask
			   (port-copy-gc-depth-1 port)
			   mask-image)
	    (tk::destroy-image mask-image))
	  (prog1
	      (x11:xcreatepixmapcursor
	       display
	       pixmap
	       (or mask 0)
	       (multiple-value-bind (red green blue)
		   (color-rgb ink1)
		 (let* ((x #.(1- (ash 1 16))))

		   (make-instance 'tk::color
				  :red (truncate (* x red))
				  :green (truncate (* x green))
				  :blue (truncate (* x blue)))))
	       (multiple-value-bind (red green blue)
		   (color-rgb ink2)
		 (let* ((x #.(1- (ash 1 16))))
		   (make-instance 'tk::color
				  :red (truncate (* x red))
				  :green (truncate (* x green))
				  :blue (truncate (* x blue)))))
	       0
	       0)
	    (tk::destroy-pixmap pixmap)
	    (when mask (tk::destroy-pixmap mask))))))))

(defvar *pointer-grabbed* nil)

(defmethod port-invoke-with-pointer-grabbed
    ((port xt-port) (sheet basic-sheet) continuation
     &key confine-to cursor (ungrab-on-error t))
  (let ((widget (sheet-mirror sheet)))
    (unwind-protect
	(progn
	  (tk::xt_grab_pointer
	   widget
	   ;; we make this true so that scroll-bars can work in
	   ;; menu-choose-from-drawer windows (cim 10/13/94)
	   1				; owner-events
	   (xt-grabbed-event-mask)	; Event-mask
	   x11:grabmodeasync		; pointer-grab-mode
	   x11:grabmodeasync		; keyboard
	   (if confine-to (tk::widget-window widget) 0) ; confine to
	   (realize-cursor port sheet cursor)
	   0				; current-time
	   )
	  (handler-bind ((error #'(lambda (condition)
				    (declare (ignore condition))
				    (when ungrab-on-error
				      (tk::xt_ungrab_pointer
				       widget 0)))))
	    (let ((*pointer-grabbed* t))
	      (funcall continuation))))
      (tk::xt_ungrab_pointer widget 0))))

(defmethod port-pointer-grabbed-p ((port xt-port))
  (let ((display (port-display port)))
    (cond
     ((not (zerop (x11:xgrabpointer
		     display
		     (x11:xdefaultrootwindow display)
		     0
		     x11:noeventmask
		     x11:grabmodesync
		     x11:grabmodesync
		     0
		     0
		     x11:currenttime))))
     (t (x11:xungrabpointer display x11:currenttime)
	nil))))

(defmethod port-remove-all-pointer-grabs ((port xt-port))
  (x11:xungrabpointer (port-display port) x11:currenttime))

(defun xt-grabbed-event-mask ()
  (tk::encode-event-mask '(:enter-window
			   :leave-window
			   :pointer-motion-hint
			   :pointer-motion
			   :button-motion
			   :button-press
			   :button-release
			   )))

(defmethod port-set-sheet-grabbed-pointer-cursor ((port xt-port) sheet cursor)
  (declare (ignore sheet))
  (when *pointer-grabbed*
    (x11::xchangeactivepointergrab
     (port-display port)
     (xt-grabbed-event-mask)					; event-mask
     (if cursor (realize-cursor port cursor) 0)
     0					; time
     )))

(defmethod port-move-frame ((port xt-port) frame x y)
  (fix-coordinates x y)
  (check-type x (signed-byte 16))
  (check-type y (signed-byte 16))
  (tk::set-values
   ;; this is to make sure that position-sheet-carefully works
   ;; correctly on Motif for _both_ dialogs and regular frames
   ;; (cim 12/13/94)
   (cond ((typep frame 'tk-silica::motif-menu-frame)
	  ;; spr25913
	  ;; For this type of frame, calling set-values on the 
	  ;; sheet-direct-mirror does not cause the frame to move.
	  ;; For now, be paranoid and specialize only on the 
	  ;; specific class.
	  (frame-shell frame))
	 ((popup-frame-p frame)
	  (sheet-direct-mirror (frame-top-level-sheet frame)))
	 (t 
	  (frame-shell frame)))
   :x x :y y))

(defmethod port-resize-frame ((port xt-port) frame width height)
  (check-type width (signed-byte 16))
  (check-type height (signed-byte 16))
  (tk::set-values (frame-shell frame) :width width :height height))

;;; why don't we use the default in silica/port.lisp???

(defmethod destroy-port ((port xt-port))
  (excl:without-interrupts
    (tk::xt_destroy_application_context (port-context port))
    (when (port-process port)
      (clim-sys:destroy-process (port-process port)))
    (port-terminated port (make-condition 'error))))

(defmethod clim-internals::port-query-pointer ((port xt-port) sheet)
  (multiple-value-bind (same-p root child root-x root-y
			native-x native-y state)
      (tk::query-pointer (tk::widget-window (sheet-mirror sheet)))
    (declare (ignore state child root same-p))
    (multiple-value-bind (x y)
	(untransform-position (sheet-device-transformation sheet) native-x native-y)
      (values x y native-x native-y root-x root-y))))


(defmethod clim-internals::port-query-pointer ((port xt-port) (sheet graft))
  (multiple-value-bind (same-p root child root-x root-y
			native-x native-y state)
      (tk::query-pointer (tk::display-root-window (port-display port)))
    (declare (ignore state child root same-p))
    (values native-x native-y native-x native-y root-x root-y)))


(defmacro with-toolkit-dialog-component ((tag value) &body body)
  `(letf-globally (((getf (mp:process-property-list mp:*current-process*) ',tag) ,value))
       ,@body))


