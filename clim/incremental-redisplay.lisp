;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: incremental-redisplay.lisp,v 1.7 92/05/22 19:28:01 cer Exp Locker: cer $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 International Lisp Associates."

;; this stuff belongs in other files...

;; needs to be atomically updated on architectures with multiple processes.
(defvar *generation-tick* 0)

;; note that this returns the new value (depending on INCF for value -- byechh).
(defmacro atomic-incf (reference)
  #+Genera `(process:atomic-incf ,reference)
  #+Minima `(incf ,reference)
  ;; Probably should use a lock, or something...
  #-(or Genera Minima) `(incf ,reference))


;;; The incremental redisplay protocol:
;;; See incremental-redisplay-protocol.text
;;; We use the term "extent" here to mean a bounding rectangle...

(define-protocol-class updating-output-record (output-record))

(defclass standard-updating-output-record 
	  (standard-sequence-output-record updating-output-record)
     ((unique-id :initarg :unique-id
		 :accessor output-record-unique-id)
      (cache-value :initarg :cache-value
		   :accessor output-record-cache-value)
      (fixed-position :initform nil :initarg :fixed-position
		      :accessor output-record-fixed-position)
      (displayer :initarg :displayer
		 :accessor output-record-displayer)
      (all-new :initform nil :initarg :all-new)
      (cache :initform nil)
      (old-cache :initform nil)
      ;; position is in absolute co-ordinates, since
      ;; UPDATING-OUTPUT-RECORD's can be easily moved through the
      ;; hierarchy, in which case it would be slightly expensive to
      ;; convert from relative to absolute coordinates.
      (old-position :initform (make-point 0 0) 
		    :initarg :old-position
		    :accessor output-record-old-cursor-position)
      (old-parent :initform nil :initarg :old-parent
		  :accessor output-record-old-parent)))

(define-output-record-constructor standard-updating-output-record
				  (&key x-position y-position (size 25)
					unique-id cache-value fixed-position
					displayer all-new
					(old-position (make-point 0 0))
					old-parent)
  :x-position x-position :y-position y-position :size size
  :unique-id unique-id :cache-value cache-value :fixed-position fixed-position
  :displayer displayer :all-new all-new :old-position old-position :old-parent old-parent)

;; Determines whether a record gets moved if its siblings adjust their
;; size.  Default is NIL - it >does< get moved (its position is >not<
;; fixed relative to its parent).
(defmethod output-record-fixed-position ((record output-record-element-mixin))
  nil)

(defmethod output-record-unique-id ((record output-record-element-mixin))
  nil)

(defmethod copy-display-state ((record output-record-element-mixin) old-is-ok)
  (with-slots (old-bounding-rectangle start-x start-y contents-ok parent) record
    ;; Don't cons a rectangle to hold the old bounding-rect until we need one
    (if (null old-bounding-rectangle)
	(setf old-bounding-rectangle (bounding-rectangle record))	;cons a new rectangle
	(bounding-rectangle record old-bounding-rectangle))
    (output-record-set-old-start-cursor-position record start-x start-y)
    (setf contents-ok (not (null old-is-ok)))
    (unless old-is-ok
      (setf parent nil))))

(defmethod copy-display-state :after ((record output-record-mixin) old-is-ok)
  (setf (slot-value record 'old-children) (output-record-children record))
  (unless old-is-ok
    (clear-output-record record)
    ;; let descendants know that they should refer to OLD-xxx if they
    ;; want to get information about what's currently on the screen.
    (setf (output-record-generation-tick record) *generation-tick*)))

(defmethod copy-display-state :after ((record standard-updating-output-record) old-is-ok)
  (setf (slot-value record 'old-cache) (slot-value record 'cache))
  (unless old-is-ok
    (setf (slot-value record 'cache) nil)))

(defmethod match-output-records ((record1 output-record-element-mixin)
				 &rest init-args)
  (declare (ignore init-args))
  ;; this is questionable default behavior.  What I really want is to
  ;; find the original init-args, and make sure they are equal to the
  ;; new ones.
  ;; If I can't do that, I'd better return nil.
  ;;
  ;; But, that cuts out an entire class of matches, and makes it seem
  ;; like we have overlapping presentations.  This wouldn't be a
  ;; problem, except in the case where we did an updating-output inside
  ;; some output-record that didn't match.  Since it didn't match, we
  ;; would try to delete it.  But, updating-output would like to copy it
  ;; from the screen.
  ;;
  ;; If we match when we shouldn't, then the slots of an output record can be wrong.
  ;; This is unacceptable.  If we don't match when we should, then the update will be
  ;; flashier.
  ;;
  ;; So, the solution we come up with is to be robust against both types
  ;; of error.  If we don't match, and hence delete something, we now
  ;; check (see COMPUTE-DIFFERENCE-SET :AROUND).  And we are careful not to
  ;; match for classes that have slots other than those involved in
  ;; incremental-redisplay (see match-output-records for presentation).
  ;;
  ;; We default to T (for the moment), and require classes built on top
  ;; of output-record-element-mixin to do the right thing in
  ;; match-output-records.  In the long term, the default for match
  ;; should be nil, but we should copy the display-state anyway.

  t)


;; The reason that this is called FIND-CHILD-OUTPUT-RECORD-1, and it calls
;; plain old vanilla FIND-CHILD-OUTPUT-RECORD, is that this is the function
;; called internally by incremental redisplay, and it just massages some of
;; the arguments, and then passes it on to the real (and generic)
;; FIND-CHILD-OUTPUT-RECORD.  FIND-CHILD-OUTPUT-RECORD is the exported
;; interface, and is defined in the protocol.
(defun find-child-output-record-1 (record record-type &rest init-args)
  (declare (dynamic-extent init-args))
   (apply #'find-child-output-record
	  record
	  (and (not (output-record-contents-ok record))
	       (eql (output-record-generation-tick record) *generation-tick*))
	  record-type
	  init-args))

(defun decache-child-output-record-1 (record child)
  (decache-child-output-record
    record child
    (and (not (output-record-contents-ok record))
	 (eql (output-record-generation-tick record) *generation-tick*))))

(defun find-cached-output-record-1 (record record-type &rest init-args)
  (declare (dynamic-extent init-args))
   (apply #'find-cached-output-record
	  record
	  (and (not (output-record-contents-ok record))
	       (eql (output-record-generation-tick record) *generation-tick*))
	  record-type
	  init-args))

(defun find-with-test (item sequence key test)
  ;;--- Allegro blows up mysteriously in the call to FIND below, so
  ;;--- instead use this "by hand" code.
  (flet ((robust-test (item1 item2)
	   ;; Suppose someone has some IDs that are numbers and others
	   ;; that are strings, and one of the ID tests is STRING-EQUAL?
	   (and (eq (class-of item1) (class-of item2))
		(funcall test item1 item2))))
    (declare (dynamic-extent #'robust-test))
    #+ignore
    (block work-around-franz-find-bug
      (dolist (candidate sequence)
	(when (funcall (if test #'robust-test #'eql) item (funcall key candidate))
	  (return-from work-around-franz-find-bug candidate))))
    #-ignore
    (if test
	(find item sequence :key key :test #'robust-test)
        (find item sequence :key key))))

(defmethod find-child-output-record
	   ((record output-record-mixin) use-old-children record-type
	    &rest init-args &key unique-id id-test &allow-other-keys)
  (declare (dynamic-extent init-args))
  ;; Other types can write their own FIND-CHILD-OUTPUT-RECORD.  This is 
  ;; the default, stupid one.
  (flet ((do-match (candidate)
	   ;; (class-name (class-of ...)) should just be type-of, but not in PCL.
	   (and (eq record-type (class-name (class-of candidate)))
		(apply #'match-output-records candidate init-args))))
    (let ((elts-to-find (when use-old-children (output-record-old-children record))))
      (if use-old-children
	  (let ((found-record
		  (if unique-id
		      (find-with-test unique-id elts-to-find
				      #'output-record-unique-id id-test)
		      ;; UNIQUE-ID can be NIL when we are coming through
		      ;; INVOKE-WITH-NEW-OUTPUT-RECORD to create new records
		      (or (and (do-match (first elts-to-find)) (first elts-to-find))
			  (let ((candidate (second elts-to-find)))
			    (and (do-match candidate)
				 ;; assume the first one was deleted, and
				 ;; therefore we don't want to match
				 ;; against it anymore.
				 ;; If they want better performance, they
				 ;; should use UID's.
				 (setf (output-record-old-children record)
				       (nconc (rest elts-to-find)
					      (list (first elts-to-find))))
				 candidate))))))
	    (when found-record
	      (setf (output-record-old-children record)
		    (delete found-record (output-record-old-children record)))
	      found-record))
	  (block find-one
	    (flet ((unique-id-test (candidate)
		     (when (funcall id-test unique-id (output-record-unique-id candidate))
		       (return-from find-one candidate)))
		   (unique-id-no-id-test (candidate)
		     (when (eql unique-id (output-record-unique-id candidate))
		       (return-from find-one candidate)))
		   (no-unique-id (candidate)
		     (when (do-match candidate)
		       (return-from find-one candidate))))
	      (declare (dynamic-extent #'unique-id-test #'unique-id-no-id-test #'no-unique-id))
	      (map-over-output-records 
		(if unique-id
		    (if id-test #'unique-id-test #'unique-id-no-id-test)
		    #'no-unique-id) 
		record)))))))

(defmethod decache-child-output-record
	   ((record output-record-mixin) child use-old-children)
  (when use-old-children
    (with-slots (old-children) record
      (setf old-children (delete child old-children)))))

;; also add the following:
;;   stream-redisplaying-p: (output-recording-stream), setf
;;   redisplayable-stream-p (output-recording-stream)

;; and a slot, stream-redisplay-record

;; this is only for the benefit of COMPUTE-DIFFERENCE-SET, so it won't have
;; to look inside this record.
;;
;; You can replace this method (as STANDARD-TEXT-OUTPUT-RECORD does), if
;; you want to reconsider the match after you do the output, to make
;; redisplay less flashy.  For example, STANDARD-TEXT-OUTPUT-RECORD
;; checks to see if the string is the same, and the style-changes
;; are the same, and if so, marks the text-record as contents-ok, so you
;; can just move it around the screen rather than having to do an erase
;; and a draw.  The big advantage, of course, comes when the record
;; hasn't moved.
(defmethod recompute-contents-ok ((record output-record-element-mixin))
  nil)

(defmethod recompute-contents-ok :around ((record output-record-element-mixin))
  (or (output-record-contents-ok record)
      (call-next-method)))

(defmethod recompute-contents-ok ((record output-record-mixin))
  (with-slots (old-bounding-rectangle) record
    (let ((delta-x nil) (delta-y nil))
      ;; There's no point in checking, if any old-children exist, because that means
      ;; that there's stuff that needs deleting.
      (when (and (null (output-record-old-children record))
		 old-bounding-rectangle
		 (bounding-rectangle-size-equal record old-bounding-rectangle))
	(flet ((recompute (child)
		 (unless (and (output-record-contents-ok child)
			      (or (multiple-value-bind (width height)
				      (bounding-rectangle-size child)
				    (declare (type coordinate width height))
				    (and (zerop width) (zerop height)))
				  (multiple-value-bind (our-delta-x our-delta-y)
				      (multiple-value-bind (sx sy)
					  (output-record-start-cursor-position child)
					(declare (type coordinate sx sy))
					(multiple-value-bind (osx osy)
					    (output-record-old-start-cursor-position child)
					  (declare (type coordinate osx osy))
					  (position-difference sx sy osx osy)))
				    (declare (type coordinate our-delta-x our-delta-y))
				    (if (null delta-x)
					(setf delta-x our-delta-x delta-y our-delta-y)
				        (and (= delta-x our-delta-x)
					     (= delta-y our-delta-y))))))
		   (return-from recompute-contents-ok nil))))
	  (declare (dynamic-extent #'recompute))
	  (map-over-output-records #'recompute record))
	;; If we reached here, then the contents are ok, but they are shifted.
	;; Make sure that bounding rectangle of the record agrees.
	(when (or (not delta-x)
		  (multiple-value-bind (width height)
		      (bounding-rectangle-size record)
		    (declare (type coordinate width height))
		    (and (zerop width) (zerop height)))
		  (multiple-value-bind (e-delta-x e-delta-y)
		      (if old-bounding-rectangle
			  (bounding-rectangle-position-difference
			    record old-bounding-rectangle)
			  (values (coordinate 0) (coordinate 0)))
		    (and (= delta-x e-delta-x) (= delta-y e-delta-y))))
	  (setf (output-record-contents-ok record) t)
	  ;; If delta-x wasn't set, then we didn't see any relevant children.
	  ;; So, this had, and has, zero extent, so it isn't relevant.
	  (when delta-x
	    ;; Just shift the old start position.  We don't have to
	    ;; readjust the old start positions of the children because
	    ;; COMPUTE-DIFFERENCE-SET will never walk down past this record,
	    ;; since contents-ok is T.
	    (multiple-value-bind (new-x new-y)
		(output-record-start-cursor-position record)
	      (declare (type coordinate new-x new-y))
	      (output-record-set-old-start-cursor-position
		record (- new-x delta-x) (- new-y delta-y)))))))))


;; Internal protocol:

(defmacro with-stream-redisplaying ((stream) &body body)
  `(letf-globally (((stream-redisplaying-p ,stream) t))
     ;; the generation is only necessary for output-records that move
     ;; in the hierarchy.
     (let ((*generation-tick* (atomic-incf *generation-tick*)))
       ,@body)))

(defun redisplay (record stream &key (check-overlapping t))
  (unless (redisplayable-stream-p stream)
    (cerror "Output on stream anyway"
	    "Stream ~S doesn't support incremental redisplay" stream))
  (redisplay-output-record record stream check-overlapping))

;; X and Y should be in coordinates relative to sup-x, sup-y.
(defmethod redisplay-output-record ((record output-record-mixin) stream
				    &optional check-overlapping x y parent-x parent-y)
  (multiple-value-bind (sup-x sup-y)
      (convert-from-relative-to-absolute-coordinates stream (output-record-parent record))
    (declare (type coordinate sup-x sup-y))
    (multiple-value-bind (x y)
	(if y
	    (values (+ x (- parent-x sup-x)) (+ y (- parent-y sup-y)))
	    (output-record-position record))
      (declare (type coordinate x y))
      (with-end-of-page-action (stream :allow)
	(with-stream-cursor-position-saved (stream)
	  (stream-set-cursor-position
	    stream (+ sup-x x) (+ sup-y y))
	  (with-stream-redisplaying (stream)
	    (let ((parent (output-record-parent record)))
	      ;; Need to delete it, because its bounding rectangle is about to be bashed.
	      ;; It will be reinserted, as a matter of course.  Watch out for when
	      ;; the parent is NIL or when the record didn't really get inserted into
	      ;; the parent, which can happen if a user aborts a redisplay.
	      (when parent
		(delete-output-record record parent nil))
	      (flet ((redisplay-new-output-records (parent)
		       (declare (ignore parent))
		       (with-output-recording-options (stream :draw nil) 
			 (compute-new-output-records record stream))))
		(declare (dynamic-extent #'redisplay-new-output-records))
		(with-output-record-1 #'redisplay-new-output-records
				      stream parent sup-x sup-y))))))
      (multiple-value-bind (erases moves draws erase-overlapping move-overlapping)
	  (compute-difference-set record t)
	(when check-overlapping
	  (multiple-value-setq (erases moves draws erase-overlapping move-overlapping)
	    (augment-draw-set record erases moves draws erase-overlapping move-overlapping)))
	(note-output-record-child-changed
	  (output-record-parent record) record :change
	  ;; You can use the state saved by COPY-DISPLAY-STATE here, since the
	  ;; contract of COMPUTE-NEW-OUTPUT-RECORDS is to maintain the old state.
	  (multiple-value-bind (x y)
	      (output-record-old-start-cursor-position record)
	    (make-point x y))
	  (output-record-old-bounding-rectangle record)
	  stream
	  erases moves draws erase-overlapping move-overlapping))))
  (force-output stream))

;; If you make a change to an existing output-record you must call
;; NOTE-OUTPUT-RECORD-CHILD-CHANGED on its parent, so that the parent
;; can choose to readjust itself based on those changes.
;; NOTE-OUTPUT-RECORD-CHILD-CHANGED assumes that the output-history already
;; reflects the changes made, but that no output (screen, or hardcopy, or
;; ...) has been done yet.  (Some) parent will (eventually) do the
;; appropriate updates.  You must pass the differences list (output in the
;; style of COMPUTE-DIFFERENCE-SET) to NOTE-OUTPUT-RECORD-CHILD-CHANGED.
;; If you don't know the optimized form of the differences, then you must
;; call COMPUTE-DIFFERENCE-SET before you update the output-record history,
;; and pass the results to NOTE-OUTPUT-RECORD-CHILD-CHANGED.  (See, for
;; example, redisplay).

;; Mode is one of :DELETE, :ADD, :CHANGE, :MOVE, or :NONE
;; If you don't recursively call your parent, then it is your responsibility
;; to call INCREMENTAL-REDISPLAY.
(defmethod note-output-record-child-changed 
	   ((parent output-record-mixin) child mode
	    old-child-position old-child-extent	;of child
	    stream
	    &optional erases moves draws erase-overlapping move-overlapping)
  (cond ((eq mode :add)
	 (add-output-record child parent))
	((eq mode :delete)
	 (delete-output-record child parent)))
  (cond
    ((propagate-output-record-changes-p parent child mode 
					old-child-position old-child-extent)
     (with-slots (old-bounding-rectangle start-x start-y old-start-x old-start-y) parent
       (let ((old-parent-extent old-bounding-rectangle)
	     (old-parent-position
	       (multiple-value-bind (x y)
		   (output-record-old-start-cursor-position parent)
		 (make-point x y))))
	 (setf old-start-x start-x old-start-y start-y)
	 (if (null old-bounding-rectangle)
	     (setf old-bounding-rectangle (bounding-rectangle parent))
	     (bounding-rectangle parent old-bounding-rectangle))
	 (multiple-value-bind (new-mode new-erases new-moves new-draws
			       new-erase-overlapping new-move-overlapping)
	     (propagate-output-record-changes parent child mode 
					      old-child-position old-child-extent
					      erases moves draws
					      erase-overlapping move-overlapping)
	   (when (output-record-parent parent)
	     (note-output-record-child-changed
	       (output-record-parent parent) parent new-mode
	       old-parent-position old-parent-extent stream 
	       new-erases new-moves new-draws
	       new-erase-overlapping new-move-overlapping))))))
    (t
     (incremental-redisplay stream 
			    (multiple-value-bind (x y)
				(output-record-start-cursor-position parent)
			      (make-point x y))
			    erases moves draws erase-overlapping move-overlapping))))

;; for efficiency we might want to pass in the five elements as an
;; array, or structure.  But we'll wait until we meter that to decide...
;;
;; The difference list returned from this is in
;; coordinates relative to the parent of first record you called
;; COMPUTE-DIFFERENCE-SET on.  [OLD-]X/Y-OFFSET gives the absolute offsets
;; from that origin to the parent of RECORD.
;;
;; Need to clip this to the visible viewport, but we'll do that later, too.
(defmethod compute-difference-set
	   ((record output-record-element-mixin)
	    &optional (check-overlapping nil)
		      (x-offset (coordinate 0)) (y-offset (coordinate 0))
		      (old-x-offset (coordinate 0)) (old-y-offset (coordinate 0)))
  (declare (type coordinate x-offset y-offset old-x-offset old-y-offset))
  (declare (values erases moves draws erase-overlapping move-overlapping))
  (let (erases moves draws erase-overlapping move-overlapping)
    (flet ((erase (record region)
	     ;; REGION is the bounding rectangle
	     (when region
	       (multiple-value-bind (width height)
		   (bounding-rectangle-size region)
		 (declare (type coordinate width height))
		 (unless (and (zerop width) (zerop height))
		   (push (list record
			       (bounding-rectangle-shift-position
				 region old-x-offset old-y-offset))
			 erases)))))
	   (move (record old-bounding-rectangle)
	     ;;--- It's probably a bug if OLD-BOUNDING-RECTANGLE is NIL
	     ;;--- but do we want to penalize the user?
	     (when old-bounding-rectangle
	       (multiple-value-bind (e-x e-y)
		   (bounding-rectangle-position record)
		 (declare (type coordinate e-x e-y))
		 (multiple-value-bind (old-e-x old-e-y)
		     (bounding-rectangle-position old-bounding-rectangle)
		   (declare (type coordinate old-e-x old-e-y))
		   (unless (and (= (+ x-offset e-x) (+ old-x-offset old-e-x))
				(= (+ y-offset e-y) (+ old-y-offset old-e-y)))
		     (push (list record
				 (bounding-rectangle-shift-position
				   old-bounding-rectangle old-x-offset old-y-offset)
				 (bounding-rectangle-shift-position
				   record x-offset y-offset))
			   moves))))))
	   (draw (record region)
	     (push (list record
			 (bounding-rectangle-shift-position
			   region x-offset y-offset))
		   draws)))
      (declare (dynamic-extent #'erase #'move #'draw))
      (with-slots (old-bounding-rectangle contents-ok) record
	(cond (contents-ok
	       ;; just check position, we know bounding-rect is ok if contents is ok.
	       (move record old-bounding-rectangle))
	      (t
	       (when (displayed-output-record-p record)
		 ;; It's a displayed output record element, erase and redraw it.
		 (erase record old-bounding-rectangle)
		 (draw record (bounding-rectangle record)))
	       (when (output-record-p record)
		 ;; We have to look at the children.
		 (multiple-value-bind (start-x start-y)
		     (output-record-start-cursor-position record)
		   (declare (type coordinate start-x start-y))
		   (multiple-value-bind (o-start-x o-start-y)
		       (output-record-old-start-cursor-position record)
		     (declare (type coordinate o-start-x o-start-y))
		     (dolist (child (output-record-old-children record))
		       (erase child (bounding-rectangle-shift-position
				      child o-start-x o-start-y)))
		     (let ((x-offset (+ x-offset start-x))
			   (y-offset (+ y-offset start-y))
			   (old-x-offset (+ old-x-offset o-start-x))
			   (old-y-offset (+ old-y-offset o-start-y)))
		       (flet ((compute-diffs (child)
				(multiple-value-bind (nerases nmoves ndraws
						      nerase-overlapping nmove-overlapping)
				    (compute-difference-set
				      child nil
				      x-offset y-offset old-x-offset old-y-offset)
				  (setq erases (append erases nerases))
				  (setq moves (append moves nmoves))
				  (setq draws (append draws ndraws))
				  (setq erase-overlapping
					(append erase-overlapping nerase-overlapping))
				  (setq move-overlapping
					(append move-overlapping nmove-overlapping)))))
			 (declare (dynamic-extent #'compute-diffs))
			 (map-over-output-records #'compute-diffs record))))))))))
    (values erases moves draws erase-overlapping move-overlapping)))

;; If there are any output records that can have overlapping children,
;; none of which have been inserted into the "erase" or "draw" sets, but
;; overlap anything in the "erase" set, then we must insert the child
;; into the "draw" set so that it does not simply disappear.
(defmethod augment-draw-set ((record output-record-element-mixin)
			     erases moves draws erase-overlapping move-overlapping
			     &optional (x-offset (coordinate 0)) (y-offset (coordinate 0))
				       (old-x-offset (coordinate 0)) (old-y-offset (coordinate 0)))
  (declare (type coordinate x-offset y-offset old-x-offset old-y-offset))
  (declare (values erases moves draws erase-overlapping move-overlapping))
  (labels ((augment-draws (record x-offset y-offset old-x-offset old-y-offset)
	     (when (and (displayed-output-record-p record)
			(not (children-never-overlap-p (output-record-parent record)))
			(not (member record draws :key #'first))
			(not (member record erases :key #'first))
			(dolist (erase erases nil)
			  (when (region-intersects-region-p record (first erase))
			    (return t))))
	       (push (list record
			   (bounding-rectangle-shift-position
			     record x-offset y-offset))
		     draws))
	     (when (output-record-p record)
	       (multiple-value-bind (start-x start-y)
		   (output-record-start-cursor-position record)
		 (declare (type coordinate start-x start-y))
		 (multiple-value-bind (o-start-x o-start-y)
		     (output-record-old-start-cursor-position record)
		   (declare (type coordinate o-start-x o-start-y))
		   (let ((x-offset (+ x-offset start-x))
			 (y-offset (+ y-offset start-y))
			 (old-x-offset (+ old-x-offset o-start-x))
			 (old-y-offset (+ old-y-offset o-start-y)))
		     (map-over-output-records
		       #'augment-draws record (coordinate 0) (coordinate 0)
		       x-offset y-offset old-x-offset old-y-offset)))))))
    (declare (dynamic-extent #'augment-draws))
    (augment-draws record x-offset y-offset old-x-offset old-y-offset))
  (values erases moves draws erase-overlapping move-overlapping))

;; This has nothing to do with output-recording.  You can call this on any
;; stream that can set the cursor position, replace existing output, and
;; can do bitblt's, although I suppose we could make the last part generic.
(defmethod incremental-redisplay ((stream output-protocol-mixin) position erases moves draws
				  erase-overlapping move-overlapping)
  (with-output-recording-options (stream :draw t :record nil)
    (multiple-value-bind (xoff yoff)
	(bounding-rectangle-position position)
      (declare (type coordinate xoff yoff))
      ;; Do the erases first, then the moves, then the draws.
      ;; Do the draws in the "right" order.
      ;; If a move, (not a draw, which might not use ALU-SETA), goes over an
      ;; erase, we don't have to do that erase.
      ;; Only do stuff inside the viewport.
      ;; Worry about offscreen copies.
      ;; Glom together, where possible.
      #+ignore (---bitblt-optimizations---)
      ;; In order to test this, we can do the simple thing first:
      ;; do the erases, erase all the moves, do the moves as if they were
      ;; draws (so the order doesn't matter), and then do the draws.
      ;; This is a good way to guarantee that we have >something< working for AAAI.
      ;; 
      ;; All of this must be done relative to position...
      (flet ((erase-rectangle (stream rectangle)
	       (with-bounding-rectangle* (left top right bottom) rectangle
		 (translate-coordinates xoff yoff left top right bottom)
		 (draw-rectangle* stream left top right bottom
				  :ink +background-ink+ :filled t)))
	     (replay-record (record stream region)
	       ;; REGION is the bounding rectangle
	       (multiple-value-bind (x y) (bounding-rectangle-position record)
		 (multiple-value-bind (eleft etop) (bounding-rectangle-position region)
		   (replay-output-record record stream nil
					 (+ xoff (- eleft x)) (+ yoff (- etop y)))))))
	(dolist (erase erases)
	  (let ((region (second erase)))
	    (erase-rectangle stream region)))
	(dolist (move moves)
	  (let ((erase (second move)))
	    (erase-rectangle stream erase)))
	(dolist (move moves)
	  (let ((record (first move))
		(region (third move)))
	    (replay-record record stream region)))
	(dolist (draw draws)
	  (let ((record (first draw))
		(region (second draw)))
	    (replay-record record stream region)))
	(dolist (erase erase-overlapping)
	  (erase-rectangle stream erase))
	(dolist (move move-overlapping)
	  (let ((erase (second move)))
	    (erase-rectangle stream erase)))
	(dolist (move move-overlapping)
	  (let ((record (first move))
		(region (third move)))
	    (replay-output-record record stream region xoff yoff)))))))

;; This is done completely for side-effect.
(defmethod compute-new-output-records ((record output-record-element-mixin) stream)
  ;; Position and bounding rectangle are identical on these guys.
  (multiple-value-bind (cursor-x cursor-y)
      (stream-cursor-position stream)
    (declare (type coordinate cursor-x cursor-y))
    (multiple-value-bind (new-x new-y)
	(multiple-value-bind (px py)
	    (point-position
	      (stream-output-history-position stream))
	  (declare (type coordinate px py))
	  (position-difference cursor-x cursor-y px py))
      (declare (type coordinate new-x new-y))
      ;; because we don't know what to do, just do the same thing as before...
      (copy-display-state record t)
      (output-record-set-start-cursor-position record new-x new-y))
    (multiple-value-bind (delta-x delta-y)
	(output-record-end-cursor-position record)
      (declare (type coordinate delta-x delta-y))
      (stream-set-cursor-position
	stream (+ cursor-x delta-x) (+ cursor-y delta-y)))))

;; If you do REDISPLAY on a random output-record, do you want it to walk
;; down its children?  Or do you just assume that unless you did
;; updating-output there's no point in updating anything?  Certainly
;; SURROUNDING-OUTPUT-WITH-BORDER could beneficially call COMPUTE-NEW-OUTPUT-RECORDS
;; recursively on its children and then recompute the border.  Similarly
;; TABLES.  A presentation could represent the object with the appropriate
;; presentation-type - although WITH-OUTPUT-AS-PRESENTATION would definitely
;; not work.
;;
;; The question is what the default behavior should be.  The current behavior
;; is sort of minimalist.
#+++ignore
(defmethod compute-new-output-records ((record output-record-mixin) stream)
  ;; walk over children?
  )
    
;; if this doesn't work, just return nil, for the purposes of testing
;; the first implementation.

;; The interesting thing in this function is that both old-child-position
;; and inferior-start-position, old-child-extent and inferior-extent, are
;; all relative to the parents start-position.  Nothing is relative to
;; the parent's old-start-position.
(defmethod propagate-output-record-changes-p
	   ((record output-record-mixin) child mode
	    &optional (old-child-position 
			(multiple-value-bind (x y)
			    (output-record-old-start-cursor-position child)
			  (make-point x y)))
		      (old-child-extent
			(output-record-old-bounding-rectangle child)))
  #---ignore
  nil
  #+++ignore ;; until PROPAGATE-OUTPUT-RECORD-CHANGES is implemented.
  (and (if (or (eq mode :change) (eq mode :move) (eq mode :none))
	   (or (not (bounding-rectangle-position-equal
		      old-child-position 
		      (multiple-value-bind (x y)
		          (output-record-start-cursor-position child)
			(make-point x y))))
	       (not (bounding-rectangle-edges-equal
		      old-child-extent (bounding-rectangle child))))
	   ;; mode is :add or :delete.
	   (multiple-value-bind (width height) (bounding-rectangle-size child)
	     (declare (type coordinate width height))
	     (not (and (zerop width) (zerop height)))))
       (multiple-value-bind (x-offset y-offset) (output-record-position record)
	 (declare (type coordinate x-offset y-offset))
	 (with-bounding-rectangle* (left top right bottom) record
	   (block anything-needs-moving
	     (flet ((needs-move (subrecord)
		      (unless (output-record-fixed-position subrecord) 
			(return-from anything-needs-moving t))))
	       (declare (dynamic-extent #'needs-move))
	       ;;--- Why aren't these calls to MAKE-BOUNDING-RECTANGLE
	       ;;--- going to make "inside-out" rectangles??  --SWM
	       (map-over-output-records-overlapping-region
		 #'needs-move record
		 (make-bounding-rectangle
		   left (+ (bounding-rectangle-bottom old-child-extent) y-offset)
		   right bottom)
		 x-offset y-offset)
	       (map-over-output-records-overlapping-region
		 #'needs-move record
		 (make-bounding-rectangle
		   (bounding-rectangle-right old-child-extent)
		   (bounding-rectangle-top old-child-extent)
		   (- right x-offset)
		   (bounding-rectangle-bottom old-child-extent)))
	       nil))))))
   
;; This needs to return the differences list in the coordinates of its parent.
;; Also, the extents in the differences list must be allowed to be side-effected.
;; (At this stage, you are allowed to modify old-bounding-rectangle and
;; old-start-position, because once you construct a differences-list, no one is
;; allowed to depend on the values of state that you save in copy-display-state.
;;--- Still needs to be implemented...
(defmethod propagate-output-record-changes
	   ((record output-record-mixin) child mode
	    &optional (old-child-position 
			(multiple-value-bind (x y)
			    (output-record-old-start-cursor-position child)
			  (make-point x y)))
		      (old-child-extent
			(output-record-old-bounding-rectangle child))
		      erases moves draws erase-overlapping move-overlapping)
  (declare (values new-mode new-erases new-moves new-draws
		   new-erase-overlapping new-move-overlapping))
  ;; If :DELETE, and deleted all children, delete self,
  ;; if :DELETE and there's anyone past the extent, move them.
  ;; if :ADD or :CHANGE, and extent grew, and there's anyone past the extent, move them.
  ;; if :CHANGE, and shrank the extent, move them, unless output-record-fixed-position
  ;; If the extent shrinks, first move the records on its right, to their left.
  ;; Then, (if nothing from the right took its place), move the records below it, up.
  (when (eq mode :delete)
    (when (null (output-record-children record))
      (values :delete 
	      (list (list record
			  (progn (let ((old-bounding-rectangle
					 (output-record-old-bounding-rectangle record)))
				   (if old-bounding-rectangle
				       (bounding-rectangle record old-bounding-rectangle)
				     (setf (output-record-old-bounding-rectangle record)
					   (bounding-rectangle record)))))))))))

;; just use default, for now.
#+++ignore
(defmethod find-child-output-record ((record standard-updating-output-record)
				     use-old-children record-type &rest init-args
				     &key unique-id id-test &allow-other-keys)
  ;;; need to explicitly define what happens here.
  (declare (dynamic-extent init-args))
  )



(defmethod compute-new-output-records ((record standard-updating-output-record) stream)
  (funcall (slot-value record 'displayer) stream))

;; the contents of the output-record are ok, but we might have to move
;; it to a new position, x y
(defmethod reposition-output-record ((output-record standard-updating-output-record)
				     stream x y abs-x abs-y)
  (multiple-value-bind (delta-x delta-y)
      (output-record-end-cursor-position output-record)
    (declare (type coordinate delta-x delta-y))
    (copy-display-state output-record t)
    (cond ((output-record-fixed-position output-record)
	   ;; --- look at this ---
	   ;; Perhaps fixed-position should >not< do anything to the
	   ;; cursor position, or should add delta-x/y to the cursor-x/y
	   ;; rather than to start-position.
	   ;; The current choice is >not< the result of a
	   ;; well-thought-out design.  It just seemed like the right
	   ;; thing after three minutes of thought.
	   (multiple-value-bind (abs-x abs-y)
	       (point-position
		 (stream-output-history-position stream))
	     (declare (type coordinate abs-x abs-y))
	     (multiple-value-bind (x y) (output-record-position output-record)
	       (declare (type coordinate x y))
	       (stream-set-cursor-position
		 stream (+ abs-x x delta-x) (+ abs-y y delta-y)))))
	  (t
	   (output-record-set-start-cursor-position output-record x y)
	   (stream-set-cursor-position
	     stream (+ abs-x delta-x) (+ abs-y delta-y))))))

;; The contents of the output-record are not yet known to be ok, so we're going to have to run
;; the CONTINUATION
(defmethod recompute-output-record ((output-record standard-updating-output-record)
				    x y abs-x abs-y
				    cache-value copy-cache-value stream continuation)
  (setf (output-record-cache-value output-record)
	(if copy-cache-value (copy-seq cache-value) cache-value))
  (letf-globally (((stream-current-redisplay-record stream) output-record))
    ;; OK, now copy and reinitialize
    (copy-display-state output-record nil)
    (output-record-set-start-cursor-position output-record x y)
    (with-output-record-1 continuation
			  stream output-record abs-x abs-y)))

(defmethod cache-output-record ((output-record standard-updating-output-record)
				child uid id-test)
  (with-slots (cache) output-record
    ;; check for duplicates
    (when (find-with-test uid cache #'output-record-unique-id id-test)
      (cerror "Ignore the duplication.  Incremental redisplay will produce incorrect results."
	      "The unique-id ~S was used in more than one ~S at the same level."
	      uid 'updating-output))
    ;; In our case, the uid is stored in the output-record.
    (push child cache)))

(defmethod find-cached-output-record ((record standard-updating-output-record)
				      use-old-elts record-type
				      &rest init-args
				      &key unique-id id-test &allow-other-keys)
  (declare (dynamic-extent init-args))
  (with-slots (cache old-cache generation-tick) record
    (let ((cache-to-check (if use-old-elts old-cache cache)))
      (let ((elt
	      (if unique-id
		  (find-with-test unique-id cache-to-check
				  #'output-record-unique-id id-test)
		  ;; UNIQUE-ID can be NIL when we are coming through
		  ;; INVOKE-WITH-NEW-OUTPUT-RECORD to create new records
		  (and (eq record-type (class-name (class-of (first cache-to-check))))
		       (apply #'match-output-records (first cache-to-check) init-args)
		       (first cache-to-check)))))
	(when (and elt use-old-elts)
	  (setf old-cache (delete elt old-cache)))
	elt))))

(defmethod compute-difference-set :around
	   ((record standard-updating-output-record)
	    &optional check-overlapping
		      (x-offset (coordinate 0)) (y-offset (coordinate 0))
		      (old-x-offset (coordinate 0)) (old-y-offset (coordinate 0)))
  (declare (values erases moves draws erase-overlapping move-overlapping))
  (declare (ignore old-x-offset old-y-offset check-overlapping))
  (with-slots (all-new old-bounding-rectangle contents-ok old-parent) record
    ;; if it's all-new, don't bother walking the hierarchy, just redraw.
    (cond (all-new
	   (values nil nil
		   (list (list record
			       (bounding-rectangle-shift-position
				 record x-offset y-offset)))
		   nil nil))
	  ;; If contents were ok, but we're being copied from something that
	  ;; isn't up-to-date, or was off the screen, then it will be erased,
	  ;; so we have to do the move (i.e. "draw") even if it is "in the
	  ;; same position".
	  ;; Even if contents weren't ok, we don't want to erase inferior
	  ;; output records, and we don't want to start copying children,
	  ;; either.  Assume the whole branch needs to be computed from
	  ;; scratch.  It's exactly like all-new.  The only screw case here
	  ;; is if another, more deeply nested output record, was moved
	  ;; from somewhere in the hierarchy that was >not< erased.  In that
	  ;; case, we could just bitblt, but we won't detect that anymore.
	  ((and old-parent
		(not (eq (output-record-parent record) old-parent))
		(not (eql (output-record-generation-tick old-parent) *generation-tick*)))
	   (values nil nil
		   (list (list record
			       (bounding-rectangle-shift-position
				 record x-offset y-offset)))
		    nil nil))
	  (t 
	   (call-next-method)))))

(defmethod match-output-records :around
	   ((record1 output-record-element-mixin) &key all-new &allow-other-keys)
  (and (not all-new)
       (call-next-method)))


;; Should INVOKE-UPDATING-OUTPUT be generic on stream, or on the output record
;; or is this OK as it is?
(defun invoke-updating-output (stream continuation record-type
			       unique-id id-test cache-value cache-test
			       copy-cache-value parent-cache old-output-record
			       &rest args &key all-new &allow-other-keys)
  (declare (non-dynamic-extent args))
  (let* ((current-output-record 
	   (or (stream-current-output-record stream)
	       (stream-output-history stream)))
	 (current-output-record-position
	   (stream-output-history-position stream))
	 (current-redisplay-cache
	   (or parent-cache (stream-current-redisplay-record stream)))
	 (output-record
	   (progn
	     (when (eq unique-id 'assign-sequential-unique-IDs)
	       (when current-redisplay-cache
		 (setq unique-id (length (slot-value current-redisplay-cache 'cache))
		       args `(:unique-id ,unique-id ,@args))))
	     (when (and (stream-redisplaying-p stream) (not all-new))
	       (or old-output-record
		   (and current-redisplay-cache
			(apply #'find-cached-output-record-1
			       current-redisplay-cache record-type
			       :unique-id unique-id :id-test id-test
			       args))
		   (and current-output-record
			(apply #'find-child-output-record-1
			       current-output-record record-type
			       :unique-id unique-id :id-test id-test
			       :cache-value cache-value
			       args))))))
	 (output-record-moved-in-hierarchy
	   (and output-record
		(not (eq (output-record-parent output-record) current-output-record)))))
    (if output-record
	;; we've already been through this path once, just update if necessary.
	(multiple-value-bind (cursor-x cursor-y)
	    (stream-cursor-position stream)
	  (declare (type coordinate cursor-x cursor-y))
	  (multiple-value-bind (x y)
	      (multiple-value-bind (px py)
		  (point-position current-output-record-position)
		(declare (type coordinate px py))
		(position-difference cursor-x cursor-y px py))
	    (declare (type coordinate x y))
	    ;; Update displayer, in case something changed.  
	    (flet ((updating-output-displayer (stream)
		     (apply #'invoke-updating-output
			    stream continuation record-type unique-id id-test
			    cache-value cache-test copy-cache-value nil output-record
			    args)))
	      (setf (slot-value output-record 'displayer) #'updating-output-displayer))
	    ;; we found it in some cache, make sure that it is decached from its immediate
	    ;; parent...
	    (when (output-record-parent output-record)
	      (decache-child-output-record-1
		(output-record-parent output-record) output-record))
	    (when current-redisplay-cache
	      (cache-output-record current-redisplay-cache output-record unique-id id-test))
	    (when output-record-moved-in-hierarchy
	      (setf (output-record-old-parent output-record)
		    (output-record-parent output-record))
	      (setf (output-record-parent output-record) current-output-record)
	      ;; old extent is bogus, since it is relative to
	      ;; old-parent. Convert it to be relative to current-output-record.
	      (multiple-value-call 
		#'output-record-set-start-cursor-position
		output-record
		;; could be wildly out of bounding box, but that's OK.
		(bounding-rectangle-position-difference
		  ;; old-position is cached absolute coordinates
		  (output-record-old-cursor-position output-record)
		  current-output-record-position)))
	    (if (and (not (eq cache-value 'unsupplied-cache-value))
		     (funcall cache-test
			      cache-value (output-record-cache-value output-record)))
		(reposition-output-record output-record stream x y cursor-x cursor-y)
		(flet ((call-continuation (record)
			 (declare (ignore record))
			 (funcall continuation stream)))
		  (declare (dynamic-extent #'call-continuation))
		  (recompute-output-record output-record x y cursor-x cursor-y
					   cache-value copy-cache-value
					   stream #'call-continuation)))
	    (when (and current-output-record
		       ;; even if we didn't move in hierarchy, because if we are
		       ;; running this code then our parent did >not< have
		       ;; contents-ok, and therefore lost all of his children (if
		       ;; he was updated this pass, so check generation-tick).
		       (or output-record-moved-in-hierarchy
			   (eql (output-record-generation-tick current-output-record)
				*generation-tick*)))
	      ;;--- is there some way to detect deletes without clearing the output-record?
	      ;;--- this current implementation has the potential for gratuitous consing...
	      ;;--- maybe we should add (yet) one >more< slot to the output-record as part
	      ;;--- of the copy-display-state protocol, whose purpose, after all, >is< to
	      ;;--- reduce torrential consathons.
	      (add-output-record output-record current-output-record))
	    (let ((position (output-record-old-cursor-position output-record)))
	      (setf (point-x position) cursor-x)
	      (setf (point-y position) cursor-y))
	    output-record))
	(flet ((invoke-updating-output-1 (new-record)
		 (declare (ignore new-record))
		 (let ((record (or (stream-current-output-record stream)
				   (stream-output-history stream)))
		       (redisplay-piece (stream-current-redisplay-record stream)))
		   (flet ((updating-output-displayer (stream)
			    (apply #'invoke-updating-output
				   stream continuation record-type unique-id id-test
				   cache-value cache-test copy-cache-value nil record
				   args)))
		     (setf (slot-value record 'displayer) #'updating-output-displayer))
		   (when redisplay-piece
		     (cache-output-record redisplay-piece record unique-id id-test))
		   (multiple-value-bind (x y)
		       (stream-cursor-position stream)
		     (letf-globally (((stream-current-redisplay-record stream) record))
		       (funcall continuation stream))
		     (let ((position (output-record-old-cursor-position record)))
		       (setf (point-x position) x)
		       (setf (point-y position) y))))))
	  (declare (dynamic-extent #'invoke-updating-output-1))
	  (apply #'invoke-with-new-output-record
		 stream #'invoke-updating-output-1
		 record-type (gethash record-type *output-record-constructor-cache*) 
		 args)))))
