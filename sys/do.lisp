;; See the file LICENSE for the full license governing this code.
;;

(in-package :user)

;;; In order to build and load versions of clim, changes in
;;; file names must be made in this file to reflect the actual drive and directory
;;; portions of pathnames.

(defvar *clim-root* (make-pathname
                       :device (pathname-device *load-pathname*)
                       :directory (butlast (pathname-directory *load-pathname*))))


(defvar *driball-dir* "c:\\temp\\")

;; EVERYTHING BELOW This is relative to the above constants.

(defvar *driball-err-out* nil)
(defun driball (&optional path)
   (if path
      (let* ((dn (concatenate 'string "clim" path ".txt"))
             (drib (ask-user-for-new-pathname
                    (format nil "Pick dribble file for ~A" path)
                    :initial-name dn
                    :allowed-types
                    '(("Dribble files" . "*.txt") ("All files" . "*.*"))
                    :host *driball-dir*
                    ))
             )
         (when *driball-err-out*
            ;; flush interrupted operation
            (driball))
         (when drib
            (dribble drib)
            (setq *driball-err-out* (list *error-output* *trace-output*))
            (setq *error-output* *standard-output*)
            (setq *trace-output* *standard-output*)
            (gc t) (gc t) (gc t)
            #-runtime-system (room t)
            )
         (if drib
            t
            (case (pop-up-message-dialog *lisp-main-window*
                     (format nil "CLIM op ~A" path)
                     "CANCEL CLIM Operation or CONTINUE with no dribble output?"
                     question-icon "CANCEL" "CONTINUE")
               (2 t)))
             )
      (when *driball-err-out*
         (gc t) (gc t) (gc t)
         #-runtime-system (room t)
         (setq *error-output* (first *driball-err-out*))
         (setq *trace-output* (second *driball-err-out*))
         (setq *driball-err-out* nil)
         (dribble))))

(defun climpath (sub)
   (merge-pathnames sub *clim-root*))


(let* ((file (pop-up-message-dialog *lisp-main-window* "CLIM operation"
                     "Choose COMPile, LOAD, or LoaD No Demo"
                     question-icon "CANCEL" "COMP" "LOAD" "LDND"))
       (*default-pathname-defaults* *load-pathname*))
  (case file
    (2 (when (driball "comp")
	 (#-runtime-system time #+runtime-system progn (load "compile.lisp"))
	 (driball)))
    (3 (when (driball "load")
	 (#-runtime-system time #+runtime-system progn (load "load.lisp"))
	 (driball)))
    (4 (when (driball "ldnd")
	 (setf user::*no-clim-demos* t)
	 (#-runtime-system time #+runtime-system progn (load "load.lisp")))
	 (driball))))
