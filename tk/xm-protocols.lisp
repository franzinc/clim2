;; See the file LICENSE for the full license governing this code.
;;

(in-package :tk)

(defmethod spread-callback-data (widget call-data (type (eql :protocol-callback)))
  (declare (ignore widget call-data))
 (values))

(defun add-protocol-callback (shell property protocol function &rest args)
  (let ((type :protocol-callback))
    (xm_add_protocol_callback
     shell
     (if (integerp property) property (xm-intern-atom shell property))
     (if (integerp protocol) protocol (xm-intern-atom shell protocol))
     (or *callback-handler-address*
	 (setq *callback-handler-address* (register-foreign-callable 'callback-handler)))
     (caar (push
	    (list (new-callback-id) (cons function args) type)
	    (widget-callback-data shell))))))

(defun add-wm-protocol-callback (shell protocol fn &rest args)
  (apply
   #'add-protocol-callback
   shell
   "WM_PROTOCOLS"
   (case protocol
     (:wm-delete-window "WM_DELETE_WINDOW")
     (:wm-save-your-self "WM_SAVE_YOUR_SELF")
     (t protocol))
   fn
   args))

(defun xm-intern-atom (shell name &optional only-if-exists)
  (xm_intern_atom
   (object-display shell)
   (lisp-string-to-string8 name)
   (if only-if-exists 1 0)))
