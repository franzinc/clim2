(in-package :tk)

(defun example-1 ()
  (let (shell)
    
(setq shell (create-popup-shell "foo" 'base-window-shell old-shell))
(setq cp (create-managed-widget 'control "fio" shell))
(setq bs (create-managed-widget 
	  'menu-button "foo" cp 
	  :label "foo"
	  :font
	  "-adobe-courier-medium-r-normal--12-120-75-75-m-70-iso8859-1"))

				
    (popup shell)))
