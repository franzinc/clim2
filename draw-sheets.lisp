(in-package :composer)

(defclass gr-sheet-display
    (grapher-motif-display xcw::composer-window-mixin)
    ()
  (:metaclass cxr:standard-class-using-x-resources)
  (:default-initargs 
   :command-table (find-command-table 'composer-grapher-command-table)))

(defclass sheet-gnode (gr::generational-gnode 
		       gr::grapher-presentation-mixin-with-chain)
   ())

(defun graph-sheet (sheet &rest args)
  (apply #'graph-descendants
	 (etypecase sheet
	   (silica::application-frame (clim-internals::frame-top-level-sheet sheet))
	   (silica::sheet sheet))
	 :display 'gr-sheet-display
	 :gnode-class 'sheet-gnode
	 :title "Sheet hierarchy"
	 :children-fn #'(lambda (x)
			  (and (typep x 'silica::sheet-parent-mixin)
			       (silica::sheet-children x)))
	 :parents-fn #'(lambda (x) (list (silica::sheet-parent x)))
	 :complete-name '("composer" "sheetGraph")
	 :complete-class '("Composer" "Grapher")
	 args))

(defun graph-output-history (sheet &rest args)
  (apply #'graph-descendants
	 (if (streamp sheet)
	     (clim::output-recording-stream-output-record sheet)
	   sheet)
	 :display 'gr-sheet-display
	 :gnode-class 'sheet-gnode
	 :title "Output History"

	 :children-fn #'(lambda (x)
			  (clim-internals::output-record-children x))

	 :parents-fn #'(lambda (x) (list (clim-internals::output-record-parent x)))
	 :complete-name '("composer" "outputHistoryGraph")
	 :complete-class '("Composer" "Grapher")
	 args))


