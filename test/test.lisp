
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca.  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, Ca.  All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
(in-package :clim)

(define-application-frame test-frame ()
			  (a b c)
			  (:pane 
			   (vertically ()
				       (silica::scrolling
					()
					(setq aaa
					  (silica::realize-pane
					   'interactor-pane)))
				       (realize-pane 'slider))))


(define-test-frame-command (com-square-it :name t :menu t)
			   ((x 'integer))
			   (present (* x x) 'integer :stream
				    *query-io*))


(define-test-frame-command (com-clear :name t :menu t)
			   ()
			   (window-clear *query-io*))

(define-test-frame-command (com-quit :name t :menu t)
			   ()
			   (frame-exit *application-frame*))


(define-test-frame-command (com-show :name t :menu t)
			   ()
			   (terpri *query-io*)
			   (display-command-table-menu 
			    (frame-command-table *application-frame*)
			    *query-io*
			    :move-cursor t)
			   (terpri *query-io*))

(define-presentation-to-command-translator square-int (integer
						       com-square-it test-frame)
					   
					   (object)
					   (list object))
