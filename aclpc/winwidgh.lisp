;;; -*- Package: win; mode: Common-Lisp -*-

(in-package :win)

#+ignore
(eval-when (compile load eval)
  (defmacro defsystemcall (name &key entry-point arguments return-type
				     arg-checking call-direct
				     (release-heap :when-ok))
    (if (constantp arguments) (setq arguments (eval arguments)))
    (setq arguments
      (if arguments
	  (mapcar #'(lambda (type)
		      (list (gensym)
			    (cond ((eq type t) :int)
				  ((eq type 'integer) :int)
				  ((eq type 'fixnum) :int)
				  (type))))
		  arguments)
	'(:void)))
    (if (constantp name) (setq name (eval name)))
    (setq return-type
      (case return-type
	(:integer :int)
	(:unsigned-integer :unsigned-int)
	(otherwise return-type)))
    `(ff:def-foreign-call (,name ,entry-point) ,arguments
       :returning ,return-type
       :call-direct ,call-direct
       :arg-checking ,arg-checking
       :release-heap ,release-heap)))

#+ignore
(defsystemcall 'createpen
    :entry-point "CreatePen"
    :arguments '(integer integer integer)
    :return-type :integer
    :arg-checking nil
    :call-direct t)

#+ignore
(defsystemcall 'createpopupmenu
    :entry-point "CreatePopupMenu"
    :arguments '()
    :return-type :unsigned-integer
    :arg-checking nil
    :call-direct t)

#+ignore
(defsystemcall 'createrectrgn
    :entry-point "CreateRectRgn"
    :arguments '(integer integer integer integer)
    :return-type :integer
    :arg-checking nil
    :call-direct t)

#+ignore
(defsystemcall 'createsolidbrush
    :entry-point "CreateSolidBrush"
    :arguments '(integer)
    :return-type :integer
    :arg-checking nil
    :call-direct t)

#+ignore
(defsystemcall 'getactivewindow
    :entry-point "GetActiveWindow"
    :arguments '()
    :return-type :unsigned-integer
    :arg-checking nil
    :call-direct t)

#+ignore
(defsystemcall 'getdc
    :entry-point "GetDC"
    :arguments '(integer)
    :return-type :integer
    :arg-checking nil
    :call-direct t)

#+ignore
(defsystemcall 'getlasterror
    :entry-point "GetLastError"
    :arguments '()
    :return-type :integer
    :arg-checking nil
    :call-direct t)

#+ignore
(defsystemcall 'getversion
    :entry-point "GetVersion"
    :arguments '()
    :return-type :unsigned-integer
    :arg-checking nil
    :call-direct t)

#+ignore
(defsystemcall 'lineto
    :entry-point "LineTo"
    :arguments '(integer fixnum fixnum)
    :return-type :boolean
    :arg-checking nil
    :call-direct t)

#+ignore
(defsystemcall 'movetoex
    :entry-point "MoveToEx"
    :arguments '(integer integer integer integer)
    :return-type :boolean
    :arg-checking nil
    :call-direct t)

#+ignore
(defsystemcall 'releasedc
    :entry-point "ReleaseDC"
    :arguments '(integer fixnum)
    :return-type :integer
    :arg-checking nil
    :call-direct t)

#+ignore
(defsystemcall 'selectobject
    :entry-point "SelectObject"
    :arguments '(integer integer)
    :return-type :integer
    :arg-checking nil
    :call-direct t)

#+ignore
(defsystemcall 'setrop2
    :entry-point "SetROP2"
    :arguments '(integer integer)
    :return-type :integer
    :arg-checking nil
    :call-direct t)

(eval-when (compile load eval)
  (export '(
	    LBS_DISABLENOSCROLL
	    HLN_SELCHANGE
	    CB_SETTOPINDEX
	    )
	  'windows))



(defconstant LBS_DISABLENOSCROLL #x1000)
(defconstant HLN_SELCHANGE #x1)
(defconstant CB_SETTOPINDEX #x015c)


#+ignore
(def-foreign-type scrollinfo
    (:struct (cbSize uint)
	     (fMask uint)
	     (nMin int)
	     (nMax int)
	     (nPage uint)
	     (nPos int)
	     (nTrackPos int)))

#+ignore
(ct:defctype lpscrollinfo (scrollinfo *))
