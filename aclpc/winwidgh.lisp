;; This file is all new material, being a header file for WinWidget dll's
;; Note that it is larger than 32K

(in-package :pc)

(cl:defconstant hm_first            (cl:+ wm_user 100))
(cl:defconstant hm_getbkgndbrush    (cl:+ hm_first 0))
(cl:defconstant hm_setbkgndbrush    (cl:+ hm_first 1))
(cl:defconstant hm_getformat        (cl:+ hm_first 2))
(cl:defconstant hm_setformat        (cl:+ hm_first 3))
(cl:defconstant hm_gethilitebrush   (cl:+ hm_first 4))
(cl:defconstant hm_sethilitebrush   (cl:+ hm_first 5))
(cl:defconstant hm_gettextcolor     (cl:+ hm_first 6))
(cl:defconstant hm_settextcolor     (cl:+ hm_first 7))
(cl:defconstant hm_getdataclass     (cl:+ hm_first 8))
(cl:defconstant hm_getdatasize      (cl:+ hm_first 9))
(cl:defconstant hm_getdatatype      (cl:+ hm_first 10))
(cl:defconstant hm_getdatalink      (cl:+ hm_first 11))
(cl:defconstant hm_setdatalink      (cl:+ hm_first 12))
(cl:defconstant hm_getfont          wm_getfont)
(cl:defconstant hm_setfont          wm_setfont)
(cl:defconstant hm_haschanged       (cl:+ hm_first 13))
(cl:defconstant hm_setchanged       (cl:+ hm_first 14))
(cl:defconstant hm_getstate         (cl:+ hm_first 17))
(cl:defconstant hm_setstate         (cl:+ hm_first 18))
(cl:defconstant hm_gettext          wm_gettext)
(cl:defconstant hm_settext          wm_settext)
(cl:defconstant hm_bequiet          (cl:+ hm_first 19))
(cl:defconstant hm_isquiet          (cl:+ hm_first 20))
(cl:defconstant hm_setfocuschild    (cl:+ hm_first 21))
(cl:defconstant hm_killfocuschild   (cl:+ hm_first 22))

;; ************************************************************************\

;;                        miscellaneous types

;; *************************************************************************/

(cl:defconstant hgb_retrieve 0)
(cl:defconstant hgb_update 1)
(cl:defconstant hgb_insert 2)
(cl:defconstant hgb_delete 3)
(cl:defconstant hgb_change 4)
(cl:defconstant hgb_validate 5)
(cl:defconstant hgb_add 6)

(cl:defconstant hg_addnew -1)

(cl:defconstant hp_bitmap #x0001)
(cl:defconstant hp_icon #x0002)
(cl:defconstant hp_dib #x0003)

(cl:defconstant winver30 #x0300)
(cl:defconstant winver31 #x030a)

;; additional notification codes
(cl:defconstant hn_return #x0101)
(cl:defconstant hn_escape #x0102)
(cl:defconstant hn_left #x0103)
(cl:defconstant hn_right #x0104)
(cl:defconstant hn_up #x0105)
(cl:defconstant hn_down #x0106)
(cl:defconstant hn_tab #x0107)
(cl:defconstant hn_backtab #x0108)
(cl:defconstant hn_altarrow #x0109)
(cl:defconstant hn_next #x010a)
(cl:defconstant hn_prior #x010b)
(cl:defconstant hn_dblclk #x010c)
(cl:defconstant hn_help #x010d)
(cl:defconstant hn_f1pressed hn_help)

;; ************************************************************************
;; the hbutt control
;; ************************************************************************

;; messages 
(cl:defconstant hbm_first (cl:+ wm_user 30))
(cl:defconstant hbm_getbkgndbrush hm_getbkgndbrush) ;; returns hilitebrush
(cl:defconstant hbm_setbkgndbrush hm_setbkgndbrush)  ;; sets hilitebrush
(cl:defconstant hbm_getpalign (cl:+ hbm_first 0))
(cl:defconstant hbm_getbalign hbm_getpalign)
(cl:defconstant hbm_getpic (cl:+ hbm_first 1))
(cl:defconstant hbm_getbitmap hbm_getpic)
(cl:defconstant hbm_getcount (cl:+ hbm_first 2))
(cl:defconstant hbm_getfont hm_getfont)
(cl:defconstant hbm_getmaskcolor (cl:+ hbm_first 3))
(cl:defconstant hbm_getralign (cl:+ hbm_first 4))
(cl:defconstant hbm_getsound (cl:+ hbm_first 5))
(cl:defconstant hbm_getstate bm_getcheck)
(cl:defconstant hbm_gettalign (cl:+ hbm_first 6))
(cl:defconstant hbm_gettext hm_gettext)
(cl:defconstant hbm_gettextcolor hm_gettextcolor)
(cl:defconstant hbm_gettype (cl:+ hbm_first 9))

(cl:defconstant hbm_click (cl:+ wm_user 99))
(cl:defconstant hbm_ispressed bm_getstate)
(cl:defconstant hbm_press bm_setstate)

(cl:defconstant hbm_setpalign (cl:+ hbm_first 20))
(cl:defconstant hbm_setbalign hbm_setpalign)	;; for compatibility
(cl:defconstant hbm_setpic (cl:+ hbm_first 21))
(cl:defconstant hbm_setbitmap hbm_setpic) ;; for compatibility
(cl:defconstant hbm_setfont hm_setfont)
(cl:defconstant hbm_setmaskcolor (cl:+ hbm_first 22))
(cl:defconstant hbm_setralign (cl:+ hbm_first 23))
(cl:defconstant hbm_setsound (cl:+ hbm_first 24))
(cl:defconstant hbm_setstate bm_setcheck)
(cl:defconstant hbm_settalign (cl:+ hbm_first 25))
(cl:defconstant hbm_settext hm_settext)
(cl:defconstant hbm_settextcolor hm_settextcolor)
(cl:defconstant hbm_settype bm_setstyle)
(cl:defconstant hbm_getdata (cl:+ hbm_first 26))
(cl:defconstant hbm_setdata (cl:+ hbm_first 27))
(cl:defconstant hbm_getdatalink (cl:+ hbm_first 28))
(cl:defconstant hbm_setdatalink (cl:+ hbm_first 29))
(cl:defconstant hbm_bequiet hm_bequiet)
(cl:defconstant hbm_isquiet hm_isquiet)
(cl:defconstant hbm_getdatasize (cl:+ hbm_first 30))
(cl:defconstant hbm_getstatecount (cl:+ hbm_first 31))
(cl:defconstant hbm_islastingroup (cl:+ hbm_first 32))
(cl:defconstant hbm_setcount (cl:+ hbm_first 33)) ; 
(cl:defconstant hbm_getpalette (cl:+ hbm_first 34))

;; styles
(cl:defconstant hbs_pushbutton #x00)
(cl:defconstant hbs_defpushbutton #x01)
(cl:defconstant hbs_checkbox #x02)
(cl:defconstant hbs_radiobutton #x03)
(cl:defconstant hbs_3state #x04)
(cl:defconstant hbs_ownerdraw #x05)
(cl:defconstant hbs_grouppush #x06)

(cl:defconstant hbs_transparent #x0010)
(cl:defconstant hbs_lefttext #x0020)
(cl:defconstant hbs_ljust #x0020)
(cl:defconstant hbs_rjust #x0040)
(cl:defconstant hbs_nofocus #x0080)
(cl:defconstant hbs_downpics #x0400)
(cl:defconstant hbs_autoadvance #x0800)
(cl:defconstant hbs_nobutton #x1000)
(cl:defconstant hbs_textindent #x2000)
(cl:defconstant hbs_squared #x4000)
(cl:defconstant hbs_async #x8000)

(cl:defconstant hbs_icons 0) ;;  for compatibility - was 0x0100l
(cl:defconstant hbs_downmaps hbs_downpics)  ;;	for compatibility

;; notification codes
(cl:defconstant hbn_clicked bn_clicked)
(cl:defconstant hbn_doubleclicked bn_doubleclicked)

;; error codes
(cl:defconstant hberr -1)

;; *************************************************************************\
;; the hedit control
;; *************************************************************************/

;;  messages
(cl:defconstant hem_first (cl:+ wm_user 40))
(cl:defconstant hem_getbkgndbrush hm_getbkgndbrush)  ;; returns hilitebrush.
(cl:defconstant hem_setbkgndbrush hm_setbkgndbrush)  ;; sets hilitebrush.
(cl:defconstant hem_getdata (cl:+ hem_first 0)) ;; retrieves data.)
(cl:defconstant hem_setdata (cl:+ hem_first 1)) ;; sets data.)
(cl:defconstant hem_getdataclass hm_getdataclass) ;; returns datatype
(cl:defconstant hem_getdatalink hm_getdatalink) ;; returns datalink.
(cl:defconstant hem_setdatalink hm_setdatalink) ;; sets datalink.
(cl:defconstant hem_getdatasize hm_getdatasize) ;; returns datasize
(cl:defconstant hem_getdatatype hm_getdatatype) ;; returns datatype
(cl:defconstant hem_getfont hm_getfont)   ;; returns the font handle.
(cl:defconstant hem_setfont hm_setfont)   ;; sets the font handle.
(cl:defconstant hem_getformat hm_getformat) ;; retrieves format string.
(cl:defconstant hem_setformat hm_setformat) ;; sets format string.
(cl:defconstant hem_getformatrect (cl:+ hem_first 2))  ;; retrieves formatrect)
(cl:defconstant hem_setformatrect (cl:+ hem_first 3))  ;; sets formatrect.)
(cl:defconstant hem_gethilitebrush hm_gethilitebrush) ;; returns hilitebrush.
(cl:defconstant hem_sethilitebrush hm_sethilitebrush) ;; sets hilitebrush.
(cl:defconstant hem_getmaxtextlen (cl:+ hem_first 4)) ;; returns textmaxlen.
(cl:defconstant hem_setmaxtextlen em_limittext) ;; sets textmaxlen.
(cl:defconstant hem_getoverwritemode (cl:+ hem_first 5)) ;; returns overwrite/insert status.
(cl:defconstant hem_setoverwritemode (cl:+ hem_first 6)) ;; toggles overwrite/insert modes.
(cl:defconstant hem_getpasswordchar (cl:+ hem_first 7)) ;; returns passwordchar.
(cl:defconstant hem_setpasswordchar (cl:+ hem_first 8)) ;; sets passwordchar.
(cl:defconstant hem_getscrollpos (cl:+ hem_first 9)) ;; retrieves scrollpos.
(cl:defconstant hem_setscrollpos (cl:+ hem_first 10)) ;; sets scrollpos.
(cl:defconstant hem_getsel em_getsel)    ;; retrieves range of selected text.
(cl:defconstant hem_setsel em_setsel)    ;; sets range of selected text.
(cl:defconstant hem_getstate hm_getstate)  ;; returns set state flags.
(cl:defconstant hem_setstate hm_setstate)  ;; sets one of state flags.
(cl:defconstant hem_gettext hm_gettext)   ;; retrieves formatted text.
(cl:defconstant hem_settext hm_settext)   ;; sets data for string class.
(cl:defconstant hem_gettextcolor hm_gettextcolor) ;; returns textcolor.
(cl:defconstant hem_settextcolor hm_settextcolor) ;; sets textcolor.
(cl:defconstant hem_gettextlen wm_gettextlength)  ;; returns textlen.
(cl:defconstant hem_update (cl:+ hem_first 13)) ;; updates text using data or vice-versa.
(cl:defconstant hem_haschanged hm_haschanged)  ;; returns the status of the changed flag
(cl:defconstant hem_setchanged hm_setchanged)  ;; sets the status of the changed flag
(cl:defconstant hem_getvalidate (cl:+ hem_first 14)) ;; returns a pointer to the validation function
(cl:defconstant hem_setvalidate (cl:+ hem_first 15)) ;; sets the validation function
(cl:defconstant hem_validate (cl:+ hem_first 16)) ;; calls the validation function
(cl:defconstant hem_bequiet hm_bequiet)   ;; disables notification
(cl:defconstant hem_isquiet hm_isquiet)   ;; tests the state of quiet flag
(cl:defconstant hem_getseltext (cl:+ hem_first 17))
(cl:defconstant hem_replacesel (cl:+ hem_first 18))
(cl:defconstant hem_last (cl:+ hem_first 20))

;; state flags
(cl:defconstant hef_displaymode #x0002)  ;;  control is in display mode
(cl:defconstant hef_right #x0004)  ;;  text is right justified in current mode
(cl:defconstant hef_center #x0008)  ;;  text is centered in the current mode
(cl:defconstant hef_noredraw #x0010)  ;;  control will not update display
(cl:defconstant hef_changed #x0080)  ;;  data has changed since last hem_setdata

;; styles
(cl:defconstant hes_displayleft #x0000)
(cl:defconstant hes_displaycenter #x0001)
(cl:defconstant hes_displayright #x0002)
(cl:defconstant hes_uppercase #x0008)
(cl:defconstant hes_lowercase #x0010)
(cl:defconstant hes_password #x0020)
(cl:defconstant hes_autohscroll #x0080)
(cl:defconstant hes_nohidesel #x0100)

(cl:defconstant hes_border3d #x0200)  ;; 3-d indented border
(cl:defconstant hes_indent #x0200)  ;; 3-d indented border
(cl:defconstant hes_extrude (cl:logior #x0200 ws_border)) ;; 3-d extruded border
(cl:defconstant hes_hilite #x0400)  ;; changes background color on setfocus

(cl:defconstant hes_editleft #x0000)
(cl:defconstant hes_editright #x0800)
(cl:defconstant hes_hungry #x1000)

;; notification codes
(cl:defconstant hen_setfocus #x0100)
(cl:defconstant hen_killfocus #x0200)
(cl:defconstant hen_change #x0300)
(cl:defconstant hen_update #x0400)
(cl:defconstant hen_errspace #x0500)
(cl:defconstant hen_maxtext #x0501)
(cl:defconstant hen_hscroll #x0601)
(cl:defconstant hen_invalid #x0602)

;; error codes
(cl:defconstant heerr -1)  ;;  a general error occurred.
(cl:defconstant heerr_general -1)  ;;  a general error occurred.
(cl:defconstant heerr_badptr -2)  ;;  a pointer was found to be null unexpectedly.
(cl:defconstant heerr_badval -3)  ;;  a value was not in the expected group or range.
(cl:defconstant heerr_nosel -4)  ;;  there is no selection.
(cl:defconstant heerr_space -5)  ;;  the control was unable to allocate memory.

;; *************************************************************************\
;; the hlist control
;; *************************************************************************/

;; messages
(cl:defconstant hlm_first (cl:+ wm_user 150)) ; 

;; multiple item messages
(cl:defconstant hlm_getcount (cl:+ hlm_first 0)) ;;  get number of items
(cl:defconstant hlm_resetcontent (cl:+ hlm_first 1)) ;;  delete all items

;; index messages
(cl:defconstant hlm_deleteitem (cl:+ hlm_first 10)) ;;  delete item by index
(cl:defconstant hlm_selectitem (cl:+ hlm_first 11)) ;;  select item by index
(cl:defconstant hlm_getdata (cl:+ hlm_first 12)) ;;  get item data by index
(cl:defconstant hlm_getcode (cl:+ hlm_first 13)) ;;  get item code by index
(cl:defconstant hlm_gettext (cl:+ hlm_first 14)) ;;  get item text by index
(cl:defconstant hlm_gettextlen (cl:+ hlm_first 15)) ;;  get item textlen by index
(cl:defconstant hlm_setcode (cl:+ hlm_first 16)) ;;  get item code by index

;; data messages
(cl:defconstant hlm_findstring (cl:+ hlm_first 18)) ; 
(cl:defconstant hlm_selectstring (cl:+ hlm_first 19)) ; 
(cl:defconstant hlm_getdataclass hm_getdataclass) ;;  get data class
(cl:defconstant hlm_getdatalink hm_getdatalink)  ;;  pointer to cursel data
(cl:defconstant hlm_setdatalink hm_setdatalink)  ;;
(cl:defconstant hlm_getdatasize hm_getdatasize)  ;;  get data size in bytes
(cl:defconstant hlm_getdatatype hm_getdatatype)  ;;  get data type
(cl:defconstant hlm_additem (cl:+ hlm_first 25)) ;;  add item without codes
(cl:defconstant hlm_insertitem (cl:+ hlm_first 26)) ;;  insert item without codes
(cl:defconstant hlm_finddata (cl:+ hlm_first 27)) ;;  find item by data
(cl:defconstant hlm_selectdata (cl:+ hlm_first 28)) ;;  select item by data
(cl:defconstant hlm_getcurdata (cl:+ hlm_first 29)) ;;  current item's data

;; code messages
(cl:defconstant hlm_getcodeclass (cl:+ hlm_first 30)) ;;  get code class
(cl:defconstant hlm_getcodelink (cl:+ hlm_first 31)) ;;  hot-link to current selection
(cl:defconstant hlm_setcodelink (cl:+ hlm_first 32)) ;;  hot-link to current selection
(cl:defconstant hlm_getcodesize (cl:+ hlm_first 33)) ;;  get code size in bytes
(cl:defconstant hlm_getcodetype (cl:+ hlm_first 34)) ;;  get data type
(cl:defconstant hlm_findcode (cl:+ hlm_first 35)) ;;  find item by code
(cl:defconstant hlm_selectcode (cl:+ hlm_first 36)) ;;  select item by data
(cl:defconstant hlm_getcurcode (cl:+ hlm_first 37)) ;;  current item's code

;; painting messages
(cl:defconstant hlm_update (cl:+ hlm_first 50)) ;;  repaint the list
(cl:defconstant hlm_getbkgndbrush hm_getbkgndbrush)  ;;  background color
(cl:defconstant hlm_setbkgndbrush hm_setbkgndbrush)
(cl:defconstant hlm_gethilitebrush hm_gethilitebrush) ;;  background color with focus
(cl:defconstant hlm_sethilitebrush hm_sethilitebrush)
(cl:defconstant hlm_gettextcolor hm_gettextcolor) ;;  text color
(cl:defconstant hlm_settextcolor hm_settextcolor)

;; selection messages
(cl:defconstant hlm_getcursel (cl:+ hlm_first 60)) ;;  currently selected item
(cl:defconstant hlm_setcursel (cl:+ hlm_first 61)) ; 
(cl:defconstant hlm_getsel (cl:+ hlm_first 62)) ;;  selection state of item
(cl:defconstant hlm_setsel (cl:+ hlm_first 63)) ; 
(cl:defconstant hlm_getselcount (cl:+ hlm_first 64)) ;;  number of selected items
(cl:defconstant hlm_selectrange (cl:+ hlm_first 65)) ;;  select a range
(cl:defconstant hlm_getselitems (cl:+ hlm_first 66)) ;;  get array of indices

;; dimension messages
(cl:defconstant hlm_setcolumnwidth (cl:+ hlm_first 70)) ;;  set the column width
(cl:defconstant hlm_gethextent (cl:+ hlm_first 71)) ;;  get the total width of list
(cl:defconstant hlm_getitemrect (cl:+ hlm_first 72)) ;;  get an item rectangle

;; miscellany
(cl:defconstant hlm_gettopindex (cl:+ hlm_first 80)) ;;  index of first visible item
(cl:defconstant hlm_settopindex (cl:+ hlm_first 81)) ; 
(cl:defconstant hlm_getscrollpos hlm_gettopindex) ;;  index of first visible item
(cl:defconstant hlm_setscrollpos hlm_settopindex)
(cl:defconstant hlm_scroll (cl:+ hlm_first 83)) ;;  scroll the list box
(cl:defconstant hlm_settabstops (cl:+ hlm_first 84)) ;;  set tab stops
(cl:defconstant hlm_setredraw (cl:+ hlm_first 87)) ;;  set redraw on/off
(cl:defconstant hlm_haschanged hm_haschanged) ;;  has selection changed since focus
(cl:defconstant hlm_setchanged hm_setchanged) ;;  has selection changed since focus
(cl:defconstant hlm_getstate (cl:+ hlm_first 91)) ;;  get state flags
(cl:defconstant hlm_setstate (cl:+ hlm_first 92)) ;;  set state flag
(cl:defconstant hlm_getformat hm_getformat) ;;  get format string
(cl:defconstant hlm_setformat hm_setformat) ;;  set format string
(cl:defconstant hlm_getfont hm_getfont)
(cl:defconstant hlm_setfont hm_setfont)
(cl:defconstant hlm_bequiet hm_bequiet)
(cl:defconstant hlm_isquiet hm_isquiet)
(cl:defconstant hlm_addsamples (cl:+ hlm_first 93)) ; 
(cl:defconstant hlm_getstyle (cl:+ hlm_first 94)) ; 
(cl:defconstant hlm_last (cl:+ hlm_first 150)) ; 

;; styles
(cl:defconstant hls_singleline #x0001)
(cl:defconstant hls_sortbydata #x0002)
(cl:defconstant hls_sortbycode #x0004)
(cl:defconstant hls_multisel #x0008)
(cl:defconstant hls_border3d #x0010)
(cl:defconstant hls_hilite #x0020)

(cl:defconstant hls_usetabs #x0080)
(cl:defconstant hls_nonintheight #x0100)
(cl:defconstant hls_multicol #x0200)
(cl:defconstant hls_extendedsel #x0800)
(cl:defconstant hls_hungry #x1000)
(cl:defconstant hls_noscroll #x4000)

(cl:defconstant hls_indent hls_border3d)
(cl:defconstant hls_extrude (cl:logior hls_border3d ws_border))
(cl:defconstant hls_standard (cl:logior ws_vscroll hls_indent hls_hilite))

;; notification codes
(cl:defconstant hln_errspace -2)
(cl:defconstant hln_selchange 1)
(cl:defconstant hln_dblclk 2)
(cl:defconstant hln_selcancel 3)
(cl:defconstant hln_setfocus 4)
(cl:defconstant hln_killfocus 5)

;; error codes
(cl:defconstant hlerr -1)
(cl:defconstant hlerr_general -1)
(cl:defconstant hlerr_space -2)
(cl:defconstant hlerr_empty -3)
(cl:defconstant hlerr_badval -4)
(cl:defconstant hlerr_badptr -5)
(cl:defconstant hlerr_nocodes -6)
(cl:defconstant hlerr_notfound -1)

;; search options
(cl:defconstant hl_all #x0000)
(cl:defconstant hl_data #x0001)
(cl:defconstant hl_code #x0002)
(cl:defconstant hl_index #x0004)
(cl:defconstant hl_selected #x0008)
(cl:defconstant hl_unselected #x0010)
(cl:defconstant hl_text #x0020)

;; action options
(cl:defconstant hl_deselect #x0000)
(cl:defconstant hl_select #x0001)
(cl:defconstant hl_toggle #x0002)

;; state flags
(cl:defconstant hlf_changed #x0080)
(cl:defconstant hlf_hascodes #x0200)

;; *************************************************************************\
;; the hcomb control
;; *************************************************************************/

;; messages
(cl:defconstant hcm_first (cl:+ wm_user 250)) ; 
(cl:defconstant hcm_getfont wm_getfont)  ;; returns font.
(cl:defconstant hcm_setfont wm_setfont)  ;; sets font.
(cl:defconstant hcm_getstate (cl:+ hcm_first 21)) ;  ;; returns set state flags.
(cl:defconstant hcm_setstate (cl:+ hcm_first 22)) ;  ;; sets one of state flags.
(cl:defconstant hcm_haschanged hm_haschanged)  ;; returns the status of the changed flag
(cl:defconstant hcm_setchanged hm_setchanged)  ;; sets the status of the changed flag
(cl:defconstant hcm_getdropheight (cl:+ hcm_first 23)) ; 
(cl:defconstant hcm_setdropheight (cl:+ hcm_first 24)) ; 

;; apply to both edit and list controls
(cl:defconstant hcm_getbkgndbrush hm_getbkgndbrush) ;; returns hilitebrush.
(cl:defconstant hcm_getdataclass hm_getdataclass)  ;; returns datatype
(cl:defconstant hcm_getdatasize hm_getdatasize) ;; returns datasize
(cl:defconstant hcm_getdatatype hm_getdatatype) ;; returns datatype
(cl:defconstant hcm_getdatalink hm_getdatalink) ;; returns datalink.
(cl:defconstant hcm_setdatalink hm_setdatalink) ;; sets datalink.
(cl:defconstant hcm_getformat hm_getformat) ;; retrieves format string.
(cl:defconstant hcm_gethilitebrush hm_gethilitebrush)  ;; returns hilitebrush.
(cl:defconstant hcm_gettextcolor hm_gettextcolor)  ;; text color

(cl:defconstant hcm_setbkgndbrush hm_setbkgndbrush) ;; sets hilitebrush.
(cl:defconstant hcm_setformat hm_setformat) ;; sets format string.
(cl:defconstant hcm_sethilitebrush hm_sethilitebrush)  ;; sets hilitebrush.
(cl:defconstant hcm_settextcolor hm_settextcolor)

;; edit control messages
(cl:defconstant hcm_geteditdata hem_getdata) ;; retrieves data.
(cl:defconstant hcm_seteditdata hem_setdata) ;; sets data.
(cl:defconstant hcm_geteditscrollpos hem_getscrollpos) ;; retrieves scrollpos.
(cl:defconstant hcm_seteditscrollpos hem_setscrollpos) ;; sets scrollpos.
(cl:defconstant hcm_geteditsel hem_getsel)  ;; retrieves range of selected text.
(cl:defconstant hcm_seteditsel hem_setsel)  ;; sets range of selected text.
(cl:defconstant hcm_getedittext hem_gettext) ;; retrieves formatted text.
(cl:defconstant hcm_setedittext hem_settext) ;; sets data for string class.
(cl:defconstant hcm_getedittextlen hem_gettextlen) ;; returns textlen.
(cl:defconstant hcm_geteditmaxtextlen hem_getmaxtextlen)  ;; returns textmaxlen.
(cl:defconstant hcm_seteditmaxtextlen hem_setmaxtextlen)  ;; sets textmaxlen.
(cl:defconstant hcm_getoverwritemode hem_getoverwritemode)  ;; returns overwrite/insert status.
(cl:defconstant hcm_setoverwritemode hem_setoverwritemode)  ;; toggles overwrite/insert modes.
(cl:defconstant hcm_update hem_update)  ;; updates text using data or vice-versa.

;; list control messages
(cl:defconstant hcm_additem hlm_additem) ;; add item without codes
(cl:defconstant hcm_deleteitem hlm_deleteitem) ;; delete item by index
(cl:defconstant hcm_findcode hlm_findcode) ;; find item by code
(cl:defconstant hcm_finddata hlm_finddata) ;; find item by data
(cl:defconstant hcm_findstring hlm_findstring) ;; only for ht_string data type
(cl:defconstant hcm_getcodeclass hlm_getcodeclass) ;; get code class
(cl:defconstant hcm_getcodelink hlm_getcodelink)  ;; hot-link to current selection
(cl:defconstant hcm_getcodesize hlm_getcodesize)  ;; get code size in bytes
(cl:defconstant hcm_getcodetype hlm_getcodetype)  ;; get data type
(cl:defconstant hcm_getcount hlm_getcount) ;; get number of items
(cl:defconstant hcm_getcurcode hlm_getcurcode) ;; current item's code
(cl:defconstant hcm_getcurdata hlm_getcurdata) ;; current item's data
(cl:defconstant hcm_getcursel hlm_getcursel)  ;; currently selected item
(cl:defconstant hcm_getcode hlm_getcode) ;; get item code by index
(cl:defconstant hcm_setcode hlm_setcode) ;; set item code by index
(cl:defconstant hcm_getdata hlm_getdata) ;; get item data by index
(cl:defconstant hcm_getscrollpos hlm_getscrollpos) ;; of first visible item
(cl:defconstant hcm_gettext hlm_gettext) ;; get item text by index
(cl:defconstant hcm_gettextlen hlm_gettextlen) ;; get item textlen by index
(cl:defconstant hcm_insertitem hlm_insertitem) ;; insert item without codes
(cl:defconstant hcm_resetcontent hlm_resetcontent) ;; delete all items
(cl:defconstant hcm_selectcode hlm_selectcode) ;; select item by data
(cl:defconstant hcm_selectdata hlm_selectdata) ;; select item by data
(cl:defconstant hcm_selectstring hlm_selectstring)
(cl:defconstant hcm_setcodelink hlm_setcodelink)  ;; hot-link to current selection
(cl:defconstant hcm_setcursel hlm_setcursel)
(cl:defconstant hcm_setscrollpos hlm_setscrollpos)
(cl:defconstant hcm_scroll hlm_scroll)  ;; scroll the list box
(cl:defconstant hcm_settabstops hlm_settabstops)  ;; set tab stops
(cl:defconstant hcm_bequiet hm_bequiet)
(cl:defconstant hcm_isquiet hm_isquiet)

;; styles
(cl:defconstant hcs_dropdown #x0001)
(cl:defconstant hcs_sortbydata #x0002)
(cl:defconstant hcs_sortbycode #x0004)
(cl:defconstant hcs_hasedit #x0008)
(cl:defconstant hcs_border3d #x0010)
(cl:defconstant hcs_hilite #x0020)
(cl:defconstant hcs_usetabs #x0080)
(cl:defconstant hcs_nonintheight #x0100)
(cl:defconstant hcs_wantkeys #x0400)
(cl:defconstant hcs_hungry #x1000)

(cl:defconstant hcs_indent hcs_border3d)
(cl:defconstant hcs_extrude (cl:logior hcs_border3d ws_border))
(cl:defconstant hcs_standard (cl:logior hcs_sortbydata ws_vscroll
				      hcs_indent hcs_hilite))

;; notification codes
(cl:defconstant hcn_errspace -2)
(cl:defconstant hcn_selchange 1)
(cl:defconstant hcn_dblclk 2)
(cl:defconstant hcn_selcancel 3)
(cl:defconstant hcn_setfocus 4)
(cl:defconstant hcn_killfocus 5)
(cl:defconstant hcn_editupdate 6)
(cl:defconstant hcn_dropdown 7)
(cl:defconstant hcn_editchange 8)

;; error codes
(cl:defconstant hcokay 0)
(cl:defconstant hcerr -1)
(cl:defconstant hcerr_general -1)
(cl:defconstant hcerr_space -2)
(cl:defconstant hcerr_empty -3)
(cl:defconstant hcerr_badval -4)
(cl:defconstant hcerr_badptr -5)
(cl:defconstant hcerr_nocodes -6)
(cl:defconstant hcerr_notfound -1)

;; search options
(cl:defconstant hc_all #x0000)
(cl:defconstant hc_data #x0001)
(cl:defconstant hc_code #x0002)
(cl:defconstant hc_index #x0004)
(cl:defconstant hc_text #x0020)

;; listbox state flags
(cl:defconstant hcf_changed #x0080)
(cl:defconstant hcf_hascodes #x0200)


;; *************************************************************************\
;; the hstat control
;; *************************************************************************/

;; messages
(cl:defconstant hsm_first (cl:+ wm_user 400)) ; 
(cl:defconstant hsm_getalign (cl:+ hsm_first 0)) ; 
(cl:defconstant hsm_setalign (cl:+ hsm_first 1)) ; 
(cl:defconstant hsm_getbkgndcolor (cl:+ hsm_first 2)) ; 
(cl:defconstant hsm_setbkgndcolor (cl:+ hsm_first 3)) ; 
(cl:defconstant hsm_getfrgndcolor (cl:+ hsm_first 4)) ; 
(cl:defconstant hsm_setfrgndcolor (cl:+ hsm_first 5)) ; 
(cl:defconstant hsm_getpic (cl:+ hsm_first 6)) ; 
(cl:defconstant hsm_setpic (cl:+ hsm_first 7)) ; 
(cl:defconstant hsm_gettext hm_gettext)
(cl:defconstant hsm_settext hm_settext)
(cl:defconstant hsm_gettype (cl:+ hsm_first 8)) ; 
(cl:defconstant hsm_settype (cl:+ hsm_first 9)) ; 
(cl:defconstant hsm_getpalette (cl:+ hsm_first 10)) ; 

;; constants
(cl:defconstant hs_left 0)
(cl:defconstant hs_center 1)
(cl:defconstant hs_right 2)

;; styles
(cl:defconstant hst_text #x00)
(cl:defconstant hst_group #x01)
(cl:defconstant hst_frame #x02)
(cl:defconstant hst_pic #x03)
(cl:defconstant hst_hline #x04)
(cl:defconstant hst_vline #x05)

(cl:defconstant hst_bitmap #x03)		;;	obsolete - replaced by hst_pic
(cl:defconstant hst_icon #x03)		;;	obsolete - replaced by hst_pic

(cl:defconstant hss_text #x00)
(cl:defconstant hss_group #x01)
(cl:defconstant hss_frame #x02)
(cl:defconstant hss_pic #x03)
(cl:defconstant hss_hline #x04)
(cl:defconstant hss_vline #x05)

(cl:defconstant hss_bitmap #x03)		;;	obsolete - replaced by hss_pic
(cl:defconstant hss_icon #x03)		;;	obsolete - replaced by hss_pic

(cl:defconstant hss_border3d #x0010)
(cl:defconstant hss_bump #x0020)
(cl:defconstant hss_left #x0040)
(cl:defconstant hss_right #x0080) 
(cl:defconstant hss_textindent #x0100)
(cl:defconstant hss_transparent #x0200)

(cl:defconstant hss_extrude (cl:logior hss_border3d ws_border))
(cl:defconstant hss_indent hss_border3d)

;; *************************************************************************\
;; the htool control
;; *************************************************************************/

;; messages
(cl:defconstant htm_getstyle (cl:+ wm_user 10)) ; 
(cl:defconstant htm_setstyle (cl:+ wm_user 11)) ; 
(cl:defconstant htm_getbrush (cl:+ wm_user 12)) ; 
(cl:defconstant htm_setbrush (cl:+ wm_user 13)) ; 
(cl:defconstant htm_getnotify (cl:+ wm_user 14)) ; 
(cl:defconstant htm_setnotify (cl:+ wm_user 15)) ; 
(cl:defconstant htm_getcaption wm_gettext)
(cl:defconstant htm_setcaption wm_settext)
(cl:defconstant htm_getthick (cl:+ wm_user 16)) ; 
(cl:defconstant htm_setthick (cl:+ wm_user 17)) ; 
(cl:defconstant htm_bequiet hm_bequiet)
(cl:defconstant htm_isquiet hm_isquiet)

;; styles
(cl:defconstant hts_float ws_popup)
(cl:defconstant hts_top #x0000)
(cl:defconstant hts_bottom #x0001)
(cl:defconstant hts_left #x0002)
(cl:defconstant hts_right #x0004)
(cl:defconstant hts_nostretch #x0010)

(cl:defconstant hts_ribbon (cl:logior ws_child ws_visible))
(cl:defconstant hts_status (cl:logior ws_child ws_visible hts_bottom))
(cl:defconstant hts_palette (cl:logior hts_float ws_caption ws_visible))

;; ************************************************************************\
;; the hgrid control
;; ************************************************************************/

;; hgrid resource type constant
(cl:defconstant rt_hgrid 300)		
(cl:defconstant ctlcolor_grid 21)

;; messages
(cl:defconstant hgm_first (cl:+ wm_user 300)) ; 
(cl:defconstant hgm_getfont hm_getfont)
(cl:defconstant hgm_setfont hm_setfont)
(cl:defconstant hgm_getbkgndbrush hm_getbkgndbrush)
(cl:defconstant hgm_setbkgndbrush hm_setbkgndbrush)

(cl:defconstant hgm_setgrid (cl:+ hgm_first 0)) ; 
(cl:defconstant hgm_getgrid (cl:+ hgm_first 1)) ;   
(cl:defconstant hgm_resetcontent (cl:+ hgm_first 2)) ; 
(cl:defconstant hgm_update (cl:+ hgm_first 5)) ; 
(cl:defconstant hgm_getbtnheight (cl:+ hgm_first 10)) ; 
(cl:defconstant hgm_setbtnheight (cl:+ hgm_first 11)) ; 
(cl:defconstant hgm_getbtnwidth (cl:+ hgm_first 12)) ; 
(cl:defconstant hgm_setbtnwidth (cl:+ hgm_first 13)) ; 
(cl:defconstant hgm_getrowheight (cl:+ hgm_first 14)) ; 
(cl:defconstant hgm_setrowheight (cl:+ hgm_first 15)) ; 
(cl:defconstant hgm_getcolcount (cl:+ hgm_first 16)) ; 
(cl:defconstant hgm_getrowcount (cl:+ hgm_first 17)) ; 
(cl:defconstant hgm_getfirstcol (cl:+ hgm_first 18)) ; 
(cl:defconstant hgm_setfirstcol (cl:+ hgm_first 19)) ; 
(cl:defconstant hgm_getfirstrec (cl:+ hgm_first 20)) ; 
(cl:defconstant hgm_setfirstrec (cl:+ hgm_first 21)) ; 
(cl:defconstant hgm_getfrozencols (cl:+ hgm_first 22)) ; 
(cl:defconstant hgm_setfrozencols (cl:+ hgm_first 23)) ; 
(cl:defconstant hgm_getselanchor (cl:+ hgm_first 24)) ; 
(cl:defconstant hgm_setselanchor (cl:+ hgm_first 25)) ; 
(cl:defconstant hgm_getselextent (cl:+ hgm_first 26)) ; 
(cl:defconstant hgm_setselextent (cl:+ hgm_first 27)) ; 
(cl:defconstant hgm_getstate (cl:+ hgm_first 28)) ; 
(cl:defconstant hgm_setstate (cl:+ hgm_first 29)) ; 
(cl:defconstant hgm_gettitle (cl:+ hgm_first 30)) ; 
(cl:defconstant hgm_settitle (cl:+ hgm_first 31)) ; 
(cl:defconstant hgm_addfld (cl:+ hgm_first 32)) ; 
(cl:defconstant hgm_deletefld (cl:+ hgm_first 33)) ; 
(cl:defconstant hgm_insertfld (cl:+ hgm_first 34)) ; 
(cl:defconstant hgm_movecol (cl:+ hgm_first 35)) ; 
(cl:defconstant hgm_addrec (cl:+ hgm_first 36)) ; 
(cl:defconstant hgm_deleterec (cl:+ hgm_first 37)) ; 
(cl:defconstant hgm_insertrec (cl:+ hgm_first 38)) ; 
(cl:defconstant hgm_moverow (cl:+ hgm_first 39)) ; 
(cl:defconstant hgm_getbaserec (cl:+ hgm_first 40)) ; 
(cl:defconstant hgm_setbaserec (cl:+ hgm_first 41)) ; 
(cl:defconstant hgm_getcurrec (cl:+ hgm_first 44)) ; 
(cl:defconstant hgm_findfld (cl:+ hgm_first 46)) ; 
(cl:defconstant hgm_getcolmap (cl:+ hgm_first 47)) ; 
(cl:defconstant hgm_setcolmap (cl:+ hgm_first 48)) ; 
(cl:defconstant hgm_bequiet hm_bequiet)
(cl:defconstant hgm_isquiet hm_isquiet)
(cl:defconstant hgm_getbufferproc (cl:+ hgm_first 49)) ; 
(cl:defconstant hgm_setbufferproc (cl:+ hgm_first 50)) ; 
(cl:defconstant hgm_getmaxrec (cl:+ hgm_first 51)) ; 
(cl:defconstant hgm_setmaxrec (cl:+ hgm_first 52)) ; 
(cl:defconstant hgm_getbuffersize (cl:+ hgm_first 53)) ; 
(cl:defconstant hgm_setbuffersize (cl:+ hgm_first 54)) ; 
(cl:defconstant hgm_getstyle (cl:+ hgm_first 55)) ; 
(cl:defconstant hgm_setstyle (cl:+ hgm_first 56)) ; 
(cl:defconstant hgm_getfld (cl:+ hgm_first 57)) ; 
(cl:defconstant hgm_getcol (cl:+ hgm_first 58)) ; 
(cl:defconstant hgm_getrow (cl:+ hgm_first 59)) ; 
(cl:defconstant hgm_getmarker (cl:+ hgm_first 60)) ; 
(cl:defconstant hgm_setmarker (cl:+ hgm_first 61)) ; 
(cl:defconstant hgm_gethscrollpos (cl:+ hgm_first 62)) ; 
(cl:defconstant hgm_getvscrollpos (cl:+ hgm_first 63)) ; 
(cl:defconstant hgm_sethscrollpos (cl:+ hgm_first 64)) ; 
(cl:defconstant hgm_setvscrollpos (cl:+ hgm_first 65)) ;  
(cl:defconstant hgm_getvscrollrange (cl:+ hgm_first 66)) ; 
(cl:defconstant hgm_gethscrollrange (cl:+ hgm_first 67)) ; 

(cl:defconstant hgrm_getstate (cl:+ hgm_first 80)) ; 
(cl:defconstant hgrm_setstate (cl:+ hgm_first 81)) ; 
(cl:defconstant hgrm_getdata (cl:+ hgm_first 82)) ; 
(cl:defconstant hgrm_setdata (cl:+ hgm_first 83)) ; 
(cl:defconstant hgrm_getlink (cl:+ hgm_first 84)) ; 
(cl:defconstant hgrm_setlink (cl:+ hgm_first 85)) ; 
(cl:defconstant hgrm_getsize (cl:+ hgm_first 86)) ; 

(cl:defconstant hgfm_gethctl (cl:+ hgm_first 100)) ; 
(cl:defconstant hgfm_getctlstyle (cl:+ hgm_first 101)) ; 
(cl:defconstant hgfm_getctltype (cl:+ hgm_first 102)) ; 
(cl:defconstant hgfm_getcodeclass (cl:+ hgm_first 103)) ; 
(cl:defconstant hgfm_getcodetype (cl:+ hgm_first 104)) ; 
(cl:defconstant hgfm_getdataclass (cl:+ hgm_first 105)) ; 
(cl:defconstant hgfm_getdatasize (cl:+ hgm_first 106)) ; 
(cl:defconstant hgfm_getdatatype (cl:+ hgm_first 107)) ; 
(cl:defconstant hgfm_getformat (cl:+ hgm_first 108)) ; 
(cl:defconstant hgfm_setformat (cl:+ hgm_first 109)) ; 
(cl:defconstant hgfm_getformatlen (cl:+ hgm_first 110)) ; 
(cl:defconstant hgfm_getname (cl:+ hgm_first 111)) ; 
(cl:defconstant hgfm_getnamelen (cl:+ hgm_first 112)) ; 
(cl:defconstant hgfm_setname (cl:+ hgm_first 113)) ; 
(cl:defconstant hgfm_getcolwidth (cl:+ hgm_first 114)) ; 
(cl:defconstant hgfm_setcolwidth (cl:+ hgm_first 115)) ; 
(cl:defconstant hgfm_getdropheight (cl:+ hgm_first 116)) ; 
(cl:defconstant hgfm_setdropheight (cl:+ hgm_first 117)) ; 
(cl:defconstant hgfm_getstate (cl:+ hgm_first 118)) ; 
(cl:defconstant hgfm_setstate (cl:+ hgm_first 119)) ; 
(cl:defconstant hgfm_getoffset (cl:+ hgm_first 122)) ; 
(cl:defconstant hgfm_freehctl (cl:+ hgm_first 123)) ; 


;; notification codes
(cl:defconstant hgn_setfocus #x0001)
(cl:defconstant hgn_killfocus #x0002)
(cl:defconstant hgn_selchange #x0003)
(cl:defconstant hgn_dblclk #x0004)
(cl:defconstant hgn_destroy #x0005)
(cl:defconstant hgn_errspace #x000d)
(cl:defconstant hgn_maxtext #x000e)
(cl:defconstant hgn_top #x000f)
(cl:defconstant hgn_bottom #x0010)
(cl:defconstant hgn_recnew #x0011)
(cl:defconstant hgn_recswitch #x0012)
(cl:defconstant hgn_recchanged #x0013)
(cl:defconstant hgn_recdelete #x0014)
(cl:defconstant hgn_colmoved #x0015)
(cl:defconstant hgn_rowsized #x0016)
(cl:defconstant hgn_colsized #x0017)
(cl:defconstant hgn_selchanging #x0018)
(cl:defconstant hgn_selextending #x0019)
(cl:defconstant hgn_hscroll #x0020)
(cl:defconstant hgn_vscroll #x0021)
(cl:defconstant hgn_hscrollrange #x0022)
(cl:defconstant hgn_vscrollrange #x0023)

;; error codes
(cl:defconstant hgerr -1)
(cl:defconstant hgerr_notfound -1)

;; styles
(cl:defconstant hgs_browse #x0001)
(cl:defconstant hgs_nolines #x0002)
(cl:defconstant hgs_mdichild #x0004)
(cl:defconstant hgs_inplace #x0008)
(cl:defconstant hgs_autoextend #x0010)
(cl:defconstant hgs_resizerows #x0020)
(cl:defconstant hgs_resizecols #x0040)
(cl:defconstant hgs_rowbuttons #x0080)
(cl:defconstant hgs_colbuttons #x0100)
(cl:defconstant hgs_keybddelins #x0200)
(cl:defconstant hgs_leaveontab #x0400)
(cl:defconstant hgs_nohidesel #x0800)
(cl:defconstant hgs_dragcols #x1000)
(cl:defconstant hgs_wholerows #x2000)
(cl:defconstant hgs_singleselect #x4000)
(cl:defconstant hgs_disablenoscroll #x8000)

;; state flags
(cl:defconstant hgf_changed #x0010)

;; field state flags
(cl:defconstant hgff_hidden #x0001)
(cl:defconstant hgff_browse #x0004)
(cl:defconstant hgff_usecode #x0008)

;; record state flags
(cl:defconstant hgrf_changed #x0001)
(cl:defconstant hgrf_new #x0002)

