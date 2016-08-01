;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: USER; Base: 10; Lowercase: Yes -*-


(in-package :win)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;;; "Translated" from \sys\include\windows.h

#||
;;; OEMRESOURCE
(defconstant OBM_CLOSE		32767)
(defconstant OBM_SIZE		32766)
(defconstant OBM_UPARROW	32765)
(defconstant OBM_DNARROW	32764)
(defconstant OBM_RGARROW	32763)
(defconstant OBM_LFARROW	32762)
(defconstant OBM_BTSIZE		32761)
(defconstant OBM_CHECK		32760)
(defconstant OBM_CHECKBOXES	32759)
(defconstant OBM_BTNCORNERS	32758)
(defconstant OBM_REDUCE		32757)
(defconstant OBM_ZOOM		32756)
(defconstant OBM_RESTORE	32755)
(defconstant OCR_NORMAL		32512)
(defconstant OCR_IBEAM		32513)
(defconstant OCR_WAIT		32514)
(defconstant OCR_CROSS		32515)
(defconstant OCR_UP		32516)
(defconstant OCR_SIZE		32640)
(defconstant OCR_ICON		32641)
(defconstant OCR_SIZENWSE	32642)
(defconstant OCR_SIZENESW	32643)
(defconstant OCR_SIZEWE		32644)
(defconstant OCR_SIZENS		32645)
(defconstant OCR_SIZEALL	32646)

(defconstant OIC_SAMPLE	32512)
(defconstant OIC_HAND	32513)
(defconstant OIC_QUES	32514)
(defconstant OIC_BANG	32515)
(defconstant OIC_NOTE	32516)
||#

#| Scroll bar constants |#
(defconstant SB_HORZ	0)
(defconstant SB_VERT	1)
(defconstant SB_CTL	2)
(defconstant SB_BOTH	3)

#| Scroll Commands |#

(defconstant SB_LINEUP		0)
(defconstant SB_LINEDOWN	1)
(defconstant SB_PAGEUP		2)
(defconstant SB_PAGEDOWN	3)
(defconstant SB_THUMBPOSITION	4)
(defconstant SB_THUMBTRACK	5)
(defconstant SB_TOP		6)
(defconstant SB_BOTTOM		7)
(defconstant SB_ENDSCROLL	8)


#| ShowWindow commands |#

(defconstant SW_HIDE		0)
(defconstant SW_SHOWNORMAL	1)
(defconstant SW_RESTORE		1)
(defconstant SW_NORMAL		1)
(defconstant SW_SHOWMINIMIZED	2)
(defconstant SW_SHOWMAXIMIZED	3)
(defconstant SW_MAXIMIZE	3)
(defconstant SW_SHOWNOACTIVATE	4)
(defconstant SW_SHOW		5)
(defconstant SW_MINIMIZE	6)
(defconstant SW_SHOWMINNOACTIVE	7)
(defconstant SW_SHOWNA		8)

#||
#| Old showwindow commands |#

(defconstant HIDE_WINDOW		0)
(defconstant SHOW_OPENWINDOW		1)
(defconstant SHOW_ICONWINDOW		2)
(defconstant SHOW_FULLSCREEN		3)
(defconstant SHOW_OPENNOACTIVATE	4)

#| identifiers for the WM_SHOWWINDOW message |#
(defconstant SW_PARENTCLOSING	1)
(defconstant SW_OTHERZOOM	2)
(defconstant SW_PARENTOPENING	3)
(defconstant SW_OTHERUNZOOM	4)

#| flags for regions |#
(defconstant ERROR		0)
(defconstant NULLREGION		1)
(defconstant SIMPLEREGION	2)
(defconstant COMPLEXREGION	3)

#| styles for CombineRgn |#
(defconstant RGN_AND	1)
(defconstant RGN_OR	2)
(defconstant RGN_XOR	3)
(defconstant RGN_DIFF	4)
(defconstant RGN_COPY	5)

#| Virtual Keys, Standard Set |#

(defconstant VK_LBUTTON	#x01)
(defconstant VK_RBUTTON	#x02)
(defconstant VK_CANCEL	#x03)
(defconstant VK_MBUTTON	#x04)	#| NOT contiguous with L & RBUTTON |#
(defconstant VK_BACK	#x08)
(defconstant VK_TAB	#x09)
(defconstant VK_CLEAR	#x0C)
(defconstant VK_RETURN	#x0D)
(defconstant VK_SHIFT	#x10)
(defconstant VK_CONTROL	#x11)
(defconstant VK_MENU	#x12)
(defconstant VK_PAUSE	#x13)
(defconstant VK_CAPITAL	#x14)
(defconstant VK_ESCAPE	#x1B)
(defconstant VK_SPACE	#x20)

(defconstant VK_PRIOR	#x21)
(defconstant VK_NEXT	#x22)
(defconstant VK_END	#x23)
(defconstant VK_HOME	#x24)
(defconstant VK_LEFT	#x25)
(defconstant VK_UP	#x26)
(defconstant VK_RIGHT	#x27)
(defconstant VK_DOWN	#x28)

#| VK_A thru VK_Z are the same as their ASCII equivalents: 'A' thru 'Z' |#
#| VK_0 thru VK_9 are the same as their ASCII equivalents: '0' thru '0' |#

(defconstant VK_SELECT	#x29)
(defconstant VK_PRINT	#x2A)
(defconstant VK_EXECUTE	#x2B)
(defconstant VK_COPY	#x2C)
(defconstant VK_INSERT	#x2D)
(defconstant VK_DELETE	#x2E)
(defconstant VK_HELP	#x2F)

(defconstant VK_NUMPAD0	#x60)
(defconstant VK_NUMPAD1	#x61)
(defconstant VK_NUMPAD2	#x62)
(defconstant VK_NUMPAD3	#x63)
(defconstant VK_NUMPAD4	#x64)
(defconstant VK_NUMPAD5	#x65)
(defconstant VK_NUMPAD6	#x66)
(defconstant VK_NUMPAD7	#x67)
(defconstant VK_NUMPAD8	#x68)
(defconstant VK_NUMPAD9	#x69)
(defconstant VK_MULTIPLY #x6A)
(defconstant VK_ADD	#x6B)
(defconstant VK_SEPARATOR #x6C)
(defconstant VK_SUBTRACT #x6D)
(defconstant VK_DECIMAL	#x6E)
(defconstant VK_DIVIDE	#x6F)

(defconstant VK_F1	#x70)
(defconstant VK_F2	#x71)
(defconstant VK_F3	#x72)
(defconstant VK_F4	#x73)
(defconstant VK_F5	#x74)
(defconstant VK_F6	#x75)
(defconstant VK_F7	#x76)
(defconstant VK_F8	#x77)
(defconstant VK_F9	#x78)
(defconstant VK_F10	#x79)
(defconstant VK_F11	#x7A)
(defconstant VK_F12	#x7B)
(defconstant VK_F13	#x7C)
(defconstant VK_F14	#x7D)
(defconstant VK_F15	#x7E)
(defconstant VK_F16	#x7F)

(defconstant VK_NUMLOCK	#x90)


#| SetWindowsHook codes |#
(defconstant WH_MSGFILTER	-1)
(defconstant WH_JOURNALRECORD	0)
(defconstant WH_JOURNALPLAYBACK	1)
(defconstant WH_KEYBOARD	2)
(defconstant WH_GETMESSAGE	3)
(defconstant WH_CALLWNDPROC	4)
(defconstant WH_CBT		5)
(defconstant WH_SYSMSGFILTER	6)
(defconstant WH_WINDOWMGR	7)

#| HC_* Hook Codes |#
(defconstant HC_LPLPFNNEXT	-2)
(defconstant HC_LPFNNEXT	-1)
(defconstant HC_ACTION		0)
(defconstant HC_GETNEXT		1)
(defconstant HC_SKIP		2)
(defconstant HC_NOREM		3)
(defconstant HC_NOREMOVE	3)

#| CBT hook codes |#
(defconstant HCBT_MOVESIZE	0)
(defconstant HCBT_MINMAX	1)
(defconstant HCBT_QS		2)

#| WH_MSGFILTER filter proc codes |#
(defconstant MSGF_DIALOGBOX	0)
(defconstant MSGF_MESSAGEBOX	1)
(defconstant MSGF_MENU		2)
(defconstant MSGF_MOVE		3)
(defconstant MSGF_SIZE		4)
(defconstant MSGF_SCROLLBAR	5)
(defconstant MSGF_NEXTWINDOW	6)

#| Define window manager hook codes |#
(defconstant WC_INIT		1)
(defconstant WC_SWP		2)
(defconstant WC_DEFWINDOWPROC	3)
(defconstant WC_MINMAX		4)
(defconstant WC_MOVE		5)
(defconstant WC_SIZE		6)
(defconstant WC_DRAWCAPTION	7)
||#

#| Binary raster ops |#
(defconstant R2_BLACK		1)	#| 0	|#
(defconstant R2_NOTMERGEPEN	2)	#| DPon	|#
(defconstant R2_MASKNOTPEN	3)	#| DPna	|#
(defconstant R2_NOTCOPYPEN	4)	#| PN	|#
(defconstant R2_MASKPENNOT	5)	#| PDna	|#
(defconstant R2_NOT		6)	#| Dn	|#
(defconstant R2_XORPEN		7)	#| DPx	|#
(defconstant R2_NOTMASKPEN	8)	#| DPan	|#
(defconstant R2_MASKPEN		9)	#| DPa	|#
(defconstant R2_NOTXORPEN	10)	#| DPxn	|#
(defconstant R2_NOP		11)	#| D	|#
(defconstant R2_MERGENOTPEN	12)	#| DPno	|#
(defconstant R2_COPYPEN		13)	#| P	|#
(defconstant R2_MERGEPENNOT	14)	#| PDno	|#
(defconstant R2_MERGEPEN	15)	#| DPo	|#
(defconstant R2_WHITE		16)	#| 1	|#

#||
#| Ternary raster operations |#
(defconstant SRCCOPY	#x00CC0020)	#| dest=source |#
(defconstant SRCPAINT	#x00EE0086)	#| dest=source OR dest |#
(defconstant SRCAND	#x008800C6)	#| dest=source AND dest |#
(defconstant SRCINVERT	#x00660046)	#| dest=source XOR	dest |#
(defconstant SRCERASE	#x00440328)	#| dest=source AND (not dest ) |#
(defconstant NOTSRCCOPY	#x00330008)	#| dest=(not source) |#
(defconstant NOTSRCERASE #x001100A6)	#| dest=(not source) AND (not dest) |#
(defconstant MERGECOPY	#x00C000CA)	#| dest=(source AND pattern) |#
(defconstant MERGEPAINT	#x00BB0226)	#| dest=(NOT source) OR dest |#
(defconstant PATCOPY	#x00F00021)	#| dest=pattern |#
(defconstant PATPAINT	#x00FB0A09)	#| dest=DPSnoo |#
(defconstant PATINVERT	#x005A0049)	#| dest=pattern XOR dest |#
(defconstant DSTINVERT	#x00550009)	#| dest=(not dest) |#
(defconstant BLACKNESS	#x00000042)	#| dest=BLACK |#
(defconstant WHITENESS	#x00FF0062)	#| dest=WHITE |#

#| StretchBlt() modes |#
(defconstant BLACKONWHITE	1)
(defconstant WHITEONBLACK	2)
(defconstant COLORONCOLOR	3)

#| PolyFill modes |#
(defconstant ALTERNATE	1)
(defconstant WINDING	2)

#| text alignment options |#
(defconstant TA_UPDATECP	1)
(defconstant TA_NOUPDATECP	0)

(defconstant TA_LEFT	0)
(defconstant TA_RIGHT	2)
(defconstant TA_CENTER	6)

(defconstant TA_TOP		0)
(defconstant TA_BOTTOM		8)
(defconstant TA_BASELINE	24)

(defconstant ETO_GRAYED		1)
(defconstant ETO_OPAQUE		2)
(defconstant ETO_CLIPPED	4)



(defconstant ASPECT_FILTERING #x00000001)

#| Meta file function numbers |#
(defconstant META_SETBKCOLOR		#x0201)
(defconstant META_SETBKMODE		#x0102)
(defconstant META_SETMAPMODE		#x0103)
(defconstant META_SETROP2		#x0104)
(defconstant META_SETRELABS		#x0105)
(defconstant META_SETPOLYFILLMODE	#x0106)
(defconstant META_SETSTRETCHBLTMODE	#x0107)
(defconstant META_SETTEXTCHAREXTRA	#x0108)
(defconstant META_SETTEXTCOLOR		#x0209)
(defconstant META_SETTEXTJUSTIFICATION	#x020A)
(defconstant META_SETWINDOWORG		#x020B)
(defconstant META_SETWINDOWEXT		#x020C)
(defconstant META_SETVIEWPORTORG	#x020D)
(defconstant META_SETVIEWPORTEXT	#x020E)
(defconstant META_OFFSETWINDOWORG	#x020F)
(defconstant META_SCALEWINDOWEXT	#x0400)
(defconstant META_OFFSETVIEWPORTORG	#x0211)
(defconstant META_SCALEVIEWPORTEXT	#x0412)
(defconstant META_LINETO		#x0213)
(defconstant META_MOVETO		#x0214)
(defconstant META_EXCLUDECLIPRECT	#x0415)
(defconstant META_INTERSECTCLIPRECT	#x0416)
(defconstant META_ARC			#x0817)
(defconstant META_ELLIPSE		#x0418)
(defconstant META_FLOODFILL		#x0419)
(defconstant META_PIE			#x081A)
(defconstant META_RECTANGLE		#x041B)
(defconstant META_ROUNDRECT		#x061C)
(defconstant META_PATBLT		#x061D)
(defconstant META_SAVEDC		#x001E)
(defconstant META_SETPIXEL		#x041F)
(defconstant META_OFFSETCLIPRGN		#x0220)
(defconstant META_TEXTOUT		#x0521)
(defconstant META_BITBLT		#x0922)
(defconstant META_STRETCHBLT		#x0B23)
(defconstant META_POLYGON		#x0324)
(defconstant META_POLYLINE		#x0325)
(defconstant META_ESCAPE		#x0626)
(defconstant META_RESTOREDC		#x0127)
(defconstant META_FILLREGION		#x0228)
(defconstant META_FRAMEREGION		#x0429)
(defconstant META_INVERTREGION		#x012A)
(defconstant META_PAINTREGION		#x012B)
(defconstant META_SELECTCLIPREGION	#x012C)
(defconstant META_SELECTOBJECT		#x012D)
(defconstant META_SETTEXTALIGN		#x012E)
(defconstant META_DRAWTEXT		#x062F)
(defconstant META_CHORD			#x0630)
(defconstant META_CREATEBRUSH		#x00F8)
(defconstant META_CREATEPATTERNBRUSH	#x01F9)
(defconstant META_CREATEPENINDIRECT	#x02FA)
(defconstant META_CREATEFONTINDIRECT	#x02FB)
(defconstant META_CREATEBRUSHINDIRECT	#x02FC)
(defconstant META_CREATEBITMAPINDIRECT	#x02FD)
(defconstant META_CREATEBITMAP		#x06FE)
(defconstant META_CREATEREGION		#x06FF)

#| GDI escapes |#
(defconstant NEWFRAME		1)
(defconstant ABORTDOC		2)
(defconstant NEXTBAND		3)
(defconstant SETCOLORTABLE	4)
(defconstant GETCOLORTABLE	5)
(defconstant FLUSHOUTPUT	6)
(defconstant DRAFTMODE		7)
(defconstant QUERYESCSUPPORT	8)
(defconstant SETABORTPROC	9)
(defconstant STARTDOC		10)
(defconstant ENDDOC		11)
(defconstant GETPHYSPAGESIZE	12)
(defconstant GETPRINTINGOFFSET	13)
(defconstant GETSCALINGFACTOR	14)
(defconstant MFCOMMENT		15)	#| Metafile comment escape |#
(defconstant GETPENWIDTH	16)
(defconstant SETCOPYCOUNT	17)
(defconstant SELECTPAPERSOURCE	18)
(defconstant DEVICEDATA		19)
(defconstant PASSTHROUGH	19)
(defconstant GETTECHNOLGY	20)
(defconstant SETENDCAP		21)
(defconstant SETLINEJOIN	22)
(defconstant SETMITERLIMIT	23)
(defconstant BANDINFO		24)
(defconstant DRAWPATTERNRECT	25)
(defconstant GETVECTORPENSIZE	26)
(defconstant GETVECTORBRUSHSIZE	27)
(defconstant ENABLEDUPLEX	28)
(defconstant ENABLEMANUALFEED	29)



(defconstant GETEXTENDEDTEXTMETRICS	256)
(defconstant GETEXTENTTABLE		257)
(defconstant GETPAIRKERNTABLE		258)
(defconstant GETTRACKKERNTABLE		259)

(defconstant EXTTEXTOUT			512)

(defconstant ENABLERELATIVEWIDTHS	768)
(defconstant ENABLEPAIRKERNING		769)
(defconstant SETKERNTRACK		770)
(defconstant STRETCHBLT			2048)


#| spooler error code |#
(defconstant SP_NOTREPORTED	#x4000)	#| set if GDI did not report error |#
(defconstant SP_ERROR		-1)	
		#| general errors who know what went wrong |#
(defconstant SP_APPABORT	-2)
		#| app aborted the job - callback function returned false |#
(defconstant SP_USERABORT	-3)
		#| user aborted the job through spooler's front end |#
(defconstant SP_OUTOFDISK	-4)	#| not enough disk space to spool |#
(defconstant SP_OUTOFMEMORY	-5)

#| spooler WM_SPOOLERSTATUS wparm classes |#

(defconstant PR_JOBSTATUS	#x0000)

#| Object definitions for GDI EnumObjects. |#
(defconstant OBJ_PEN	1)
(defconstant OBJ_BRUSH	2)

#| Window field offsets for GetWindowLong & GetWindowWord |#
(defconstant GWL_WNDPROC	-4)
(defconstant GWW_HINSTANCE	-6)
(defconstant GWW_HWNDPARENT	-8)
(defconstant GWW_HWNDTEXT	-10)
(defconstant GWW_ID		-12)
(defconstant GWL_STYLE		-16)

#| Class field offsets for GetClassLong & GetClassWord |#
(defconstant GCL_MENUNAME	-8)
(defconstant GCW_HBRBACKGROUND	-10)
(defconstant GCW_HCURSOR	-12)
(defconstant GCW_HICON		-14)
(defconstant GCW_HMODULE	-16)
(defconstant GCW_CBWNDEXTRA	-18)
(defconstant GCW_CBCLSEXTRA	-20)
(defconstant GCL_WNDPROC	-24)
(defconstant GCW_STYLE		-26)

#| ** Window Procedure Messages |#

(defconstant WM_NULL		#x0000)
(defconstant WM_CREATE		#x0001)
(defconstant WM_DESTROY		#x0002)
||#
(defconstant WM_MOVE		#x0003)
#||
(defconstant WM_SIZEWAIT	#x0004)
||#
(defconstant WM_SIZE		#x0005)
#||
(defconstant WM_ACTIVATE	#x0006)
(defconstant WM_SETFOCUS	#x0007)
(defconstant WM_KILLFOCUS	#x0008)
(defconstant WM_SETVISIBLE	#x0009)
(defconstant WM_ENABLE		#x000a)
(defconstant WM_SETREDRAW	#x000b)
(defconstant WM_SETTEXT		#x000c)
(defconstant WM_GETTEXT		#x000d)
(defconstant WM_GETTEXTLENGTH	#x000e)
||#
(defconstant WM_PAINT		#x000f)
#||
(defconstant WM_CLOSE		#x0010)
(defconstant WM_QUERYENDSESSION	#x0011)
(defconstant WM_QUIT		#x0012)
(defconstant WM_QUERYOPEN	#x0013)
(defconstant WM_ERASEBKGND	#x0014)
(defconstant WM_SYSCOLORCHANGE	#x0015)
(defconstant WM_ENDSESSION	#x0016)
(defconstant WM_SYSTEMERROR	#x0017)
(defconstant WM_SHOWWINDOW	#x0018)
(defconstant WM_CTLCOLOR	#x0019)
(defconstant WM_WININICHANGE	#x001A)
(defconstant WM_DEVMODECHANGE	#x001B)
(defconstant WM_ACTIVATEAPP	#x001C)
(defconstant WM_FONTCHANGE	#x001D)
(defconstant WM_TIMECHANGE	#x001E)
(defconstant WM_CANCELMODE	#x001F)
(defconstant WM_SETCURSOR	#x0020)
(defconstant WM_MOUSEACTIVATE	#x0021)
(defconstant WM_CHILDACTIVATE	#x0022)
(defconstant WM_QUEUESYNC	#x0023)
(defconstant WM_GETMINMAXINFO	#x0024)
(defconstant WM_PAINTICON	#x0026)
(defconstant WM_ICONERASEBKGND	#x0027)
(defconstant WM_NEXTDLGCTL	#x0028)
(defconstant WM_ALTTABACTIVE	#x0029)	#| for win386 only |#
(defconstant WM_SPOOLERSTATUS	#x002A)


(defconstant WM_NCCREATE	#x0081)
(defconstant WM_NCDESTROY	#x0082)
(defconstant WM_NCCALCSIZE	#x0083)
(defconstant WM_NCHITTEST	#x0084)
(defconstant WM_NCPAINT		#x0085)
(defconstant WM_NCACTIVATE	#x0086)
(defconstant WM_GETDLGCODE	#x0087)
(defconstant WM_SYNCPAINT	#x0088)
(defconstant WM_SYNCTASK	#x0089)

(defconstant ST_BEGINSWP	0)
(defconstant ST_ENDSWP		1)

(defconstant WM_NCMOUSEMOVE	#x00a0)
(defconstant WM_NCLBUTTONDOWN	#x00a1)
(defconstant WM_NCLBUTTONUP	#x00a2)
(defconstant WM_NCLBUTTONDBLCLK	#x00a3)
(defconstant WM_NCRBUTTONDOWN	#x00a4)
(defconstant WM_NCRBUTTONUP	#x00a5)
(defconstant WM_NCRBUTTONDBLCLK	#x00a6)
(defconstant WM_NCMBUTTONDOWN	#x00a7)
(defconstant WM_NCMBUTTONUP	#x00a8)
(defconstant WM_NCMBUTTONDBLCLK	#x00a9)

#| WINWhere area codes |#
(defconstant HTERROR		-2)
(defconstant HTTRANSPARENT	-1)
(defconstant HTNOWHERE		0)
(defconstant HTCLIENT		1)
(defconstant HTCAPTION		2)
(defconstant HTSYSMENU		3)
(defconstant HTGROWBOX		4)
(defconstant HTSIZE		HTGROWBOX)
(defconstant HTMENU		5)
(defconstant HTHSCROLL		6)
(defconstant HTVSCROLL		7)
(defconstant HTREDUCE		8)
(defconstant HTZOOM		9)
(defconstant HTLEFT		10)
(defconstant HTRIGHT		11)
(defconstant HTTOP		12)
(defconstant HTTOPLEFT		13)
(defconstant HTTOPRIGHT		14)
(defconstant HTBOTTOM		15)
(defconstant HTBOTTOMLEFT	16)
(defconstant HTBOTTOMRIGHT	17)
(defconstant HTSIZEFIRST	HTLEFT)
(defconstant HTSIZELAST		HTBOTTOMRIGHT)

#| WM_MOUSEACTIVATE return codes |#
(defconstant MA_ACTIVATE	1)
(defconstant MA_ACTIVATEANDEAT	2)
(defconstant MA_NOACTIVATE	3)

(defconstant WM_KEYFIRST	#x0100)
(defconstant WM_KEYLAST		#x0108)

||#
(defconstant WM_KEYDOWN		#x0100)
(defconstant WM_KEYUP		#x0101)
(defconstant WM_CHAR		#x0102)
(defconstant WM_SYSCHAR		#x0106)
#||
(defconstant WM_DEADCHAR	#x0103)
(defconstant WM_SYSKEYDOWN	#x0104)
(defconstant WM_SYSKEYUP	#x0105)

(defconstant WM_SYSDEADCHAR	#x0107)
(defconstant WM_YOMICHAR	#x0108)
(defconstant WM_CONVERTREQUEST	#x010A)
(defconstant WM_CONVERTRESULT	#x010B)

(defconstant WM_INITDIALOG	#x0110)
(defconstant WM_COMMAND		#x0111)
(defconstant WM_SYSCOMMAND	#x0112)
(defconstant WM_TIMER		#x0113)
||#
(defconstant WM_HSCROLL		#x0114)
(defconstant WM_VSCROLL		#x0115)
#||
(defconstant WM_INITMENU	#x0116)
(defconstant WM_INITMENUPOPUP	#x0117)
(defconstant WM_SYSTIMER	#x0118)
(defconstant WM_MENUSELECT	#x011f)
(defconstant WM_MENUCHAR	#x0120)
(defconstant WM_ENTERIDLE	#x0121)


(defconstant WM_MOUSEFIRST	#x0200)
(defconstant WM_MOUSELAST	#x0209)
||#
(defconstant WM_MOUSEMOVE	#x0200)	#| mouse related constants |#
(defconstant WM_LBUTTONDOWN	#x0201)
(defconstant WM_LBUTTONUP	#x0202)
(defconstant WM_LBUTTONDBLCLK	#x0203)
(defconstant WM_RBUTTONDOWN	#x0204)
(defconstant WM_RBUTTONUP	#x0205)
(defconstant WM_RBUTTONDBLCLK	#x0206)
(defconstant WM_MBUTTONDOWN	#x0207)
(defconstant WM_MBUTTONUP	#x0208)
(defconstant WM_MBUTTONDBLCLK	#x0209)
#||
(defconstant WM_KANJIFIRST	#x0280)
(defconstant WM_KANJILAST	#x029f)

#| clipboard messages |#
(defconstant WM_CUT		#x0300)
(defconstant WM_COPY		#x0301)
(defconstant WM_PASTE		#x0302)
(defconstant WM_CLEAR		#x0303)
(defconstant WM_UNDO		#x0304)
(defconstant WM_RENDERFORMAT	#x0305)
(defconstant WM_RENDERALLFORMATS #x0306)
(defconstant WM_DESTROYCLIPBOARD #x0307)
(defconstant WM_DRAWCLIPBOARD	#x0308)
(defconstant WM_PAINTCLIPBOARD	#x0309)
(defconstant WM_VSCROLLCLIPBOARD #x030A)
(defconstant WM_SIZECLIPBOARD	#x030B)
(defconstant WM_ASKCBFORMATNAME	#x030C)
(defconstant WM_CHANGECBCHAIN	#x030D)
(defconstant WM_HSCROLLCLIPBOARD #x030E)

#| #x03f0 to #x03ff are reserved |#
#| private window messages start here |#
(defconstant WM_USER		#x0400)
||#

#| Size message commands |#
(defconstant SIZENORMAL		0)
(defconstant SIZEICONIC		1)
(defconstant SIZEFULLSCREEN	2)
(defconstant SIZEZOOMSHOW	3)
(defconstant SIZEZOOMHIDE	4)
#| Key state masks for mouse messages |#
(defconstant MK_LBUTTON	#x0001)
(defconstant MK_RBUTTON	#x0002)
(defconstant MK_SHIFT	#x0004)
(defconstant MK_CONTROL	#x0008)
(defconstant MK_MBUTTON	#x0010)


#| Window styles |#
;;;*** Note: we've shifted these right by 4 bits to make them fixnums;
;;;***  see the definition of STORE-28BIT.
(defconstant WS_TILED		#x0000000)
(defconstant WS_OVERLAPPED	WS_TILED)
(defconstant WS_ICONICPOPUP	#xc000000)
(defconstant WS_POPUP		#x8000000)
(defconstant WS_CHILD		#x4000000)
(defconstant WS_MINIMIZE	#x2000000)
(defconstant WS_VISIBLE		#x1000000)
(defconstant WS_DISABLED	#x0800000)
(defconstant WS_CLIPSIBLINGS	#x0400000)
(defconstant WS_CLIPCHILDREN	#x0200000)
(defconstant WS_MAXIMIZE	#x0100000)

(defconstant WS_BORDER		#x0080000)
(defconstant WS_CAPTION		#x00c0000)
(defconstant WS_DLGFRAME	#x0040000)
(defconstant WS_VSCROLL		#x0020000)
(defconstant WS_HSCROLL		#x0010000)
(defconstant WS_SYSMENU		#x0008000)
(defconstant WS_SIZEBOX		#x0004000)
(defconstant WS_THICKFRAME	#x0004000)
(defconstant WS_GROUP		#x0002000)
(defconstant WS_TABSTOP		#x0001000)

(defconstant WS_MINIMIZEBOX	#x0002000)
(defconstant WS_MAXIMIZEBOX	#x0001000)

(defconstant WS_ICONIC		WS_MINIMIZE)

#| Class styles |#
(defconstant CS_VREDRAW		#x0001)
(defconstant CS_HREDRAW		#x0002)
(defconstant CS_KEYCVTWINDOW	#x0004)
(defconstant CS_DBLCLKS		#x0008)
(defconstant CS_OEMCHARS	#x0010)
(defconstant CS_OWNDC		#x0020)
(defconstant CS_CLASSDC		#x0040)
(defconstant CS_PARENTDC	#x0080)
(defconstant CS_NOKEYCVT	#x0100)
(defconstant CS_SAVEBITS	#x0800)
(defconstant CS_NOCLOSE		#x0200)
(defconstant CS_BYTEALIGNCLIENT #x1000)
(defconstant CS_BYTEALIGNWINDOW #x2000)

#| Shorthand for the common cases |#
(defconstant WS_TILEDWINDOW	(logior WS_TILED WS_CAPTION WS_SYSMENU
				 WS_THICKFRAME WS_MINIMIZEBOX WS_MAXIMIZEBOX))
(defconstant WS_OVERLAPPEDWINDOW	WS_TILEDWINDOW)
(defconstant WS_POPUPWINDOW	(logior WS_POPUP WS_BORDER WS_SYSMENU))
(defconstant WS_CHILDWINDOW	WS_CHILD)

#||
#| clipboard metafile picture structure |#

#| predefined clipboard formats |#
(defconstant CF_TEXT		1)
(defconstant CF_BITMAP		2)
(defconstant CF_METAFILEPICT	3)
(defconstant CF_SYLK		4)
(defconstant CF_DIF		5)
(defconstant CF_TIFF		6)
(defconstant CF_OEMTEXT		7)

(defconstant CF_OWNERDISPLAY	#x80)	#| owner display |#
(defconstant CF_DSPTEXT		#x81)	#| display text |#
(defconstant CF_DSPBITMAP	#x82)	#| display bitmap |#
(defconstant CF_DSPMETAFILEPICT	#x83)	#| display metafile |#

#| Private clipboard format range |#
(defconstant CF_PRIVATEFIRST	#x200)	#| Anything in this range doesn't |#
(defconstant CF_PRIVATELAST	#x2FF)	#| get GlobalFree'd |#
(defconstant CF_GDIOBJFIRST	#x300)	#| Anything in this range gets |#
(defconstant CF_GDIOBJLAST	#x3FF)	#| DeleteObject'ed |#

#| Logical font constants |#

(defconstant OUT_DEFAULT_PRECIS		0)
(defconstant OUT_STRING_PRECIS		1)
(defconstant OUT_CHARACTER_PRECIS	2)
(defconstant OUT_STROKE_PRECIS		3)

(defconstant CLIP_DEFAULT_PRECIS	0)
(defconstant CLIP_CHARACTER_PRECIS	1)
(defconstant CLIP_STROKE_PRECIS		2)

(defconstant DEFAULT_QUALITY	0)
(defconstant DRAFT_QUALITY	1)
(defconstant PROOF_QUALITY	2)

(defconstant DEFAULT_PITCH	0)
(defconstant FIXED_PITCH	1)
(defconstant VARIABLE_PITCH	2)

(defconstant ANSI_CHARSET	0)
(defconstant SHIFTJIS_CHARSET	128)	#| Kanji CharSet |#
(defconstant OEM_CHARSET	255)

#| GDI font families. |#
(defconstant FF_DONTCARE 0)	#| Don't care or don't know. |#
(defconstant FF_ROMAN	16)	#| Variable stroke width, serifed. |#
				#| Times Roman, Century Schoolbook, etc. |#
(defconstant FF_SWISS	32)	#| Variable stroke width, sans-serifed. |#
				#| Helvetica, Swiss, etc. |#
(defconstant FF_MODERN	48)	#| Constant stroke width, serif or sans. |#
				#| Pica, Elite, Courier, etc. |#
(defconstant FF_SCRIPT	64)	#| Cursive, etc. |#
(defconstant FF_DECORATIVE 80)	#| Old English, etc. |#

#| Font weights lightest to darkest. |#
(defconstant FW_DONTCARE	0)
(defconstant FW_THIN		100)
(defconstant FW_EXTRALIGHT	200)
(defconstant FW_LIGHT		300)
(defconstant FW_NORMAL		400)
(defconstant FW_MEDIUM		500)
(defconstant FW_SEMIBOLD	600)
(defconstant FW_BOLD		700)
(defconstant FW_EXTRABOLD	800)
(defconstant FW_HEAVY		900)

(defconstant FW_ULTRALIGHT	FW_EXTRALIGHT)
(defconstant FW_REGULAR		FW_NORMAL)
(defconstant FW_DEMIBOLD	FW_SEMIBOLD)
(defconstant FW_ULTRABOLD	FW_EXTRABOLD)
(defconstant FW_BLACK		FW_HEAVY)


#| EnumFonts masks. |#
(defconstant RASTER_FONTTYPE #x0001)
(defconstant DEVICE_FONTTYPE #x0002)

#| GDI rgb values packed into a dword |#

#| +++ make these into Lisp macros!
(defconstant RGB(r,g,b) (( ((b) << 8 | (g)) << 8) | (r)))
(defconstant GetRValue(rgb) ((BYTE)(rgb)))
(defconstant GetGValue(rgb) ((BYTE)(((WORD)(rgb)) >> 8)))
(defconstant GetBValue(rgb) ((BYTE)((rgb)>>16)))
|#

#| GDI Background Modes |#

(defconstant TRANSPARENT	1)
(defconstant OPAQUE		2)

#| GDI map modes |#
(defconstant MM_TEXT		1)
(defconstant MM_LOMETRIC	2)
(defconstant MM_HIMETRIC	3)
(defconstant MM_LOENGLISH	4)
(defconstant MM_HIENGLISH	5)
(defconstant MM_TWIPS		6)
(defconstant MM_ISOTROPIC	7)
(defconstant MM_ANISOTROPIC	8)

#| GDI coordinate modes |#

(defconstant ABSOLUTE	1)
(defconstant RELATIVE	2)
||#

#| Stock Logical Objects |#
(defconstant WHITE_BRUSH	0)
(defconstant LTGRAY_BRUSH	1)
(defconstant GRAY_BRUSH		2)
(defconstant DKGRAY_BRUSH	3)
(defconstant BLACK_BRUSH	4)
(defconstant NULL_BRUSH		5)
(defconstant HOLLOW_BRUSH	NULL_BRUSH)
(defconstant WHITE_PEN		6)
(defconstant BLACK_PEN		7)
(defconstant NULL_PEN		8)
(defconstant OEM_FIXED_FONT	10)
(defconstant ANSI_FIXED_FONT	11)
(defconstant ANSI_VAR_FONT	12)
(defconstant SYSTEM_FONT	13)
(defconstant DEVICEDEFAULT_FONT 14)

#| GDI Brush Style definitions. |#

(defconstant BS_SOLID	0)
(defconstant BS_NULL	1)
(defconstant BS_HOLLOW	BS_NULL)
(defconstant BS_HATCHED	2)
(defconstant BS_PATTERN	3)
(defconstant BS_INDEXED	4)

#| GDI Hatch Style definitions. |#

(defconstant HS_HORIZONTAL	0)	#| ----- |#
(defconstant HS_VERTICAL	1)	#| ||||| |#
(defconstant HS_FDIAGONAL	2)	#| ///// |#
(defconstant HS_BDIAGONAL	3)	#| \\\\\ |#
(defconstant HS_CROSS		4)	#| +++++ |#
(defconstant HS_DIAGCROSS	5)	#| xxxxx |#


#| GDI Pen Style definitions |#
(defconstant PS_SOLID		0)	#| solid pen |#
(defconstant PS_DASH		1)	#| ------- |#
(defconstant PS_DOT		2)	#| ....... |#
(defconstant PS_DASHDOT		3)	#| _._._._ |#
(defconstant PS_DASHDOTDOT	4)	#| _.._.._ |#
(defconstant PS_NULL		5)	#|         |#

#||
#| Device Parameters for GetDeviceCaps() |#

(defconstant DRIVERVERSION	0)	#| Device driver version |#
(defconstant TECHNOLOGY		2)	#| Device classification |#
(defconstant HORZSIZE		4)	#| Horizontal size in millimeters |#
(defconstant VERTSIZE		6)	#| Vertical size in millimeters |#
(defconstant HORZRES		8)	#| Horizontal width in pixels |#
(defconstant VERTRES		10)	#| Vertical width in pixels |#
(defconstant BITSPIXEL		12)	#| Number of bits per pixel |#
(defconstant PLANES		14)	#| Number of planes |#
(defconstant NUMBRUSHES		16)	#| Number of brushes the device has |#
(defconstant NUMPENS		18)	#| Number of pens the device has |#
(defconstant NUMMARKERS		20)	#| Number of markers the device has |#
(defconstant NUMFONTS		22)	#| Number of fonts the device has |#
(defconstant NUMCOLORS		24)
(defconstant PDEVICESIZE	26)	#| Size required for device descriptor |#
(defconstant CURVECAPS		28)	#| Curves capabilities |#
(defconstant LINECAPS		30)	#| Line	capabilities |#
(defconstant POLYGONALCAPS	32)	#| Polygonal capabilities |#
(defconstant TEXTCAPS		34)	#| Text	capabilities |#
(defconstant CLIPCAPS		36)	#| Clipping	capabilities |#
(defconstant RASTERCAPS		38)	#| Bitblt	capabilities |#
(defconstant ASPECTX		40)	#| Length of the X leg |#
(defconstant ASPECTY		42)	#| Length of the Y leg |#
(defconstant ASPECTXY		44)	#| Length of the hypotenuse |#

(defconstant LOGPIXELSX		88)	#| Logical pixels/inch in X |#
(defconstant LOGPIXELSY		90)	#| Logical pixels/inch in Y |#

#| Device capability masks |#
#| Device Technologies |#

(defconstant DT_PLOTTER		0)	#| Vector plotter |#
(defconstant DT_RASDISPLAY	1)	#| Raster display |#
(defconstant DT_RASPRINTER	2)	#| Raster printer |#
(defconstant DT_RASCAMERA	3)	#| Raster camera |#
(defconstant DT_CHARSTREAM	4)	#| Character-stream, PLP |#
(defconstant DT_METAFILE	5)	#| Metafile, VDM |#
(defconstant DT_DISPFILE	6)	#| Display-file |#

#| Curve Capabilities |#

(defconstant CC_NONE		0)	#| Curves not supported |#
(defconstant CC_CIRCLES		1)	#| Can do circles |#
(defconstant CC_PIE		2)	#| Can do pie wedges |#
(defconstant CC_CHORD		4)	#| Can do chord arcs |#
(defconstant CC_ELLIPSES	8)	#| Can do ellipese |#
(defconstant CC_WIDE		16)	#| Can do wide lines |#
(defconstant CC_STYLED		32)	#| Can do styled lines |#
(defconstant CC_WIDESTYLED	64)	#| Can do wide styled lines|#
(defconstant CC_INTERIORS	128)	#| Can do interiors |#

#| Line Capabilities |#

(defconstant LC_NONE		0)	#| Lines not supported |#
(defconstant LC_POLYLINE	2)	#| Can do polylines |#
(defconstant LC_MARKER		4)	#| Can do markers |#
(defconstant LC_POLYMARKER	8)	#| Can do polymarkers |#
(defconstant LC_WIDE		16)	#| Can do wide lines |#
(defconstant LC_STYLED		32)	#| Can do styled lines |#
(defconstant LC_WIDESTYLED	64)	#| Can do wide styled lines|#
(defconstant LC_INTERIORS	128)	#| Can do interiors |#

#| Polygonal Capabilities |#

(defconstant PC_NONE		0)	#| Polygonals not supported|#
(defconstant PC_POLYGON		1)	#| Can do polygons |#
(defconstant PC_RECTANGLE	2)	#| Can do rectangles |#
(defconstant PC_TRAPEZOID	4)	#| Can do trapezoids |#
(defconstant PC_SCANLINE	8)	#| Can do scanlines |#
(defconstant PC_WIDE		16)	#| Can do wide borders |#
(defconstant PC_STYLED		32)	#| Can do styled borders |#
(defconstant PC_WIDESTYLED	64)	#| Can do wide styled borders|#
(defconstant PC_INTERIORS	128)	#| Can do interiors |#

#| Polygonal Capabilities |#

(defconstant CP_NONE		0)	#| no clipping of Output |#
(defconstant CP_RECTANGLE	1)	#| Output clipped to Rects |#

#| Text Capabilities |#

(defconstant TC_OP_CHARACTER	#x0001)	#| Can do OutputPrecision CHARACTER |#
(defconstant TC_OP_STROKE	#x0002)	#| Can do OutputPrecision STROKE |#
(defconstant TC_CP_STROKE	#x0004)	#| Can do ClipPrecision	STROKE |#
(defconstant TC_CR_90		#x0008)	#| Can do CharRotAbility	90 |#
(defconstant TC_CR_ANY		#x0010)	#| Can do CharRotAbility	ANY |#
(defconstant TC_SF_X_YINDEP	#x0020)	#|  " " ScaleFreedom X_YINDEPENDENT |#
(defconstant TC_SA_DOUBLE	#x0040)	#| Can do ScaleAbility DOUBLE |#
(defconstant TC_SA_INTEGER	#x0080)	#| Can do ScaleAbility INTEGER |#
(defconstant TC_SA_CONTIN	#x0100)	#| Can do ScaleAbility CONTINUOUS |#
(defconstant TC_EA_DOUBLE	#x0200)	#| Can do EmboldenAbility DOUBLE |#
(defconstant TC_IA_ABLE		#x0400)	#| Can do ItalisizeAbility ABLE |#
(defconstant TC_UA_ABLE		#x0800)	#| Can do UnderlineAbility ABLE |#
(defconstant TC_SO_ABLE		#x1000)	#| Can do StrikeOutAbility ABLE |#
(defconstant TC_RA_ABLE		#x2000)	#| Can do RasterFontAble ABLE |#
(defconstant TC_VA_ABLE		#x4000)	#| Can do VectorFontAble ABLE |#
(defconstant TC_RESERVED	#x8000)	#| Reserved. |#

#| Raster Capabilities |#

(defconstant RC_BITBLT 		1)	#| Can do standard non-stretching, non-inverting BLT. |#
(defconstant RC_BANDING		2)	#| Device requires banding support |#
(defconstant RC_SCALING		4)	#| Device requires scaling support |#
(defconstant RC_BITMAP64	8)	#| Device can support >64K bitmap |#


#| PeekMessage options |#
(defconstant PM_REMOVE		1)
(defconstant PM_NOREMOVE	0)
(defconstant PM_NOYIELD		2)

(defconstant CW_USEDEFAULT	#x8000)	#|used on both x and cx |#
||#

#| SetWindowPos flags |#

(defconstant SWP_NOSIZE		#x01)
(defconstant SWP_NOMOVE		#x02)
(defconstant SWP_NOZORDER	#x04)
(defconstant SWP_NOREDRAW	#x08)
(defconstant SWP_NOACTIVATE	#x10)
(defconstant SWP_DRAWFRAME	#x20)
(defconstant SWP_SHOWWINDOW	#x40)
(defconstant SWP_HIDEWINDOW	#x80)
(defconstant SWP_NOCOPYBITS	#x0100)
(defconstant SWP_NOREPOSITION	#x200)

#||
#| DrawFrame and associated defines |#
(defconstant DF_SHIFT0		#x0000)
(defconstant DF_SHIFT1		#x0001)
(defconstant DF_SHIFT2		#x0002)
(defconstant DF_SHIFT3		#x0003)
(defconstant DF_PATCOPY		#x0000)
(defconstant DF_PATINVERT	#x0004)

#| +++ Fix these !!!
(defconstant DF_SCROLLBAR	(COLOR_SCROLLBAR << 3))
(defconstant DF_BACKGROUND	(COLOR_BACKGROUND << 3))
(defconstant DF_ACTIVECAPTION	(COLOR_ACTIVECAPTION << 3))
(defconstant DF_INACTIVECAPTION	(COLOR_INACTIVECAPTION << 3))
(defconstant DF_MENU		(COLOR_MENU << 3))
(defconstant DF_WINDOW		(COLOR_WINDOW << 3))
(defconstant DF_WINDOWFRAME	(COLOR_WINDOWFRAME << 3))
(defconstant DF_MENUTEXT	(COLOR_MENUTEXT << 3))
(defconstant DF_WINDOWTEXT	(COLOR_WINDOWTEXT << 3))
(defconstant DF_CAPTIONTEXT	(COLOR_CAPTIONTEXT << 3))
(defconstant DF_ACTIVEBORDER	(COLOR_ACTIVEBORDER << 3))
(defconstant DF_INACTIVEBORDER	(COLOR_INACTIVEBORDER << 3))
(defconstant DF_APPWORKSPACE	(COLOR_APPWORKSPACE << 3))
(defconstant DF_GRAY		(DF_APPWORKSPACE + (1 << 3)))
|#

#| DrawText format flags |#
(defconstant DT_LEFT		#x00)
(defconstant DT_CENTER		#x01)
(defconstant DT_RIGHT		#x02)
(defconstant DT_TOP		#x00)
(defconstant DT_VCENTER		#x04)
(defconstant DT_BOTTOM		#x08)
(defconstant DT_WORDBREAK	#x10)
(defconstant DT_SINGLELINE	#x20)
(defconstant DT_EXPANDTABS	#x40)
(defconstant DT_TABSTOP		#x80)
(defconstant DT_NOCLIP		#x100)
(defconstant DT_EXTERNALLEADING #x200)
(defconstant DT_CALCRECT	#x400)
(defconstant DT_NOPREFIX	#x800)
(defconstant DT_INTERNAL	#x1000)

#| GetSystemMetrics codes |#
(defconstant SM_CXSCREEN	0)
(defconstant SM_CYSCREEN	1)
(defconstant SM_CXVSCROLL	2)
(defconstant SM_CYHSCROLL	3)
(defconstant SM_CYCAPTION	4)
(defconstant SM_CXBORDER	5)
(defconstant SM_CYBORDER	6)
(defconstant SM_CXDLGFRAME	7)
(defconstant SM_CYDLGFRAME	8)
(defconstant SM_CYVTHUMB	9)
(defconstant SM_CXHTHUMB	10)
(defconstant SM_CXICON		11)
(defconstant SM_CYICON		12)
(defconstant SM_CXCURSOR	13)
(defconstant SM_CYCURSOR	14)
(defconstant SM_CYMENU		15)
(defconstant SM_CXFULLSCREEN	16)
(defconstant SM_CYFULLSCREEN	17)
(defconstant SM_CYKANJIWINDOW	18)
(defconstant SM_MOUSEPRESENT	19)
(defconstant SM_CYVSCROLL	20)
(defconstant SM_CXHSCROLL	21)
(defconstant SM_DEBUG		22)
(defconstant SM_SWAPBUTTON	23)
(defconstant SM_RESERVED1	24)
(defconstant SM_RESERVED2	25)
(defconstant SM_RESERVED3	26)	#| new additions since 2.0 |#
(defconstant SM_RESERVED4	27)
(defconstant SM_CXMIN		28)
(defconstant SM_CYMIN		29)
(defconstant SM_CXSIZE		30)
(defconstant SM_CYSIZE		31)
(defconstant SM_CXFRAME		32)
(defconstant SM_CYFRAME		33)
(defconstant SM_CXMINTRACK	34)
(defconstant SM_CYMINTRACK	35)
(defconstant SM_CMETRICS	36)

#| MessageBox type flags |#
(defconstant MB_OK		#x0000)
(defconstant MB_OKCANCEL	#x0001)
(defconstant MB_ABORTRETRYIGNORE #x0002)
(defconstant MB_YESNOCANCEL	#x0003)
(defconstant MB_YESNO		#x0004)
(defconstant MB_RETRYCANCEL	#x0005)

(defconstant MB_ICONHAND	#x0010)
(defconstant MB_ICONQUESTION	#x0020)
(defconstant MB_ICONEXCLAMATION	#x0030)
(defconstant MB_ICONASTERISK	#x0040)

(defconstant MB_DEFBUTTON1	#x0000)
(defconstant MB_DEFBUTTON2	#x0100)
(defconstant MB_DEFBUTTON3	#x0200)

(defconstant MB_APPLMODAL	#x0000)
(defconstant MB_SYSTEMMODAL	#x1000)
(defconstant MB_NOFOCUS		#x8000)
(defconstant MB_MISCMASK	#xC000)
(defconstant MB_TYPEMASK	#x000F)
(defconstant MB_ICONMASK	#x00F0)
(defconstant MB_DEFMASK		#x0F00)
(defconstant MB_MODEMASK	#x3000)

#| color type indices |#
#| for the WM_CTLCOLOR message |#
(defconstant CTLCOLOR_MSGBOX	0)
(defconstant CTLCOLOR_EDIT	1)
(defconstant CTLCOLOR_LISTBOX	2)
(defconstant CTLCOLOR_BTN	3)
(defconstant CTLCOLOR_DLG	4)
(defconstant CTLCOLOR_SCROLLBAR	5)
(defconstant CTLCOLOR_STATIC	6)
(defconstant CTLCOLOR_MAX	8)	#| three bits max |#

(defconstant COLOR_SCROLLBAR		0)
(defconstant COLOR_BACKGROUND		1)
(defconstant COLOR_ACTIVECAPTION	2)
(defconstant COLOR_INACTIVECAPTION	3)
(defconstant COLOR_MENU			4)
(defconstant COLOR_WINDOW		5)
(defconstant COLOR_WINDOWFRAME		6)
(defconstant COLOR_MENUTEXT		7)
(defconstant COLOR_WINDOWTEXT		8)
(defconstant COLOR_CAPTIONTEXT		9)
(defconstant COLOR_ACTIVEBORDER		10)
(defconstant COLOR_INACTIVEBORDER	11)
(defconstant COLOR_APPWORKSPACE		12)

(defconstant CP_GETBEEP		1)
(defconstant CP_SETBEEP		2)
(defconstant CP_GETMOUSE	3)
(defconstant CP_SETMOUSE	4)
(defconstant CP_GETBORDER	5)
(defconstant CP_SETBORDER	6)
(defconstant CP_TIMEOUTS	7)
(defconstant CP_KANJIMENU	8)

#| Flags for GetTempFileName |#
(defconstant TF_FORCEDRIVE	#x80)	#| Forces use of current dir of |#
					#| passed drive |#
#| Flags for OpenFile |#
(defconstant OF_REOPEN	#x8000)
(defconstant OF_EXIST	#x4000)
(defconstant OF_PROMPT	#x2000)
(defconstant OF_CREATE	#x1000)
(defconstant OF_CANCEL	#x0800)
(defconstant OF_VERIFY	#x0400)
(defconstant OF_DELETE	#x0200)
(defconstant OF_PARSE	#x0100)

(defconstant OF_READ		0)
(defconstant OF_WRITE		1)
(defconstant OF_READWRITE	2)

#| Interface to global memory manager |#
(defconstant GMEM_FIXED		#x0000)
(defconstant GMEM_MOVEABLE	#x0002)
(defconstant GMEM_NOCOMPACT	#x0010)
(defconstant GMEM_NODISCARD	#x0020)
(defconstant GMEM_ZEROINIT	#x0040)
(defconstant GMEM_MODIFY	#x0080)
(defconstant GMEM_DISCARDABLE	#x0F00)
(defconstant GHND		(logior GMEM_MOVEABLE GMEM_ZEROINIT))
(defconstant GPTR		(logior GMEM_FIXED GMEM_ZEROINIT))
(defconstant GMEM_SHARE		#x2000)
(defconstant GMEM_DDESHARE	#x2000)
(defconstant GMEM_NOT_BANKED	#x1000)
(defconstant GMEM_NOTIFY	#x4000)
(defconstant GMEM_LOWER		GMEM_NOT_BANKED)

#| Flags returned by GlobalFlags (in addition to GMEM_DISCARDABLE) |#
(defconstant GMEM_DISCARDED	#x4000)
(defconstant GMEM_SWAPPED	#x8000)
(defconstant GMEM_LOCKCOUNT	#x00FF)

#| Interface to local memory manager |#

(defconstant LMEM_FIXED		#x0000)
(defconstant LMEM_MOVEABLE	#x0002)
(defconstant LMEM_NOCOMPACT	#x0010)
(defconstant LMEM_NODISCARD	#x0020)
(defconstant LMEM_ZEROINIT	#x0040)
(defconstant LMEM_MODIFY	#x0080)
(defconstant LMEM_DISCARDABLE	#x0F00)
(defconstant LHND		(logior LMEM_MOVEABLE LMEM_ZEROINIT))
(defconstant LPTR		(logior LMEM_FIXED LMEM_ZEROINIT))
(defconstant NONZEROLHND	LMEM_MOVEABLE)
(defconstant NONZEROLPTR	LMEM_FIXED)

(defconstant LNOTIFY_OUTOFMEM 0)
(defconstant LNOTIFY_MOVE	1)
(defconstant LNOTIFY_DISCARD	2)

#| Flags returned by LocalFlags (in addition to LMEM_DISCARDABLE) |#
(defconstant LMEM_DISCARDED	#x4000)
(defconstant LMEM_LOCKCOUNT	#x00FF)

#| Predefined resource types |#
(defconstant RT_CURSOR		1)
(defconstant RT_BITMAP	 	2)
(defconstant RT_ICON		3)
(defconstant RT_MENU	 	4)
(defconstant RT_DIALOG	 	5)
(defconstant RT_STRING	 	6)
(defconstant RT_FONTDIR	 	7)
(defconstant RT_FONT		8)
(defconstant RT_ACCELERATOR	9)
(defconstant RT_RCDATA		10)

#| GetWindow() and constants |#
(defconstant GW_HWNDFIRST	0)
(defconstant GW_HWNDLAST	1)
(defconstant GW_HWNDNEXT	2)
(defconstant GW_HWNDPREV	3)
(defconstant GW_OWNER		4)
(defconstant GW_CHILD		5)

#| Menu flags for Add/Check/EnableMenuItem |#
(defconstant MF_CHANGE		#x0080)
(defconstant MF_INSERT		#x0000)
(defconstant MF_APPEND		#x0100)
(defconstant MF_DELETE		#x0200)
(defconstant MF_BYPOSITION	#x0400)
(defconstant MF_SEPARATOR	#x0800)
(defconstant MF_REMOVE		#x1000)
(defconstant MF_BYCOMMAND	#x0000)
(defconstant MF_GRAYED		#x0001)
(defconstant MF_DISABLED	#x0002)
(defconstant MF_ENABLED		#x0000)
(defconstant MF_CHECKED		#x0008)
(defconstant MF_UNCHECKED	#x0000)
(defconstant MF_BITMAP		#x0004)
(defconstant MF_STRING		#x0000)
(defconstant MF_POPUP		#x0010)
(defconstant MF_MENUBARBREAK 	#x0020)
(defconstant MF_MENUBREAK	#x0040)
(defconstant MF_HILITE		#x0080)
(defconstant MF_UNHILITE	#x0000)
(defconstant MF_HELP		#x4000)
(defconstant MF_SYSMENU		#x2000)
(defconstant MF_MOUSESELECT	#x8000)

#| System Menu Command Values |#
(defconstant SC_SIZE		#xF000)
(defconstant SC_MOVE		#xF010)
(defconstant SC_MINIMIZE	#xF020)
(defconstant SC_MAXIMIZE	#xF030)
(defconstant SC_NEXTWINDOW	#xF040)
(defconstant SC_PREVWINDOW	#xF050)
(defconstant SC_CLOSE		#xF060)
(defconstant SC_VSCROLL		#xF070)
(defconstant SC_HSCROLL		#xF080)
(defconstant SC_MOUSEMENU	#xF090)
(defconstant SC_KEYMENU		#xF100)
(defconstant SC_ARRANGE		#xF110)
(defconstant SC_RESTORE		#xF120)
(defconstant SC_ICON		SC_MINIMIZE)
(defconstant SC_ZOOM		SC_MAXIMIZE)
||#

#| Standard cursor IDs |#
(defconstant IDC_ARROW		32512)
(defconstant IDC_IBEAM		32513)
(defconstant IDC_WAIT		32514)
(defconstant IDC_CROSS		32515)
(defconstant IDC_UPARROW	32516)
(defconstant IDC_SIZE		32640)
(defconstant IDC_ICON		32641)
(defconstant IDC_SIZENWSE	32642)
(defconstant IDC_SIZENESW	32643)
(defconstant IDC_SIZEWE		32644)
(defconstant IDC_SIZENS		32645)

#||
#| Standard icon IDs |#
(defconstant IDI_APPLICATION	32512)
(defconstant IDI_HAND		32513)
(defconstant IDI_QUESTION	32514)
(defconstant IDI_EXCLAMATION	32515)
(defconstant IDI_ASTERISK	32516)

(defconstant CP_HWND	0)
(defconstant CP_OPEN	1)
(defconstant CP_DIRECT	2)

#| VK from the keyboard driver |#
(defconstant VK_KANA		#x15)
(defconstant VK_ROMAJI		#x16)
(defconstant VK_ZENKAKU		#x17)
(defconstant VK_HIRAGANA	#x18)
(defconstant VK_KANJI		#x19)

#| VK to send to Applications |#
(defconstant VK_CONVERT		#x1C)
(defconstant VK_NONCONVERT	#x1D)
(defconstant VK_ACCEPT		#x1E)
(defconstant VK_MODECHANGE	#x1F)

#| Conversion function numbers |#
(defconstant KNJ_START		#x01)
(defconstant KNJ_END		#x02)
(defconstant KNJ_QUERY		#x03)

(defconstant KNJ_LEARN_MODE	#x10)
(defconstant KNJ_GETMODE	#x11)
(defconstant KNJ_SETMODE	#x12)

(defconstant KNJ_CODECONVERT	#x20)
(defconstant KNJ_CONVERT	#x21)
(defconstant KNJ_NEXT		#x22)
(defconstant KNJ_PREVIOUS	#x23)
(defconstant KNJ_ACCEPT		#x24)

(defconstant KNJ_LEARN		#x30)
(defconstant KNJ_REGISTER	#x31)
(defconstant KNJ_REMOVE		#x32)
(defconstant KNJ_CHANGE_UDIC	#x33)

#| note: DEFAULT= 0
	JIS1	= 1
	JIS2	= 2
	SJIS2	= 3
	JIS1KATAKANA	= 4
	SJIS2HIRAGANA	= 5
	SJIS2KATAKANA	= 6
	OEM	= F
|#

(defconstant KNJ_JIS1toJIS1KATAKANA	#x14)
(defconstant KNJ_JIS1toSJIS2		#x13)
(defconstant KNJ_JIS1toSJIS2HIRAGANA	#x15)
(defconstant KNJ_JIS1toSJIS2KATAKANA	#x16)
(defconstant KNJ_JIS1toDEFAULT		#x10)
(defconstant KNJ_JIS1toSJIS2OEM		#x1F)
(defconstant KNJ_JIS2toSJIS2		#x23)
(defconstant KNJ_SJIS2toJIS2		#x32)

#| see KNJ_GETMODE for definition |#
(defconstant KNJ_MD_ALPHA	#x01)
(defconstant KNJ_MD_HIRAGANA	#x02)
(defconstant KNJ_MD_HALF	#x04)
(defconstant KNJ_MD_JIS		#x08)
(defconstant KNJ_MD_SPECIAL	#x10)

#| conversion modes, low word of lParam when VK_CONVERT is sent to the app |#
(defconstant KNJ_CVT_NEXT	#x01)
(defconstant KNJ_CVT_PREV	#x02)
(defconstant KNJ_CVT_KATAKANA	#x03)
(defconstant KNJ_CVT_HIRAGANA	#x04)
(defconstant KNJ_CVT_JIS1	#x05)
(defconstant KNJ_CVT_SJIS2	#x06)
(defconstant KNJ_CVT_DEFAULT	#x07)
(defconstant KNJ_CVT_TYPED	#x08)

#| Conventional dialog box and message box command IDs |#
(defconstant IDOK	1)
(defconstant IDCANCEL	2)
(defconstant IDABORT	3)
(defconstant IDRETRY	4)
(defconstant IDIGNORE	5)
(defconstant IDYES	6)
(defconstant IDNO	7)

#| Control manager structures & definitions |#
#| Edit control class stuff |#

#| styles |#
(defconstant ES_LEFT		0)
(defconstant ES_CENTER		1)
(defconstant ES_RIGHT		2)
(defconstant ES_MULTILINE	4)
(defconstant ES_AUTOVSCROLL	64)
(defconstant ES_AUTOHSCROLL	128)
(defconstant ES_NOHIDESEL	256)

#| notification codes |#
(defconstant EN_SETFOCUS	#x0100)
(defconstant EN_KILLFOCUS	#x0200)
(defconstant EN_CHANGE		#x0300)
(defconstant EN_UPDATE		#x0400)
(defconstant EN_ERRSPACE	#x0500)
(defconstant EN_HSCROLL		#x0601)
(defconstant EN_VSCROLL		#x0602)

#| control messages: |#
#| +++ Fix these!
(defconstant EM_GETSEL		(WM_USER+0)
(defconstant EM_SETSEL		(WM_USER+1)
(defconstant EM_GETRECT		(WM_USER+2)
(defconstant EM_SETRECT		(WM_USER+3)
(defconstant EM_SETRECTNP	(WM_USER+4)
(defconstant EM_SCROLL		(WM_USER+5)
(defconstant EM_LINESCROLL	(WM_USER+6)
(defconstant EM_GETMODIFY	(WM_USER+8)
(defconstant EM_SETMODIFY	(WM_USER+9)
(defconstant EM_GETLINECOUNT	(WM_USER+10)
(defconstant EM_LINEINDEX	(WM_USER+11)
(defconstant EM_SETHANDLE	(WM_USER+12)
(defconstant EM_GETHANDLE	(WM_USER+13)
(defconstant EM_GETTHUMB	(WM_USER+14)
(defconstant EM_LINELENGTH	(WM_USER+17)
(defconstant EM_REPLACESEL	(WM_USER+18)
(defconstant EM_SETFONT		(WM_USER+19)
(defconstant EM_GETLINE		(WM_USER+20)
(defconstant EM_LIMITTEXT	(WM_USER+21)
(defconstant EM_CANUNDO		(WM_USER+22)
(defconstant EM_UNDO		(WM_USER+23)
(defconstant EM_FMTLINES	(WM_USER+24)
(defconstant EM_LINEFROMCHAR	(WM_USER+25)
(defconstant EM_SETWORDBREAK	(WM_USER+26)
|#

#| button control styles |#
(defconstant BS_PUSHBUTTON	0)
(defconstant BS_DEFPUSHBUTTON	1)
(defconstant BS_CHECKBOX	2)
(defconstant BS_AUTOCHECKBOX	3)
(defconstant BS_RADIOBUTTON	4)
(defconstant BS_3STATE		5)
(defconstant BS_AUTO3STATE	6)
(defconstant BS_GROUPBOX	7)
(defconstant BS_USERBUTTON	8)
(defconstant BS_AUTORADIOBUTTON 9)
(defconstant BS_PUSHBOX		10)
(defconstant BS_LEFTTEXT	#x20)

#| user button notification codes |#
(defconstant BN_CLICKED		0)
(defconstant BN_PAINT		1)
(defconstant BN_HILITE		2)
(defconstant BN_UNHILITE	3)
(defconstant BN_DISABLE		4)
(defconstant BN_DOUBLECLICKED	5)

#| control messages |#
#| +++ Fix these!
(defconstant BM_GETCHECK	WM_USER+0)
(defconstant BM_SETCHECK	WM_USER+1)
(defconstant BM_GETSTATE	WM_USER+2)
(defconstant BM_SETSTATE	WM_USER+3)
(defconstant BM_SETSTYLE	WM_USER+4)
|#

#| Static control constants |#

(defconstant SS_LEFT		0)
(defconstant SS_CENTER		1)
(defconstant SS_RIGHT		2)
(defconstant SS_ICON		3)
(defconstant SS_BLACKRECT	4)
(defconstant SS_GRAYRECT	5)
(defconstant SS_WHITERECT	6)
(defconstant SS_BLACKFRAME	7)
(defconstant SS_GRAYFRAME	8)
(defconstant SS_WHITEFRAME	9)
(defconstant SS_USERITEM	10)
(defconstant SS_SIMPLE		11)
(defconstant SS_NOPREFIX	128)	#| #x80 - don't do "&" character translation |#

#| Dialog style bits |#
(defconstant DS_ABSALIGN	#x000000001)
(defconstant DS_SYSMODAL	#x000000002)
(defconstant DS_LOCALEDIT	#x000000020)	#| Edit items get Local storage. |#

#| +++ Fix these!
(defconstant DM_GETDEFID (WM_USER+0)
(defconstant DM_SETDEFID (WM_USER+1)
|#
(defconstant DC_HASDEFID #x534B)

#| Dialog codes (returned by WM_GETDLGCODE message): |#

(defconstant DLGC_WANTARROWS	#x01)	#| control wants arrow keys |#
(defconstant DLGC_WANTTAB	#x02)	#| control wants tab keys |#
(defconstant DLGC_WANTALLKEYS	#x04)	#| control wants all keys |#
(defconstant DLGC_WANTMESSAGE	#x04)	#| pass message to control |#
(defconstant DLGC_HASSETSEL	#x08)	#| understands EM_SETSEL message |#
(defconstant DLGC_DEFPUSHBUTTON	#x10)	#| Default pushbutton |#
(defconstant DLGC_UNDEFPUSHBUTTON #x20)	#| Non-default pushbutton |#
(defconstant DLGC_RADIOBUTTON	#x40)	#| radio button |#
(defconstant DLGC_WANTCHARS	#x80)	#| Want WM_CHAR messages |#
(defconstant DLGC_STATIC	#x100)	#| Static item: don't include |#
(defconstant DLGC_BUTTON	#x2000)	#| Button item: can be checked |#

(defconstant LB_CTLCODE	0)

#| Listbox control return values |#
(defconstant LB_OKAY		0)
(defconstant LB_ERR		-1)
(defconstant LB_ERRSPACE	-2)

#| listbox notification codes |#
(defconstant LBN_ERRSPACE	-2)
(defconstant LBN_SELCHANGE	1)
(defconstant LBN_DBLCLK		2)

#| listbox messages |#
#| +++ Fix these!
(defconstant LB_ADDSTRING	(1+WM_USER)
(defconstant LB_INSERTSTRING	(2+WM_USER)
(defconstant LB_DELETESTRING	(3+WM_USER)
(defconstant LB_RESETCONTENT	(5+WM_USER)
(defconstant LB_SETSEL		(6+WM_USER)
(defconstant LB_SETCURSEL	(7+WM_USER)
(defconstant LB_GETSEL		(8+WM_USER)
(defconstant LB_GETCURSEL	(9+WM_USER)
(defconstant LB_GETTEXT		(10+WM_USER)
(defconstant LB_GETTEXTLEN	(11+WM_USER)
(defconstant LB_GETCOUNT	(12+WM_USER)
(defconstant LB_SELECTSTRING	(13+WM_USER)
(defconstant LB_DIR		(14+WM_USER)
(defconstant LB_GETTOPINDEX	(15+WM_USER)
(defconstant LB_MSGMAX		(16+WM_USER)
|#

#| listbox style bits |#
(defconstant LBS_NOTIFY		#x0001)
(defconstant LBS_SORT		#x0002)
(defconstant LBS_NOREDRAW	#x0004)
(defconstant LBS_MULTIPLESEL	#x0008)
(defconstant LBS_STANDARD	(logior LBS_NOTIFY LBS_SORT
					WS_VSCROLL WS_BORDER))

#| scroll bar styles |#
(defconstant SBS_HORZ			#x0000)
(defconstant SBS_VERT			#x0001)
(defconstant SBS_TOPALIGN		#x0002)
(defconstant SBS_LEFTALIGN		#x0002)
(defconstant SBS_BOTTOMALIGN		#x0004)
(defconstant SBS_RIGHTALIGN		#x0004)
(defconstant SBS_SIZEBOXTOPLEFTALIGN	#x0002)
(defconstant SBS_SIZEBOXBOTTOMRIGHTALIGN #x0004)
(defconstant SBS_SIZEBOX		#x0008)

#| constants used to specify return condition for WaitSoundState |#

(defconstant S_QUEUEEMPTY	0)
(defconstant S_THRESHOLD	1)
(defconstant S_ALLTHRESHOLD	2)

#| constants used to specify accent mode |#

(defconstant S_NORMAL	0)
(defconstant S_LEGATO	1)
(defconstant S_STACCATO	2)

#| constants used to specify source in SetSoundNoise |#
(defconstant S_PERIOD512	0)	#| freq = N/512 high pitch, less coarse hiss |#
(defconstant S_PERIOD1024	1)	#| freq = N/1024 |#
(defconstant S_PERIOD2048	2)	#| freq = N/2048 low pitch, more coarse hiss |#
(defconstant S_PERIODVOICE	3)	#| source is frequency from voice channel (3) |#

(defconstant S_WHITE512		4)	#| freq = N/512 high pitch, less coarse hiss |#
(defconstant S_WHITE1024	5)	#| freq = N/1024 |#
(defconstant S_WHITE2048	6)	#| freq = N/2048 low pitch, more coarse hiss |#
(defconstant S_WHITEVOICE	7)	#| source is frequency from voice channel (3) |#

(defconstant S_SERDVNA	-1)	#| device not available |#
(defconstant S_SEROFM	-2)	#| out of memory |#
(defconstant S_SERMACT	-3)	#| music active |#
(defconstant S_SERQFUL	-4)	#| queue full |#
(defconstant S_SERBDNT	-5)	#| invalid note |#
(defconstant S_SERDLN	-6)	#| invalid note length |#
(defconstant S_SERDCC	-7)	#| invalid note count |#
(defconstant S_SERDTP	-8)	#| invalid tempo |#
(defconstant S_SERDVL	-9)	#| invalid volume |#
(defconstant S_SERDMD	-10)	#| invalid mode |#
(defconstant S_SERDSH	-11)	#| invalid shape |#
(defconstant S_SERDPT	-12)	#| invalid pitch |#
(defconstant S_SERDFQ	-13)	#| invalid frequency |#
(defconstant S_SERDDR	-14)	#| invalid duration |#
(defconstant S_SERDSR	-15)	#| invalid source |#
(defconstant S_SERDST	-16)	#| invalid state |#


#|
**
** dcb field definitions.
**
|#

(defconstant NOPARITY	0)
(defconstant ODDPARITY	1)
(defconstant EVENPARITY	2)
(defconstant MARKPARITY	3)
(defconstant SPACEPARITY 4)

(defconstant ONESTOPBIT		0)
(defconstant ONE5STOPBITS	1)
(defconstant TWOSTOPBITS	2)

(defconstant IGNORE	0)	#| Ignore signal |#
(defconstant INFINITE	#xFFFF)	#| Infinite timeout |#



#|
**
** Comm Device Driver Error Bits.
**
|#

(defconstant CE_RXOVER		#x0001)	#| Receive Queue overflow |#
(defconstant CE_OVERRUN		#x0002)	#| Receive Overrun Error |#
(defconstant CE_RXPARITY	#x0004)	#| Receive Parity Error |#
(defconstant CE_FRAME		#x0008)	#| Receive Framing error |#
(defconstant CE_BREAK		#x0010)	#| Break Detected |#
(defconstant CE_CTSTO		#x0020)	#| CTS Timeout |#
(defconstant CE_DSRTO		#x0040)	#| DSR Timeout |#
(defconstant CE_RLSDTO		#x0080)	#| RLSD Timeout |#
(defconstant CE_TXFULL		#x0100)	#| TX Queue is full |#
(defconstant CE_PTO		#x0200)	#| LPTx Timeout |#
(defconstant CE_IOE		#x0400)	#| LPTx I/O Error |#
(defconstant CE_DNS		#x0800)	#| LPTx Device not selected |#
(defconstant CE_OOP		#x1000)	#| LPTx Out-Of-Paper |#
(defconstant CE_MODE		#x8000)	#| Requested mode unsupported |#


#|************************************************************************
**
** Initialization Error Codes
**
************************************************************************|#

(defconstant IE_BADID		-1)	#| Invalid or unsupported id |#
(defconstant IE_OPEN		-2)	#| Device Already Open |#
(defconstant IE_NOPEN		-3)	#| Device Not Open |#
(defconstant IE_MEMORY		-4)	#| Unable to allocate queues |#
(defconstant IE_DEFAULT		-5)	#| Error in default parameters |#
(defconstant IE_HARDWARE	-10)	#| Hardware Not Present |#
(defconstant IE_BYTESIZE	-11)	#| Illegal Byte Size |#
(defconstant IE_BAUDRATE	-12)	#| Unsupported BaudRate |#


#|************************************************************************
**
** Event Definitions
**
************************************************************************|#

(defconstant EV_RXCHAR	#x0001)	#| Any Character received |#
(defconstant EV_RXFLAG	#x0002)	#| Received certain character |#
(defconstant EV_TXEMPTY	#x0004)	#| Transmitt Queue Empty |#
(defconstant EV_CTS	#x0008)	#| CTS changed state |#
(defconstant EV_DSR	#x0010)	#| DSR changed state |#
(defconstant EV_RLSD	#x0020)	#| RLSD changed state |#
(defconstant EV_BREAK	#x0040)	#| BREAK received |#
(defconstant EV_ERR	#x0080)	#| Line status error occurred |#
(defconstant EV_RING	#x0100)	#| Ring signal detected |#
(defconstant EV_PERR	#x0200)	#| Printer error occured |#


#|************************************************************************
**
** Escape Functions
**
************************************************************************|#

(defconstant SETXOFF	1)	#| Simulate XOFF received |#
(defconstant SETXON	2)	#| Simulate XON received |#
(defconstant SETRTS	3)	#| Set RTS high |#
(defconstant CLRRTS	4)	#| Set RTS low |#
(defconstant SETDTR	5)	#| Set DTR high |#
(defconstant CLRDTR	6)	#| Set DTR low |#
(defconstant RESETDEV	7)	#| Reset device if possible |#
||#
