(in-package :user)

;; All the foreign function stuff from Windoz.


#+(version>= 5 0)
(eval-when (compile load eval)
  (require :ffcompat)
  (require :cltl1)
  (require :aclwffi)			; for ctypes (CT) package
  (require :for)			; for FOR macro
  ;;(require :climg)
  (setq excl::*enable-package-locked-errors* nil)
  )

(defpackage :windows
  (:nicknames  #:win)
  (:use #:lisp #:excl #:foreign-functions #:ct)
  (:shadow #:defcstruct  ;; must remove the need for this
	   #:ignore)
  (:export #:cset
	   #:csets
	   
	   #:wm_user
	   #:wm_getfont
	   #:wm_setfont
	   #:wm_gettext
	   #:wm_settext
	   #:bm_getcheck
	   #:bm_setcheck
	   #:bm_getstate
	   #:bm_setstate
	   #:bm_setstyle
	   #:bn_clicked
	   #:bn_doubleclicked
	   #:em_limittext
	   #:em_getsel
	   #:em_setsel
	   #:wm_gettextlength
	   #:ws_border
	   #:ws_vscroll
	   #:ws_popup
	   #:ws_child
	   #:ws_visible
	   #:ws_caption
	   #:transparent
		       )
  )			;referred to by pc-defs

(in-package :windows)

(eval-when (compile load eval)
  (export '(
	    ABORTDOC
	    ABSOLUTE
	    ACLStartupFetch
	    ACLStartupQuery
	    ACLStartupShow
	    ALTERNATE
	    ANSI_CHARSET
	    ANSI_FIXED_FONT
	    ANSI_VAR_FONT
	    APPCLASS_MASK
	    APPCLASS_STANDARD
	    APPCMD_CLIENTONLY
	    APPCMD_FILTERINITS
	    APPCMD_MASK
	    ASPECTX
	    ASPECTXY
	    ASPECTY
	    ASPECT_FILTERING
	    AbortDoc
	    AddAtom
	    AddFontResource
	    AdjustWindowRect
	    AdjustWindowRectEx
	    AnimatePalette
	    AnyPopup
	    AppendMenu
	    Arc
	    ArrangeIconicWindows
	    BANDINFO
	    BEGIN_PATH
	    BITSPIXEL
	    BI_RGB
	    BI_RLE4
	    BI_RLE8
	    BLACKNESS
	    BLACKONWHITE
	    BLACK_BRUSH
	    BLACK_PEN
	    BM_GETCHECK
	    BM_GETSTATE
	    BM_SETCHECK
	    BM_SETSTATE
	    BM_SETSTYLE
	    BN_CLICKED
	    BN_DISABLE
	    BN_DOUBLECLICKED
	    BN_HILITE
	    BN_PAINT
	    BN_UNHILITE
	    BS_3STATE
	    BS_AUTO3STATE
	    BS_AUTOCHECKBOX
	    BS_AUTORADIOBUTTON
	    BS_CHECKBOX
	    BS_DEFPUSHBUTTON
	    BS_DIBPATTERN
	    BS_GROUPBOX
	    BS_HATCHED
	    BS_HOLLOW
	    BS_INDEXED
	    BS_LEFTTEXT
	    BS_NULL
	    BS_OWNERDRAW
	    BS_PATTERN
	    BS_PUSHBUTTON
	    BS_RADIOBUTTON
	    BS_SOLID
	    BS_USERBUTTON
	    BeginDeferWindowPos
	    BeginPaint
	    BitBlt
	    BringWindowToTop
	    BuildCommDCB
	    CADV_LATEACK
	    CBF_FAIL_ADVISES
	    CBF_FAIL_ALLSVRXACTIONS
	    CBF_FAIL_CONNECTIONS
	    CBF_FAIL_EXECUTES
	    CBF_FAIL_POKES
	    CBF_FAIL_REQUESTS
	    CBF_FAIL_SELFCONNECTIONS
	    CBF_SKIP_ALLNOTIFICATIONS
	    CBF_SKIP_CONNECT_CONFIRMS
	    CBF_SKIP_DISCONNECTS
	    CBF_SKIP_REGISTRATIONS
	    CBF_SKIP_UNREGISTRATIONS
	    CBM_INIT
	    CBN_DBLCLK
	    CBN_DROPDOWN
	    CBN_EDITCHANGE
	    CBN_EDITUPDATE
	    CBN_ERRSPACE
	    CBN_KILLFOCUS
	    CBN_SELCHANGE
	    CBN_SETFOCUS
	    CBN_CLOSEUP
	    CBR_BLOCK
	    CBS_AUTOHSCROLL
	    CBS_DROPDOWN
	    CBS_DROPDOWNLIST
	    CBS_HASSTRINGS
	    CBS_NOINTEGRALHEIGHT
	    CBS_OEMCONVERT
	    CBS_OWNERDRAWFIXED
	    CBS_OWNERDRAWVARIABLE
	    CBS_SIMPLE
	    CBS_SORT
	    CB_ADDSTRING
	    CB_DELETESTRING
	    CB_DIR
	    CB_ERR
	    CB_ERRSPACE
	    CB_FINDSTRING
	    CB_GETCOUNT
	    CB_GETCURSEL
	    CB_GETDROPPEDCONTROLRECT
	    CB_GETEDITSEL
	    CB_GETITEMDATA
	    CB_GETLBTEXT
	    CB_GETLBTEXTLEN
	    CB_INSERTSTRING
	    CB_LIMITTEXT
	    CB_OKAY
	    CB_RESETCONTENT
	    CB_SELECTSTRING
	    CB_SETCURSEL
	    CB_SETEDITSEL
	    CB_SETITEMDATA
	    CB_SHOWDROPDOWN
	    CCHDEVICENAME
	    CCS_ADJUSTABLE
	    CCS_BOTTOM
	    CCS_NODIVIDER
	    CCS_NOHILITE
	    CCS_NOMOVEY
	    CCS_NOPARENTALIGN
	    CCS_NORESIZE
	    CCS_TOP
	    CC_CHORD
	    CC_CIRCLES
	    CC_ELLIPSES
	    CC_INTERIORS
	    CC_NONE
	    CC_PIE
	    CC_STYLED
	    CC_WIDE
	    CC_WIDESTYLED
	    CE_BREAK
	    CE_DNS
	    CE_FRAME
	    CE_IOE
	    CE_MODE
	    CE_OOP
	    CE_OVERRUN
	    CE_PTO
	    CE_RXOVER
	    CE_RXPARITY
	    CE_TXFULL
	    CF_BITMAP
	    CF_DIB
	    CF_DIF
	    CF_DSPBITMAP
	    CF_DSPMETAFILEPICT
	    CF_DSPTEXT)
	  'windows))

(eval-when (compile load eval)
  (export '(
	    CF_GDIOBJFIRST
	    CF_GDIOBJLAST
	    CF_METAFILEPICT
	    CF_OEMTEXT
	    CF_OWNERDISPLAY
	    CF_PALETTE
	    CF_PRIVATEFIRST
	    CF_PRIVATELAST
	    CF_SYLK
	    CF_TEXT
	    CF_TIFF
	    CLIPCAPS
	    CLIP_CHARACTER_PRECIS
	    CLIP_DEFAULT_PRECIS
	    CLIP_STROKE_PRECIS
	    CLIP_TO_PATH
	    CLRDTR
	    CLRRTS
	    CLR_DEFAULT
	    CLR_HILIGHT
	    CLR_NONE
	    COLORONCOLOR
	    COLORRES
	    COLOR_ACTIVEBORDER
	    COLOR_ACTIVECAPTION
	    COLOR_APPWORKSPACE
	    COLOR_BACKGROUND
	    COLOR_BTNFACE
	    COLOR_BTNHIGHLIGHT
	    COLOR_BTNSHADOW
	    COLOR_BTNTEXT
	    COLOR_CAPTIONTEXT
	    COLOR_ENDCOLORS
	    COLOR_GRAYTEXT
	    COLOR_HIGHLIGHT
	    COLOR_HIGHLIGHTTEXT
	    COLOR_INACTIVEBORDER
	    COLOR_INACTIVECAPTION
	    COLOR_INACTIVECAPTIONTEXT
	    COLOR_MENU
	    COLOR_MENUTEXT
	    COLOR_SCROLLBAR
	    COLOR_WINDOW
	    COLOR_WINDOWFRAME
	    COLOR_WINDOWTEXT
	    COMPLEXREGION
	    CP_NONE
	    CP_RECTANGLE
	    CP_WINANSI
	    CP_WINUNICODE
	    CREATE_ALWAYS
	    CS_BYTEALIGNCLIENT
	    CS_BYTEALIGNWINDOW
	    CS_CLASSDC
	    CS_DBLCLKS
	    CS_GLOBALCLASS
	    CS_HREDRAW
	    CS_KEYCVTWINDOW
	    CS_NOCLOSE
	    CS_NOKEYCVT
	    CS_OWNDC
	    CS_PARENTDC
	    CS_SAVEBITS
	    CS_VREDRAW
	    CURVECAPS
	    CW_USEDEFAULT
	    CallMsgFilter
	    CallWindowProc
	    ChangeClipboardChain
	    CheckDlgButton
	    CheckMenuItem
	    CheckRadioButton
	    ChildWindowFromPoint_ptr
	    Chord
	    ClearCommBreak
	    ClientToScreen
	    ClipCursor
	    CloseClipboard
	    CloseMetaFile
	    CloseWindow
	    CombineRgn
	    CommDlgExtendedError
	    CopyMetaFile
	    CopyRect
	    CountClipboardFormats
	    CreateBitmap
	    CreateBitmapIndirect
	    CreateBrushIndirect
	    CreateCaret
	    CreateCompatibleBitmap
	    CreateCompatibleDC
	    CreateCursor
	    CreateDC
	    CreateDIBPatternBrush
	    CreateDIBitmap
	    CreateDialog
	    CreateDialogIndirect
	    CreateDialogIndirectParam
	    CreateDialogParam
	    CreateDiscardableBitmap
	    CreateEllipticRgn
	    CreateEllipticRgnIndirect
	    CreateFont
	    CreateFontIndirect
	    CreateHatchBrush
	    CreateIC
	    CreateIcon
	    CreateMenu
	    CreateMetaFile
	    CreatePalette
	    CreatePatternBrush
	    CreatePen
	    CreatePenIndirect
	    CreatePolyPolygonRgn
	    CreatePolygonRgn
	    CreatePopupMenu
	    CreateRectRgn
	    CreateRectRgnIndirect
	    CreateRoundRectRgn
	    CreateSolidBrush
	    CreateUpDownControl
	    CreateWindow
	    CreateWindowEx
	    DC_BINS
	    DC_DRIVER
	    DC_DUPLEX
	    DC_EXTRA
	    DC_FIELDS
	    DC_HASDEFID
	    DC_MAXEXTENT
	    DC_MINEXTENT
	    DC_PAPERS
	    DC_PAPERSIZE
	    DC_SIZE
	    DC_VERSION
	    DDE_FACK
	    DDE_FACKREQ
	    DDE_FACKRESERVED
	    DDE_FADVRESERVED
	    DDE_FAPPSTATUS
	    DDE_FBUSY
	    DDE_FDATRESERVED
	    DDE_FDEFERUPD
	    DDE_FNOTPROCESSED
	    DDE_FPOKRESERVED
	    DDE_FRELEASE
	    DDE_FREQUESTED
	    DDL_ARCHIVE
	    DDL_DIRECTORY
	    DDL_DRIVES
	    DDL_EXCLUSIVE
	    DDL_HIDDEN
	    DDL_POSTMSGS
	    DDL_READONLY
	    DDL_READWRITE
	    DDL_SYSTEM
	    DEFAULT_PALETTE
	    DEFAULT_PITCH
	    DEFAULT_QUALITY
	    DEVICEDATA
	    DEVICE_DEFAULT_FONT
	    DEVICE_FONTTYPE
	    DIB_PAL_COLORS
	    DIB_RGB_COLORS
	    DKGRAY_BRUSH
	    DLGC_BUTTON
	    DLGC_DEFPUSHBUTTON
	    DLGC_HASSETSEL
	    DLGC_RADIOBUTTON
	    DLGC_STATIC
	    DLGC_UNDEFPUSHBUTTON
	    DLGC_WANTALLKEYS
	    DLGC_WANTARROWS
	    DLGC_WANTCHARS
	    DLGC_WANTMESSAGE
	    DLGC_WANTTAB
	    DLGWINDOWEXTRA
	    DMBIN_AUTO
	    DMBIN_CASSETTE
	    DMBIN_ENVELOPE
	    DMBIN_ENVMANUAL
	    DMBIN_LARGECAPACITY
	    DMBIN_LARGEFMT
	    DMBIN_LAST
	    DMBIN_LOWER
	    DMBIN_MANUAL
	    DMBIN_MIDDLE
	    DMBIN_ONLYONE
	    DMBIN_SMALLFMT
	    DMBIN_TRACTOR
	    DMBIN_UPPER
	    DMBIN_USER
	    DMCOLOR_COLOR
	    DMCOLOR_MONOCHROME
	    DMDUP_HORIZONTAL
	    DMDUP_SIMPLEX
	    DMDUP_VERTICAL
	    DMLERR_ADVACKTIMEOUT
	    DMLERR_BUSY
	    DMLERR_DATAACKTIMEOUT
	    DMLERR_DLL_NOT_INITIALIZED
	    DMLERR_DLL_USAGE
	    DMLERR_EXECACKTIMEOUT
	    DMLERR_FIRST
	    DMLERR_INVALIDPARAMETER
	    DMLERR_LAST
	    DMLERR_LOW_MEMORY
	    DMLERR_MEMORY_ERROR
	    DMLERR_NOTPROCESSED
	    DMLERR_NO_CONV_ESTABLISHED
	    DMLERR_NO_ERROR
	    DMLERR_POKEACKTIMEOUT
	    DMLERR_POSTMSG_FAILED
	    DMLERR_REENTRANCY
	    DMLERR_SERVER_DIED
	    DMLERR_SYS_ERROR
	    DMLERR_UNADVACKTIMEOUT
	    DMLERR_UNFOUND_QUEUE_ID
	    DMORIENT_LANDSCAPE
	    DMORIENT_PORTRAIT)
	  'windows))

(eval-when (compile load eval)
  (export '(
	    DMPAPER_10X14
	    DMPAPER_11X17
	    DMPAPER_A3
	    DMPAPER_A4
	    DMPAPER_A4SMALL
	    DMPAPER_A5
	    DMPAPER_B4
	    DMPAPER_B5
	    DMPAPER_CSHEET
	    DMPAPER_DSHEET
	    DMPAPER_ENV_10
	    DMPAPER_ENV_11
	    DMPAPER_ENV_12
	    DMPAPER_ENV_14
	    DMPAPER_ENV_9
	    DMPAPER_ESHEET
	    DMPAPER_EXECUTIVE
	    DMPAPER_FOLIO
	    DMPAPER_LAST
	    DMPAPER_LEDGER
	    DMPAPER_LEGAL
	    DMPAPER_LETTER
	    DMPAPER_LETTERSMALL
	    DMPAPER_NOTE
	    DMPAPER_QUARTO
	    DMPAPER_STATEMENT
	    DMPAPER_TABLOID
	    DMPAPER_USER
	    DMRES_DRAFT
	    DMRES_HIGH
	    DMRES_LOW
	    DMRES_MEDIUM
	    DM_COLOR
	    DM_COPIES
	    DM_COPY
	    DM_DEFAULTSOURCE
	    DM_DUPLEX
	    DM_GETDEFID
	    DM_IN_BUFFER
	    DM_IN_PROMPT
	    DM_MODIFY
	    DM_ORIENTATION
	    DM_OUT_BUFFER
	    DM_OUT_DEFAULT
	    DM_PAPERLENGTH
	    DM_PAPERSIZE
	    DM_PAPERWIDTH
	    DM_PRINTQUALITY
	    DM_PROMPT
	    DM_SCALE
	    DM_SETDEFID
	    DM_SPECVERSION
	    DM_UPDATE
	    DNS_FILTEROFF
	    DNS_FILTERON
	    DNS_REGISTER
	    DNS_UNREGISTER
	    DPtoLP
	    DRAFTMODE
	    DRAFT_QUALITY
	    DRAWPATTERNRECT
	    DRIVERVERSION
	    DRIVE_FIXED
	    DRIVE_REMOTE
	    DRIVE_REMOVABLE
	    DSTINVERT
	    DS_ABSALIGN
	    DS_LOCALEDIT
	    DS_MODALFRAME
	    DS_NOIDLEMSG
	    DS_SETFOREGROUND
	    DS_3DLOOK
	    DS_SETFONT
	    DS_SYSMODAL
	    DT_BOTTOM
	    DT_CALCRECT
	    DT_CENTER
	    DT_CHARSTREAM
	    DT_DISPFILE
	    DT_EXPANDTABS
	    DT_EXTERNALLEADING
	    DT_INTERNAL
	    DT_LEFT
	    DT_METAFILE
	    DT_NOCLIP
	    DT_NOPREFIX
	    DT_PLOTTER
	    DT_RASCAMERA
	    DT_RASDISPLAY
	    DT_RASPRINTER
	    DT_RIGHT
	    DT_SINGLELINE
	    DT_TABSTOP
	    DT_TOP
	    DT_VCENTER
	    DT_WORDBREAK
	    DdeAbandonTransaction
	    DdeAccessData
	    DdeAddData
	    DdeClientTransaction
	    DdeCmpStringHandles
	    DdeConnect
	    DdeConnectList
	    DdeCreateDataHandle
	    DdeCreateStringHandle
	    DdeDisconnect
	    DdeDisconnectList
	    DdeEnableCallback
	    DdeFreeDataHandle
	    DdeFreeStringHandle
	    DdeGetData
	    DdeGetLastError
	    DdeImpersonateClient
	    DdeInitialize
	    DdeKeepStringHandle
	    DdeNameService
	    DdePostAdvise
	    DdeQueryConvInfo
	    DdeQueryNextServer
	    DdeQueryString
	    DdeReconnect
	    DdeSetUserHandle
	    DdeUnaccessData
	    DdeUninitialize
	    DebugBreak
	    DefDlgProc
	    DefFrameProc
	    DefMDIChildProc
	    DefWindowProc
	    DeferWindowPos
	    DeleteAtom
	    DeleteDC
	    DeleteMenu
	    DeleteMetaFile
	    DeleteObject
	    DestroyCaret
	    DestroyCursor
	    DestroyIcon
	    DestroyMenu
	    DestroyWindow
	    DialogBox
	    DialogBoxIndirect
	    DialogBoxIndirectParam
	    DialogBoxParam
	    DispatchMessage
	    DlgDirList
	    DlgDirListComboBox
	    DlgDirSelectComboBoxEx
	    DlgDirSelectEx
	    DrawFocusRect
	    DrawIcon
	    DrawMenuBar
	    DrawStatusText
	    DrawText
	    EC_DISABLE
	    EC_ENABLEALL
	    EC_ENABLEONE
	    EC_QUERYWAITING
	    EM_CANUNDO
	    EM_EMPTYUNDOBUFFER
	    EM_EXGETSEL
	    EM_EXLIMITTEXT
	    EM_EXLINEFROMCHAR
	    EM_EXSETSEL
	    EM_FINDTEXTEX
	    EM_FMTLINES
	    EM_GETHANDLE
	    EM_GETLINE
	    EM_GETLINECOUNT
	    EM_GETMODIFY
	    EM_GETRECT
	    EM_GETSEL
	    EM_GETTEXTRANGE
	    EM_GETTHUMB
	    EM_LIMITTEXT
	    EM_LINEFROMCHAR
	    EM_LINEINDEX
	    EM_LINELENGTH
	    EM_LINESCROLL
	    EM_REPLACESEL
	    EM_SCROLL
	    EM_SCROLLCARET
	    EM_SETHANDLE
	    EM_SETMODIFY
	    EM_SETPASSWORDCHAR
	    EM_SETREADONLY
	    EM_SETRECT
	    EM_SETRECTNP
	    EM_SETSEL
	    EM_SETTABSTOPS
	    EM_UNDO
	    ENABLEDUPLEX
	    ENABLEPAIRKERNING
	    ENABLERELATIVEWIDTHS
	    ENDDOC
	    END_PATH
	    ENUMPAPERBINS
	    ENUMPAPERMETRICS
	    EN_CHANGE
	    EN_ERRSPACE
	    EN_HSCROLL
	    EN_KILLFOCUS
	    EN_MAXTEXT
	    EN_SETFOCUS
	    EN_UPDATE
	    EN_VSCROLL
	    EPSPRINTING
	    ES_AUTOHSCROLL
	    ES_AUTOVSCROLL
	    ES_CENTER
	    ES_LEFT
	    ES_LOWERCASE
	    ES_MULTILINE
	    ES_NOHIDESEL
	    ES_OEMCONVERT
	    ES_PASSWORD
	    ES_READONLY
	    ES_RIGHT
	    ES_UPPERCASE
	    ETO_CLIPPED
	    ETO_GRAYED
	    ETO_OPAQUE
	    EVENPARITY
	    EV_BREAK
	    EV_CTS
	    EV_DSR
	    EV_ERR
	    EV_PERR
	    EV_RING
	    EV_RLSD
	    EV_RXCHAR
	    EV_RXFLAG
	    EV_TXEMPTY
	    EXTTEXTOUT
	    EXT_DEVICE_CAPS
	    Ellipse
	    EmptyClipboard
	    EnableMenuItem
	    EnableWindow
	    EndDeferWindowPos
	    EndDialog
	    EndDoc
	    EndPage
	    EndPaint
	    EnumChildWindows
	    EnumClipboardFormats
	    EnumFonts
	    EnumMetaFile
	    EnumObjects
	    EnumProps
	    EnumThreadWindows
	    EnumWindows
	    EqualRect
	    EqualRgn
	    Escape
	    EscapeCommFunction
	    ExcludeClipRect
	    ExcludeUpdateRgn
	    ExitWindows
	    ExtFloodFill
	    ExtTextOut
	    ExtractIcon
	    FALSE
	    FF_DECORATIVE
	    FF_DONTCARE
	    FF_MODERN
	    FF_ROMAN
	    FF_SCRIPT
	    FF_SWISS
	    FILE_ATTRIBUTE_NORMAL
	    FILE_SHARE_READ
	    FILE_SHARE_WRITE
	    FIXED_PITCH
	    FLOODFILLBORDER
	    FLOODFILLSURFACE
	    FLUSHOUTPUT
	    FR_DOWN
	    FR_MATCHCASE
	    FR_WHOLEWORD
	    FW_BLACK
	    FW_BOLD
	    FW_DEMIBOLD
	    FW_DONTCARE
	    FW_EXTRABOLD
	    FW_EXTRALIGHT
	    FW_HEAVY
	    FW_LIGHT
	    FW_MEDIUM
	    FW_NORMAL
	    FW_REGULAR
	    FW_SEMIBOLD
	    FW_THIN
	    FW_ULTRABOLD
	    FW_ULTRALIGHT
	    FatalExit
	    FillRect
	    FillRgn
	    FindAtom
	    FindResource
	    FindWindow
	    FlashWindow
	    FloodFill
	    FrameRect
	    FrameRgn
	    FreeLibrary
	    GCL_MENUNAME
	    GCL_WNDPROC
	    GENERIC_READ
	    GENERIC_WRITE
	    GETCOLORTABLE
	    GETEXTENDEDTEXTMETRICS
	    GETEXTENTTABLE
	    GetOpenFileName
	    GETPAIRKERNTABLE
	    GETPENWIDTH
	    GETPHYSPAGESIZE
	    GETPRINTINGOFFSET
	    GetSaveFileName
	    GETSCALINGFACTOR
	    GETSETPAPERBINS
	    GETSETPAPERMETRICS
	    GETSETPRINTORIENT
	    GETTECHNOLGY
	    GETTECHNOLOGY
	    GETTRACKKERNTABLE
	    GETVECTORBRUSHSIZE
	    GETVECTORPENSIZE
	    GHND
	    GMEM_DDESHARE
	    GMEM_DISCARDABLE
	    GMEM_DISCARDED
	    GMEM_FIXED
	    GMEM_LOCKCOUNT
	    GMEM_LOWER
	    GMEM_MODIFY
	    GMEM_MOVEABLE
	    GMEM_NOCOMPACT
	    GMEM_NODISCARD
	    GMEM_NOTIFY
	    GMEM_NOT_BANKED
	    GMEM_SHARE
	    GMEM_ZEROINIT
	     GPTR)
    'windows))

(eval-when (compile load eval)
  (export '(
	    GRAY_BRUSH
	    GWL_EXSTYLE
	    GWL_HINSTANCE
	    GWL_HWNDPARENT
	    GWL_ID
	    GWL_STYLE
	    GWL_WNDPROC
	    GW_CHILD
	    GW_HWNDFIRST
	    GW_HWNDLAST
	    GW_HWNDNEXT
	    GW_HWNDPREV
	    GW_OWNER
	    GetActiveWindow
	    GetAspectRatioFilterEx
	    GetAsyncKeyState
	    GetAtomName
	    GetBitmapBits
	    GetBitmapDimensionEx
	    GetBkColor
	    GetBkMode
	    GetBrushOrgEx
	    GetCapture
	    GetCaretBlinkTime
	    GetCaretPos
	    GetCharWidth
	    GetClassInfo
	    GetClassLong
	    GetClassName
	    GetClassWord
	    GetClientRect
	    GetClipBox
	    GetClipboardData
	    GetClipboardFormatName
	    GetClipboardOwner
	    GetClipboardViewer
	    GetCurrentPositionEx
	    GetCurrentProcess
	    GetCurrentTime
	    GetCursorPos
	    GetDC
	    GetDCOrgEx
	    GetDIBits
	    GetDesktopWindow
	    GetDeviceCaps
	    GetDialogBaseUnits
	    GetDlgCtrlID
	    GetDlgItem
	    GetDlgItemInt
	    GetDlgItemText
	    GetDoubleClickTime
	    GetDriveType
	    GetFocus
	    GetInputState
	    GetKeyNameText
	    GetKeyState
	    GetKeyboardState
	    GetLastActivePopup
	    GetLastError
	    GetMapMode
	    GetMenu
	    GetMenuCheckMarkDimensions
	    GetMenuItemCount
	    GetMenuItemID
	    GetMenuState
	    GetMenuString
	    GetMessage
	    GetMessagePos
	    GetMessageTime
	    GetMetaFile
	    GetMetaFileBitsEx
	    GetModuleFileName
	    GetModuleHandle
	    GetNearestColor
	    GetNearestPaletteIndex
	    GetNextDlgGroupItem
	    GetNextDlgTabItem
	    GetNextWindow
	    GetObject
	    GetPaletteEntries
	    GetParent
	    GetPixel
	    GetPolyFillMode
	    GetPriorityClipboardFormat
	    GetPrivateProfileInt
	    GetPrivateProfileString
	    GetProcAddress
	    GetProfileInt
	    GetProfileString
	    GetProp
	    GetROP2
	    GetRgnBox
	    GetScrollPos
	    GetScrollRange
	    GetStockObject
	    GetStretchBltMode
	    GetSubMenu
	    GetSysColor
	    GetSystemDirectory
	    GetSystemInfo
	    GetSystemMenu
	    GetSystemMetrics
	    GetSystemPaletteEntries
	    GetSystemPaletteUse
	    GetTabbedTextExtent
	    GetTempFileName
	    GetTextAlign
	    GetTextCharacterExtra
	    GetTextColor
	    GetTextExtentPoint
	    GetTextFace
	    GetTextMetrics
	    GetTickCount
	    GetTopWindow
	    GetUpdateRect
	    GetUpdateRgn
	    GetVersion
	    GetViewportExtEx
	    GetViewportOrgEx
	    GetWinMainArgs
	    GetWindow
	    GetWindowDC
	    GetWindowExtEx
	    GetWindowLong
	    GetWindowOrgEx
	    GetWindowRect
	    GetWindowText
	    GetWindowTextLength
	    GetWindowThreadProcessId
	    GetWindowWord
	    GetWindowsDirectory
	    GlobalAddAtom
	    GlobalAlloc
	    GlobalDeleteAtom
	    GlobalFindAtom
	    GlobalFlags
	    GlobalFree
	    GlobalGetAtomName
	    GlobalHandle
	    GlobalLock
	    GlobalReAlloc
	    GlobalSize
	    GlobalUnlock
	    GrayString
	    HBT_SPRING
	    HCBT_MINMAX
	    HCBT_MOVESIZE
	    HCBT_QS
	    HC_ACTION
	    HC_GETNEXT
	    HC_NOREM
	    HC_NOREMOVE
	    HC_SKIP
	    HC_SYSMODALOFF
	    HC_SYSMODALON
	    HDATA_APPOWNED
	    HELP_COMMAND
	    HELP_CONTENTS
	    HELP_CONTEXT
	    HELP_CONTEXTPOPUP
	    HELP_FORCEFILE
	    HELP_HELPONHELP
	    HELP_INDEX
	    HELP_KEY
	    HELP_MULTIKEY
	    HELP_PARTIALKEY
	    HELP_QUIT
	    HELP_SETCONTENTS
	    HELP_SETINDEX
	    HELP_SETWINPOS
	    HIDE_WINDOW
	    HOLLOW_BRUSH
	    HORZRES
	    HORZSIZE
	    HS_BDIAGONAL
	    HS_CROSS
	    HS_DIAGCROSS
	    HS_FDIAGONAL
	    HS_HORIZONTAL
	    HS_VERTICAL
	    HTBOTTOM
	    HTBOTTOMLEFT
	    HTBOTTOMRIGHT
	    HTCAPTION
	    HTCLIENT
	    HTERROR
	    HTGROWBOX
	    HTHSCROLL
	    HTLEFT
	    HTMENU
	    HTNOWHERE
	    HTREDUCE
	    HTRIGHT
	    HTSIZE
	    HTSIZEFIRST
	    HTSIZELAST
	    HTSYSMENU
	    HTTOP
	    HTTOPLEFT
	    HTTOPRIGHT
	    HTTRANSPARENT
	    HTVSCROLL
	    HTZOOM
	    HWND_BOTTOM
	    HWND_NOTOPMOST
	    HWND_TOP
	    HWND_TOPMOST
	    HideCaret
	    HiliteMenuItem
	    IDABORT
	    IDCANCEL
	    IDC_ARROW
	    IDC_CROSS
	    IDC_IBEAM
	    IDC_ICON
	    IDC_SIZE
	    IDC_SIZENESW
	    IDC_SIZENS
	    IDC_SIZENWSE
	    IDC_SIZEWE
	    IDC_UPARROW
	    IDC_WAIT
	    IDIGNORE
	    IDI_APPLICATION
	    IDI_ASTERISK
	    IDI_EXCLAMATION
	    IDI_HAND
	    IDI_QUESTION
	    IDNO
	    IDOK
	    IDRETRY
	    IDYES
	    IE_BADID
	    IE_BAUDRATE
	    IE_BYTESIZE
	    IE_DEFAULT
	    IE_HARDWARE
	    IE_MEMORY
	    IE_NOPEN
	    IE_OPEN
	    ;;IGNORE
	    ILC_COLOR
	    ILC_COLOR16
	    ILC_COLOR24
	    ILC_COLOR32
	    ILC_COLOR4
	    ILC_COLOR8
	    ILC_COLORDDB
	    ILC_MASK
	    ILC_PALETTE
	    ILD_BLEND
	    ILD_BLEND25
	    ILD_BLEND50
	    ILD_FOCUS
	    ILD_IMAGE
	    ILD_MASK
	    ILD_NORMAL
	    ILD_OVERLAYMASK
	    ILD_SELECTED
	    ILD_TRANSPARENT
	    IMAGE_BITMAP
	    IMAGE_CURSOR
	    IMAGE_ENHMETAFILE
	    IMAGE_ICON
	    INDEXTOSTATEIMAGEMASK
	    INFINITE
	    INVALID_HANDLE_VALUE
	    I_IMAGECALLBACK
	    ImageList_Add
	    ImageList_AddIcon
	    ImageList_AddMasked
	    ImageList_BeginDrag
	    ImageList_Create
	    ImageList_Destory
	    ImageList_DragEnter
	    ImageList_DragLeave
	    ImageList_DragMove
	    ImageList_DragShowLock
	    ImageList_Draw
	    ImageList_DrawEx
	    ImageList_EndDrag
	    ImageList_ExtractIcon
	    ImageList_GetBkColor
	    ImageList_GetDrawImage
	    ImageList_GetIcon
	    ImageList_GetIconSize
	    ImageList_GetImageCount
	    ImageList_GetImageInfo
	    ImageList_LoadBitmap
	    ImageList_LoadImage
	    ImageList_Merge
	    ImageList_Remove
	    ImageList_RemoveAll
	    ImageList_Replace
	    ImageList_ReplaceIcon
	    ImageList_SetBkColor
	    ImageList_SetDragCursorImage
	    ImageList_SetIconSize
	    ImageList_SetOverlayImage
	    InSendMessage
	    IndexToOverlayMask
	    InflateRect
	    InitAtomTable
	    InitCommonControls
	    InsertMenu
	    IntersectClipRect
	    IntersectRect
	    InvalidateRect
	    InvalidateRgn
	    InvertRect
	    InvertRgn
	    IsCharAlpha
	    IsCharAlphaNumeric
	    IsCharLower
	    IsCharUpper
	    IsChild
	    IsClipboardFormatAvailable
	    IsDialogMessage
	    IsDlgButtonChecked
	    IsIconic
	    IsRectEmpty
	    IsWindow
	    IsWindowEnabled
	    IsWindowVisible
	    IsZoomed
	    KillTimer
	    LBN_DBLCLK
	    LBN_ERRSPACE
	    LBN_KILLFOCUS
	    LBN_SELCANCEL
	    LBN_SELCHANGE
	    LBN_SETFOCUS
	    LBS_EXTENDEDSEL
	    LBS_HASSTRINGS
	    LBS_MULTICOLUMN
	    LBS_MULTIPLESEL
	    LBS_NOINTEGRALHEIGHT
	    LBS_NOREDRAW
	    LBS_NOTIFY
	    LBS_OWNERDRAWFIXED
	    LBS_OWNERDRAWVARIABLE
	    LBS_SORT
	    LBS_STANDARD
	    LBS_USETABSTOPS
	    LBS_WANTKEYBOARDINPUT
	    LB_ADDSTRING
	    LB_CTLCODE
	    LB_DELETESTRING
	    LB_DIR
	    LB_ERR
	    LB_ERRSPACE
	    LB_FINDSTRING
	    LB_GETCOUNT
	    LB_GETCURSEL
	    LB_GETHORIZONTALEXTENT
	    LB_GETITEMDATA
	    LB_GETITEMRECT
	    LB_GETSEL
	    LB_GETSELCOUNT
	    LB_GETSELITEMS
	    LB_GETTEXT
	    LB_GETTEXTLEN
	    LB_GETTOPINDEX
	    LB_INSERTSTRING
	    LB_MSGMAX
	    LB_OKAY
	    LB_RESETCONTENT
	    LB_SELECTSTRING
	    LB_SELITEMRANGE
	    LB_SETCOLUMNWIDTH
	    LB_SETCURSEL
	    LB_SETHORIZONTALEXTENT
	    LB_SETITEMDATA
	    LB_SETSEL
	    LB_SETTABSTOPS
	    LB_SETTOPINDEX
	    LC_INTERIORS
	    LC_MARKER
	    LC_NONE
	    LC_POLYLINE
	    LC_POLYMARKER
	    LC_STYLED
	    LC_WIDE
	    LC_WIDESTYLED
	    LF_FACESIZE
	    LHND
	    LINECAPS
	    LMEM_DISCARDABLE
	    LMEM_DISCARDED
	    LMEM_FIXED
	    LMEM_LOCKCOUNT
	    LMEM_MODIFY
	    LMEM_MOVEABLE
	    LMEM_NOCOMPACT
	    LMEM_NODISCARD
	    LMEM_ZEROINIT
	    LOGPIXELSX
	    LOGPIXELSY
	    LPTR
	    LPTX
	    LPtoDP
	    LTGRAY_BRUSH
	    LVA_ALIGNBOTTOM
	    LVA_ALIGNLEFT
	    LVA_ALIGNRIGHT
	    LVA_ALIGNTOP
	    LVA_DEFAULT
	    LVA_SNAPTOGRID
	    LVA_SORTASCENDING
	    LVA_SORTDESCENDING
	    LVCFMT_CENTER
	    LVCFMT_LEFT
	    LVCFMT_RIGHT
	    LVCF_FMT
	    LVCF_SUBITEM
	    LVCF_TEXT
	    LVCF_WIDTH
	    LVFI_NEARESTXY
	    LVFI_PARAM
	    LVFI_PARTIAL
	    LVFI_STRING
	    LVFI_WRAP
	    LVHT_ABOVE
	    LVHT_BELOW
	    LVHT_NOWHERE
	    LVHT_ONITEM
	    LVHT_ONITEMICON
	    LVHT_ONITEMLABEL
	    LVHT_ONITEMSTATEICON
	    LVHT_TOLEFT
	    LVHT_TORIGHT
	    LVIF_DI_SETITEM
	    LVIF_IMAGE
	    LVIF_PARAM
	    LVIF_STATE
	    LVIF_TEXT
	    LVIR_BOUNDS
	    LVIR_ICON
	    LVIR_LABEL
	    LVIR_SELECTBOUNDS
	    LVIS_CUT
	    LVIS_DROPHILITED
	    LVIS_FOCUSED
	    LVIS_LINK
	    LVIS_OVERLAYMASK
	    LVIS_SELECTED
	    LVIS_STATEIMAGEMASK)
	  'windows))

(eval-when (compile load eval)
  (export '(
	    LVM_ARRANGE
	    LVM_CREATEDRAGIMAGE
	    LVM_DELETEALLITEMS
	    LVM_DELETECOLUMN
	    LVM_DELETEITEM
	    LVM_EDITLABEL
	    LVM_ENSUREVISIBLE
	    LVM_FINDITEM
	    LVM_FIRST
	    LVM_GETBKCOLOR
	    LVM_GETCALLBACKMASK
	    LVM_GETCOLUMN
	    LVM_GETCOLUMNWIDTH
	    LVM_GETCOUNTPERPAGE
	    LVM_GETEDITCONTROL
	    LVM_GETIMAGELIST
	    LVM_GETISEARCHSTRING
	    LVM_GETITEM
	    LVM_GETITEMCOUNT
	    LVM_GETITEMPOSITION
	    LVM_GETITEMRECT
	    LVM_GETITEMSPACING
	    LVM_GETITEMSTATE
	    LVM_GETITEMTEXT
	    LVM_GETNEXTITEM
	    LVM_GETORIGIN
	    LVM_GETSELECTEDCOUNT
	    LVM_GETSTRINGWIDTH
	    LVM_GETTEXTBKCOLOR
	    LVM_GETTEXTCOLOR
	    LVM_GETTOPINDEX
	    LVM_GETVIEWRECT
	    LVM_HITTEST
	    LVM_INSERTCOLUMN
	    LVM_INSERTITEM
	    LVM_REDRAWITEMS
	    LVM_SCROLL
	    LVM_SETBKCOLOR
	    LVM_SETCALLBACKMASK
	    LVM_SETCOLUMN
	    LVM_SETCOLUMNWIDTH
	    LVM_SETIMAGELIST
	    LVM_SETITEM
	    LVM_SETITEMCOUNT
	    LVM_SETITEMPOSITION
	    LVM_SETITEMPOSITION32
	    LVM_SETITEMSTATE
	    LVM_SETITEMTEXT
	    LVM_SETTEXTBKCOLOR
	    LVM_SETTEXTCOLOR
	    LVM_SORTITEMS
	    LVM_UPDATE
	    LVNI_ABOVE
	    LVNI_ALL
	    LVNI_BELOW
	    LVNI_CUT
	    LVNI_DROPHILITED
	    LVNI_FOCUSED
	    LVNI_PREVIOUS
	    LVNI_SELECTED
	    LVNI_TOLEFT
	    LVNI_TORIGHT
	    LVN_BEGINDRAG
	    LVN_BEGINLABELEDIT
	    LVN_BEGINRDRAG
	    LVN_COLUMNCLICK
	    LVN_DELETEALLITEMS
	    LVN_DELETEITEM
	    LVN_ENDDRAG
	    LVN_ENDLABELEDIT
	    LVN_ENDRDRAG
	    LVN_FIRST
	    LVN_GETDISPINFO
	    LVN_INSERTITEM
	    LVN_ITEMCHANGED
	    LVN_ITEMCHANGING
	    LVN_KEYDOWN
	    LVN_PEN
	    LVN_SETDISPINFO
	    LVSCW_AUTOSIZE
	    LVSCW_AUTOSIZE_USEHEADER
	    LVSIL_NORMAL
	    LVSIL_SMALL
	    LVSIL_STATE
	    LVS_ALIGNLEFT
	    LVS_ALIGNMASK
	    LVS_ALIGNTOP
	    LVS_AUTOARRANGE
	    LVS_EDITLABELS
	    LVS_ICON
	    LVS_LIST
	    LVS_NOCOLUMNHEADER
	    LVS_NOLABELWRAP
	    LVS_NOSCROLL
	    LVS_NOSORTHEADER
	    LVS_OWNERDRAWFIXED
	    LVS_REPORT
	    LVS_SHAREIMAGELISTS
	    LVS_SHOWSELALWAYS
	    LVS_SINGLESEL
	    LVS_SMALLICON
	    LVS_SORTASCENDING
	    LVS_SORTDESCENDING
	    LVS_TYPEMASK
	    LVS_TYPESTYLEMASK
	    LineDDA
	    LineTo
	    ListView_Arrange
	    ListView_CreateDragImage
	    ListView_DeleteAllItems
	    ListView_DeleteAllItems
	    ListView_DeleteColumn
	    ListView_DeleteItem
	    ListView_EditLabel
	    ListView_EnsureVisible
	    ListView_FindItem
	    ListView_GetBkColor
	    ListView_GetCallbackMask
	    ListView_GetColumn
	    ListView_GetColumnWidth
	    ListView_GetCountPerPage
	    ListView_GetEditControl
	    ListView_GetISearchString
	    ListView_GetImageList
	    ListView_GetItem
	    ListView_GetItemCount
	    ListView_GetItemPosition
	    ListView_GetItemRect
	    ListView_GetItemSpacing
	    ListView_GetItemState
	    ListView_GetItemText
	    ListView_GetNextItem
	    ListView_GetOrigin
	    ListView_GetSelectedCount
	    ListView_GetStringWidth
	    ListView_GetTextBkColor
	    ListView_GetTextColor
	    ListView_GetTopIndex
	    ListView_GetViewRect
	    ListView_HitTest
	    ListView_InsertColumn
	    ListView_InsertItem
	    ListView_RedrawItems
	    ListView_Scroll
	    ListView_SetBkColor
	    ListView_SetCallbackMask
	    ListView_SetColumn
	    ListView_SetColumnWidth
	    ListView_SetImageList
	    ListView_SetItem
	    ListView_SetItemCount
	    ListView_SetItemPosition
	    ListView_SetItemPosition32
	    ListView_SetItemState
	    ListView_SetItemText
	    ListView_SetTextBkColor
	    ListView_SetTextColor
	    ListView_SortItems
	    ListView_Update
	    LoadAccelerators
	    LoadBitmap
	    LoadCursor
	    LoadIcon
	    LoadImage
	    LoadLibrary
	    LoadMenu
	    LoadMenuIndirect
	    LoadResource
	    LoadString
	    LocalAlloc
	    LocalFlags
	    LocalFree
	    LocalHandle
	    LocalReAlloc
	    LocalSize
	    LocalUnlock
	    LockResource
	    MARKPARITY
	    MAX_PATH
	    MA_ACTIVATE
	    MA_ACTIVATEANDEAT
	    MA_NOACTIVATE
	    MB_ABORTRETRYIGNORE
	    MB_APPLMODAL
	    MB_DEFBUTTON1
	    MB_DEFBUTTON2
	    MB_DEFBUTTON3
	    MB_DEFMASK
	    MB_ICONASTERISK
	    MB_ICONEXCLAMATION
	    MB_ICONHAND
	    MB_ICONINFORMATION
	    MB_ICONMASK
	    MB_ICONQUESTION
	    MB_ICONSTOP
	    MB_MISCMASK
	    MB_MODEMASK
	    MB_NOFOCUS
	    MB_OK
	    MB_OKCANCEL
	    MB_RETRYCANCEL
	    MB_SYSTEMMODAL
	    MB_TASKMODAL
	    MB_TYPEMASK
	    MB_YESNO
	    MB_YESNOCANCEL
	    MCI_ANIM_GETDEVCAPS_CAN_REVERSE
	    MCI_ANIM_GETDEVCAPS_CAN_STRETCH
	    MCI_ANIM_GETDEVCAPS_FAST_RATE
	    MCI_ANIM_GETDEVCAPS_MAX_WINDOWS
	    MCI_ANIM_GETDEVCAPS_NORMAL_RATE
	    MCI_ANIM_GETDEVCAPS_PALETTES
	    MCI_ANIM_GETDEVCAPS_SLOW_RATE
	    MCI_ANIM_OPEN_NOSTATIC
	    MCI_ANIM_OPEN_PARENT
	    MCI_ANIM_OPEN_WS
	    MCI_ANIM_PLAY_FAST
	    MCI_ANIM_PLAY_REVERSE
	    MCI_ANIM_PLAY_SCAN
	    MCI_ANIM_PLAY_SLOW
	    MCI_ANIM_PLAY_SPEED
	    MCI_ANIM_STATUS_FORWARD
	    MCI_ANIM_STATUS_HPAL
	    MCI_ANIM_STATUS_HWND
	    MCI_ANIM_STATUS_SPEED
	    MCI_ANIM_STATUS_STRETCH
	    MCI_ANIM_STEP_FRAMES
	    MCI_ANIM_STEP_REVERSE
	    MCI_BREAK
	    MCI_CD_OFFSET
	    MCI_CLOSE
	    MCI_COPY
	    MCI_CUE
	    MCI_CUT
	    MCI_DELETE
	    MCI_DEVTYPE_ANIMATION
	    MCI_DEVTYPE_CD_AUDIO
	    MCI_DEVTYPE_DAT
	    MCI_DEVTYPE_DIGITAL_VIDEO
	    MCI_DEVTYPE_OTHER
	    MCI_DEVTYPE_OVERLAY
	    MCI_DEVTYPE_SCANNER
	    MCI_DEVTYPE_SEQUENCER
	    MCI_DEVTYPE_VCR
	    MCI_DEVTYPE_VIDEODISC
	    MCI_DEVTYPE_WAVEFORM_AUDIO
	    MCI_ESCAPE
	    MCI_FORMAT_BYTES
	    MCI_FORMAT_FRAMES
	    MCI_FORMAT_HMS
	    MCI_FORMAT_MILLISECONDS
	    MCI_FORMAT_MSF
	    MCI_FORMAT_SAMPLES
	    MCI_FORMAT_SMPTE_24
	    MCI_FORMAT_SMPTE_25
	    MCI_FORMAT_SMPTE_30
	    MCI_FORMAT_SMPTE_30DROP
	    MCI_FORMAT_TMSF
	    MCI_FREEZE
	    MCI_FROM
	    MCI_GETDEVCAPS
	    MCI_GETDEVCAPS_CAN_EJECT
	    MCI_GETDEVCAPS_CAN_PLAY
	    MCI_GETDEVCAPS_CAN_RECORD
	    MCI_GETDEVCAPS_CAN_SAVE
	    MCI_GETDEVCAPS_COMPOUND_DEVICE
	    MCI_GETDEVCAPS_DEVICE_TYPE
	    MCI_GETDEVCAPS_HAS_AUDIO
	    MCI_GETDEVCAPS_HAS_VIDEO
	    MCI_GETDEVCAPS_ITEM
	    MCI_GETDEVCAPS_USES_FILES
	    MCI_INFO
	    MCI_LOAD
	    MCI_MODE_NOT_READY
	    MCI_MODE_OPEN
	    MCI_MODE_PAUSE
	    MCI_MODE_PLAY
	    MCI_MODE_RECORD
	    MCI_MODE_SEEK
	    MCI_MODE_STOP
	    MCI_NOTIFY
	    MCI_NOTIFY_ABORTED
	    MCI_NOTIFY_FAILURE
	    MCI_NOTIFY_SUCCESSFUL
	    MCI_NOTIFY_SUPERSEDED
	    MCI_OPEN
	    MCI_OPEN_ALIAS
	    MCI_OPEN_ELEMENT
	    MCI_OPEN_ELEMENT_ID
	    MCI_OPEN_SHAREABLE
	    MCI_OPEN_TYPE
	    MCI_OPEN_TYPE_ID
	    MCI_OVLY_GETDEVCAPS_CAN_FREEZE
	    MCI_OVLY_GETDEVCAPS_CAN_STRETCH
	    MCI_OVLY_GETDEVCAPS_MAX_WINDOWS
	    MCI_OVLY_OPEN_PARENT
	    MCI_OVLY_OPEN_WS
	    MCI_OVLY_STATUS_HWND
	    MCI_OVLY_STATUS_STRETCH
	    MCI_PASTE
	    MCI_PAUSE
	    MCI_PLAY
	    MCI_PUT
	    MCI_REALIZE
	    MCI_RECORD
	    MCI_RECORD_INSERT
	    MCI_RECORD_OVERWRITE
	    MCI_RESUME
	    MCI_SAVE
	    MCI_SAVE_FILE
	    MCI_SEEK
	    MCI_SEEK_TO_END
	    MCI_SEEK_TO_START
	    MCI_SEQ_FILE
	    MCI_SEQ_FORMAT_SONGPTR
	    MCI_SEQ_MIDI
	    MCI_SEQ_NONE
	    MCI_SEQ_OFFSET
	    MCI_SEQ_SET_MASTER
	    MCI_SEQ_SET_OFFSET
	    MCI_SEQ_SET_PORT
	    MCI_SEQ_SET_SLAVE
	    MCI_SEQ_SET_TEMPO
	    MCI_SEQ_SMPTE
	    MCI_SET
	    MCI_SET_AUDIO
	    MCI_SET_AUDIO_ALL
	    MCI_SET_AUDIO_LEFT
	    MCI_SET_AUDIO_RIGHT
	    MCI_SET_DOOR_CLOSED
	    MCI_SET_DOOR_OPEN
	    MCI_SET_OFF
	    MCI_SET_ON
	    MCI_SET_TIME_FORMAT
	    MCI_SET_VIDEO
	    MCI_SOUND
	    MCI_SPIN
	    MCI_STATUS
	    MCI_STATUS_CURRENT_TRACK
	    MCI_STATUS_ITEM
	    MCI_STATUS_LENGTH
	    MCI_STATUS_MEDIA_PRESENT
	    MCI_STATUS_MODE
	    MCI_STATUS_NUMBER_OF_TRACKS
	    MCI_STATUS_POSITION
	    MCI_STATUS_READY
	    MCI_STATUS_START
	    MCI_STATUS_TIME_FORMAT
	    MCI_STEP
	    MCI_STOP
	    MCI_STRING_OFFSET
	    MCI_SYSINFO
	    MCI_SYSINFO_INSTALLNAME
	    MCI_SYSINFO_NAME
	    MCI_SYSINFO_OPEN
	    MCI_SYSINFO_QUANTITY
	    MCI_TO
	    MCI_TRACK
	    MCI_UNFREEZE
	    MCI_UPDATE
	    MCI_VD_GETDEVCAPS_CAN_REVERSE
	    MCI_VD_GETDEVCAPS_CAV
	    MCI_VD_GETDEVCAPS_CLV
	    MCI_VD_GETDEVCAPS_FAST_RATE
	    MCI_VD_GETDEVCAPS_NORMAL_RATE
	    MCI_VD_GETDEVCAPS_SLOW_RATE
	    MCI_VD_MEDIA_CAV
	    MCI_VD_MEDIA_CLV
	    MCI_VD_MEDIA_OTHER
	    MCI_VD_MODE_PARK
	    MCI_VD_OFFSET
	    MCI_VD_PLAY_FAST
	    MCI_VD_PLAY_REVERSE
	    MCI_VD_PLAY_SCAN
	    MCI_VD_PLAY_SLOW
	    MCI_VD_PLAY_SPEED
	    MCI_VD_SEEK_REVERSE
	    MCI_VD_STATUS_DISC_SIZE
	    MCI_VD_STATUS_FORWARD
	    MCI_VD_STATUS_MEDIA_TYPE
	    MCI_VD_STATUS_SIDE
	    MCI_VD_STATUS_SPEED
	    MCI_VD_STEP_FRAMES
	    MCI_VD_STEP_REVERSE
	    MCI_WAIT
	    MCI_WAVE_GETDEVCAPS_INPUTS
	    MCI_WAVE_GETDEVCAPS_OUTPUTS
	    MCI_WAVE_INPUT
	    MCI_WAVE_OFFSET
	    MCI_WAVE_OPEN_BUFFER
	    MCI_WAVE_OUTPUT
	    MCI_WAVE_SET_ANYINPUT
	    MCI_WAVE_SET_ANYOUTPUT
	    MCI_WAVE_SET_AVGBYTESPERSEC
	    MCI_WAVE_SET_BITSPERSAMPLE
	    MCI_WAVE_SET_BLOCKALIGN
	    MCI_WAVE_SET_CHANNELS
	    MCI_WAVE_SET_FORMATTAG
	    MCI_WAVE_SET_SAMPLESPERSEC
	    MCI_WAVE_STATUS_AVGBYTESPERSEC
	    MCI_WAVE_STATUS_BITSPERSAMPLE
	    MCI_WAVE_STATUS_BLOCKALIGN
	    MCI_WAVE_STATUS_CHANNELS
	    MCI_WAVE_STATUS_FORMATTAG
	    MCI_WAVE_STATUS_LEVEL
	    MCI_WAVE_STATUS_SAMPLESPERSEC
	    MCI_WHERE
	    MCI_WINDOW
	    MERGECOPY
	    MERGEPAINT
	    META_ANIMATEPALETTE
	    META_ARC
	    META_BITBLT
	    META_CHORD
	    META_CREATEBRUSHINDIRECT
	    META_CREATEFONTINDIRECT
	    META_CREATEPALETTE
	    META_CREATEPATTERNBRUSH
	    META_CREATEPENINDIRECT
	    META_CREATEREGION
	    META_DELETEOBJECT
	    META_DIBBITBLT
	    META_DIBCREATEPATTERNBRUSH
	    META_DIBSTRETCHBLT
	    META_ELLIPSE
	    META_ESCAPE
	    META_EXCLUDECLIPRECT
	    META_EXTTEXTOUT
	    META_FILLREGION
	    META_FLOODFILL
	    META_FRAMEREGION
	    META_INTERSECTCLIPRECT
	    META_INVERTREGION
	    META_LINETO
	    META_MOVETO
	    META_OFFSETCLIPRGN
	    META_OFFSETVIEWPORTORG
	    META_OFFSETWINDOWORG
	    META_PAINTREGION
	    META_PATBLT
	    META_PIE
	    META_POLYGON
	    META_POLYLINE
	    META_POLYPOLYGON
	    META_REALIZEPALETTE
	    META_RECTANGLE
	    META_RESIZEPALETTE
	    META_RESTOREDC
	    META_ROUNDRECT
	    META_SAVEDC
	    META_SCALEVIEWPORTEXT
	    META_SCALEWINDOWEXT
	    META_SELECTCLIPREGION
	    META_SELECTOBJECT
	    META_SELECTPALETTE
	    META_SETBKCOLOR
	    META_SETBKMODE
	    META_SETDIBTODEV
	    META_SETMAPMODE
	    META_SETMAPPERFLAGS
	    META_SETPALENTRIES
	    META_SETPIXEL
	    META_SETPOLYFILLMODE
	    META_SETRELABS
	    META_SETROP2
	    META_SETSTRETCHBLTMODE
	    META_SETTEXTALIGN
	    META_SETTEXTCHAREXTRA
	    META_SETTEXTCOLOR
	    META_SETTEXTJUSTIFICATION
	    META_SETVIEWPORTEXT
	    META_SETVIEWPORTORG
	    META_SETWINDOWEXT
	    META_SETWINDOWORG
	    META_STRETCHBLT
	    META_STRETCHDIB
	    META_TEXTOUT
	    MFCOMMENT
	    MF_APPEND
	    MF_BITMAP
	    MF_BYCOMMAND
	    MF_BYPOSITION
	    MF_CHANGE
	    MF_CHECKED
	    MF_DELETE
	    MF_DISABLED
	    MF_ENABLED
	    MF_END
	    MF_GRAYED
	    MF_HELP
	    MF_HILITE
	    MF_INSERT
	    MF_MENUBARBREAK
	    MF_MENUBREAK
	    MF_MOUSESELECT
	    MF_OWNERDRAW
	    MF_POPUP
	    MF_REMOVE
	    MF_SEPARATOR
	    MF_STRING
	    MF_SYSMENU
	    MF_UNCHECKED
	    MF_UNHILITE
	    MF_USECHECKBITMAPS
	    MK_CONTROL
	    MK_LBUTTON
	    MK_MBUTTON
	    MK_RBUTTON
	    MK_SHIFT
	    MM_ANISOTROPIC
	    MM_HIENGLISH
	    MM_HIMETRIC
	    MM_ISOTROPIC
	    MM_LOENGLISH
	    MM_LOMETRIC
	    MM_MCINOTIFY
	    MM_TEXT
	    MM_TWIPS
	    MSGF_DDEMGR
	    MSGF_DIALOGBOX
	    MSGF_MENU
	    MSGF_MESSAGEBOX
	    MSGF_MOVE
	    MSGF_NEXTWINDOW
	    MSGF_SCROLLBAR
	    MSGF_SIZE
	    MapDialogRect
	    MapVirtualKey
	    MessageBeep
	    MessageBox
	    ModifyMenu
	    MoveToEx
	    MoveWindow
	    MulDiv
	    NEWFRAME
	    NEXTBAND
	    NM_BTNCLK
	    NM_CLICK
	    NM_DBLCLK
	    NM_ENDWAIT
	    NM_FIRST
	    NM_KILLFOCUS
	    NM_LAST
	    NM_OUTOFMEMORY
	    NM_RCLICK
	    NM_RDBLCLK
	    NM_RETURN
	    NM_SETFOCUS
	    NM_STARTWAIT
	    NONZEROLHND
	    NONZEROLPTR
	    NOPARITY
	    NOTSRCCOPY
	    NOTSRCERASE
	    NULLREGION
	    NULL_BRUSH
	    NULL_PEN
	    NUMBRUSHES
	    NUMCOLORS
	    NUMFONTS
	    NUMMARKERS
	    NUMPENS
	    NUMRESERVED
	    OBJ_BRUSH
	    OBJ_PEN
	    ODA_DRAWENTIRE
	    ODA_FOCUS
	    ODA_SELECT
	    ODDPARITY
	    ODS_CHECKED
	    ODS_DISABLED
	    ODS_FOCUS
	    ODS_GRAYED
	    ODS_SELECTED
	    ODT_BUTTON
	    ODT_COMBOBOX
	    ODT_LISTBOX
	    ODT_MENU
	    ODT_TAB
	    OEM_CHARSET
	    OEM_FIXED_FONT
	    OFN_ALLOWMULTISELECT
	    OFN_CREATEPROMPT
	    OFN_ENABLEHOOK
	    OFN_ENABLETEMPLATE
	    OFN_ENABLETEMPLATEHANDLE
	    OFN_EXTENSIONDIFFERENT
	    OFN_FILEMUSTEXIST
	    OFN_HIDEREADONLY
	    OFN_NOCHANGEDIR
	    OFN_NOLONGNAMES
	    OFN_NONETWORKBUTTON
	    OFN_NOREADONLYRETURN
	    OFN_NOTESTFILECREATE
	    OFN_NOVALIDATE
	    OFN_OVERWRITEPROMPT
	    OFN_PATHMUSTEXIST
	    OFN_READONLY
	    OFN_SHAREAWARE
	    OFN_SHOWHELP
	    OF_CANCEL
	    OF_CREATE
	    OF_DELETE
	    OF_EXIST
	    OF_PARSE
	    OF_PROMPT
	    OF_READ
	    OF_READWRITE
	    OF_REOPEN
	    OF_SHARE_COMPAT
	    OF_SHARE_DENY_NONE
	    OF_SHARE_DENY_READ
	    OF_SHARE_DENY_WRITE
	    OF_SHARE_EXCLUSIVE
	    OF_VERIFY
	    OF_WRITE
	    ONE5STOPBITS
	    ONESTOPBIT
	    OPAQUE
	    OPEN_EXISTING
	    ORD_LANGDRIVER
	    OUT_CHARACTER_PRECIS
	    OUT_DEFAULT_PRECIS
	    OUT_STRING_PRECIS
	    OUT_STROKE_PRECIS
	    OffsetClipRgn
	    OffsetRect
	    OffsetRgn
	    OffsetViewportOrgEx
	    OffsetWindowOrgEx
	    OpenClipboard
	    OpenFile
	    OpenIcon
	    OutputDebugString
	    PASSTHROUGH
	    PATCOPY
	    PATINVERT
	    PATPAINT
	    PBM_DELTAPOS
	    PBM_SETPOS
	    PBM_SETRANGE
	    PBM_SETSTEP
	    PBM_STEPIT
	    PC_EXPLICIT
	    PC_INTERIORS
	    PC_NOCOLLAPSE
	    PC_NONE
	    PC_POLYGON
	    PC_RECTANGLE
	    PC_RESERVED
	    PC_SCANLINE
	    PC_STYLED
	    PC_TRAPEZOID
	    PC_WIDE
	    PC_WIDESTYLED
	    PC_WINDPOLYGON
	    PDEVICESIZE
	    PLANES
	    PM_NOREMOVE
	    PM_NOYIELD
	    PM_REMOVE
	    POLYGONALCAPS
	    POSTSCRIPT_DATA
	    POSTSCRIPT_IGNORE
	    PROGRESS_CLASS
	    PROGRESS_CLASSA
	    PROOF_QUALITY
	    PR_JOBSTATUS
	    PS_DASH
	    PS_DASHDOT
	    PS_DASHDOTDOT
	    PS_DOT
	    PS_INSIDEFRAME
	    PS_NULL
	    PS_SOLID
	    PaintRgn
	    PatBlt
	    PeekMessage
	    Pie
	    PlayMetaFile
	    PlayMetaFileRecord
	    PolyPolygon
	    Polygon
	    Polyline
	    PostMessage
	    PostQuitMessage
	    PostThreadMessage
	    PtInRect
	    PtInRegion
	    PtVisible
	    QID_SYNC
	    QUERYESCSUPPORT
	    R2_BLACK
	    R2_COPYPEN
	    R2_MASKNOTPEN
	    R2_MASKPEN
	    R2_MASKPENNOT
	    R2_MERGENOTPEN
	    R2_MERGEPEN
	    R2_MERGEPENNOT
	    R2_NOP
	    R2_NOT
	    R2_NOTCOPYPEN
	    R2_NOTMASKPEN
	    R2_NOTMERGEPEN
	    R2_NOTXORPEN
	    R2_WHITE
	    R2_XORPEN
	    RASTERCAPS
	    RASTER_FONTTYPE
	    RC_BANDING
	    RC_BIGFONT
	    RC_BITBLT
	    RC_BITMAP64
	    RC_DIBTODEV
	    RC_DI_BITMAP
	    RC_FLOODFILL
	    RC_GDI20_OUTPUT
	    RC_PALETTE
	    RC_SCALING
	    RC_STRETCHBLT
	    RC_STRETCHDIB
	    RELATIVE
	    RESETDEV
	    RESTORE_CTM
	    RGN_AND
	    RGN_COPY
	    RGN_DIFF
	    RGN_OR
	    RGN_XOR
	    RealizePalette
	    RectInRegion
	    RectVisible
	    Rectangle
	    RegisterClass
	    RegisterClassEx
	    RegisterClipboardFormat
	    RegisterWindowMessage
	    ReleaseCapture
	    ReleaseDC
	    RemoveFontResource
	    RemoveMenu
	    RemoveProp
	    ReplyMessage
	    ResizePalette
	    RestoreDC
	    RoundRect
	    SAVE_CTM
	    SBARS_SIZEGRIP
	    SBS_BOTTOMALIGN
	    SBS_HORZ
	    SBS_LEFTALIGN
	    SBS_RIGHTALIGN
	    SBS_SIZEBOX
	    SBS_SIZEBOXBOTTOMRIGHTALIGN
	    SBS_SIZEBOXTOPLEFTALIGN
	    SBS_TOPALIGN
	    SBS_VERT
	    SBT_NOBORDERS
	    SBT_OWNERDRAW
	    SBT_POPOUT
	    SB_BOTH
	    SB_BOTTOM
	    SB_CTL
	    SB_ENDSCROLL
	    SB_GETBORDERS
	    SB_GETPARTS
	    SB_GETRECT
	    SB_GETTEXT
	    SB_GETTEXTA
	    SB_GETTEXTLENGTH
	    SB_GETTEXTLENGTHA
	    SB_HORZ
	    SB_LINEDOWN
	    SB_LINEUP
	    SB_PAGEDOWN
	    SB_PAGEUP
	    SB_SETMINHEIGHT
	    SB_SETPARTS
	    SB_SETTEXT
	    SB_SETTEXTA
	    SB_SIMPLE
	    SB_THUMBPOSITION
	    SB_THUMBTRACK
	    SB_TOP
	    SB_VERT
	    SC_ARRANGE
	    SC_CLOSE
	    SC_HSCROLL
	    SC_ICON
	    SC_KEYMENU
	    SC_MAXIMIZE
	    SC_MINIMIZE
	    SC_MOUSEMENU
	    SC_MOVE
	    SC_NEXTWINDOW
	    SC_PREVWINDOW
	    SC_RESTORE
	    SC_SIZE
	    SC_TASKLIST
	    SC_VSCROLL
	    SC_ZOOM
	    SELECTPAPERSOURCE
	    SETABORTPROC
	    SETALLJUSTVALUES
	    SETCHARSET
	    SETCOLORTABLE
	    SETCOPYCOUNT
	    SETDIBSCALING
	    SETDTR
	    SETKERNTRACK
	    SETLINEJOIN
	    SETMITERLIMIT
	    SETRTS
	    SETXOFF
	    SETXON
	    SET_ARC_DIRECTION
	    SET_BACKGROUND_COLOR
	    SET_BOUNDS
	    SET_CLIP_BOX
	    SET_MIRROR_MODE
	    SET_POLY_MODE
	    SET_SCREEN_ANGLE
	    SET_SPREAD
	    SHIFTJIS_CHARSET
	    SHOW_FULLSCREEN
	    SHOW_ICONWINDOW
	    SHOW_OPENNOACTIVATE
	    SHOW_OPENWINDOW
	    SIMPLEREGION
	    SIZEFULLSCREEN
	    SIZEICONIC
	    SIZENORMAL
	    SIZEPALETTE
	    SIZEZOOMHIDE
	    SIZEZOOMSHOW
	    SM_CMETRICS
	    SM_CXBORDER
	    SM_CXCURSOR
	    SM_CXDLGFRAME
	    SM_CXFRAME
	    SM_CXFULLSCREEN
	    SM_CXHSCROLL
	    SM_CXHTHUMB
	    SM_CXICON
	    SM_CXMIN
	    SM_CXMINTRACK
	    SM_CXSCREEN
	    SM_CXSIZE
	    SM_CXSMSIZE
	    SM_CXVSCROLL
	    SM_CYBORDER
	    SM_CYCAPTION
	    SM_CYCURSOR
	    SM_CYDLGFRAME
	    SM_CYFRAME
	    SM_CYFULLSCREEN
	    SM_CYHSCROLL
	    SM_CYICON
	    SM_CYKANJIWINDOW
	    SM_CYMENU
	    SM_CYMIN
	    SM_CYMINTRACK
	    SM_CYSCREEN
	    SM_CYSIZE
	    SM_CYSMSIZE
	    SM_CYVSCROLL
	    SM_CYVTHUMB
	    SM_DEBUG
	    SM_MOUSEPRESENT
	    SM_RESERVED1
	    SM_RESERVED2
	    SM_RESERVED3
	    SM_RESERVED4
	    SM_SWAPBUTTON
	    SPACEPARITY
	    SPI_GETNONCLIENTMETRICS
	    SPI_GETWORKAREA
	    SP_APPABORT
	    SP_ERROR
	    SP_NOTREPORTED
	    SP_OUTOFDISK
	    SP_OUTOFMEMORY
	    SP_USERABORT
	    SRCAND
	    SRCCOPY
	    SRCERASE
	    SRCINVERT
	    SRCPAINT
	    SS_BLACKFRAME
	    SS_BLACKRECT
	    SS_CENTER
	    SS_GRAYFRAME
	    SS_GRAYRECT
	    SS_ICON
	    SS_LEFT
	    SS_LEFTNOWORDWRAP
	    SS_NOPREFIX
	    SS_RIGHT
	    SS_SIMPLE
	    SS_USERITEM
	    SS_WHITEFRAME
	    SS_WHITERECT
	    STARTDOC
	    STATUSCLASSNAME
	    STATUSCLASSNAMEA
	    STRETCHBLT
	    ST_ADVISE
	    ST_BEGINSWP
	    ST_CLIENT
	    ST_CONNECTED
	    ST_ENDSWP
	    ST_INLIST
	    ST_ISLOCAL
	    ST_ISSELF
	    ST_TERMINATED
	    SWP_DRAWFRAME
	    SWP_HIDEWINDOW
	    SWP_NOACTIVATE
	    SWP_NOCOPYBITS
	    SWP_NOMOVE
	    SWP_NOREDRAW
	    SWP_NOREPOSITION
	    SWP_NOSIZE
	    SWP_NOZORDER
	    SWP_SHOWWINDOW
	    SW_HIDE
	    SW_MAXIMIZE
	    SW_MINIMIZE
	    SW_NORMAL
	    SW_OTHERUNZOOM
	    SW_OTHERZOOM
	    SW_PARENTCLOSING
	    SW_PARENTOPENING
	    SW_RESTORE
	    SW_SHOW
	    SW_SHOWMAXIMIZED
	    SW_SHOWMINIMIZED
	    SW_SHOWMINNOACTIVE
	    SW_SHOWNA
	    SW_SHOWNOACTIVATE
	    SW_SHOWNORMAL
	    SYMBOL_CHARSET
	    SYSPAL_NOSTATIC
	    SYSPAL_STATIC
	    SYSTEM_FIXED_FONT
	    SYSTEM_FONT
	    S_ALLTHRESHOLD
	    S_LEGATO
	    S_NORMAL
	    S_PERIOD1024
	    S_PERIOD2048
	    S_PERIOD512
	    S_PERIODVOICE
	    S_QUEUEEMPTY
	    S_SERBDNT
	    S_SERDCC
	    S_SERDDR
	    S_SERDFQ
	    S_SERDLN
	    S_SERDMD
	    S_SERDPT
	    S_SERDSH
	    S_SERDSR
	    S_SERDST
	    S_SERDTP
	    S_SERDVL
	    S_SERDVNA
	    S_SERMACT
	    S_SEROFM
	    S_SERQFUL
	    S_STACCATO
	    S_THRESHOLD
	    S_WHITE1024
	    S_WHITE2048
	    S_WHITE512
	    S_WHITEVOICE
	    SaveDC
	    ScaleViewportExtEx
	    ScaleWindowExtEx
	    ScreenToClient
	    ScrollDC
	    ScrollWindow
	    SelectClipRgn
	    SelectObject
	    SelectPalette
	    SendDlgItemMessage
	    SendMessage
	    SendMessage
	    SetAbortProc
	    SetActiveWindow
	    SetBitmapBits
	    SetBitmapDimensionEx
	    SetBkColor
	    SetBkMode
	    SetBrushOrgEx
	    SetCapture
	    SetCaretBlinkTime
	    SetCaretPos
	    SetClassLong
	    SetClassWord
	    SetClipboardData
	    SetClipboardViewer
	    SetCommBreak
	    SetCommMask
	    SetCommState
	    SetCursor
	    SetCursorPos
	    SetDIBits
	    SetDIBitsToDevice
	    SetDlgItemInt
	    SetDlgItemText
	    SetDoubleClickTime
	    SetEnvironmentVariable
	    SetErrorMode
	    SetFocus
	    SetForegroundWindow
	    SetKeyboardState
	    SetMapMode
	    SetMapperFlags
	    SetMenu
	    SetMenuItemBitmaps
	    SetMetaFileBitsEx
	    SetPaletteEntries
	    SetParent
	    SetPixel
	    SetPolyFillMode
	    SetProp
	    SetROP2
	    SetRect
	    SetRectEmpty
	    SetRectRgn
	    SetScrollPos
	    SetScrollRange
	    SetStretchBltMode
	    SetSysColors
	    SetSystemPaletteUse
	    SetTextAlign
	    SetTextCharacterExtra
	    SetTextColor
	    SetTextJustification
	    SetTimer
	    SetViewportExtEx
	    SetViewportOrgEx
	    SetWindowExtEx
	    SetWindowLong
	    SetWindowOrgEx
	    SetWindowPos
	    SetWindowText
	    SetWindowWord
	    SetWindowsHook
	    ShowCaret
	    ShowCursor
	    ShowOwnedPopups
	    ShowScrollBar
	    ShowWindow
	    SizeofResource
	    StartDoc
	    StartPage
	    StretchBlt
	    StretchDIBits
	    SwapMouseButton
	    SystemParametersInfo
	    TA_BASELINE
	    TA_BOTTOM
	    TA_CENTER
	    TA_LEFT
	    TA_NOUPDATECP
	    TA_RIGHT
	    TA_TOP
	    TA_UPDATECP
	    TBM_CLEARSEL
	    TBM_CLEARTICS
	    TBM_GETCHANNELRECT
	    TBM_GETLINESIZE
	    TBM_GETNUMTICS
	    TBM_GETPAGESIZE
	    TBM_GETPOS
	    TBM_GETPTICS
	    TBM_GETRANGEMAX
	    TBM_GETRANGEMIN
	    TBM_GETSELEND
	    TBM_GETSELSTART
	    TBM_GETTHUMBLENGTH
	    TBM_GETTHUMBRECT
	    TBM_GETTIC
	    TBM_GETTICPOS
	    TBM_SETLINESIZE
	    TBM_SETPAGESIZE
	    TBM_SETPOS
	    TBM_SETRANGE
	    TBM_SETRANGEMAX
	    TBM_SETRANGEMIN
	    TBM_SETSEL
	    TBM_SETSELEND
	    TBM_SETSELSTART
	    TBM_SETTHUMBLENGTH
	    TBM_SETTIC
	    TBM_SETTICFREQ
	    TBS_AUTOTICKS
	    TBS_BOTH
	    TBS_BOTTOM
	    TBS_ENABLESELRANGE
	    TBS_FIXEDLENGTH
	    TBS_HORZ
	    TBS_LEFT
	    TBS_NOTHUMB
	    TBS_NOTICKS
	    TBS_RIGHT
	    TBS_TOP
	    TBS_VERT
	    TB_BOTTOM
	    TB_ENDTRACK
	    TB_LINEDOWN
	    TB_LINEUP
	    TB_PAGEDOWN
	    TB_PAGEUP
	    TB_THUMBPOSITION
	    TB_THUMBTRACK
	    TB_TOP
	    TCHT_NOWHERE
	    TCHT_ONITEM
	    TCHT_ONITEMICON
	    TCHT_ONITEMLABEL
	    TCIF_IMAGE
	    TCIF_PARAM
	    TCIF_TEXT
	    TCM_ADJUSTRECT
	    TCM_DELETEALLITEMS
	    TCM_DELETEITEM
	    TCM_FIRST
	    TCM_GETBKCOLOR
	    TCM_GETCURFOCUS
	    TCM_GETCURSEL
	    TCM_GETIMAGELIST
	    TCM_GETITEM
	    TCM_GETITEMA
	    TCM_GETITEMCOUNT
	    TCM_GETITEMRECT
	    TCM_GETROWCOUNT
	    TCM_GETTOOLTIPS
	    TCM_HITTEST
	    TCM_INSERTITEM
	    TCM_INSERTITEMA
	    TCM_REMOVEIMAGE
	    TCM_SETBKCOLOR
	    TCM_SETCURFOCUS
	    TCM_SETCURSEL
	    TCM_SETIMAGELIST
	    TCM_SETITEM
	    TCM_SETITEMA
	    TCM_SETITEMEXTRA
	    TCM_SETITEMSIZE
	    TCM_SETPADDING
	    TCM_SETTOOLTIPS
	    TCN_FIRST
	    TCN_KEYDOWN
	    TCN_LAST
	    TCN_SELCHANGE
	    TCN_SELCHANGING
	    TCS_BUTTONS
	    TCS_FIXEDWIDTH
	    TCS_FOCUSNEVER
	    TCS_FOCUSONBUTTONDOWN
	    TCS_FORCEICONLEFT
	    TCS_FORCELABELLEFT
	    TCS_MULTILINE
	    TCS_OWNERDRAWFIXED
	    TCS_RAGGEDRIGHT
	    TCS_RIGHTJUSTIFY
	    TCS_SHAREIMAGELISTS
	    TCS_SINGLELINE
	    TCS_TABS
	    TCS_TOOLTIPS
	    TC_CP_STROKE
	    TC_CR_90
	    TC_CR_ANY
	    TC_EA_DOUBLE
	    TC_IA_ABLE
	    TC_OP_CHARACTER
	    TC_OP_STROKE
	    TC_RA_ABLE
	    TC_RESERVED
	    TC_SA_CONTIN
	    TC_SA_DOUBLE
	    TC_SA_INTEGER
	    TC_SF_X_YINDEP
	    TC_SO_ABLE
	    TC_UA_ABLE
	    TC_VA_ABLE
	    TECHNOLOGY
	    TEXTCAPS
	    TIMEOUT_ASYNC
	    TIME_ONESHOT
	    TIME_PERIODIC
	    TPM_RETURNCMD
	    TPM_NONOTIFY
	    TPM_LEFTBUTTON
	    TPM_RIGHTBUTTON
	    TRACKBAR_CLASS
	    TRACKBAR_CLASSA
	    TRANSFORM_CTM
	    TRANSPARENT
	    TRUE
	    TWOSTOPBITS
	    TabCtrl_AdjustRect
	    TabCtrl_DeleteAllItems
	    TabCtrl_DeleteItem
	    TabCtrl_GetBkColor
	    TabCtrl_GetCurFocus
	    TabCtrl_GetCurSel
	    TabCtrl_GetImageList
	    TabCtrl_GetItem
	    TabCtrl_GetItemCount
	    TabCtrl_GetItemRect
	    TabCtrl_GetRowCount
	    TabCtrl_GetToolTips
	    TabCtrl_HitTest
	    TabCtrl_InsertItem
	    TabCtrl_RemoveImage
	    TabCtrl_SetBkColor
	    TabCtrl_SetCurFocus
	    TabCtrl_SetCurSel
	    TabCtrl_SetImageList
	    TabCtrl_SetItem
	    TabCtrl_SetItemExtra
	    TabCtrl_SetItemSize
	    TabCtrl_SetPadding
	    TabCtrl_SetToolTips
	    TabbedTextOut
	    TextOut
	    ToAscii
	    TrackPopupMenu
	    TranslateAccelerator
	    TranslateMDISysAccel
	    TranslateMessage
	    TransmitCommChar
	    UDM_GETACCEL
	    UDM_GETBASE
	    UDM_GETBUDDY
	    UDM_GETPOS
	    UDM_GETRANGE
	    UDM_SETACCEL
	    UDM_SETBASE
	    UDM_SETBUDDY
	    UDM_SETPOS
	    UDM_SETRANGE
	    UDN_DELTAPOS
	    UDN_FIRST
	    UDN_LAST
	    UDS_ALIGNLEFT
	    UDS_ALIGNRIGHT
	    UDS_ARROWKEYS
	    UDS_AUTOBUDDY
	    UDS_HORZ
	    UDS_NOTHOUSANDS
	    UDS_SETBUDDYINT
	    UDS_WRAP
	    UD_MAXVAL
	    UD_MINVAL
	    UPDOWN_CLASS
	    UPDOWN_CLASSA
	    UnhookWindowsHook
	    UnionRect
	    UnregisterClass
	    UpdateColors
	    UpdateWindow
	    VARIABLE_PITCH
	    VERTRES
	    VERTSIZE
	    VK_ADD
	    VK_BACK
	    VK_CANCEL
	    VK_CAPITAL
	    VK_CLEAR
	    VK_CONTROL
	    VK_DECIMAL
	    VK_DELETE
	    VK_DIVIDE
	    VK_DOWN
	    VK_END
	    VK_ESCAPE
	    VK_EXECUTE
	    VK_F1
	    VK_F10
	    VK_F11
	    VK_F12
	    VK_F13
	    VK_F14
	    VK_F15
	    VK_F16
	    VK_F2
	    VK_F3
	    VK_F4
	    VK_F5
	    VK_F6
	    VK_F7
	    VK_F8
	    VK_F9
	    VK_HELP
	    VK_HOME
	    VK_INSERT
	    VK_LBUTTON
	    VK_LEFT
	    VK_MBUTTON
	    VK_MENU
	    VK_MULTIPLY
	    VK_NEXT
	    VK_NUMLOCK
	    VK_NUMPAD0
	    VK_NUMPAD1
	    VK_NUMPAD2
	    VK_NUMPAD3
	    VK_NUMPAD4
	    VK_NUMPAD5
	    VK_NUMPAD6
	    VK_NUMPAD7
	    VK_NUMPAD8
	    VK_NUMPAD9
	    VK_PAUSE
	    VK_PRINT
	    VK_PRIOR
	    VK_RBUTTON
	    VK_RETURN
	    VK_RIGHT
	    VK_SELECT
	    VK_SEPARATOR
	    VK_SHIFT
	    VK_SNAPSHOT
	    VK_SPACE
	    VK_SUBTRACT
	    VK_TAB
	    VK_UP
	    ValidateRect
	    ValidateRgn
	    VirtualLock
	    VirtualUnlock
	    VkKeyScan
	    WC_DEFWINDOWPROC
	    WC_DRAWCAPTION
	    WC_INIT
	    WC_LISTVIEW
	    WC_MINMAX
	    WC_MOVE
	    WC_SIZE
	    WC_SWP
	    WC_TABCONTROL
	    WC_TABCONTROLA
	    WHITENESS
	    WHITEONBLACK
	    WHITE_BRUSH
	    WHITE_PEN
	    WH_CALLWNDPROC
	    WH_CBT
	    WH_GETMESSAGE
	    WH_JOURNALPLAYBACK
	    WH_JOURNALRECORD
	    WH_KEYBOARD
	    WH_MSGFILTER
	    WH_SYSMSGFILTER
	    WINDING
	    WM_ACTIVATE
	    WM_ACTIVATEAPP
	    WM_ASKCBFORMATNAME
	    WM_CANCELMODE
	    WM_CHANGECBCHAIN
	    WM_CHAR
	    WM_CHARTOITEM
	    WM_CHILDACTIVATE
	    WM_CLEAR
	    WM_CLOSE
	    WM_COMMAND
	    WM_COMPACTING
	    WM_COMPAREITEM
	    WM_COPY
	    WM_CREATE
	    WM_CTLCOLORBTN
	    WM_CTLCOLORDLG
	    WM_CTLCOLOREDIT
	    WM_CTLCOLORLISTBOX
	    WM_CTLCOLORMSGBOX
	    WM_CTLCOLORSCROLLBAR
	    WM_CTLCOLORSTATIC
	    WM_CUT
	    WM_DDE_ACK
	    WM_DDE_ADVISE
	    WM_DDE_DATA
	    WM_DDE_EXECUTE
	    WM_DDE_FIRST
	    WM_DDE_INITIATE
	    WM_DDE_LAST
	    WM_DDE_POKE
	    WM_DDE_REQUEST
	    WM_DDE_TERMINATE
	    WM_DDE_UNADVISE
	    WM_DEADCHAR
	    WM_DELETEITEM
	    WM_DESTROY
	    WM_DESTROYCLIPBOARD
	    WM_DEVMODECHANGE
	    WM_DRAWCLIPBOARD
	    WM_DRAWITEM
	    WM_ENABLE
	    WM_ENDSESSION
	    WM_ENTERIDLE
	    WM_ENTERMENULOOP
	    WM_ERASEBKGND
	    WM_EXITMENULOOP
	    WM_FONTCHANGE
	    WM_GETDLGCODE
	    WM_GETFONT
	    WM_GETMINMAXINFO
	    WM_GETTEXT
	    WM_GETTEXTLENGTH
	    WM_HSCROLL
	    WM_HSCROLLCLIPBOARD
	    WM_ICONERASEBKGND
	    WM_INITDIALOG
	    WM_INITMENU
	    WM_INITMENUPOPUP
	    WM_KEYDOWN
	    WM_KEYFIRST
	    WM_KEYLAST
	    WM_KEYUP
	    WM_KILLFOCUS
	    WM_LBUTTONDBLCLK
	    WM_LBUTTONDOWN
	    WM_LBUTTONUP
	    WM_MBUTTONDBLCLK
	    WM_MBUTTONDOWN
	    WM_MBUTTONUP
	    WM_MDIACTIVATE
	    WM_MDICASCADE
	    WM_MDICREATE
	    WM_MDIDESTROY
	    WM_MDIGETACTIVE
	    WM_MDIICONARRANGE
	    WM_MDIMAXIMIZE
	    WM_MDINEXT
	    WM_MDIRESTORE
	    WM_MDISETMENU
	    WM_MDITILE
	    WM_MEASUREITEM
	    WM_MENUCHAR
	    WM_MENUSELECT
	    WM_MOUSEACTIVATE
	    WM_MOUSEFIRST
	    WM_MOUSELAST
	    WM_MOUSEMOVE
	    WM_MOVE
	    WM_NCACTIVATE
	    WM_NCCALCSIZE
	    WM_NCCREATE
	    WM_NCDESTROY
	    WM_NCHITTEST
	    WM_NCLBUTTONDBLCLK
	    WM_NCLBUTTONDOWN
	    WM_NCLBUTTONUP
	    WM_NCMBUTTONDBLCLK
	    WM_NCMBUTTONDOWN
	    WM_NCMBUTTONUP
	    WM_NCMOUSEMOVE
	    WM_NCPAINT
	    WM_NCRBUTTONDBLCLK
	    WM_NCRBUTTONDOWN
	    WM_NCRBUTTONUP
	    WM_NEXTDLGCTL
	    WM_NULL
	    WM_PAINT
	    WM_PAINTCLIPBOARD
	    WM_PAINTICON
	    WM_PALETTECHANGED
	    WM_PALETTEISCHANGING
	    WM_PARENTNOTIFY
	    WM_PASTE
	    WM_QUERYDRAGICON
	    WM_QUERYENDSESSION
	    WM_QUERYNEWPALETTE
	    WM_QUERYOPEN
	    WM_QUEUESYNC
	    WM_QUIT
	    WM_RBUTTONDBLCLK
	    WM_RBUTTONDOWN
	    WM_RBUTTONUP
	    WM_RENDERALLFORMATS
	    WM_RENDERFORMAT
	    WM_SETCURSOR
	    WM_SETFOCUS
	    WM_SETFONT
	    WM_SETREDRAW
	    WM_SETTEXT
	    WM_SHOWWINDOW
	    WM_SIZE
	    WM_SIZECLIPBOARD
	    WM_SPOOLERSTATUS
	    WM_SYSCHAR
	    WM_SYSCOLORCHANGE
	    WM_SYSCOMMAND
	    WM_SYSDEADCHAR
	    WM_SYSKEYDOWN
	    WM_SYSKEYUP
	    WM_TIMECHANGE
	    WM_TIMER
	    WM_UNDO
	    WM_USER
	    WM_VKEYTOITEM
	    WM_VSCROLL
	    WM_VSCROLLCLIPBOARD
	    WM_WININICHANGE
	    WS_BORDER
	    WS_CAPTION
	    WS_CHILD
	    WS_CHILDWINDOW
	    WS_CLIPCHILDREN
	    WS_CLIPSIBLINGS
	    WS_DISABLED
	    WS_DLGFRAME
	    WS_EX_DLGMODALFRAME
	    WS_EX_NOPARENTNOTIFY
	    WS_EX_TOPMOST
	    WS_EX_ACCEPTFILES
	    WS_EX_TRANSPARENT
	    WS_EX_MDICHILD
	    WS_EX_TOOLWINDOW
	    WS_EX_WINDOWEDGE
	    WS_EX_CLIENTEDGE
	    WS_EX_CONTEXTHELP
	    WS_EX_RIGHT
	    WS_EX_LEFT
	    WS_EX_RTLREADING
	    WS_EX_LTRREADING
	    WS_EX_LEFTSCROLLBAR
	    WS_EX_RIGHTSCROLLBAR
	    WS_EX_CONTROLPARENT
	    WS_EX_STATICEDGE
	    WS_EX_APPWINDOW
	    WS_EX_OVERLAPPEDWINDOW
	    WS_EX_PALETTEWINDOW
	    WS_GROUP
	    WS_HSCROLL
	    WS_ICONIC
	    WS_MAXIMIZE
	    WS_MAXIMIZEBOX
	    WS_MINIMIZE
	    WS_MINIMIZEBOX
	    WS_OVERLAPPED
	    WS_OVERLAPPEDWINDOW
	    WS_POPUP
	    WS_POPUPWINDOW
	    WS_SIZEBOX
	    WS_SYSMENU
	    WS_TABSTOP
	    WS_THICKFRAME
	    WS_TILED
	    WS_TILEDWINDOW
	    WS_VISIBLE
	    WS_VSCROLL
	    WaitMessage
	    WinExec
	    WinHelp
	    WindowFromPoint_ptr
	    WritePrivateProfileString
	    WriteProfileString
	    XCLASS_BOOL
	    XCLASS_DATA
	    XCLASS_FLAGS
	    XCLASS_MASK
	    XCLASS_NOTIFICATION
	    XST_ADVACKRCVD
	    XST_ADVDATAACKRCVD
	    XST_ADVDATASENT
	    XST_ADVSENT
	    XST_CONNECTED
	    XST_DATARCVD
	    XST_EXECACKRCVD
	    XST_EXECSENT
	    XST_INCOMPLETE
	    XST_INIT1
	    XST_INIT2
	    XST_NULL
	    XST_POKEACKRCVD
	    XST_POKESENT
	    XST_REQSENT
	    XST_UNADVACKRCVD
	    XST_UNADVSENT
	    XTYPF_ACKREQ
	    XTYPF_NOBLOCK
	    XTYPF_NODATA
	    XTYP_ADVDATA
	    XTYP_ADVREQ
	    XTYP_ADVSTART
	    XTYP_ADVSTOP
	    XTYP_CONNECT
	    XTYP_CONNECT_CONFIRM
	    XTYP_DISCONNECT
	    XTYP_ERROR
	    XTYP_EXECUTE
	    XTYP_MASK
	    XTYP_POKE
	    XTYP_REGISTER
	    XTYP_REQUEST
	    XTYP_SHIFT
	    XTYP_UNREGISTER
	    XTYP_WILDCONNECT
	    XTYP_XACT_COMPLETE
	    _lclose
	    _lcreat
	    _llseek
	    _lopen
	    _lread
	    _lwrite
	    atom
	    bi_bitfields
	    bitmap
	    bitmapinfo
	    bitmapinfoheader
	    bool
	    byte
	    cc_fullopen
	    cc_preventfullopen
	    cc_rgbinit
	    char
	    charrange
	    choosecolor
	    choosefont
	    colorref
	    convcontext
	    convinfo
	    dcb
	    devmode
	    dlgproc
	    docinfo
	    drawitemstruct
	    dword
	    farproc
	    fi-choosefont
	    fi-openfilename
	    filetime
	    findtextex
	    fontenumproc
	    get_user_name
	    gobjenumproc
 #-(version>= 5 0)
	    handle
	    handletable
	    hbitmap
	    hbrush
	    hconv
	    hconvlist
	    hcursor
	    hdc
	    hddedata
	    hfont
	    hicon
	    hinstance
	    hmenu
	    hookproc
	    hpalette
	    hpen
	    hrgn
	    hsz
	    hszpair
	    hwnd
	    imageinfo
 #-(version>= 5 0)
	    int
	    ipc_accept_connection
	    ipc_closesocket
	    ipc_data_available
	    ipc_event_donep
	    ipc_get_last_error
	    ipc_get_socket_ipaddr
	    ipc_get_socket_peer_ipaddr
	    ipc_get_socket_peer_port
	    ipc_get_socket_port
	    ipc_inet_dgram_socket
	    ipc_inet_receive_from
	    ipc_inet_send_to
	    ipc_inet_socket_connect
	    ipc_inet_socket_passive
	    ipc_initialize
	    ipc_ipaddr_to_name_start
	    ipc_locate_port
	    ipc_lookup_hostname
 #-(version>= 5 0)
	    lhandle
	    logbrush
	    logfont
	    logpalette
	    logpen
	    long
	    lparam
	    lpbitmap
	    lpbitmapinfoheader
	    lpchoosefont
	    lpcolorref
	    lpcstr
	    lpctstr
	    lpdcb
	    lpdevmode
	    lpdi
	    lpdword
	    lphandletable
	    lpint
	    lplogbrush
	    lplogfont
	    lplogpalette
	    lplogpen
	    lpmetarecord
	    lpmsg
	    lpnonclientmetrics
	    lpofstruct
	    lpopenfilename
	    lppaintstruct
	    lppaletteentry
	    lppoint
	    lprect
	    lpsize
	    lpstr
	    lpsystem_info
	    lptextmetric
	    lptstr
	    lpvoid
	    lpwin32_find_dataa
	    lpwndclass
	    lpwndclassex
	    lresult
	    lstrcat
	    lstrcmp
	    lstrcmpi
	    lstrcpy
	    lstrlen
	    lv_column
	    lv_dispinfo
	    lv_findinfo
	    lv_hittestinfo
	    lv_item
	    lv_keydown
	    mciGetErrorString
	    mciSendCommand
	    mci_anim_open_parms
	    mci_anim_play_parms
	    mci_anim_step_parms
	    mci_generic_parms
	    mci_getdevcaps_parms
	    mci_open_parms
	    mci_ovly_open_parms
	    mci_ovly_save_parms
	    mci_play_parms
	    mci_record_parms
	    mci_save_parms
	    mci_seek_parms
	    mci_seq_set_parms
	    mci_set_parms
	    mci_status_parms
	    mci_sysinfo_parms
	    mci_vd_play_parms
	    mci_vd_step_parms
	    mci_wave_open_parms
	    mci_wave_set_parms
	    memory_status_dump
	    metarecord
	    mfenumproc
	    minmaxinfo
	    mmtime_milliseconds
	    mmtime_smpte
	    msg
	    nm_listview
	    nm_updown
	    nmhdr
	    nonclientmetrics
	    ofstruct
	    openfilename
	    pagesetupdlg
	    paintstruct
	    paintstruct
	    paletteentry
	    pconvcontext
	    pconvinfo
	    phszpair
	    point
	    printdlg
	    propenumproc
	    rect
	    rgbquad
	    security_context_tracking_mode
	    security_impersonation_level
	    security_quality_of_service
	    shandle
	    short
 #-(version>= 5 0)
	    size
	    str_RegisterClass
	    str_RegisterClassEx
	    system_info
	    tc_hittestinfo
	    tc_item
	    tc_itemheader
	    tc_keydown
	    tchar
	    textmetric
	    textrange
	    timecaps
	    timerproc
	    ts_ChooseColor
	    ts_ChooseFont
	    ts_GetOpenFileName
	    ts_GetSaveFileName
	    ts_PrintDlg
	    udaccel
	    uint
	    void
	    win32_find_dataa
	    wndclass
	    wndclassex
	    wndenumproc
	    wndproc
	    word
	    wparam
	    wvsprintf)
	  'windows))

#+ignore
(eval-when (compile load eval)
  (export '(WINDOWS::SENDMESSAGE
	    WINDOWS::SWP_NOACTIVATE
	    WINDOWS::SWP_NOZORDER
	    WINDOWS::LB_SELITEMRANGE
	    WINDOWS::LB_SETCURSEL
	    WINDOWS::WM_NULL
	    WINDOWS::CBS_DROPDOWNLIST
	    WINDOWS::CB_INSERTSTRING
	    WINDOWS::CB_SETCURSEL
	    WINDOWS::LBS_NOINTEGRALHEIGHT
	    )
	  (find-package :win)))



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

;; Try to keep these alphabetical.

(eval-when (load compile eval)
  (load "user32.dll"))

(defsystemcall 'appendmenu
    :entry-point "AppendMenuA"
    :arguments '(t t t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'arc
    :entry-point "Arc"
    :arguments '(t t t t t t t t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'bitblt
    :entry-point "BitBlt"
    :arguments '(t t t t t t t t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'callwindowproc
    :entry-point "CallWindowProcA"
    :arguments '(t t t t t)
    :return-type :integer
    :arg-checking nil)

(defsystemcall 'clienttoscreen
    :entry-point "ClientToScreen"
    :arguments '(t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'COMMDLGEXTENDEDERROR
    :entry-point "CommDlgExtendedError"
    :arguments '()
    :return-type :integer
    :arg-checking nil)

(defsystemcall 'createbitmap
    :entry-point "CreateBitmap"
    :arguments '(t t t t t)
    :return-type :unsigned-integer
    :arg-checking nil)

(defsystemcall 'createcompatiblebitmap
    :entry-point "CreateCompatibleBitmap"
    :arguments '(t t t)
    :return-type :unsigned-integer
    :arg-checking nil)

(defsystemcall 'createcompatibledc
    :entry-point "CreateCompatibleDC"
    :arguments '(t)
    :return-type :unsigned-integer
    :arg-checking nil)

(defsystemcall 'createdc
    :entry-point "CreateDCA"
    :arguments '(t t t t)
    :return-type :unsigned-integer
    :arg-checking nil)

(defsystemcall 'createdibitmap
    :entry-point "CreateDIBitmap"
    :arguments '(t t t t t t)
    :return-type :unsigned-integer
    :arg-checking nil)

(defsystemcall 'createfont
    :entry-point "CreateFontA"
    :arguments '(t t t t t t t t t t t t t t) 
    :return-type :unsigned-integer
    :arg-checking nil)

(defsystemcall 'createicon
    :entry-point "CreateIcon"
    :arguments '(t t t t t t t)
    :return-type :integer
    :arg-checking nil)

(defsystemcall 'createmenu
    :entry-point "CreateMenu"
    :arguments '()
    :return-type :unsigned-integer
    :arg-checking nil)

(defsystemcall 'createpatternbrush
    :entry-point "CreatePatternBrush"
    :arguments '(t) 
    :return-type :unsigned-integer
    :arg-checking nil)

(defsystemcall 'createpen
    :entry-point "CreatePen"
    :arguments '(integer integer integer)
    :return-type :integer
    :arg-checking nil
    :call-direct t)

(defsystemcall 'createpopupmenu
    :entry-point "CreatePopupMenu"
    :arguments '()
    :return-type :unsigned-integer
    :arg-checking nil
    :call-direct t)

(defsystemcall 'createrectrgn
    :entry-point "CreateRectRgn"
    :arguments '(integer integer integer integer)
    :return-type :integer
    :arg-checking nil
    :call-direct t)

(defsystemcall 'createsolidbrush
    :entry-point "CreateSolidBrush"
    :arguments '(integer)
    :return-type :integer
    :arg-checking nil
    :call-direct t)

(defsystemcall 'createwindow
    :entry-point "CreateWindowA"
    :arguments '(t t t t t t t t t t t)
    :return-type :unsigned-integer
    :arg-checking nil)

(defsystemcall 'createwindowex
    :entry-point "CreateWindowExA"
    :arguments '(t t t t t t t t t t t t)
    :return-type :unsigned-integer
    :arg-checking nil)

(defsystemcall 'defwindowproc
    :entry-point "DefWindowProcA"
    :arguments '(t t t t)
    :return-type :integer
    :arg-checking nil)

(defsystemcall 'deleteobject
    :entry-point "DeleteObject"
    :arguments '(integer)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'deletedc
    :entry-point "DeleteDC"
    :arguments '(t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'destroymenu
    :entry-point "DestroyMenu"
    :arguments '(t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'destroywindow
    :entry-point "DestroyWindow"
    :arguments '(t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'dispatchmessage
    :entry-point "DispatchMessageA"
    :arguments '(t)
    :return-type :integer
    :arg-checking nil)

(defsystemcall 'drawicon
    :entry-point "DrawIcon"
    :arguments '(t t t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'ellipse
    :entry-point "Ellipse"
    :arguments '(t t t t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'enablemenuitem
    :entry-point "EnableMenuItem"
    :arguments '(t t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'enablewindow
    :entry-point "EnableWindow"
    :arguments '(t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'getactivewindow
    :entry-point "GetActiveWindow"
    :arguments '()
    :return-type :unsigned-integer
    :arg-checking nil
    :call-direct t)

(defsystemcall 'getclientrect
    :entry-point "GetClientRect"
    :arguments '(t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'getcursorpos
    :entry-point "GetCursorPos"
    :arguments '(t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'getdc
    :entry-point "GetDC"
    :arguments '(integer)
    :return-type :integer
    :arg-checking nil
    :call-direct t)

(defsystemcall 'getdevicecaps
    :entry-point "GetDeviceCaps"
    :arguments '(t t)
    :return-type :integer
    :arg-checking nil)

(defsystemcall 'getkeystate
    :entry-point "GetKeyState"
    :arguments '(t)
    :return-type :integer
    :arg-checking nil)

(defsystemcall 'getlasterror
    :entry-point "GetLastError"
    :arguments '()
    :return-type :integer
    :arg-checking nil
    :call-direct t)

(defsystemcall 'getopenfilename
    :entry-point "GetOpenFileNameA"
    :arguments '(integer)
    :return-type :boolean)

(defsystemcall 'getmenu
    :entry-point "GetMenu"
    :arguments '(t)
    :return-type :unsigned-integer
    :arg-checking nil)

(defsystemcall 'getmessage
    :entry-point "GetMessageA"
    :arguments '(t t t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'getsavefilename
    :entry-point "GetSaveFileNameA"
    :arguments '(integer)
    :return-type :boolean)

(defsystemcall 'getscrollpos
    :entry-point "GetScrollPos"
    :arguments '(t t)
    :return-type :integer
    :arg-checking nil)

(defsystemcall 'getstockobject
    :entry-point "GetStockObject"
    :arguments '(t)
    :return-type :unsigned-integer
    :arg-checking nil)

(defsystemcall 'getsyscolor
    :entry-point "GetSysColor"
    :arguments '(t)
    :return-type :integer
    :arg-checking nil)

(defsystemcall 'getsystemmetrics
    :entry-point "GetSystemMetrics"
    :arguments '(t)
    :return-type :integer
    :arg-checking nil)

(defsystemcall 'gettextextentpoint
    :entry-point "GetTextExtentPointA"
    :arguments '(t t t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'gettextmetrics
    :entry-point "GetTextMetricsA"
    :arguments '(t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'getupdaterect
    :entry-point "GetUpdateRect"
    :arguments '(t t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'getversion
    :entry-point "GetVersion"
    :arguments '()
    :return-type :unsigned-integer
    :arg-checking nil
    :call-direct t)

(defsystemcall 'getwindowlong
    :entry-point "GetWindowLongA"
    :arguments '(t t)
    :return-type :integer
    :arg-checking nil)

(defsystemcall 'getwindowrect
    :entry-point "GetWindowRect"
    :arguments '(t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'getwindowtext
    :entry-point "GetWindowTextA"
    :arguments '(t t t)
    :return-type :integer
    :arg-checking nil)

(defsystemcall 'GETWINMAINARGS
    :ENTRY-POINT 
  #+allegro-v4.3 '(:INDEX "GetWinMainArgs")
  #-allegro-v4.3 "GetWinMainArgs"
    :ARGUMENTS '(T)
    :RETURN-TYPE :INTEGER)

(defsystemcall 'globalalloc
    :entry-point "GlobalAlloc"
    :arguments '(t t)
    :return-type :integer
    :arg-checking nil)

(defsystemcall 'globalfree
    :entry-point "GlobalFree"
    :arguments '(t)
    :return-type :integer
    :arg-checking nil)

(defsystemcall 'globallock
    :entry-point "GlobalLock"
    :arguments '(t)
    :return-type :void
    :arg-checking nil)

(defsystemcall 'globalunlock
    :entry-point "GlobalUnlock"
    :arguments '(t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'invalidaterect
    :entry-point "InvalidateRect"
    :arguments '(t t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'isiconic
    :entry-point "IsIconic"
    :arguments '(t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'iswindow
    :entry-point "IsWindow"
    :arguments '(t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'iswindowvisible
    :entry-point "IsWindowVisible"
    :arguments '(t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'lineto
    :entry-point "LineTo"
    :arguments '(integer fixnum fixnum)
    :return-type :boolean
    :arg-checking nil
    :call-direct t)

(defsystemcall 'loadcursor
    :entry-point "LoadCursorA"
    :arguments '(t t)
    :return-type :unsigned-integer
    :arg-checking nil)

(defsystemcall 'loadicon
    :entry-point "LoadIconA"
    :arguments '(t t)
    :return-type :unsigned-integer
    :arg-checking nil)

(defsystemcall 'messagebeep
    :entry-point "MessageBeep"
    :arguments '(t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'messagebox
    :entry-point "MessageBoxA"
    :arguments '(integer integer integer integer)
    :return-type :integer
    :arg-checking nil)

(defsystemcall 'movetoex
    :entry-point "MoveToEx"
    :arguments '(integer integer integer integer)
    :return-type :boolean
    :arg-checking nil
    :call-direct t)

(defsystemcall 'peekmessage
    :entry-point "PeekMessageA"
    :arguments '(t t t t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'pie
    :entry-point "Pie"
    :arguments '(t t t t t t t t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'polygon
    :entry-point "Polygon"
    :arguments '(t t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'polyline
    :entry-point "Polyline"
    :arguments '(t t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'rectangle
    :entry-point "Rectangle"
    :arguments '(t t t t t) 
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'registerclass
    :entry-point "RegisterClassA"
    :arguments '(t) 
    :return-type :integer
    :arg-checking nil)

(defsystemcall 'registerclassex
    :ENTRY-POINT "RegisterClassExA"
    :ARGUMENTS '(T)
    :RETURN-TYPE :INTEGER
    :arg-checking nil)

(defsystemcall 'releasedc
    :entry-point "ReleaseDC"
    :arguments '(integer fixnum)
    :return-type :integer
    :arg-checking nil
    :call-direct t)

(defsystemcall 'screentoclient
    :entry-point "ScreenToClient"
    :arguments '(t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'scrolldc
    :entry-point "ScrollDC"
    :arguments '(t t t t t t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'selectobject
    :entry-point "SelectObject"
    :arguments '(integer integer)
    :return-type :integer
    :arg-checking nil
    :call-direct t)

(defsystemcall 'sendmessage
    :entry-point "SendMessageA"
    :arguments '(t t t t)
    :return-type :integer
    :arg-checking nil)

(defsystemcall 'setbkcolor
    :entry-point "SetBkColor"
    :arguments '(t t)
    :return-type :unsigned-integer
    :arg-checking nil)

(defsystemcall 'setbkmode
    :entry-point "SetBkMode"
    :arguments '(t t)
    :return-type :integer
    :arg-checking nil)

(defsystemcall 'setcursor
    :entry-point "SetCursor"
    :arguments '(t)
    :return-type :unsigned-integer
    :arg-checking nil)

(defsystemcall 'setcursorpos
    :entry-point "SetCursorPos"
    :arguments '(t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'setfocus
    :entry-point "SetFocus"
    :arguments '(t)
    :return-type :unsigned-integer
    :arg-checking nil)

(defsystemcall 'setforegroundwindow
    :entry-point "SetForegroundWindow"
    :arguments '(integer)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'setscrollpos
    :entry-point "SetScrollPos"
    :arguments '(t t t t)
    :return-type :integer
    :arg-checking nil)

(defsystemcall 'setscrollrange
    :entry-point "SetScrollRange"
    :arguments '(t t t t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'setsyscolors
    :entry-point "SetSysColors"
    :arguments '(t t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'settextcolor
    :entry-point "SetTextColor"
    :arguments '(t t)
    :return-type :unsigned-integer
    :arg-checking nil)

(defsystemcall 'setwindowlong
    :entry-point "SetWindowLongA"
    :arguments ' (t t t) 
    :return-type :integer
    :arg-checking nil)

(defsystemcall 'setwindowpos
    :entry-point "SetWindowPos"
    :arguments ' (t t t t t t t) 
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'setwindowtext
    :entry-point "SetWindowTextA"
    :arguments ' (t t) 
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'setrop2
    :entry-point "SetROP2"
    :arguments '(integer integer)
    :return-type :integer
    :arg-checking nil
    :call-direct t)

(defsystemcall 'showwindow
    :entry-point "ShowWindow"
    :arguments '(t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'STR_REGISTERCLASSEX
    :ENTRY-POINT 
  #+allegro-v4.3 '(:INDEX "str_RegisterClassEx")
  #-allegro-v4.3 "str_RegisterClassEx"
  :ARGUMENTS '(T T T)
  :RETURN-TYPE :INTEGER)

(defsystemcall 'stretchdibits
    :entry-point "StretchDIBits"
    :arguments '(t t t t t t t t t t t t t)
    :return-type :integer)

(defsystemcall 'textout
    :entry-point "TextOutA"
    :arguments '(t t t t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'trackpopupmenu
    :entry-point "TrackPopupMenu"
    :arguments '(t t t t t t t)
    :return-type :integer
    :arg-checking nil)

(defsystemcall 'translatemessage
    :entry-point "TranslateMessage"
    :arguments '(t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'updatewindow
    :entry-point "UpdateWindow"
    :arguments '(t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'validaterect
    :entry-point "ValidateRect"
    :arguments '(t t)
    :return-type :boolean
    :arg-checking nil)

(defsystemcall 'vkkeyscan
    :entry-point "VkKeyScanA"
    :arguments '(t)
    :return-type :integer
    :arg-checking nil)



(defconstant FALSE 0)
(defconstant TRUE 1)
(defconstant OF_READ 0)
(defconstant OF_WRITE 1)
(defconstant OF_READWRITE 2)
(defconstant OF_SHARE_COMPAT 0)
(defconstant OF_SHARE_EXCLUSIVE 16)
;;				#x10

(defconstant OF_SHARE_DENY_WRITE 32)
;;				#x20

(defconstant OF_SHARE_DENY_READ 48)
;;				#x30

(defconstant OF_SHARE_DENY_NONE 64)
;;				#x40

(defconstant OF_PARSE 256)
;;				#x100

(defconstant OF_DELETE 512)
;;				#x200

(defconstant OF_VERIFY 1024)
;;				#x400

(defconstant OF_CANCEL 2048)
;;				#x800

(defconstant OF_CREATE 4096)
;;				#x1000

(defconstant OF_PROMPT 8192)
;;				#x2000

(defconstant OF_EXIST 16384)
;;				#x4000

(defconstant OF_REOPEN 32768)
;;				#x8000

;(defconstant TF_FORCEDRIVE 128)
;;				#x80

(defconstant DRIVE_REMOVABLE 2)
(defconstant DRIVE_FIXED 3)
(defconstant DRIVE_REMOTE 4)
(defconstant GMEM_FIXED 0)
(defconstant GMEM_MOVEABLE 2)
(defconstant GMEM_NOCOMPACT 16)
;;				#x10

(defconstant GMEM_NODISCARD 32)
;;				#x20

(defconstant GMEM_ZEROINIT 64)
;;				#x40

(defconstant GMEM_MODIFY 128)
;;				#x80

(defconstant GMEM_DISCARDABLE 256)
;;				#x100

(defconstant GMEM_NOT_BANKED 4096)
;;				#x1000

(defconstant GMEM_SHARE 8192)
;;				#x2000

(defconstant GMEM_DDESHARE 8192)
;;				#x2000

(defconstant GMEM_NOTIFY 16384)
;;				#x4000

(defconstant GMEM_LOWER 4096)
;;				#x1000

(defconstant GHND 66)
;;				#x42

(defconstant GPTR 64)
;;				#x40

(defconstant GMEM_DISCARDED 16384)
;;				#x4000

(defconstant GMEM_LOCKCOUNT 255)
;;				#xff

(defconstant LMEM_FIXED 0)
(defconstant LMEM_MOVEABLE 2)
(defconstant LMEM_NOCOMPACT 16)
;;				#x10

(defconstant LMEM_NODISCARD 32)
;;				#x20

(defconstant LMEM_ZEROINIT 64)
;;				#x40


(defconstant LMEM_MODIFY 128)
;;				#x80

(defconstant LMEM_DISCARDABLE 3840)
;;				#xf00

(defconstant LHND 66)
;;				#x42

(defconstant LPTR 64)
;;				#x40

(defconstant NONZEROLHND 2)
(defconstant NONZEROLPTR 0)
;(defconstant LNOTIFY_OUTOFMEM 0)
;(defconstant LNOTIFY_MOVE 1)
;(defconstant LNOTIFY_DISCARD 2)
(defconstant LMEM_DISCARDED 16384)
;;				#x4000

(defconstant LMEM_LOCKCOUNT 255)
;;				#xff

#+aclpc
(defconstant RT_CURSOR 1)
#+aclpc
(defconstant RT_BITMAP 2)
#+aclpc
(defconstant RT_ICON 3)
#+aclpc
(defconstant RT_MENU 4)
#+aclpc
(defconstant RT_DIALOG 5)
#+aclpc
(defconstant RT_STRING 6)
#+aclpc
(defconstant RT_FONTDIR 7)
#+aclpc
(defconstant RT_FONT 8)
#+aclpc
(defconstant RT_ACCELERATOR 9)
#+aclpc
(defconstant RT_RCDATA 10)
;;				#xa

;(defconstant WF_PMODE 1)
;(defconstant WF_CPU286 2)
;(defconstant WF_CPU386 4)
;(defconstant WF_CPU486 8)
;(defconstant WF_STANDARD 16)
;;				#x10

;(defconstant WF_WIN286 16)
;;				#x10

;(defconstant WF_ENHANCED 32)
;;				#x20

;(defconstant WF_WIN386 32)
;;				#x20

;(defconstant WF_CPU086 64)
;;				#x40

;(defconstant WF_CPU186 128)
;;				#x80

;(defconstant WF_LARGEFRAME 256)
;;				#x100

;(defconstant WF_SMALLFRAME 512)
;;				#x200

;(defconstant WF_80X87 1024)
;;				#x400

;(defconstant WEP_SYSTEM_EXIT 1)
;(defconstant WEP_FREE_DLL 0)
(defconstant R2_BLACK 1)
(defconstant R2_NOTMERGEPEN 2)
(defconstant R2_MASKNOTPEN 3)
(defconstant R2_NOTCOPYPEN 4)
(defconstant R2_MASKPENNOT 5)
(defconstant R2_NOT 6)
(defconstant R2_XORPEN 7)
(defconstant R2_NOTMASKPEN 8)
(defconstant R2_MASKPEN 9)
(defconstant R2_NOTXORPEN 10)
;;				#xa

(defconstant R2_NOP 11)
;;				#xb

(defconstant R2_MERGENOTPEN 12)
;;				#xc

(defconstant R2_COPYPEN 13)
;;				#xd

(defconstant R2_MERGEPENNOT 14)
;;				#xe

(defconstant R2_MERGEPEN 15)
;;				#xf

(defconstant R2_WHITE 16)
;;				#x10

(defconstant SRCCOPY 13369376) 

(defconstant SRCPAINT 15597702) 

(defconstant SRCAND 8913094) 

(defconstant SRCINVERT 6684742) 

(defconstant SRCERASE 4457256) 

(defconstant NOTSRCCOPY 3342344) 

(defconstant NOTSRCERASE 1114278) 

(defconstant MERGECOPY 12583114) 

(defconstant MERGEPAINT 12255782) 

(defconstant PATCOPY 15728673) 

(defconstant PATPAINT 16452105) 

(defconstant PATINVERT 5898313) 

(defconstant DSTINVERT 5570569) 

(defconstant BLACKNESS 66)
;;				#x42

(defconstant WHITENESS 16711778) 

(defconstant BLACKONWHITE 1)
(defconstant WHITEONBLACK 2)
(defconstant COLORONCOLOR 3)
(defconstant ALTERNATE 1)
(defconstant WINDING 2)
(defconstant TA_NOUPDATECP 0)
(defconstant TA_UPDATECP 1)
(defconstant TA_LEFT 0)
(defconstant TA_RIGHT 2)
(defconstant TA_CENTER 6)
(defconstant TA_TOP 0)
(defconstant TA_BOTTOM 8)
(defconstant TA_BASELINE 24)
;;				#x18

(defconstant ETO_GRAYED 1)
(defconstant ETO_OPAQUE 2)
(defconstant ETO_CLIPPED 4)
(defconstant ASPECT_FILTERING 1)
(defconstant META_SETBKCOLOR 513)
;;				#x201

(defconstant META_SETBKMODE 258)
;;				#x102

(defconstant META_SETMAPMODE 259)
;;				#x103

(defconstant META_SETROP2 260)
;;				#x104

(defconstant META_SETRELABS 261)
;;				#x105

(defconstant META_SETPOLYFILLMODE 262)
;;				#x106

(defconstant META_SETSTRETCHBLTMODE 263)
;;				#x107

(defconstant META_SETTEXTCHAREXTRA 264)
;;				#x108

(defconstant META_SETTEXTCOLOR 521)
;;				#x209

(defconstant META_SETTEXTJUSTIFICATION 522)
;;				#x20a

(defconstant META_SETWINDOWORG 523)
;;				#x20b

(defconstant META_SETWINDOWEXT 524)
;;				#x20c

(defconstant META_SETVIEWPORTORG 525)
;;				#x20d

(defconstant META_SETVIEWPORTEXT 526)
;;				#x20e

(defconstant META_OFFSETWINDOWORG 527)
;;				#x20f

(defconstant META_SCALEWINDOWEXT 1040)	; was 1024
;;					#x410

(defconstant META_OFFSETVIEWPORTORG 529)
;;				#x211

(defconstant META_SCALEVIEWPORTEXT 1042)
;;				#x412

(defconstant META_LINETO 531)
;;				#x213

(defconstant META_MOVETO 532)
;;				#x214

(defconstant META_EXCLUDECLIPRECT 1045)
;;				#x415

(defconstant META_INTERSECTCLIPRECT 1046)
;;				#x416

(defconstant META_ARC 2071)
;;				#x817

(defconstant META_ELLIPSE 1048)
;;				#x418

(defconstant META_FLOODFILL 1049)
;;				#x419

(defconstant META_PIE 2074)
;;				#x81a

(defconstant META_RECTANGLE 1051)
;;				#x41b

(defconstant META_ROUNDRECT 1564)
;;				#x61c

(defconstant META_PATBLT 1565)
;;				#x61d

(defconstant META_SAVEDC 30)
;;				#x1e

(defconstant META_SETPIXEL 1055)
;;				#x41f

(defconstant META_OFFSETCLIPRGN 544)
;;				#x220

(defconstant META_TEXTOUT 1313)
;;				#x521

(defconstant META_BITBLT 2338)
;;				#x922

(defconstant META_STRETCHBLT 2851)
;;				#xb23

(defconstant META_POLYGON 804)
;;				#x324

(defconstant META_POLYLINE 805)
;;				#x325

(defconstant META_ESCAPE 1574)
;;				#x626

(defconstant META_RESTOREDC 295)
;;				#x127

(defconstant META_FILLREGION 552)
;;				#x228

(defconstant META_FRAMEREGION 1065)
;;				#x429

(defconstant META_INVERTREGION 298)
;;				#x12a

(defconstant META_PAINTREGION 299)
;;				#x12b

(defconstant META_SELECTCLIPREGION 300)
;;				#x12c

(defconstant META_SELECTOBJECT 301)
;;				#x12d

(defconstant META_SETTEXTALIGN 302)
;;				#x12e

;(defconstant META_DRAWTEXT 1583)
;;				#x62f

(defconstant META_CHORD 2096)
;;				#x830

(defconstant META_SETMAPPERFLAGS 561)
;;				#x231

(defconstant META_EXTTEXTOUT 2610)
;;				#xa32

(defconstant META_SETDIBTODEV 3379)
;;				#xd33

(defconstant META_SELECTPALETTE 564)
;;				#x234

(defconstant META_REALIZEPALETTE 53)
;;				#x35

(defconstant META_ANIMATEPALETTE 1078)
;;				#x436

(defconstant META_SETPALENTRIES 55)
;;				#x37

(defconstant META_POLYPOLYGON 1336)
;;				#x538

(defconstant META_RESIZEPALETTE 313)
;;				#x139

(defconstant META_DIBBITBLT 2368)
;;				#x940

(defconstant META_DIBSTRETCHBLT 2881)
;;				#xb41

(defconstant META_DIBCREATEPATTERNBRUSH 322)
;;				#x142

(defconstant META_STRETCHDIB 3907)
;;				#xf43

(defconstant META_DELETEOBJECT 496)
;;				#x1f0

(defconstant META_CREATEPALETTE 247)
;;				#xf7

;(defconstant META_CREATEBRUSH 248)
;;				#xf8

(defconstant META_CREATEPATTERNBRUSH 505)
;;				#x1f9

(defconstant META_CREATEPENINDIRECT 762)
;;				#x2fa

(defconstant META_CREATEFONTINDIRECT 763)
;;				#x2fb

(defconstant META_CREATEBRUSHINDIRECT 764)
;;				#x2fc

;(defconstant META_CREATEBITMAPINDIRECT 765)
;;				#x2fd

;(defconstant META_CREATEBITMAP 1790)
;;				#x6fe

(defconstant META_CREATEREGION 1791)
;;				#x6ff

(defconstant NEWFRAME 1)
(defconstant ABORTDOC 2)
(defconstant NEXTBAND 3)
(defconstant SETCOLORTABLE 4)
(defconstant GETCOLORTABLE 5)
(defconstant FLUSHOUTPUT 6)
(defconstant DRAFTMODE 7)
(defconstant QUERYESCSUPPORT 8)
(defconstant SETABORTPROC 9)
(defconstant STARTDOC 10)
;;				#xa

(defconstant ENDDOC 11)
;;				#xb

(defconstant GETPHYSPAGESIZE 12)
;;				#xc

(defconstant GETPRINTINGOFFSET 13)
;;				#xd

(defconstant GETSCALINGFACTOR 14)
;;				#xe

(defconstant MFCOMMENT 15)
;;				#xf

(defconstant GETPENWIDTH 16)
;;				#x10

(defconstant SETCOPYCOUNT 17)
;;				#x11

(defconstant SELECTPAPERSOURCE 18)
;;				#x12

(defconstant DEVICEDATA 19)
;;				#x13

(defconstant PASSTHROUGH 19)
;;				#x13

(defconstant GETTECHNOLGY 20)
;;				#x14

(defconstant GETTECHNOLOGY 20)
;;				#x14

;(defconstant SETENDCAP 21)
;;				#x15

(defconstant SETLINEJOIN 22)
;;				#x16

(defconstant SETMITERLIMIT 23)
;;				#x17

(defconstant BANDINFO 24)
;;				#x18

(defconstant DRAWPATTERNRECT 25)
;;				#x19

(defconstant GETVECTORPENSIZE 26)
;;				#x1a

(defconstant GETVECTORBRUSHSIZE 27)
;;				#x1b

(defconstant ENABLEDUPLEX 28)
;;				#x1c

(defconstant GETSETPAPERBINS 29)
;;				#x1d

(defconstant GETSETPRINTORIENT 30)
;;				#x1e

(defconstant ENUMPAPERBINS 31)
;;				#x1f

(defconstant SETDIBSCALING 32)
;;				#x20

(defconstant EPSPRINTING 33)
;;				#x21

(defconstant ENUMPAPERMETRICS 34)
;;				#x22

(defconstant GETSETPAPERMETRICS 35)
;;				#x23

(defconstant POSTSCRIPT_DATA 37)
;;				#x25

(defconstant POSTSCRIPT_IGNORE 38)
;;				#x26

(defconstant GETEXTENDEDTEXTMETRICS 256)
;;				#x100

(defconstant GETEXTENTTABLE 257)
;;				#x101

(defconstant GETPAIRKERNTABLE 258)
;;				#x102

(defconstant GETTRACKKERNTABLE 259)
;;				#x103

(defconstant EXTTEXTOUT 512)
;;				#x200

(defconstant ENABLERELATIVEWIDTHS 768)
;;				#x300

(defconstant ENABLEPAIRKERNING 769)
;;				#x301

(defconstant SETKERNTRACK 770)
;;				#x302

(defconstant SETALLJUSTVALUES 771)
;;				#x303

(defconstant SETCHARSET 772)
;;				#x304

(defconstant STRETCHBLT 2048)
;;				#x800

(defconstant BEGIN_PATH 4096)
;;				#x1000

(defconstant CLIP_TO_PATH 4097)
;;				#x1001

(defconstant END_PATH 4098)
;;				#x1002

(defconstant EXT_DEVICE_CAPS 4099)
;;				#x1003

(defconstant RESTORE_CTM 4100)
;;				#x1004

(defconstant SAVE_CTM 4101)
;;				#x1005

(defconstant SET_ARC_DIRECTION 4102)
;;				#x1006

(defconstant SET_BACKGROUND_COLOR 4103)
;;				#x1007

(defconstant SET_POLY_MODE 4104)
;;				#x1008

(defconstant SET_SCREEN_ANGLE 4105)
;;				#x1009

(defconstant SET_SPREAD 4106)
;;				#x100a

(defconstant TRANSFORM_CTM 4107)
;;				#x100b

(defconstant SET_CLIP_BOX 4108)
;;				#x100c

(defconstant SET_BOUNDS 4109)
;;				#x100d

(defconstant SET_MIRROR_MODE 4110)
;;				#x100e

(defconstant SP_NOTREPORTED 16384)
;;				#x4000

(defconstant SP_ERROR -1)
;;				#xffffffff

(defconstant SP_APPABORT -2)
;;				#xfffffffe

(defconstant SP_USERABORT -3)
;;				#xfffffffd

(defconstant SP_OUTOFDISK -4)
;;				#xfffffffc

(defconstant SP_OUTOFMEMORY -5)
;;				#xfffffffb

(defconstant PR_JOBSTATUS 0)
(defconstant OBJ_PEN 1)
(defconstant OBJ_BRUSH 2)
(defconstant BI_RGB 0)
(defconstant BI_RLE8 1)
(defconstant BI_RLE4 2)
(defconstant LF_FACESIZE 32)
;;				#x20

(defconstant OUT_DEFAULT_PRECIS 0)
(defconstant OUT_STRING_PRECIS 1)
(defconstant OUT_CHARACTER_PRECIS 2)
(defconstant OUT_STROKE_PRECIS 3)
(defconstant CLIP_DEFAULT_PRECIS 0)
(defconstant CLIP_CHARACTER_PRECIS 1)
(defconstant CLIP_STROKE_PRECIS 2)
(defconstant DEFAULT_QUALITY 0)
(defconstant DRAFT_QUALITY 1)
(defconstant PROOF_QUALITY 2)
(defconstant DEFAULT_PITCH 0)
(defconstant FIXED_PITCH 1)
(defconstant VARIABLE_PITCH 2)
(defconstant ANSI_CHARSET 0)
(defconstant SYMBOL_CHARSET 2)
(defconstant SHIFTJIS_CHARSET 128)
;;				#x80

(defconstant OEM_CHARSET 255)
;;				#xff

(defconstant FF_DONTCARE 0)
(defconstant FF_ROMAN 16)
;;				#x10

(defconstant FF_SWISS 32)
;;				#x20

(defconstant FF_MODERN 48)
;;				#x30

(defconstant FF_SCRIPT 64)
;;				#x40

(defconstant FF_DECORATIVE 80)
;;				#x50

(defconstant FW_DONTCARE 0)
(defconstant FW_THIN 100)
;;				#x64

(defconstant FW_EXTRALIGHT 200)
;;				#xc8

(defconstant FW_LIGHT 300)
;;				#x12c

(defconstant FW_NORMAL 400)
;;				#x190

(defconstant FW_MEDIUM 500)
;;				#x1f4

(defconstant FW_SEMIBOLD 600)
;;				#x258

(defconstant FW_BOLD 700)
;;				#x2bc

(defconstant FW_EXTRABOLD 800)
;;				#x320

(defconstant FW_HEAVY 900)
;;				#x384

(defconstant FW_ULTRALIGHT 200)
;;				#xc8

(defconstant FW_REGULAR 400)
;;				#x190

(defconstant FW_DEMIBOLD 600)
;;				#x258

(defconstant FW_ULTRABOLD 800)
;;				#x320

(defconstant FW_BLACK 900)
;;				#x384

(defconstant RASTER_FONTTYPE 1)
(defconstant DEVICE_FONTTYPE 2)
(defconstant TRANSPARENT 1)
(defconstant OPAQUE 2)
(defconstant MM_TEXT 1)
(defconstant MM_LOMETRIC 2)
(defconstant MM_HIMETRIC 3)
(defconstant MM_LOENGLISH 4)
(defconstant MM_HIENGLISH 5)
(defconstant MM_TWIPS 6)
(defconstant MM_ISOTROPIC 7)
(defconstant MM_ANISOTROPIC 8)
(defconstant ABSOLUTE 1)
(defconstant RELATIVE 2)
(defconstant WHITE_BRUSH 0)
(defconstant LTGRAY_BRUSH 1)
(defconstant GRAY_BRUSH 2)
(defconstant DKGRAY_BRUSH 3)
(defconstant BLACK_BRUSH 4)
(defconstant NULL_BRUSH 5)
(defconstant HOLLOW_BRUSH 5)
(defconstant WHITE_PEN 6)
(defconstant BLACK_PEN 7)
(defconstant NULL_PEN 8)
(defconstant OEM_FIXED_FONT 10)
;;				#xa

(defconstant ANSI_FIXED_FONT 11)
;;				#xb

(defconstant ANSI_VAR_FONT 12)
;;				#xc

(defconstant SYSTEM_FONT 13)
;;				#xd

(defconstant DEVICE_DEFAULT_FONT 14)
;;				#xe

(defconstant DEFAULT_PALETTE 15)
;;				#xf

(defconstant SYSTEM_FIXED_FONT 16)
;;				#x10

(defconstant BS_SOLID 0)
(defconstant BS_NULL 1)
(defconstant BS_HOLLOW 1)
(defconstant BS_HATCHED 2)
(defconstant BS_PATTERN 3)
(defconstant BS_INDEXED 4)
(defconstant BS_DIBPATTERN 5)
(defconstant HS_HORIZONTAL 0)
(defconstant HS_VERTICAL 1)
(defconstant HS_FDIAGONAL 2)
(defconstant HS_BDIAGONAL 3)
(defconstant HS_CROSS 4)
(defconstant HS_DIAGCROSS 5)
(defconstant PS_SOLID 0)
(defconstant PS_DASH 1)
(defconstant PS_DOT 2)
(defconstant PS_DASHDOT 3)
(defconstant PS_DASHDOTDOT 4)
(defconstant PS_NULL 5)
(defconstant PS_INSIDEFRAME 6)
(defconstant DRIVERVERSION 0)
(defconstant TECHNOLOGY 2)
(defconstant HORZSIZE 4)
(defconstant VERTSIZE 6)
(defconstant HORZRES 8)
(defconstant VERTRES 10)
;;				#xa

(defconstant BITSPIXEL 12)
;;				#xc

(defconstant PLANES 14)
;;				#xe

(defconstant NUMBRUSHES 16)
;;				#x10

(defconstant NUMPENS 18)
;;				#x12

(defconstant NUMMARKERS 20)
;;				#x14

(defconstant NUMFONTS 22)
;;				#x16

(defconstant NUMCOLORS 24)
;;				#x18

(defconstant PDEVICESIZE 26)
;;				#x1a

(defconstant CURVECAPS 28)
;;				#x1c

(defconstant LINECAPS 30)
;;				#x1e

(defconstant POLYGONALCAPS 32)
;;				#x20

(defconstant TEXTCAPS 34)
;;				#x22

(defconstant CLIPCAPS 36)
;;				#x24

(defconstant RASTERCAPS 38)
;;				#x26

(defconstant ASPECTX 40)
;;				#x28

(defconstant ASPECTY 42)
;;				#x2a

(defconstant ASPECTXY 44)
;;				#x2c

(defconstant LOGPIXELSX 88)
;;				#x58

(defconstant LOGPIXELSY 90)
;;				#x5a

(defconstant SIZEPALETTE 104)
;;				#x68

(defconstant NUMRESERVED 106)
;;				#x6a

(defconstant COLORRES 108)
;;				#x6c

(defconstant DT_PLOTTER 0)
(defconstant DT_RASDISPLAY 1)
(defconstant DT_RASPRINTER 2)
(defconstant DT_RASCAMERA 3)
(defconstant DT_CHARSTREAM 4)
(defconstant DT_METAFILE 5)
(defconstant DT_DISPFILE 6)
(defconstant CC_NONE 0)
(defconstant CC_CIRCLES 1)
(defconstant CC_PIE 2)
(defconstant CC_CHORD 4)
(defconstant CC_ELLIPSES 8)
(defconstant CC_WIDE 16)
;;				#x10

(defconstant CC_STYLED 32)
;;				#x20

(defconstant CC_WIDESTYLED 64)
;;				#x40

(defconstant CC_INTERIORS 128)
;;				#x80

(defconstant LC_NONE 0)
(defconstant LC_POLYLINE 2)
(defconstant LC_MARKER 4)
(defconstant LC_POLYMARKER 8)
(defconstant LC_WIDE 16)
;;				#x10

(defconstant LC_STYLED 32)
;;				#x20

(defconstant LC_WIDESTYLED 64)
;;				#x40

(defconstant LC_INTERIORS 128)
;;				#x80

(defconstant PC_NONE 0)
(defconstant PC_POLYGON 1)
(defconstant PC_RECTANGLE 2)
(defconstant PC_WINDPOLYGON 4)
(defconstant PC_TRAPEZOID 4)
(defconstant PC_SCANLINE 8)
(defconstant PC_WIDE 16)
;;				#x10

(defconstant PC_STYLED 32)
;;				#x20

(defconstant PC_WIDESTYLED 64)
;;				#x40

(defconstant PC_INTERIORS 128)
;;				#x80

(defconstant CP_NONE 0)
(defconstant CP_RECTANGLE 1)
(defconstant TC_OP_CHARACTER 1)
(defconstant TC_OP_STROKE 2)
(defconstant TC_CP_STROKE 4)
(defconstant TC_CR_90 8)
(defconstant TC_CR_ANY 16)
;;				#x10

(defconstant TC_SF_X_YINDEP 32)
;;				#x20

(defconstant TC_SA_DOUBLE 64)
;;				#x40

(defconstant TC_SA_INTEGER 128)
;;				#x80

(defconstant TC_SA_CONTIN 256)
;;				#x100

(defconstant TC_EA_DOUBLE 512)
;;				#x200

(defconstant TC_IA_ABLE 1024)
;;				#x400

(defconstant TC_UA_ABLE 2048)
;;				#x800

(defconstant TC_SO_ABLE 4096)
;;				#x1000

(defconstant TC_RA_ABLE 8192)
;;				#x2000

(defconstant TC_VA_ABLE 16384)
;;				#x4000

(defconstant TC_RESERVED 32768)
;;				#x8000

(defconstant RC_BITBLT 1)
(defconstant RC_BANDING 2)
(defconstant RC_SCALING 4)
(defconstant RC_BITMAP64 8)
(defconstant RC_GDI20_OUTPUT 16)
;;				#x10

(defconstant RC_DI_BITMAP 128)
;;				#x80

(defconstant RC_PALETTE 256)
;;				#x100

(defconstant RC_DIBTODEV 512)
;;				#x200

(defconstant RC_BIGFONT 1024)
;;				#x400

(defconstant RC_STRETCHBLT 2048)
;;				#x800

(defconstant RC_FLOODFILL 4096)
;;				#x1000

(defconstant RC_STRETCHDIB 8192)
;;				#x2000

(defconstant PC_RESERVED 1)
(defconstant PC_EXPLICIT 2)
(defconstant PC_NOCOLLAPSE 4)
(defconstant DIB_RGB_COLORS 0)
(defconstant DIB_PAL_COLORS 1)
(defconstant SYSPAL_STATIC 1)
(defconstant SYSPAL_NOSTATIC 2)
(defconstant CBM_INIT 4)
(defconstant DT_TOP 0)
(defconstant DT_LEFT 0)
(defconstant DT_CENTER 1)
(defconstant DT_RIGHT 2)
(defconstant DT_VCENTER 4)
(defconstant DT_BOTTOM 8)
(defconstant DT_WORDBREAK 16)
;;				#x10

(defconstant DT_SINGLELINE 32)
;;				#x20

(defconstant DT_EXPANDTABS 64)
;;				#x40

(defconstant DT_TABSTOP 128)
;;				#x80

(defconstant DT_NOCLIP 256)
;;				#x100

(defconstant DT_EXTERNALLEADING 512)
;;				#x200

(defconstant DT_CALCRECT 1024)
;;				#x400

(defconstant DT_NOPREFIX 2048)
;;				#x800

(defconstant DT_INTERNAL 4096)
;;				#x1000

(defconstant FLOODFILLBORDER 0)
(defconstant FLOODFILLSURFACE 1)
(defconstant SB_HORZ 0)
(defconstant SB_VERT 1)
(defconstant SB_CTL 2)
(defconstant SB_BOTH 3)
(defconstant SB_LINEUP 0)
(defconstant SB_LINEDOWN 1)
(defconstant SB_PAGEUP 2)
(defconstant SB_PAGEDOWN 3)
(defconstant SB_THUMBPOSITION 4)
(defconstant SB_THUMBTRACK 5)
(defconstant SB_TOP 6)
(defconstant SB_BOTTOM 7)
(defconstant SB_ENDSCROLL 8)
(defconstant SW_HIDE 0)
(defconstant SW_SHOWNORMAL 1)
(defconstant SW_NORMAL 1)
(defconstant SW_SHOWMINIMIZED 2)
(defconstant SW_SHOWMAXIMIZED 3)
(defconstant SW_MAXIMIZE 3)
(defconstant SW_SHOWNOACTIVATE 4)
(defconstant SW_SHOW 5)
(defconstant SW_MINIMIZE 6)
(defconstant SW_SHOWMINNOACTIVE 7)
(defconstant SW_SHOWNA 8)
(defconstant SW_RESTORE 9)
(defconstant HIDE_WINDOW 0)
(defconstant SHOW_OPENWINDOW 1)
(defconstant SHOW_ICONWINDOW 2)
(defconstant SHOW_FULLSCREEN 3)
(defconstant SHOW_OPENNOACTIVATE 4)
(defconstant SW_PARENTCLOSING 1)
(defconstant SW_OTHERZOOM 2)
(defconstant SW_PARENTOPENING 3)
(defconstant SW_OTHERUNZOOM 4)
#+aclpc
(defconstant ERROR 0)	;; don't define a Lisp package symbol
(defconstant NULLREGION 1)
(defconstant SIMPLEREGION 2)
(defconstant COMPLEXREGION 3)
(defconstant RGN_AND 1)
(defconstant RGN_OR 2)
(defconstant RGN_XOR 3)
(defconstant RGN_DIFF 4)
(defconstant RGN_COPY 5)
(defconstant VK_LBUTTON 1)
(defconstant VK_RBUTTON 2)
(defconstant VK_CANCEL 3)
(defconstant VK_MBUTTON 4)
(defconstant VK_BACK 8)
(defconstant VK_TAB 9)
(defconstant VK_CLEAR 12)
;;				#xc

(defconstant VK_RETURN 13)
;;				#xd

(defconstant VK_SHIFT 16)
;;				#x10

(defconstant VK_CONTROL 17)
;;				#x11

(defconstant VK_MENU 18)
;;				#x12

(defconstant VK_PAUSE 19)
;;				#x13

(defconstant VK_CAPITAL 20)
;;				#x14

(defconstant VK_ESCAPE 27)
;;				#x1b

(defconstant VK_SPACE 32)
;;				#x20

(defconstant VK_PRIOR 33)
;;				#x21

(defconstant VK_NEXT 34)
;;				#x22

(defconstant VK_END 35)
;;				#x23

(defconstant VK_HOME 36)
;;				#x24

(defconstant VK_LEFT 37)
;;				#x25

(defconstant VK_UP 38)
;;				#x26

(defconstant VK_RIGHT 39)
;;				#x27

(defconstant VK_DOWN 40)
;;				#x28

(defconstant VK_SELECT 41)
;;				#x29

(defconstant VK_PRINT 42)
;;				#x2a

(defconstant VK_EXECUTE 43)
;;				#x2b

(defconstant VK_SNAPSHOT 44)
;;				#x2c

(defconstant VK_INSERT 45)
;;				#x2d

(defconstant VK_DELETE 46)
;;				#x2e

(defconstant VK_HELP 47)
;;				#x2f

(defconstant VK_NUMPAD0 96)
;;				#x60

(defconstant VK_NUMPAD1 97)
;;				#x61

(defconstant VK_NUMPAD2 98)
;;				#x62

(defconstant VK_NUMPAD3 99)
;;				#x63

(defconstant VK_NUMPAD4 100)
;;				#x64

(defconstant VK_NUMPAD5 101)
;;				#x65

(defconstant VK_NUMPAD6 102)
;;				#x66

(defconstant VK_NUMPAD7 103)
;;				#x67

(defconstant VK_NUMPAD8 104)
;;				#x68

(defconstant VK_NUMPAD9 105)
;;				#x69

(defconstant VK_MULTIPLY 106)
;;				#x6a

(defconstant VK_ADD 107)
;;				#x6b

(defconstant VK_SEPARATOR 108)
;;				#x6c

(defconstant VK_SUBTRACT 109)
;;				#x6d

(defconstant VK_DECIMAL 110)
;;				#x6e

(defconstant VK_DIVIDE 111)
;;				#x6f

(defconstant VK_F1 112)
;;				#x70

(defconstant VK_F2 113)
;;				#x71

(defconstant VK_F3 114)
;;				#x72

(defconstant VK_F4 115)
;;				#x73

(defconstant VK_F5 116)
;;				#x74

(defconstant VK_F6 117)
;;				#x75

(defconstant VK_F7 118)
;;				#x76

(defconstant VK_F8 119)
;;				#x77

(defconstant VK_F9 120)
;;				#x78

(defconstant VK_F10 121)
;;				#x79

(defconstant VK_F11 122)
;;				#x7a

(defconstant VK_F12 123)
;;				#x7b

(defconstant VK_F13 124)
;;				#x7c

(defconstant VK_F14 125)
;;				#x7d

(defconstant VK_F15 126)
;;				#x7e

(defconstant VK_F16 127)
;;				#x7f

(defconstant VK_NUMLOCK 144)
;;				#x90

(defconstant WH_MSGFILTER -1)
;;				#xffffffff

(defconstant WH_JOURNALRECORD 0)
(defconstant WH_JOURNALPLAYBACK 1)
(defconstant WH_KEYBOARD 2)
(defconstant WH_GETMESSAGE 3)
(defconstant WH_CALLWNDPROC 4)
(defconstant WH_CBT 5)
(defconstant WH_SYSMSGFILTER 6)
;(defconstant WH_WINDOWMGR 7)
;(defconstant HC_LPLPFNNEXT -2)
;;				#xfffffffe

;(defconstant HC_LPFNNEXT -1)
;;				#xffffffff



(defconstant HC_ACTION 0)
(defconstant HC_GETNEXT 1)
(defconstant HC_SKIP 2)
(defconstant HC_NOREM 3)
(defconstant HC_NOREMOVE 3)
(defconstant HC_SYSMODALON 4)
(defconstant HC_SYSMODALOFF 5)
(defconstant HCBT_MOVESIZE 0)
(defconstant HCBT_MINMAX 1)
(defconstant HCBT_QS 2)
(defconstant MSGF_DIALOGBOX 0)
(defconstant MSGF_MESSAGEBOX 1)
(defconstant MSGF_MENU 2)
(defconstant MSGF_MOVE 3)
(defconstant MSGF_SIZE 4)
(defconstant MSGF_SCROLLBAR 5)
(defconstant MSGF_NEXTWINDOW 6)
(defconstant WC_INIT 1)
(defconstant WC_SWP 2)
(defconstant WC_DEFWINDOWPROC 3)
(defconstant WC_MINMAX 4)
(defconstant WC_MOVE 5)
(defconstant WC_SIZE 6)
(defconstant WC_DRAWCAPTION 7)
(defconstant GWL_WNDPROC -4)
;;				#xfffffffc

;(defconstant GWW_HINSTANCE -6)
;;				#xfffffffa

(defconstant GWL_HINSTANCE -6)
;;				#xfffffffa

;(defconstant GWW_HWNDPARENT -8)
;;				#xfffffff8

(defconstant GWL_HWNDPARENT -8)
;;				#xfffffff8

;(defconstant GWW_ID -12)
;;				#xfffffff4

(defconstant GWL_ID -12)
;;					#xfffffff4

(defconstant GWL_STYLE -16)
;;				#xfffffff0

(defconstant GWL_EXSTYLE -20)
;;				#xffffffec

(defconstant GCL_MENUNAME -8)
;;				#xfffffff8

;(defconstant GCW_HBRBACKGROUND -10)
;;				#xfffffff6

;(defconstant GCW_HCURSOR -12)
;;				#xfffffff4

;(defconstant GCW_HICON -14)
;;				#xfffffff2

;(defconstant GCW_HMODULE -16)
;;				#xfffffff0

;(defconstant GCW_CBWNDEXTRA -18)
;;				#xffffffee

;(defconstant GCW_CBCLSEXTRA -20)
;;				#xffffffec

(defconstant GCL_WNDPROC -24)
;;				#xffffffe8

;(defconstant GCW_STYLE -26)
;;				#xffffffe6

(defconstant WM_NULL 0)
(defconstant WM_CREATE 1)
(defconstant WM_DESTROY 2)
(defconstant WM_MOVE 3)
(defconstant WM_SIZE 5)
(defconstant WM_ACTIVATE 6)
(defconstant WM_SETFOCUS 7)
(defconstant WM_KILLFOCUS 8)
(defconstant WM_ENABLE 10)
;;				#xa

(defconstant WM_SETREDRAW 11)
;;				#xb

(defconstant WM_SETTEXT 12)
;;				#xc

(defconstant WM_GETTEXT 13)
;;				#xd

(defconstant WM_GETTEXTLENGTH 14)
;;				#xe

(defconstant WM_PAINT 15)
;;				#xf

(defconstant WM_CLOSE 16)
;;				#x10

(defconstant WM_QUERYENDSESSION 17)
;;				#x11

(defconstant WM_QUIT 18)
;;				#x12

(defconstant WM_QUERYOPEN 19)
;;				#x13

(defconstant WM_ERASEBKGND 20)
;;				#x14

(defconstant WM_SYSCOLORCHANGE 21)
;;				#x15

(defconstant WM_ENDSESSION 22)
;;				#x16

(defconstant WM_SHOWWINDOW 24)
;;				#x18

;(defconstant WM_CTLCOLOR 25)
;;				#x19

(defconstant WM_WININICHANGE 26)
;;				#x1a

(defconstant WM_DEVMODECHANGE 27)
;;				#x1b

(defconstant WM_ACTIVATEAPP 28)
;;				#x1c

(defconstant WM_FONTCHANGE 29)
;;				#x1d

(defconstant WM_TIMECHANGE 30)
;;				#x1e

(defconstant WM_CANCELMODE 31)
;;				#x1f

(defconstant WM_SETCURSOR 32)
;;				#x20

(defconstant WM_MOUSEACTIVATE 33)
;;				#x21

(defconstant WM_CHILDACTIVATE 34)
;;				#x22

(defconstant WM_QUEUESYNC 35)
;;				#x23

(defconstant WM_GETMINMAXINFO 36)
;;				#x24

(defconstant WM_PAINTICON 38)
;;				#x26

(defconstant WM_ICONERASEBKGND 39)
;;				#x27

(defconstant WM_NEXTDLGCTL 40)
;;				#x28

(defconstant WM_SPOOLERSTATUS 42)
;;				#x2a

(defconstant WM_DRAWITEM 43)
;;				#x2b

(defconstant WM_MEASUREITEM 44)
;;				#x2c

(defconstant WM_DELETEITEM 45)
;;				#x2d

(defconstant WM_VKEYTOITEM 46)
;;				#x2e

(defconstant WM_CHARTOITEM 47)
;;				#x2f

(defconstant WM_SETFONT 48)
;;				#x30

(defconstant WM_GETFONT 49)
;;				#x31

(defconstant WM_QUERYDRAGICON 55)
;;				#x37

(defconstant WM_COMPAREITEM 57)
;;				#x39

(defconstant WM_COMPACTING 65)
;;				#x41

(defconstant WM_NCCREATE 129)
;;				#x81

(defconstant WM_NCDESTROY 130)
;;				#x82

(defconstant WM_NCCALCSIZE 131)
;;				#x83

(defconstant WM_NCHITTEST 132)
;;				#x84

(defconstant WM_NCPAINT 133)
;;				#x85

(defconstant WM_NCACTIVATE 134)
;;				#x86

(defconstant WM_GETDLGCODE 135)
;;				#x87

(defconstant WM_NCMOUSEMOVE 160)
;;				#xa0

(defconstant WM_NCLBUTTONDOWN 161)
;;				#xa1

(defconstant WM_NCLBUTTONUP 162)
;;				#xa2

(defconstant WM_NCLBUTTONDBLCLK 163)
;;				#xa3

(defconstant WM_NCRBUTTONDOWN 164)
;;				#xa4

(defconstant WM_NCRBUTTONUP 165)
;;				#xa5

(defconstant WM_NCRBUTTONDBLCLK 166)
;;				#xa6

(defconstant WM_NCMBUTTONDOWN 167)
;;				#xa7

(defconstant WM_NCMBUTTONUP 168)
;;				#xa8

(defconstant WM_NCMBUTTONDBLCLK 169)
;;				#xa9

(defconstant WM_KEYFIRST 256)
;;				#x100

(defconstant WM_KEYDOWN 256)
;;				#x100

(defconstant WM_KEYUP 257)
;;				#x101

(defconstant WM_CHAR 258)
;;				#x102

(defconstant WM_DEADCHAR 259)
;;				#x103

(defconstant WM_SYSKEYDOWN 260)
;;				#x104

(defconstant WM_SYSKEYUP 261)
;;				#x105

(defconstant WM_SYSCHAR 262)
;;				#x106

(defconstant WM_SYSDEADCHAR 263)
;;				#x107

(defconstant WM_KEYLAST 264)
;;				#x108

(defconstant WM_INITDIALOG 272)
;;				#x110

(defconstant WM_COMMAND 273)
;;				#x111

(defconstant WM_SYSCOMMAND 274)
;;				#x112

(defconstant WM_TIMER 275)
;;				#x113

(defconstant WM_HSCROLL 276)
;;				#x114

(defconstant WM_VSCROLL 277)
;;				#x115

(defconstant WM_INITMENU 278)
;;				#x116

(defconstant WM_INITMENUPOPUP 279)
;;				#x117

(defconstant WM_MENUSELECT 287)
;;				#x11f

(defconstant WM_MENUCHAR 288)
;;				#x120

(defconstant WM_ENTERIDLE 289)
;;				#x121

(defconstant WM_CTLCOLORMSGBOX #x0132)

(defconstant WM_CTLCOLOREDIT #x0133)

(defconstant WM_CTLCOLORLISTBOX #x0134)

(defconstant WM_CTLCOLORBTN #x0135)

(defconstant WM_CTLCOLORDLG #x0136)

(defconstant WM_CTLCOLORSCROLLBAR #x0137)

(defconstant WM_CTLCOLORSTATIC #x0138)

(defconstant WM_MOUSEFIRST 512)
;;				#x200

(defconstant WM_MOUSEMOVE 512)
;;				#x200

(defconstant WM_LBUTTONDOWN 513)
;;				#x201

(defconstant WM_LBUTTONUP 514)
;;				#x202

(defconstant WM_LBUTTONDBLCLK 515)
;;				#x203

(defconstant WM_RBUTTONDOWN 516)
;;				#x204

(defconstant WM_RBUTTONUP 517)
;;				#x205

(defconstant WM_RBUTTONDBLCLK 518)
;;				#x206

(defconstant WM_MBUTTONDOWN 519)
;;				#x207

(defconstant WM_MBUTTONUP 520)
;;				#x208

(defconstant WM_MBUTTONDBLCLK 521)
;;				#x209

(defconstant WM_MOUSELAST 521)
;;				#x209

(defconstant WM_PARENTNOTIFY 528)
;;				#x210

(defconstant WM_ENTERMENULOOP 529)
;;				#x211
(defconstant WM_EXITMENULOOP 530)
;;				#x212

(defconstant WM_MDICREATE 544)
;;				#x220

(defconstant WM_MDIDESTROY 545)
;;				#x221

(defconstant WM_MDIACTIVATE 546)
;;				#x222

(defconstant WM_MDIRESTORE 547)
;;				#x223

(defconstant WM_MDINEXT 548)
;;				#x224

(defconstant WM_MDIMAXIMIZE 549)
;;				#x225

(defconstant WM_MDITILE 550)
;;				#x226

(defconstant WM_MDICASCADE 551)
;;				#x227

(defconstant WM_MDIICONARRANGE 552)
;;				#x228

(defconstant WM_MDIGETACTIVE 553)
;;				#x229

(defconstant WM_MDISETMENU 560)
;;				#x230

(defconstant WM_CUT 768)
;;				#x300

(defconstant WM_COPY 769)
;;				#x301

(defconstant WM_PASTE 770)
;;				#x302

(defconstant WM_CLEAR 771)
;;				#x303

(defconstant WM_UNDO 772)
;;				#x304

(defconstant WM_RENDERFORMAT 773)
;;				#x305

(defconstant WM_RENDERALLFORMATS 774)
;;				#x306

(defconstant WM_DESTROYCLIPBOARD 775)
;;				#x307

(defconstant WM_DRAWCLIPBOARD 776)
;;				#x308

(defconstant WM_PAINTCLIPBOARD 777)
;;				#x309

(defconstant WM_VSCROLLCLIPBOARD 778)
;;				#x30a

(defconstant WM_SIZECLIPBOARD 779)
;;				#x30b

(defconstant WM_ASKCBFORMATNAME 780)
;;				#x30c

(defconstant WM_CHANGECBCHAIN 781)
;;				#x30d

(defconstant WM_HSCROLLCLIPBOARD 782)
;;				#x30e

(defconstant WM_QUERYNEWPALETTE 783)
;;				#x30f

(defconstant WM_PALETTEISCHANGING 784)
;;				#x310

(defconstant WM_PALETTECHANGED 785)
;;				#x311

(defconstant WM_USER 1024)
;;				#x400

(defconstant ST_BEGINSWP 0)
(defconstant ST_ENDSWP 1)
(defconstant HTERROR -2)
;;				#xfffffffe

(defconstant HTTRANSPARENT -1)
;;				#xffffffff

(defconstant HTNOWHERE 0)
(defconstant HTCLIENT 1)
(defconstant HTCAPTION 2)
(defconstant HTSYSMENU 3)
(defconstant HTGROWBOX 4)
(defconstant HTSIZE 4)
(defconstant HTMENU 5)
(defconstant HTHSCROLL 6)
(defconstant HTVSCROLL 7)
(defconstant HTREDUCE 8)
(defconstant HTZOOM 9)
(defconstant HTLEFT 10)
;;				#xa

(defconstant HTRIGHT 11)
;;				#xb

(defconstant HTTOP 12)
;;				#xc

(defconstant HTTOPLEFT 13)
;;				#xd

(defconstant HTTOPRIGHT 14)
;;				#xe

(defconstant HTBOTTOM 15)
;;				#xf

(defconstant HTBOTTOMLEFT 16)
;;				#x10

(defconstant HTBOTTOMRIGHT 17)
;;				#x11

(defconstant HTSIZEFIRST 10)
;;				#xa

(defconstant HTSIZELAST 17)
;;				#x11

(defconstant MA_ACTIVATE 1)
(defconstant MA_ACTIVATEANDEAT 2)
(defconstant MA_NOACTIVATE 3)
(defconstant SIZENORMAL 0)
(defconstant SIZEICONIC 1)
(defconstant SIZEFULLSCREEN 2)
(defconstant SIZEZOOMSHOW 3)
(defconstant SIZEZOOMHIDE 4)
(defconstant MK_LBUTTON 1)
(defconstant MK_RBUTTON 2)
(defconstant MK_SHIFT 4)
(defconstant MK_CONTROL 8)
(defconstant MK_MBUTTON 16)
;;				#x10

(defconstant WS_OVERLAPPED 0)
(defconstant WS_POPUP 2147483648) 

(defconstant WS_CHILD 1073741824) 

(defconstant WS_MINIMIZE 536870912) 

(defconstant WS_VISIBLE 268435456) 

(defconstant WS_DISABLED 134217728) 

(defconstant WS_CLIPSIBLINGS 67108864) 

(defconstant WS_CLIPCHILDREN 33554432) 

(defconstant WS_MAXIMIZE 16777216) 

(defconstant WS_CAPTION 12582912) 

(defconstant WS_BORDER 8388608) 

(defconstant WS_DLGFRAME 4194304) 

(defconstant WS_VSCROLL 2097152) 

(defconstant WS_HSCROLL 1048576) 

(defconstant WS_SYSMENU 524288)
;;				#x80000

(defconstant WS_THICKFRAME 262144)
;;				#x40000

(defconstant WS_GROUP 131072)
;;				#x20000

(defconstant WS_TABSTOP 65536)
;;				#x10000

(defconstant WS_MINIMIZEBOX 131072)
;;				#x20000

(defconstant WS_MAXIMIZEBOX 65536)
;;				#x10000

(defconstant WS_TILED 0)
(defconstant WS_ICONIC 536870912) 

(defconstant WS_SIZEBOX 262144)
;;				#x40000

(defconstant WS_OVERLAPPEDWINDOW 13565952) 

(defconstant WS_POPUPWINDOW 2156396544) 

(defconstant WS_CHILDWINDOW 1073741824) 

(defconstant WS_TILEDWINDOW 13565952) 

(defconstant WS_EX_DLGMODALFRAME 1)	; #x01
(defconstant WS_EX_NOPARENTNOTIFY 4)	; #x04
(defconstant WS_EX_TOPMOST 8)		; #x08
(defconstant WS_EX_ACCEPTFILES 16)	; #x10
(defconstant WS_EX_TRANSPARENT 32)	; #x20
(defconstant WS_EX_MDICHILD 64)		; #x40
(defconstant WS_EX_TOOLWINDOW 128)	; #x80
(defconstant WS_EX_WINDOWEDGE 256)	; #x100
(defconstant WS_EX_CLIENTEDGE 512)	; #x200
(defconstant WS_EX_CONTEXTHELP 1024)	; #x400

(defconstant WS_EX_RIGHT #x1000)
(defconstant WS_EX_LEFT #x0)
(defconstant WS_EX_RTLREADING #x2000)
(defconstant WS_EX_LTRREADING #x0)
(defconstant WS_EX_LEFTSCROLLBAR #x4000)
(defconstant WS_EX_RIGHTSCROLLBAR #x0)
(defconstant WS_EX_CONTROLPARENT #x10000)
(defconstant WS_EX_STATICEDGE #x20000)
(defconstant WS_EX_APPWINDOW #x40000)
(defconstant WS_EX_OVERLAPPEDWINDOW 
    (logior WS_EX_WINDOWEDGE WS_EX_CLIENTEDGE))
(defconstant WS_EX_PALETTEWINDOW 
    (logior WS_EX_WINDOWEDGE WS_EX_TOOLWINDOW WS_EX_TOPMOST))


(defconstant CS_VREDRAW 1)
(defconstant CS_HREDRAW 2)
(defconstant CS_KEYCVTWINDOW 4)
(defconstant CS_DBLCLKS 8)
(defconstant CS_OWNDC 32)
;;				#x20

(defconstant CS_CLASSDC 64)
;;				#x40

(defconstant CS_PARENTDC 128)
;;				#x80

(defconstant CS_NOKEYCVT 256)
;;				#x100

(defconstant CS_NOCLOSE 512)
;;				#x200

(defconstant CS_SAVEBITS 2048)
;;				#x800

(defconstant CS_BYTEALIGNCLIENT 4096)
;;				#x1000

(defconstant CS_BYTEALIGNWINDOW 8192)
;;				#x2000

(defconstant CS_GLOBALCLASS 16384)
;;				#x4000

(defconstant CF_TEXT 1)
(defconstant CF_BITMAP 2)
(defconstant CF_METAFILEPICT 3)
(defconstant CF_SYLK 4)
(defconstant CF_DIF 5)
(defconstant CF_TIFF 6)
(defconstant CF_OEMTEXT 7)
(defconstant CF_DIB 8)
(defconstant CF_PALETTE 9)
(defconstant CF_OWNERDISPLAY 128)
;;				#x80

(defconstant CF_DSPTEXT 129)
;;				#x81

(defconstant CF_DSPBITMAP 130)
;;				#x82

(defconstant CF_DSPMETAFILEPICT 131)
;;				#x83

(defconstant CF_PRIVATEFIRST 512)
;;				#x200

(defconstant CF_PRIVATELAST 767)
;;				#x2ff

(defconstant CF_GDIOBJFIRST 768)
;;				#x300

(defconstant CF_GDIOBJLAST 1023)
;;				#x3ff

(defconstant ODT_MENU 1)
(defconstant ODT_LISTBOX 2)
(defconstant ODT_COMBOBOX 3)
(defconstant ODT_BUTTON 4)
(defconstant ODA_DRAWENTIRE 1)
(defconstant ODA_SELECT 2)
(defconstant ODA_FOCUS 4)
(defconstant ODS_SELECTED 1)
(defconstant ODS_GRAYED 2)
(defconstant ODS_DISABLED 4)
(defconstant ODS_CHECKED 8)
(defconstant ODS_FOCUS 16)
;;				#x10

(defconstant PM_NOREMOVE 0)
(defconstant PM_REMOVE 1)
(defconstant PM_NOYIELD 2)
;(defconstant READ 0)
;(defconstant WRITE 1)
;(defconstant READ_WRITE 2)
(defconstant CW_USEDEFAULT 2147483648)	; was 32768)

(defconstant SWP_NOSIZE 1)
(defconstant SWP_NOMOVE 2)
(defconstant SWP_NOZORDER 4)
(defconstant SWP_NOREDRAW 8)
(defconstant SWP_NOACTIVATE 16)
;;				#x10

(defconstant SWP_DRAWFRAME 32)
;;				#x20

(defconstant SWP_SHOWWINDOW 64)
;;				#x40

(defconstant SWP_HIDEWINDOW 128)
;;				#x80

(defconstant SWP_NOCOPYBITS 256)
;;				#x100

(defconstant SWP_NOREPOSITION 512)
;;				#x200

;; more useful SetWindowPos bits

(defconstant HWND_TOP 	0)
(defconstant HWND_BOTTOM 	1)
(defconstant HWND_TOPMOST   -1)
(defconstant HWND_NOTOPMOST -2)




(defconstant DLGWINDOWEXTRA 30)
;;				#x1e

(defconstant SM_CXSCREEN 0)
(defconstant SM_CYSCREEN 1)
(defconstant SM_CXVSCROLL 2)
(defconstant SM_CYHSCROLL 3)
(defconstant SM_CYCAPTION 4)
(defconstant SM_CXBORDER 5)
(defconstant SM_CYBORDER 6)
(defconstant SM_CXDLGFRAME 7)
(defconstant SM_CYDLGFRAME 8)
(defconstant SM_CYVTHUMB 9)
(defconstant SM_CXHTHUMB 10)
;;				#xa

(defconstant SM_CXICON 11)
;;				#xb

(defconstant SM_CYICON 12)
;;				#xc

(defconstant SM_CXCURSOR 13)
;;				#xd

(defconstant SM_CYCURSOR 14)
;;				#xe

(defconstant SM_CYMENU 15)
;;				#xf

(defconstant SM_CXFULLSCREEN 16)
;;				#x10

(defconstant SM_CYFULLSCREEN 17)
;;				#x11

(defconstant SM_CYKANJIWINDOW 18)
;;				#x12

(defconstant SM_MOUSEPRESENT 19)
;;				#x13

(defconstant SM_CYVSCROLL 20)
;;				#x14

(defconstant SM_CXHSCROLL 21)
;;				#x15

(defconstant SM_DEBUG 22)
;;				#x16

(defconstant SM_SWAPBUTTON 23)
;;				#x17

(defconstant SM_RESERVED1 24)
;;				#x18

(defconstant SM_RESERVED2 25)
;;				#x19

(defconstant SM_RESERVED3 26)
;;				#x1a

(defconstant SM_RESERVED4 27)
;;				#x1b

(defconstant SM_CXMIN 28)
;;				#x1c

(defconstant SM_CYMIN 29)
;;				#x1d

(defconstant SM_CXSIZE 30)
;;				#x1e

(defconstant SM_CYSIZE 31)
;;				#x1f

(defconstant SM_CXFRAME 32)
;;				#x20

(defconstant SM_CYFRAME 33)
;;				#x21

(defconstant SM_CXMINTRACK 34)
;;				#x22

(defconstant SM_CYMINTRACK 35)
;;				#x23

(defconstant SM_CMETRICS 44)		; was 36
;;					#x2c

(defconstant SM_CXSMSIZE 52)
(defconstant SM_CYSMSIZE 53)

(defconstant MB_OK 0)
(defconstant MB_OKCANCEL 1)
(defconstant MB_ABORTRETRYIGNORE 2)
(defconstant MB_YESNOCANCEL 3)
(defconstant MB_YESNO 4)
(defconstant MB_RETRYCANCEL 5)
(defconstant MB_ICONHAND 16)
;;				#x10

(defconstant MB_ICONQUESTION 32)
;;				#x20

(defconstant MB_ICONEXCLAMATION 48)
;;				#x30

(defconstant MB_ICONASTERISK 64)
;;				#x40

(defconstant MB_ICONINFORMATION 64)
;;				#x40

(defconstant MB_ICONSTOP 16)
;;				#x10

(defconstant MB_DEFBUTTON1 0)
(defconstant MB_DEFBUTTON2 256)
;;				#x100

(defconstant MB_DEFBUTTON3 512)
;;				#x200

(defconstant MB_APPLMODAL 0)
(defconstant MB_SYSTEMMODAL 4096)
;;				#x1000

(defconstant MB_TASKMODAL 8192)
;;				#x2000

(defconstant MB_NOFOCUS 32768)
;;				#x8000

(defconstant MB_TYPEMASK 15)
;;				#xf

(defconstant MB_ICONMASK 240)
;;				#xf0

(defconstant MB_DEFMASK 3840)
;;				#xf00

(defconstant MB_MODEMASK 12288)
;;				#x3000

(defconstant MB_MISCMASK 49152)
;;				#xc000

;(defconstant CTLCOLOR_MSGBOX 0)
;(defconstant CTLCOLOR_EDIT 1)
;(defconstant CTLCOLOR_LISTBOX 2)
;(defconstant CTLCOLOR_BTN 3)
;(defconstant CTLCOLOR_DLG 4)
;(defconstant CTLCOLOR_SCROLLBAR 5)
;(defconstant CTLCOLOR_STATIC 6)
;(defconstant CTLCOLOR_MAX 8)
(defconstant COLOR_SCROLLBAR 0)
(defconstant COLOR_BACKGROUND 1)
(defconstant COLOR_ACTIVECAPTION 2)
(defconstant COLOR_INACTIVECAPTION 3)
(defconstant COLOR_MENU 4)
(defconstant COLOR_WINDOW 5)
(defconstant COLOR_WINDOWFRAME 6)
(defconstant COLOR_MENUTEXT 7)
(defconstant COLOR_WINDOWTEXT 8)
(defconstant COLOR_CAPTIONTEXT 9)
(defconstant COLOR_ACTIVEBORDER 10)
;;					#xa

(defconstant COLOR_INACTIVEBORDER 11)
;;					#xb

(defconstant COLOR_APPWORKSPACE 12)
;;					#xc

(defconstant COLOR_HIGHLIGHT 13)
;;					#xd

(defconstant COLOR_HIGHLIGHTTEXT 14)
;;					#xe

(defconstant COLOR_BTNFACE 15)
;;					#xf

(defconstant COLOR_BTNSHADOW 16)
;;					#x10

(defconstant COLOR_GRAYTEXT 17)
;;					#x11

(defconstant COLOR_BTNTEXT 18)
;;					#x12

(defconstant COLOR_INACTIVECAPTIONTEXT 19)
;;					#x13

(defconstant COLOR_BTNHIGHLIGHT 20)
;;					#x14

(defconstant COLOR_ENDCOLORS 20)
;;					#x14

(defconstant GW_HWNDFIRST 0)
(defconstant GW_HWNDLAST 1)
(defconstant GW_HWNDNEXT 2)
(defconstant GW_HWNDPREV 3)
(defconstant GW_OWNER 4)
(defconstant GW_CHILD 5)
(defconstant MF_INSERT 0)
(defconstant MF_CHANGE 128)
;;					#x80

(defconstant MF_APPEND 256)
;;					#x100

(defconstant MF_DELETE 512)
;;					#x200

(defconstant MF_REMOVE 4096)
;;					#x1000

(defconstant MF_BYCOMMAND 0)
(defconstant MF_BYPOSITION 1024)
;;					#x400

(defconstant MF_SEPARATOR 2048)
;;					#x800

(defconstant MF_ENABLED 0)
(defconstant MF_GRAYED 1)
(defconstant MF_DISABLED 2)
(defconstant MF_UNCHECKED 0)
(defconstant MF_CHECKED 8)
(defconstant MF_USECHECKBITMAPS 512)
;;					#x200

(defconstant MF_STRING 0)
(defconstant MF_BITMAP 4)
(defconstant MF_OWNERDRAW 256)
;;					#x100

(defconstant MF_POPUP 16)
;;					#x10

(defconstant MF_MENUBARBREAK 32)
;;					#x20

(defconstant MF_MENUBREAK 64)
;;					#x40

(defconstant MF_UNHILITE 0)
(defconstant MF_HILITE 128)
;;					#x80

(defconstant MF_SYSMENU 8192)
;;					#x2000

(defconstant MF_HELP 16384)
;;					#x4000

(defconstant MF_MOUSESELECT 32768)
;;					#x8000

(defconstant MF_END 128)
;;					#x80

(defconstant SC_SIZE 61440)
;;					#xf000

(defconstant SC_MOVE 61456)
;;					#xf010

(defconstant SC_MINIMIZE 61472)
;;					#xf020

(defconstant SC_MAXIMIZE 61488)
;;					#xf030

(defconstant SC_NEXTWINDOW 61504)
;;					#xf040

(defconstant SC_PREVWINDOW 61520)
;;					#xf050

(defconstant SC_CLOSE 61536)
;;					#xf060

(defconstant SC_VSCROLL 61552)
;;					#xf070

(defconstant SC_HSCROLL 61568)
;;					#xf080

(defconstant SC_MOUSEMENU 61584)
;;					#xf090

(defconstant SC_KEYMENU 61696)
;;					#xf100

(defconstant SC_ARRANGE 61712)
;;					#xf110

(defconstant SC_RESTORE 61728)
;;					#xf120

(defconstant SC_TASKLIST 61744)
;;					#xf130

(defconstant SC_ICON 61472)
;;					#xf020

(defconstant SC_ZOOM 61488)
;;					#xf030

(defconstant IDC_ARROW 32512)
;;					#x7f00

(defconstant IDC_IBEAM 32513)
;;					#x7f01

(defconstant IDC_WAIT 32514)
;;					#x7f02

(defconstant IDC_CROSS 32515)
;;					#x7f03

(defconstant IDC_UPARROW 32516)
;;					#x7f04

(defconstant IDC_SIZE 32640)
;;					#x7f80

(defconstant IDC_ICON 32641)
;;					#x7f81

(defconstant IDC_SIZENWSE 32642)
;;					#x7f82

(defconstant IDC_SIZENESW 32643)
;;					#x7f83

(defconstant IDC_SIZEWE 32644)
;;					#x7f84

(defconstant IDC_SIZENS 32645)
(defconstant IDC_SIZEALL         32646)
;;				
(defconstant IDC_APPSTARTING     32650) ;; not in win3.1
(defconstant ORD_LANGDRIVER 1)
(defconstant IDI_APPLICATION 32512)
;;					#x7f00

(defconstant IDI_HAND 32513)
;;					#x7f01

(defconstant IDI_QUESTION 32514)
;;					#x7f02

(defconstant IDI_EXCLAMATION 32515)
;;					#x7f03

(defconstant IDI_ASTERISK 32516)
;;					#x7f04

(defconstant IDOK 1)
(defconstant IDCANCEL 2)
(defconstant IDABORT 3)
(defconstant IDRETRY 4)
(defconstant IDIGNORE 5)
(defconstant IDYES 6)
(defconstant IDNO 7)
(defconstant ES_LEFT 0)
(defconstant ES_CENTER 1)
(defconstant ES_RIGHT 2)
(defconstant ES_MULTILINE 4)
(defconstant ES_UPPERCASE 8)
(defconstant ES_LOWERCASE 16)
;;					#x10

(defconstant ES_PASSWORD 32)
;;					#x20

(defconstant ES_AUTOVSCROLL 64)
;;					#x40

(defconstant ES_AUTOHSCROLL 128)
;;					#x80

(defconstant ES_NOHIDESEL 256)
;;					#x100

(defconstant ES_OEMCONVERT 1024)
;;					#x400

(defconstant ES_READONLY 2048)

(defconstant EN_SETFOCUS 256)
;;					#x100

(defconstant EN_KILLFOCUS 512)
;;					#x200

(defconstant EN_CHANGE 768)
;;					#x300

(defconstant EN_UPDATE 1024)
;;					#x400

(defconstant EN_ERRSPACE 1280)
;;					#x500

(defconstant EN_MAXTEXT 1281)
;;					#x501

(defconstant EN_HSCROLL 1537)
;;					#x601

(defconstant EN_VSCROLL 1538)
;;					#x602

#-mainwin16
(defconstant EM_GETSEL 176)		; was 1024
;;					#xb0
#+MAINWIN16
(defconstant EM_GETSEL WM_USER)		

#-mainwin16
(defconstant EM_SETSEL 177)		; was 1025
;;					#xb1
#+MAINWIN16
(defconstant EM_SETSEL     (+ WM_USER 1))

#-mainwin16
(defconstant EM_GETRECT 178)		; was 1026
;;					#xb2
#+MAINWIN16
(defconstant EM_GETRECT    (+ WM_USER 2))		

#-mainwin16
(defconstant EM_SETRECT 179)		; was 1027
;;					#xb3
#+MAINWIN16
(defconstant EM_SETRECT    (+ WM_USER 3))	

#-mainwin16
(defconstant EM_SETRECTNP 180)	; was 1028
;;					#xb4
#+MAINWIN16
(defconstant EM_SETRECTNP  (+ WM_USER 4))


(defconstant EM_SCROLL 181)		; was 1029
;;					#xb5


(defconstant EM_LINESCROLL 182)	; was 1030
;;					#x
#+MAINWIN16
(defconstant EM_LINESCROLL (+ WM_USER 6))




(defconstant EM_SCROLLCARET 183)
;;					#xb7


(defconstant EM_GETMODIFY 184)	; was 1032
;;					#xb8
#+MAINWIN16
(defconstant EM_GETMODIFY  (+ WM_USER 8))


(defconstant EM_SETMODIFY 185)	; was 1033
;;					#xb9
#+MAINWIN16
(defconstant EM_SETMODIFY  (+ WM_USER 9))


(defconstant EM_GETLINECOUNT 186)	; was 1034
;;					#xba
#+MAINWIN16
(defconstant EM_GETLINECOUNT (+ WM_USER 10))


(defconstant EM_LINEINDEX 187)	; was 1035
;;					#xbb
#+MAINWIN16
(defconstant EM_LINEINDEX  (+ WM_USER 11))


(defconstant EM_SETHANDLE 188)	; was 1036
;;					#xbc
#+MAINWIN16
(defconstant EM_SETHANDLE  (+ WM_USER 12))


(defconstant EM_GETHANDLE 189)	; was 1037
;;					#xbd
#+MAINWIN16
(defconstant EM_GETHANDLE  (+ WM_USER 13))


(defconstant EM_GETTHUMB 190)	; was 1038
;;					#xbe


(defconstant EM_LINELENGTH 193)	; was 1041
;;					#xc1
#+MAINWIN16
(defconstant EM_LINELENGTH (+ WM_USER 17))


(defconstant EM_REPLACESEL 194)	; was 1042
;;					#xc2
#+MAINWIN16
(defconstant EM_REPLACESEL (+ WM_USER 18))

;(defconstant EM_SETFONT 1043)
;;					#x413


(defconstant EM_GETLINE 196)		; was 1044
;;					#xc4
#+MAINWIN16
(defconstant EM_GETLINE    (+ WM_USER 20))


(defconstant EM_LIMITTEXT 197)	; was 1045
;;					#xc5
#+MAINWIN16
(defconstant EM_LIMITTEXT  (+ WM_USER 21))


(defconstant EM_CANUNDO 198)		; was 1046
;;					#xc6
#+MAINWIN16
(defconstant EM_CANUNDO    (+ WM_USER 22))


(defconstant EM_UNDO 199)		; was 1047
;;					#xc7
#+MAINWIN16
(defconstant EM_UNDO       (+ WM_USER 23))


(defconstant EM_FMTLINES 200)	; was 1048
;;					#xc8
#+MAINWIN16
(defconstant EM_FMTLINES   (+ WM_USER 24))


(defconstant EM_LINEFROMCHAR 201)	; was 1049

(defconstant EM_SETREADONLY 207) ; #xcf

;;					#xc9
#+MAINWIN16
(defconstant EM_LINEFROMCHAR (+ WM_USER 25))

;(defconstant EM_SETWORDBREAK 1050)
;;					#x41a


(defconstant EM_SETTABSTOPS 203)	; was 1051
;;					#xcb
#+MAINWIN16
(defconstant EM_SETTABSTOPS        (+ WM_USER 27))


(defconstant EM_SETPASSWORDCHAR 204)	; was 1052
;;					#xcc
#+MAINWIN16
(defconstant EM_SETPASSWORDCHAR (+ WM_USER 28))


(defconstant EM_EMPTYUNDOBUFFER 205)	; was 1053
;;					#xcd
#+MAINWIN16
(defconstant EM_EMPTYUNDOBUFFER  (+ WM_USER 29))



(defconstant EM_EXGETSEL (+ WM_USER 52))
(defconstant EM_EXSETSEL (+ WM_USER 55))
(defconstant EM_EXLIMITTEXT (+ WM_USER 53))
(defconstant EM_EXLINEFROMCHAR (+ WM_USER 54))
(defconstant EM_GETTEXTRANGE (+ WM_USER 75))
(defconstant EM_FINDTEXTEX (+ WM_USER 79))


;(defconstant EM_MSGMAX 1054)
;;					#x41e

(defconstant BS_PUSHBUTTON 0)
(defconstant BS_DEFPUSHBUTTON 1)
(defconstant BS_CHECKBOX 2)
(defconstant BS_AUTOCHECKBOX 3)
(defconstant BS_RADIOBUTTON 4)
(defconstant BS_3STATE 5)
(defconstant BS_AUTO3STATE 6)
(defconstant BS_GROUPBOX 7)
(defconstant BS_USERBUTTON 8)
(defconstant BS_AUTORADIOBUTTON 9)
;(defconstant BS_PUSHBOX 10)
;;					#xa

(defconstant BS_OWNERDRAW 11)
;;					#xb

(defconstant BS_LEFTTEXT 32)
;;					#x20

(defconstant BN_CLICKED 0)
(defconstant BN_PAINT 1)
(defconstant BN_HILITE 2)
(defconstant BN_UNHILITE 3)
(defconstant BN_DISABLE 4)
(defconstant BN_DOUBLECLICKED 5)

(defconstant BM_GETCHECK 240)		; was 1024
;;					#xf0
#+MAINWIN16
(defconstant BM_GETCHECK  WM_USER)


(defconstant BM_SETCHECK 241)		; was 1025
;;					#xf1
#+MAINWIN16
(defconstant BM_SETCHECK (+ WM_USER 1))


(defconstant BM_GETSTATE 242)		; was 1026
;;					#xf2
#+MAINWIN16
(defconstant BM_GETSTATE (+ WM_USER 2))


(defconstant BM_SETSTATE 243)		; was 1027
;;					#xf3
#+MAINWIN16
(defconstant BM_SETSTATE (+ WM_USER 3))


(defconstant BM_SETSTYLE 244)		; was 1028
;;					#xf4
#+MAINWIN16
(defconstant BM_SETSTYLE (+ WM_USER 4))

(defconstant SS_LEFT 0)
(defconstant SS_CENTER 1)
(defconstant SS_RIGHT 2)
(defconstant SS_ICON 3)
(defconstant SS_BLACKRECT 4)
(defconstant SS_GRAYRECT 5)
(defconstant SS_WHITERECT 6)
(defconstant SS_BLACKFRAME 7)
(defconstant SS_GRAYFRAME 8)
(defconstant SS_WHITEFRAME 9)
(defconstant SS_USERITEM 10)
;;					#xa

(defconstant SS_SIMPLE 11)
;;					#xb

(defconstant SS_LEFTNOWORDWRAP 12)
;;					#xc

(defconstant SS_NOPREFIX 128)
;;					#x80

(defconstant DS_ABSALIGN 1)
(defconstant DS_SYSMODAL 2)
(defconstant DS_LOCALEDIT 32)		; #x20
(defconstant DS_SETFONT 64)		; #x40
(defconstant DS_MODALFRAME 128)		; #x80
(defconstant DS_NOIDLEMSG 256)		; #x100
(defconstant DS_SETFOREGROUND 512)	; #x200
(defconstant DS_3DLOOK 4)

(defconstant DM_GETDEFID 1024)		; #x400
#+MAINWIN16
(defconstant DM_GETDEFID  WM_USER)


(defconstant DM_SETDEFID 1025)
;;					#x401
#+MAINWIN16
(defconstant DM_SETDEFID (+ WM_USER 1))

(defconstant DC_HASDEFID 21323)
;;					#x534b

(defconstant DLGC_WANTARROWS 1)
(defconstant DLGC_WANTTAB 2)
(defconstant DLGC_WANTALLKEYS 4)
(defconstant DLGC_WANTMESSAGE 4)
(defconstant DLGC_HASSETSEL 8)
(defconstant DLGC_DEFPUSHBUTTON 16)
;;					#x10

(defconstant DLGC_UNDEFPUSHBUTTON 32)
;;					#x20

(defconstant DLGC_RADIOBUTTON 64)
;;					#x40

(defconstant DLGC_WANTCHARS 128)
;;					#x80

(defconstant DLGC_STATIC 256)
;;					#x100

(defconstant DLGC_BUTTON 8192)
;;					#x2000

(defconstant LB_CTLCODE 0)
(defconstant LB_OKAY 0)
(defconstant LB_ERR -1)
;;					#xffffffff

(defconstant LB_ERRSPACE -2)
;;					#xfffffffe

(defconstant LBN_ERRSPACE -2)
;;					#xfffffffe

(defconstant LBN_SELCHANGE 1)
(defconstant LBN_DBLCLK 2)
(defconstant LBN_SELCANCEL 3)
(defconstant LBN_SETFOCUS 4)
(defconstant LBN_KILLFOCUS 5)

(defconstant LB_ADDSTRING 384)		; was 1025
;;					#x180
#+MAINWIN16
(defconstant LB_ADDSTRING (+ WM_USER 1))


(defconstant LB_INSERTSTRING 385)		; was 1026
;;					#x181
#+MAINWIN16
(defconstant LB_INSERTSTRING (+ WM_USER 2))


(defconstant LB_DELETESTRING 386)		; was 1027
;;					#x182
#+MAINWIN16
(defconstant LB_DELETESTRING (+ WM_USER 3))


(defconstant LB_RESETCONTENT 388)		; was 1029
;;					#x184
#+MAINWIN16
(defconstant LB_RESETCONTENT (+ WM_USER 5))


(defconstant LB_SETSEL 389)		; was 1030
;;					#x185
#+MAINWIN16
(defconstant LB_SETSEL    (+ WM_USER 6))


(defconstant LB_SETCURSEL 390)		; was 1031
;;					#x186
#+MAINWIN16
(defconstant LB_SETCURSEL (+ WM_USER 7))


(defconstant LB_GETSEL 391)		; was 1032
;;					#x187
#+MAINWIN16
(defconstant LB_GETSEL    (+ WM_USER 8))


(defconstant LB_GETCURSEL 392)		; was 1033
;;					#x188
#+MAINWIN16
(defconstant LB_GETCURSEL (+ WM_USER 9))


(defconstant LB_GETTEXT 393)		; was 1034
;;					#x189
#+MAINWIN16
(defconstant LB_GETTEXT   (+ WM_USER 10))


(defconstant LB_GETTEXTLEN 394)		; was 1035
;;					#x18a
#+MAINWIN16
(defconstant LB_GETTEXTLEN (+ WM_USER 11))


(defconstant LB_GETCOUNT 395)		; was 1036
;;					#x18b
#+MAINWIN16
(defconstant LB_GETCOUNT  (+ WM_USER 12))


(defconstant LB_SELECTSTRING 396)		; was 1037
;;					#x18c
#+MAINWIN16
(defconstant LB_SELECTSTRING (+ WM_USER 13))


(defconstant LB_DIR 397)		; was 1038
;;					#x18d
#+MAINWIN16
(defconstant LB_DIR               (+ WM_USER 14))


(defconstant LB_GETTOPINDEX 398)		; was 1039
;;					#x18e
#+MAINWIN16
(defconstant LB_GETTOPINDEX       (+ WM_USER 15))


(defconstant LB_FINDSTRING 399)		; was 1040
;;					#x18f
#+MAINWIN16
(defconstant LB_FINDSTRING (+ WM_USER 16))


(defconstant LB_GETSELCOUNT 400)		; was 1041
;;					#x190
#+MAINWIN16
(defconstant LB_GETSELCOUNT       (+ WM_USER 17))


(defconstant LB_GETSELITEMS 401)		; was 1042
;;					#x191
#+MAINWIN16
(defconstant LB_GETSELITEMS       (+ WM_USER 18))


(defconstant LB_SETTABSTOPS 402)		; was 1043
;;					#x192
#+MAINWIN16
(defconstant LB_SETTABSTOPS (+ WM_USER 19))


(defconstant LB_GETHORIZONTALEXTENT 403)		; was 1044
;;					#x193
#+MAINWIN16
(defconstant LB_GETHORIZONTALEXTENT (+ WM_USER 20))


(defconstant LB_SETHORIZONTALEXTENT 404)		; was 1045
;;					#x194
#+MAINWIN16
(defconstant LB_SETHORIZONTALEXTENT (+ WM_USER 21))


(defconstant LB_SETCOLUMNWIDTH 405)		; was 1046
;;					#x195
#+MAINWIN16
(defconstant LB_SETCOLUMNWIDTH (+ WM_USER 22))


(defconstant LB_SETTOPINDEX 407)		; was 1048
;;					#x197
#+MAINWIN16
(defconstant LB_SETTOPINDEX       (+ WM_USER 24))


(defconstant LB_GETITEMRECT 408)		; was 1049
;;					#x198
#+MAINWIN16
(defconstant LB_GETITEMRECT       (+ WM_USER 25))


(defconstant LB_GETITEMDATA 409)		; was 1050
;;					#x199
#+MAINWIN16
(defconstant LB_GETITEMDATA (+ WM_USER 26))


(defconstant LB_SETITEMDATA 410)		; was 1051
;;					#x19a
#+MAINWIN16
(defconstant LB_SETITEMDATA (+ WM_USER 27))


(defconstant LB_SELITEMRANGE 411)		; was 1052
;;					#x19b
#+MAINWIN16
(defconstant LB_SELITEMRANGE (+ WM_USER 28))


(defconstant LB_MSGMAX 424)		; was 1057
;;					#x1a8

(defconstant LBS_NOTIFY 1)
(defconstant LBS_SORT 2)
(defconstant LBS_NOREDRAW 4)
(defconstant LBS_MULTIPLESEL 8)
(defconstant LBS_OWNERDRAWFIXED 16)
;;					#x10

(defconstant LBS_OWNERDRAWVARIABLE 32)
;;					#x20

(defconstant LBS_HASSTRINGS 64)
;;					#x40

(defconstant LBS_USETABSTOPS 128)
;;					#x80

(defconstant LBS_NOINTEGRALHEIGHT 256)
;;					#x100

(defconstant LBS_MULTICOLUMN 512)
;;					#x200

(defconstant LBS_WANTKEYBOARDINPUT 1024)
;;					#x400

(defconstant LBS_EXTENDEDSEL 2048)
;;					#x800

(defconstant LBS_STANDARD 10485763)

(defconstant CB_OKAY 0)
(defconstant CB_ERR -1)
;;					#xffffffff

(defconstant CB_ERRSPACE -2)
;;					#xfffffffe

(defconstant CBN_ERRSPACE -1)
;;					#xffffffff

(defconstant CBN_SELCHANGE 1)
(defconstant CBN_DBLCLK 2)
(defconstant CBN_SETFOCUS 3)
(defconstant CBN_KILLFOCUS 4)
(defconstant CBN_EDITCHANGE 5)
(defconstant CBN_EDITUPDATE 6)
(defconstant CBN_DROPDOWN 7)
(defconstant CBN_CLOSEUP 8) 
(defconstant CBS_SIMPLE 1)
(defconstant CBS_DROPDOWN 2)
(defconstant CBS_DROPDOWNLIST 3)
(defconstant CBS_OWNERDRAWFIXED 16)
;;					#x10

(defconstant CBS_OWNERDRAWVARIABLE 32)
;;					#x20

(defconstant CBS_AUTOHSCROLL 64)
;;					#x40

(defconstant CBS_OEMCONVERT 128)
;;					#x80

(defconstant CBS_SORT 256)
;;					#x100

(defconstant CBS_HASSTRINGS 512)
;;					#x200

(defconstant CBS_NOINTEGRALHEIGHT 1024)
;;					#x400


(defconstant CB_GETEDITSEL 320)		; was 1024
;;					#x140
#+MAINWIN16
(defconstant CB_GETEDITSEL     WM_USER)


(defconstant CB_LIMITTEXT 321)		; was 1025
;;					#x141
#+MAINWIN16
(defconstant CB_LIMITTEXT   (+ WM_USER 1))


(defconstant CB_SETEDITSEL 322)		; was 1026
;;					#x142
#+MAINWIN16
(defconstant CB_SETEDITSEL  (+ WM_USER 2))


(defconstant CB_ADDSTRING 323)		; was 1027
;;					#x143
#+MAINWIN16
(defconstant CB_ADDSTRING   (+ WM_USER 3))


(defconstant CB_DELETESTRING 324)		; was 1028
;;					#x144
#+MAINWIN16
(defconstant CB_DELETESTRING        (+ WM_USER 4))


(defconstant CB_DIR 325)		; was 1029
;;					#x145
#+MAINWIN16
(defconstant CB_DIR           (+ WM_USER 5))


(defconstant CB_GETCOUNT 326)		; was 1030
;;					#x146
#+MAINWIN16
(defconstant CB_GETCOUNT    (+ WM_USER 6))


(defconstant CB_GETCURSEL 327)		; was 1031
;;					#x147
#+MAINWIN16
(defconstant CB_GETCURSEL   (+ WM_USER 7))


(defconstant CB_GETLBTEXT 328)		; was 1032
;;					#x148
#+MAINWIN16
(defconstant CB_GETLBTEXT   (+ WM_USER 8))


(defconstant CB_GETLBTEXTLEN 329)		; was 1033
;;					#x149
#+MAINWIN16
(defconstant CB_GETLBTEXTLEN        (+ WM_USER 9))


(defconstant CB_INSERTSTRING 330)		; was 1034
;;					#x14a
#+MAINWIN16
(defconstant CB_INSERTSTRING  (+ WM_USER 10))


(defconstant CB_RESETCONTENT 331)		; was 1035
;;					#x14b
#+MAINWIN16
(defconstant CB_RESETCONTENT        (+ WM_USER 11))


(defconstant CB_FINDSTRING 332)		; was 1036
;;					#x14c
#+MAINWIN16
(defconstant CB_FINDSTRING  (+ WM_USER 12))


(defconstant CB_SELECTSTRING 333)		; was 1037
;;					#x14d
#+MAINWIN16
(defconstant CB_SELECTSTRING        (+ WM_USER 13))


(defconstant CB_SETCURSEL 334)		; was 1038
;;					#x14e
#+MAINWIN16
(defconstant CB_SETCURSEL   (+ WM_USER 14))


(defconstant CB_SHOWDROPDOWN 335)		; was 1039
;;					#x14f
#+MAINWIN16
(defconstant CB_SHOWDROPDOWN  (+ WM_USER 15))


(defconstant CB_GETITEMDATA 336)		; was 1040
;;					#x150
#+MAINWIN16
(defconstant CB_GETITEMDATA   (+ WM_USER 16))


(defconstant CB_SETITEMDATA 337)		; was 1041
;;					#x151
#+MAINWIN16
(defconstant CB_SETITEMDATA   (+ WM_USER 17))

  
(defconstant CB_GETDROPPEDCONTROLRECT 338)		; was 1042
;;					#x152
#+MAINWIN16
(defconstant CB_GETDROPPEDCONTROLRECT (+ WM_USER 18))

#+aclpc
(defconstant CB_MSGMAX 347)		; was 1043
;;					#x15b

(defconstant SBS_HORZ 0)
(defconstant SBS_VERT 1)
(defconstant SBS_TOPALIGN 2)
(defconstant SBS_LEFTALIGN 2)
(defconstant SBS_BOTTOMALIGN 4)
(defconstant SBS_RIGHTALIGN 4)
(defconstant SBS_SIZEBOXTOPLEFTALIGN 2)
(defconstant SBS_SIZEBOXBOTTOMRIGHTALIGN 4)
(defconstant SBS_SIZEBOX 8)
(defconstant S_QUEUEEMPTY 0)
(defconstant S_THRESHOLD 1)
(defconstant S_ALLTHRESHOLD 2)
(defconstant S_NORMAL 0)
(defconstant S_LEGATO 1)
(defconstant S_STACCATO 2)
(defconstant S_PERIOD512 0)
(defconstant S_PERIOD1024 1)
(defconstant S_PERIOD2048 2)
(defconstant S_PERIODVOICE 3)
(defconstant S_WHITE512 4)
(defconstant S_WHITE1024 5)
(defconstant S_WHITE2048 6)
(defconstant S_WHITEVOICE 7)
(defconstant S_SERDVNA -1)
;;					#xffffffff

(defconstant S_SEROFM -2)
;;					#xfffffffe

(defconstant S_SERMACT -3)
;;					#xfffffffd

(defconstant S_SERQFUL -4)
;;					#xfffffffc

(defconstant S_SERBDNT -5)
;;					#xfffffffb

(defconstant S_SERDLN -6)
;;					#xfffffffa

(defconstant S_SERDCC -7)
;;					#xfffffff9

(defconstant S_SERDTP -8)
;;					#xfffffff8

(defconstant S_SERDVL -9)
;;					#xfffffff7

(defconstant S_SERDMD -10)
;;					#xfffffff6

(defconstant S_SERDSH -11)
;;					#xfffffff5

(defconstant S_SERDPT -12)
;;					#xfffffff4

(defconstant S_SERDFQ -13)
;;					#xfffffff3

(defconstant S_SERDDR -14)
;;					#xfffffff2

(defconstant S_SERDSR -15)
;;					#xfffffff1

(defconstant S_SERDST -16)
;;					#xfffffff0

(defconstant NOPARITY 0)
(defconstant ODDPARITY 1)
(defconstant EVENPARITY 2)
(defconstant MARKPARITY 3)
(defconstant SPACEPARITY 4)
(defconstant ONESTOPBIT 0)
(defconstant ONE5STOPBITS 1)
(defconstant TWOSTOPBITS 2)
;;(defconstant IGNORE 0)
(defconstant INFINITE 4294967295)		; was 65535

(defconstant CE_RXOVER 1)
(defconstant CE_OVERRUN 2)
(defconstant CE_RXPARITY 4)
(defconstant CE_FRAME 8)
(defconstant CE_BREAK 16)
;;					#x10

;(defconstant CE_CTSTO 32)
;;					#x20

;(defconstant CE_DSRTO 64)
;;					#x40

;(defconstant CE_RLSDTO 128)
;;					#x80

(defconstant CE_TXFULL 256)
;;					#x100

(defconstant CE_PTO 512)
;;					#x200

(defconstant CE_IOE 1024)
;;					#x400

(defconstant CE_DNS 2048)
;;					#x800

(defconstant CE_OOP 4096)
;;					#x1000

(defconstant CE_MODE 32768)
;;					#x8000

(defconstant IE_BADID -1)
;;					#xffffffff

(defconstant IE_OPEN -2)
;;					#xfffffffe

(defconstant IE_NOPEN -3)
;;					#xfffffffd

(defconstant IE_MEMORY -4)
;;					#xfffffffc

(defconstant IE_DEFAULT -5)
;;					#xfffffffb

(defconstant IE_HARDWARE -10)
;;					#xfffffff6

(defconstant IE_BYTESIZE -11)
;;					#xfffffff5

(defconstant IE_BAUDRATE -12)
;;					#xfffffff4

(defconstant EV_RXCHAR 1)
(defconstant EV_RXFLAG 2)
(defconstant EV_TXEMPTY 4)
(defconstant EV_CTS 8)
(defconstant EV_DSR 16)
;;					#x10

(defconstant EV_RLSD 32)
;;					#x20

(defconstant EV_BREAK 64)
;;					#x40

(defconstant EV_ERR 128)
;;					#x80

(defconstant EV_RING 256)
;;					#x100

(defconstant EV_PERR 512)
;;					#x200

(defconstant SETXOFF 1)
(defconstant SETXON 2)
(defconstant SETRTS 3)
(defconstant CLRRTS 4)
(defconstant SETDTR 5)
(defconstant CLRDTR 6)
(defconstant RESETDEV 7)
(defconstant LPTX 128)
;;					#x80

(defconstant HELP_CONTEXT 1)
(defconstant HELP_QUIT 2)
(defconstant HELP_INDEX 3)
(defconstant HELP_CONTENTS 3)
(defconstant HELP_HELPONHELP 4)
(defconstant HELP_SETINDEX 5)
(defconstant HELP_SETCONTENTS 5)
(defconstant HELP_CONTEXTPOPUP 8)
(defconstant HELP_FORCEFILE 9)
(defconstant HELP_KEY #x101)

(defconstant HELP_COMMAND #x102)

(defconstant HELP_PARTIALKEY #x105)

(defconstant HELP_MULTIKEY #x201)

(defconstant HELP_SETWINPOS #x203)

(defconstant WM_DDE_FIRST 992)
;;					#x3e0

(defconstant WM_DDE_INITIATE 992)
;;					#x3e0

(defconstant WM_DDE_TERMINATE 993)
;;					#x3e1

(defconstant WM_DDE_ADVISE 994)
;;					#x3e2

(defconstant WM_DDE_UNADVISE 995)
;;					#x3e3

(defconstant WM_DDE_ACK 996)
;;					#x3e4

(defconstant WM_DDE_DATA 997)
;;					#x3e5

(defconstant WM_DDE_REQUEST 998)
;;					#x3e6

(defconstant WM_DDE_POKE 999)
;;					#x3e7

(defconstant WM_DDE_EXECUTE 1000)
;;					#x3e8

(defconstant WM_DDE_LAST 1000)
;;					#x3e8

(defconstant CCHDEVICENAME 32)
;;					#x20

(defconstant DM_SPECVERSION 800)		; was 768
;;					#x320

(defconstant DM_ORIENTATION 1)
(defconstant DM_PAPERSIZE 2)
(defconstant DM_PAPERLENGTH 4)
(defconstant DM_PAPERWIDTH 8)
(defconstant DM_SCALE 16)
;;					#x10

(defconstant DM_COPIES 256)
;;					#x100

(defconstant DM_DEFAULTSOURCE 512)
;;					#x200

(defconstant DM_PRINTQUALITY 1024)
;;					#x400

(defconstant DM_COLOR 2048)
;;					#x800

(defconstant DM_DUPLEX 4096)
;;					#x1000

(defconstant DMORIENT_PORTRAIT 1)
(defconstant DMORIENT_LANDSCAPE 2)
(defconstant DMPAPER_LETTER 1)
(defconstant DMPAPER_LETTERSMALL 2)
(defconstant DMPAPER_TABLOID 3)
(defconstant DMPAPER_LEDGER 4)
(defconstant DMPAPER_LEGAL 5)
(defconstant DMPAPER_STATEMENT 6)
(defconstant DMPAPER_EXECUTIVE 7)
(defconstant DMPAPER_A3 8)
(defconstant DMPAPER_A4 9)
(defconstant DMPAPER_A4SMALL 10)
;;					#xa

(defconstant DMPAPER_A5 11)
;;					#xb

(defconstant DMPAPER_B4 12)
;;					#xc

(defconstant DMPAPER_B5 13)
;;					#xd

(defconstant DMPAPER_FOLIO 14)
;;					#xe

(defconstant DMPAPER_QUARTO 15)
;;					#xf

(defconstant DMPAPER_10X14 16)
;;					#x10

(defconstant DMPAPER_11X17 17)
;;					#x11

(defconstant DMPAPER_NOTE 18)
;;					#x12

(defconstant DMPAPER_ENV_9 19)
;;					#x13

(defconstant DMPAPER_ENV_10 20)
;;					#x14

(defconstant DMPAPER_ENV_11 21)
;;					#x15

(defconstant DMPAPER_ENV_12 22)
;;					#x16

(defconstant DMPAPER_ENV_14 23)
;;					#x17

(defconstant DMPAPER_CSHEET 24)
;;					#x18

(defconstant DMPAPER_DSHEET 25)
;;					#x19

(defconstant DMPAPER_ESHEET 26)
;;					#x1a

(defconstant DMPAPER_LAST 41)		; was 26
;;					#x29

(defconstant DMPAPER_USER 256)
;;					#x100

(defconstant DMBIN_UPPER 1)
(defconstant DMBIN_ONLYONE 1)
(defconstant DMBIN_LOWER 2)
(defconstant DMBIN_MIDDLE 3)
(defconstant DMBIN_MANUAL 4)
(defconstant DMBIN_ENVELOPE 5)
(defconstant DMBIN_ENVMANUAL 6)
(defconstant DMBIN_AUTO 7)
(defconstant DMBIN_TRACTOR 8)
(defconstant DMBIN_SMALLFMT 9)
(defconstant DMBIN_LARGEFMT 10)
;;					#xa

(defconstant DMBIN_LARGECAPACITY 11)
;;					#xb

(defconstant DMBIN_CASSETTE 14)
;;					#xe

(defconstant DMBIN_LAST 14)
;;					#xe

(defconstant DMBIN_USER 256)
;;					#x100

(defconstant DMRES_DRAFT -1)
;;					#xffffffff

(defconstant DMRES_LOW -2)
;;					#xfffffffe

(defconstant DMRES_MEDIUM -3)
;;					#xfffffffd

(defconstant DMRES_HIGH -4)
;;					#xfffffffc

(defconstant DMCOLOR_MONOCHROME 1)
(defconstant DMCOLOR_COLOR 2)
(defconstant DMDUP_SIMPLEX 1)
(defconstant DMDUP_VERTICAL 2)
(defconstant DMDUP_HORIZONTAL 3)
(defconstant DM_UPDATE 1)
(defconstant DM_COPY 2)
(defconstant DM_PROMPT 4)
(defconstant DM_MODIFY 8)
(defconstant DM_IN_BUFFER 8)
(defconstant DM_IN_PROMPT 4)
(defconstant DM_OUT_BUFFER 2)
(defconstant DM_OUT_DEFAULT 1)
(defconstant DC_FIELDS 1)
(defconstant DC_PAPERS 2)
(defconstant DC_PAPERSIZE 3)
(defconstant DC_MINEXTENT 4)
(defconstant DC_MAXEXTENT 5)
(defconstant DC_BINS 6)
(defconstant DC_DUPLEX 7)
(defconstant DC_SIZE 8)
(defconstant DC_EXTRA 9)
(defconstant DC_VERSION 10)
;;					#xa

(defconstant DC_DRIVER 11)
;;					#xb

;(defconstant PROC_EXTDEVICEMODE 90)
;;					#x5a

;(defconstant PROC_DEVICECAPABILITIES 91)
;;					#x5b

;(defconstant PROC_OLDDEVICEMODE 13)
;;					#xd

;(defconstant CTLTYPES 12)
;;					#xc

;(defconstant CTLDESCR 22)
;;					#x16

;(defconstant CTLCLASS 20)
;;					#x14

;(defconstant CTLTITLE 94)
;;					#x5e

; openfilename constants

(defconstant OFN_READONLY #x00000001)

(defconstant OFN_OVERWRITEPROMPT #x00000002)

(defconstant OFN_HIDEREADONLY #x00000004)

(defconstant OFN_NOCHANGEDIR #x00000008)

(defconstant OFN_SHOWHELP #x00000010)

(defconstant OFN_ENABLEHOOK #x00000020)

(defconstant OFN_ENABLETEMPLATE #x00000040)

(defconstant OFN_ENABLETEMPLATEHANDLE #x00000080)

(defconstant OFN_NOVALIDATE #x00000100)

(defconstant OFN_ALLOWMULTISELECT #x00000200)

(defconstant OFN_EXTENSIONDIFFERENT #x00000400)

(defconstant OFN_PATHMUSTEXIST #x00000800)

(defconstant OFN_FILEMUSTEXIST #x00001000)

(defconstant OFN_CREATEPROMPT #x00002000)

(defconstant OFN_SHAREAWARE #x00004000)

(defconstant OFN_NOREADONLYRETURN #x00008000)

(defconstant OFN_NOTESTFILECREATE #x00010000)

(defconstant OFN_NONETWORKBUTTON #x00020000)

(defconstant OFN_NOLONGNAMES #x00040000)

;;; ddeml defs

; ***** conversation states (usState) *****


(defconstant XST_NULL 0)	; quiescent states
(defconstant XST_INCOMPLETE 1)
(defconstant XST_CONNECTED 2)
(defconstant XST_INIT1 3)	; mid-initiation states
(defconstant XST_INIT2 4)
(defconstant XST_REQSENT 5)	; active conversation states
(defconstant XST_DATARCVD 6)
(defconstant XST_POKESENT 7)
(defconstant XST_POKEACKRCVD 8)
(defconstant XST_EXECSENT 9)
(defconstant XST_EXECACKRCVD 10)
;;					#xa

(defconstant XST_ADVSENT 11)
;;					#xb

(defconstant XST_UNADVSENT 12)
;;					#xc

(defconstant XST_ADVACKRCVD 13)
;;					#xd

(defconstant XST_UNADVACKRCVD 14)
;;					#xe

(defconstant XST_ADVDATASENT 15)
;;					#xf

(defconstant XST_ADVDATAACKRCVD 16)
;;					#x10

;;; used in LOWORD(dwData1) of XTYP_ADVREQ callbacks... 

(defconstant CADV_LATEACK #x0000FFFF)

;;; ***** conversation status bits (fsStatus) *****


(defconstant ST_CONNECTED #x0001)

(defconstant ST_ADVISE #x0002)

(defconstant ST_ISLOCAL #x0004)

(eval-when (compile load eval)
(defconstant ST_BLOCKED #x0008)
)

(defconstant ST_CLIENT #x0010)

(defconstant ST_TERMINATED #x0020)

(defconstant ST_INLIST #x0040)

(eval-when (compile load eval)
  (defconstant ST_BLOCKNEXT #x0080)
  )


(defconstant ST_ISSELF #x0100)


;;; DDE constants for wStatus field


(defconstant DDE_FACK #x00008000)

(defconstant DDE_FBUSY #x00004000)

(defconstant DDE_FDEFERUPD #x00004000)

(defconstant DDE_FACKREQ #x00008000)

(defconstant DDE_FRELEASE #x00002000)

(defconstant DDE_FREQUESTED #x00001000)

(defconstant DDE_FAPPSTATUS #x000000ff)

(defconstant DDE_FNOTPROCESSED #x00000000)

(defconstant DDE_FACKRESERVED
   #x00003f00 ; (lognot (logior dde_fack dde_fbusy dde_fappstatus))
   )
(defconstant DDE_FADVRESERVED
   #x00003fff ; (lognot (logior dde_fackreq dde_fdeferupd))
   )
(defconstant DDE_FDATRESERVED
   #x00004fff ; (lognot (logior dde_fackreq dde_frelease dde_frequested))
   )
(defconstant DDE_FPOKRESERVED
   #x0000dfff ; (lognot dde_frelease)
   )

;;; ***** message filter hook types *****


(defconstant MSGF_DDEMGR #x00008001)

;;; ***** codepage constants ****


(defconstant CP_WINANSI 1004)	; default codepage for windows
;;					#x3ec
					; & old DDE convs.

(defconstant CP_WINUNICODE 1200)
;;					#x4b0

;;; ***** transaction types *****


(defconstant XTYPF_NOBLOCK #x00000002)	; CBR_BLOCK will not work

(defconstant XTYPF_NODATA #x00000004)	; DDE_FDEFERUPD

(defconstant XTYPF_ACKREQ #x00000008)	; DDE_FACKREQ

(defconstant XCLASS_MASK #x0000FC00)

(defconstant XCLASS_BOOL #x00001000)

(defconstant XCLASS_DATA #x00002000)

(defconstant XCLASS_FLAGS #x00004000)

(defconstant XCLASS_NOTIFICATION #x00008000)

(defconstant XTYP_ERROR
   #x00008002 ; (logior #x0000 xclass_notification xtypf_noblock)
   )
(defconstant XTYP_ADVDATA
   #x00004010 ; (logior #x0010 xclass_flags)
   )
(defconstant XTYP_ADVREQ
   #x00002022 ; (logior #x0020 xclass_data xtypf_noblock)
   )
(defconstant XTYP_ADVSTART
   #x00001030 ; (logior #x0030 xclass_bool)
   )
(defconstant XTYP_ADVSTOP
   #x00008040 ; (logior #x0040 xclass_notification)
   )
(defconstant XTYP_EXECUTE
   #x00004050 ; (logior #x0050 xclass_flags)
   )
(defconstant XTYP_CONNECT
   #x00001062 ; (logior #x0060 xclass_bool xtypf_noblock)
   )
(defconstant XTYP_CONNECT_CONFIRM
   #x00008072 ; (logior #x0070 xclass_notification xtypf_noblock)
   )
(defconstant XTYP_XACT_COMPLETE
   #x00008080 ; (logior #x0080 xclass_notification)
   )
(defconstant XTYP_POKE
   #x00004090 ; (logior #x0090 xclass_flags)
   )
(defconstant XTYP_REGISTER
   #x000080A2 ; (logior #x00A0 xclass_notification xtypf_noblock)
   )
(defconstant XTYP_REQUEST
   #x000020B0 ; (logior #x00B0 xclass_data)
   )
(defconstant XTYP_DISCONNECT
   #x000080C2 ; (logior #x00C0 xclass_notification xtypf_noblock)
   )
(defconstant XTYP_UNREGISTER
   #x000080D2 ; (logior #x00D0 xclass_notification xtypf_noblock)
   )
(defconstant XTYP_WILDCONNECT
   #x000020E2 ; (logior #x00E0 xclass_data xtypf_noblock)
   )


(defconstant XTYP_MASK #x000000F0)

(defconstant XTYP_SHIFT 4)	; shift to turn XTYP_ into an index
;;; ***** Timeout constants *****


(defconstant TIMEOUT_ASYNC #xFFFFFFFF)

;;; ***** Transaction ID constants *****


(defconstant QID_SYNC #xFFFFFFFF)


(defconstant CBR_BLOCK #xffffffff)

;;; Callback filter flags for use with standard apps.


(defconstant CBF_FAIL_SELFCONNECTIONS #x00001000)

(defconstant CBF_FAIL_CONNECTIONS #x00002000)

(defconstant CBF_FAIL_ADVISES #x00004000)

(defconstant CBF_FAIL_EXECUTES #x00008000)

(defconstant CBF_FAIL_POKES #x00010000)

(defconstant CBF_FAIL_REQUESTS #x00020000)

(defconstant CBF_FAIL_ALLSVRXACTIONS #x0003f000)


(defconstant CBF_SKIP_CONNECT_CONFIRMS #x00040000)

(defconstant CBF_SKIP_REGISTRATIONS #x00080000)

(defconstant CBF_SKIP_UNREGISTRATIONS #x00100000)

(defconstant CBF_SKIP_DISCONNECTS #x00200000)

(defconstant CBF_SKIP_ALLNOTIFICATIONS #x003c0000)

;;; Application command flags


(defconstant APPCMD_CLIENTONLY #x00000010)

(defconstant APPCMD_FILTERINITS #x00000020)

(defconstant APPCMD_MASK #x00000FF0)

;;; Application classification flags


(defconstant APPCLASS_STANDARD #x00000000)

(defconstant APPCLASS_MASK #x0000000F)


(defconstant EC_ENABLEALL 0)
(defconstant EC_ENABLEONE ST_BLOCKNEXT)

(defconstant EC_DISABLE ST_BLOCKED)

(defconstant EC_QUERYWAITING 2)

(defconstant DNS_REGISTER #x0001)

(defconstant DNS_UNREGISTER #x0002)

(defconstant DNS_FILTERON #x0004)

(defconstant DNS_FILTEROFF #x0008)


(defconstant HDATA_APPOWNED #x0001)


(defconstant DMLERR_NO_ERROR 0)	; must be 0

(defconstant DMLERR_FIRST #x4000)


(defconstant DMLERR_ADVACKTIMEOUT #x4000)

(defconstant DMLERR_BUSY #x4001)

(defconstant DMLERR_DATAACKTIMEOUT #x4002)

(defconstant DMLERR_DLL_NOT_INITIALIZED #x4003)

(defconstant DMLERR_DLL_USAGE #x4004)

(defconstant DMLERR_EXECACKTIMEOUT #x4005)

(defconstant DMLERR_INVALIDPARAMETER #x4006)

(defconstant DMLERR_LOW_MEMORY #x4007)

(defconstant DMLERR_MEMORY_ERROR #x4008)

(defconstant DMLERR_NOTPROCESSED #x4009)

(defconstant DMLERR_NO_CONV_ESTABLISHED #x400a)

(defconstant DMLERR_POKEACKTIMEOUT #x400b)

(defconstant DMLERR_POSTMSG_FAILED #x400c)

(defconstant DMLERR_REENTRANCY #x400d)

(defconstant DMLERR_SERVER_DIED #x400e)

(defconstant DMLERR_SYS_ERROR #x400f)

(defconstant DMLERR_UNADVACKTIMEOUT #x4010)

(defconstant DMLERR_UNFOUND_QUEUE_ID #x4011)


(defconstant DMLERR_LAST #x4011)

;;mm If this constant changes, change it in ntwinh also
(defconstant MAX_PATH 260)		; cac 9aug95

(defconstant FILE_SHARE_READ 1)
(defconstant FILE_SHARE_WRITE 2)
(defconstant OPEN_EXISTING 3)
(defconstant INVALID_HANDLE_VALUE -1)

(defconstant GENERIC_READ #x80000000)
(defconstant GENERIC_WRITE #x40000000)
(defconstant CREATE_ALWAYS 2)
(defconstant FILE_ATTRIBUTE_NORMAL #x80)


;; added by cac for dvg 18nov96
(defconstant SPI_GETWORKAREA 48)
(defconstant SPI_GETNONCLIENTMETRICS 41)


(defconstant FR_MATCHCASE #x4)
(defconstant FR_WHOLEWORD #x2)
(defconstant FR_DOWN #x1)


;;;;;;;;;;;; mci constants

;; Offsets
(defconstant MCI_STRING_OFFSET 512)
(defconstant MCI_VD_OFFSET 1024)
(defconstant MCI_CD_OFFSET 1088)
(defconstant MCI_WAVE_OFFSET 1152)
(defconstant MCI_SEQ_OFFSET 1216)

;; MCI Command Message Names

(defconstant MCI_OPEN #x0803)
(defconstant MCI_CLOSE #x0804)
(defconstant MCI_ESCAPE #x0805)
(defconstant MCI_PLAY #x0806)
(defconstant MCI_SEEK #x0807)
(defconstant MCI_STOP #x0808)
(defconstant MCI_PAUSE #x0809)
(defconstant MCI_INFO #x080A)
(defconstant MCI_GETDEVCAPS #x080B)
(defconstant MCI_SPIN #x080C)
(defconstant MCI_SET #x080D)
(defconstant MCI_STEP #x080E)
(defconstant MCI_RECORD #x080F)
(defconstant MCI_SYSINFO #x0810)
(defconstant MCI_BREAK #x0811)
(defconstant MCI_SOUND #x0812)
(defconstant MCI_SAVE #x0813)
(defconstant MCI_STATUS #x0814)
(defconstant MCI_CUE #x0830)
(defconstant MCI_REALIZE #x0840)
(defconstant MCI_WINDOW #x0841)
(defconstant MCI_PUT #x0842)
(defconstant MCI_WHERE #x0843)
(defconstant MCI_FREEZE #x0844)
(defconstant MCI_UNFREEZE #x0845)
(defconstant MCI_LOAD #x0850)
(defconstant MCI_CUT #x0851)
(defconstant MCI_COPY #x0852)
(defconstant MCI_PASTE #x0853)
(defconstant MCI_UPDATE #x0854)
(defconstant MCI_RESUME #x0855)
(defconstant MCI_DELETE #x0856)

;; open flags
(defconstant MCI_OPEN_SHAREABLE #x00000100)
(defconstant MCI_OPEN_ELEMENT #x00000200)
(defconstant MCI_OPEN_ALIAS #x00000400)
(defconstant MCI_OPEN_ELEMENT_ID #x00000800)
(defconstant MCI_OPEN_TYPE_ID #x00001000)
(defconstant MCI_OPEN_TYPE #x00002000)
;; open flags for animation
(defconstant MCI_ANIM_OPEN_WS #x00010000)
(defconstant MCI_ANIM_OPEN_PARENT #x00020000)
(defconstant MCI_ANIM_OPEN_NOSTATIC #x00040000)
;; open flags for overlay
(defconstant MCI_OVLY_OPEN_WS #x00010000)
(defconstant MCI_OVLY_OPEN_PARENT #x00020000)
;; open flags for wave
(defconstant MCI_WAVE_OPEN_BUFFER #x00010000)

;; play flags
(defconstant MCI_NOTIFY #x1)
(defconstant MCI_WAIT #x2)
(defconstant MCI_FROM #x4)
(defconstant MCI_TO #x8)
(defconstant MCI_TRACK #x10)
;; play flags for animation
(defconstant MCI_ANIM_PLAY_SPEED #x00010000)
(defconstant MCI_ANIM_PLAY_REVERSE #x00020000)
(defconstant MCI_ANIM_PLAY_FAST #x00040000)
(defconstant MCI_ANIM_PLAY_SLOW #x00080000)
(defconstant MCI_ANIM_PLAY_SCAN #x00100000)
;; play flags for video-disc
(defconstant MCI_VD_PLAY_REVERSE #x00010000)
(defconstant MCI_VD_PLAY_FAST #x00020000)
(defconstant MCI_VD_PLAY_SPEED #x00040000)
(defconstant MCI_VD_PLAY_SCAN #x00080000)
(defconstant MCI_VD_PLAY_SLOW #x00100000)

;; record flags
(defconstant MCI_RECORD_INSERT #x100)
(defconstant MCI_RECORD_OVERWRITE #x200)

;; step flags
(defconstant MCI_VD_STEP_FRAMES #x00010000)
(defconstant MCI_VD_STEP_REVERSE #x00020000)
(defconstant MCI_ANIM_STEP_REVERSE #x00010000)
(defconstant MCI_ANIM_STEP_FRAMES #x00020000)

;; save flags
(defconstant MCI_SAVE_FILE #x100)

;; seek flags
(defconstant MCI_SEEK_TO_START #x0100)
(defconstant MCI_SEEK_TO_END #x0200)
(defconstant MCI_VD_SEEK_REVERSE #x00010000)

;; sysinfo flags
(defconstant MCI_SYSINFO_QUANTITY #x100)
(defconstant MCI_SYSINFO_OPEN #x200)
(defconstant MCI_SYSINFO_NAME #x400)
(defconstant MCI_SYSINFO_INSTALLNAME #x800)

;; status flags
(defconstant MCI_STATUS_LENGTH #x1)
(defconstant MCI_STATUS_POSITION #x2)
(defconstant MCI_STATUS_NUMBER_OF_TRACKS #x3)
(defconstant MCI_STATUS_MODE #x4)
(defconstant MCI_STATUS_MEDIA_PRESENT #x5)
(defconstant MCI_STATUS_TIME_FORMAT #x6)
(defconstant MCI_STATUS_READY #x7)
(defconstant MCI_STATUS_CURRENT_TRACK #x8)
(defconstant MCI_STATUS_ITEM #x100)
(defconstant MCI_STATUS_START #x200)
(defconstant MCI_ANIM_STATUS_SPEED #x4001)
(defconstant MCI_ANIM_STATUS_FORWARD #x4002)
(defconstant MCI_ANIM_STATUS_HWND #x4003)
(defconstant MCI_ANIM_STATUS_HPAL #x4004)
(defconstant MCI_ANIM_STATUS_STRETCH #x4005)
(defconstant MCI_WAVE_INPUT #x00400000)
(defconstant MCI_WAVE_OUTPUT #x00800000)
(defconstant MCI_VD_STATUS_SPEED #x00004002)
(defconstant MCI_VD_STATUS_FORWARD #x00004003)
(defconstant MCI_VD_STATUS_MEDIA_TYPE #x00004004)
(defconstant MCI_VD_STATUS_SIDE #x00004005)
(defconstant MCI_VD_STATUS_DISC_SIZE #x00004006)
(defconstant MCI_WAVE_STATUS_FORMATTAG #x00004001)
(defconstant MCI_WAVE_STATUS_CHANNELS #x00004002)
(defconstant MCI_WAVE_STATUS_SAMPLESPERSEC #x00004003)
(defconstant MCI_WAVE_STATUS_AVGBYTESPERSEC #x00004004)
(defconstant MCI_WAVE_STATUS_BLOCKALIGN #x00004005)
(defconstant MCI_WAVE_STATUS_BITSPERSAMPLE #x00004006)
(defconstant MCI_WAVE_STATUS_LEVEL #x00004007)
(defconstant MCI_OVLY_STATUS_HWND #x00004001)
(defconstant MCI_OVLY_STATUS_STRETCH #x00004002)

;; status modes
(defconstant MCI_MODE_NOT_READY (+ MCI_STRING_OFFSET 12))
(defconstant MCI_MODE_STOP (+ MCI_STRING_OFFSET 13))
(defconstant MCI_MODE_PLAY (+ MCI_STRING_OFFSET 14))
(defconstant MCI_MODE_RECORD (+ MCI_STRING_OFFSET 15))
(defconstant MCI_MODE_SEEK (+ MCI_STRING_OFFSET 16))
(defconstant MCI_MODE_PAUSE (+ MCI_STRING_OFFSET 17))
(defconstant MCI_MODE_OPEN (+ MCI_STRING_OFFSET 18))
(defconstant MCI_VD_MODE_PARK (+ MCI_VD_OFFSET 1))
(defconstant MCI_VD_MEDIA_CLV (+ MCI_VD_OFFSET 2))
(defconstant MCI_VD_MEDIA_CAV (+ MCI_VD_OFFSET 3))
(defconstant MCI_VD_MEDIA_OTHER (+ MCI_VD_OFFSET 4))

;; status time formats
(defconstant MCI_FORMAT_MILLISECONDS 0)
(defconstant MCI_FORMAT_HMS 1)
(defconstant MCI_FORMAT_MSF 2)
(defconstant MCI_FORMAT_FRAMES 3)
(defconstant MCI_FORMAT_SMPTE_24 4)
(defconstant MCI_FORMAT_SMPTE_25 5)
(defconstant MCI_FORMAT_SMPTE_30 6)
(defconstant MCI_FORMAT_SMPTE_30DROP 7)
(defconstant MCI_FORMAT_BYTES 8)
(defconstant MCI_FORMAT_SAMPLES 9)
(defconstant MCI_FORMAT_TMSF 10)

;; set command flags
(defconstant MCI_SET_DOOR_OPEN #x00000100)
(defconstant MCI_SET_DOOR_CLOSED #x00000200)
(defconstant MCI_SET_TIME_FORMAT #x00000400)
(defconstant MCI_SET_AUDIO #x00000800)
(defconstant MCI_SET_VIDEO #x00001000)
(defconstant MCI_SET_ON #x00002000)
(defconstant MCI_SET_OFF #x00004000)
(defconstant MCI_WAVE_SET_FORMATTAG #x00010000)
(defconstant MCI_WAVE_SET_CHANNELS #x00020000)
(defconstant MCI_WAVE_SET_SAMPLESPERSEC #x00040000)
(defconstant MCI_WAVE_SET_AVGBYTESPERSEC #x00080000)
(defconstant MCI_WAVE_SET_BLOCKALIGN #x00100000)
(defconstant MCI_WAVE_SET_BITSPERSAMPLE #x00200000)
(defconstant MCI_WAVE_SET_ANYINPUT #x04000000)
(defconstant MCI_WAVE_SET_ANYOUTPUT #x08000000)
(defconstant MCI_SEQ_SET_TEMPO #x00010000)
(defconstant MCI_SEQ_SET_PORT #x00020000)
(defconstant MCI_SEQ_SET_SLAVE #x00040000)
(defconstant MCI_SEQ_SET_MASTER #x00080000)
(defconstant MCI_SEQ_SET_OFFSET #x01000000)
(defconstant MCI_SET_AUDIO_ALL #x00000000)
(defconstant MCI_SET_AUDIO_LEFT #x00000001)
(defconstant MCI_SET_AUDIO_RIGHT #x00000002)
(defconstant MCI_SEQ_FORMAT_SONGPTR #x400)
(defconstant MCI_SEQ_FILE #x400)
(defconstant MCI_SEQ_MIDI #x400)
(defconstant MCI_SEQ_SMPTE #x400)
(defconstant MCI_SEQ_NONE 65533)
(defconstant TIME_PERIODIC 1)
(defconstant TIME_ONESHOT 0)

;; getdevcaps request item types
(defconstant MCI_GETDEVCAPS_ITEM #x100)
(defconstant MCI_GETDEVCAPS_CAN_RECORD #x1)
(defconstant MCI_GETDEVCAPS_HAS_AUDIO #x2)
(defconstant MCI_GETDEVCAPS_HAS_VIDEO #x3)
(defconstant MCI_GETDEVCAPS_DEVICE_TYPE #x4)
(defconstant MCI_GETDEVCAPS_USES_FILES #x5)
(defconstant MCI_GETDEVCAPS_COMPOUND_DEVICE #x6)
(defconstant MCI_GETDEVCAPS_CAN_EJECT #x7)
(defconstant MCI_GETDEVCAPS_CAN_PLAY #x8)
(defconstant MCI_GETDEVCAPS_CAN_SAVE #x9)
;; videodisk flags
(defconstant MCI_VD_GETDEVCAPS_CLV #x0)
(defconstant MCI_VD_GETDEVCAPS_CAV #x0)
;; videodisk items
(defconstant MCI_VD_GETDEVCAPS_CAN_REVERSE #x2)
(defconstant MCI_VD_GETDEVCAPS_FAST_RATE #x3)
(defconstant MCI_VD_GETDEVCAPS_SLOW_RATE #x4)
(defconstant MCI_VD_GETDEVCAPS_NORMAL_RATE #x5)
;; wave items
(defconstant MCI_WAVE_GETDEVCAPS_INPUTS #x1)
(defconstant MCI_WAVE_GETDEVCAPS_OUTPUTS #x2)
;; animation items
(defconstant MCI_ANIM_GETDEVCAPS_CAN_REVERSE #x1)
(defconstant MCI_ANIM_GETDEVCAPS_FAST_RATE #x2)
(defconstant MCI_ANIM_GETDEVCAPS_SLOW_RATE #x3)
(defconstant MCI_ANIM_GETDEVCAPS_NORMAL_RATE #x4)
(defconstant MCI_ANIM_GETDEVCAPS_PALETTES #x6)
(defconstant MCI_ANIM_GETDEVCAPS_CAN_STRETCH #x7)
(defconstant MCI_ANIM_GETDEVCAPS_MAX_WINDOWS #x8)
;; overlay items
(defconstant MCI_OVLY_GETDEVCAPS_CAN_STRETCH #x1)
(defconstant MCI_OVLY_GETDEVCAPS_CAN_FREEZE #x2)
(defconstant MCI_OVLY_GETDEVCAPS_MAX_WINDOWS #x3)

;; device type constants
(defconstant MCI_DEVTYPE_VCR (+ MCI_STRING_OFFSET 1))
(defconstant MCI_DEVTYPE_VIDEODISC (+ MCI_STRING_OFFSET 2))
(defconstant MCI_DEVTYPE_OVERLAY (+ MCI_STRING_OFFSET 3))
(defconstant MCI_DEVTYPE_CD_AUDIO (+ MCI_STRING_OFFSET 4))
(defconstant MCI_DEVTYPE_DAT (+ MCI_STRING_OFFSET 5))
(defconstant MCI_DEVTYPE_SCANNER (+ MCI_STRING_OFFSET 6))
(defconstant MCI_DEVTYPE_ANIMATION (+ MCI_STRING_OFFSET 7))
(defconstant MCI_DEVTYPE_DIGITAL_VIDEO (+ MCI_STRING_OFFSET 8))
(defconstant MCI_DEVTYPE_OTHER (+ MCI_STRING_OFFSET 9))
(defconstant MCI_DEVTYPE_WAVEFORM_AUDIO (+ MCI_STRING_OFFSET 10))
(defconstant MCI_DEVTYPE_SEQUENCER (+ MCI_STRING_OFFSET 11))

;; callback notification
(defconstant MM_MCINOTIFY #x3b9)
(defconstant MCI_NOTIFY_SUCCESSFUL #x0001)
(defconstant MCI_NOTIFY_SUPERSEDED #x0002)
(defconstant MCI_NOTIFY_ABORTED #x0004)
(defconstant MCI_NOTIFY_FAILURE #x0008)

;;;;;;;;; end mci constants


;;;;;;;;;; trackbar constants
; from trackbah.cl
;



(defconstant TRACKBAR_CLASSA "msctls_trackbar32")

(defconstant  TRACKBAR_CLASS TRACKBAR_CLASSA)

(defconstant TBS_AUTOTICKS #x0001)
(defconstant TBS_VERT #x0002)
(defconstant TBS_HORZ #x0000)
(defconstant TBS_TOP #x0004)
(defconstant TBS_BOTTOM #x0000)
(defconstant TBS_LEFT #x0004)
(defconstant TBS_RIGHT #x0000)
(defconstant TBS_BOTH #x0008)
(defconstant TBS_NOTICKS #x0010)
(defconstant TBS_ENABLESELRANGE #x0020)
(defconstant TBS_FIXEDLENGTH #x0040)
(defconstant TBS_NOTHUMB #x0080)

(defconstant TBM_GETPOS WM_USER)

(defconstant TBM_GETRANGEMIN (+ WM_USER 1))
(defconstant TBM_GETRANGEMAX (+ WM_USER 2))
(defconstant TBM_GETTIC (+ WM_USER 3))
(defconstant TBM_SETTIC (+ WM_USER 4))
(defconstant TBM_SETPOS (+ WM_USER 5))
(defconstant TBM_SETRANGE (+ WM_USER 6))
(defconstant TBM_SETRANGEMIN (+ WM_USER 7))
(defconstant TBM_SETRANGEMAX (+ WM_USER 8))
(defconstant TBM_CLEARTICS (+ WM_USER 9))
(defconstant TBM_SETSEL (+ WM_USER 10))
(defconstant TBM_SETSELSTART (+ WM_USER 11))
(defconstant TBM_SETSELEND (+ WM_USER 12))
(defconstant TBM_GETPTICS (+ WM_USER 14))
(defconstant TBM_GETTICPOS (+ WM_USER 15))
(defconstant TBM_GETNUMTICS (+ WM_USER 16))
(defconstant TBM_GETSELSTART (+ WM_USER 17))
(defconstant TBM_GETSELEND (+ WM_USER 18))
(defconstant TBM_CLEARSEL (+ WM_USER 19))
(defconstant TBM_SETTICFREQ (+ WM_USER 20))
(defconstant TBM_SETPAGESIZE (+ WM_USER 21))
(defconstant TBM_GETPAGESIZE (+ WM_USER 22))
(defconstant TBM_SETLINESIZE (+ WM_USER 23))
(defconstant TBM_GETLINESIZE (+ WM_USER 24))
(defconstant TBM_GETTHUMBRECT (+ WM_USER 25))
(defconstant TBM_GETCHANNELRECT (+ WM_USER 26))
(defconstant TBM_SETTHUMBLENGTH (+ WM_USER 27))
(defconstant TBM_GETTHUMBLENGTH (+ WM_USER 28))

(defconstant TB_LINEUP 0)
(defconstant TB_LINEDOWN 1)
(defconstant TB_PAGEUP 2)
(defconstant TB_PAGEDOWN 3)
(defconstant TB_THUMBPOSITION 4)
(defconstant TB_THUMBTRACK 5)
(defconstant TB_TOP 6)
(defconstant TB_BOTTOM 7)
(defconstant TB_ENDTRACK 8)


;;;;; form imaglish.cl

;; These  four are from winuser.h
(defconstant IMAGE_BITMAP 0)
(defconstant IMAGE_ICON 1)
(defconstant IMAGE_CURSOR 2)
(defconstant IMAGE_ENHMETAFILE 3)

(defconstant CLR_NONE #xFFFFFFFF)
(defconstant CLR_DEFAULT #xFF000000)


(defconstant ILC_MASK #x0001)
(defconstant ILC_COLOR #x00FE)
(defconstant ILC_COLORDDB #x00FE)
(defconstant ILC_COLOR4 #x0004)
(defconstant ILC_COLOR8 #x0008)
(defconstant ILC_COLOR16 #x0010)
(defconstant ILC_COLOR24 #x0018)
(defconstant ILC_COLOR32 #x0020)
(defconstant ILC_PALETTE #x0800)


(defconstant ILD_NORMAL #x0000)
(defconstant ILD_TRANSPARENT #x0001)
(defconstant ILD_MASK #x0010)
(defconstant ILD_IMAGE #x0020)
(defconstant ILD_BLEND #x000E)
(defconstant ILD_BLEND25 #x0002)
(defconstant ILD_BLEND50 #x0004)
(defconstant ILD_OVERLAYMASK #x0F00)


(defconstant ILD_SELECTED ILD_BLEND50)
(defconstant ILD_FOCUS ILD_BLEND25)
(defconstant CLR_HILIGHT CLR_DEFAULT)


;; from inith.cl

(defconstant NM_FIRST 0)       ;; generic to all controls
(defconstant NM_LAST -99)

;; ------------------------------------------------------------------------
;; Generic WM_NOTIFY notification codes

(defconstant NM_OUTOFMEMORY (- NM_FIRST 1))
(defconstant NM_CLICK (- NM_FIRST 2))
(defconstant NM_DBLCLK (- NM_FIRST 3))
(defconstant NM_RETURN (- NM_FIRST 4))
(defconstant NM_RCLICK (- NM_FIRST 5))
(defconstant NM_RDBLCLK (- NM_FIRST 6))
(defconstant NM_SETFOCUS (- NM_FIRST 7))
(defconstant NM_KILLFOCUS (- NM_FIRST 8))
(defconstant NM_STARTWAIT (- NM_FIRST 9))
(defconstant NM_ENDWAIT (- NM_FIRST 10))
(defconstant NM_BTNCLK (- NM_FIRST 10))

;; ------------------------------------------------------------------------
;; COMMON CONTROL STYLES

(defconstant CCS_TOP #x00000001)
(defconstant CCS_NOMOVEY #x00000002)
(defconstant CCS_BOTTOM #x00000003)
(defconstant CCS_NORESIZE #x00000004)
(defconstant CCS_NOPARENTALIGN #x00000008)
(defconstant CCS_NOHILITE #x00000010)
(defconstant CCS_ADJUSTABLE #x00000020)
(defconstant CCS_NODIVIDER #x00000040)




(defconstant LVM_FIRST #x1000)
(defconstant LVN_FIRST -100)

(defconstant WC_LISTVIEW "SysListView32")

(defconstant LVS_ICON #x0000)
(defconstant LVS_REPORT #x0001)
(defconstant LVS_SMALLICON #x0002)
(defconstant LVS_LIST #x0003)
(defconstant LVS_TYPEMASK #x0003)
(defconstant LVS_SINGLESEL #x0004)
(defconstant LVS_SHOWSELALWAYS #x0008)
(defconstant LVS_SORTASCENDING #x0010)
(defconstant LVS_SORTDESCENDING #x0020)
(defconstant LVS_SHAREIMAGELISTS #x0040)
(defconstant LVS_NOLABELWRAP #x0080)
(defconstant LVS_AUTOARRANGE #x0100)
(defconstant LVS_EDITLABELS #x0200)
(defconstant LVS_NOSCROLL #x2000)

(defconstant LVS_TYPESTYLEMASK #xfc00)

(defconstant LVS_ALIGNTOP #x0000)
(defconstant LVS_ALIGNLEFT #x0800)
(defconstant LVS_ALIGNMASK #x0c00)

(defconstant LVS_OWNERDRAWFIXED #x0400)
(defconstant LVS_NOCOLUMNHEADER #x4000)
(defconstant LVS_NOSORTHEADER #x8000)

(defconstant LVM_GETBKCOLOR (+ LVM_FIRST  0))



(defconstant LVM_SETBKCOLOR (+ LVM_FIRST 1))



(defconstant LVM_GETIMAGELIST (+ LVM_FIRST 2))



(defconstant LVSIL_NORMAL 0)
(defconstant LVSIL_SMALL 1)
(defconstant LVSIL_STATE 2)

(defconstant LVM_SETIMAGELIST (+ LVM_FIRST 3))



(defconstant LVM_GETITEMCOUNT (+ LVM_FIRST 4))



(defconstant LVIF_TEXT #x0001)
(defconstant LVIF_IMAGE #x0002)
(defconstant LVIF_PARAM #x0004)
(defconstant LVIF_STATE #x0008)

(defconstant LVIS_FOCUSED #x0001)
(defconstant LVIS_SELECTED #x0002)
(defconstant LVIS_CUT #x0004)
(defconstant LVIS_DROPHILITED #x0008)
(defconstant LVIS_LINK #x0040)

(defconstant LVIS_OVERLAYMASK #x0F00)
(defconstant LVIS_STATEIMAGEMASK #xF000)

 ;; ??? was ((i) << 12)


;; huh???
;;(defconstant LPSTR_TEXTCALLBACK (pc::bignum-to-unsigned-long -1))

(defconstant I_IMAGECALLBACK -1)

(defconstant LVM_GETITEM (+ LVM_FIRST 5))



(defconstant LVM_SETITEM (+ LVM_FIRST 6))



(defconstant LVM_INSERTITEM (+ LVM_FIRST 7))



(defconstant LVM_DELETEITEM (+ LVM_FIRST 8))



(defconstant LVM_DELETEALLITEMS (+ LVM_FIRST 9))



(defconstant LVM_GETCALLBACKMASK (+ LVM_FIRST 10))



(defconstant LVM_SETCALLBACKMASK (+ LVM_FIRST 11))



(defconstant LVNI_ALL #x0000)
(defconstant LVNI_FOCUSED #x0001)
(defconstant LVNI_SELECTED #x0002)
(defconstant LVNI_CUT #x0004)
(defconstant LVNI_DROPHILITED #x0008)
(defconstant LVNI_PREVIOUS #x0020)

(defconstant LVNI_ABOVE #x0100)
(defconstant LVNI_BELOW #x0200)
(defconstant LVNI_TOLEFT #x0400)
(defconstant LVNI_TORIGHT #x0800)

(defconstant LVM_GETNEXTITEM (+ LVM_FIRST 12))



(defconstant LVFI_PARAM #x0001)
(defconstant LVFI_STRING #x0002)
(defconstant LVFI_PARTIAL #x0008)
(defconstant LVFI_WRAP #x0020)
(defconstant LVFI_NEARESTXY #x0040)



(defconstant LVM_FINDITEM (+ LVM_FIRST 13))



(defconstant LVIR_BOUNDS 0)
(defconstant LVIR_ICON 1)
(defconstant LVIR_LABEL 2)
(defconstant LVIR_SELECTBOUNDS 3)

(defconstant LVM_GETITEMRECT (+ LVM_FIRST 14))



(defconstant LVM_SETITEMPOSITION (+ LVM_FIRST 15))



(defconstant LVM_GETITEMPOSITION (+ LVM_FIRST 16))



(defconstant LVM_GETSTRINGWIDTH (+ LVM_FIRST 17))



(defconstant LVHT_NOWHERE #x0001)
(defconstant LVHT_ONITEMICON #x0002)
(defconstant LVHT_ONITEMLABEL #x0004)
(defconstant LVHT_ONITEMSTATEICON #x0008)

(defconstant LVHT_ONITEM
 (logior LVHT_ONITEMICON LVHT_ONITEMLABEL LVHT_ONITEMSTATEICON))

(defconstant LVHT_ABOVE #x0008)
(defconstant LVHT_BELOW #x0010)
(defconstant LVHT_TORIGHT #x0020)
(defconstant LVHT_TOLEFT #x0040)



(defconstant LVM_HITTEST (+ LVM_FIRST 18))



(defconstant LVM_ENSUREVISIBLE (+ LVM_FIRST 19))



(defconstant LVM_SCROLL (+ LVM_FIRST 20))



(defconstant LVM_REDRAWITEMS (+ LVM_FIRST 21))



(defconstant LVA_DEFAULT #x0000)
(defconstant LVA_ALIGNLEFT #x0001)
(defconstant LVA_ALIGNTOP #x0002)
(defconstant LVA_ALIGNRIGHT #x0003)
(defconstant LVA_ALIGNBOTTOM #x0004)
(defconstant LVA_SNAPTOGRID #x0005)

(defconstant LVA_SORTASCENDING #x0100)
(defconstant LVA_SORTDESCENDING #x0200)

(defconstant LVM_ARRANGE (+ LVM_FIRST 22))



(defconstant LVM_EDITLABEL (+ LVM_FIRST 23))



(defconstant LVM_GETEDITCONTROL (+ LVM_FIRST 24))





(defconstant LVCF_FMT #x0001)
(defconstant LVCF_WIDTH #x0002)
(defconstant LVCF_TEXT #x0004)
(defconstant LVCF_SUBITEM #x0008)

(defconstant LVCFMT_LEFT 0)
(defconstant LVCFMT_RIGHT 1)
(defconstant LVCFMT_CENTER 2)

(defconstant LVM_GETCOLUMN (+ LVM_FIRST 25))



(defconstant LVM_SETCOLUMN (+ LVM_FIRST 26))



(defconstant LVM_INSERTCOLUMN (+ LVM_FIRST 27))



(defconstant LVM_DELETECOLUMN (+ LVM_FIRST 28))



(defconstant LVM_GETCOLUMNWIDTH (+ LVM_FIRST 29))



(defconstant LVSCW_AUTOSIZE -1)
(defconstant LVSCW_AUTOSIZE_USEHEADER -2)

(defconstant LVM_SETCOLUMNWIDTH (+ LVM_FIRST 30))



(defconstant LVM_CREATEDRAGIMAGE (+ LVM_FIRST 33))



(defconstant LVM_GETVIEWRECT (+ LVM_FIRST 34))



(defconstant LVM_GETTEXTCOLOR (+ LVM_FIRST 35))



(defconstant LVM_SETTEXTCOLOR (+ LVM_FIRST 36))



(defconstant LVM_GETTEXTBKCOLOR (+ LVM_FIRST 37))



(defconstant LVM_SETTEXTBKCOLOR (+ LVM_FIRST 38))



(defconstant LVM_GETTOPINDEX (+ LVM_FIRST 39))



(defconstant LVM_GETCOUNTPERPAGE (+ LVM_FIRST 40))



(defconstant LVM_GETORIGIN (+ LVM_FIRST 41))



(defconstant LVM_UPDATE (+ LVM_FIRST 42))



(defconstant LVM_SETITEMSTATE (+ LVM_FIRST 43))



(defconstant LVM_GETITEMSTATE (+ LVM_FIRST 44))



(defconstant LVM_GETITEMTEXT (+ LVM_FIRST 45))



(defconstant LVM_SETITEMTEXT (+ LVM_FIRST 46))



(defconstant LVM_SETITEMCOUNT (+ LVM_FIRST 47))



#| augh
typedef int (CALLBACK *PFNLVCOMPARE)(LPARAM LPARAM LPARAM);
|#

(defconstant LVM_SORTITEMS (+ LVM_FIRST 48))



(defconstant LVM_SETITEMPOSITION32 (+ LVM_FIRST 49))



(defconstant LVM_GETSELECTEDCOUNT (+ LVM_FIRST 50))



(defconstant LVM_GETITEMSPACING (+ LVM_FIRST 51))



(defconstant LVM_GETISEARCHSTRING (+ LVM_FIRST 52))





(defconstant LVN_ITEMCHANGING (- LVN_FIRST 0))
(defconstant LVN_ITEMCHANGED (- LVN_FIRST 1))
(defconstant LVN_INSERTITEM (- LVN_FIRST 2))
(defconstant LVN_DELETEITEM (- LVN_FIRST 3))
(defconstant LVN_DELETEALLITEMS (- LVN_FIRST 4))
(defconstant LVN_BEGINLABELEDIT (- LVN_FIRST 5))
(defconstant LVN_ENDLABELEDIT (- LVN_FIRST 6))
(defconstant LVN_COLUMNCLICK (- LVN_FIRST 8))
(defconstant LVN_BEGINDRAG (- LVN_FIRST 9))
(defconstant LVN_ENDDRAG (- LVN_FIRST 10))
(defconstant LVN_BEGINRDRAG (- LVN_FIRST 11))
(defconstant LVN_ENDRDRAG (- LVN_FIRST 12))

(defconstant LVN_PEN (- LVN_FIRST 20))

(defconstant LVN_GETDISPINFO (- LVN_FIRST 50))
(defconstant LVN_SETDISPINFO (- LVN_FIRST 51))

(defconstant LVIF_DI_SETITEM #x1000)



(defconstant LVN_KEYDOWN (- LVN_FIRST 55))


;;;;; from progresh.cl



(defconstant PROGRESS_CLASSA "msctls_progress32")
(defconstant  PROGRESS_CLASS PROGRESS_CLASSA)

(defconstant PBM_SETRANGE (+ WM_USER 1))
(defconstant PBM_SETPOS (+ WM_USER 2))
(defconstant PBM_DELTAPOS (+ WM_USER 3))
(defconstant PBM_SETSTEP (+ WM_USER 4))
(defconstant PBM_STEPIT (+ WM_USER 5))


;;;;; from starbarh.cl



(defconstant SBARS_SIZEGRIP #x0100)


(defconstant STATUSCLASSNAMEA "msctls_statusbar32")

(defconstant STATUSCLASSNAME STATUSCLASSNAMEA)

(defconstant SB_SETTEXTA (+ WM_USER 1))
(defconstant SB_GETTEXTA (+ WM_USER 2))
(defconstant SB_GETTEXTLENGTHA (+ WM_USER 3))

(defconstant SB_GETTEXT SB_GETTEXTA)
(defconstant SB_SETTEXT SB_SETTEXTA)
(defconstant SB_GETTEXTLENGTH SB_GETTEXTLENGTHA)

(defconstant SB_SETPARTS (+ WM_USER 4))
(defconstant SB_GETPARTS (+ WM_USER 6))
(defconstant SB_GETBORDERS (+ WM_USER 7))
(defconstant SB_SETMINHEIGHT (+ WM_USER 8))
(defconstant SB_SIMPLE (+ WM_USER 9))
(defconstant SB_GETRECT (+ WM_USER 10))

(defconstant SBT_OWNERDRAW #x1000)
(defconstant SBT_NOBORDERS #x0100)
(defconstant SBT_POPOUT #x0200)
(defconstant HBT_SPRING #x0400)


;;;;; from tabh.cl


(defconstant ODT_TAB 101)

(defconstant TCN_FIRST -550)
(defconstant TCN_LAST -580)

(defconstant WC_TABCONTROLA "SysTabControl32")
(defconstant  WC_TABCONTROL WC_TABCONTROLA)

(defconstant TCS_FORCEICONLEFT #x0010)
(defconstant TCS_FORCELABELLEFT #x0020)
(defconstant TCS_SHAREIMAGELISTS #x0040)
(defconstant TCS_TABS #x0000)
(defconstant TCS_BUTTONS #x0100)
(defconstant TCS_SINGLELINE #x0000)
(defconstant TCS_MULTILINE #x0200)
(defconstant TCS_RIGHTJUSTIFY #x0000)
(defconstant TCS_FIXEDWIDTH #x0400)
(defconstant TCS_RAGGEDRIGHT #x0800)
(defconstant TCS_FOCUSONBUTTONDOWN #x1000)
(defconstant TCS_OWNERDRAWFIXED #x2000)
(defconstant TCS_TOOLTIPS #x4000)
(defconstant TCS_FOCUSNEVER #x8000)

(defconstant TCM_FIRST #x1300)

(defconstant TCM_GETBKCOLOR (+ TCM_FIRST 0))



(defconstant TCM_SETBKCOLOR (+ TCM_FIRST 1))



(defconstant TCM_GETIMAGELIST (+ TCM_FIRST 2))



(defconstant TCM_SETIMAGELIST (+ TCM_FIRST 3))



(defconstant TCM_GETITEMCOUNT (+ TCM_FIRST 4))



(defconstant TCIF_TEXT #x0001)
(defconstant TCIF_IMAGE #x0002)
(defconstant TCIF_PARAM #x0008)





(defconstant TCM_GETITEMA (+ TCM_FIRST 5))
(defconstant TCM_GETITEM TCM_GETITEMA)



(defconstant TCM_SETITEMA (+ TCM_FIRST 6))
(defconstant TCM_SETITEM TCM_SETITEMA)



(defconstant TCM_INSERTITEMA (+ TCM_FIRST 7))
(defconstant TCM_INSERTITEM TCM_INSERTITEMA)



(defconstant TCM_DELETEITEM (+ TCM_FIRST 8))



(defconstant TCM_DELETEALLITEMS (+ TCM_FIRST 9))



(defconstant TCM_GETITEMRECT (+ TCM_FIRST 10))



(defconstant TCM_GETCURSEL (+ TCM_FIRST 11))



(defconstant TCM_SETCURSEL (+ TCM_FIRST 12))



(defconstant TCHT_NOWHERE #x0001)
(defconstant TCHT_ONITEMICON #x0002)
(defconstant TCHT_ONITEMLABEL #x0004)

(defconstant TCHT_ONITEM (logior TCHT_ONITEMICON TCHT_ONITEMLABEL))



(defconstant TCM_HITTEST (+ TCM_FIRST 13))



(defconstant TCM_SETITEMEXTRA (+ TCM_FIRST 14))



(defconstant TCM_ADJUSTRECT (+ TCM_FIRST 40))

;; chee 9jan97 5.0 added the pc:: here; not sure why it only seemed
;; to get called once (by rows-box)


(defconstant TCM_SETITEMSIZE (+ TCM_FIRST 41))



(defconstant TCM_REMOVEIMAGE (+ TCM_FIRST 42))



(defconstant TCM_SETPADDING (+ TCM_FIRST 43))



(defconstant TCM_GETROWCOUNT (+ TCM_FIRST 44))



(defconstant TCM_GETTOOLTIPS (+ TCM_FIRST 45))



(defconstant TCM_SETTOOLTIPS (+ TCM_FIRST 46))



(defconstant TCM_GETCURFOCUS (+ TCM_FIRST 47))



(defconstant TCM_SETCURFOCUS (+ TCM_FIRST 48))



(defconstant TCN_KEYDOWN (- TCN_FIRST 0))



(defconstant TCN_SELCHANGE (- TCN_FIRST 1))
(defconstant TCN_SELCHANGING (cl:- TCN_FIRST 2))


;;;;; updownh.cl



(defconstant UDN_FIRST -721)
(defconstant UDN_LAST -740)

(defconstant UPDOWN_CLASSA "msctls_updown32")
(defconstant  UPDOWN_CLASS UPDOWN_CLASSA)



(defconstant UD_MAXVAL #x7fff)
(defconstant UD_MINVAL (- UD_MAXVAL))

(defconstant UDS_WRAP #x0001)
(defconstant UDS_SETBUDDYINT #x0002)
(defconstant UDS_ALIGNRIGHT #x0004)
(defconstant UDS_ALIGNLEFT #x0008)
(defconstant UDS_AUTOBUDDY #x0010)
(defconstant UDS_ARROWKEYS #x0020)
(defconstant UDS_HORZ #x0040)
(defconstant UDS_NOTHOUSANDS #x0080)

(defconstant UDM_SETRANGE (+ WM_USER 101))
(defconstant UDM_GETRANGE (+ WM_USER 102))
(defconstant UDM_SETPOS (+ WM_USER 103))
(defconstant UDM_GETPOS (+ WM_USER 104))
(defconstant UDM_SETBUDDY (+ WM_USER 105))
(defconstant UDM_GETBUDDY (+ WM_USER 106))
(defconstant UDM_SETACCEL (+ WM_USER 107))
(defconstant UDM_GETACCEL (+ WM_USER 108))
(defconstant UDM_SETBASE (+ WM_USER 109))
(defconstant UDM_GETBASE (+ WM_USER 110))




(defconstant UDN_DELTAPOS (- UDN_FIRST 1))

(defconstant DDL_READWRITE #x0000)
(defconstant DDL_READONLY #x0001)
(defconstant DDL_HIDDEN #x0002)
(defconstant DDL_SYSTEM #x0004)
(defconstant DDL_DIRECTORY #x0010)
(defconstant DDL_ARCHIVE #x0020)
(defconstant DDL_POSTMSGS #x2000)
(defconstant DDL_DRIVES #x4000)
(defconstant DDL_EXCLUSIVE #x8000)

(defconstant TPM_RETURNCMD #x100)
(defconstant TPM_NONOTIFY #x80)
(defconstant TPM_LEFTBUTTON 0)
(defconstant TPM_RIGHTBUTTON 2)











(def-foreign-type byte :unsigned-char)
(def-foreign-type char :char)
(def-foreign-type dword :unsigned-long)
(def-foreign-type farproc (* :void))
(def-foreign-type int :long)
(def-foreign-type lhandle :long)
(def-foreign-type long :long)
(def-foreign-type lparam :long)
(def-foreign-type lpstr (* :char))
(def-foreign-type lpcstr lpstr)
(def-foreign-type lpvoid (* :void))
(def-foreign-type short :short)
(def-foreign-type uint :unsigned-long)
(def-foreign-type void :void)
(def-foreign-type wparam :long)
(def-foreign-type wndproc farproc)
(def-foreign-type word :unsigned-short)

(defmacro deflhandle (name ignorex)
  (declare (cl:ignore ignorex))
  `(ff::def-foreign-type ,name :long))

(cl:defmacro defwhandle (n i) `(deflhandle ,n ,i))
(defwhandle handle 0)
(defwhandle hbitmap 1)
(defwhandle hbrush 2)
(defwhandle hicon 3)
(defwhandle hdc 4)
(defwhandle hfont 5)
(defwhandle hmenu 6)
(defwhandle hpalette 7)
(defwhandle hpen 8)
(defwhandle hrgn 9)
(defwhandle hwnd 10)
(defwhandle hinstance 11)
(defwhandle hconvlist 12)
(defwhandle hconv 13)
(defwhandle hsz 14)
(defwhandle hddedata 15)

(DEF-FOREIGN-TYPE hcursor hicon)

(def-foreign-type rect 
    (:struct 
     (left   :long) 
     (top    :long) 
     (right  :long) 
     (bottom :long)))

(def-foreign-type lprect (* rect))

(def-foreign-type point 
    (:struct 
     (x :long) 
     (y :long)))

(def-foreign-type lppoint (* point))

(def-foreign-type msg
    (:struct
     (hwnd hwnd)
     (message uint)
     (wparam wparam)
     (lparam lparam)
     (time dword)
     (pt point)))

(def-foreign-type wndclassex
    (:struct 
     (cbsize uint)
     (style uint)
     (lpfnwndproc wndproc)
     (cbclsextra int)
     (cbwndextra int)
     (hinstance hinstance)
     (hicon hicon)
     (hcursor hcursor)
     (hbrbackground hbrush)
     (lpszmenuname lpcstr)
     (lpszclassname lpcstr)
     (hiconsm hicon)))

(def-foreign-type openfilename
    (:struct 
     (lstructsize long) 
     (hwndowner hwnd)
     (hinstance hinstance) 
     (lpstrfilter lpcstr)
     (lpstrcustomfilter lpstr) 
     (nmaxcustfilter long)
     (nfilterindex long) 
     (lpstrfile lpstr) 
     (nmaxfile long)
     (lpstrfiletitle lpstr)
     (nmaxfiletitle long)
     (lpstrinitialdir lpstr) 
     (lpstrtitle lpstr) 
     (flags long)
     (nfileoffset short) 
     (nfileextension short)
     (lpstrdefext lpcstr) 
     (lcustdata long) 
     (lpfnhook (* :void))
     (lptemplatename lpcstr)))

					; this version of openfilename is here to support ken's patches
;; do we still need/want it?

(def-foreign-type fi-openfilename
    (:struct
     (struct-size long)
     (owner hwnd)
     (hinst hinstance)
     (file-filter lpcstr)
     (custom-filter lpstr)
     (max-custom-filter long)
     (filter-index long)
     (selected-file lpstr)
     (max-file long)
     (file-title lpstr)
     (max-file-title long)
     (initial-dir lpstr)
     (window-title lpstr)
     (flags long)
     (file-offset short)
     (file-extension short)
     (default-extension lpcstr)
     (custom-data long)
     (callback-function (* :void))
     (template-name lpcstr)
     ))

(def-foreign-type size
    (:struct
     (cx long)
     (cy long)))

(def-foreign-type textmetric
   (:struct (tmHeight long)
    (tmAscent long)
    (tmDescent long)
    (tmInternalLeading long)
    (tmExternalLeading long)
    (tmAveCharWidth long)
    (tmMaxCharWidth long)
    (tmWeight long)
    (tmOverhang long)
    (tmDigitizedAspectX long)
    (tmDigitizedAspectY long)
    (tmFirstChar byte)
    (tmLastChar byte)
    (tmDefaultChar byte)
    (tmBreakChar byte)
    (tmItalic byte)
    (tmUnderlined byte)
    (tmStruckOut byte)
    (tmPitchAndFamily byte)
    (tmCharSet byte)))

(def-foreign-type wndclassex
  (:struct (cbSize uint)
   (style uint)
   (lpfnWndProc wndproc)
   (cbClsExtra int)
   (cbWndExtra int)
   (hInstance hinstance)
   (hIcon hicon)
   (hCursor hcursor)
   (hbrBackground hbrush)
   (lpszMenuName lpcstr)
   (lpszClassName lpcstr)
   (hIconSm hicon)))


(DEF-FOREIGN-TYPE RGBQUAD
                  (:STRUCT (RGBBLUE BYTE) (RGBGREEN BYTE) (RGBRED BYTE)
			   (RGBRESERVED BYTE)))

(DEF-FOREIGN-TYPE BITMAPINFOHEADER
                  (:STRUCT (BISIZE DWORD) (BIWIDTH LONG) (BIHEIGHT LONG)
                   (BIPLANES WORD) (BIBITCOUNT WORD) (BICOMPRESSION DWORD)
                   (BISIZEIMAGE DWORD) (BIXPELSPERMETER LONG)
                   (BIYPELSPERMETER LONG) (BICLRUSED DWORD)
                   (BICLRIMPORTANT DWORD)))

(DEF-FOREIGN-TYPE BITMAPINFO
                  (:STRUCT (BMIHEADER BITMAPINFOHEADER)
			   (BMICOLORS (:ARRAY RGBQUAD 1))))

(def-foreign-type openfilename
    (:struct 
     (lStructSize long)
     (hwndOwner hwnd)
     (hInstance hinstance)
     (lpstrFilter lpcstr)
     (lpstrCustomFilter lpstr)
     (nMaxCustFilter long)
     (nFilterIndex long)
     (lpstrFile lpstr)
     (nMaxFile long)
     (lpstrFileTitle lpstr)
     (nMaxFileTitle long)
     (lpstrInitialDir lpstr)
     (lpstrTitle lpstr)
     (Flags long)
     (nFileOffset short)
     (nFileExtension short)
     (lpstrDefExt lpcstr)
     (lCustData long)
     (lpfnHook (* :void))
     (lpTemplateName lpcstr)
     ))

(def-foreign-type fi-openfilename
    (:struct 
     (struct-size long)
     (owner hwnd)
     (hinst hinstance)
     (file-filter lpcstr)
     (custom-filter lpstr)
     (max-custom-filter long)
     (filter-index long)
     (selected-file lpstr)
     (max-file long)
     (file-title lpstr)
     (max-file-title long)
     (initial-dir lpstr)
     (window-title lpstr)
     (flags long)
     (file-offset short)
     (file-extension short)
     (default-extension lpcstr)
     (custom-data long)
     (callback-function (* :void))
     (template-name lpcstr)
     ))

