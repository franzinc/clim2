;; See the file LICENSE for the full license governing this code.
;;

(defpackage :x11
  ;; I don't know if this is OK (can we assume clim-utils?).  In any
  ;; case we want that definition of fintern.
  (:import-from :clim-utils #:fintern)
  ;; These next two import symbols when loading from clim.fasl.
  (:export #:int #:short)
  (:export #:screen #:depth #:visual #:colormap
	   #:pixmap #:window #:display)
  #+ignore
  (:size 2053)
  #+ignore
  (:export #:|XK-Down| #:|XK-E| #:|XK-ETH| #:|XK-Eacute| #:|XK-Ecircumflex|
	   #:|XK-Ediaeresis| #:|XK-Egrave| #:|XK-Eisu-Shift| #:|XK-Eisu-toggle|
	   #:|XK-End| #:|XK-Escape| #:|XK-Eth| #:|XK-Execute| #:|XK-F|
	   #:|XK-F1| #:|XK-F10| #:|XK-F11| #:|XK-F12| #:|XK-F13| #:|XK-F14|
	   #:|XK-F15| #:|XK-F16| #:|XK-F17| #:|XK-F18| #:|XK-F19| #:|XK-F2|
	   #:|XK-F20| #:|XK-F21| #:|XK-F22| #:|XK-F23| #:|XK-F24| #:|XK-F25|
	   #:|XK-F26| #:|XK-F27| #:|XK-F28| #:|XK-F29| #:|XK-F3| #:|XK-F30|
	   #:|XK-F31| #:|XK-F32| #:|XK-F33| #:|XK-F34| #:|XK-F35| #:|XK-F4|
	   #:|XK-F5| #:|XK-F6| #:|XK-F7| #:|XK-F8| #:|XK-F9| #:|XK-Find|
	   #:|XK-G| #:|XK-H| #:|XK-Hankaku| #:|XK-Help| #:|XK-Henkan|
	   #:|XK-Henkan-Mode| #:|XK-Hiragana| #:|XK-Hiragana-Katakana|
	   #:|XK-Home| #:|XK-Hyper-L| #:|XK-Hyper-R| #:|XK-I| #:|XK-Iacute|
	   #:|XK-Icircumflex| #:|XK-Idiaeresis| #:|XK-Igrave| #:|XK-Insert|
	   #:|XK-J| #:|XK-K| #:|XK-KP-0| #:|XK-KP-1| #:|XK-KP-2| #:|XK-KP-3|
	   #:|XK-KP-4| #:|XK-KP-5| #:|XK-KP-6| #:|XK-KP-7| #:|XK-KP-8|
	   #:|XK-KP-9| #:|XK-KP-Add| #:|XK-KP-Decimal| #:|XK-KP-Divide|
	   #:|XK-KP-Enter| #:|XK-KP-Equal| #:|XK-KP-F1| #:|XK-KP-F2|
	   #:|XK-KP-F3| #:|XK-KP-F4| #:|XK-KP-Multiply| #:|XK-KP-Separator|
	   #:|XK-KP-Space| #:|XK-KP-Subtract| #:|XK-KP-Tab| #:|XK-Kana-Lock|
	   #:|XK-Kana-Shift| #:|XK-Kanji| #:|XK-Katakana| #:|XK-L| #:|XK-L1|
	   #:|XK-L10| #:|XK-L2| #:|XK-L3| #:|XK-L4| #:|XK-L5| #:|XK-L6|
	   #:|XK-L7| #:|XK-L8| #:|XK-L9| #:|XK-Left| #:|XK-Linefeed|
	   #:|XK-M| #:|XK-Massyo| #:|XK-Menu| #:|XK-Meta-L| #:|XK-Meta-R|
	   #:|XK-Mode-switch| #:|XK-Muhenkan| #:|XK-Multi-key| #:|XK-N|
	   #:|XK-Next| #:|XK-Ntilde| #:|XK-Num-Lock| #:|XK-O| #:|XK-Oacute|
	   #:|XK-Ocircumflex| #:|XK-Odiaeresis| #:|XK-Ograve| #:|XK-Ooblique|
	   #:|XK-Otilde| #:|XK-P| #:|XK-Pause| #:|XK-Print| #:|XK-Prior|
	   #:|XK-Q| #:|XK-R| #:|XK-R1| #:|XK-R10| #:|XK-R11| #:|XK-R12|
	   #:|XK-R13| #:|XK-R14| #:|XK-R15| #:|XK-R2| #:|XK-R3| #:|XK-R4|
	   #:|XK-R5| #:|XK-R6| #:|XK-R7| #:|XK-R8| #:|XK-R9| #:|XK-Redo|
	   #:|XK-Return| #:|XK-Right| #:|XK-Romaji| #:|XK-S|
	   #:|XK-Scroll-Lock| #:|XK-Select| #:|XK-Shift-L| #:|XK-Shift-Lock|
	   #:|XK-Shift-R| #:|XK-Super-L| #:|XK-Super-R| #:|XK-T| #:|XK-THORN|
	   #:|XK-Tab| #:|XK-Thorn| #:|XK-Touroku| #:|XK-U| #:|XK-Uacute|
	   #:|XK-Ucircumflex| #:|XK-Udiaeresis| #:|XK-Ugrave| #:|XK-Undo|
	   #:|XK-Up| #:|XK-V| #:|XK-W| #:|XK-X| #:|XK-Y| #:|XK-Yacute|
	   #:|XK-Z| #:|XK-Zenkaku| #:|XK-Zenkaku-Hankaku| #:|XK-a|
	   #:|XK-aacute| #:|XK-acircumflex| #:|XK-acute| #:|XK-adiaeresis|
	   #:|XK-ae| #:|XK-agrave| #:|XK-ampersand| #:|XK-apostrophe|
	   #:|XK-aring| #:|XK-asciicircum| #:|XK-asciitilde| #:|XK-asterisk|
	   #:|XK-at| #:|XK-atilde| #:|XK-b| #:|XK-backslash| #:|XK-bar|
	   #:|XK-braceleft| #:|XK-braceright| #:|XK-bracketleft|
	   #:|XK-bracketright| #:|XK-brokenbar| #:|XK-c| #:|XK-ccedilla|
	   #:|XK-cedilla| #:|XK-cent| #:|XK-colon| #:|XK-comma|
	   #:|XK-copyright| #:|XK-currency| #:|XK-d| #:|XK-degree|
	   #:|XK-diaeresis| #:|XK-division| #:|XK-dollar| #:|XK-e|
	   #:|XK-eacute| #:|XK-ecircumflex| #:|XK-ediaeresis| #:|XK-egrave|
	   #:|XK-equal| #:|XK-eth| #:|XK-exclam| #:|XK-exclamdown| #:|XK-f|
	   #:|XK-g| #:|XK-grave| #:|XK-greater| #:|XK-guillemotleft|
	   #:|XK-guillemotright| #:|XK-h| #:|XK-hyphen| #:|XK-i| #:|XK-iacute|
	   #:|XK-icircumflex| #:|XK-idiaeresis| #:|XK-igrave| #:|XK-j|
	   #:|XK-k| #:|XK-l| #:|XK-less| #:|XK-m| #:|XK-macron|
	   #:|XK-masculine| #:|XK-minus| #:|XK-mu| #:|XK-multiply| #:|XK-n|
	   #:|XK-nobreakspace| #:|XK-notsign| #:|XK-ntilde| #:|XK-numbersign|
	   #:|XK-o| #:|XK-oacute| #:|XK-ocircumflex| #:|XK-odiaeresis|
	   #:|XK-ograve| #:|XK-onehalf| #:|XK-onequarter| #:|XK-onesuperior|
	   #:|XK-ordfeminine| #:|XK-oslash| #:|XK-otilde| #:|XK-p|
	   #:|XK-paragraph| #:|XK-parenleft| #:|XK-parenright| #:|XK-percent|
	   #:|XK-period| #:|XK-periodcentered| #:|XK-plus| #:|XK-plusminus|
	   #:|XK-q| #:|XK-question| #:|XK-questiondown| #:|XK-quotedbl|
	   #:|XK-quoteleft| #:|XK-quoteright| #:|XK-r| #:|XK-registered|
	   #:|XK-s| #:|XK-script-switch| #:|XK-section| #:|XK-semicolon|
	   #:|XK-slash| #:|XK-space| #:|XK-ssharp| #:|XK-sterling| #:|XK-t|
	   #:|XK-thorn| #:|XK-threequarters| #:|XK-threesuperior|
	   #:|XK-twosuperior| #:|XK-u| #:|XK-uacute| #:|XK-ucircumflex|
	   #:|XK-udiaeresis| #:|XK-ugrave| #:|XK-underscore| #:|XK-v| #:|XK-w|
	   #:|XK-x| #:|XK-y| #:|XK-yacute| #:|XK-ydiaeresis| #:|XK-yen|
	   #:|XK-z| #:|XKG-VoidSymbol| #:_xdisplay #:_xevent #:_xextension
	   #:_xextension-close-display #:_xextension-codes #:_xextension-copy-gc
	   #:_xextension-create-font #:_xextension-create-gc #:_xextension-error
	   #:_xextension-error-string #:_xextension-flush-gc
	   #:_xextension-free-font #:_xextension-free-gc #:_xextension-next
	   #:_xgc #:_xgc-dashes #:_xgc-dirty #:_xgc-ext-data #:_xgc-gid
	   #:_xgc-rects #:_xgc-values #:_xqevent #:_xqevent-event
	   #:_xqevent-next #:_xsqevent #:allocall #:allocnone #:allowexposures
	   #:alltemporary #:alreadygrabbed #:always #:anybutton #:anykey
	   #:anymodifier #:anypropertytype #:apl-keysyms #:arabic-keysyms
	   #:arcchord #:arcpieslice #:asyncboth #:asynckeyboard #:asyncpointer
	   #:atom #:autorepeatmodedefault #:autorepeatmodeoff #:autorepeatmodeon
	   #:badaccess #:badalloc #:badatom #:badcolor #:badcursor
	   #:baddrawable #:badfont #:badgc #:badidchoice #:badimplementation
	   #:badlength #:badmatch #:badname #:badpixmap #:badrequest
	   #:badvalue #:badwindow #:below #:bitmapfileinvalid #:bitmapnomemory
	   #:bitmapopenfailed #:bitmapsuccess #:bottomif #:bsl-union
	   #:bsl-union-b #:bsl-union-l #:bsl-union-s #:button1 #:button1mask
	   #:button1motionmask #:button2 #:button2mask #:button2motionmask
	   #:button3 #:button3mask #:button3motionmask #:button4 #:button4mask
	   #:button4motionmask #:button5 #:button5mask #:button5motionmask
	   #:buttonmotionmask #:buttonpress #:buttonpressmask #:buttonrelease
	   #:buttonreleasemask #:caddr-t #:callback-function-addr #:capbutt
	   #:capnotlast #:capprojecting #:capround #:centergravity #:char
	   #:circulatenotify #:circulaterequest #:clientmessage #:clipbychildren
	   #:colormap #:colormapchangemask #:colormapinstalled #:colormapnotify
	   #:colormapuninstalled #:x11-complex #:configurenotify
           #:configurerequest
	   #:controlmapindex #:controlmask #:convex #:coordmodeorigin
	   #:coordmodeprevious #:copyfromparent #:createnotify #:currenttime
	   #:cursor #:cursorshape #:cwbackingpixel #:cwbackingplanes
	   #:cwbackingstore #:cwbackpixel #:cwbackpixmap #:cwbitgravity
	   #:cwborderpixel #:cwborderpixmap #:cwborderwidth #:cwcolormap
	   #:cwcursor #:cwdontpropagate #:cweventmask #:cwheight
	   #:cwoverrideredirect #:cwsaveunder #:cwsibling #:cwstackmode
	   #:cwwidth #:cwwingravity #:cwx #:cwy #:cyrillic-keysyms
	   #:defaultblanking #:defaultexposures #:depth #:depth-depth
	   #:depth-nvisuals #:depth-visuals #:destroyall #:destroynotify
	   #:directcolor #:disableaccess #:disablescreeninterval
	   #:disablescreensaver #:display #:display-bitmap-bit-order
	   #:display-bitmap-pad #:display-bitmap-unit #:display-buffer
	   #:display-bufmax #:display-bufptr #:display-byte-order
	   #:display-current #:display-cursor-font #:display-db
	   #:display-default-screen #:display-display-name #:display-event-vec
	   #:display-ext-data #:display-ext-number #:display-ext-procs
	   #:display-fd #:display-head #:display-key-bindings #:display-keysyms
	   #:display-keysyms-per-keycode #:display-last-req
	   #:display-last-request-read #:display-lock #:display-lock-meaning
	   #:display-max-keycode #:display-max-request-size #:display-min-keycode
	   #:display-modifiermap #:display-motion-buffer #:display-next
	   #:display-nformats #:display-nscreens #:display-pixmap-format
	   #:display-proto-major-version #:display-proto-minor-version
	   #:display-qlen #:display-release #:display-request
	   #:display-resource-alloc #:display-resource-base #:display-resource-id
	   #:display-resource-mask #:display-resource-shift
	   #:display-scratch-buffer #:display-scratch-length #:display-screens
	   #:display-synchandler #:display-tail #:display-vendor
	   #:display-vnumber #:display-wire-vec #:display-xdefaults #:doblue
	   #:dogreen #:dontallowexposures #:dontcarestate #:dontpreferblanking
	   #:dored #:double #:drawable #:eastgravity #:enableaccess
	   #:enternotify #:enterwindowmask #:evenoddrule #:expose
	   #:exposuremask #:false #:familychaos #:familydecnet
	   #:familyinternet #:fd-mask #:fillopaquestippled #:fillsolid
	   #:fillstippled #:filltiled #:firstextensionerror #:fixnum-drawable
	   #:fixnum-int #:fixnum-unsigned-int #:float #:focuschangemask #:focusin
	   #:focusout #:font #:fontchange #:fontlefttoright #:fontrighttoleft
	   #:forgetgravity #:funcs #:funcs-add-pixel #:funcs-create-image
	   #:funcs-destroy-image #:funcs-get-pixel #:funcs-put-pixel
	   #:funcs-sub-image #:gc #:gcarcmode #:gcbackground #:gccapstyle
	   #:gcclipmask #:gcclipxorigin #:gcclipyorigin #:gcdashlist
	   #:gcdashoffset #:gcfillrule #:gcfillstyle #:gcfont #:gcforeground
	   #:gcfunction #:gcgraphicsexposures #:gcjoinstyle #:gclastbit
	   #:gclinestyle #:gclinewidth #:gcontext #:gcplanemask #:gcstipple
	   #:gcsubwindowmode #:gctile #:gctilestipxorigin #:gctilestipyorigin
	   #:grabfrozen #:grabinvalidtime #:grabmodeasync #:grabmodesync
	   #:grabnotviewable #:grabsuccess #:graphicsexpose #:gravitynotify
	   #:grayscale #:greek-keysyms #:gxand #:gxandinverted #:gxandreverse
	   #:gxclear #:gxcopy #:gxcopyinverted #:gxequiv #:gxinvert #:gxnand
	   #:gxnoop #:gxnor #:gxor #:gxorinverted #:gxorreverse #:gxset
	   #:gxxor #:hebrew-keysyms #:hostdelete #:hostinsert #:iconicstate
	   #:iconmaskhint #:iconpixmaphint #:iconpositionhint #:iconwindowhint
	   #:inactivestate #:includeinferiors #:inputfocus #:inputhint
	   #:inputonly #:inputoutput #:int #:isunmapped #:isunviewable
	   #:isviewable #:joinbevel #:joinmiter #:joinround #:kana-keysyms
	   #:kbautorepeatmode #:kbbellduration #:kbbellpercent #:kbbellpitch
	   #:kbkey #:kbkeyclickpercent #:kbled #:kbledmode #:keyboard-keysyms
	   #:keycode #:keymapnotify #:keymapstatemask #:keypress
	   #:keypressmask #:keyrelease #:keyreleasemask #:keysym #:lastevent
	   #:lastextensionerror #:latin-1-keysyms #:latin-2-keysyms
	   #:latin-3-keysyms #:latin-4-keysyms #:leavenotify #:leavewindowmask
	   #:ledmodeoff #:ledmodeon #:linedoubledash #:lineonoffdash
	   #:linesolid #:lisp-xdrawstring #:lockmapindex #:lockmask #:long
	   #:long-float #:long-int #:lowerhighest #:lsbfirst #:make-_xextension
	   #:make-_xgc #:make-_xqevent #:make-bsl-union #:make-depth
	   #:make-display #:make-funcs #:make-screen #:make-screenformat
	   #:make-visual #:make-xanyevent #:make-xarc #:make-xbuttonevent
	   #:make-xchar2b #:make-xcharstruct #:make-xcirculateevent
	   #:make-xcirculaterequestevent #:make-xclientmessageevent #:make-xcolor
	   #:make-xcolormapevent #:make-xcomposestatus #:make-xconfigureevent
	   #:make-xconfigurerequestevent #:make-xcreatewindowevent
	   #:make-xcrossingevent #:make-xdestroywindowevent #:make-xerrorevent
	   #:make-xevent #:make-xexposeevent #:make-xextcodes #:make-xextdata
	   #:make-xfocuschangeevent #:make-xfontprop #:make-xfontstruct
	   #:make-xgcvalues #:make-xgraphicsexposeevent #:make-xgravityevent
	   #:make-xhostaddress #:make-ximage #:make-xkeyboardcontrol
	   #:make-xkeyboardstate #:make-xkeyevent #:make-xkeymapevent
	   #:make-xmapevent #:make-xmappingevent #:make-xmaprequestevent
	   #:make-xmodifierkeymap #:make-xmotionevent #:make-xnoexposeevent
	   #:make-xpoint #:make-xpropertyevent #:make-xrectangle
	   #:make-xreparentevent #:make-xresizerequestevent
	   #:make-xrmoptiondescrec #:make-xrmvalue #:make-xsegment
	   #:make-xselectionclearevent #:make-xselectionevent
	   #:make-xselectionrequestevent #:make-xsetwindowattributes
	   #:make-xtextitem #:make-xtextitem16 #:make-xtimecoord
	   #:make-xunmapevent #:make-xvisibilityevent #:make-xwindowattributes
	   #:make-xwindowchanges #:make-xwmhints #:mapnotify #:mappingbusy
	   #:mappingfailed #:mappingkeyboard #:mappingmodifier #:mappingnotify
	   #:mappingpointer #:mappingsuccess #:maprequest #:mask
	   #:mod1mapindex #:mod1mask #:mod2mapindex #:mod2mask #:mod3mapindex
	   #:mod3mask #:mod4mapindex #:mod4mask #:mod5mapindex #:mod5mask
	   #:motionnotify #:msbfirst #:noeventmask #:noexpose #:nonconvex
	   #:none #:normalstate #:northeastgravity #:northgravity
	   #:northwestgravity #:nosymbol #:notifyancestor #:notifydetailnone
	   #:notifygrab #:notifyhint #:notifyinferior #:notifynonlinear
	   #:notifynonlinearvirtual #:notifynormal #:notifypointer
	   #:notifypointerroot #:notifyungrab #:notifyvirtual
	   #:notifywhilegrabbed #:notuseful #:opposite #:ownergrabbuttonmask
	   #:parentrelative #:pixmap #:placeonbottom #:placeontop
	   #:pointermotionhintmask #:pointermotionmask #:pointerroot
	   #:pointerwindow #:preferblanking #:propertychangemask
	   #:propertydelete #:propertynewvalue #:propertynotify #:propmodeappend
	   #:propmodeprepend #:propmodereplace #:pseudocolor
	   #:publishing-keysyms #:queuedafterflush #:queuedafterreading
	   #:queuedalready #:raiselowest #:reparentnotify #:replaykeyboard
	   #:replaypointer #:resizeredirectmask #:resizerequest
	   #:retainpermanent #:retaintemporary #:reverttonone #:reverttoparent
	   #:reverttopointerroot #:screen #:screen-backing-store
	   #:screen-black-pixel #:screen-cmap #:screen-default-gc
	   #:screen-depths #:screen-display #:screen-ext-data #:screen-height
	   #:screen-max-maps #:screen-mheight #:screen-min-maps #:screen-mwidth
	   #:screen-ndepths #:screen-root #:screen-root-depth
	   #:screen-root-input-mask #:screen-root-visual #:screen-save-unders
	   #:screen-white-pixel #:screen-width #:screenformat
	   #:screenformat-bits-per-pixel #:screenformat-depth
	   #:screenformat-ext-data #:screenformat-scanline-pad #:screensaveractive
	   #:screensaverreset #:selectionclear #:selectionnotify
	   #:selectionrequest #:setmodedelete #:setmodeinsert #:shiftmapindex
	   #:shiftmask #:short #:short-int #:southeastgravity #:southgravity
	   #:southwestgravity #:special-keysyms #:statehint #:staticcolor
	   #:staticgravity #:staticgray #:stippleshape #:structurenotifymask
	   #:substructurenotifymask #:substructureredirectmask #:success
	   #:syncboth #:synckeyboard #:syncpointer #:technical-keysyms
	   #:tileshape #:time #:topif #:true #:truecolor #:u-char #:u-int
	   #:u-long #:u-short #:unix #:unmapgravity #:unmapnotify #:unsigned
	   #:unsigned-char #:unsigned-int #:unsigned-long #:unsigned-short
	   #:unsorted #:visibilitychangemask #:visibilityfullyobscured
	   #:visibilitynotify #:visibilitypartiallyobscured #:visibilityunobscured
	   #:visual #:visual-bits-per-rgb #:visual-blue-mask #:visual-class
	   #:visual-ext-data #:visual-green-mask #:visual-map-entries
	   #:visual-red-mask #:visual-visualid #:visualid #:void
	   #:was-called-above #:westgravity #:whenmapped #:windingrule
	   #:window #:windowgrouphint #:withdrawnstate #:x11-char-string
	   #:xa-arc #:xa-atom #:xa-bitmap #:xa-cap-height #:xa-cardinal
	   #:xa-colormap #:xa-copyright #:xa-cursor #:xa-cut-buffer0
	   #:xa-cut-buffer1 #:xa-cut-buffer2 #:xa-cut-buffer3 #:xa-cut-buffer4
	   #:xa-cut-buffer5 #:xa-cut-buffer6 #:xa-cut-buffer7 #:xa-drawable
	   #:xa-end-space #:xa-family-name #:xa-font #:xa-font-name
	   #:xa-full-name #:xa-integer #:xa-italic-angle #:xa-last-predefined
	   #:xa-max-space #:xa-min-space #:xa-norm-space #:xa-notice
	   #:xa-pixmap #:xa-point #:xa-point-size #:xa-primary #:xa-quad-width
	   #:xa-rectangle #:xa-resolution #:xa-resource-manager
	   #:xa-rgb-best-map #:xa-rgb-blue-map #:xa-rgb-color-map
	   #:xa-rgb-default-map #:xa-rgb-gray-map #:xa-rgb-green-map
	   #:xa-rgb-red-map #:xa-secondary #:xa-strikeout-ascent
	   #:xa-strikeout-descent #:xa-string #:xa-subscript-x #:xa-subscript-y
	   #:xa-superscript-x #:xa-superscript-y #:xa-underline-position
	   #:xa-underline-thickness #:xa-visualid #:xa-weight #:xa-window
	   #:xa-wm-class #:xa-wm-client-machine #:xa-wm-command #:xa-wm-hints
	   #:xa-wm-icon-name #:xa-wm-icon-size #:xa-wm-name #:xa-wm-normal-hints
	   #:xa-wm-size-hints #:xa-wm-transient-for #:xa-wm-zoom-hints
	   #:xa-x-height #:xactivatescreensaver #:xaddextension #:xaddhost
	   #:xaddhosts #:xaddpixel #:xaddtosaveset #:xalloccolor
	   #:xalloccolorcells #:xalloccolorplanes #:xallocnamedcolor
	   #:xallocwmhints #:xallowevents #:xallplanes #:xanyevent
	   #:xanyevent-display #:xanyevent-send-event #:xanyevent-serial
	   #:xanyevent-type #:xanyevent-window #:xarc #:xarc-angle1
	   #:xarc-angle2 #:xarc-height #:xarc-width #:xarc-x #:xarc-y
	   #:xautorepeatoff #:xautorepeaton #:xbell #:xbitmapbitorder
	   #:xbitmappad #:xbitmapunit #:xblackpixel #:xblackpixelofscreen
	   #:xbuttonevent #:xbuttonevent-button #:xbuttonevent-display
	   #:xbuttonevent-root #:xbuttonevent-same-screen
	   #:xbuttonevent-send-event #:xbuttonevent-serial #:xbuttonevent-state
	   #:xbuttonevent-subwindow #:xbuttonevent-time #:xbuttonevent-type
	   #:xbuttonevent-window #:xbuttonevent-x #:xbuttonevent-x-root
	   #:xbuttonevent-y #:xbuttonevent-y-root #:xbuttonpressedevent
	   #:xbuttonreleasedevent #:xc-arrow #:xc-based-arrow-down
	   #:xc-based-arrow-up #:xc-boat #:xc-bogosity #:xc-bottom-left-corner
	   #:xc-bottom-right-corner #:xc-bottom-side #:xc-bottom-tee
	   #:xc-box-spiral #:xc-center-ptr #:xc-circle #:xc-clock
	   #:xc-coffee-mug #:xc-cross #:xc-cross-reverse #:xc-crosshair
	   #:xc-diamond-cross #:xc-dot #:xc-dotbox #:xc-double-arrow
	   #:xc-draft-large #:xc-draft-small #:xc-draped-box #:xc-exchange
	   #:xc-fleur #:xc-gobbler #:xc-gumby #:xc-hand1 #:xc-hand2
	   #:xc-heart #:xc-icon #:xc-iron-cross #:xc-left-ptr #:xc-left-side
	   #:xc-left-tee #:xc-leftbutton #:xc-ll-angle #:xc-lr-angle #:xc-man
	   #:xc-middlebutton #:xc-mouse #:xc-pencil #:xc-pirate #:xc-plus
	   #:xc-question-arrow #:xc-right-ptr #:xc-right-side #:xc-right-tee
	   #:xc-rightbutton #:xc-rtl-logo #:xc-sailboat #:xc-sb-down-arrow
	   #:xc-sb-h-double-arrow #:xc-sb-left-arrow #:xc-sb-right-arrow
	   #:xc-sb-up-arrow #:xc-sb-v-double-arrow #:xc-shuttle #:xc-sizing
	   #:xc-spider #:xc-spraycan #:xc-star #:xc-target #:xc-tcross
	   #:xc-top-left-arrow #:xc-top-left-corner #:xc-top-right-corner
	   #:xc-top-side #:xc-top-tee #:xc-trek #:xc-ul-angle #:xc-umbrella
	   #:xc-ur-angle #:xc-watch #:xc-x-cursor #:xc-xterm #:xcellsofscreen
	   #:xchangeactivepointergrab #:xchangegc #:xchangekeyboardcontrol
	   #:xchangekeyboardmapping #:xchangepointercontrol #:xchangeproperty
	   #:xchangesaveset #:xchangewindowattributes #:xchar2b #:xchar2b-byte1
	   #:xchar2b-byte2 #:xcharstruct #:xcharstruct-ascent
	   #:xcharstruct-attributes #:xcharstruct-descent #:xcharstruct-lbearing
	   #:xcharstruct-rbearing #:xcharstruct-width #:xcheckifevent
	   #:xcheckmaskevent #:xchecktypedevent #:xchecktypedwindowevent
	   #:xcheckwindowevent #:xcirculateevent #:xcirculateevent-display
	   #:xcirculateevent-event #:xcirculateevent-place
	   #:xcirculateevent-send-event #:xcirculateevent-serial
	   #:xcirculateevent-type #:xcirculateevent-window
	   #:xcirculaterequestevent #:xcirculaterequestevent-display
	   #:xcirculaterequestevent-parent #:xcirculaterequestevent-place
	   #:xcirculaterequestevent-send-event #:xcirculaterequestevent-serial
	   #:xcirculaterequestevent-type #:xcirculaterequestevent-window
	   #:xcirculatesubwindows #:xcirculatesubwindowsdown
	   #:xcirculatesubwindowsup #:xcleararea #:xclearwindow
	   #:xclientmessageevent #:xclientmessageevent-data
	   #:xclientmessageevent-display #:xclientmessageevent-format
	   #:xclientmessageevent-message-type #:xclientmessageevent-send-event
	   #:xclientmessageevent-serial #:xclientmessageevent-type
	   #:xclientmessageevent-window #:xclosedisplay #:xcnoent #:xcnomem
	   #:xcolor #:xcolor-blue #:xcolor-flags #:xcolor-green #:xcolor-pad
	   #:xcolor-pixel #:xcolor-red #:xcolormapevent
	   #:xcolormapevent-colormap #:xcolormapevent-display #:xcolormapevent-new
	   #:xcolormapevent-send-event #:xcolormapevent-serial
	   #:xcolormapevent-state #:xcolormapevent-type #:xcolormapevent-window
	   #:xcomposestatus #:xcomposestatus-chars-matched
	   #:xcomposestatus-compose-ptr #:xconfigureevent #:xconfigureevent-above
	   #:xconfigureevent-border-width #:xconfigureevent-display
	   #:xconfigureevent-event #:xconfigureevent-height
	   #:xconfigureevent-override-redirect #:xconfigureevent-send-event
	   #:xconfigureevent-serial #:xconfigureevent-type #:xconfigureevent-width
	   #:xconfigureevent-window #:xconfigureevent-x #:xconfigureevent-y
	   #:xconfigurerequestevent #:xconfigurerequestevent-above
	   #:xconfigurerequestevent-border-width #:xconfigurerequestevent-detail
	   #:xconfigurerequestevent-display #:xconfigurerequestevent-height
	   #:xconfigurerequestevent-parent #:xconfigurerequestevent-send-event
	   #:xconfigurerequestevent-serial #:xconfigurerequestevent-type
	   #:xconfigurerequestevent-value-mask #:xconfigurerequestevent-width
	   #:xconfigurerequestevent-window #:xconfigurerequestevent-x
	   #:xconfigurerequestevent-y #:xconfigurewindow #:xconnectionnumber
	   #:xcontext #:xconvertselection #:xcopyarea #:xcopycolormapandfree
	   #:xcopygc #:xcopyplane #:xcreatecolormap #:xcreatefontcursor
	   #:xcreategc #:xcreateglyphcursor #:xcreateimage #:xcreatepixmap
	   #:xcreatepixmapcursor #:xcreatesimplewindow #:xcreatewindow
	   #:xcreatewindowevent #:xcreatewindowevent-border-width
	   #:xcreatewindowevent-display #:xcreatewindowevent-height
	   #:xcreatewindowevent-override-redirect #:xcreatewindowevent-parent
	   #:xcreatewindowevent-send-event #:xcreatewindowevent-serial
	   #:xcreatewindowevent-type #:xcreatewindowevent-width
	   #:xcreatewindowevent-window #:xcreatewindowevent-x
	   #:xcreatewindowevent-y #:xcrossingevent #:xcrossingevent-detail
	   #:xcrossingevent-display #:xcrossingevent-focus #:xcrossingevent-mode
	   #:xcrossingevent-root #:xcrossingevent-same-screen
	   #:xcrossingevent-send-event #:xcrossingevent-serial
	   #:xcrossingevent-state #:xcrossingevent-subwindow #:xcrossingevent-time
	   #:xcrossingevent-type #:xcrossingevent-window #:xcrossingevent-x
	   #:xcrossingevent-x-root #:xcrossingevent-y #:xcrossingevent-y-root
	   #:xcsuccess #:xdefaultcolormap #:xdefaultcolormapofscreen
	   #:xdefaultdepth #:xdefaultdepthofscreen #:xdefaultgc
	   #:xdefaultgcofscreen #:xdefaultrootwindow #:xdefaultscreen
	   #:xdefaultscreenofdisplay #:xdefaultvisual #:xdefaultvisualofscreen
	   #:xdefinecursor #:xdeletecontext #:xdeletemodifiermapentry
	   #:xdeleteproperty #:xdestroyimage #:xdestroysubwindows
	   #:xdestroywindow #:xdestroywindowevent #:xdestroywindowevent-display
	   #:xdestroywindowevent-event #:xdestroywindowevent-send-event
	   #:xdestroywindowevent-serial #:xdestroywindowevent-type
	   #:xdestroywindowevent-window #:xdisableaccesscontrol #:xdisplaycells
	   #:xdisplayheight #:xdisplayheightmm #:xdisplaykeycodes
	   #:xdisplaymotionbuffersize #:xdisplayname #:xdisplayofscreen
	   #:xdisplayplanes #:xdisplaystring #:xdisplaywidth #:xdisplaywidthmm
	   #:xdoesbackingstore #:xdoessaveunders #:xdrawarc #:xdrawarcs
	   #:xdrawchar #:xdrawimagestring #:xdrawimagestring16 #:xdrawline
	   #:xdrawlines #:xdrawpoint #:xdrawpoints #:xdrawrectangle
	   #:xdrawrectangles #:xdrawsegments #:xdrawstring #:xdrawstring16
	   #:xdrawtext #:xdrawtext16 #:xedataobject #:xeheadofextensionlist
	   #:xenableaccesscontrol #:xenterwindowevent #:xerrorevent
	   #:xerrorevent-display #:xerrorevent-error-code #:xerrorevent-minor-code
	   #:xerrorevent-request-code #:xerrorevent-resourceid
	   #:xerrorevent-serial #:xerrorevent-type #:xesetclosedisplay
	   #:xesetcopygc #:xesetcreatefont #:xesetcreategc #:xeseterror
	   #:xeseterrorstring #:xeseteventtowire #:xesetflushgc #:xesetfreefont
	   #:xesetfreegc #:xesetwiretoevent #:xevent #:xevent-pad
	   #:xevent-type #:xevent-xany #:xevent-xbutton #:xevent-xcirculate
	   #:xevent-xcirculaterequest #:xevent-xclient #:xevent-xcolormap
	   #:xevent-xconfigure #:xevent-xconfigurerequest #:xevent-xcreatewindow
	   #:xevent-xcrossing #:xevent-xdestroywindow #:xevent-xerror
	   #:xevent-xexpose #:xevent-xfocus #:xevent-xgraphicsexpose
	   #:xevent-xgravity #:xevent-xkey #:xevent-xkeymap #:xevent-xmap
	   #:xevent-xmapping #:xevent-xmaprequest #:xevent-xmotion
	   #:xevent-xnoexpose #:xevent-xproperty #:xevent-xreparent
	   #:xevent-xresizerequest #:xevent-xselection #:xevent-xselectionclear
	   #:xevent-xselectionrequest #:xevent-xunmap #:xevent-xvisibility
	   #:xeventmaskofscreen #:xeventsqueued #:xexposeevent
	   #:xexposeevent-count #:xexposeevent-display #:xexposeevent-height
	   #:xexposeevent-send-event #:xexposeevent-serial #:xexposeevent-type
	   #:xexposeevent-width #:xexposeevent-window #:xexposeevent-x
	   #:xexposeevent-y #:xextcodes #:xextcodes-extension
	   #:xextcodes-first-error #:xextcodes-first-event
	   #:xextcodes-major-opcode #:xextdata #:xextdata-free-private
	   #:xextdata-next #:xextdata-number #:xextdata-private-data
	   #:xfetchname #:xfillarc #:xfillarcs #:xfillpolygon #:xfillrectangle
	   #:xfillrectangles #:xfindcontext #:xfindonextensionlist #:xflush
	   #:xfocuschangeevent #:xfocuschangeevent-detail
	   #:xfocuschangeevent-display #:xfocuschangeevent-mode
	   #:xfocuschangeevent-send-event #:xfocuschangeevent-serial
	   #:xfocuschangeevent-type #:xfocuschangeevent-window #:xfocusinevent
	   #:xfocusoutevent #:xfontprop #:xfontprop-card32 #:xfontprop-name
	   #:xfontstruct #:xfontstruct-all-chars-exist #:xfontstruct-ascent
	   #:xfontstruct-default-char #:xfontstruct-descent
	   #:xfontstruct-direction #:xfontstruct-ext-data #:xfontstruct-fid
	   #:xfontstruct-max-bounds #:xfontstruct-max-byte1
	   #:xfontstruct-max-char-or-byte2 #:xfontstruct-min-bounds
	   #:xfontstruct-min-byte1 #:xfontstruct-min-char-or-byte2
	   #:xfontstruct-n-properties #:xfontstruct-per-char
	   #:xfontstruct-properties #:xforcescreensaver #:xfree #:xfreecolormap
	   #:xfreecolors #:xfreecursor #:xfreeextensionlist #:xfreefont
	   #:xfreefontinfo #:xfreefontnames #:xfreefontpath #:xfreegc
	   #:xfreemodifiermap #:xfreepixmap #:xgcontextfromgc #:xgcvalues
	   #:xgcvalues-arc-mode #:xgcvalues-background #:xgcvalues-cap-style
	   #:xgcvalues-clip-mask #:xgcvalues-clip-x-origin
	   #:xgcvalues-clip-y-origin #:xgcvalues-dash-offset #:xgcvalues-dashes
	   #:xgcvalues-fill-rule #:xgcvalues-fill-style #:xgcvalues-font
	   #:xgcvalues-foreground #:xgcvalues-function
	   #:xgcvalues-graphics-exposures #:xgcvalues-join-style
	   #:xgcvalues-line-style #:xgcvalues-line-width #:xgcvalues-plane-mask
	   #:xgcvalues-stipple #:xgcvalues-subwindow-mode #:xgcvalues-tile
	   #:xgcvalues-ts-x-origin #:xgcvalues-ts-y-origin #:xgetatomname
	   #:xgeterrordatabasetext #:xgeterrortext #:xgetfontpath
	   #:xgetfontproperty #:xgetgeometry #:xgeticonname #:xgetimage
	   #:xgetinputfocus #:xgetkeyboardcontrol #:xgetkeyboardmapping
	   #:xgetmodifiermapping #:xgetmotionevents #:xgetpixel
	   #:xgetpointercontrol #:xgetpointermapping #:xgetscreensaver
	   #:xgetselectionowner #:xgetsubimage #:xgetwindowattributes
	   #:xgetwindowproperty #:xgetwmhints #:xgrabbutton #:xgrabkey
	   #:xgrabkeyboard #:xgrabpointer #:xgrabserver #:xgraphicsexposeevent
	   #:xgraphicsexposeevent-count #:xgraphicsexposeevent-display
	   #:xgraphicsexposeevent-drawable #:xgraphicsexposeevent-height
	   #:xgraphicsexposeevent-major-code #:xgraphicsexposeevent-minor-code
	   #:xgraphicsexposeevent-send-event #:xgraphicsexposeevent-serial
	   #:xgraphicsexposeevent-type #:xgraphicsexposeevent-width
	   #:xgraphicsexposeevent-x #:xgraphicsexposeevent-y #:xgravityevent
	   #:xgravityevent-display #:xgravityevent-event
	   #:xgravityevent-send-event #:xgravityevent-serial #:xgravityevent-type
	   #:xgravityevent-window #:xgravityevent-x #:xgravityevent-y
	   #:xheightmmofscreen #:xheightofscreen #:xhostaddress
	   #:xhostaddress-address #:xhostaddress-family #:xhostaddress-length
	   #:xid #:xifevent #:ximage #:ximage-bitmap-bit-order
	   #:ximage-bitmap-pad #:ximage-bitmap-unit #:ximage-bits-per-pixel
	   #:ximage-blue-mask #:ximage-byte-order #:ximage-bytes-per-line
	   #:ximage-data #:ximage-depth #:ximage-f #:ximage-format
	   #:ximage-green-mask #:ximage-height #:ximage-obdata #:ximage-red-mask
	   #:ximage-width #:ximage-xoffset #:ximagebyteorder #:xinitextension
	   #:xinsertmodifiermapentry #:xinstallcolormap #:xinternatom
	   #:xkeyboardcontrol #:xkeyboardcontrol-auto-repeat-mode
	   #:xkeyboardcontrol-bell-duration #:xkeyboardcontrol-bell-percent
	   #:xkeyboardcontrol-bell-pitch #:xkeyboardcontrol-key
	   #:xkeyboardcontrol-key-click-percent #:xkeyboardcontrol-led
	   #:xkeyboardcontrol-led-mode #:xkeyboardstate
	   #:xkeyboardstate-auto-repeats #:xkeyboardstate-bell-duration
	   #:xkeyboardstate-bell-percent #:xkeyboardstate-bell-pitch
	   #:xkeyboardstate-global-auto-repeat #:xkeyboardstate-key-click-percent
	   #:xkeyboardstate-led-mask #:xkeyevent #:xkeyevent-display
	   #:xkeyevent-keycode #:xkeyevent-root #:xkeyevent-same-screen
	   #:xkeyevent-send-event #:xkeyevent-serial #:xkeyevent-state
	   #:xkeyevent-subwindow #:xkeyevent-time #:xkeyevent-type
	   #:xkeyevent-window #:xkeyevent-x #:xkeyevent-x-root #:xkeyevent-y
	   #:xkeyevent-y-root #:xkeymapevent #:xkeymapevent-display
	   #:xkeymapevent-key-vector #:xkeymapevent-send-event
	   #:xkeymapevent-serial #:xkeymapevent-type #:xkeymapevent-window
	   #:xkeypressedevent #:xkeyreleasedevent #:xkeysymtostring
	   #:xkillclient #:xlastknownrequestprocessed #:xleavewindowevent
	   #:xlistextensions #:xlistfonts #:xlistfontswithinfo #:xlisthosts
	   #:xlistinstalledcolormaps #:xlistproperties #:xloadfont
	   #:xloadqueryfont #:xlookupcolor #:xlookupstring #:xlowerwindow
	   #:xmapevent #:xmapevent-display #:xmapevent-event
	   #:xmapevent-override-redirect #:xmapevent-send-event #:xmapevent-serial
	   #:xmapevent-type #:xmapevent-window #:xmappingevent
	   #:xmappingevent-count #:xmappingevent-display
	   #:xmappingevent-first-keycode #:xmappingevent-request
	   #:xmappingevent-send-event #:xmappingevent-serial #:xmappingevent-type
	   #:xmappingevent-window #:xmapraised #:xmaprequestevent
	   #:xmaprequestevent-display #:xmaprequestevent-parent
	   #:xmaprequestevent-send-event #:xmaprequestevent-serial
	   #:xmaprequestevent-type #:xmaprequestevent-window #:xmapsubwindows
	   #:xmapwindow #:xmaskevent #:xmaxcmapsofscreen #:xmaxrequestsize
	   #:xmincmapsofscreen #:xmodifierkeymap #:xmodifierkeymap-max-keypermod
	   #:xmodifierkeymap-modifiermap #:xmotionevent #:xmotionevent-display
	   #:xmotionevent-is-hint #:xmotionevent-root #:xmotionevent-same-screen
	   #:xmotionevent-send-event #:xmotionevent-serial #:xmotionevent-state
	   #:xmotionevent-subwindow #:xmotionevent-time #:xmotionevent-type
	   #:xmotionevent-window #:xmotionevent-x #:xmotionevent-x-root
	   #:xmotionevent-y #:xmotionevent-y-root #:xmoveresizewindow
	   #:xmovewindow #:xnewmodifiermap #:xnextevent #:xnextrequest
	   #:xnoexposeevent #:xnoexposeevent-display #:xnoexposeevent-drawable
	   #:xnoexposeevent-major-code #:xnoexposeevent-minor-code
	   #:xnoexposeevent-send-event #:xnoexposeevent-serial
	   #:xnoexposeevent-type #:xnoop #:xopendisplay #:xparsecolor
	   #:xparsegeometry #:xpeekevent #:xpeekifevent #:xpending
	   #:xpermalloc #:xplanesofscreen #:xpoint #:xpoint-x #:xpoint-y
	   #:xpointermovedevent #:xpropertyevent #:xpropertyevent-atom
	   #:xpropertyevent-display #:xpropertyevent-send-event
	   #:xpropertyevent-serial #:xpropertyevent-state #:xpropertyevent-time
	   #:xpropertyevent-type #:xpropertyevent-window #:xprotocolrevision
	   #:xprotocolversion #:xputbackevent #:xputimage #:xputpixel
	   #:xqlength #:xquerybestcursor #:xquerybestsize #:xquerybeststipple
	   #:xquerybesttile #:xquerycolor #:xquerycolors #:xqueryextension
	   #:xqueryfont #:xquerykeymap #:xquerypointer #:xquerytextextents
	   #:xquerytextextents16 #:xquerytree #:xraisewindow #:xreadbitmapfile
	   #:xrebindkeysym #:xrecolorcursor #:xrectangle #:xrectangle-height
	   #:xrectangle-width #:xrectangle-x #:xrectangle-y
	   #:xrefreshkeyboardmapping #:xremovefromsaveset #:xremovehost
	   #:xremovehosts #:xreparentevent #:xreparentevent-display
	   #:xreparentevent-event #:xreparentevent-override-redirect
	   #:xreparentevent-parent #:xreparentevent-send-event
	   #:xreparentevent-serial #:xreparentevent-type #:xreparentevent-window
	   #:xreparentevent-x #:xreparentevent-y #:xreparentwindow
	   #:xresetscreensaver #:xresizerequestevent #:xresizerequestevent-display
	   #:xresizerequestevent-height #:xresizerequestevent-send-event
	   #:xresizerequestevent-serial #:xresizerequestevent-type
	   #:xresizerequestevent-width #:xresizerequestevent-window
	   #:xresizewindow #:xresourcemanagerstring #:xrestackwindows
	   #:xrmbinding #:xrmbindinglist #:xrmclass #:xrmclasslist
	   #:xrmdatabase #:xrmgetfiledatabase #:xrmgetresource
	   #:xrmgetstringdatabase #:xrminitialize #:xrmmergedatabases #:xrmname
	   #:xrmnamelist #:xrmoptiondesclist #:xrmoptiondescrec
	   #:xrmoptiondescrec-argkind #:xrmoptiondescrec-option
	   #:xrmoptiondescrec-specifier #:xrmoptiondescrec-value #:xrmoptionkind
	   #:xrmparsecommand #:xrmputfiledatabase #:xrmputlineresource
	   #:xrmputresource #:xrmputstringresource #:xrmqgetresource
	   #:xrmqgetsearchlist #:xrmqgetsearchresource #:xrmqputresource
	   #:xrmqputstringresource #:xrmquark #:xrmquarklist #:xrmquarktostring
	   #:xrmrepresentation #:xrmsearchlist #:xrmstring
	   #:xrmstringtobindingquarklist #:xrmstringtoquark #:xrmstringtoquarklist
	   #:xrmuniquequark #:xrmvalue #:xrmvalue-addr #:xrmvalue-size
	   #:xrmvalueptr #:xrootwindow #:xrootwindowofscreen
	   #:xrotatewindowproperties #:xsavecontext #:xscreencount
	   #:xscreenofdisplay #:xsegment #:xsegment-x1 #:xsegment-x2
	   #:xsegment-y1 #:xsegment-y2 #:xselectinput #:xselectionclearevent
	   #:xselectionclearevent-display #:xselectionclearevent-selection
	   #:xselectionclearevent-send-event #:xselectionclearevent-serial
	   #:xselectionclearevent-time #:xselectionclearevent-type
	   #:xselectionclearevent-window #:xselectionevent
	   #:xselectionevent-display #:xselectionevent-property
	   #:xselectionevent-requestor #:xselectionevent-selection
	   #:xselectionevent-send-event #:xselectionevent-serial
	   #:xselectionevent-target #:xselectionevent-time #:xselectionevent-type
	   #:xselectionrequestevent #:xselectionrequestevent-display
	   #:xselectionrequestevent-owner #:xselectionrequestevent-property
	   #:xselectionrequestevent-requestor #:xselectionrequestevent-selection
	   #:xselectionrequestevent-send-event #:xselectionrequestevent-serial
	   #:xselectionrequestevent-target #:xselectionrequestevent-time
	   #:xselectionrequestevent-type #:xsendevent #:xservervendor
	   #:xsetaccesscontrol #:xsetafterfunction #:xsetarcmode
	   #:xsetbackground #:xsetclipmask #:xsetcliporigin #:xsetcliprectangles
	   #:xsetclosedownmode #:xsetcommand #:xsetdashes #:xseterrorhandler
	   #:xsetfillrule #:xsetfillstyle #:xsetfont #:xsetfontpath
	   #:xsetforeground #:xsetfunction #:xsetgraphicsexposures
	   #:xseticonname #:xsetinputfocus #:xsetioerrorhandler
	   #:xsetlineattributes #:xsetmodifiermapping #:xsetplanemask
	   #:xsetpointermapping #:xsetscreensaver #:xsetselectionowner
	   #:xsetstate #:xsetstipple #:xsetsubwindowmode #:xsettile
	   #:xsettsorigin #:xsetwindowattributes
	   #:xsetwindowattributes-background-pixel
	   #:xsetwindowattributes-background-pixmap
	   #:xsetwindowattributes-backing-pixel
	   #:xsetwindowattributes-backing-planes
	   #:xsetwindowattributes-backing-store #:xsetwindowattributes-bit-gravity
	   #:xsetwindowattributes-border-pixel #:xsetwindowattributes-border-pixmap
	   #:xsetwindowattributes-colormap #:xsetwindowattributes-cursor
	   #:xsetwindowattributes-do-not-propagate-mask
	   #:xsetwindowattributes-event-mask
	   #:xsetwindowattributes-override-redirect
	   #:xsetwindowattributes-save-under #:xsetwindowattributes-win-gravity
	   #:xsetwindowbackground #:xsetwindowbackgroundpixmap #:xsetwindowborder
	   #:xsetwindowborderpixmap #:xsetwindowborderwidth #:xsetwindowcolormap
	   #:xsetwmhints #:xstorecolor #:xstorecolors #:xstorename
	   #:xstorenamedcolor #:xstringtokeysym #:xsubimage #:xsync
	   #:xsynchronize #:xtextextents #:xtextextents16 #:xtextitem
	   #:xtextitem-chars #:xtextitem-delta #:xtextitem-font
	   #:xtextitem-nchars #:xtextitem16 #:xtextitem16-chars
	   #:xtextitem16-delta #:xtextitem16-font #:xtextitem16-nchars
	   #:xtextwidth #:xtextwidth16 #:xtimecoord #:xtimecoord-time
	   #:xtimecoord-x #:xtimecoord-y #:xtranslatecoordinates
	   #:xundefinecursor #:xungrabbutton #:xungrabkey #:xungrabkeyboard
	   #:xungrabpointer #:xungrabserver #:xuninstallcolormap #:xunloadfont
	   #:xunmapevent #:xunmapevent-display #:xunmapevent-event
	   #:xunmapevent-from-configure #:xunmapevent-send-event
	   #:xunmapevent-serial #:xunmapevent-type #:xunmapevent-window
	   #:xunmapsubwindows #:xunmapwindow #:xusekeymap #:xvendorrelease
	   #:xvisibilityevent #:xvisibilityevent-display
	   #:xvisibilityevent-send-event #:xvisibilityevent-serial
	   #:xvisibilityevent-state #:xvisibilityevent-type
	   #:xvisibilityevent-window #:xvisualidfromvisual #:xwarppointer
	   #:xwhitepixel #:xwhitepixelofscreen #:xwidthmmofscreen
	   #:xwidthofscreen #:xwindowattributes
	   #:xwindowattributes-all-event-masks #:xwindowattributes-backing-pixel
	   #:xwindowattributes-backing-planes #:xwindowattributes-backing-store
	   #:xwindowattributes-bit-gravity #:xwindowattributes-border-width
	   #:xwindowattributes-class #:xwindowattributes-colormap
	   #:xwindowattributes-depth #:xwindowattributes-do-not-propagate-mask
	   #:xwindowattributes-height #:xwindowattributes-map-installed
	   #:xwindowattributes-map-state #:xwindowattributes-override-redirect
	   #:xwindowattributes-root #:xwindowattributes-save-under
	   #:xwindowattributes-screen #:xwindowattributes-visual
	   #:xwindowattributes-width #:xwindowattributes-win-gravity
	   #:xwindowattributes-x #:xwindowattributes-y
	   #:xwindowattributes-your-event-mask #:xwindowchanges
	   #:xwindowchanges-border-width #:xwindowchanges-height
	   #:xwindowchanges-sibling #:xwindowchanges-stack-mode
	   #:xwindowchanges-width #:xwindowchanges-x #:xwindowchanges-y
	   #:xwindowevent #:xwmhints #:xwmhints-flags #:xwmhints-icon-mask
	   #:xwmhints-icon-pixmap #:xwmhints-icon-window #:xwmhints-icon-x
	   #:xwmhints-icon-y #:xwmhints-initial-state #:xwmhints-input
	   #:xwmhints-window-group #:xwritebitmapfile #:xybitmap #:xypixmap
	   #:ysorted #:yxbanded #:yxsorted #:zoomstate #:zpixmap
	   #:xiconifywindow)
  )

(in-package :x11)
