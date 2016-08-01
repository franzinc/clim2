;; See the file LICENSE for the full license governing this code.
;;

;;; (c) Copyright  1990 Sun Microsystems, Inc.  All Rights Reserved.
;;      (c) Copyright 1989, 1990, 1991 Sun Microsystems, Inc. Sun design
;;      patents pending in the U.S. and foreign countries. OPEN LOOK is a
;;      registered trademark of USL. Used by written permission of the owners.
;;
;;      (c) Copyright Bigelow & Holmes 1986, 1985. Lucida is a registered
;;      trademark of Bigelow & Holmes. Permission to use the Lucida
;;      trademark is hereby granted only in association with the images
;;      and fonts described in this file.
;;
;;      SUN MICROSYSTEMS, INC., USL, AND BIGELOW & HOLMES
;;      MAKE NO REPRESENTATIONS ABOUT THE SUITABILITY OF
;;      THIS SOURCE OR OBJECT CODE FOR ANY PURPOSE. IT IS PROVIDED "AS IS"
;;      WITHOUT EXPRESS OR IMPLIED WARRANTY OF ANY KIND.
;;      SUN  MICROSYSTEMS, INC., USL AND BIGELOW  & HOLMES,
;;      SEVERALLY AND INDIVIDUALLY, DISCLAIM ALL WARRANTIES
;;      WITH REGARD TO THIS CODE, INCLUDING ALL IMPLIED
;;      WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;;      PARTICULAR PURPOSE. IN NO EVENT SHALL SUN MICROSYSTEMS,
;;      INC., USL OR BIGELOW & HOLMES BE LIABLE FOR ANY
;;      SPECIAL, INDIRECT, INCIDENTAL, OR CONSEQUENTIAL DAMAGES,
;;      OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
;;      OR PROFITS, WHETHER IN AN ACTION OF  CONTRACT, NEGLIGENCE
;;      OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
;;      WITH THE USE OR PERFORMANCE OF THIS OBJECT CODE.

(in-package :x11)

;;; This is a Lucid Common Lisp Foreign Function Interface for Xlib.  It was generated
;;; largely from a version of Xlib.h written by David Harrison of UC Berkeley in 1988 that
;;; included function prototype declarations for all of the Xlib exported functions.
;;;
;;; All of the C identifiers have been converted to Lisp style identifiers: embedded
;;; underscores have been converted to dashes and all characters have been (effectively)
;;; uppercased.  This led to one collision: the #defined constant Above collided with
;;; the slots called above in xconfigurerequestevent and xconfigureevent.  The Above
;;; constant was renamed "was-called-above"; its value is 0.
;;;
;;; The organization of this file should roughly follow the organization of Xlib.h (which
;;; #includes X.h).  Near the top of the file is a set of extensions and convienence
;;; functions that have been added locally.
;;;
;;; LOADING:
;;; This file depends on ffi.lisp. After ffi.lisp and this file load the X11 library:
;;; (FFI:load-foreign-libraries nil '("-X11" "-lm" "-lc")
;;;
;;; HISTORY:
;;; 4-1-89 - Hans Muller, hmuller@sun.COM, created.
;;;
;;; 5-30-89 - Hans Muller, hmuller@sun.COM, Now only export names without a leading
;;; underscore.  Export structure constructor, predicate, accessors.
;;;
;;; 11-20-89 - Hans Muller, hmuller@sun.COM. The drawing functions have been segregated
;;; and moved to the end of the file, :fixnum types have been substituted where possible.
;;; Added an XDrawChar function.  These changes yielded about an 8X speedup over
;;; the original definitions.
;;;
;;; 3-1-90 - Hans Muller, hmuller@sun.COM.  Appended definitions for the X11 resource
;;; manager from Xresource.h.
;;;
;;; BUGS:
;;; - Most of the C preprocessor macros where not translated.  A list of all of the
;;;   untranslated macros appears at the end of this file.  Most of them could
;;;   be translated into comparable Lisp macros.
;;;
;;; - Structure slots whose type is :pointer :pointer are probably C function pointers.
;;;   A synonym type should be used instead.
;;;
;;; - The translation of the union types should be checked.
;;;


;;; Standard C types from ffi.lisp

;; (exports moved to pkg.lisp -- jdi)


;;; Above is renamed here to avoid colliding with the slots called above
;;; in xconfigurerequestevent and xconfigureevent.

(def-exported-constant was-called-above 0)             ;; #define Above   0


(def-exported-constant allocall 1)                     ;; #define AllocAll 	1
(def-exported-constant allocnone 0)                    ;; #define AllocNone 	0
(def-exported-constant allowexposures 1)               ;; #define AllowExposures 	1
(def-exported-constant alltemporary 0)                 ;; #define AllTemporary         0L
(def-exported-constant alreadygrabbed 1)               ;; #define AlreadyGrabbed 	1
(def-exported-constant always 2)                       ;; #define Always                  2
(def-exported-constant anybutton 0)                    ;; #define AnyButton            0L
(def-exported-constant anykey 0)                       ;; #define AnyKey 	     0L
(def-exported-constant anymodifier 32768)              ;; #define AnyModifier 	(1<<15)
(def-exported-constant anypropertytype 0)              ;; #define AnyPropertyType      0L
(def-exported-constant arcchord 0)                     ;; #define ArcChord 	0
(def-exported-constant arcpieslice 1)                  ;; #define ArcPieSlice 	1
(def-exported-constant asyncboth 6)                    ;; #define AsyncBoth 	6
(def-exported-constant asynckeyboard 3)                ;; #define AsyncKeyboard 	3
(def-exported-constant asyncpointer 0)                 ;; #define AsyncPointer 	0
(def-exported-constant autorepeatmodedefault 2)        ;; #define AutoRepeatModeDefault 2
(def-exported-constant autorepeatmodeoff 0)            ;; #define AutoRepeatModeOff 0
(def-exported-constant autorepeatmodeon 1)             ;; #define AutoRepeatModeOn 1
(def-exported-constant badaccess 10)                   ;; #define BadAccess   10
(def-exported-constant badalloc 11)                    ;; #define BadAlloc   11
(def-exported-constant badatom 5)                      ;; #define BadAtom 	   5
(def-exported-constant badcolor 12)                    ;; #define BadColor   12
(def-exported-constant badcursor 6)                    ;; #define BadCursor    6
(def-exported-constant baddrawable 9)                  ;; #define BadDrawable    9
(def-exported-constant badfont 7)                      ;; #define BadFont 	   7
(def-exported-constant badgc 13)                       ;; #define BadGC 	  13
(def-exported-constant badidchoice 14)                 ;; #define BadIDChoice   14
(def-exported-constant badimplementation 17)           ;; #define BadImplementation 17
(def-exported-constant badlength 16)                   ;; #define BadLength   16
(def-exported-constant badmatch 8)                     ;; #define BadMatch    8
(def-exported-constant badname 15)                     ;; #define BadName 	  15
(def-exported-constant badpixmap 4)                    ;; #define BadPixmap    4
(def-exported-constant badrequest 1)                   ;; #define BadRequest    1
(def-exported-constant badvalue 2)                     ;; #define BadValue    2
(def-exported-constant badwindow 3)                    ;; #define BadWindow    3
(def-exported-constant below 1)                        ;; #define Below                   1
(def-exported-constant bottomif 3)                     ;; #define BottomIf                3
(def-exported-constant button1 1)                      ;; #define Button1 		1
(def-exported-constant button1mask 256)                ;; #define Button1Mask 	(1<<8)
(def-exported-constant button1motionmask 256)          ;; #define Button1MotionMask 	(1L<<8)
(def-exported-constant button2 2)                      ;; #define Button2 		2
(def-exported-constant button2mask 512)                ;; #define Button2Mask 	(1<<9)
(def-exported-constant button2motionmask 512)          ;; #define Button2MotionMask 	(1L<<9)
(def-exported-constant button3 3)                      ;; #define Button3 		3
(def-exported-constant button3mask 1024)               ;; #define Button3Mask 	(1<<10)
(def-exported-constant button3motionmask 1024)         ;; #define Button3MotionMask 	(1L<<10)
(def-exported-constant button4 4)                      ;; #define Button4 		4
(def-exported-constant button4mask 2048)               ;; #define Button4Mask 	(1<<11)
(def-exported-constant button4motionmask 2048)         ;; #define Button4MotionMask 	(1L<<11)
(def-exported-constant button5 5)                      ;; #define Button5 		5
(def-exported-constant button5mask 4096)               ;; #define Button5Mask 	(1<<12)
(def-exported-constant button5motionmask 4096)         ;; #define Button5MotionMask 	(1L<<12)
(def-exported-constant buttonmotionmask 8192)          ;; #define ButtonMotionMask 	(1L<<13)
(def-exported-constant buttonpress 4)                  ;; #define ButtonPress 	4
(def-exported-constant buttonpressmask 4)              ;; #define ButtonPressMask 		(1L<<2)
(def-exported-constant buttonrelease 5)                ;; #define ButtonRelease 	5
(def-exported-constant buttonreleasemask 8)            ;; #define ButtonReleaseMask 	(1L<<3)
(def-exported-constant capbutt 1)                      ;; #define CapButt 		1
(def-exported-constant capnotlast 0)                   ;; #define CapNotLast 	0
(def-exported-constant capprojecting 3)                ;; #define CapProjecting 	3
(def-exported-constant capround 2)                     ;; #define CapRound 	2
(def-exported-constant centergravity 5)                ;; #define CenterGravity 	5
(def-exported-constant circulatenotify 26)             ;; #define CirculateNotify 	26
(def-exported-constant circulaterequest 27)            ;; #define CirculateRequest 27
(def-exported-constant clientmessage 33)               ;; #define ClientMessage 	33
(def-exported-constant clipbychildren 0)               ;; #define ClipByChildren 	0
(def-exported-constant colormapchangemask 8388608)     ;; #define ColormapChangeMask 	(1L<<23)
(def-exported-constant colormapinstalled 1)            ;; #define ColormapInstalled 1
(def-exported-constant colormapnotify 32)              ;; #define ColormapNotify 	32
(def-exported-constant colormapuninstalled 0)          ;; #define ColormapUninstalled 0
(def-exported-constant x11-complex 0)                  ;; #define Complex 		0 (uses a different name to avoid clashing with cl:complex - spr34277)
(def-exported-constant configurenotify 22)             ;; #define ConfigureNotify 	22
(def-exported-constant configurerequest 23)            ;; #define ConfigureRequest 23
(def-exported-constant controlmapindex 2)              ;; #define ControlMapIndex 	2
(def-exported-constant controlmask 4)                  ;; #define ControlMask 	(1<<2)
(def-exported-constant convex 2)                       ;; #define Convex 		2
(def-exported-constant coordmodeorigin 0)              ;; #define CoordModeOrigin 	0
(def-exported-constant coordmodeprevious 1)            ;; #define CoordModePrevious       1
(def-exported-constant copyfromparent 0)               ;; #define CopyFromParent       0L
(def-exported-constant createnotify 16)                ;; #define CreateNotify 	16
(def-exported-constant currenttime 0)                  ;; #define CurrentTime          0L
(def-exported-constant cursorshape 0)                  ;; #define CursorShape 	0
(def-exported-constant cwbackingpixel 256)             ;; #define CWBackingPixel         (1L<<8)
(def-exported-constant cwbackingplanes 128)            ;; #define CWBackingPlanes         (1L<<7)
(def-exported-constant cwbackingstore 64)              ;; #define CWBackingStore          (1L<<6)
(def-exported-constant cwbackpixel 2)                  ;; #define CWBackPixel 	(1L<<1)
(def-exported-constant cwbackpixmap 1)                 ;; #define CWBackPixmap 	(1L<<0)
(def-exported-constant cwbitgravity 16)                ;; #define CWBitGravity 	(1L<<4)
(def-exported-constant cwborderpixel 8)                ;; #define CWBorderPixel           (1L<<3)
(def-exported-constant cwborderpixmap 4)               ;; #define CWBorderPixmap 	(1L<<2)
(def-exported-constant cwborderwidth 16)               ;; #define CWBorderWidth 	(1<<4)
(def-exported-constant cwcolormap 8192)                ;; #define CWColormap 	(1L<<13)
(def-exported-constant cwcursor 16384)                 ;; #define CWCursor         (1L<<14)
(def-exported-constant cwdontpropagate 4096)           ;; #define CWDontPropagate         (1L<<12)
(def-exported-constant cweventmask 2048)               ;; #define CWEventMask 	(1L<<11)
(def-exported-constant cwheight 8)                     ;; #define CWHeight 	(1<<3)
(def-exported-constant cwoverrideredirect 512)         ;; #define CWOverrideRedirect (1L<<9)
(def-exported-constant cwsaveunder 1024)               ;; #define CWSaveUnder 	(1L<<10)
(def-exported-constant cwsibling 32)                   ;; #define CWSibling 	(1<<5)
(def-exported-constant cwstackmode 64)                 ;; #define CWStackMode 	(1<<6)
(def-exported-constant cwwidth 4)                      ;; #define CWWidth 		(1<<2)
(def-exported-constant cwwingravity 32)                ;; #define CWWinGravity 	(1L<<5)
(def-exported-constant cwx 1)                          ;; #define CWX 		(1<<0)
(def-exported-constant cwy 2)                          ;; #define CWY 		(1<<1)
(def-exported-constant defaultblanking 2)              ;; #define DefaultBlanking 	2
(def-exported-constant defaultexposures 2)             ;; #define DefaultExposures 2
(def-exported-constant destroyall 0)                   ;; #define DestroyAll              0
(def-exported-constant destroynotify 17)               ;; #define DestroyNotify 	17
(def-exported-constant directcolor 5)                  ;; #define DirectColor 	5
(def-exported-constant disableaccess 0)                ;; #define DisableAccess 	0
(def-exported-constant disablescreeninterval 0)        ;; #define DisableScreenInterval 0
(def-exported-constant disablescreensaver 0)           ;; #define DisableScreenSaver 0
(def-exported-constant doblue 4)                       ;; #define DoBlue 		(1<<2)
(def-exported-constant dogreen 2)                      ;; #define DoGreen 		(1<<1)
(def-exported-constant dontallowexposures 0)           ;; #define DontAllowExposures 0
(def-exported-constant dontpreferblanking 0)           ;; #define DontPreferBlanking 0
(def-exported-constant dored 1)                        ;; #define DoRed 		(1<<0)
(def-exported-constant eastgravity 6)                  ;; #define EastGravity 	6
(def-exported-constant enableaccess 1)                 ;; #define EnableAccess 	1
(def-exported-constant enternotify 7)                  ;; #define EnterNotify 	7
(def-exported-constant enterwindowmask 16)             ;; #define EnterWindowMask 		(1L<<4)
(def-exported-constant evenoddrule 0)                  ;; #define EvenOddRule 	0
(def-exported-constant expose 12)                      ;; #define Expose 		12
(def-exported-constant exposuremask 32768)             ;; #define ExposureMask 		(1L<<15)
(def-exported-constant false 0)                        ;; #define False 0
(def-exported-constant familychaos 2)                  ;; #define FamilyChaos 	2
(def-exported-constant familydecnet 1)                 ;; #define FamilyDECnet 	1
(def-exported-constant familyinternet 0)               ;; #define FamilyInternet 	0
(def-exported-constant fillopaquestippled 3)           ;; #define FillOpaqueStippled 3
(def-exported-constant fillsolid 0)                    ;; #define FillSolid 	0
(def-exported-constant fillstippled 2)                 ;; #define FillStippled 	2
(def-exported-constant filltiled 1)                    ;; #define FillTiled 	1
(def-exported-constant firstextensionerror 128)        ;; #define FirstExtensionError 128
(def-exported-constant focuschangemask 2097152)        ;; #define FocusChangeMask 		(1L<<21)
(def-exported-constant focusin 9)                      ;; #define FocusIn 		9
(def-exported-constant focusout 10)                    ;; #define FocusOut 	10
(def-exported-constant fontchange 255)                 ;; #define FontChange 	255
(def-exported-constant fontlefttoright 0)              ;; #define FontLeftToRight 	0
(def-exported-constant fontrighttoleft 1)              ;; #define FontRightToLeft 	1
(def-exported-constant forgetgravity 0)                ;; #define ForgetGravity 	0
(def-exported-constant gcarcmode 4194304)              ;; #define GCArcMode 	(1L<<22)
(def-exported-constant gcbackground 8)                 ;; #define GCBackground            (1L<<3)
(def-exported-constant gccapstyle 64)                  ;; #define GCCapStyle              (1L<<6)
(def-exported-constant gcclipmask 524288)              ;; #define GCClipMask 	(1L<<19)
(def-exported-constant gcclipxorigin 131072)           ;; #define GCClipXOrigin 	(1L<<17)
(def-exported-constant gcclipyorigin 262144)           ;; #define GCClipYOrigin 	(1L<<18)
(def-exported-constant gcdashlist 2097152)             ;; #define GCDashList 	(1L<<21)
(def-exported-constant gcdashoffset 1048576)           ;; #define GCDashOffset 	(1L<<20)
(def-exported-constant gcfillrule 512)                 ;; #define GCFillRule 	(1L<<9)
(def-exported-constant gcfillstyle 256)                ;; #define GCFillStyle 	(1L<<8)
(def-exported-constant gcfont 16384)                   ;; #define GCFont 			(1L<<14)
(def-exported-constant gcforeground 4)                 ;; #define GCForeground            (1L<<2)
(def-exported-constant gcfunction 1)                   ;; #define GCFunction              (1L<<0)
(def-exported-constant gcgraphicsexposures 65536)      ;; #define GCGraphicsExposures     (1L<<16)
(def-exported-constant gcjoinstyle 128)                ;; #define GCJoinStyle 	(1L<<7)
(def-exported-constant gclastbit 22)                   ;; #define GCLastBit 	22
(def-exported-constant gclinestyle 32)                 ;; #define GCLineStyle             (1L<<5)
(def-exported-constant gclinewidth 16)                 ;; #define GCLineWidth             (1L<<4)
(def-exported-constant gcplanemask 2)                  ;; #define GCPlaneMask             (1L<<1)
(def-exported-constant gcstipple 2048)                 ;; #define GCStipple 	(1L<<11)
(def-exported-constant gcsubwindowmode 32768)          ;; #define GCSubwindowMode 	(1L<<15)
(def-exported-constant gctile 1024)                    ;; #define GCTile 		(1L<<10)
(def-exported-constant gctilestipxorigin 4096)         ;; #define GCTileStipXOrigin (1L<<12)
(def-exported-constant gctilestipyorigin 8192)         ;; #define GCTileStipYOrigin (1L<<13)
(def-exported-constant grabfrozen 4)                   ;; #define GrabFrozen 	4
(def-exported-constant grabinvalidtime 2)              ;; #define GrabInvalidTime 	2
(def-exported-constant grabmodeasync 1)                ;; #define GrabModeAsync 	1
(def-exported-constant grabmodesync 0)                 ;; #define GrabModeSync 	0
(def-exported-constant grabnotviewable 3)              ;; #define GrabNotViewable 	3
(def-exported-constant grabsuccess 0)                  ;; #define GrabSuccess 	0
(def-exported-constant graphicsexpose 13)              ;; #define GraphicsExpose 	13
(def-exported-constant gravitynotify 24)               ;; #define GravityNotify 	24
(def-exported-constant grayscale 1)                    ;; #define GrayScale 	1
(def-exported-constant gxand 1)                        ;; #define GXand 		0x1
(def-exported-constant gxandinverted 4)                ;; #define GXandInverted 	0x4
(def-exported-constant gxandreverse 2)                 ;; #define GXandReverse 	0x2
(def-exported-constant gxclear 0)                      ;; #define GXclear 		0x0
(def-exported-constant gxcopy 3)                       ;; #define GXcopy 		0x3
(def-exported-constant gxcopyinverted 12)              ;; #define GXcopyInverted 	0xc
(def-exported-constant gxequiv 9)                      ;; #define GXequiv 		0x9
(def-exported-constant gxinvert 10)                    ;; #define GXinvert 	0xa
(def-exported-constant gxnand 14)                      ;; #define GXnand 		0xe
(def-exported-constant gxnoop 5)                       ;; #define GXnoop 		0x5
(def-exported-constant gxnor 8)                        ;; #define GXnor 		0x8
(def-exported-constant gxor 7)                         ;; #define GXor 		0x7
(def-exported-constant gxorinverted 13)                ;; #define GXorInverted 	0xd
(def-exported-constant gxorreverse 11)                 ;; #define GXorReverse 	0xb
(def-exported-constant gxset 15)                       ;; #define GXset 		0xf
(def-exported-constant gxxor 6)                        ;; #define GXxor 		0x6
(def-exported-constant hostdelete 1)                   ;; #define HostDelete 	1
(def-exported-constant hostinsert 0)                   ;; #define HostInsert 	0
(def-exported-constant includeinferiors 1)             ;; #define IncludeInferiors 1
(def-exported-constant inputfocus 1)                   ;; #define InputFocus           1L
(def-exported-constant inputonly 2)                    ;; #define InputOnly 	2
(def-exported-constant inputoutput 1)                  ;; #define InputOutput 	1
(def-exported-constant isunmapped 0)                   ;; #define IsUnmapped 	0
(def-exported-constant isunviewable 1)                 ;; #define IsUnviewable 	1
(def-exported-constant isviewable 2)                   ;; #define IsViewable 	2
(def-exported-constant joinbevel 2)                    ;; #define JoinBevel 	2
(def-exported-constant joinmiter 0)                    ;; #define JoinMiter 	0
(def-exported-constant joinround 1)                    ;; #define JoinRound 	1
(def-exported-constant kbautorepeatmode 128)           ;; #define KBAutoRepeatMode (1L<<7)
(def-exported-constant kbbellduration 8)               ;; #define KBBellDuration 	(1L<<3)
(def-exported-constant kbbellpercent 2)                ;; #define KBBellPercent 	(1L<<1)
(def-exported-constant kbbellpitch 4)                  ;; #define KBBellPitch 	(1L<<2)
(def-exported-constant kbkey 64)                       ;; #define KBKey 		(1L<<6)
(def-exported-constant kbkeyclickpercent 1)            ;; #define KBKeyClickPercent (1L<<0)
(def-exported-constant kbled 16)                       ;; #define KBLed 		(1L<<4)
(def-exported-constant kbledmode 32)                   ;; #define KBLedMode 	(1L<<5)
(def-exported-constant keymapnotify 11)                ;; #define KeymapNotify 	11
(def-exported-constant keymapstatemask 16384)          ;; #define KeymapStateMask 		(1L<<14)
(def-exported-constant keypress 2)                     ;; #define KeyPress 	2
(def-exported-constant keypressmask 1)                 ;; #define KeyPressMask 		(1L<<0)
(def-exported-constant keyrelease 3)                   ;; #define KeyRelease 	3
(def-exported-constant keyreleasemask 2)               ;; #define KeyReleaseMask 		(1L<<1)
(def-exported-constant lastevent 35)                   ;; #define LASTEvent 	35
(def-exported-constant lastextensionerror 255)         ;; #define LastExtensionError 255
(def-exported-constant leavenotify 8)                  ;; #define LeaveNotify 	8
(def-exported-constant leavewindowmask 32)             ;; #define LeaveWindowMask 		(1L<<5)
(def-exported-constant ledmodeoff 0)                   ;; #define LedModeOff 	0
(def-exported-constant ledmodeon 1)                    ;; #define LedModeOn 	1
(def-exported-constant linedoubledash 2)               ;; #define LineDoubleDash 	2
(def-exported-constant lineonoffdash 1)                ;; #define LineOnOffDash 	1
(def-exported-constant linesolid 0)                    ;; #define LineSolid 	0
(def-exported-constant lockmapindex 1)                 ;; #define LockMapIndex 	1
(def-exported-constant lockmask 2)                     ;; #define LockMask 	(1<<1)
(def-exported-constant lowerhighest 1)                 ;; #define LowerHighest            1
(def-exported-constant lsbfirst 0)                     ;; #define LSBFirst 	0
(def-exported-constant mapnotify 19)                   ;; #define MapNotify 	19
(def-exported-constant mappingbusy 1)                  ;; #define MappingBusy        	1
(def-exported-constant mappingfailed 2)                ;; #define MappingFailed 	2
(def-exported-constant mappingkeyboard 1)              ;; #define MappingKeyboard 	1
(def-exported-constant mappingmodifier 0)              ;; #define MappingModifier 	0
(def-exported-constant mappingnotify 34)               ;; #define MappingNotify 	34
(def-exported-constant mappingpointer 2)               ;; #define MappingPointer 	2
(def-exported-constant mappingsuccess 0)               ;; #define MappingSuccess     	0
(def-exported-constant maprequest 20)                  ;; #define MapRequest 	20
(def-exported-constant mod1mapindex 3)                 ;; #define Mod1MapIndex 	3
(def-exported-constant mod1mask 8)                     ;; #define Mod1Mask 	(1<<3)
(def-exported-constant mod2mapindex 4)                 ;; #define Mod2MapIndex 	4
(def-exported-constant mod2mask 16)                    ;; #define Mod2Mask 	(1<<4)
(def-exported-constant mod3mapindex 5)                 ;; #define Mod3MapIndex 	5
(def-exported-constant mod3mask 32)                    ;; #define Mod3Mask 	(1<<5)
(def-exported-constant mod4mapindex 6)                 ;; #define Mod4MapIndex 	6
(def-exported-constant mod4mask 64)                    ;; #define Mod4Mask 	(1<<6)
(def-exported-constant mod5mapindex 7)                 ;; #define Mod5MapIndex 	7
(def-exported-constant mod5mask 128)                   ;; #define Mod5Mask 	(1<<7)
(def-exported-constant motionnotify 6)                 ;; #define MotionNotify 	6
(def-exported-constant msbfirst 1)                     ;; #define MSBFirst 	1
(def-exported-constant noeventmask 0)                  ;; #define NoEventMask 		0L
(def-exported-constant noexpose 14)                    ;; #define NoExpose 	14
(def-exported-constant nonconvex 1)                    ;; #define Nonconvex 	1
(def-exported-constant none 0)                         ;; #define None                 0L
(def-exported-constant northeastgravity 3)             ;; #define NorthEastGravity 3
(def-exported-constant northgravity 2)                 ;; #define NorthGravity 	2
(def-exported-constant northwestgravity 1)             ;; #define NorthWestGravity 1
(def-exported-constant nosymbol 0)                     ;; #define NoSymbol      0L
(def-exported-constant notifyancestor 0)               ;; #define NotifyAncestor 	0
(def-exported-constant notifydetailnone 7)             ;; #define NotifyDetailNone 7
(def-exported-constant notifygrab 1)                   ;; #define NotifyGrab 	1
(def-exported-constant notifyhint 1)                   ;; #define NotifyHint 	1
(def-exported-constant notifyinferior 2)               ;; #define NotifyInferior 	2
(def-exported-constant notifynonlinear 3)              ;; #define NotifyNonlinear 	3
(def-exported-constant notifynonlinearvirtual 4)       ;; #define NotifyNonlinearVirtual 4
(def-exported-constant notifynormal 0)                 ;; #define NotifyNormal 	0
(def-exported-constant notifypointer 5)                ;; #define NotifyPointer 	5
(def-exported-constant notifypointerroot 6)            ;; #define NotifyPointerRoot 6
(def-exported-constant notifyungrab 2)                 ;; #define NotifyUngrab 	2
(def-exported-constant notifyvirtual 1)                ;; #define NotifyVirtual 	1
(def-exported-constant notifywhilegrabbed 3)           ;; #define NotifyWhileGrabbed 3
(def-exported-constant notuseful 0)                    ;; #define NotUseful               0
(def-exported-constant opposite 4)                     ;; #define Opposite                4
(def-exported-constant ownergrabbuttonmask 16777216)   ;; #define OwnerGrabButtonMask 	(1L<<24)
(def-exported-constant parentrelative 1)               ;; #define ParentRelative       1L
(def-exported-constant placeonbottom 1)                ;; #define PlaceOnBottom 	1
(def-exported-constant placeontop 0)                   ;; #define PlaceOnTop 	0
(def-exported-constant pointermotionhintmask 128)      ;; #define PointerMotionHintMask 	(1L<<7)
(def-exported-constant pointermotionmask 64)           ;; #define PointerMotionMask 	(1L<<6)
(def-exported-constant pointerroot 1)                  ;; #define PointerRoot          1L
(def-exported-constant pointerwindow 0)                ;; #define PointerWindow        0L
(def-exported-constant preferblanking 1)               ;; #define PreferBlanking 	1
(def-exported-constant propertychangemask 4194304)     ;; #define PropertyChangeMask 	(1L<<22)
(def-exported-constant propertydelete 1)               ;; #define PropertyDelete 	1
(def-exported-constant propertynewvalue 0)             ;; #define PropertyNewValue 0
(def-exported-constant propertynotify 28)              ;; #define PropertyNotify 	28
(def-exported-constant propmodeappend 2)               ;; #define PropModeAppend          2
(def-exported-constant propmodeprepend 1)              ;; #define PropModePrepend         1
(def-exported-constant propmodereplace 0)              ;; #define PropModeReplace         0
(def-exported-constant pseudocolor 3)                  ;; #define PseudoColor 	3
(def-exported-constant queuedafterflush 2)             ;; #define QueuedAfterFlush 2
(def-exported-constant queuedafterreading 1)           ;; #define QueuedAfterReading 1
(def-exported-constant queuedalready 0)                ;; #define QueuedAlready 0
(def-exported-constant raiselowest 0)                  ;; #define RaiseLowest             0
(def-exported-constant reparentnotify 21)              ;; #define ReparentNotify 	21
(def-exported-constant replaykeyboard 5)               ;; #define ReplayKeyboard 	5
(def-exported-constant replaypointer 2)                ;; #define ReplayPointer 	2
(def-exported-constant resizeredirectmask 262144)      ;; #define ResizeRedirectMask 	(1L<<18)
(def-exported-constant resizerequest 25)               ;; #define ResizeRequest 	25
(def-exported-constant retainpermanent 1)              ;; #define RetainPermanent         1
(def-exported-constant retaintemporary 2)              ;; #define RetainTemporary         2
(def-exported-constant reverttoparent 2)               ;; #define RevertToParent 	2
(def-exported-constant reverttopointerroot 1)          ;; #define RevertToPointerRoot (int)PointerRoot
(def-exported-constant reverttonone 0)                 ;; #define RevertToNone 	(int)None
(def-exported-constant screensaveractive 1)            ;; #define ScreenSaverActive 1
(def-exported-constant screensaverreset 0)             ;; #define ScreenSaverReset 0
(def-exported-constant selectionclear 29)              ;; #define SelectionClear 	29
(def-exported-constant selectionnotify 31)             ;; #define SelectionNotify 	31
(def-exported-constant selectionrequest 30)            ;; #define SelectionRequest 30
(def-exported-constant setmodedelete 1)                ;; #define SetModeDelete           1
(def-exported-constant setmodeinsert 0)                ;; #define SetModeInsert           0
(def-exported-constant shiftmapindex 0)                ;; #define ShiftMapIndex 	0
(def-exported-constant shiftmask 1)                    ;; #define ShiftMask 	(1<<0)
(def-exported-constant southeastgravity 9)             ;; #define SouthEastGravity 9
(def-exported-constant southgravity 8)                 ;; #define SouthGravity 	8
(def-exported-constant southwestgravity 7)             ;; #define SouthWestGravity 7
(def-exported-constant staticcolor 2)                  ;; #define StaticColor 	2
(def-exported-constant staticgravity 10)               ;; #define StaticGravity 	10
(def-exported-constant staticgray 0)                   ;; #define StaticGray 	0
(def-exported-constant stippleshape 2)                 ;; #define StippleShape 	2
(def-exported-constant structurenotifymask 131072)     ;; #define StructureNotifyMask 	(1L<<17)
(def-exported-constant substructurenotifymask 524288)  ;; #define SubstructureNotifyMask 	(1L<<19)
(def-exported-constant substructureredirectmask 1048576)  ;; #define SubstructureRedirectMask (1L<<20)
(def-exported-constant success 0)                      ;; #define Success 	   0
(def-exported-constant syncboth 7)                     ;; #define SyncBoth 	7
(def-exported-constant synckeyboard 4)                 ;; #define SyncKeyboard 	4
(def-exported-constant syncpointer 1)                  ;; #define SyncPointer 	1
(def-exported-constant tileshape 1)                    ;; #define TileShape 	1
(def-exported-constant topif 2)                        ;; #define TopIf                   2
(def-exported-constant true 1)                         ;; #define True 1
(def-exported-constant truecolor 4)                    ;; #define TrueColor 	4
(def-exported-constant unix 1)                         ;; #define unix 1
(def-exported-constant unmapgravity 0)                 ;; #define UnmapGravity 	0
(def-exported-constant unmapnotify 18)                 ;; #define UnmapNotify 	18
(def-exported-constant unsorted 0)                     ;; #define Unsorted 	0
(def-exported-constant visibilitychangemask 65536)     ;; #define VisibilityChangeMask 	(1L<<16)
(def-exported-constant visibilityfullyobscured 2)      ;; #define VisibilityFullyObscured 	2
(def-exported-constant visibilitynotify 15)            ;; #define VisibilityNotify 15
(def-exported-constant visibilitypartiallyobscured 1)  ;; #define VisibilityPartiallyObscured 1
(def-exported-constant visibilityunobscured 0)         ;; #define VisibilityUnobscured 	0
(def-exported-constant westgravity 4)                  ;; #define WestGravity 	4
(def-exported-constant whenmapped 1)                   ;; #define WhenMapped              1
(def-exported-constant windingrule 1)                  ;; #define WindingRule 	1
(def-exported-constant xybitmap 0)                     ;; #define XYBitmap 	0
(def-exported-constant xypixmap 1)                     ;; #define XYPixmap 	1
(def-exported-constant ysorted 1)                      ;; #define YSorted 		1
(def-exported-constant yxbanded 3)                     ;; #define YXBanded 	3
(def-exported-constant yxsorted 2)                     ;; #define YXSorted 	2
(def-exported-constant zpixmap 2)                      ;; #define ZPixmap 		2

(def-exported-constant bitmapsuccess 0)                ;; #define BitmapSuccess		0
(def-exported-constant bitmapopenfailed 1)             ;; #define BitmapOpenFailed 	1
(def-exported-constant bitmapfileinvalid 2)            ;; #define BitmapFileInvalid 	2
(def-exported-constant bitmapnomemory 3)               ;; #define BitmapNoMemory		3


;; X Cursor Constants from cursorfont.h

(def-exported-constant xc-x-cursor 0) 		      ;; #define XC_X_cursor 0
(def-exported-constant xc-arrow 2) 		      ;; #define XC_arrow 2
(def-exported-constant xc-based-arrow-down 4)         ;; #define XC_based_arrow_down 4
(def-exported-constant xc-based-arrow-up 6)           ;; #define XC_based_arrow_up 6
(def-exported-constant xc-boat 8)                     ;; #define XC_boat 8
(def-exported-constant xc-bogosity 10)                ;; #define XC_bogosity 10
(def-exported-constant xc-bottom-left-corner 12)      ;; #define XC_bottom_left_corner 12
(def-exported-constant xc-bottom-right-corner 14)     ;; #define XC_bottom_right_corner 14
(def-exported-constant xc-bottom-side 16)             ;; #define XC_bottom_side 16
(def-exported-constant xc-bottom-tee 18)              ;; #define XC_bottom_tee 18
(def-exported-constant xc-box-spiral 20)              ;; #define XC_box_spiral 20
(def-exported-constant xc-center-ptr 22)              ;; #define XC_center_ptr 22
(def-exported-constant xc-circle 24)                  ;; #define XC_circle 24
(def-exported-constant xc-clock 26)                   ;; #define XC_clock 26
(def-exported-constant xc-coffee-mug 28)              ;; #define XC_coffee_mug 28
(def-exported-constant xc-cross 30)                   ;; #define XC_cross 30
(def-exported-constant xc-cross-reverse 32)           ;; #define XC_cross_reverse 32
(def-exported-constant xc-crosshair 34)               ;; #define XC_crosshair 34
(def-exported-constant xc-diamond-cross 36)           ;; #define XC_diamond_cross 36
(def-exported-constant xc-dot 38)                     ;; #define XC_dot 38
(def-exported-constant xc-dotbox 40)                  ;; #define XC_dotbox 40
(def-exported-constant xc-double-arrow 42)            ;; #define XC_double_arrow 42
(def-exported-constant xc-draft-large 44)             ;; #define XC_draft_large 44
(def-exported-constant xc-draft-small 46)             ;; #define XC_draft_small 46
(def-exported-constant xc-draped-box 48)              ;; #define XC_draped_box 48
(def-exported-constant xc-exchange 50)                ;; #define XC_exchange 50
(def-exported-constant xc-fleur 52)                   ;; #define XC_fleur 52
(def-exported-constant xc-gobbler 54)                 ;; #define XC_gobbler 54
(def-exported-constant xc-gumby 56)                   ;; #define XC_gumby 56
(def-exported-constant xc-hand1 58)                   ;; #define XC_hand1 58
(def-exported-constant xc-hand2 60)                   ;; #define XC_hand2 60
(def-exported-constant xc-heart 62)                   ;; #define XC_heart 62
(def-exported-constant xc-icon 64)                    ;; #define XC_icon 64
(def-exported-constant xc-iron-cross 66)              ;; #define XC_iron_cross 66
(def-exported-constant xc-left-ptr 68)                ;; #define XC_left_ptr 68
(def-exported-constant xc-left-side 70)               ;; #define XC_left_side 70
(def-exported-constant xc-left-tee 72)                ;; #define XC_left_tee 72
(def-exported-constant xc-leftbutton 74)              ;; #define XC_leftbutton 74
(def-exported-constant xc-ll-angle 76)                ;; #define XC_ll_angle 76
(def-exported-constant xc-lr-angle 78)                ;; #define XC_lr_angle 78
(def-exported-constant xc-man 80)                     ;; #define XC_man 80
(def-exported-constant xc-middlebutton 82)            ;; #define XC_middlebutton 82
(def-exported-constant xc-mouse 84)                   ;; #define XC_mouse 84
(def-exported-constant xc-pencil 86)                  ;; #define XC_pencil 86
(def-exported-constant xc-pirate 88)                  ;; #define XC_pirate 88
(def-exported-constant xc-plus 90)                    ;; #define XC_plus 90
(def-exported-constant xc-question-arrow 92)          ;; #define XC_question_arrow 92
(def-exported-constant xc-right-ptr 94)               ;; #define XC_right_ptr 94
(def-exported-constant xc-right-side 96)              ;; #define XC_right_side 96
(def-exported-constant xc-right-tee 98)               ;; #define XC_right_tee 98
(def-exported-constant xc-rightbutton 100)            ;; #define XC_rightbutton 100
(def-exported-constant xc-rtl-logo 102)               ;; #define XC_rtl_logo 102
(def-exported-constant xc-sailboat 104)               ;; #define XC_sailboat 104
(def-exported-constant xc-sb-down-arrow 106)          ;; #define XC_sb_down_arrow 106
(def-exported-constant xc-sb-h-double-arrow 108)      ;; #define XC_sb_h_double_arrow 108
(def-exported-constant xc-sb-left-arrow 110)          ;; #define XC_sb_left_arrow 110
(def-exported-constant xc-sb-right-arrow 112)         ;; #define XC_sb_right_arrow 112
(def-exported-constant xc-sb-up-arrow 114)            ;; #define XC_sb_up_arrow 114
(def-exported-constant xc-sb-v-double-arrow 116)      ;; #define XC_sb_v_double_arrow 116
(def-exported-constant xc-shuttle 118)                ;; #define XC_shuttle 118
(def-exported-constant xc-sizing 120)                 ;; #define XC_sizing 120
(def-exported-constant xc-spider 122)                 ;; #define XC_spider 122
(def-exported-constant xc-spraycan 124)               ;; #define XC_spraycan 124
(def-exported-constant xc-star 126)                   ;; #define XC_star 126
(def-exported-constant xc-target 128)                 ;; #define XC_target 128
(def-exported-constant xc-tcross 130)                 ;; #define XC_tcross 130
(def-exported-constant xc-top-left-arrow 132)         ;; #define XC_top_left_arrow 132
(def-exported-constant xc-top-left-corner 134)        ;; #define XC_top_left_corner 134
(def-exported-constant xc-top-right-corner 136)       ;; #define XC_top_right_corner 136
(def-exported-constant xc-top-side 138)               ;; #define XC_top_side 138
(def-exported-constant xc-top-tee 140)                ;; #define XC_top_tee 140
(def-exported-constant xc-trek 142)                   ;; #define XC_trek 142
(def-exported-constant xc-ul-angle 144)               ;; #define XC_ul_angle 144
(def-exported-constant xc-umbrella 146)               ;; #define XC_umbrella 146
(def-exported-constant xc-ur-angle 148)               ;; #define XC_ur_angle 148
(def-exported-constant xc-watch 150)                  ;; #define XC_watch 150
(def-exported-constant xc-xterm 152)                  ;; #define XC_xterm 152


;; Predefined X Atom Constants from Xatom.h

(def-exported-constant xa-primary 1)                  ;; #define XA_PRIMARY ((Atom) 1)
(def-exported-constant xa-secondary 2)                ;; #define XA_SECONDARY ((Atom) 2)
(def-exported-constant xa-arc 3)                      ;; #define XA_ARC ((Atom) 3)
(def-exported-constant xa-atom 4)                     ;; #define XA_ATOM ((Atom) 4)
(def-exported-constant xa-bitmap 5)                   ;; #define XA_BITMAP ((Atom) 5)
(def-exported-constant xa-cardinal 6)                 ;; #define XA_CARDINAL ((Atom) 6)
(def-exported-constant xa-colormap 7)                 ;; #define XA_COLORMAP ((Atom) 7)
(def-exported-constant xa-cursor 8)                   ;; #define XA_CURSOR ((Atom) 8)
(def-exported-constant xa-cut-buffer0 9)              ;; #define XA_CUT_BUFFER0 ((Atom) 9)
(def-exported-constant xa-cut-buffer1 10)             ;; #define XA_CUT_BUFFER1 ((Atom) 10)
(def-exported-constant xa-cut-buffer2 11)             ;; #define XA_CUT_BUFFER2 ((Atom) 11)
(def-exported-constant xa-cut-buffer3 12)             ;; #define XA_CUT_BUFFER3 ((Atom) 12)
(def-exported-constant xa-cut-buffer4 13)             ;; #define XA_CUT_BUFFER4 ((Atom) 13)
(def-exported-constant xa-cut-buffer5 14)             ;; #define XA_CUT_BUFFER5 ((Atom) 14)
(def-exported-constant xa-cut-buffer6 15)             ;; #define XA_CUT_BUFFER6 ((Atom) 15)
(def-exported-constant xa-cut-buffer7 16)             ;; #define XA_CUT_BUFFER7 ((Atom) 16)
(def-exported-constant xa-drawable 17)                ;; #define XA_DRAWABLE ((Atom) 17)
(def-exported-constant xa-font 18)                    ;; #define XA_FONT ((Atom) 18)
(def-exported-constant xa-integer 19)                 ;; #define XA_INTEGER ((Atom) 19)
(def-exported-constant xa-pixmap 20)                  ;; #define XA_PIXMAP ((Atom) 20)
(def-exported-constant xa-point 21)                   ;; #define XA_POINT ((Atom) 21)
(def-exported-constant xa-rectangle 22)               ;; #define XA_RECTANGLE ((Atom) 22)
(def-exported-constant xa-resource-manager 23)        ;; #define XA_RESOURCE_MANAGER ((Atom) 23)
(def-exported-constant xa-rgb-color-map 24)           ;; #define XA_RGB_COLOR_MAP ((Atom) 24)
(def-exported-constant xa-rgb-best-map 25)            ;; #define XA_RGB_BEST_MAP ((Atom) 25)
(def-exported-constant xa-rgb-blue-map 26)            ;; #define XA_RGB_BLUE_MAP ((Atom) 26)
(def-exported-constant xa-rgb-default-map 27)         ;; #define XA_RGB_DEFAULT_MAP ((Atom) 27)
(def-exported-constant xa-rgb-gray-map 28)            ;; #define XA_RGB_GRAY_MAP ((Atom) 28)
(def-exported-constant xa-rgb-green-map 29)           ;; #define XA_RGB_GREEN_MAP ((Atom) 29)
(def-exported-constant xa-rgb-red-map 30)             ;; #define XA_RGB_RED_MAP ((Atom) 30)
(def-exported-constant xa-string 31)                  ;; #define XA_STRING ((Atom) 31)
(def-exported-constant xa-visualid 32)                ;; #define XA_VISUALID ((Atom) 32)
(def-exported-constant xa-window 33)                  ;; #define XA_WINDOW ((Atom) 33)
(def-exported-constant xa-wm-command 34)              ;; #define XA_WM_COMMAND ((Atom) 34)
(def-exported-constant xa-wm-hints 35)                ;; #define XA_WM_HINTS ((Atom) 35)
(def-exported-constant xa-wm-client-machine 36)       ;; #define XA_WM_CLIENT_MACHINE ((Atom) 36)
(def-exported-constant xa-wm-icon-name 37)            ;; #define XA_WM_ICON_NAME ((Atom) 37)
(def-exported-constant xa-wm-icon-size 38)            ;; #define XA_WM_ICON_SIZE ((Atom) 38)
(def-exported-constant xa-wm-name 39)                 ;; #define XA_WM_NAME ((Atom) 39)
(def-exported-constant xa-wm-normal-hints 40)         ;; #define XA_WM_NORMAL_HINTS ((Atom) 40)
(def-exported-constant xa-wm-size-hints 41)           ;; #define XA_WM_SIZE_HINTS ((Atom) 41)
(def-exported-constant xa-wm-zoom-hints 42)           ;; #define XA_WM_ZOOM_HINTS ((Atom) 42)
(def-exported-constant xa-min-space 43)               ;; #define XA_MIN_SPACE ((Atom) 43)
(def-exported-constant xa-norm-space 44)              ;; #define XA_NORM_SPACE ((Atom) 44)
(def-exported-constant xa-max-space 45)               ;; #define XA_MAX_SPACE ((Atom) 45)
(def-exported-constant xa-end-space 46)               ;; #define XA_END_SPACE ((Atom) 46)
(def-exported-constant xa-superscript-x 47)           ;; #define XA_SUPERSCRIPT_X ((Atom) 47)
(def-exported-constant xa-superscript-y 48)           ;; #define XA_SUPERSCRIPT_Y ((Atom) 48)
(def-exported-constant xa-subscript-x 49)             ;; #define XA_SUBSCRIPT_X ((Atom) 49)
(def-exported-constant xa-subscript-y 50)             ;; #define XA_SUBSCRIPT_Y ((Atom) 50)
(def-exported-constant xa-underline-position 51)      ;; #define XA_UNDERLINE_POSITION ((Atom) 51)
(def-exported-constant xa-underline-thickness 52)     ;; #define XA_UNDERLINE_THICKNESS ((Atom) 52)
(def-exported-constant xa-strikeout-ascent 53)        ;; #define XA_STRIKEOUT_ASCENT ((Atom) 53)
(def-exported-constant xa-strikeout-descent 54)       ;; #define XA_STRIKEOUT_DESCENT ((Atom) 54)
(def-exported-constant xa-italic-angle 55)            ;; #define XA_ITALIC_ANGLE ((Atom) 55)
(def-exported-constant xa-x-height 56)                ;; #define XA_X_HEIGHT ((Atom) 56)
(def-exported-constant xa-quad-width 57)              ;; #define XA_QUAD_WIDTH ((Atom) 57)
(def-exported-constant xa-weight 58)                  ;; #define XA_WEIGHT ((Atom) 58)
(def-exported-constant xa-point-size 59)              ;; #define XA_POINT_SIZE ((Atom) 59)
(def-exported-constant xa-resolution 60)              ;; #define XA_RESOLUTION ((Atom) 60)
(def-exported-constant xa-copyright 61)               ;; #define XA_COPYRIGHT ((Atom) 61)
(def-exported-constant xa-notice 62)                  ;; #define XA_NOTICE ((Atom) 62)
(def-exported-constant xa-font-name 63)               ;; #define XA_FONT_NAME ((Atom) 63)
(def-exported-constant xa-family-name 64)             ;; #define XA_FAMILY_NAME ((Atom) 64)
(def-exported-constant xa-full-name 65)               ;; #define XA_FULL_NAME ((Atom) 65)
(def-exported-constant xa-cap-height 66)              ;; #define XA_CAP_HEIGHT ((Atom) 66)
(def-exported-constant xa-wm-class 67)                ;; #define XA_WM_CLASS ((Atom) 67)
(def-exported-constant xa-wm-transient-for 68)        ;; #define XA_WM_TRANSIENT_FOR ((Atom) 68)
(def-exported-constant xa-last-predefined 68)         ;; #define XA_LAST_PREDEFINED ((Atom) 68)


(def-exported-foreign-synonym-type xid unsigned-long)
(def-exported-foreign-synonym-type window xid)
(def-exported-foreign-synonym-type drawable xid)
(def-exported-foreign-synonym-type font xid)
(def-exported-foreign-synonym-type pixmap xid)
(def-exported-foreign-synonym-type cursor xid)
(def-exported-foreign-synonym-type colormap xid)
(def-exported-foreign-synonym-type gcontext xid)
(def-exported-foreign-synonym-type keysym
    #+alpha unsigned-int
    #-alpha xid)
(def-exported-foreign-synonym-type mask unsigned-long)
(def-exported-foreign-synonym-type atom unsigned-long)
(def-exported-foreign-synonym-type visualid unsigned-long)
(def-exported-foreign-synonym-type time
    #+alpha unsigned-int
    #-alpha unsigned-long)
(def-exported-foreign-synonym-type keycode unsigned-char)

(def-exported-foreign-synonym-type xfontset unsigned-long)

(def-exported-foreign-synonym-type fixnum-drawable :fixnum)
(def-exported-foreign-synonym-type fixnum-int :fixnum)
(def-exported-foreign-synonym-type fixnum-unsigned-int :fixnum)

(def-exported-foreign-synonym-type callback-function-addr :signed-32bit)


(def-exported-foreign-struct xextdata
  (number :type int)
  (next :type (:pointer xextdata))
  (free-private :type (:pointer :pointer))
  (private-data :type (:pointer char)))

(def-exported-foreign-struct xextcodes
  (extension :type int)
  (major-opcode :type int)
  (first-event :type int)
  (first-error :type int))

(def-exported-foreign-struct _xextension
  (next :type (:pointer _xextension))
  (codes :type xextcodes)
  (create-gc :type (:pointer :pointer))
  (copy-gc :type (:pointer :pointer))
  (flush-gc :type (:pointer :pointer))
  (free-gc :type (:pointer :pointer))
  (create-font :type (:pointer :pointer))
  (free-font :type (:pointer :pointer))
  (close-display :type (:pointer :pointer))
  (error :type (:pointer :pointer))
  (error-string :type (:pointer :pointer)))

(def-exported-foreign-struct xgcvalues
  (function :type int)
  (plane-mask :type unsigned-long)
  (foreground :type unsigned-long)
  (background :type unsigned-long)
  (line-width :type int)
  (line-style :type int)
  (cap-style :type int)
  (join-style :type int)
  (fill-style :type int)
  (fill-rule :type int)
  (arc-mode :type int)
  (tile :type pixmap)
  (stipple :type pixmap)
  (ts-x-origin :type int)
  (ts-y-origin :type int)
  (font :type font)
  (subwindow-mode :type int)
  (graphics-exposures :type int)
  (clip-x-origin :type int)
  (clip-y-origin :type int)
  (clip-mask :type pixmap)
  (dash-offset :type int)
  (dashes :type char))

(def-exported-foreign-struct _xgc
  (ext-data :type (:pointer xextdata))
  (gid :type gcontext)
  (rects :type int)
  (dashes :type int)
  (dirty :type unsigned-long)
  (values :type xgcvalues))
(def-exported-foreign-synonym-type gc (:pointer _xgc))

(def-exported-foreign-struct visual
  (ext-data :type (:pointer xextdata))
  (visualid :type visualid)
  (class :type int)
  (red-mask :type unsigned-long)
  (green-mask :type unsigned-long)
  (blue-mask :type unsigned-long)
  (bits-per-rgb :type int)
  (map-entries :type int))

(def-exported-foreign-struct depth
  (depth :type int)
  (nvisuals :type int)
  (visuals :type (:pointer visual)))

(def-exported-foreign-struct screen
  (ext-data :type (:pointer xextdata))
  (display :type (:pointer display))
  (root :type window)
  (width :type int)
  (height :type int)
  (mwidth :type int)
  (mheight :type int)
  (ndepths :type int)
  (depths :type (:pointer depth))
  (root-depth :type int)
  (root-visual :type (:pointer visual))
  (default-gc :type gc)
  (cmap :type colormap)
  (white-pixel :type unsigned-long)
  (black-pixel :type unsigned-long)
  (max-maps :type int)
  (min-maps :type int)
  (backing-store :type int)
  (save-unders :type int)
  (root-input-mask :type long))

(def-exported-foreign-struct screenformat
  (ext-data :type (:pointer xextdata))
  (depth :type int)
  (bits-per-pixel :type int)
  (scanline-pad :type int))

(def-exported-foreign-struct xsetwindowattributes
  (background-pixmap :type pixmap)
  (background-pixel :type unsigned-long)
  (border-pixmap :type pixmap)
  (border-pixel :type unsigned-long)
  (bit-gravity :type int)
  (win-gravity :type int)
  (backing-store :type int)
  (backing-planes :type unsigned-long)
  (backing-pixel :type unsigned-long)
  (save-under :type int)
  (event-mask :type long)
  (do-not-propagate-mask :type long)
  (override-redirect :type int)
  (colormap :type colormap)
  (cursor :type cursor))

(def-exported-foreign-struct xwindowattributes
  (x :type int)
  (y :type int)
  (width :type int)
  (height :type int)
  (border-width :type int)
  (depth :type int)
  (visual :type (:pointer visual))
  (root :type window)
  (class :type int)
  (bit-gravity :type int)
  (win-gravity :type int)
  (backing-store :type int)
  (backing-planes :type unsigned-long)
  (backing-pixel :type unsigned-long)
  (save-under :type int)
  (colormap :type colormap)
  (map-installed :type int)
  (map-state :type int)
  (all-event-masks :type long)
  (your-event-mask :type long)
  (do-not-propagate-mask :type long)
  (override-redirect :type int)
  (screen :type (:pointer screen)))

(def-exported-foreign-struct xhostaddress
  (family :type int)
  (length :type int)
  (address :type (:pointer char)))

(def-exported-foreign-struct funcs
  (create-image :type (:pointer :pointer))
  (destroy-image :type (:pointer :pointer))
  (get-pixel :type (:pointer :pointer))
  (put-pixel :type (:pointer :pointer))
  (sub-image :type (:pointer :pointer))
  (add-pixel :type (:pointer :pointer)))

(def-exported-foreign-struct ximage
  (width :type int)
  (height :type int)
  (xoffset :type int)
  (format :type int)
  (data :type (:pointer char))
  (byte-order :type int)
  (bitmap-unit :type int)
  (bitmap-bit-order :type int)
  (bitmap-pad :type int)
  (depth :type int)
  (bytes-per-line :type int)
  (bits-per-pixel :type int)
  (red-mask :type unsigned-long)
  (green-mask :type unsigned-long)
  (blue-mask :type unsigned-long)
  (obdata :type (:pointer char))
  (f :type funcs))

(def-exported-foreign-struct xwindowchanges
  (x :type int)
  (y :type int)
  (width :type int)
  (height :type int)
  (border-width :type int)
  (sibling :type window)
  (stack-mode :type int))

(def-exported-foreign-struct (xcolor :array)
  (pixel :type unsigned-long)
  (red :type unsigned-short)
  (green :type unsigned-short)
  (blue :type unsigned-short)
  (flags :type unsigned-char)
  (pad :type unsigned-char))

(def-exported-foreign-struct (xsegment :array)
  (x1 :type short)
  (y1 :type short)
  (x2 :type short)
  (y2 :type short))

(def-exported-foreign-struct (xpoint :array)
  (x :type short)
  (y :type short))

(def-exported-foreign-struct (xrectangle :array)
  (x :type short)
  (y :type short)
  (width :type unsigned-short)
  (height :type unsigned-short))

(def-exported-foreign-struct (xarc :array)
  (x :type short)
  (y :type short)
  (width :type unsigned-short)
  (height :type unsigned-short)
  (angle1 :type short)
  (angle2 :type short))

(def-exported-foreign-struct xkeyboardcontrol
  (key-click-percent :type int)
  (bell-percent :type int)
  (bell-pitch :type int)
  (bell-duration :type int)
  (led :type int)
  (led-mode :type int)
  (key :type int)
  (auto-repeat-mode :type int))

(def-exported-foreign-struct xkeyboardstate
  (key-click-percent :type int)
  (bell-percent :type int)
  (bell-pitch :type unsigned-int)
  (bell-duration :type unsigned-int)
  (led-mask :type unsigned-long)
  (global-auto-repeat :type int)
  (auto-repeats :type (:array char (32))))

(def-exported-foreign-struct xtimecoord
  (time :type time)
  (x :type short)
  (y :type short))

(def-exported-foreign-struct xmodifierkeymap
  (max-keypermod :type int)
  (modifiermap :type (:pointer keycode)))

(def-exported-foreign-struct display
  (ext-data :type (:pointer xextdata))
  (next :type (:pointer display))
  (fd :type int)
  (lock :type int)
  (proto-major-version :type int)
  (proto-minor-version :type int)
  (vendor :type (:pointer char))
  (resource-base :type long)
  (resource-mask :type long)
  (resource-id :type long)
  (resource-shift :type int)
  (resource-alloc :type (:pointer :pointer))
  (byte-order :type int)
  (bitmap-unit :type int)
  (bitmap-pad :type int)
  (bitmap-bit-order :type int)
  (nformats :type int)
  (pixmap-format :type (:pointer screenformat))
  (vnumber :type int)
  (release :type int)
  (head :type (:pointer _xsqevent))
  (tail :type (:pointer _xsqevent))
  (qlen :type int)
  (last-request-read :type unsigned-long)
  (request :type unsigned-long)
  (last-req :type (:pointer char))
  (buffer :type (:pointer char))
  (bufptr :type (:pointer char))
  (bufmax :type (:pointer char))
  (max-request-size :type unsigned)
  (db :type (:pointer _xrmhashbucketrec))
  (synchandler :type (:pointer :pointer))
  (display-name :type (:pointer char))
  (default-screen :type int)
  (nscreens :type int)
  (screens :type (:pointer screen))
  (motion-buffer :type unsigned-long)
  (current :type window)
  (min-keycode :type int)
  (max-keycode :type int)
  (keysyms :type (:pointer keysym))
  (modifiermap :type (:pointer xmodifierkeymap))
  (keysyms-per-keycode :type int)
  (xdefaults :type (:pointer char))
  (scratch-buffer :type (:pointer char))
  (scratch-length :type unsigned-long)
  (ext-number :type int)
  (ext-procs :type (:pointer _xextension))
  (event-vec :type (:array (:pointer :pointer) (128)))
  (wire-vec :type (:array (:pointer :pointer) (128)))
  (lock-meaning :type keysym)
  (key-bindings :type (:pointer xkeytrans))
  (cursor-font :type font))

(def-exported-foreign-synonym-type _xdisplay display)

(def-exported-foreign-struct xkeyevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (window :type window)
  (root :type window)
  (subwindow :type window)
  (time :type time)
  (x :type int)
  (y :type int)
  (x-root :type int)
  (y-root :type int)
  (state :type unsigned-int)
  (keycode :type unsigned-int)
  (same-screen :type int))
(def-exported-foreign-synonym-type xkeypressedevent xkeyevent)
(def-exported-foreign-synonym-type xkeyreleasedevent xkeyevent)

(def-exported-foreign-struct xbuttonevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (window :type window)
  (root :type window)
  (subwindow :type window)
  (time :type time)
  (x :type int)
  (y :type int)
  (x-root :type int)
  (y-root :type int)
  (state :type unsigned-int)
  (button :type unsigned-int)
  (same-screen :type int))

(def-exported-foreign-synonym-type xbuttonpressedevent xbuttonevent)
(def-exported-foreign-synonym-type xbuttonreleasedevent xbuttonevent)

(def-exported-foreign-struct xmotionevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (window :type window)
  (root :type window)
  (subwindow :type window)
  (time :type time)
  (x :type int)
  (y :type int)
  (x-root :type int)
  (y-root :type int)
  (state :type unsigned-int)
  (is-hint :type char)
  (same-screen :type int))

(def-exported-foreign-synonym-type xpointermovedevent xmotionevent)

(def-exported-foreign-struct xcrossingevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (window :type window)
  (root :type window)
  (subwindow :type window)
  (time :type time)
  (x :type int)
  (y :type int)
  (x-root :type int)
  (y-root :type int)
  (mode :type int)
  (detail :type int)
  (same-screen :type int)
  (focus :type int)
  (state :type unsigned-int))

(def-exported-foreign-synonym-type xenterwindowevent xcrossingevent)
(def-exported-foreign-synonym-type xleavewindowevent xcrossingevent)

(def-exported-foreign-struct xfocuschangeevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (window :type window)
  (mode :type int)
  (detail :type int))

(def-exported-foreign-synonym-type xfocusinevent  xfocuschangeevent)
(def-exported-foreign-synonym-type xfocusoutevent xfocuschangeevent)

(def-exported-foreign-struct xkeymapevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (window :type window)
  (key-vector :type (:array char (32))))

(def-exported-foreign-struct xexposeevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (window :type window)
  (x :type int)
  (y :type int)
  (width :type int)
  (height :type int)
  (count :type int))

(def-exported-foreign-struct xgraphicsexposeevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (drawable :type drawable)
  (x :type int)
  (y :type int)
  (width :type int)
  (height :type int)
  (count :type int)
  (major-code :type int)
  (minor-code :type int))

(def-exported-foreign-struct xnoexposeevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (drawable :type drawable)
  (major-code :type int)
  (minor-code :type int))

(def-exported-foreign-struct xvisibilityevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (window :type window)
  (state :type int))

(def-exported-foreign-struct xcreatewindowevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (parent :type window)
  (window :type window)
  (x :type int)
  (y :type int)
  (width :type int)
  (height :type int)
  (border-width :type int)
  (override-redirect :type int))

(def-exported-foreign-struct xdestroywindowevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (event :type window)
  (window :type window))

(def-exported-foreign-struct xunmapevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (event :type window)
  (window :type window)
  (from-configure :type int))

(def-exported-foreign-struct xmapevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (event :type window)
  (window :type window)
  (override-redirect :type int))

(def-exported-foreign-struct xmaprequestevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (parent :type window)
  (window :type window))

(def-exported-foreign-struct xreparentevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (event :type window)
  (window :type window)
  (parent :type window)
  (x :type int)
  (y :type int)
  (override-redirect :type int))

(def-exported-foreign-struct xconfigureevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (event :type window)
  (window :type window)
  (x :type int)
  (y :type int)
  (width :type int)
  (height :type int)
  (border-width :type int)
  (above :type window)
  (override-redirect :type int))

(def-exported-foreign-struct xgravityevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (event :type window)
  (window :type window)
  (x :type int)
  (y :type int))

(def-exported-foreign-struct xresizerequestevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (window :type window)
  (width :type int)
  (height :type int))

(def-exported-foreign-struct xconfigurerequestevent
   (type :type int)
   (serial :type unsigned-long)
   (send-event :type int)
   (display :type (:pointer display))
   (parent :type window)
   (window :type window)
   (x :type int)
   (y :type int)
   (width :type int)
   (height :type int)
   (border-width :type int)
   (above :type window)
   (detail :type int)
   (value-mask :type unsigned-long))

(def-exported-foreign-struct xcirculateevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (event :type window)
  (window :type window)
  (place :type int))

(def-exported-foreign-struct xcirculaterequestevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type
	   (:pointer display))
  (parent :type window)
  (window :type window)
  (place :type int))

(def-exported-foreign-struct xpropertyevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (window :type window)
  (atom :type atom)
  (time :type time)
  (state :type int))

(def-exported-foreign-struct xselectionclearevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (window :type window)
  (selection :type atom)
  (time :type time))

(def-exported-foreign-struct xselectionrequestevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type
	   (:pointer display))
  (owner :type window)
  (requestor :type window)
  (selection :type atom)
  (target :type atom)
  (property :type atom)
  (time :type time))

(def-exported-foreign-struct xselectionevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (requestor :type window)
  (selection :type atom)
  (target :type atom)
  (property :type atom)
  (time :type time))

(def-exported-foreign-struct xcolormapevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (window :type window)
  (colormap :type colormap)
  (new :type int)
  (state :type int))

(def-exported-foreign-struct bsl-union
  (b :type (:array char (20)))
  (s :type (:array short (10)) :overlays b)
  (l :type (:array long (5)) :overlays b))

(def-exported-foreign-struct xclientmessageevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (window :type window)
  (message-type :type atom)
  (format :type int)
  (data :type bsl-union))

(def-exported-foreign-struct xmappingevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (window :type window)
  (request :type int)
  (first-keycode :type int)
  (count :type int))

(def-exported-foreign-struct xerrorevent
  (type :type int)
  (display :type (:pointer display))
  (resourceid :type xid)
  (serial :type unsigned-long)
  (error-code :type unsigned-char)
  (request-code :type unsigned-char)
  (minor-code :type unsigned-char))

(def-exported-foreign-struct xanyevent
  (type :type int)
  (serial :type unsigned-long)
  (send-event :type int)
  (display :type (:pointer display))
  (window :type window))

(def-exported-foreign-struct (xevent :no-substruct-accessors)
  (type :type int)
  (xany :type xanyevent :overlays type)
  (xkey :type xkeyevent :overlays xany)
  (xbutton :type xbuttonevent :overlays xany)
  (xmotion :type xmotionevent :overlays xany)
  (xcrossing :type xcrossingevent :overlays xany)
  (xfocus :type xfocuschangeevent :overlays xany)
  (xexpose :type xexposeevent :overlays xany)
  (xgraphicsexpose :type xgraphicsexposeevent :overlays xany)
  (xnoexpose :type xnoexposeevent :overlays xany)
  (xvisibility :type xvisibilityevent :overlays xany)
  (xcreatewindow :type xcreatewindowevent :overlays xany)
  (xdestroywindow :type xdestroywindowevent :overlays xany)
  (xunmap :type xunmapevent :overlays xany)
  (xmap :type xmapevent :overlays xany)
  (xmaprequest :type xmaprequestevent :overlays xany)
  (xreparent :type xreparentevent :overlays xany)
  (xconfigure :type xconfigureevent :overlays xany)
  (xgravity :type xgravityevent :overlays xany)
  (xresizerequest :type xresizerequestevent :overlays xany)
  (xconfigurerequest :type xconfigurerequestevent :overlays xany)
  (xcirculate :type xcirculateevent :overlays xany)
  (xcirculaterequest :type xcirculaterequestevent :overlays xany)
  (xproperty :type xpropertyevent :overlays xany)
  (xselectionclear :type xselectionclearevent :overlays xany)
  (xselectionrequest :type xselectionrequestevent :overlays xany)
  (xselection :type xselectionevent :overlays xany)
  (xcolormap :type xcolormapevent :overlays xany)
  (xclient :type xclientmessageevent :overlays xany)
  (xmapping :type xmappingevent :overlays xany)
  (xerror :type xerrorevent :overlays xany)
  (xkeymap :type xkeymapevent :overlays xany)
  (pad :type (:array long (24)) :overlays xany))

(def-exported-foreign-synonym-type _xevent xevent)

(def-exported-foreign-struct _xqevent
  (next :type (:pointer _xsqevent))
  (event :type xevent))

(def-exported-foreign-synonym-type _xsqevent _xqevent)

(def-exported-foreign-struct xcharstruct
  (lbearing :type short)
  (rbearing :type short)
  (width :type short)
  (ascent :type short)
  (descent :type short)
  (attributes :type unsigned-short))

(def-exported-foreign-struct xfontprop
  (name :type atom)
  (card32 :type unsigned-long))

(def-exported-foreign-struct xfontstruct
  (ext-data :type (:pointer xextdata))
  (fid :type font)
  (direction :type unsigned)
  (min-char-or-byte2 :type unsigned)
  (max-char-or-byte2 :type unsigned)
  (min-byte1 :type unsigned)
  (max-byte1 :type unsigned)
  (all-chars-exist :type int)
  (default-char :type unsigned)
  (n-properties :type int)
  (properties :type (:pointer xfontprop))
  (min-bounds :type xcharstruct)
  (max-bounds :type xcharstruct)
  (per-char :type (:pointer xcharstruct))
  (ascent :type int)
  (descent :type int))

(def-exported-foreign-struct xtextitem
  (chars :type (:pointer char))
  (nchars :type int)
  (delta :type int)
  (font :type font))

(def-exported-foreign-struct xchar2b
  (byte1 :type unsigned-char)
  (byte2 :type unsigned-char))

(def-exported-foreign-struct xtextitem16
  (chars :type (:pointer xchar2b))
  (nchars :type int)
  (delta :type int)
  (font :type font))


#|
typedef union { Display *display;
		GC gc;
		Visual *visual;
		Screen *screen;
		ScreenFormat *pixmap_format;
		XFontStruct *font; } XEDataObject;
|#
(def-exported-foreign-synonym-type xedataobject caddr-t)

;;; Xlib Resource Manager Definitions from Xresource.h

(def-exported-foreign-synonym-type xrmquark int)
(def-exported-foreign-synonym-type xrmquarklist (:pointer int))
(def-exported-foreign-synonym-type xrmstring (:pointer char))

(defconstant XrmBindTightly 0)
(defconstant XrmBindLoosely 1)
(def-exported-foreign-synonym-type XrmBinding :fixnum)
(def-exported-foreign-synonym-type XrmBindingList (:pointer :fixnum))

(def-exported-foreign-synonym-type xrmname xrmquark)
(def-exported-foreign-synonym-type xrmnamelist xrmquarklist)
(def-exported-foreign-synonym-type xrmclass xrmquark)
(def-exported-foreign-synonym-type xrmclasslist xrmquarklist)
(def-exported-foreign-synonym-type xrmrepresentation xrmquark)

(def-exported-foreign-struct xrmvalue
  (size :type unsigned-int)
  (addr :type caddr-t))
(def-exported-foreign-synonym-type xrmvalueptr (:pointer xrmvalue))

(def-exported-foreign-synonym-type xrmsearchlist int)
(def-exported-foreign-synonym-type xrmdatabase int)
(def-exported-foreign-synonym-type xim int)
(def-exported-foreign-synonym-type xic int)

(defconstant XNInputStyle "inputStyle")
(defconstant XNClientWindow "clientWindow")
(defconstant XNFocusWindow "focusWindow")
(defconstant XNPreeditState "preeditState")
(defconstant XIMPreeditNothing #x8)
(defconstant XIMPreeditPosition #x4)
(defconstant XIMStatusNothing #x400)
(defconstant XIMStatusNone #x800)

(defconstant XIMPreeditEnable #x1)

(defconstant XBufferOverflow -1)
(defconstant XLookupNone 1)
(defconstant XLookupChars 2)
(defconstant XLookupKeySym 3)
(defconstant XLookupKeyBoth 4)


(defconstant XrmoptionNoArg 0)
(defconstant XrmoptionIsArg 1)
(defconstant XrmoptionStickyArg 2)
(defconstant XrmoptionSepArg 3)
(defconstant XrmoptionResArg 4)
(defconstant XrmoptionSkipArg 5)
(defconstant XrmoptionSkipLine 6)
(def-exported-foreign-synonym-type XrmOptionKind int)

(def-exported-foreign-struct xrmoptiondescrec
  (option :type (:pointer char))
  (specifier :type (:pointer char))
  (argkind :type XrmOptionKind)
  (value :type caddr-t))
(def-exported-foreign-synonym-type xrmoptiondesclist (:pointer xrmoptiondescrec))


;;; Utility Definitions from Xutil.h

(def-exported-foreign-struct xwmhints
  (flags :type long)
  (input :type int)
  (initial-state :type int)
  (icon-pixmap :type pixmap)
  (icon-window :type window)
  (icon-x :type int)
  (icon-y :type int)
  (icon-mask :type pixmap)
  (window-group :type xid))

(def-exported-constant inputhint 1)          ;; #define InputHint        (1L << 0)
(def-exported-constant statehint 2)          ;; #define StateHint        (1L << 1)
(def-exported-constant iconpixmaphint 4)     ;; #define IconPixmapHint   (1L << 2)
(def-exported-constant iconwindowhint 8)     ;; #define IconWindowHint   (1L << 3)
(def-exported-constant iconpositionhint 16)  ;; #define IconPositionHint (1L << 4)
(def-exported-constant iconmaskhint 32)      ;; #define IconMaskHint     (1L << 5)
(def-exported-constant windowgrouphint 64)   ;; #define WindowGroupHint  (1L << 6)

(def-exported-constant WithdrawnState 0)     ;; #define WithdrawnState 0
(def-exported-constant NormalState 1)        ;; #define NormalState 1
(def-exported-constant IconicState 3)        ;; #define IconicState 3
(def-exported-constant DontCareState 0)      ;; #define DontCareState 0
(def-exported-constant ZoomState 2)          ;; #define ZoomState 2
(def-exported-constant InactiveState 4)      ;; #define InactiveState 4

(def-exported-foreign-struct xsizehints
  (flags :type long)
  (x :type int)				; Obsolete
  (y :type int)				; Obsolete
  (width :type int)			; Obsolete
  (height :type int)			; Obsolete
  (min-width :type int)
  (min-height :type int)
  (max-width :type int)
  (max-height :type int)
  (width-inc :type int)
  (height-inc :type int)
  (min-aspect-x :type int)
  (min-aspect-y :type int)
  (max-aspect-x :type int)
  (max-aspect-y :type int)
  (base-width :type int)
  (base-height :type int)
  (win-gravity :type int))

(def-exported-constant uspositionhint 1)
(def-exported-constant ussizehint 2)
(def-exported-constant ppositionhint 4)
(def-exported-constant psizehint 8)
(def-exported-constant pminsizehint 16)
(def-exported-constant pmaxsizehint 32)
(def-exported-constant presizeincint 64)
(def-exported-constant paspecthint 128)
(def-exported-constant pbasesizehint 256)
(def-exported-constant pwingravityhint 512)

(def-exported-constant xcsuccess 0)  ;; #define XCSUCCESS 0
(def-exported-constant xcnomem   1)  ;; #define XCNOMEM   1
(def-exported-constant xcnoent   2)  ;; #define XCNOENT   2

(def-exported-foreign-synonym-type xcontext int)


(def-exported-foreign-struct xcomposestatus
  (compose-ptr :type (:pointer char))
  (chars-matched :type int))


;;; Untranslated C preprocessor #define statements
#|

#define MinCmapsOfScreen(s) ((s)->min_maps)
#define DoesSaveUnders(s) ((s)->save_unders)
#define EventMaskOfScreen(s) ((s)->root_input_mask)
#define ScreenCount(dpy) 	((dpy)->nscreens)
#define HeightOfScreen(s) ((s)->height)
#define RootWindowOfScreen(s) ((s)->root)
#define BitmapUnit(dpy) 	((dpy)->bitmap_unit)
#define MaxCmapsOfScreen(s) ((s)->max_maps)
#define ProtocolVersion(dpy) 	((dpy)->proto_major_version)
#define DoesBackingStore(s) ((s)->backing_store)
#define WhitePixel(dpy, scr) 	(((dpy)->screens[(scr)]).white_pixel)
#define PlanesOfScreen(s) ((s)->root_depth)
#define DefaultGC(dpy, scr) 	(((dpy)->screens[(scr)]).default_gc)
#define DefaultScreen(dpy) 	((dpy)->default_screen)
#define DisplayHeightMM(dpy, scr) (((dpy)->screens[(scr)]).mheight)
#define WidthMMOfScreen(s) ((s)->mwidth)
#define DisplayWidthMM(dpy, scr) (((dpy)->screens[(scr)]).mwidth)
#define ConnectionNumber(dpy) 	((dpy)->fd)
#define ProtocolRevision(dpy) 	((dpy)->proto_minor_version)
#define RootWindow(dpy, scr) 	(((dpy)->screens[(scr)]).root)
#define DefaultScreenOfDisplay(dpy) (&((dpy)->screens[(dpy)->default_screen]))
#define ScreenOfDisplay(dpy, scr) (&((dpy)->screens[(scr)]))
#define XAllocID(dpy) ((*(dpy)->resource_alloc)((dpy)))
#define BitmapPad(dpy) 		((dpy)->bitmap_pad)
#define VendorRelease(dpy) 	((dpy)->release)
#define DefaultDepth(dpy, scr) 	(((dpy)->screens[(scr)]).root_depth)
#define ServerVendor(dpy) 	((dpy)->vendor)
#define DefaultDepthOfScreen(s) ((s)->root_depth)
#define DisplayPlanes(dpy, scr) (((dpy)->screens[(scr)]).root_depth)
#define DisplayWidth(dpy, scr) 	(((dpy)->screens[(scr)]).width)
#define DisplayOfScreen(s) ((s)->display)
#define DefaultColormap(dpy, scr) (((dpy)->screens[(scr)]).cmap)
#define BlackPixel(dpy, scr) 	(((dpy)->screens[(scr)]).black_pixel)
#define HeightMMOfScreen(s) ((s)->mheight)
#define DefaultVisualOfScreen(s) ((s)->root_visual)
#define BitmapBitOrder(dpy) 	((dpy)->bitmap_bit_order)
#define LastKnownRequestProcessed(dpy) ((dpy)->last_request_read)
#define DisplayHeight(dpy, scr) (((dpy)->screens[(scr)]).height)
#define NextRequest(dpy) ((dpy)->request + 1)
#define DefaultVisual(dpy, scr) (((dpy)->screens[(scr)]).root_visual)
#define BlackPixelOfScreen(s) ((s)->black_pixel)
#define ImageByteOrder(dpy) 	((dpy)->byte_order)
#define DisplayCells(dpy, scr) 	(DefaultVisual((dpy), (scr))->map_entries)
#define DefaultColormapOfScreen(s) ((s)->cmap)
#define CellsOfScreen(s) (DefaultVisualOfScreen((s))->map_entries)
#define WidthOfScreen(s) ((s)->width)
#define DefaultGCOfScreen(s) ((s)->default_gc)
#define DefaultRootWindow(dpy) 	(((dpy)->screens[(dpy)->default_screen]).root)
#define AllPlanes 		(~0)
#define DisplayString(dpy) 	((dpy)->display_name)
#define QLength(dpy) 		((dpy)->qlen)
#define WhitePixelOfScreen(s) ((s)->white_pixel)

|#
