;; -*- mode: common-lisp; package: x11 -*-
;;; (c) Copyright  1990 Sun Microsystems, Inc.  All Rights Reserved.
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

;;; $fiHeader: xlib-funs.lisp,v 1.6 92/05/13 17:10:48 cer Exp $

(in-package :x11)

(def-exported-foreign-function (xalloccolorcells (:return-type int) (:name "_XAllocColorCells"))
   (dpy (:pointer display))
   (cmap colormap)
   (contig int)
   (ncolors unsigned-int)
   (nplanes unsigned-int)
   (masks (:pointer unsigned-long))
   (pixels (:pointer unsigned-long)))

(def-exported-foreign-function (xalloccolorplanes (:return-type int) (:name "_XAllocColorPlanes")) 
   (dpy (:pointer display))
   (cmap colormap)
   (contig int)
   (pixels (:pointer unsigned-long))
   (ncolors int)
   (nreds int)
   (ngreens int)
   (nblues int)
   (rmask (:pointer unsigned-long))
   (gmask (:pointer unsigned-long))
   (bmask (:pointer unsigned-long)))

(def-exported-foreign-function (xallowevents (:return-type int) (:name "_XAllowEvents")) 
   (dpy (:pointer display))
   (mode int)
   (time :unsigned-32bit))

(def-exported-foreign-function (xautorepeaton (:return-type int) (:name "_XAutoRepeatOn")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xautorepeatoff (:return-type int) (:name "_XAutoRepeatOff")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xsetwindowbackground (:return-type int) (:name "_XSetWindowBackground")) 
   (dpy (:pointer display))
   (w window)
   (pixel unsigned-long))

(def-exported-foreign-function (xsetwindowborderwidth (:return-type int) (:name "_XSetWindowBorderWidth")) 
   (dpy (:pointer display))
   (w window)
   (width unsigned-int))

(def-exported-foreign-function (xbell (:return-type int) (:name "_XBell")) 
   (dpy (:pointer display))
   (percent int))

(def-exported-foreign-function (xsetwindowborder (:return-type int) (:name "_XSetWindowBorder")) 
   (dpy (:pointer display))
   (w window)
   (pixel unsigned-long))

(def-exported-foreign-function (xenableaccesscontrol (:return-type int) (:name "_XEnableAccessControl")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xdisableaccesscontrol (:return-type int) (:name "_XDisableAccessControl")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xsetaccesscontrol (:return-type int) (:name "_XSetAccessControl")) 
   (dpy (:pointer display))
   (mode int))

(def-exported-foreign-function (xchangeactivepointergrab (:return-type int) (:name "_XChangeActivePointerGrab")) 
   (dpy (:pointer display))
   (event-mask unsigned-int)
   (curs cursor)
   (time :unsigned-32bit))

(def-exported-foreign-function (xsetclosedownmode (:return-type int) (:name "_XSetCloseDownMode")) 
   (dpy (:pointer display))
   (mode int))

(def-exported-foreign-function (xsetwindowcolormap (:return-type int) (:name "_XSetWindowColormap")) 
   (dpy (:pointer display))
   (w window)
   (colormap colormap))

(def-exported-foreign-function (xchangegc (:return-type int) (:name "_XChangeGC")) 
   (dpy (:pointer display))
   (gc gc)
   (valuemask unsigned-long)
   (values (:pointer xgcvalues)))

(def-exported-foreign-function (xchangekeyboardcontrol (:return-type int) (:name "_XChangeKeyboardControl")) 
   (dpy (:pointer display))
   (mask unsigned-long)
   (value-list (:pointer xkeyboardcontrol)))

(def-exported-foreign-function (xchangepointercontrol (:return-type int) (:name "_XChangePointerControl")) 
   (dpy (:pointer display))
   (do-acc int)
   (do-thresh int)
   (acc-numerator int)
   (acc-denominator int)
   (threshold int))

(def-exported-foreign-function (xchangeproperty (:return-type int) (:name "_XChangeProperty")) 
   (dpy (:pointer display))
   (w window)
   (property :unsigned-32bit)
   (type :unsigned-32bit)
   (format int)
   (mode int)
   (data (:pointer unsigned-char))
   (nelements int))

(def-exported-foreign-function (xchangesaveset (:return-type int) (:name "_XChangeSaveSet")) 
   (dpy (:pointer display))
   (win window)
   (mode int))

(def-exported-foreign-function (xaddtosaveset (:return-type int) (:name "_XAddToSaveSet")) 
   (dpy (:pointer display))
   (win window))

(def-exported-foreign-function (xremovefromsaveset (:return-type int) (:name "_XRemoveFromSaveSet")) 
   (dpy (:pointer display))
   (win window))

(def-exported-foreign-function (xchangewindowattributes (:return-type int) (:name "_XChangeWindowAttributes")) 
   (dpy (:pointer display))
   (w window)
   (valuemask unsigned-long)
   (attributes (:pointer xsetwindowattributes)))

(def-exported-foreign-function (xresizewindow (:return-type int) (:name "_XResizeWindow")) 
   (dpy (:pointer display))
   (w window)
   (width unsigned-int)
   (height unsigned-int))

(def-exported-foreign-function (xcheckifevent (:return-type int) (:name "_XCheckIfEvent")) 
   (dpy (:pointer display))
   (event (:pointer xevent))
   (predicate (:pointer :pointer))
   (arg (:pointer char)))

(def-exported-foreign-function (xcheckmaskevent (:return-type int) (:name "_XCheckMaskEvent")) 
   (dpy (:pointer display))
   (mask long)
   (event (:pointer xevent)))

(def-exported-foreign-function (xchecktypedevent (:return-type int) (:name "_XCheckTypedEvent")) 
   (dpy (:pointer display))
   (type int)
   (event (:pointer xevent)))

(def-exported-foreign-function (xchecktypedwindowevent (:return-type int) (:name "_XCheckTypedWindowEvent")) 
   (dpy (:pointer display))
   (w window)
   (type int)
   (event (:pointer xevent)))

(def-exported-foreign-function (xcheckwindowevent (:return-type int) (:name "_XCheckWindowEvent")) 
   (dpy (:pointer display))
   (w window)
   (mask long)
   (event (:pointer xevent)))

(def-exported-foreign-function (xcirculatesubwindows (:return-type int) (:name "_XCirculateSubwindows")) 
   (dpy (:pointer display))
   (w window)
   (direction int))

(def-exported-foreign-function (xcirculatesubwindowsdown (:return-type int) (:name "_XCirculateSubwindowsDown")) 
   (dpy (:pointer display))
   (w window))

(def-exported-foreign-function (xcirculatesubwindowsup (:return-type int) (:name "_XCirculateSubwindowsUp")) 
   (dpy (:pointer display))
   (w window))

(def-exported-foreign-function (xclosedisplay (:return-type int) (:name "_XCloseDisplay")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xclearwindow (:return-type int) (:name "_XClearWindow")) 
   (dpy (:pointer display))
   (w window))

(def-exported-foreign-function (xcleararea (:return-type int) (:name "_XClearArea")) 
   (dpy (:pointer display))
   (w window)
   (x int)
   (y int)
   (width unsigned-int)
   (height unsigned-int)
   (exposures int))

(def-exported-foreign-function (xmoveresizewindow (:return-type int) (:name "_XMoveResizeWindow")) 
   (dpy (:pointer display))
   (w window)
   (x int)
   (y int)
   (width unsigned-int)
   (height unsigned-int))

(def-exported-foreign-function (xconvertselection (:return-type int) (:name "_XConvertSelection")) 
   (dpy (:pointer display))
   (selection :unsigned-32bit)
   (target :unsigned-32bit)
   (property :unsigned-32bit)
   (requestor window)
   (time :unsigned-32bit))

(def-exported-foreign-function (xcopyarea (:return-type int) (:name "_XCopyArea")) 
   (dpy (:pointer display))
   (src-drawable drawable)
   (dst-drawable drawable)
   (gc gc)
   (src-x int)
   (src-y int)
   (width unsigned-int)
   (height unsigned-int)
   (dst-x int)
   (dst-y int))

(def-exported-foreign-function (xcopycolormapandfree (:return-type colormap) (:name "_XCopyColormapAndFree")) 
   (dpy (:pointer display))
   (src-cmap colormap))

(def-exported-foreign-function (xcopygc (:return-type int) (:name "_XCopyGC")) 
   (dpy (:pointer display))
   (mask unsigned-long)
   (srcgc gc)
   (destgc gc))

(def-exported-foreign-function (xcopyplane (:return-type int) (:name "_XCopyPlane")) 
   (dpy (:pointer display))
   (src-drawable drawable)
   (dst-drawable drawable)
   (gc gc)
   (src-x int)
   (src-y int)
   (width unsigned-int)
   (height unsigned-int)
   (dst-x int)
   (dst-y int)
   (bit-plane unsigned-long))

(def-exported-foreign-function (xcreatecolormap (:return-type colormap) (:name "_XCreateColormap")) 
   (dpy (:pointer display))
   (w window)
   (visual (:pointer visual))
   (alloc int))

(def-exported-foreign-function (xcreatepixmapcursor (:return-type cursor) (:name "_XCreatePixmapCursor")) 
   (dpy (:pointer display))
   (source pixmap)
   (mask pixmap)
   (foreground (:pointer xcolor))
   (background (:pointer xcolor))
   (x unsigned-int)
   (y unsigned-int))

(def-exported-foreign-function (xcreategc (:return-type gc) (:name "_XCreateGC")) 
   (dpy (:pointer display))
   (d drawable)
   (valuemask unsigned-long)
   (values (:pointer xgcvalues)))

(def-exported-foreign-function (xgcontextfromgc (:return-type gcontext) (:name "_XGContextFromGC")) 
   (gc gc))

(def-exported-foreign-function (xcreateglyphcursor (:return-type cursor) (:name "_XCreateGlyphCursor")) 
   (dpy (:pointer display))
   (source-font font)
   (mask-font font)
   (source-char unsigned-int)
   (mask-char unsigned-int)
   (foreground (:pointer xcolor))
   (background (:pointer xcolor)))

(def-exported-foreign-function (xcreatepixmap (:return-type pixmap) (:name "_XCreatePixmap")) 
   (dpy (:pointer display))
   (d drawable)
   (width unsigned-int)
   (height unsigned-int)
   (depth unsigned-int))

(def-exported-foreign-function (xcreatesimplewindow (:return-type window) (:name "_XCreateSimpleWindow")) 
   (dpy (:pointer display))
   (parent window)
   (x int)
   (y int)
   (width unsigned-int)
   (height unsigned-int)
   (borderwidth unsigned-int)
   (border unsigned-long)
   (background unsigned-long))

(def-exported-foreign-function (xcreatefontcursor (:return-type cursor) (:name "_XCreateFontCursor")) 
   (dpy (:pointer display))
   (which unsigned-int))

(def-exported-foreign-function (xdefinecursor (:return-type int) (:name "_XDefineCursor")) 
   (dpy (:pointer display))
   (w window)
   (cursor cursor))

(def-exported-foreign-function (xdeleteproperty (:return-type int) (:name "_XDeleteProperty")) 
   (dpy (:pointer display))
   (window window)
   (property :unsigned-32bit))

(def-exported-foreign-function (xdestroysubwindows (:return-type int) (:name "_XDestroySubwindows")) 
   (dpy (:pointer display))
   (win window))

(def-exported-foreign-function (xdestroywindow (:return-type int) (:name "_XDestroyWindow")) 
   (dpy (:pointer display))
   (w window))

(def-exported-foreign-function (xdisplayname (:return-type (:pointer char)) (:name "_XDisplayName")) 
   (display (:pointer char)))


(def-exported-foreign-function (xgeterrortext (:return-type :null) (:name "_XGetErrorText")) 
   (dpy (:pointer display))
   (code int)
   (buffer (:pointer char))
   (nbytes int))  

(def-exported-foreign-function (xgeterrordatabasetext (:return-type int) (:name "_XGetErrorDatabaseText")) 
   (name (:pointer char))
   (type (:pointer char))
   (defaultp (:pointer char))
   (dpy (:pointer display))
   (buffer (:pointer char))
   (nbytes int))

(def-exported-foreign-function (xseterrorhandler (:return-type :null) (:name "_XSetErrorHandler")) 
   (handler callback-function-addr))

(def-exported-foreign-function (xsetioerrorhandler (:return-type :null) (:name "_XSetIOErrorHandler")) 
   (handler callback-function-addr))

(def-exported-foreign-function (xactivatescreensaver (:return-type int) (:name "_XActivateScreenSaver")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xresetscreensaver (:return-type int) (:name "_XResetScreenSaver")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xforcescreensaver (:return-type int) (:name "_XForceScreenSaver")) 
   (dpy (:pointer display))
   (mode int))

(def-exported-foreign-function (xfetchname (:return-type int) (:name "_XFetchName")) 
   (dpy (:pointer display))
   (w window)
   (name (:pointer (:pointer char))))

(def-exported-foreign-function (xgeticonname (:return-type int) (:name "_XGetIconName")) 
   (dpy (:pointer display))
   (w window)
   (icon-name (:pointer (:pointer char))))

(def-exported-foreign-function (xfillarc (:return-type int) (:name "_XFillArc")) 
   (dpy (:pointer display))
   (d drawable)
   (gc gc)
   (x int)
   (y int)
   (width unsigned-int)
   (height unsigned-int)
   (angle1 int)
   (angle2 int))

(def-exported-foreign-function (xfillarcs (:return-type int) (:name "_XFillArcs")) 
   (dpy (:pointer display))
   (d drawable)
   (gc gc)
   (arcs (:pointer xarc))
   (n-arcs int))

(def-exported-foreign-function (xfillpolygon (:return-type int) (:name "_XFillPolygon")) 
   (dpy (:pointer display))
   (d drawable)
   (gc gc)
   (points (:pointer xpoint))
   (n-points int)
   (shape int)
   (mode int))

(def-exported-foreign-function (xfillrectangle (:return-type int) (:name "_XFillRectangle")) 
   (dpy (:pointer display))
   (d drawable)
   (gc gc)
   (x int)
   (y int)
   (width unsigned-int)
   (height unsigned-int))

(def-exported-foreign-function (xfillrectangles (:return-type int) (:name "_XFillRectangles")) 
   (dpy (:pointer display))
   (d drawable)
   (gc gc)
   (rectangles (:pointer xrectangle))
   (n-rects int))

(def-exported-foreign-function (xflush (:name "_XFlush") (:return-type :fixnum))
   (dpy (:pointer display)))

(def-exported-foreign-function (xloadqueryfont (:return-type (:pointer xfontstruct)) (:name "_XLoadQueryFont")) 
   (dpy (:pointer display))
   (name (:pointer char)))

(def-exported-foreign-function (xfreefont (:return-type int) (:name "_XFreeFont")) 
   (dpy (:pointer display))
   (fs (:pointer xfontstruct)))

(def-exported-foreign-function (xqueryfont (:return-type (:pointer xfontstruct)) (:name "_XQueryFont")) 
   (dpy (:pointer display))
   (fid font))

(def-exported-foreign-function (xlistfontswithinfo (:return-type (:pointer (:pointer char))) (:name "_XListFontsWithInfo")) 
   (dpy (:pointer display))
   (pattern (:pointer char))
   (maxnames int)
   (actualcount (:pointer int))
   (info (:pointer (:pointer xfontstruct))))

(def-exported-foreign-function (xfreefontinfo (:return-type int) (:name "_XFreeFontInfo")) 
   (names (:pointer (:pointer char)))
   (info (:pointer xfontstruct))
   (actualcount int))

(def-exported-foreign-function (xlistfonts (:return-type (:pointer (:pointer char))) (:name "_XListFonts")) 
   (dpy (:pointer display))
   (pattern (:pointer char))
   (maxnames int)
   (actualcount (:pointer int)))

(def-exported-foreign-function (xfreefontnames (:return-type int) (:name "_XFreeFontNames")) 
   (list (:pointer (:pointer char))))

(def-exported-foreign-function (xfreecolormap (:return-type int) (:name "_XFreeColormap")) 
   (dpy (:pointer display))
   (cmap colormap))

(def-exported-foreign-function (xfreecolors (:return-type int) (:name "_XFreeColors")) 
   (dpy (:pointer display))
   (cmap colormap)
   (pixels (:pointer unsigned-long))
   (npixels int)
   (planes unsigned-long))

(def-exported-foreign-function (xfreecursor (:return-type int) (:name "_XFreeCursor")) 
   (dpy (:pointer display))
   (cursor cursor))

(def-exported-foreign-function (xfreegc (:return-type int) (:name "_XFreeGC")) 
   (dpy (:pointer display))
   (gc gc))

(def-exported-foreign-function (xfreepixmap (:return-type int) (:name "_XFreePixmap")) 
   (dpy (:pointer display))
   (pixmap pixmap))

(def-exported-foreign-function (xsetarcmode (:return-type int) (:name "_XSetArcMode")) 
   (dpy (:pointer display))
   (gc gc)
   (arc-mode int))

(def-exported-foreign-function (xsetfillrule (:return-type int) (:name "_XSetFillRule")) 
   (dpy (:pointer display))
   (gc gc)
   (fill-rule int))

(def-exported-foreign-function (xsetfillstyle (:return-type int) (:name "_XSetFillStyle")) 
   (dpy (:pointer display))
   (gc gc)
   (fill-style int))

(def-exported-foreign-function (xsetgraphicsexposures (:return-type int) (:name "_XSetGraphicsExposures")) 
   (dpy (:pointer display))
   (gc gc)
   (graphics-exposures int))

(def-exported-foreign-function (xsetsubwindowmode (:return-type int) (:name "_XSetSubwindowMode")) 
   (dpy (:pointer display))
   (gc gc)
   (subwindow-mode int))

(def-exported-foreign-function (xgetatomname (:return-type (:pointer char)) (:name "_XGetAtomName")) 
   (dpy (:pointer display))
   (atom :unsigned-32bit))

(def-exported-foreign-function (xallocnamedcolor (:return-type int) (:name "_XAllocNamedColor")) 
   (dpy (:pointer display))
   (cmap colormap)
   (colorname (:pointer char))
   (hard-def (:pointer xcolor))
   (exact-def (:pointer xcolor)))

(def-exported-foreign-function (xgetfontpath (:return-type (:pointer (:pointer char))) (:name "_XGetFontPath")) 
   (dpy (:pointer display))
   (npaths (:pointer int)))

(def-exported-foreign-function (xfreefontpath (:return-type int) (:name "_XFreeFontPath")) 
   (list (:pointer (:pointer char))))

(def-exported-foreign-function (xgetfontproperty (:return-type int) (:name "_XGetFontProperty")) 
   (fs (:pointer xfontstruct))
   (name :unsigned-32bit)
   (valueptr (:pointer unsigned-long)))

(def-exported-foreign-function (xgetgeometry (:return-type int) (:name "_XGetGeometry")) 
   (dpy (:pointer display))
   (d drawable)
   (root (:pointer window))
   (x (:pointer int))
   (y (:pointer int))
   (width (:pointer unsigned-int))
   (height (:pointer unsigned-int))
   (borderwidth (:pointer unsigned-int))
   (depth (:pointer unsigned-int)))

(def-exported-foreign-function (xalloccolor (:return-type int) (:name "_XAllocColor")) 
   (dpy (:pointer display))
   (cmap colormap)
   (def (:pointer xcolor)))

(def-exported-foreign-function (xgetinputfocus (:return-type int) (:name "_XGetInputFocus")) 
   (dpy (:pointer display))
   (focus (:pointer window))
   (revert-to (:pointer int)))

(def-exported-foreign-function (xgetimage (:return-type (:pointer ximage)) (:name "_XGetImage")) 
   (dpy (:pointer display))
   (d drawable)
   (x int)
   (y int)
   (width unsigned-int)
   (height unsigned-int)
   (plane-mask unsigned-long)
   (format int))

(def-exported-foreign-function (xgetsubimage (:return-type (:pointer ximage)) (:name "_XGetSubImage")) 
   (dpy (:pointer display))
   (d drawable)
   (x int)
   (y int)
   (width unsigned-int)
   (height unsigned-int)
   (plane-mask unsigned-long)
   (format int)
   (dest-image (:pointer ximage))
   (dest-x int)
   (dest-y int))

(def-exported-foreign-function (xgetkeyboardcontrol (:return-type void) (:name "_XGetKeyboardControl")) 
   (dpy (:pointer display))
   (state (:pointer xkeyboardstate)))

(def-exported-foreign-function (xgetmotionevents (:return-type (:pointer xtimecoord)) (:name "_XGetMotionEvents")) 
   (dpy (:pointer display))
   (start :unsigned-32bit)
   (stop :unsigned-32bit)
   (w window)
   (nevents (:pointer int)))

(def-exported-foreign-function (xgetpointercontrol (:return-type int) (:name "_XGetPointerControl")) 
   (dpy (:pointer display))
   (accel-numer (:pointer int))
   (accel-denom (:pointer int))
   (threshold (:pointer int)))

(def-exported-foreign-function (xgetpointermapping (:return-type int) (:name "_XGetPointerMapping")) 
   (dpy (:pointer display))
   (map (:pointer unsigned-char))
   (nmaps int))

(def-exported-foreign-function (xgetkeyboardmapping (:return-type (:pointer keysym)) (:name "_XGetKeyboardMapping")) 
   (dpy (:pointer display))
   (first-keycode :character) ;; (first-keycode keycode)
   (count int)
   (keysyms-per-keycode (:pointer int)))

(def-exported-foreign-function (xgetwindowproperty (:return-type int) (:name "_XGetWindowProperty")) 
   (dpy (:pointer display))
   (w window)
   (property :unsigned-32bit)
   (offset long)
   (length long)
   (delete int)
   (req-type :unsigned-32bit)
   (actual-type (:pointer atom))
   (actual-format (:pointer int))
   (nitems (:pointer unsigned-long))
   (bytesafter (:pointer unsigned-long))
   (prop (:pointer (:pointer unsigned-char))))

(def-exported-foreign-function (xgetselectionowner (:return-type window) (:name "_XGetSelectionOwner")) 
   (dpy (:pointer display))
   (selection :unsigned-32bit))

(def-exported-foreign-function (xgetscreensaver (:return-type int) (:name "_XGetScreenSaver")) 
   (dpy (:pointer display))
   (timeout (:pointer int))
   (interval (:pointer int))
   (prefer-blanking (:pointer int))
   (allow-exp (:pointer int)))

(def-exported-foreign-function (xgetwindowattributes (:return-type int) (:name "_XGetWindowAttributes")) 
   (dpy (:pointer display))
   (w window)
   (att (:pointer xwindowattributes)))

(def-exported-foreign-function (xgrabbutton (:return-type int) (:name "_XGrabButton")) 
   (dpy (:pointer display))
   (modifiers unsigned-int)
   (button unsigned-int)
   (grab-window window)
   (owner-events int)
   (event-mask unsigned-int)
   (pointer-mode int)
   (keyboard-mode int)
   (confine-to window)
   (curs cursor))

(def-exported-foreign-function (xgrabkey (:return-type int) (:name "_XGrabKey")) 
   (dpy (:pointer display))
   (key int)
   (modifiers unsigned-int)
   (grab-window window)
   (owner-events int)
   (pointer-mode int)
   (keyboard-mode int))

(def-exported-foreign-function (xgrabkeyboard (:return-type int) (:name "_XGrabKeyboard")) 
   (dpy (:pointer display))
   (window window)
   (ownerevents int)
   (pointermode int)
   (keyboardmode int)
   (time :unsigned-32bit))

(def-exported-foreign-function (xgrabpointer (:return-type int) (:name "_XGrabPointer")) 
   (dpy (:pointer display))
   (grab-window window)
   (owner-events int)
   (event-mask unsigned-int)
   (pointer-mode int)
   (keyboard-mode int)
   (confine-to window)
   (curs cursor)
   (time :unsigned-32bit))

(def-exported-foreign-function (xgrabserver (:return-type int) (:name "_XGrabServer")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xaddhost (:return-type int) (:name "_XAddHost")) 
   (dpy (:pointer display))
   (host (:pointer xhostaddress)))

(def-exported-foreign-function (xremovehost (:return-type int) (:name "_XRemoveHost")) 
   (dpy (:pointer display))
   (host (:pointer xhostaddress)))

(def-exported-foreign-function (xaddhosts (:return-type int) (:name "_XAddHosts")) 
   (dpy (:pointer display))
   (hosts (:pointer xhostaddress))
   (n int))

(def-exported-foreign-function (xremovehosts (:return-type int) (:name "_XRemoveHosts")) 
   (dpy (:pointer display))
   (hosts (:pointer xhostaddress))
   (n int))

(def-exported-foreign-function (xifevent (:return-type int) (:name "_XIfEvent")) 
   (dpy (:pointer display))
   (event (:pointer xevent))
   (predicate (:pointer :pointer))
   (arg (:pointer char)))

(def-exported-foreign-function (xinitextension (:return-type (:pointer xextcodes)) (:name "_XInitExtension")) 
   (dpy (:pointer display))
   (name (:pointer char)))

(def-exported-foreign-function (xaddextension (:return-type (:pointer xextcodes)) (:name "_XAddExtension")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xeheadofextensionlist (:return-type (:pointer (:pointer xextdata))) (:name "_XEHeadOfExtensionList")) 
   (object xedataobject))

(def-exported-foreign-function (xfindonextensionlist (:return-type (:pointer xextdata)) (:name "_XFindOnExtensionList")) 
   (structure (:pointer (:pointer xextdata)))
   (number int))

(def-exported-foreign-function (xesetcreategc (:return-type (:pointer :pointer)) (:name "_XESetCreateGC")) 
   (dpy (:pointer display))
   (extension int)
   (proc (:pointer :pointer)))

(def-exported-foreign-function (xesetcopygc (:return-type (:pointer :pointer)) (:name "_XESetCopyGC")) 
   (dpy (:pointer display))
   (extension int)
   (proc (:pointer :pointer)))

(def-exported-foreign-function (xesetflushgc (:return-type (:pointer :pointer)) (:name "_XESetFlushGC")) 
   (dpy (:pointer display))
   (extension int)
   (proc (:pointer :pointer)))

(def-exported-foreign-function (xesetfreegc (:return-type (:pointer :pointer)) (:name "_XESetFreeGC")) 
   (dpy (:pointer display))
   (extension int)
   (proc (:pointer :pointer)))

(def-exported-foreign-function (xesetcreatefont (:return-type (:pointer :pointer)) (:name "_XESetCreateFont")) 
   (dpy (:pointer display))
   (extension int)
   (proc (:pointer :pointer)))

(def-exported-foreign-function (xesetfreefont (:return-type (:pointer :pointer)) (:name "_XESetFreeFont")) 
   (dpy (:pointer display))
   (extension int)
   (proc (:pointer :pointer)))

(def-exported-foreign-function (xesetclosedisplay (:return-type (:pointer :pointer)) (:name "_XESetCloseDisplay")) 
   (dpy (:pointer display))
   (extension int)
   (proc (:pointer :pointer)))

(def-exported-foreign-function (xesetwiretoevent (:return-type (:pointer :pointer)) (:name "_XESetWireToEvent")) 
   (dpy (:pointer display))
   (proc (:pointer :pointer))
   (event-number int))

(def-exported-foreign-function (xeseteventtowire (:return-type (:pointer :pointer)) (:name "_XESetEventToWire")) 
   (dpy (:pointer display))
   (proc (:pointer :pointer))
   (event-number int))

(def-exported-foreign-function (xeseterror (:return-type (:pointer :pointer)) (:name "_XESetError")) 
   (dpy (:pointer display))
   (extension int)
   (proc (:pointer :pointer)))

(def-exported-foreign-function (xeseterrorstring (:return-type (:pointer :pointer)) (:name "_XESetErrorString")) 
   (dpy (:pointer display))
   (extension int)
   (proc (:pointer :pointer)))

(def-exported-foreign-function (xinstallcolormap (:return-type int) (:name "_XInstallColormap")) 
   (dpy (:pointer display))
   (cmap colormap))

(def-exported-foreign-function (xinternatom (:return-type :unsigned-32bit) (:name "_XInternAtom")) 
   (dpy (:pointer display))
   (name (:pointer char))
   (onlyifexists int))

(def-exported-foreign-function (xrefreshkeyboardmapping (:return-type int) (:name "_XRefreshKeyboardMapping")) 
   (event (:pointer xmappingevent)))

#+ignore
(def-exported-foreign-function (xusekeymap (:return-type int) (:name "_XUseKeymap")) 
   (filename (:pointer char)))

(def-exported-foreign-function (xrebindkeysym (:return-type int) (:name "_XRebindKeysym")) 
   (dpy (:pointer display))
   (keysym keysym)
   (mlist (:pointer keysym))
   (nm int)
   (str (:pointer unsigned-char))
   (nbytes int))

(def-exported-foreign-function (xkillclient (:return-type int) (:name "_XKillClient")) 
   (dpy (:pointer display))
   (resource xid))

(def-exported-foreign-function (xlisthosts (:return-type (:pointer xhostaddress)) (:name "_XListHosts")) 
   (dpy (:pointer display))
   (nhosts (:pointer int))
   (enabled (:pointer int)))

(def-exported-foreign-function (xlistinstalledcolormaps (:return-type (:pointer colormap)) (:name "_XListInstalledColormaps")) 
   (dpy (:pointer display))
   (win window)
   (n (:pointer int)))

(def-exported-foreign-function (xlistproperties (:return-type (:pointer atom)) (:name "_XListProperties")) 
   (dpy (:pointer display))
   (window window)
   (n-props (:pointer int)))

(def-exported-foreign-function (xlistextensions (:return-type (:pointer (:pointer char))) (:name "_XListExtensions")) 
   (dpy (:pointer display))
   (nextensions (:pointer int)))

(def-exported-foreign-function (xfreeextensionlist (:return-type int) (:name "_XFreeExtensionList")) 
   (list (:pointer (:pointer char))))

(def-exported-foreign-function (xloadfont (:return-type font) (:name "_XLoadFont")) 
   (dpy (:pointer display))
   (name (:pointer char)))

(def-exported-foreign-function (xlookupcolor (:return-type int) (:name "_XLookupColor")) 
   (dpy (:pointer display))
   (cmap colormap)
   (spec (:pointer char))
   (def (:pointer xcolor))
   (scr (:pointer xcolor)))

(def-exported-foreign-function (xlowerwindow (:return-type int) (:name "_XLowerWindow")) 
   (dpy (:pointer display))
   (w window))

(def-exported-foreign-function (xconnectionnumber (:return-type int) (:name "_XConnectionNumber")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xrootwindow (:return-type window) (:name "_XRootWindow")) 
   (dpy (:pointer display))
   (scr int))

(def-exported-foreign-function (xdefaultscreen (:return-type int) (:name "_XDefaultScreen")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xdefaultrootwindow (:return-type window) (:name "_XDefaultRootWindow")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xdefaultvisual (:return-type (:pointer visual)) (:name "_XDefaultVisual")) 
   (dpy (:pointer display))
   (scr int))

(def-exported-foreign-function (xdefaultgc (:return-type gc) (:name "_XDefaultGC")) 
   (dpy (:pointer display))
   (scr int))

(def-exported-foreign-function (xblackpixel (:return-type unsigned-long) (:name "_XBlackPixel")) 
   (dpy (:pointer display))
   (scr int))

(def-exported-foreign-function (xwhitepixel (:return-type unsigned-long) (:name "_XWhitePixel")) 
   (dpy (:pointer display))
   (scr int))

(def-exported-foreign-function (xallplanes (:return-type unsigned-long) (:name "_XAllPlanes")) )

(def-exported-foreign-function (xqlength (:return-type int) (:name "_XQLength")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xdisplaywidth (:return-type int) (:name "_XDisplayWidth")) 
   (dpy (:pointer display))
   (scr int))

(def-exported-foreign-function (xdisplayheight (:return-type int) (:name "_XDisplayHeight")) 
   (dpy (:pointer display))
   (scr int))

(def-exported-foreign-function (xdisplaywidthmm (:return-type int) (:name "_XDisplayWidthMM")) 
   (dpy (:pointer display))
   (scr int))

(def-exported-foreign-function (xdisplayheightmm (:return-type int) (:name "_XDisplayHeightMM")) 
   (dpy (:pointer display))
   (scr int))

(def-exported-foreign-function (xdisplayplanes (:return-type int) (:name "_XDisplayPlanes")) 
   (dpy (:pointer display))
   (scr int))

(def-exported-foreign-function (xdisplaycells (:return-type int) (:name "_XDisplayCells")) 
   (dpy (:pointer display))
   (scr int))

(def-exported-foreign-function (xscreencount (:return-type int) (:name "_XScreenCount")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xservervendor (:return-type (:pointer char)) (:name "_XServerVendor")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xprotocolversion (:return-type int) (:name "_XProtocolVersion")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xprotocolrevision (:return-type int) (:name "_XProtocolRevision")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xvendorrelease (:return-type int) (:name "_XVendorRelease")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xdisplaystring (:return-type (:pointer char)) (:name "_XDisplayString")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xdefaultdepth (:return-type int) (:name "_XDefaultDepth")) 
   (dpy (:pointer display))
   (scr int))

(def-exported-foreign-function (xdefaultcolormap (:return-type colormap) (:name "_XDefaultColormap")) 
   (dp (:pointer display))
   (scr int))

(def-exported-foreign-function (xbitmapunit (:return-type int) (:name "_XBitmapUnit")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xbitmapbitorder (:return-type int) (:name "_XBitmapBitOrder")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xbitmappad (:return-type int) (:name "_XBitmapPad")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (ximagebyteorder (:return-type int) (:name "_XImageByteOrder")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xnextrequest (:return-type unsigned-long) (:name "_XNextRequest")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xlastknownrequestprocessed (:return-type unsigned-long) (:name "_XLastKnownRequestProcessed")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xscreenofdisplay (:return-type (:pointer screen)) (:name "_XScreenOfDisplay")) 
   (dpy (:pointer display))
   (scr int))

(def-exported-foreign-function (xdefaultscreenofdisplay (:return-type (:pointer screen)) (:name "_XDefaultScreenOfDisplay")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xdisplayofscreen (:return-type (:pointer display)) (:name "_XDisplayOfScreen")) 
   (s (:pointer screen)))

(def-exported-foreign-function (xrootwindowofscreen (:return-type window) (:name "_XRootWindowOfScreen")) 
   (s (:pointer screen)))

(def-exported-foreign-function (xblackpixelofscreen (:return-type unsigned-long) (:name "_XBlackPixelOfScreen")) 
   (s (:pointer screen)))

(def-exported-foreign-function (xwhitepixelofscreen (:return-type unsigned-long) (:name "_XWhitePixelOfScreen")) 
   (s (:pointer screen)))

(def-exported-foreign-function (xdefaultcolormapofscreen (:return-type colormap) (:name "_XDefaultColormapOfScreen")) 
   (s (:pointer screen)))

(def-exported-foreign-function (xdefaultdepthofscreen (:return-type int) (:name "_XDefaultDepthOfScreen")) 
   (s (:pointer screen)))

(def-exported-foreign-function (xdefaultgcofscreen (:return-type gc) (:name "_XDefaultGCOfScreen")) 
   (s (:pointer screen)))

(def-exported-foreign-function (xdefaultvisualofscreen (:return-type (:pointer visual)) (:name "_XDefaultVisualOfScreen")) 
   (s (:pointer screen)))

(def-exported-foreign-function (xwidthofscreen (:return-type int) (:name "_XWidthOfScreen")) 
   (s (:pointer screen)))

(def-exported-foreign-function (xheightofscreen (:return-type int) (:name "_XHeightOfScreen")) 
   (s (:pointer screen)))

(def-exported-foreign-function (xwidthmmofscreen (:return-type int) (:name "_XWidthMMOfScreen")) 
   (s (:pointer screen)))

(def-exported-foreign-function (xheightmmofscreen (:return-type int) (:name "_XHeightMMOfScreen")) 
   (s (:pointer screen)))

(def-exported-foreign-function (xplanesofscreen (:return-type int) (:name "_XPlanesOfScreen")) 
   (s (:pointer screen)))

(def-exported-foreign-function (xcellsofscreen (:return-type int) (:name "_XCellsOfScreen")) 
   (s (:pointer screen)))

(def-exported-foreign-function (xmincmapsofscreen (:return-type int) (:name "_XMinCmapsOfScreen")) 
   (s (:pointer screen)))

(def-exported-foreign-function (xmaxcmapsofscreen (:return-type int) (:name "_XMaxCmapsOfScreen")) 
   (s (:pointer screen)))

(def-exported-foreign-function (xdoessaveunders (:return-type int) (:name "_XDoesSaveUnders")) 
   (s (:pointer screen)))

(def-exported-foreign-function (xdoesbackingstore (:return-type int) (:name "_XDoesBackingStore")) 
   (s (:pointer screen)))

(def-exported-foreign-function (xeventmaskofscreen (:return-type long) (:name "_XEventMaskOfScreen")) 
   (s (:pointer screen)))

(def-exported-foreign-function (xnoop (:return-type int) (:name "_XNoOp")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (XParseColor (:return-type int) 
					    (:name "_XParseColor"))
  (display (:pointer Display))
  (colormap Colormap)
  (spec (:pointer char))
  (exact-def-return (:pointer XColor)))

(def-exported-foreign-function (XParseGeometry (:return-type int)
					       (:name "_XParseGeometry"))
  (parsestring (:pointer char))
  (x-return (:pointer int))
  (y-return (:pointer int))
  (width-return (:pointer unsigned-int))
  (height-return (:pointer unsigned-int)))

(def-exported-foreign-function (xmapraised (:return-type int) (:name "_XMapRaised")) 
   (dpy (:pointer display))
   (w window))

(def-exported-foreign-function (xmapsubwindows (:return-type int) (:name "_XMapSubwindows")) 
   (dpy (:pointer display))
   (win window))

(def-exported-foreign-function (xmapwindow (:return-type int) (:name "_XMapWindow")) 
   (dpy (:pointer display))
   (win window))

(def-exported-foreign-function (xmaskevent (:return-type int) (:name "_XMaskEvent")) 
   (dpy (:pointer display))
   (mask long)
   (event (:pointer xevent)))

(def-exported-foreign-function (xmaxrequestsize (:return-type long) (:name "_XMaxRequestSize")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xresourcemanagerstring (:return-type (:pointer char)) (:name "_XResourceManagerString")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xdisplaymotionbuffersize (:return-type unsigned-long) (:name "_XDisplayMotionBufferSize")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xdisplaykeycodes (:return-type int) (:name "_XDisplayKeycodes")) 
   (dpy (:pointer display))
   (min-keycode-return (:pointer int))
   (max-keycode-return (:pointer int)))

(def-exported-foreign-function (xvisualidfromvisual (:return-type visualid) (:name "_XVisualIDFromVisual")) 
   (visual (:pointer visual)))

(def-exported-foreign-function (xgetmodifiermapping (:return-type (:pointer xmodifierkeymap)) (:name "_XGetModifierMapping")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xsetmodifiermapping (:return-type int) (:name "_XSetModifierMapping")) 
   (dpy (:pointer display))
   (modifier-map (:pointer xmodifierkeymap)))

(def-exported-foreign-function (xnewmodifiermap (:return-type (:pointer xmodifierkeymap)) (:name "_XNewModifiermap")) 
   (keyspermodifier int))

(def-exported-foreign-function (xfreemodifiermap (:return-type void) (:name "_XFreeModifiermap")) 
   (map (:pointer xmodifierkeymap)))

(def-exported-foreign-function (xinsertmodifiermapentry (:return-type (:pointer xmodifierkeymap)) (:name "_XInsertModifiermapEntry")) 
   (map (:pointer xmodifierkeymap))
   (keysym :character) ;; (keysym keycode)
   (modifier int))

(def-exported-foreign-function (xdeletemodifiermapentry (:return-type (:pointer xmodifierkeymap)) (:name "_XDeleteModifiermapEntry")) 
   (map (:pointer xmodifierkeymap))
   (keysym :character) ;; (keysym keycode)
   (modifier int))

(def-exported-foreign-function (xmovewindow (:return-type int) (:name "_XMoveWindow")) 
   (dpy (:pointer display))
   (w window)
   (x int)
   (y int))

(def-exported-foreign-function (xnextevent (:return-type int) (:name "_XNextEvent")) 
   (dpy (:pointer display))
   (event (:pointer xevent)))

(def-exported-foreign-function (xopendisplay (:return-type (:pointer display)) (:name "_XOpenDisplay")) 
   (display (:pointer char)))

(def-exported-foreign-function (xpeekevent (:return-type int) (:name "_XPeekEvent")) 
   (dpy (:pointer display))
   (event (:pointer xevent)))

(def-exported-foreign-function (xpeekifevent (:return-type int) (:name "_XPeekIfEvent")) 
   (dpy (:pointer display))
   (event (:pointer xevent))
   (predicate (:pointer :pointer))
   (arg (:pointer char)))

(def-exported-foreign-function (xeventsqueued (:return-type int) (:name "_XEventsQueued")) 
   (dpy (:pointer display))
   (mode int))

(def-exported-foreign-function (xpending (:return-type fixnum-int) (:name "_XPending")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xsetwindowbackgroundpixmap (:return-type int) (:name "_XSetWindowBackgroundPixmap")) 
   (dpy (:pointer display))
   (w window)
   (pixmap pixmap))

(def-exported-foreign-function (xsetwindowborderpixmap (:return-type int) (:name "_XSetWindowBorderPixmap")) 
   (dpy (:pointer display))
   (w window)
   (pixmap pixmap))

(def-exported-foreign-function (xputbackevent (:return-type int) (:name "_XPutBackEvent")) 
   (dpy (:pointer display))
   (event (:pointer xevent)))

(def-exported-foreign-function (xputimage (:return-type int) (:name "_XPutImage")) 
   (dpy (:pointer display))
   (d drawable)
   (gc gc)
   (image (:pointer ximage))
   (x int)
   (y int)
   (req-width unsigned-int)
   (req-height unsigned-int)
   (req-xoffset int)
   (req-yoffset int))

(def-exported-foreign-function (xquerybestsize (:return-type int) (:name "_XQueryBestSize")) 
   (dpy (:pointer display))
   (class int)
   (drawable drawable)
   (width unsigned-int)
   (height unsigned-int)
   (ret-width (:pointer unsigned-int))
   (ret-height (:pointer unsigned-int)))

(def-exported-foreign-function (xquerycolor (:return-type int) (:name "_XQueryColor")) 
   (dpy (:pointer display))
   (cmap colormap)
   (def (:pointer xcolor)))

(def-exported-foreign-function (xquerycolors (:return-type int) (:name "_XQueryColors")) 
   (dpy (:pointer display))
   (cmap colormap)
   (defs (:pointer xcolor))
   (ncolors int))

(def-exported-foreign-function (xquerybestcursor (:return-type int) (:name "_XQueryBestCursor")) 
   (dpy (:pointer display))
   (drawable drawable)
   (width unsigned-int)
   (height unsigned-int)
   (ret-width (:pointer unsigned-int))
   (ret-height (:pointer unsigned-int)))

(def-exported-foreign-function (xqueryextension (:return-type int) (:name "_XQueryExtension")) 
   (dpy (:pointer display))
   (name (:pointer char))
   (major-opcode (:pointer int))
   (first-event (:pointer int))
   (first-error (:pointer int)))

(def-exported-foreign-function (xquerykeymap (:return-type int) (:name "_XQueryKeymap")) 
   (dpy (:pointer display))
   (keys (:array char (32))))

(def-exported-foreign-function (xquerypointer (:return-type int) (:name "_XQueryPointer")) 
   (dpy (:pointer display))
   (w window)
   (root (:pointer window))
   (child (:pointer window))
   (root-x (:pointer int))
   (root-y (:pointer int))
   (win-x (:pointer int))
   (win-y (:pointer int))
   (mask (:pointer unsigned-int)))

(def-exported-foreign-function (xquerybeststipple (:return-type int) (:name "_XQueryBestStipple")) 
   (dpy (:pointer display))
   (drawable drawable)
   (width unsigned-int)
   (height unsigned-int)
   (ret-width (:pointer unsigned-int))
   (ret-height (:pointer unsigned-int)))

(def-exported-foreign-function (xquerytextextents16 (:return-type int) (:name "_XQueryTextExtents16")) 
   (dpy (:pointer display))
   (fid font)
   (string (:pointer xchar2b))
   (nchars int)
   (dir (:pointer int))
   (font-ascent (:pointer int))
   (font-descent (:pointer int))
   (overall (:pointer xcharstruct)))

(def-exported-foreign-function (xquerytextextents (:return-type int) (:name "_XQueryTextExtents")) 
   (dpy (:pointer display))
   (fid font)
   (string (:pointer char))
   (nchars int)
   (dir (:pointer int))
   (font-ascent (:pointer int))
   (font-descent (:pointer int))
   (overall (:pointer xcharstruct)))

(def-exported-foreign-function (xquerybesttile (:return-type int) (:name "_XQueryBestTile")) 
   (dpy (:pointer display))
   (drawable drawable)
   (width unsigned-int)
   (height unsigned-int)
   (ret-width (:pointer unsigned-int))
   (ret-height (:pointer unsigned-int)))

(def-exported-foreign-function (xquerytree (:return-type int) (:name "_XQueryTree")) 
   (dpy (:pointer display))
   (w window)
   (root (:pointer window))
   (parent (:pointer window))
   (children (:pointer (:pointer window)))
   (nchildren (:pointer unsigned-int)))

(def-exported-foreign-function (xraisewindow (:return-type int) (:name "_XRaiseWindow")) 
   (dpy (:pointer display))
   (w window))

(def-exported-foreign-function (xrecolorcursor (:return-type int) (:name "_XRecolorCursor")) 
   (dpy (:pointer display))
   (cursor cursor)
   (foreground (:pointer xcolor))
   (background (:pointer xcolor)))

(def-exported-foreign-function (xconfigurewindow (:return-type int) (:name "_XConfigureWindow")) 
   (dpy (:pointer display))
   (w window)
   (mask unsigned-int)
   (changes (:pointer xwindowchanges)))

(def-exported-foreign-function (xreparentwindow (:return-type int) (:name "_XReparentWindow")) 
   (dpy (:pointer display))
   (w window)
   (p window)
   (x int)
   (y int))

(def-exported-foreign-function (xrestackwindows (:return-type int) (:name "_XRestackWindows")) 
   (dpy (:pointer display))
   (windows (:pointer window))
   (n int))

(def-exported-foreign-function (xrotatewindowproperties (:return-type int) (:name "_XRotateWindowProperties")) 
   (dpy (:pointer display))
   (w window)
   (properties (:pointer atom))
   (nprops int)
   (npositions int))

(def-exported-foreign-function (xselectinput (:name "_XSelectInput"))
   (dpy (:pointer display))
   (w window)
   (mask long))

(def-exported-foreign-function (xsendevent (:return-type int) (:name "_XSendEvent")) 
   (dpy (:pointer display))
   (w window)
   (propagate int)
   (event-mask long)
   (event (:pointer xevent)))

(def-exported-foreign-function (xsetbackground (:return-type int) (:name "_XSetBackground")) 
   (dpy (:pointer display))
   (gc gc)
   (background unsigned-long))

(def-exported-foreign-function (xsetcliprectangles (:return-type int) (:name "_XSetClipRectangles")) 
   (dpy (:pointer display))
   (gc gc)
   (clip-x-origin int)
   (clip-y-origin int)
   (rectangles (:pointer xrectangle))
   (n int)
   (ordering int))

(def-exported-foreign-function (xsetclipmask (:return-type int) (:name "_XSetClipMask")) 
   (dpy (:pointer display))
   (gc gc)
   (mask pixmap))

(def-exported-foreign-function (xsetcliporigin (:return-type int) (:name "_XSetClipOrigin")) 
   (dpy (:pointer display))
   (gc gc)
   (xorig int)
   (yorig int))

(def-exported-foreign-function (xsetdashes (:return-type int) (:name "_XSetDashes")) 
   (dpy (:pointer display))
   (gc gc)
   (dash-offset int)
   (list (:pointer char))
   (n int))

(def-exported-foreign-function (xsetfontpath (:return-type int) (:name "_XSetFontPath")) 
   (dpy (:pointer display))
   (directories (:pointer (:pointer char)))
   (ndirs int))

(def-exported-foreign-function (xsetfont (:return-type int) (:name "_XSetFont")) 
   (dpy (:pointer display))
   (gc gc)
   (font font))

(def-exported-foreign-function (xsetforeground (:return-type int) (:name "_XSetForeground")) 
   (dpy (:pointer display))
   (gc gc)
   (foreground unsigned-long))

(def-exported-foreign-function (xsetfunction (:return-type int) (:name "_XSetFunction")) 
   (dpy (:pointer display))
   (gc gc)
   (function int))

(def-exported-foreign-function (xsetcommand (:return-type int) (:name "_XSetCommand")) 
   (dpy (:pointer display))
   (w window)
   (argv (:pointer (:pointer char)))
   (argc int))

(def-exported-foreign-function (xsetinputfocus (:return-type int) (:name "_XSetInputFocus")) 
   (dpy (:pointer display))
   (focus window)
   (revert-to int)
   (time :unsigned-32bit))

(def-exported-foreign-function (xsetlineattributes (:return-type int) (:name "_XSetLineAttributes")) 
   (dpy (:pointer display))
   (gc gc)
   (linewidth unsigned-int)
   (linestyle int)
   (capstyle int)
   (joinstyle int))

(def-exported-foreign-function (xsetplanemask (:return-type int) (:name "_XSetPlaneMask")) 
   (dpy (:pointer display))
   (gc gc)
   (planemask unsigned-long))

(def-exported-foreign-function (xsetpointermapping (:return-type int) (:name "_XSetPointerMapping")) 
   (dpy (:pointer display))
   (map (:pointer unsigned-char))
   (nmaps int))

(def-exported-foreign-function (xchangekeyboardmapping (:return-type int) (:name "_XChangeKeyboardMapping")) 
   (dpy (:pointer display))
   (first-keycode int)
   (keysyms-per-keycode int)
   (keysyms (:pointer keysym))
   (nkeycodes int))

(def-exported-foreign-function (xsetselectionowner (:return-type int) (:name "_XSetSelectionOwner")) 
   (dpy (:pointer display))
   (selection :unsigned-32bit)
   (owner window)
   (time :unsigned-32bit))

(def-exported-foreign-function (xsetscreensaver (:return-type int) (:name "_XSetScreenSaver")) 
   (dpy (:pointer display))
   (timeout int)
   (interval int)
   (prefer-blank int)
   (allow-exp int))

(def-exported-foreign-function (xsetstate (:return-type int) (:name "_XSetState")) 
   (dpy (:pointer display))
   (gc gc)
   (function int)
   (planemask unsigned-long)
   (foreground unsigned-long)
   (background unsigned-long))

(def-exported-foreign-function (xsetstipple (:return-type int) (:name "_XSetStipple")) 
   (dpy (:pointer display))
   (gc gc)
   (stipple pixmap))

(def-exported-foreign-function (xsettsorigin (:return-type int) (:name "_XSetTSOrigin")) 
   (dpy (:pointer display))
   (gc gc)
   (x int)
   (y int))

(def-exported-foreign-function (xsettile (:return-type int) (:name "_XSetTile")) 
   (dpy (:pointer display))
   (gc gc)
   (tile pixmap))

(def-exported-foreign-function (xstorecolor (:return-type int) (:name "_XStoreColor")) 
   (dpy (:pointer display))
   (cmap colormap)
   (def (:pointer xcolor)))

(def-exported-foreign-function (xstorecolors (:return-type int) (:name "_XStoreColors")) 
   (dpy (:pointer display))
   (cmap colormap)
   (defs (:pointer xcolor))
   (ncolors int))

(def-exported-foreign-function (xstorenamedcolor (:return-type int) (:name "_XStoreNamedColor")) 
   (dpy (:pointer display))
   (cmap colormap)
   (pixel unsigned-long)
   (name (:pointer char))
   (flags int))

(def-exported-foreign-function (xstorename (:return-type int) (:name "_XStoreName")) 
   (dpy (:pointer display))
   (w window)
   (name (:pointer char)))

(def-exported-foreign-function (xseticonname (:return-type int) (:name "_XSetIconName")) 
   (dpy (:pointer display))
   (w window)
   (icon-name (:pointer char)))

(def-exported-foreign-function (xstringtokeysym (:return-type keysym) (:name "_XStringToKeysym")) 
   (s (:pointer char)))

(def-exported-foreign-function (xkeysymtostring (:return-type (:pointer char)) (:name "_XKeysymToString")) 
   (ks keysym))

(def-exported-foreign-function (xsync (:return-type int) (:name "_XSync")) 
   (dpy (:pointer display))
   (discard int))

(def-exported-foreign-function (xsynchronize (:return-type (:pointer :pointer)) (:name "_XSynchronize")) 
   (dpy (:pointer display))
   (onoff int))

(def-exported-foreign-function (xsetafterfunction (:return-type (:pointer :pointer)) (:name "_XSetAfterFunction")) 
   (dpy (:pointer display))
   (func (:pointer :pointer)))

(def-exported-foreign-function (xtextextents (:return-type int) (:name "_XTextExtents")) 
   (fontstruct (:pointer xfontstruct))
   (string (:pointer char))
   (nchars int)
   (dir (:pointer int))
   (font-ascent (:pointer int))
   (font-descent (:pointer int))
   (overall (:pointer xcharstruct)))

(def-exported-foreign-function (xtextwidth (:return-type int) (:name "_XTextWidth")) 
   (fontstruct (:pointer xfontstruct))
   (string (:pointer char))
   (count int))

(def-exported-foreign-function (xtextextents16 (:return-type int) (:name "_XTextExtents16")) 
   (fontstruct (:pointer xfontstruct))
   (string (:pointer xchar2b))
   (nchars int)
   (dir (:pointer int))
   (font-ascent (:pointer int))
   (font-descent (:pointer int))
   (overall (:pointer xcharstruct)))

(def-exported-foreign-function (xtextwidth16 (:return-type int) (:name "_XTextWidth16")) 
   (fontstruct (:pointer xfontstruct))
   (string (:pointer xchar2b))
   (count int))

(def-exported-foreign-function (xtranslatecoordinates (:return-type int) (:name "_XTranslateCoordinates")) 
   (dpy (:pointer display))
   (src-win window)
   (dest-win window)
   (src-x int)
   (src-y int)
   (dst-x (:pointer int))
   (dst-y (:pointer int))
   (child (:pointer window)))

(def-exported-foreign-function (xundefinecursor (:return-type int) (:name "_XUndefineCursor")) 
   (dpy (:pointer display))
   (w window))

(def-exported-foreign-function (xungrabbutton (:return-type int) (:name "_XUngrabButton")) 
   (dpy (:pointer display))
   (button unsigned-int)
   (modifiers unsigned-int)
   (grab-window window))

(def-exported-foreign-function (xungrabkeyboard (:return-type int) (:name "_XUngrabKeyboard")) 
   (dpy (:pointer display))
   (time :unsigned-32bit))

(def-exported-foreign-function (xungrabkey (:return-type int) (:name "_XUngrabKey")) 
   (dpy (:pointer display))
   (key int)
   (modifiers unsigned-int)
   (grab-window window))

(def-exported-foreign-function (xungrabpointer (:return-type int) (:name "_XUngrabPointer")) 
   (dpy (:pointer display))
   (time :unsigned-32bit))

(def-exported-foreign-function (xungrabserver (:return-type int) (:name "_XUngrabServer")) 
   (dpy (:pointer display)))

(def-exported-foreign-function (xuninstallcolormap (:return-type int) (:name "_XUninstallColormap")) 
   (dpy (:pointer display))
   (cmap colormap))

(def-exported-foreign-function (xunloadfont (:return-type int) (:name "_XUnloadFont")) 
   (dpy (:pointer display))
   (font font))

(def-exported-foreign-function (xunmapsubwindows (:return-type int) (:name "_XUnmapSubwindows")) 
   (dpy (:pointer display))
   (win window))

(def-exported-foreign-function (xunmapwindow (:return-type int) (:name "_XUnmapWindow")) 
   (dpy (:pointer display))
   (w window))

(def-exported-foreign-function (xwarppointer (:return-type int) (:name "_XWarpPointer")) 
   (dpy (:pointer display))
   (src-win window)
   (dest-win window)
   (src-x int)
   (src-y int)
   (src-width unsigned-int)
   (src-height unsigned-int)
   (dest-x int)
   (dest-y int))

(def-exported-foreign-function (xwindowevent (:return-type int) (:name "_XWindowEvent")) 
   (dpy (:pointer display))
   (w window)
   (mask long)
   (event (:pointer xevent)))

(def-exported-foreign-function (xcreatewindow (:return-type window) (:name "_XCreateWindow")) 
   (dpy (:pointer display))
   (parent window)
   (x int)
   (y int)
   (width unsigned-int)
   (height unsigned-int)
   (borderwidth unsigned-int)
   (depth int)
   (class unsigned-int)
   (visual (:pointer visual))
   (valuemask unsigned-long)
   (attributes (:pointer xsetwindowattributes)))

(def-exported-foreign-function (xfree (:return-type fixnum-int) (:name "_XFree")) 
   (data (:pointer char)))


(def-exported-foreign-function (xcreateimage (:return-type (:pointer ximage))
					     (:name "_XCreateImage"))
   (dpy (:pointer display))
   (visual (:pointer visual))
   (depth unsigned-int)
   (format int)
   (offset int)
   (data  (:pointer char))
   (width unsigned-int)
   (height unsigned-int)
   (bitmap-pad int)
   (bytes-per-line int))

(def-exported-foreign-macro (xdestroyimage (:return-type int) (:name "_XDestroyImage"))
  (ximage (:pointer ximage)))

(def-exported-foreign-macro (xgetpixel (:return-type unsigned-long) (:name "_XGetPixel"))
  (ximage (:pointer ximage))
  (x int)
  (y int))

(def-exported-foreign-macro (xputpixel (:return-type int) (:name "_XPutPixel"))
  (ximage (:pointer ximage))
  (x int)
  (y int)
  (pixel unsigned-long))

(def-exported-foreign-macro (xsubimage (:return-type (:pointer ximage)) (:name "_XSubImage"))
  (ximage (:pointer ximage))
  (x int)
  (y int)
  (subimage-width unsigned-int)
  (subimage-height unsigned-int))

(def-exported-foreign-macro (xaddpixel (:return-type int) (:name "_XAddPixel"))
  (ximage (:pointer ximage))
  (value long))

(def-exported-foreign-function (xreadbitmapfile (:return-type int) (:name "_XReadBitmapFile"))
  (display (:pointer display))
  (d drawable)
  (filename (:pointer char))
  (width_return (:pointer unsigned-int))
  (height_return (:pointer unsigned-int))
  (bitmap_return (:pointer pixmap))
  (x-hot-return (:pointer int))
  (y-hot-return (:pointer int)))

(def-exported-foreign-function (xwritebitmapfile (:return-type int) (:name "_XWriteBitmapFile"))
  (display (:pointer display))
  (filename (:pointer char))
  (bitmap pixmap)
  (width unsigned-int)
  (height unsigned-int)
  (x-hot int)
  (y-hot int))



;;; Xlib Output Functions
;;;
;;; All of the following functions have been modified - the type of int coordinate 
;;; arguments have been changed to fixnum-int, the type of drawable arguments
;;; have been changed to fixnum-drawable, the type of int and unsigned-int dimensions
;;; (length, npoints etc) arguments have been changed to fixnum-int or fixnum-unsigned-int, 
;;; and the :return-type option has been omitted (to avoid consing up an integer 
;;; return value).  A function that draws a single character, XDrawChar function has 
;;; been added.

(def-exported-foreign-function (xdrawarc (:name "_XDrawArc")) 
   (dpy (:pointer display))
   (d fixnum-drawable)
   (gc gc)
   (x fixnum-int)
   (y fixnum-int)
   (width fixnum-int)
   (height fixnum-int)
   (angle1 fixnum-int)
   (angle2 fixnum-int))

(def-exported-foreign-function (xdrawarcs (:name "_XDrawArcs")) 
   (dpy (:pointer display))
   (d fixnum-drawable)
   (gc gc)
   (arcs (:pointer xarc))
   (n-arcs fixnum-int)) 

(def-exported-foreign-function (xdrawline (:name "_XDrawLine")) 
   (dpy (:pointer display))
   (d fixnum-drawable)
   (gc gc)
   (x1 fixnum-int)
   (y1 fixnum-int)
   (x2 fixnum-int)
   (y2 fixnum-int))

(def-exported-foreign-function (xdrawlines (:name "_XDrawLines")) 
   (dpy (:pointer display))
   (d fixnum-drawable)
   (gc gc)
   (points (:pointer xpoint))
   (npoints fixnum-int) 
   (mode fixnum-int))   

(def-exported-foreign-function (xdrawpoint (:name "_XDrawPoint")) 
   (dpy (:pointer display))
   (d fixnum-drawable)
   (gc gc)
   (x fixnum-int)
   (y fixnum-int))

(def-exported-foreign-function (xdrawpoints (:name "_XDrawPoints")) 
   (dpy (:pointer display))
   (d fixnum-drawable)
   (gc gc)
   (points (:pointer xpoint))
   (n-points fixnum-int) 
   (mode fixnum-int))    

(def-exported-foreign-function (xdrawrectangle (:name "_XDrawRectangle")) 
   (dpy (:pointer display))
   (d fixnum-drawable)
   (gc gc)
   (x fixnum-int)
   (y fixnum-int)
   (width fixnum-unsigned-int)     
   (height fixnum-unsigned-int))   

(def-exported-foreign-function (xdrawrectangles (:name "_XDrawRectangles")) 
   (dpy (:pointer display))
   (d fixnum-drawable)
   (gc gc)
   (rects (:pointer xrectangle))
   (n-rects fixnum-int)) ;; was int

(def-exported-foreign-function (xdrawsegments (:name "_XDrawSegments")) 
   (dpy (:pointer display))
   (d fixnum-drawable)
   (gc gc)
   (segments (:pointer xsegment))
   (nsegments fixnum-int)) 

(def-exported-foreign-function (xdrawimagestring (:name "_XDrawImageString")) 
   (dpy (:pointer display))
   (d fixnum-drawable)
   (gc gc)
   (x fixnum-int)
   (y fixnum-int)
   (string (:pointer char))
   (length fixnum-int)) 

(def-exported-foreign-function (xdrawimagestring16 (:name "_XDrawImageString16")) 
   (dpy (:pointer display))
   (d fixnum-drawable)
   (gc gc)
   (x fixnum-int)
   (y fixnum-int)
   (string (:pointer xchar2b))
   (length fixnum-int)) 

(def-exported-foreign-function (xdrawtext (:name "_XDrawText")) 
   (dpy (:pointer display))
   (d fixnum-drawable)
   (gc gc)
   (x fixnum-int)
   (y fixnum-int)
   (items (:pointer xtextitem))
   (nitems fixnum-int)) 

(def-exported-foreign-function (xdrawtext16 (:name "_XDrawText16")) 
   (dpy (:pointer display))
   (d fixnum-drawable)
   (gc gc)
   (x fixnum-int)
   (y fixnum-int)
   (items (:pointer xtextitem16))
   (nitems fixnum-int))

(def-exported-foreign-function (xdrawstring (:name "_XDrawString")) 
   (dpy (:pointer display))
   (d fixnum-drawable)
   (gc gc)
   (x fixnum-int)
   (y fixnum-int)
   (string (:pointer char))
   (length fixnum-int))

(def-exported-foreign-function (xdrawstring16 (:name "_XDrawString16")) 
   (dpy (:pointer display))
   (d fixnum-drawable)
   (gc gc)
   (x fixnum-int)
   (y fixnum-int)
   (string (:pointer xchar2b))
   (length fixnum-int))

(def-exported-foreign-function (XDrawChar (:name "_XDrawString"))
   (dpy (:pointer display))
   (d fixnum-drawable)
   (gc gc)
   (x fixnum-int)
   (y fixnum-int)
   (string x11-char-string)
   (length fixnum-int))


(def-exported-foreign-function (lisp-xdrawstring (:name "_lisp_XDrawString"))
   (dpy (:pointer display))
   (d fixnum-drawable)
   (gc gc)
   (x fixnum-int)
   (y fixnum-int)
   (string (:pointer char))
   (start fixnum-int)
   (end fixnum-int))


(def-exported-foreign-function 
  (xpermalloc (:return-type (:pointer char)) 
	      (:name "_Xpermalloc"))
  (size unsigned-int))

(def-exported-foreign-function
  (xrmstringtoquark (:return-type xrmquark)
                    (:name "_XrmStringToQuark"))
  (name xrmstring))

(def-exported-foreign-function
  (xrmquarktostring (:return-type xrmstring)
                    (:name "_XrmQuarkToString"))
  (name xrmquark))

(def-exported-foreign-function
  (xrmuniquequark (:return-type xrmquark)
                  (:name "_XrmUniqueQuark")))

(def-exported-foreign-function
  (xrmstringtoquarklist (:return-type void)
                        (:name "_XrmStringToQuarkList"))
  (name (:pointer char))
  (quarks xrmquarklist))

(def-exported-foreign-function
  (xrmstringtobindingquarklist (:return-type void)
                               (:name "_XrmStringToBindingQuarkList"))
  (name (:pointer char))
  (bindings xrmbindinglist)
  (quarks xrmquarklist))

(def-exported-foreign-function
  (xrminitialize (:return-type void)
                 (:name "_XrmInitialize")))

(def-exported-foreign-function
  (xrmqputresource (:return-type void)
                   (:name "_XrmQPutResource"))
  (pdb (:pointer xrmdatabase))
  (bindings xrmbindinglist)
  (quarks xrmquarklist)
  (type xrmrepresentation)
  (value (:pointer xrmvalue)))

(def-exported-foreign-function
  (xrmputresource (:return-type void)
                  (:name "_XrmPutResource"))
  (pdb (:pointer xrmdatabase))
  (specifier (:pointer char))
  (type (:pointer char))
  (value (:pointer xrmvalue)))

(def-exported-foreign-function
  (xrmqputstringresource (:return-type void)
                         (:name "_XrmQPutStringResource"))
  (pdb (:pointer xrmdatabase))
  (bindings xrmbindinglist)
  (quarks xrmquarklist)
  (str (:pointer char)))

(def-exported-foreign-function
  (xrmputstringresource (:return-type void)
                        (:name "_XrmPutStringResource"))
  (pdb (:pointer xrmdatabase))
  (specifier (:pointer char))
  (str (:pointer char)))

(def-exported-foreign-function
  (xrmputlineresource (:return-type void)
                      (:name "_XrmPutLineResource"))
  (pdb (:pointer xrmdatabase))
  (line (:pointer char)))

(def-exported-foreign-function
  (xrmqgetresource (:return-type int)
                   (:name "_XrmQGetResource"))
  (db xrmdatabase)
  (names xrmnamelist)
  (classes xrmclasslist)
  (type (:pointer xrmrepresentation))
  (value (:pointer xrmvalue)))

(def-exported-foreign-function
  (xrmgetresource (:return-type :fixnum)
                  (:name "_XrmGetResource"))
  (db xrmdatabase)
  (name-str (:pointer char))
  (class-str (:pointer char))
  (type (:pointer (:pointer char)))
  (value (:pointer xrmvalue)))

(def-exported-foreign-function
  (xrmqgetsearchlist (:return-type :fixnum)
                     (:name "_XrmQGetSearchList"))
  (db xrmdatabase)
  (names xrmnamelist)
  (classes xrmclasslist)
  (searchlist xrmsearchlist)
  (listlength int))

(def-exported-foreign-function
  (xrmqgetsearchresource (:return-type :fixnum)
                         (:name "_XrmQGetSearchResource"))
  (searchlist xrmsearchlist)
  (name xrmname)
  (class xrmclass)
  (type (:pointer xrmrepresentation))
  (value (:pointer xrmvalue)))

(def-exported-foreign-function
  (xrmgetfiledatabase (:return-type xrmdatabase)
                      (:name "_XrmGetFileDatabase"))
  (filename (:pointer char)))

(def-exported-foreign-function
  (xrmgetstringdatabase (:return-type xrmdatabase)
                        (:name "_XrmGetStringDatabase"))
  (data (:pointer char)))

(def-exported-foreign-function
  (xrmputfiledatabase (:return-type void)
                      (:name "_XrmPutFileDatabase"))
  (db xrmdatabase)
  (filename (:pointer char)))

(def-exported-foreign-function
  (xrmmergedatabases (:return-type void)
                     (:name "_XrmMergeDatabases"))
  (new xrmdatabase)
  (into (:pointer xrmdatabase)))

(def-exported-foreign-function
  (xrmparsecommand (:return-type void)
                   (:name "_XrmParseCommand"))
  (pdb (:pointer xrmdatabase))
  (options xrmoptiondesclist)
  (num-options int)
  (prefix (:pointer char))
  (argc (:pointer int))
  (argv (:pointer (:pointer char))))

(def-exported-foreign-function (xallocwmhints 
				(:name "_XAllocWMHints")
				(:return-type (:pointer xwmhints))))
  
(def-exported-foreign-function (xsetwmhints (:name "_XSetWMHints"))
  (display (:pointer display))
  (window window)
  (wmhints (:pointer xwmhints)))

(def-exported-foreign-function (xgetwmhints (:return-type (:pointer xwmhints))
					    (:name "_XGetWMHints"))
  (display (:pointer display))
  (window window))

(def-exported-foreign-function (xallocsizehints
				(:name "_XAllocSizeHints")
				(:return-type (:pointer xsizehints))))
  
(def-exported-foreign-function (xsetwmnormalhints (:return-type void)
						  (:name "_XSetWMNormalHints"))
  (display (:pointer display))
  (window window)
  (sizehints (:pointer xsizehints)))

(def-exported-foreign-function (xgetwmnormalhints (:return-type int)
						  (:name "_XGetWMNormalHints"))
  (display (:pointer display))
  (window window)
  (hints-return (:pointer xsizehints))
  (supplied-return (:pointer long)))


(def-exported-foreign-function (xsavecontext (:return-type fixnum-int)
					     (:name "_XSaveContext"))
  (window window)
  (context xcontext)
  (data (:pointer :signed-32bit)))

(def-exported-foreign-function (xfindcontext (:return-type fixnum-int)
					     (:name "_XFindContext"))
  (display (:pointer display))
  (window window)
  (context xcontext)
  (data (:pointer :signed-32bit)))

(def-exported-foreign-function (xdeletecontext (:return-type fixnum-int)
					       (:name "_XDeleteContext"))
  (window window)
  (context xcontext))

(def-exported-foreign-function (xlookupstring (:return-type fixnum-int)
					      (:name "_XLookupString"))
  (event-struct  (:pointer xkeyevent))
  (buffer-return (:pointer char))
  (bytes-buffer	int)
  (keysym-return (:pointer keysym))
  (status-in-out (:pointer xcomposestatus)))
