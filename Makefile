# $fiHeader: Makefile,v 1.63 92/11/11 17:13:48 cer Exp $
# 
#  Makefile for CLIM 2.0
#
#CL	= /usr/tech/cer/cl/src/dcl
CL	= /net/vapor/scm2/4.2.beta/src/dcl
PWD	= /usr/tech/cer/stuff/clim-2.0
DUMP-CL	= $(CL)
CLOPTS	= -qq

# Training

TRAIN_TIMES=2

TRAIN_LISP= \
	(progn \
	(load \"test/test.lisp\") \
	(clim-user::train-clim-2  $(TRAIN_TIMES)) \
	(clim-user::do-frame-tests) \
	(compile-file \"misc/clos-preload.cl\" \
	:output-file \
	(if (excl::featurep :clim-motif) \"misc/clos-preloadxm.fasl\" \
	    \"misc/clos-preloadol.fasl\") \
	)) \
	(exit 0)

TRAIN_TEXT = \
	$(ECHO) "\
	$(TRAIN_LISP) \
" | $(CLIM) $(CLOPTS) -batch \
	; echo CLIM trained!!!!	

# Info
LOAD_SOURCE_FILE_INFO=t
LOAD_XREF_INFO=nil
RECORD_SOURCE_FILE_INFO=t
RECORD_XREF_INFO=nil

# Lisp optimization for compiling
SPEED	= 3
SAFETY	= 1
# This next should be set to 1 for distribution
DEBUG   = 2

make = make SPEED=${SPEED} SAFETY=${SAFETY} DEBUG=${DEBUG} \
	LOAD_SOURCE_FILE_INFO=${LOAD_SOURCE_FILE_INFO} \
	RECORD_SOURCE_FILE_INFO=${RECORD_SOURCE_FILE_INFO} \
	LOAD_XREF_INFO=${LOAD_XREF_INFO} \
	RECORD_XREF_INFO=${RECORD_XREF_INFO}


CFLAGS	= -O -D_NO_PROTO -DSTRINGS_ALIGNED -DNO_REGEX -DNO_ISDIR -DUSE_RE_COMP -DUSER_GETWD -I/x11/motif-1.1/lib

OLDSPACE = 15000000
NEWSPACE = 5000000
PREMALLOCS = '-m 401408'

# Name of dumped lisp
CLIM	= ./slim
CLIMOL= $(CLIM)
CLIMXM= $(CLIM)

CLIM-SMALL	= ./slim-small

PUBDIRS	= sys utils silica clim demo test genera clx pre-silica postscript compatibility
DIRS0	=  tk tk-silica misc cloe
DIRS	= $(PUBDIRS) xlib $(DIRS0)
CHEAP_CLEAN	= $(PUBDIRS) $(DIRS0)

DEVICE	= /dev/null
RM	= /bin/rm
CAT	= /bin/cat
ECHO	= /bin/echo
MV	= /usr/fi/mv-nfs
TAGS	= /usr/fi/lib/emacs/etc/etags
TMP	= /usr/tmp

SRC_FILES = */*.lisp *.lisp Makefile misc/make-stub-file \
	    misc/undefinedsymbols misc/undefinedsymbols.olit \
	    misc/undefinedsymbols.motif misc/undefinedsymbols.xt \
	    xlib/xlibsupport.c misc/MyDrawingA*.[hc] misc/olsupport.c \
	    misc/clos-preload.cl misc/xtsupport.c

DEST=/dev/null

CL_SRC=/usr/tech/cer/cl/src
OPENWINHOME=/usr/openwin-3.0


DEBUGLIB=
MOTIFLIB=/x11/R4/sun4-lib/libXm$(DEBUGLIB).a
MOTIFLIB_d=/x11/R4/sun4-lib/libXm_d.a
XLIB= /x11/R4/sun4-lib/libX11$(DEBUGLIB).a 
XTLIB=/x11/R4/sun4-lib/libXt$(DEBUGLIB).a
XLIBS= $(XTLIB) $(XLIB)

OLCOPYLIB=/vapor/x11/olit-3.0/lib3/sun4
OLXLIBS=$(OLCOPYLIB)/libXt.a $(OLCOPYLIB)/libX11.a
LIBXOL=$(OLCOPYLIB)/libXol.a
LIBXOL_d=$(LIBXOL)

# This has to be kept consistent with xlib.lisp
UNDEFS=misc/undefinedsymbols

# This should be the same as load-xt
XT_UNDEFS=misc/undefinedsymbols.xt

# This should be the same as load-xm
XM_UNDEFS=misc/undefinedsymbols.motif

# This should be the same as load-ol
OL_UNDEFS=misc/undefinedsymbols.olit

# These are the fasls and the .o that form the product

CLIMFASLS= climg.fasl climol.fasl climxm.fasl clim-debug.fasl clim-debugol.fasl \
	   clim-debugxm.fasl climps.fasl climgg.fasl # clim1compat.fasl

PUBLIC_OBJS=  stub-xt.o stub-x.o stub-olit.o stub-motif.o \
	  xlibsupport.o MyDrawingA.o \
	  olsupport.o xtsupport.o

OL_LICENSED_OBJS = clim-olit.o clim-olit_d.o
XM_LICENSED_OBJS = clim-motif_d.o clim-motif.o 

CLIMOBJS=$(PUBLIC_OBJS) $(XM_LICENSED_OBJS) $(OL_LICENSED_OBJS)

INSTALLED_CLIMOBJS=$(PUBLIC_OBJS) $(XM_LICENSED_OBJS) $(OL_LICENSED_OBJS)


# These are linked into the distribution
# `pwd`/clim-olit_d.o \ `pwd`/clim-olit.o \

#FCLIMOBJS= `pwd`/clim-motif_d.o `pwd`/clim-motif.o \
#	    `pwd`/stub-xt.o `pwd`/stub-x.o \
#	   `pwd`/xlibsupport.o `pwd`/MyDrawingA.o `pwd`/olsupport.o `pwd`/xtsupport.o

# These are built into xm-dcl and ol-dcl.
COMPOSEROBJS= /scm/4.1/sparc/src/code/excldep.o /scm/4.1/sparc/src/code/socket.o \
	      /scm/4.1/sparc/src/code/gc_cursor.o \
	      /scm/4.1/sparc/src/code/unixsocket.o \
	      /scm/4.1/sparc/src/code/io.o /scm/4.1/sparc/src/code/exclio.o \
	      /scm/4.1/sparc/src/code/RunStatus.o

MALLOCOBJS =
# Uncomment to enable malloc debugging.
#
# MALLOCOBJS= $(PWD)/malloclib/malloc.o $(PWD)/malloclib/free.o $(PWD)/malloclib/realloc.o \
#	    $(PWD)/malloclib/calloc.o $(PWD)/malloclib/string.o \
#	    $(PWD)/malloclib/malloc_chk.o $(PWD)/malloclib/malloc_chn.o \
#	    $(PWD)/malloclib/memory.o $(PWD)/malloclib/tostring.o \
#	    $(PWD)/malloclib/m_perror.o $(PWD)/malloclib/m_init.o \
#	    $(PWD)/malloclib/mallopt.o $(PWD)/malloclib/dump.o $(PWD)/malloclib/leak.o

#
# "Compile time objects" -- these go into clim-debug.fasl
#
DEBUG-OBJS = xlib/ffi.fasl xlib/xlib-defs.fasl xlib/xlib-funs.fasl \
	     xlib/x11-keysyms.fasl xlib/last.fasl \
	     tk/xt-defs.fasl tk/xt-funs.fasl 

XM-DEBUG-OBJS = tk/xm-defs.fasl tk/xm-funs.fasl
OL-DEBUG-OBJS = tk/ol-defs.fasl tk/ol-funs.fasl

#
# "Load time objects" -- these go into clim.fasl
#
CLIM-UTILS-OBJS = utils/excl-verification.fasl \
                   utils/lisp-package-fixups.fasl \
                   utils/defpackage.fasl \
                   utils/packages.fasl \
                   utils/defun-utilities.fasl \
                   utils/reader.fasl \
                   utils/clos-patches.fasl \
                   utils/clos.fasl \
                   utils/utilities.fasl \
                   utils/lisp-utilities.fasl \
                   utils/processes.fasl \
                   utils/queue.fasl \
		   utils/timers.fasl \
                   utils/protocols.fasl \
                   utils/autoconstructor.fasl \
                   utils/clim-streams.fasl \
                   utils/excl-streams.fasl \
                   utils/clim-macros.fasl \
                   utils/transformations.fasl \
                   utils/regions.fasl \
                   utils/region-arithmetic.fasl \
                   utils/extended-regions.fasl \
		   utils/base-designs.fasl \
                   utils/designs.fasl

CLIM-SILICA-OBJS = silica/classes.fasl \
                    silica/text-style.fasl \
                    silica/macros.fasl \
                    silica/sheet.fasl \
                    silica/mirror.fasl \
                    silica/event.fasl \
                    silica/port.fasl \
                    silica/medium.fasl \
                    silica/framem.fasl \
                    silica/graphics.fasl \
		    silica/pixmaps.fasl \
                    silica/std-sheet.fasl \
                    silica/layout.fasl \
                    silica/db-layout.fasl \
                    silica/db-box.fasl \
                    silica/db-table.fasl \
                    silica/gadgets.fasl \
                    silica/db-scroll.fasl \
                    silica/db-border.fasl

CLIM-STANDALONE-OBJS = clim/gestures.fasl \
                        clim/defprotocol.fasl \
                        clim/stream-defprotocols.fasl \
                        clim/defresource.fasl \
                        clim/temp-strings.fasl \
                        clim/clim-defs.fasl \
                        clim/stipples.fasl \
                        clim/stream-class-defs.fasl \
                        clim/interactive-defs.fasl \
                        clim/cursor.fasl \
                        clim/view-defs.fasl \
                        clim/input-defs.fasl \
                        clim/input-protocol.fasl \
                        clim/output-protocol.fasl \
                        clim/recording-protocol.fasl \
			clim/recording-defs.fasl \
			clim/text-recording.fasl \
			clim/graphics-recording.fasl \
                        clim/interactive-protocol.fasl \
                        clim/input-editor-commands.fasl \
                        clim/formatted-output-defs.fasl \
                        clim/incremental-redisplay.fasl \
                        clim/coordinate-sorted-set.fasl \
			clim/r-tree.fasl \
                        clim/window-stream.fasl \
                        clim/pixmap-streams.fasl \
                        clim/ptypes1.fasl \
                        clim/completer.fasl \
                        clim/presentations.fasl \
                        clim/translators.fasl \
                        clim/histories.fasl \
                        clim/ptypes2.fasl \
			clim/excl-presentations.fasl \
                        clim/standard-types.fasl \
                        clim/table-formatting.fasl \
                        clim/graph-formatting.fasl \
                        clim/surround-output.fasl \
                        clim/text-formatting.fasl \
                        clim/tracking-pointer.fasl \
                        clim/dragging-output.fasl \
                        clim/db-stream.fasl \
                        clim/gadget-output.fasl \
                        clim/accept.fasl \
                        clim/present.fasl \
                        clim/command.fasl \
                        clim/command-processor.fasl \
                        clim/basic-translators.fasl \
                        clim/frames.fasl \
			clim/default-frame.fasl \
                        clim/activities.fasl \
			clim/noting-progress.fasl \
                        clim/menus.fasl \
                        clim/accept-values.fasl \
			clim/drag-and-drop.fasl \
                        clim/item-list-manager.fasl \
                        clim/stream-trampolines.fasl

GENERIC-GADGETS = clim/db-menu.fasl clim/db-list.fasl clim/db-text.fasl silica/db-button.fasl \
	    silica/db-slider.fasl	   

XLIB-CLIM-OBJS = xlib/pkg.fasl xlib/load-xlib.fasl


LOAD-XM-OBJS=	tk/load-xm.fasl
LOAD-OL-OBJS=	tk/load-ol.fasl

XT-TK-OBJS =  	tk/pkg.fasl \
                tk/foreign-obj.fasl \
                tk/macros.fasl \
                tk/xlib.fasl \
                tk/font.fasl \
                tk/gcontext.fasl \
                tk/graphics.fasl \
                tk/meta-tk.fasl \
                tk/make-classes.fasl \
                tk/foreign.fasl \
                tk/widget.fasl \
                tk/resources.fasl \
                tk/event.fasl \
                tk/callbacks.fasl \
                tk/xt-classes.fasl \
                tk/xt-init.fasl

XM-TK-OBJS = tk/xm-classes.fasl \
		tk/xm-callbacks.fasl \
                tk/xm-init.fasl \
                tk/xm-widgets.fasl \
                tk/xm-font-list.fasl \
                tk/xm-protocols.fasl \
                tk/convenience.fasl \
                tk/make-widget.fasl

OL-CLIM-OBJS = tk/ol-classes.fasl \
                tk/ol-init.fasl \
		tk/ol-widgets.fasl \
                tk/ol-callbacks.fasl \
                tk/make-widget.fasl

MOTIF-CLIM-OBJS = tk-silica/pkg.fasl \
                   tk-silica/xt-silica.fasl \
                   tk-silica/xm-silica.fasl \
                   tk-silica/xt-graphics.fasl \
                   tk-silica/xm-graphics.fasl \
                   tk-silica/image.fasl \
                   tk-silica/xt-frames.fasl \
                   tk-silica/xm-frames.fasl \
                   tk-silica/xm-dialogs.fasl \
                   tk-silica/xt-gadgets.fasl \
                   tk-silica/xm-gadgets.fasl \
                   tk-silica/xm-menus.fasl \
                   tk-silica/xt-pixmaps.fasl \
                   tk-silica/xt-cursor.fasl \
                   tk-silica/last.fasl


OPENLOOK-CLIM-OBJS = tk-silica/pkg.fasl \
                      tk-silica/xt-silica.fasl \
                      tk-silica/ol-silica.fasl \
                      tk-silica/xt-graphics.fasl \
                      tk-silica/ol-graphics.fasl \
                      tk-silica/image.fasl \
                      tk-silica/xt-frames.fasl \
                      tk-silica/ol-frames.fasl \
                      tk-silica/xt-gadgets.fasl \
                      tk-silica/ol-gadgets.fasl \
		      tk-silica/xt-cursor.fasl \
                      tk-silica/xt-pixmaps.fasl \
		      tk-silica/last.fasl

POSTSCRIPT_CLIM= postscript/pkgdcl.fasl \
	postscript/postscript-port.fasl \
	postscript/postscript-medium.fasl \
	postscript/laserwriter-metrics.fasl 

# Used for tags
ALL_SRC =	   utils/excl-verification.lisp \
                   utils/lisp-package-fixups.lisp \
                   utils/defpackage.lisp \
                   utils/packages.lisp \
                   utils/defun-utilities.lisp \
                   utils/reader.lisp \
                   utils/clos-patches.lisp \
                   utils/clos.lisp \
                   utils/utilities.lisp \
                   utils/lisp-utilities.lisp \
                   utils/processes.lisp \
                   utils/queue.lisp \
                   utils/protocols.lisp \
                   utils/autoconstructor.lisp \
                   utils/clim-streams.lisp \
                   utils/excl-streams.lisp \
                   utils/clim-macros.lisp \
                   utils/transformations.lisp \
                   utils/regions.lisp \
                   utils/region-arithmetic.lisp \
                   utils/extended-regions.lisp \
                   utils/designs.lisp \
		    silica/classes.lisp \
                    silica/text-style.lisp \
                    silica/macros.lisp \
                    silica/sheet.lisp \
                    silica/mirror.lisp \
                    silica/event.lisp \
                    silica/port.lisp \
                    silica/medium.lisp \
                    silica/framem.lisp \
                    silica/graphics.lisp \
                    silica/pixmaps.lisp \
                    silica/std-sheet.lisp \
                    silica/layout.lisp \
                    silica/db-layout.lisp \
                    silica/db-box.lisp \
                    silica/db-table.lisp \
                    silica/gadgets.lisp \
                    silica/db-border.lisp \
                    silica/db-scroll.lisp \
                    silica/db-button.lisp \
                    silica/db-slider.lisp \
			clim/gestures.lisp \
                        clim/defprotocol.lisp \
                        clim/stream-defprotocols.lisp \
                        clim/defresource.lisp \
                        clim/temp-strings.lisp \
                        clim/clim-defs.lisp \
                        clim/stipples.lisp \
                        clim/stream-class-defs.lisp \
                        clim/interactive-defs.lisp \
                        clim/cursor.lisp \
                        clim/view-defs.lisp \
                        clim/input-defs.lisp \
                        clim/input-protocol.lisp \
                        clim/output-protocol.lisp \
                        clim/recording-defs.lisp \
                        clim/recording-protocol.lisp \
                        clim/text-recording.lisp \
                        clim/graphics-recording.lisp \
                        clim/interactive-protocol.lisp \
                        clim/input-editor-commands.lisp \
                        clim/formatted-output-defs.lisp \
                        clim/incremental-redisplay.lisp \
                        clim/coordinate-sorted-set.lisp \
                        clim/window-stream.lisp \
                        clim/pixmap-streams.lisp \
                        clim/ptypes1.lisp \
                        clim/completer.lisp \
                        clim/presentations.lisp \
                        clim/translators.lisp \
                        clim/histories.lisp \
                        clim/ptypes2.lisp \
                        clim/standard-types.lisp \
                        clim/excl-presentations.lisp \
                        clim/table-formatting.lisp \
                        clim/graph-formatting.lisp \
                        clim/surround-output.lisp \
                        clim/text-formatting.lisp \
                        clim/tracking-pointer.lisp \
                        clim/dragging-output.lisp \
                        clim/db-stream.lisp \
                        clim/gadget-output.lisp \
                        clim/accept.lisp \
                        clim/present.lisp \
                        clim/command.lisp \
                        clim/command-processor.lisp \
                        clim/basic-translators.lisp \
                        clim/frames.lisp \
                        clim/default-frame.lisp \
                        clim/noting-progress.lisp \
                        clim/menus.lisp \
                        clim/accept-values.lisp \
                        clim/drag-and-drop.lisp \
                        clim/item-list-manager.lisp \
                        clim/stream-trampolines.lisp \
	     xlib/pkg.lisp \
             xlib/ffi.lisp \
             xlib/xlib-defs.lisp \
             xlib/load-xlib.lisp \
             xlib/xlib-funs.lisp \
             xlib/x11-keysyms.lisp \
             xlib/last.lisp \
	      tk/load-xm.lisp \
              tk/load-ol.lisp \
	      tk/pkg.lisp \
              tk/macros.lisp \
              tk/xt-defs.lisp \
              tk/foreign-obj.lisp \
              tk/xlib.lisp \
              tk/font.lisp \
              tk/gcontext.lisp \
              tk/graphics.lisp \
              tk/meta-tk.lisp \
              tk/make-classes.lisp \
              tk/foreign.lisp \
              tk/widget.lisp \
              tk/resources.lisp \
              tk/event.lisp \
              tk/callbacks.lisp \
              tk/xt-classes.lisp \
              tk/xt-init.lisp \
              tk/xm-defs.lisp \
              tk/xm-classes.lisp \
              tk/xm-callbacks.lisp \
              tk/xt-funs.lisp \
              tk/xm-funs.lisp \
              tk/xm-classes.lisp \
              tk/xm-init.lisp \
              tk/xm-widgets.lisp \
              tk/xm-font-list.lisp \
              tk/xm-protocols.lisp \
              tk/convenience.lisp \
              tk/make-widget.lisp \
		   tk-silica/pkg.lisp \
                   tk-silica/xt-silica.lisp \
                   tk-silica/xm-silica.lisp \
                   tk-silica/xt-graphics.lisp \
                   tk-silica/xm-graphics.lisp \
                   tk-silica/image.lisp \
                   tk-silica/xt-frames.lisp \
                   tk-silica/xm-frames.lisp \
                   tk-silica/xt-gadgets.lisp \
                   tk-silica/xm-gadgets.lisp \
                   tk-silica/xm-menus.lisp \
                   tk-silica/xt-cursor.lisp \
                   tk-silica/xt-pixmaps.lisp \
	      tk/ol-defs.lisp \
              tk/ol-funs.lisp \
              tk/ol-classes.lisp \
              tk/ol-init.lisp \
              tk/ol-widgets.lisp \
              tk/ol-callbacks.lisp \
                      tk-silica/ol-silica.lisp \
                      tk-silica/ol-graphics.lisp \
                      tk-silica/ol-frames.lisp \
                      tk-silica/ol-gadgets.lisp


GENERIC-OBJS= $(CLIM-UTILS-OBJS) $(CLIM-SILICA-OBJS) $(CLIM-STANDALONE-OBJS)
MOTIF-OBJS = $(LOAD-XM-OBJS) $(XT-TK-OBJS) $(XM-TK-OBJS) $(MOTIF-CLIM-OBJS) 
OPENLOOK-OBJS = $(LOAD-OL-OBJS) $(XT-TK-OBJS) $(OL-CLIM-OBJS) $(OPENLOOK-CLIM-OBJS)

default: all-xm

trained-clim-xm:	
	(${make} all-xm train ; ${make} clim-xm)

trained-clim-ol:	
	(${make} all-ol train ; ${make} clim-ol)

all-xm:	compile-xm cat-xm clim-xm
all-ol:	compile-ol cat-ol clim-ol

compile-xm:	$(CLIMOBJS) FORCE
	$(ECHO) "\
	(si::system-compile-wrapper \
	 (function \
	  (lambda () \
	    (setq sys::*libxt-pathname* \"$(XTLIB)\") \
	    (setq sys::*libx11-pathname* \"$(XLIB)\") \
	    (setq sys::*clim-motif-pathname* \"clim-motif$(DEBUGLIB).o\") \
	    (load \"misc/compile-xm.lisp\"))) \
	 :speed $(SPEED) :debug $(DEBUG) :safety $(SAFETY) \
	 :record-source-file-info $(RECORD_SOURCE_FILE_INFO) \
	 :record-xref-info $(RECORD_XREF_INFO) \
	 :compile-print nil :compile-verbose nil \
	 :redefinition-warnings t :gcprint nil)" | $(CL) $(CLOPTS) -batch

compile-ol:	$(CLIMOBJS) FORCE
	$(ECHO) "\
	(si::system-compile-wrapper \
	 (function \
	  (lambda () \
	    (setf excl:*load-xref-info* $(LOAD_XREF_INFO)) \
	    (setq sys::*libxt-pathname* \"$(XTLIB)\") \
	    (setq sys::*libx11-pathname* \"$(XLIB)\") \
	    (setq sys::*clim-olit-pathname* \"clim-olit$(DEBUGLIB).o\") \
	    (setq *ignore-package-name-case* t) \
	    (set-case-mode :case-insensitive-lower) \
	    (load \"misc/compile-ol.lisp\"))) \
	 :speed $(SPEED) :debug $(DEBUG) :safety $(SAFETY) \
	 :record-source-file-info $(RECORD_SOURCE_FILE_INFO) \
	 :record-xref-info $(RECORD_XREF_INFO) \
	 :compile-print nil :compile-verbose nil \
	 :redefinition-warnings t :gcprint nil)" | $(CL) $(CLOPTS) -batch

# Concatenation

cat:	cat-xm cat-ol
cat-g:	climg.fasl clim-debug.fasl climps.fasl climgg.fasl # clim1compat.fasl
cat-xm:	cat-g climxm.fasl clim-debugxm.fasl 
cat-ol:	cat-g climol.fasl clim-debugol.fasl 

climg.fasl	: $(GENERIC-OBJS) $(XLIB-CLIM-OBJS)
	$(CAT) $(GENERIC-OBJS) $(XLIB-CLIM-OBJS) > $(TMP)/clim.fasl_`whoami`
	$(MV) $(TMP)/clim.fasl_`whoami` climg.fasl
	ls -lt climg.fasl >> Clim-sizes.n
	ls -lt climg.fasl

climgg.fasl	: $(GENERIC-GADGETS)
	$(CAT) $(GENERIC-GADGETS) > $(TMP)/clim.fasl_`whoami`
	$(MV) $(TMP)/clim.fasl_`whoami` climgg.fasl
	ls -lt climgg.fasl >> Clim-sizes.n
	ls -lt climgg.fasl


climxm.fasl	: $(MOTIF-OBJS) $(XLIB-CLIM-OBJS)
	$(CAT) $(MOTIF-OBJS) > $(TMP)/clim.fasl_`whoami`
	$(MV) $(TMP)/clim.fasl_`whoami` climxm.fasl
	ls -lt climxm.fasl >> Clim-sizes.n
	ls -lt climxm.fasl

climol.fasl	: $(OPENLOOK-OBJS)
	$(CAT) $(OPENLOOK-OBJS) > $(TMP)/clim.fasl_`whoami`
	$(MV) $(TMP)/clim.fasl_`whoami` climol.fasl
	ls -lt climol.fasl >> Clim-sizes.n
	ls -lt climol.fasl

echo-fasls:
	ls -lt $(MOTIF-OBJS) > /tmp/foo

clim-debug.fasl:	$(DEBUG-OBJS)
	$(CAT) $(DEBUG-OBJS) > $(TMP)/clim-debug.fasl_`whoami`
	$(MV) $(TMP)/clim-debug.fasl_`whoami` clim-debug.fasl
	ls -lt clim-debug.fasl >> Clim-sizes.n
	ls -lt clim-debug.fasl

clim-debugxm.fasl:	$(XM-DEBUG-OBJS)
	$(CAT) $(XM-DEBUG-OBJS) > $(TMP)/clim-debugxm.fasl_`whoami`
	$(MV) $(TMP)/clim-debugxm.fasl_`whoami` clim-debugxm.fasl
	ls -lt clim-debugxm.fasl >> Clim-sizes.n
	ls -lt clim-debugxm.fasl

clim-debugol.fasl:	$(OL-DEBUG-OBJS)
	$(CAT) $(OL-DEBUG-OBJS) > $(TMP)/clim-debugol.fasl_`whoami`
	$(MV) $(TMP)/clim-debugol.fasl_`whoami` clim-debugol.fasl
	ls -lt clim-debugol.fasl >> Clim-sizes.n
	ls -lt clim-debugol.fasl

climps.fasl: 	$(POSTSCRIPT_CLIM)
	$(CAT) $(POSTSCRIPT_CLIM) > $(TMP)/climps.fasl_`whoami`
	$(MV) $(TMP)/climps.fasl_`whoami` climps.fasl

CLIM1COMPAT= compatibility/packages.fasl compatibility/clim1-compatibility.fasl

clim1compat.fasl : $(CLIM1COMPAT)
	$(CAT) $(CLIM1COMPAT) > $(TMP)/clim1compat.fasl_`whoami`
	$(MV) $(TMP)/clim1compat.fasl_`whoami` clim1compat.fasl
	ls -lt clim1compat.fasl >> Clim-sizes.n
	ls -lt clim1compat.fasl

# We should only run these rules when
# We do this because we because we might have only compiled one port

tk/xm-defs.fasl : tk/xm-defs.lisp
	echo Foo


# Building

clim-xm:	FORCE $(CLIMOBJS)
#	-$(RM) $(CLIM)
	$(ECHO) " \
		(setq sys::*libxt-pathname* \"$(XTLIB)\") \
		(setq sys::*libx11-pathname* \"$(XLIB)\") \
	        (setq sys::*clim-motif-pathname* \"clim-motif$(DEBUGLIB).o\") \
		(load \"misc/dev-load-xm.lisp\") \
		(load \"misc/dump.lisp\")" | $(DUMP-CL) $(CLOPTS) -batch
	$(MV) $(TMP)/clim.temp_`whoami` $(CLIMXM)
	ls -lLt $(CLIMXM) >> Clim-sizes.n
	size $(CLIMXM) >> Clim-sizes.n
	ls -lLt $(CLIMXM)
	echo CLIM-XM built!!!!	

clim-ol:	FORCE $(CLIMOBJS)
#	-$(RM) $(CLIM)
	$(ECHO) " \
		(setq sys::*libxt-pathname* \"$(XTLIB)\") \
		(setq sys::*libx11-pathname* \"$(XLIB)\") \
	        (setq sys::*clim-olit-pathname* \"clim-olit$(DEBUGLIB).o\") \
		(load \"misc/dev-load-ol.lisp\") \
		(load \"misc/dump.lisp\")" | $(DUMP-CL) $(CLOPTS) -batch
	$(MV) $(TMP)/clim.temp_`whoami` $(CLIMOL)
	ls -lLt $(CLIMOL) >> Clim-sizes.n
	size $(CLIMOL) >> Clim-sizes.n
	ls -lLt $(CLIMOL)
	echo CLIM-OL built!!!!		

clim-small:	FORCE
	$(ECHO) " \
		(load \"misc/load-xm.lisp\") \
		(load \"misc/dump.lisp\")" | $(DUMP-CL) $(CLOPTS) -batch
	$(MV) $(TMP)/clim.temp_`whoami` $(CLIM-SMALL)
	ls -lt $(CLIM-SMALL) >> Clim-sizes.n
	size $(CLIM-SMALL) >> Clim-sizes.n
	ls -lt $(CLIM-SMALL)

# Training

train	:	FORCE
	$(TRAIN_TEXT)

BENCHMARK_FILE=nil

benchmark:
	$(ECHO) "\
	(load \"climtoys/test-clim.lisp\") \
	(clim-user::benchmark-clim $(BENCHMARK_FILE)) \
"  | $(CLIM) $(CLOPTS) -batch


PSVIEW=view

testps :
	$(ECHO) "\
	(load \"climtoys/test-clim.lisp\") \
	(load \"test/postscript-tests.lisp\") \
	(clim-user::run-postscript-tests :output :$(PSVIEW)) \
"  | $(CLIM) $(CLOPTS) -batch


echo-train:
	-$(ECHO) "$(TRAIN_LISP)"

# Misc


cleanobjs: 
	rm -f $(CLIMOBJS) stub-motif.o stub-olit.o

clean:
	find $(DIRS) -name "*.fasl" -print | xargs rm -f ; rm -f $(CLIMFASLS) \
	  $(CLIMOBJS) stub-motif.o stub-olit.o slim slim-small


cheapclean:
	find $(CHEAP_CLEAN) -name "*.fasl" -print | xargs rm -f


tags:
	$(TAGS) $(ALL_SRC)

wc:
	wc $(ALL_SRC)

swm-tape:
	tar cf $(DEVICE) `find $(PUBDIRS) '(' -name "*.cl" -o -name "*.lisp" ')' -print`

dist:	FORCE
	tar -cf -  $(SRC_FILES) | compress >  Dist/src.tar.Z

rcscheck:
	rcscheck $(DIRS) | grep -v .fasl

FORCE:

################## Make-dist stuff

makeclimfasls	: makeclimxmfasls makeclimolfasls
makeclimxmfasls	: compile-xm cat-xm
makeclimolfasls	: compile-ol cat-ol


install_clim	:
	cp $(CLIMFASLS) $(DEST)
	cp $(INSTALLED_CLIMOBJS) $(DEST)

# Link in the libraries & distribution object files with standard names

link-objects	:
	ln -s $(XTLIB) $(DEST)/libXt.a
	ln -s $(XLIB) $(DEST)/libX11.a
#	ln -s $(INSTALLED_CLIMOBJS) $(DEST)

# Backwards compatibility...
link-motif-libraries:	link-objects

echo_src_files:
	@ls $(SRC_FILES) | cat
	
makeclimobjs	: $(CLIMOBJS)

################## Lower level Makefile stuff


ol-dcl	:  stub-x.o stub-xt.o clim-olit.o xlibsupport.o olsupport.o xtsupport.o  $(MALLOCOBJS)
	cd $(CL_SRC) ; /bin/rm -f ucl ;\
	make initial_oldspace=$(OLDSPACE) oldspace=$(OLDSPACE) newspace=$(NEWSPACE) premallocs=$(PREMALLOCS) ucl_xtras='$(PWD)/stub-x.o $(PWD)/stub-xt.o $(PWD)/clim-olit.o $(PWD)/xlibsupport.o $(PWD)/olsupport.o $(PWD)/xtsupport.o $(COMPOSEROBJS) $(MALLOCOBJS) $(OLXLIBS)' dcl

xm-dcl	: stub-x.o stub-xt.o clim-motif.o xlibsupport.o xtsupport.o  MyDrawingA.o $(MALLOCOBJS)
	cd $(CL_SRC) ; /bin/rm -f ucl ;\
	make initial_oldspace=$(OLDSPACE) oldspace=$(OLDSPACE) newspace=$(NEWSPACE) premallocs=$(PREMALLOCS) ucl_xtras='$(PWD)/stub-x.o $(PWD)/stub-xt.o $(PWD)/clim-motif.o $(PWD)/xlibsupport.o $(PWD)/MyDrawingA.o $(PWD)/xtsupport.o $(COMPOSEROBJS) $(MALLOCOBJS) $(XTLIB) $(XLIB)' dcl	

dcl	: 
	cd $(CL_SRC) ; /bin/rm -f ucl ;\
	make dcl	

clim-motif.o	: stub-motif.o $(MOTIFLIB)
	ld -r -o clim-motif.o stub-motif.o $(MOTIFLIB)

clim-olit.o	: stub-olit.o $(LIBXOL)
	ld -r -o clim-olit.o stub-olit.o $(LIBXOL)

clim-motif_d.o	: stub-motif.o $(MOTIFLIB_d)
	ld -r -o clim-motif_d.o stub-motif.o $(MOTIFLIB_d)

clim-olit_d.o	: stub-olit.o $(LIBXOL_d)
	ld -r -o clim-olit_d.o stub-olit.o $(LIBXOL_d)


stub-motif.c	:  $(XT_UNDEFS) $(XM_UNDEFS) misc/make-stub-file
	misc/make-stub-file "void ___lisp_load_motif_stub ()"  $(XT_UNDEFS) $(XM_UNDEFS) > stub-motif.c 

stub-olit.c	:   $(XT_UNDEFS) $(OL_UNDEFS) misc/make-stub-file
	misc/make-stub-file "void ___lisp_load_olit_stub ()"   $(OL_UNDEFS) > stub-olit.c 

stub-x.c	:  $(UNDEFS) $(OL_UNDEFS) misc/make-stub-file
	misc/make-stub-file "void ___lisp_load_x_stub ()"  $(UNDEFS) > stub-x.c 

stub-xt.c	:  $(XT_UNDEFS) misc/make-stub-file
	misc/make-stub-file "void ___lisp_load_xt_stub ()"  $(XT_UNDEFS)  > stub-xt.c 

xlibsupport.o	: xlib/xlibsupport.c
	$(CC) -c $(CFLAGS) -o xlibsupport.o xlib/xlibsupport.c


xtsupport.o: misc/xtsupport.c
	$(CC) -c $(CFLAGS) -o xtsupport.o misc/xtsupport.c

MyDrawingA.o: misc/MyDrawingA.c
	$(CC) -c $(CFLAGS) -o MyDrawingA.o misc/MyDrawingA.c

olsupport.o: misc/olsupport.c
	$(CC) -c $(CFLAGS) -o olsupport.o misc/olsupport.c

FORCE	: 

xm-composer : xm-dcl
	cd /usr/composer2 ; make CL=$(CL) rebuild-c2

ol-composer : ol-dcl
	cd /usr/composer2 ; make CL=$(CL) rebuild-c2


