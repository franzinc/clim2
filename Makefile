# $fiHeader: Makefile,v 1.31 92/05/26 14:33:21 cer Exp $
# 
#  Makefile for CLIM 2.0
#
CL	= /usr/tech/cer/cl/src/dcl
DUMP-CL	= $(CL)
CLOPTS	= -qq

# Training
TRAIN_LISP= (load \"tk-silica/test-clim.lisp\") \
		(clim-user::train-clim-2) \
		(compile-file "misc/clos-preload.cl") \
		(exit 0) 

TRAIN_TEXT = \
	$(ECHO) "\
	$(TRAIN_LISP) \
" | $(CLIM) $(CLOPTS) -batch \
	; echo CLIM trained!!!!	

# Info
LOAD_SOURCE_FILE_INFO=t
RECORD_SOURCE_FILE_INFO=t
RECORD_XREF_INFO=nil
LOAD_XREF_INFO=nil

# Lisp optimization for compiling
SPEED	= 3
SAFETY	= 1

CFLAGS	= -O -D_NO_PROTO -DSTRINGS_ALIGNED -DNO_REGEX -DNO_ISDIR -DUSE_RE_COMP -DUSER_GETWD -I/x11/motif-1.1/lib

OLDSPACE = 15000000
NEWSPACE = 5000000

# Name of dumped lisp
CLIM	= ./slim
CLIM-SMALL	= ./slim-small

PUBDIRS	= sys utils silica clim demo test genera clx pre-silica
DIRS0	=  tk tk-silica misc
DIRS	= $(PUBDIRS) xlib $(DIRS0)
CHEAP_CLEAN	= $(PUBDIRS) $(DIRS0)

DEVICE	= /dev/null
RM	= /bin/rm
CAT	= /bin/cat
ECHO	= /bin/echo
MV	= /usr/fi/mv-nfs
TAGS	= /usr/fi/lib/emacs/etc/etags
TMP	= /usr/tmp

SRC_FILES = */*.lisp *.lisp Makefile */Makefile misc/make-stub-file \
	    misc/undefinedsymbols misc/undefinedsymbols.olit \
	    misc/undefinedsymbols.motif misc/undefinedsymbols.xt \
	    xlib/xlibsupport.c misc/MyDrawingA*.[hc]

DEST=/dev/null

CL_SRC=/usr/tech/cer/cl/src
OPENWINHOME=/usr/openwin-3.0

#MOTIFHOME=/usr/motif
#MOTIFLIB=$(MOTIFHOME)/usr/lib/libXm.a 
#XLIBS=$(MOTIFHOME)/usr/lib/libXt.a $(MOTIFHOME)/usr/lib/libX11.a

MOTIFLIB=/x11/motif-1.1/lib/Xm/libXm.a
XLIB= /x11/R4/sun4-lib/libX_d.a 
XTLIB=/x11/R4/sun4-lib/libXt_d.a
XLIBS= $(XTLIB) $(XLIB)

#OLXLIBS=$(XTLIB) $(XLIB)
OLCOPYLIB=/usr/tech/cer/stuff/clim-2.0/tk/lib2/
OLXLIBS=$(OLCOPYLIB)/libXt.a $(OLCOPYLIB)/libX11.a
#LIBXOL=$(OPENWINHOME)/lib/libXol.a
LIBXOL=$(OLCOPYLIB)/libXol.a

# This has to be kept consistent with xlib.lisp

UNDEFS=misc/undefinedsymbols

XT_UNDEFS=misc/undefinedsymbols.xt

# This should be the same as load-xm

XM_UNDEFS=misc/undefinedsymbols.motif

# This should be the same as load-ol

OL_UNDEFS=misc/undefinedsymbols.olit

CLIMFASLS= climg.fasl climol.fasl climxm.fasl clim-debug.fasl
CLIMOBJS= stub-x.o stub-xt.o stub-motif.o stub-olit.o xlibsupport.o MyDrawingA.o
FCLIMOBJS= `pwd`/stub-motif.o `pwd`/stub-olit.o `pwd`/stub-x.o `pwd`/stub-xt.o `pwd`/xlibsupport.o `pwd`/MyDrawingA.o


#
# "Compile time objects" -- these go into clim-debug.fasl
#
DEBUG-OBJS = xlib/ffi.fasl xlib/xlib-defs.fasl xlib/xlib-funs.fasl \
	     xlib/x11-keysyms.fasl xlib/load-xlib.fasl xlib/last.fasl \
	     tk/xt-defs.fasl tk/xm-defs.fasl 

# This should be there also
# tk/ol-defs.fasl


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
                   utils/protocols.fasl \
                   utils/autoconstructor.fasl \
                   utils/clim-streams.fasl \
                   utils/excl-streams.fasl \
                   utils/clim-macros.fasl \
                   utils/transformations.fasl \
                   utils/regions.fasl \
                   utils/region-arithmetic.fasl \
                   utils/extended-regions.fasl \
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
                        clim/window-stream.fasl \
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
			clim/noting-progress.fasl \
                        clim/menus.fasl \
                        clim/accept-values.fasl \
			clim/drag-and-drop.fasl \
                        clim/item-list-manager.fasl \
                        clim/pixmap-streams.fasl \
                        clim/stream-trampolines.fasl

XLIB-CLIM-OBJS = xlib/pkg.fasl

# Is this the correct place to put this load-xm??

XT-TK-OBJS =  xlib/load-xlib.fasl \
		tk/pkg.fasl \
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
	     tk/load-xm.fasl \
		tk/xt-funs.fasl \
		tk/xm-funs.fasl \
		tk/xm-callbacks.fasl \
                tk/xm-init.fasl \
                tk/xm-widgets.fasl \
                tk/xm-font-list.fasl \
                tk/xm-protocols.fasl \
                tk/convenience.fasl \
                tk/make-widget.fasl

OL-CLIM-OBJS = tk/ol-classes.fasl \
	     tk/load-ol.fasl \
		tk/xt-funs.fasl \
		tk/ol-funs.fasl \
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
                   tk-silica/xt-gadgets.fasl \
                   tk-silica/xm-gadgets.fasl \
                   tk-silica/xm-menus.fasl \
                   tk-silica/xt-pixmaps.fasl \
                   tk-silica/xt-cursor.fasl


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
                      tk-silica/xt-pixmaps.fasl

GENERIC-OBJS= $(CLIM-UTILS-OBJS) $(CLIM-SILICA-OBJS) $(CLIM-STANDALONE-OBJS)
XT-OBJS= $(XLIB-CLIM-OBJS) $(XT-TK-OBJS)
MOTIF-OBJS =  $(XM-TK-OBJS) $(MOTIF-CLIM-OBJS) 
OPENLOOK-OBJS = $(OL-CLIM-OBJS) $(OPENLOOK-CLIM-OBJS)

default: all-xm

trained-clim-xm:	
	(make all-xm train ; make clim-xm)

all-xm:	compile-xm cat-xm clim-xm
all-ol:	compile-ol cat-ol clim-ol

compile-xm:	$(CLIMOBJS) FORCE
	$(ECHO) " \
		(setf excl:*load-source-file-info* $(LOAD_SOURCE_FILE_INFO)) \
		(setf excl:*record-source-file-info* $(RECORD_SOURCE_FILE_INFO)) \
		(setf excl:*record-xref-info* $(RECORD_XREF_INFO)) \
		(setf excl:*load-xref-info* $(LOAD_XREF_INFO)) \
		(proclaim '(optimize (speed $(SPEED)) (safety $(SAFETY)))) \
		(load \"misc/compile-xm.lisp\")" | $(CL) $(CLOPTS) -batch
		echo CLIM-XM compiled!!!!

compile-ol:	$(CLIMOBJS) FORCE
	$(ECHO) " \
		(setf excl:*load-source-file-info* $(LOAD_SOURCE_FILE_INFO)) \
		(setf excl:*record-source-file-info* $(RECORD_SOURCE_FILE_INFO)) \
		(setf excl:*record-xref-info* $(RECORD_XREF_INFO)) \
		(setf excl:*load-xref-info* $(LOAD_XREF_INFO)) \
		(setq *ignore-package-name-case* t) \
		(set-case-mode :case-insensitive-lower) \
		(proclaim '(optimize (speed $(SPEED)) (safety $(SAFETY)))) \
		(load \"misc/compile-ol.lisp\")" | $(CL) $(CLOPTS) -batch
		echo CLIM-OL compiled!!!!

# Concatenation

cat:	cat-xm cat-ol
cat-g:	climg.fasl clim-debug.fasl
cat-xm:	cat-g climxm.fasl
cat-ol:	cat-g climol.fasl

climg.fasl	: $(GENERIC-OBJS) $(XT-OBJS)
	$(CAT)  $(GENERIC-OBJS) $(XT-OBJS) > $(TMP)/clim.fasl_`whoami`
	$(MV) $(TMP)/clim.fasl_`whoami` climg.fasl
	ls -lt climg.fasl >> Clim-sizes.n
	ls -lt climg.fasl

climxm.fasl	: $(MOTIF-OBJS)
	$(CAT)  $(MOTIF-OBJS) > $(TMP)/clim.fasl_`whoami`
	$(MV) $(TMP)/clim.fasl_`whoami` climxm.fasl
	ls -lt climxm.fasl >> Clim-sizes.n
	ls -lt climxm.fasl

climol.fasl	: $(OPENLOOK-OBJS)
	$(CAT)  $(OPENLOOK-OBJS) > $(TMP)/clim.fasl_`whoami`
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

# We should only run these rules when
# We do this because we because we might have only compiled one port

tk/xm-defs.fasl : tk/xm-defs.lisp
	echo Foo


# Building

clim-xm:	FORCE
	-$(RM) $(CLIM)
	$(ECHO) " \
		(load \"misc/dev-load-xm.lisp\") \
		(load \"misc/dump.lisp\")" | $(DUMP-CL) $(CLOPTS) -batch
	$(MV) $(TMP)/clim.temp_`whoami` $(CLIM)
	ls -lt $(CLIM) >> Clim-sizes.n
	size $(CLIM) >> Clim-sizes.n
	ls -lt $(CLIM)
	echo CLIM-XM built!!!!	

clim-ol:	FORCE
	-$(RM) $(CLIM)
	$(ECHO) " \
		(setq *ignore-package-name-case* t) \
		(set-case-mode :case-insensitive-lower) \
		(load \"misc/dev-load-ol.lisp\") \
		(load \"misc/dump.lisp\")" | $(DUMP-CL) $(CLOPTS) -batch
	$(MV) $(TMP)/clim.temp_`whoami` $(CLIM)
	ls -lt $(CLIM) >> Clim-sizes.n
	size $(CLIM) >> Clim-sizes.n
	ls -lt $(CLIM)
	echo CLIM-OL built!!!!		

clim-small:	FORCE
	$(ECHO) " \
		(load \"misc/load-xm.lisp\") \
		(load \"misc/dump.lisp\")" | $(DUMP-CL) $(CLOPTS) -batch
	$(MV) $(TMP)/clim.temp_`whoami` $(CLIM-SMALL)
	ls -lt $(CLIM-SMALL) >> Clim-sizes.n
	size $(CLIM) >> Clim-sizes.n
	ls -lt $(CLIM-SMALL)

# Training

train	:	FORCE
	$(TRAIN_TEXT)

# Misc


clean:
	find $(DIRS) -name "*.fasl" -print | xargs rm -f ; rm -f $(CLIMFASLS) \
	  $(CLIMOBJS) slim slim-small


cheapclean:
	find $(CHEAP_CLEAN) -name "*.fasl" -print | xargs rm -f


tags:
	$(TAGS) `find $(DIRS) '(' -name "*.cl" -o -name "*.lisp" ')' -print`

swm-tape:
	tar cf $(DEVICE) `find $(PUBDIRS) '(' -name "*.cl" -o -name "*.lisp" ')' -print`

dist:
	tar -cf -  $(SRC_FILES) | compress >  Dist/src.tar.Z

rcscheck:
	rcscheck $(DIRS) | grep -v .fasl

FORCE:

################## Make-dist stuff

makeclimfasls	: makeclimxmfasls makeclimolfasls
makeclimxmfasls	: compile-xm cat-xm
makeclimolfasls	: compile-ol cat-ol


install_clim	:
	cp $(CLIMOBJS) $(CLIMFASLS) $(DEST)

# Link in the libraries with standard names

link-motif-libraries	:
	ln -s  $(MOTIFLIB) $(DEST)/libXm.a
	ln -s $(XTLIB) $(DEST)/libXt.a
	ln -s $(XLIB) $(DEST)/libX11.a
	ln -s $(FCLIMOBJS) $(DEST)

echo_src_files:
	@ls $(SRC_FILES) | cat
	
makeclimobjs	: $(CLIMOBJS)

################## Lower level Makefile stuff


ol-dcl	:  stub-x.o stub-xt.o stub-olit.o xlibsupport.o MyDrawingA.o
	cd $(CL_SRC) ; /bin/rm -f ucl ;\
	make initial_oldspace=$(OLDSPACE) oldspace=$(OLDSPACE) newspace=$(NEWSPACE) ucl_xtras='$(PWD)/stub-x.o $(PWD)/stub-xt.o $(PWD)/stub-olit.o $(PWD)/xlibsupport.o $(PWD)/MyDrawingA.o $(LIBXOL) $(OLXLIBS)' dcl

xm-dcl	: stub-x.o stub-xt.o stub-motif.o xlibsupport.o MyDrawingA.o
	cd $(CL_SRC) ; /bin/rm -f ucl ;\
	make initial_oldspace=$(OLDSPACE) oldspace=$(OLDSPACE) newspace=$(NEWSPACE) ucl_xtras='$(PWD)/stub-x.o $(PWD)/stub-xt.o $(PWD)/stub-motif.o $(PWD)/xlibsupport.o $(PWD)/MyDrawingA.o $(MOTIFLIB) $(XTLIB) $(XLIB)' dcl	

dcl	: 
	cd $(CL_SRC) ; /bin/rm -f ucl ;\
	make dcl	


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

MyDrawingA.o: misc/MyDrawingA.c
	$(CC) -c $(CFLAGS) -o MyDrawingA.o misc/MyDrawingA.c

FRC	: 

xm-composer : xm-dcl
	cd /usr/composer2 ; make CL=$(CL) rebuild-c2

ol-composer : ol-dcl
	cd /usr/composer2 ; make CL=$(CL) rebuild-c2


