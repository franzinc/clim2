# $fiHeader: Makefile,v 1.13 92/02/28 09:18:01 cer Exp Locker: cer $
# 
#  Makefile for CLIM 2.0
#
CL	= /net/sparky/usr/tech/jdi/4.1/src/dcl
DUMP-CL	= $(CL)
CLOPTS	= -qq

# Lisp optimization for compiling
SPEED	= 3
SAFETY	= 1

# Name of dumped lisp
CLIM	= ./slim
CLIM-SMALL	= ./slim-small

PUBDIRS	= sys utils silica clim demo test genera clx
DIRS0	=  tk xm-silica misc
DIRS	= $(PUBDIRS) xlib $(DIRS0)
CHEAP_CLEAN	= $(PUBDIRS) $(DIRS0)

DEVICE	= /dev/null
RM	= /bin/rm
CAT	= /bin/cat
ECHO	= /bin/echo
MV	= mv
TAGS	= /usr/fi/lib/emacs/etc/etags
TMP	= /usr/tmp

#
# "Compile time objects" -- these go into clim-debug.fasl
#
DEBUG-OBJS = xlib/xlib.fasl

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
			clim/text-recording.fasl \
			clim/graphics-recording.fasl \
                        clim/interactive-protocol.fasl \
                        clim/input-editor-commands.fasl \
                        clim/formatted-output-defs.fasl \
                        clim/incremental-redisplay.fasl \
                        clim/coordinate-sorted-set.fasl \
                        clim/window-stream.fasl \
                        clim/completer.fasl \
                        clim/ptypes1.fasl \
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
                        clim/menus.fasl \
                        clim/accept-values.fasl \
                        clim/item-list-manager.fasl \
                        clim/pixmap-streams.fasl \
                        clim/stream-trampolines.fasl

XLIB-CLIM-OBJS = xlib/pkg.fasl \
                  xlib/ffi.fasl \
                  xlib/x11-keysyms.fasl \
                  xlib/last.fasl

XT-CLIM-OBJS = tk/pkg.fasl \
                tk/foreign-obj.fasl \
                tk/macros.fasl \
                tk/xlib.fasl \
                tk/font.fasl \
                tk/gcontext.fasl \
                tk/graphics.fasl \
                tk/xtk.fasl \
                tk/meta-tk.fasl \
                tk/make-classes.fasl \
                tk/foreign.fasl \
                tk/widget.fasl \
                tk/resources.fasl \
                tk/event.fasl \
                tk/callbacks.fasl \
                tk/xt-classes.fasl \
                tk/xt-init.fasl

XM-CLIM-OBJS = tk/xm-classes.fasl \
                tk/xm-init.fasl \
                tk/xm-widgets.fasl \
                tk/xm-font-list.fasl \
                tk/xm-protocols.fasl \
                tk/convenience.fasl \
                tk/make-widget.fasl

MOTIF-CLIM-OBJS = xm-silica/pkg.fasl \
                   xm-silica/xt-silica.fasl \
                   xm-silica/xm-silica.fasl \
                   xm-silica/xt-graphics.fasl \
                   xm-silica/xm-graphics.fasl \
                   xm-silica/image.fasl \
                   xm-silica/xt-frames.fasl \
                   xm-silica/xm-frames.fasl \
                   xm-silica/xt-gadgets.fasl \
                   xm-silica/xm-gadgets.fasl \
                   xm-silica/xm-menus.fasl \
                   xm-silica/xt-pixmaps.fasl \
                   xm-silica/xm-cursor.fasl

OL-CLIM-OBJS = tk/ol-classes.fasl \
                tk/ol-init.fasl \
                tk/ol-callbacks.fasl \
                tk/make-widget.fasl

OPENLOOK-CLIM-OBJS = xm-silica/pkg.fasl \
                      xm-silica/xt-silica.fasl \
                      xm-silica/ol-silica.fasl \
                      xm-silica/xt-graphics.fasl \
                      xm-silica/ol-graphics.fasl \
                      xm-silica/image.fasl \
                      xm-silica/xt-frames.fasl \
                      xm-silica/ol-frames.fasl \
                      xm-silica/xt-gadgets.fasl \
                      xm-silica/ol-gadgets.fasl \
                      xm-silica/xt-pixmaps.fasl

MOTIF-OBJS = $(CLIM-UTILS-OBJS) $(CLIM-SILICA-OBJS) $(CLIM-STANDALONE-OBJS) \
	     $(XLIB-CLIM-OBJS) $(XT-CLIM-OBJS) $(XM-CLIM-OBJS) \
	     $(MOTIF-CLIM-OBJS) 

OPENLOOK-OBJS = $(CLIM-UTILS-OBJS) $(CLIM-SILICA-OBJS) \
		$(CLIM-STANDALONE-OBJS) $(XLIB-CLIM-OBJS) $(XT-CLIM-OBJS) \
		$(OL-CLIM-OBJS) $(OPENLOOK-CLIM-OBJS)

default: compile clim

all:	compile cat clim

compile:	FORCE
	$(ECHO) " \
		(setq *ignore-package-name-case* t) \
		(set-case-mode :case-insensitive-lower) \
		(proclaim '(optimize (speed $(SPEED)) (safety $(SAFETY)))) \
		(load \"misc/compile-xm.lisp\")" | $(CL) $(CLOPTS) -batch

clim.fasl:	$(MOTIF-OBJS)
	$(CAT) $(MOTIF-OBJS) > $(TMP)/clim.fasl_`whoami`
	$(MV) $(TMP)/clim.fasl_`whoami` clim.fasl
	ls -lt clim.fasl >> Clim-sizes.n
	ls -lt clim.fasl

echo-fasls:
	ls -lt $(MOTIF-OBJS) > /tmp/foo

clim-debug.fasl:	$(MOTIF-OBJS)
	$(CAT) $(DEBUG-OBJS) > $(TMP)/clim-debug.fasl_`whoami`
	($(MV) $(TMP)/clim-debug.fasl_`whoami` clim-debug.fasl

cat:	clim.fasl clim-debug.fasl

clim:	FORCE
	-$(RM) $(CLIM)
	$(ECHO) " \
		(setq *ignore-package-name-case* t) \
		(set-case-mode :case-insensitive-lower) \
		(load \"misc/dev-load-xm.lisp\") \
		(load \"misc/dump.lisp\")" | $(DUMP-CL) $(CLOPTS) -batch
	$(MV) $(TMP)/clim.temp_`whoami` $(CLIM)
	ls -lt $(CLIM) >> Clim-sizes.n
	ls -lt $(CLIM)

clim-small:	FORCE
	$(ECHO) " \
		(setq *ignore-package-name-case* t) \
		(set-case-mode :case-insensitive-lower) \
		(load \"misc/load-xm.lisp\") \
		(load \"misc/dump.lisp\")" | $(DUMP-CL) $(CLOPTS) -batch
	$(MV) $(TMP)/clim.temp_`whoami` $(CLIM-SMALL)
	ls -lt $(CLIM-SMALL) >> Clim-sizes.n
	ls -lt $(CLIM-SMALL)

xm-composer:
	cd tk ; $(MAKE) xm-composer

xm-dcl:
	cd tk ; $(MAKE) xm-dcl

clean:
	find $(DIRS) -name "*.fasl" -print | xargs rm -f ; rm -f clim.fasl

cheapclean:
	find $(CHEAP_CLEAN) -name "*.fasl" -print | xargs rm -f


tags:
	$(TAGS) `find $(DIRS) '(' -name "*.cl" -o -name "*.lisp" ')' -print`

swm-tape:
	tar cf $(DEVICE) `find $(PUBDIRS) '(' -name "*.cl" -o -name "*.lisp" ')' -print`

dist:
	tar -cf - \
	*/*.lisp *.lisp Makefile */Makefile \
	| compress >  Dist/src.tar.Z

rcscheck:
	rcscheck $(DIRS) | grep -v .fasl

# For the day the make dist happens.
echo_src_files:
	@find . '(' -name '*.cl' -o -name '*.lisp' ')' -print

FORCE:

