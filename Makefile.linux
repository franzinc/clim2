
include Makefile.defs

Makefile=Makefile.linux

ifeq ($(shell if test -d /usr/include/openmotif; then echo yes; fi),yes)
XINCLUDES = -I/usr/include/openmotif
XLIBDIR   = /usr/lib/openmotif
endif

# This is the old location, but include it here, just in case 
ifeq ($(shell if test -d /usr/X11R6/include; then echo yes; fi),yes)
XINCLUDES = -I/usr/X11R6/include
XLIBDIR   = /usr/X11R6/lib
endif

XINCLUDES ?= /usr/include
XLIBDIR   ?= /usr/lib

TKLIB=-lXm -lXpm -lXext -lXp
XTLIB=-lXt
XLIB=-lX11

ifdef FI_USE_DMALLOC
THREADLIB = -lpthread -ldmallocth
CFLAGS = -I/usr/local/include
else
THREADLIB = -lpthread
endif

SET_LIBRARY_PATH = LD_RUN_PATH=$(XLIBDIR):/lib:/usr/lib; export LD_RUN_PATH

PRODUCT-OBJS= $(PRODUCT-GENERIC-OBJS) $(STATIC-XM-OBJS) $(SHARED-XM-OBJS)

#MOTIFXTRAS=-lgen

#PICFLAGS = -K pic
SHAREFLAGS = 
MAKE_SHARED = ld -shared -L$(XLIBDIR)
STD_DEFINES = -DSVR4 -DSYSV
AR = ar cq

include Makefile.generic
