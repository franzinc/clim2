# $Header: /repo/cvs.copy/clim2/Makefile,v 1.90.2.2.26.1 2002/08/10 14:41:25 duane Exp $

# If ../makefile.top doesn't exist, then you must specify HOST=xxx on the
# `make' command line.

include ../makefile.top
include ../makefile.defs

ifeq ($(OS_NAME),windows)
HOST = windows
endif

ifeq ($(OS_NAME),sunos)
ifeq ($(MACHINE),sparc)
HOST = sun4-svr4
endif
endif

ifeq ($(OS_NAME),linux)
ifeq ($(MACHINE),ppc)
HOST = linuxppc
else
HOST = linux
endif
endif

ifeq ($(OS_NAME),freebsd)
HOST = freebsd
endif

ifeq ($(OS_NAME),darwin)
HOST = macosx
endif

ifeq ($(OS_NAME),hp-ux)
ifeq ($(SIXTYFOURBIT),yes)
HOST = hp64
else
HOST = hpprism
endif
endif

ifeq ($(OS_NAME),irix)
HOST = sgi4d-svr4
endif

ifeq ($(OS_NAME),aix)
ifeq ($(SIXTYFOURBIT),yes)
HOST = power64
else
HOST = rs6000
endif
endif

ifeq ($(OS_NAME),osf1)
ifeq ($(MACHINE),alpha)
HOST = alpha
endif
endif

# If no HOST specified then we'd like to get an error immediately
# rather than ploughing on with an inappropriate default

ifndef HOST
HOST = unknown
endif

include Makefile.$(HOST)
