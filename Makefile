# $Header: /repo/cvs.copy/clim2/Makefile,v 1.84.2.2 1998/04/16 18:55:44 layer Exp $

# If ../makefile.top doesn't exist, then you must specify HOST=xxx on the
# `make' command line.

include ../makefile.top

ifeq ($(OS_NAME),sunos)
ifeq ($(MACHINE),sparc)
HOST = sun4-svr4
endif
endif

ifeq ($(OS_NAME),hp-ux)
HOST = hpprism
endif

ifeq ($(OS_NAME),irix)
HOST = sgi4d-svr4
endif

ifeq ($(OS_NAME),aix)
HOST = rs6000
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
