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
ifeq ($(MACHINE),x86_64)
HOST = solamd64
endif
endif

ifeq ($(OS_NAME),linux)
ifeq ($(MACHINE),ppc)
HOST = linuxppc
else
ifeq ($(MACHINE),arm64)
HOST = arm64
else
ifeq ($(SIXTYFOURBIT),yes)
HOST = amd64
else
HOST = linux
endif
endif
endif
endif

ifeq ($(OS_NAME),freebsd)
HOST = freebsd
endif

ifeq ($(OS_NAME),darwin)
ifeq ($(MACHINE),arm64)
HOST = macarm
else
HOST = macosx
endif
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
