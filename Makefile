# Start with a decision as to whether compiling at a Franz Inc site or not:

HOSTNAME := $(shell hostname -d)
# for testing - uncomment to force a non-Franz situation
#HOSTNAME := foo

ifeq ($(findstring franz.com,$(HOSTNAME)),franz.com)
ATFRANZ = yes
include ../makefile.top
include ../makefile.defs
else
# Be sure to link or copy one of the files in makefile.templates to
# Makefile.nonfranz and modify the paths to the lisp executable and
# image:
include Makefile.nonfranz
endif

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
