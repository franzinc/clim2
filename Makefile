# $fiHeader: Makefile,v 1.78 1994/06/08 06:56:21 duane Exp $

# If no HOST specified then we'd like to get an error immediately
# rather than ploughing on with an inappropriate default

HOST = unknown
stubs =

include Makefile.$(HOST)
