# $fiHeader: Makefile,v 1.79 1994/12/04 23:56:21 colin Exp $

# If no HOST specified then we'd like to get an error immediately
# rather than ploughing on with an inappropriate default

HOST = unknown
include Makefile.$(HOST)
