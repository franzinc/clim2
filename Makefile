# $Header: /repo/cvs.copy/clim2/Makefile,v 1.81 1997/02/05 01:41:43 tomj Exp $

# If no HOST specified then we'd like to get an error immediately
# rather than ploughing on with an inappropriate default

HOST = unknown
include Makefile.$(HOST)
