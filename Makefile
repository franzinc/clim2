# $fiHeader: Makefile,v 1.6 92/01/30 10:09:16 cer Exp $

SOMEDIRS=utils silica clim
DIRS=$(SOMEDIRS) xlib tk xm-silica
DEVICE=/dev/null
CL=/net/vapor/usr/composer2/composer2
CLOPTS	= -qq
ECHO	= /bin/echo


default: compile

compile : FRC
	$(ECHO) " \
		(load \"misc/go-xm.cl\")" | $(CL) $(CLOPTS) -batch

clean : FRC
	find $(DIRS)  -name "*.fasl" -exec $(RM) "{}" \;

FRC :


swm-tape:
	tar cf $(DEVICE) `find misc $(SOMEDIRS) '(' -name "*.cl" -o -name "*.lisp" ')' -print`

xm-composer:
	cd tk ; $(MAKE) xm-composer

xm-dcl:
	cd tk ; $(MAKE) xm-dcl

dist	:
	gtar -z -cf - \
	*/*.lisp */*.cl Makefile \
	> Dist/src.tar.Z

rcscheck:
	rcscheck $(DIRS) | grep -v .fasl

# For the day the make dist happens.

echo_src_files:
	@find . '(' -name '*.cl' -o -name '*.lisp' ')' -print


