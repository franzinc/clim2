SOMEDIRS=utils silica clim ws xm-silica
DIRS=$(SOMEDIRS) xlib tk
DEVICE=/dev/null
CL=/usr/composer2/composer2

default: compile

compile : FRC
	$(CL) < misc/go.cl

clean : FRC
	find $(DIRS)  -name "*.fasl" -exec $(RM) "{}" \;

FRC :


swm-tape:
	tar cf $(DEVICE) `find misc $(SOMEDIRS) '(' -name "*.cl" -o -name "*.lisp" ')' -print`

xm-composer:
	cd tk ; $(MAKE) xm-composer

xm-dcl:
	cd tk ; $(MAKE) xm-dcl

# For the day the make dist happens.

echo_src_files:
	@find . '(' -name '*.cl' -o -name '*.lisp' ')' -print

