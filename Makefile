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
