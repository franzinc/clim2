DIRS= . 

clean : FRC
	find $(DIRS)  -name "*.fasl" -exec $(RM) "{}" \;

FRC :
