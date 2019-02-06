.FORCE:
~/.local/bin/value_check: .FORCE
	$(MAKE) 
	cp -vu ../bin/value_check $@
	cp -vu ../bin/grat $@
	cp -vu ../bin/polygon_check $@
