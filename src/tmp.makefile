.FORCE:
~/.local/bin/value_check: .FORCE
	ls
	# $(MAKE) PEDANTIC=1 
	@$(MAKE) --no-print-directory
	@sudo make install  --no-print-directory
	cd $$GS/test/tests/ && ./t18.sh
	# ../bin/value_check -F ./test/data/swvl_201802_1_onsala.nc : swvl1 -S onsala -Dm -wn
	# ../bin/value_check  -Il@I -F ./test/data/swvl_201802_1_onsala.nc : swvl1 -S onsala -Dm -wn
