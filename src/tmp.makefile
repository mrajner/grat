.FORCE:
~/.local/bin/value_check: .FORCE
	# $(MAKE) PEDANTIC=1 
	@$(MAKE) --no-print-directory
	sudo make install
	cd $$GS/test/tests/ && bash ./t20.sh
	# ../bin/value_check -F ./test/data/swvl_201802_1_onsala.nc : swvl1 -S onsala -Dm -wn
	# ../bin/value_check  -Il@I -F ./test/data/swvl_201802_1_onsala.nc : swvl1 -S onsala -Dm -wn
