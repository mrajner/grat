testspeed: .FORCE
	@$(MAKE)
	../bin/grat  -F 10 @SP -Sj -M1

dddx:
	# time ../bin/grat  -F ERA @SP  -D 20140205 -S j -!
	# time ../bin/grat  -F ERA @SP : @ offset=-101325 -D 20140205 -S j -!
	# time ../bin/grat -! -F ERA @SP : @ offset=-101325 -D 20140205 -S j
	# time ../bin/grat  -F   /home/mrajner/dat/erainterim/sp.2014.nc_d6d0 @SP : sp  -D 20140205 -S j -!
	time ../bin/grat  -F   /home/mrajner/dat/erainterim/sp.2014.nc_d6d0 @SP : sp @ offset=-101325 -D 20140205 -S j -!
	# time ../bin/grat -! -F /home/mrajner/dat/erainterim/sp.2014.nc_d6d0 @SP : sp @ offset=-101325 -D 20140205 -S j

.FORCE:
# ~/.local/bin/value_check: .FORCE
# 	# $(MAKE) PEDANTIC=1 
# 	@$(MAKE) --no-print-directory
# 	# sudo make install
# 	# cd $$GS/test/tests/ && bash ./t22.sh
# 	# ../bin/value_check -F ./test/data/swvl_201802_1_onsala.nc : swvl1 -S onsala -Dm -wn
# 	# ../bin/value_check  -Il@I -F ./test/data/swvl_201802_1_onsala.nc : swvl1 -S onsala -Dm -wn
#
all: 
	@$(MAKE) -f Makefile
	$G/bin/grat \
			-S onsala \
			-F /home/mrajner/dat/CopernicusOcean/BAL_all.nc @ EWT: sla \
			-G \
			~/src/gotic2/data/grn1.data@GR : 1 : 4  \
			-M 2D -Dm  \
			-I10@DE -L /tmp/p @ p  -H
	$G/bin/grat \
			-S onsala \
			-F /home/mrajner/dat/CopernicusOcean/BAL_all.nc @ EWT: sla \
			-G \
			~/src/gotic2/data/grn1.data@GR : 1 : 4  \
			-M 2D -Dm  \
			-I1.4@DE -L /tmp/p2 @ p  -H

