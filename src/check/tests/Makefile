RESULTS = $(wildcard result*dat)
TESTS   = $(subst result,test,$(RESULTS))

all: $(TESTS)

test1.dat: .FORCE
	@value_check                         \
		-F data/air.2012.nc : air         \
		-S jozefoslaw : 52.0 : 21.0 : 110 \
		-D 20120606 : 30@H                \
		-J100 -w n                        \
		-o $@
	$(call check) 

test2.dat: .FORCE
	@value_check                         \
		-!                                \
		-F data/air.2012.nc : air         \
		-S jozefoslaw : 52.0 : 21.0 : 110 \
		-D 20120606 : 30@H                \
		-J100 -w n > test2.dat
	$(call check) 

test3.dat: .FORCE
	@value_check                         \
		-!                                \
		-F data/air.2012.nc : air         \
		-S jozefoslaw : 52.0 : 21.0 : 110 \
		-D 20120606 : 30@H                \
		-J100 -w n -V $@ -o /dev/null
	$(call check) 

define check
	@diff $@ $@ \
		&& { tput setaf 2 ;  echo "$@ -- passed" ; tput sgr0 ; } \
		|| { tput setaf 1 ;  echo "$@ -- failed" ; tput sgr0 ; } 
endef

.FORCE:
