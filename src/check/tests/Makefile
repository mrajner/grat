TESTS   = $(wildcard t*.sh)
RESULTS = $(subst .sh,.dat,$(TESTS))

all: stat

stat: $(RESULTS)
	@$(foreach res,$(RESULTS),$(call check,t1.dat,r2.dat))

%.dat:%.sh
	bash $<
	


clean:
	rm t*.dat

define check
@printf "%s\n" "diff $(1) $(2)"
endef

#  .FORCE:
	#  && { tput setaf 2 ;  echo "$@ -- passed" ; tput sgr0 ; } \
	#  || { tput setaf 1 ;  echo "$@ -- failed" ; tput sgr0 ; } 
