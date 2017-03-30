OPT:= -O2

SHELL_TESTS   := $(wildcard t[0-9]*.sh)
#temporary
#  SHELL_TESTS   := $(wildcard t[1-6].sh)

FORTRAN_TESTS := $(wildcard t*.f90)

REAL          := $(subst .sh,.dat.r,$(SHELL_TESTS))
SYNTHETIC     := $(subst .sh,.dat.s,$(SHELL_TESTS))
CONSTANT      := $(subst .sh,.dat.c,$(SHELL_TESTS))
FORTRAN       := $(subst .f90,.dat,$(FORTRAN_TESTS))

RESULTS :=     \
	$(FORTRAN)   \
	$(REAL)      \
	$(SYNTHETIC) \
	$(CONSTANT)  \

all: $(RESULTS)

all_s: $(SYNTHETIC) $(CONSTANT)

stat: $(RESULTS)
	bash stat.sh
	
%.dat:%.sh
	-bash $< 

%.dat:%.f90 
	$(FC) $(OPT) $< -o $(subst .dat,,$@) -L../.. -lgrat -I../..
	./$(subst .dat,,$@) | tee $@

%.dat.s:%.sh
	-time { bash $< s ; }
%.dat.r:%.sh
	-time { bash $< r ; }
%.dat.c:%.sh
	-time { bash $< c ; }

clean:
	rm -v t[_0-9]*.dat* t_fortran

copy_non_existing_dat_to_benchmark_result: 
	@tput setaf 1; echo -n Sure update benchmark results?: ; read  answer ; tput sgr0 ; \
		case $${answer:-} in \
		Y|y) echo yes ; \
		for i in t*.dat* ; do \
		[[ -s $$i && ! -f $${i/t/r} ]] && cp -uv $$i $${i/t/r} || : ; done ; \
		;; \
		esac ;

failed: all
	@bash stat.sh -b

.PHONY:.FORCE
.FORCE:
