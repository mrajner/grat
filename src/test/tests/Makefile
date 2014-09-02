FC := ifort

SHELL_TESTS   := $(wildcard t6*.sh)
FORTRAN_TESTS := t_fortran.f90
BUGS_TESTS     := t_bugs.sh

REAL      := $(subst .sh,.dat.r,$(SHELL_TESTS))
SYNTHETIC := $(subst .sh,.dat.s,$(SHELL_TESTS))
CONSTANT  := $(subst .sh,.dat.c,$(SHELL_TESTS))
FORTRAN   := $(subst .f90,.dat,$(FORTRAN_TESTS))
BUGS      := $(subst .sh,.dat,$(BUGS_TESTS))

RESULTS :=     \
	$(REAL)      \
	$(SYNTHETIC) \
	$(CONSTANT)  \
	#  $(FORTRAN)   \
	#  $(BUGS)

all: $(RESULTS)

../../libgrat.a:
	make -C ../../ libgrat.a NODEP="yes" FC=$(FC)

all_s: $(SYNTHETIC) $(CONSTANT)

stat: $(RESULTS)
	bash stat.sh
	
%.dat:%.sh
	bash $< 

%.dat:%.f90 ../../libgrat.a
	$(FC) $< -o $(subst .dat,,$@) -L../.. -lgrat -I../../
	./$(subst .dat,,$@) > $@

%.dat.s:%.sh
	bash $< s
%.dat.r:%.sh
	bash $< r
%.dat.c:%.sh
	bash $< c

clean:
	rm -v t*.dat* t_fortran

copy_dat_to_benchmark: 
	@tput setaf 1; echo -n Sure update benchmark results?: ; read  answer ; tput sgr0 ; \
		case $${answer:-} in \
		Y|y) echo yes ; \
		for i in t*.dat* ; do [[ -s $$i ]] && cp -uv $$i $${i/t/r} ; done ; \
		;; \
		esac ;

failed: all
	@bash stat.sh -b

.PHONY:.FORCE
.FORCE:
