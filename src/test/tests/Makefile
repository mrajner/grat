FC = ifort

#  aaa:
	#  @ifort -O0 tmp.f90 -o tmp-O0
	#  @./tmp-O0
	#  @ifort -O2 tmp.f90 -o tmp-O2
	#  @./tmp-O2

tmp: O2 O0
	-diff a b
	#  paste a b | awk '{print $$0,$$1-$$2}'

O0:
	make -C $(GS) libgrat_ifort-$@.a OPT=-$@
	$(FC) -$@ t_aggf.f90                               \
		/home/mrajner_slow/src/grat/src/libgrat_ifort-O0.a \
		-o t_aggf_00
	time ./t_aggf_00 > a
O2:
	make -C $(GS) libgrat_ifort-$@.a OPT=-$@
	$(FC) -$@ t_aggf.f90                               \
		/home/mrajner_slow/src/grat/src/libgrat_ifort-O2.a \
		-o t_aggf_02
	time ./t_aggf_02 > b

FC := ifort
#  FC := gfortran
OPT:= -O2

ifeq ($(shell hostname),lidka)
	FC := gfortran
endif

ifeq ($(FC),gfortran)
LD_LIBRARY_PATH := /home/mrajner/src/netcdf-gfortran/fortran
endif

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
	$(FC) $(OPT) $< -o $(subst .dat,,$@) -L../.. -lgrat -I../../
	./$(subst .dat,,$@) | tee $@

%.dat.s:%.sh
	-time { bash $< s ; }
%.dat.r:%.sh
	-time { bash $< r ; }
%.dat.c:%.sh
	-time { bash $< c ; }

clean:
	rm -v t[_0-9]*.dat* t_fortran

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
