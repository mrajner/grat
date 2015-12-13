all: gfortran.out ifort.out

test = ./test/tests/t_3_fortran.f90 mod_utilities.f90
OPT = -O0

#  %.o: %.mod
%.out: %  $(test)
	@type $< && { \
	make $(<)/mod_utilities$(OPT).o  FC=$< ; \
	$(<) ./test/tests/t_3_fortran.f90 -I$(<) \
		$(<)/mod_utilities$(OPT).o -o $@ ;\
	./$@  ; \
	} || :

.PHONY: gfortran ifort


xo:
	make B FC=ifort
	make B FC=gfortran
	make B FC=ifort > tifort
	make B FC=gfortran >tgfortran
	vimdiff tgfortran tifort
