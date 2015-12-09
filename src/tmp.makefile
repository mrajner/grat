all: gfortran.out ifort.out
test = ./test/tests/t_3_fortran.f90 mod_utilities.f90
%.out: %  $(test)
	make $(<)/mod_utilities.o  FC=$<
	$(<) ./test/tests/t_3_fortran.f90 -I$(<) \
		$(<)/mod_utilities.o -o $@
	./$@

%.o: %.mod
