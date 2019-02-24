# this makefile simple link to rules in main Makefile
# in grat/src

FC ?= gfortran
all: example_aggf run 

example_aggf: example_aggf.f90
	$(FC) $< -ffree-line-length-none \
		-I/usr/local/include -lgrat \
		-I /home/mrajner/bin/fortran/libmr/ \
		-L /home/mrajner/bin/fortran/libmr/ -lmr \
	 	-lnetcdf -lnetcdff \
		-o $@

/usr/local/lib/libgrat.a:
	make -C ../src/ install

run:
	@./example_aggf
