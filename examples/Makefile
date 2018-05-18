# this makefile simple link to rules in main Makefile
# in grat/src

FC ?= gfortran
all: example_aggf run 

example_aggf: example_aggf.f90
	$(FC) $< -I/usr/local/include -lgrat -lnetcdf -lnetcdff -o $@ -openmp

/usr/local/lib/libgrat.a:
	make -C ../src/ install

run:
	@./example_aggf
