# this makefile simple link to rules in main Makefile
# in grat/src

FC ?= gfortran
all: example_aggf run 

example_aggf: example_aggf.f90 /usr/local/lib/libgrat.a
	$(FC) $< -I../src/ -lgrat -lnetcdf -lnetcdff -openmp -ffree-line-length-none -o $@

/usr/local/lib/libgrat.a:
	make -C ../src/ install

run:
	@./example_aggf
