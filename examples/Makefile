# this makefile simple link to rules in main Makefile
# in grat/src

FC := $(FC)
all: example_aggf run 

example_aggf: example_aggf.f90 /usr/local/lib/libgrat.a
	$(FC) $< -I/usr/local/include -lgrat -lnetcdf -lnetcdff -o $@ -openmp -ffree-line-length-none

/usr/local/lib/libgrat.a:
	make -C ../src/ install

run:
	@./example_aggf
