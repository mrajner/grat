# this makefile simple link to rules in main Makefile
# in grat/src

FC = ifort
all: example_aggf run 

example_aggf: example_aggf.f90 /usr/local/lib/libgrat.a
	$(FC) $< -lgrat -lnetcdf -lnetcdff -o $@ -openmp

/usr/local/lib/libgrat.a:
	make -C ../src/ install

run:
	@./example_aggf
