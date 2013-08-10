# this makefile simple link to rules in main Makefile
# in grat/src

all: example_aggf run

example_aggf: example_aggf.f90 ../src/mod_atmosphere.f90 ../src/mod_aggf.f90 ../src/mod_green.f90
	@make mod_green.o mod_atmosphere.o mod_aggf.o -C ../src/
	@make ../examples/example_aggf -C ../src/

run:
	@./example_aggf
