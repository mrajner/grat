# this makefile simple link to rules in main Makefile
# in grat/src

all: example_aggf run

example_aggf: example_aggf.f90 ../src/mod_aggf.o
	make ../examples/example_aggf -C ../src/

run:
	./example_aggf
