TESTS   = $(wildcard t*.sh)
RESULTS = $(subst .sh,.dat,$(TESTS))

all: stat

stat: $(RESULTS)
	@bash stat.sh
	

%.dat:%.sh
	@bash $<

clean:
	rm t*.dat


copy_dat_to_benchmark:
	for i in t*.dat; do mv $$i $${i/t/r} ; done

