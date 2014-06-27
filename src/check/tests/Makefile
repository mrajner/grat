TESTS   = $(wildcard t*.sh)
RESULTS = $(subst .sh,.dat,$(TESTS))

all: stat

stat: $(RESULTS)
	@bash stat.sh
	
%.dat:%.sh
	@bash $< 

clean:
	rm -v t*.dat*

copy_dat_to_benchmark: stat
	@tput setaf 1; echo -n Sure update benchmark results?: ; read  answer ; tput sgr0 ; \
		case $${answer:-} in \
		Y|y) echo yes ; \
		for i in t*.dat*; do cp -uv $$i $${i/t/r} ; done ; \
		;; \
		esac ;

failed:

