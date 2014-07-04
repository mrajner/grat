TESTS   = $(wildcard t2*.sh)

REAL      = $(subst .sh,.dat.r,$(TESTS))
SYNTHETIC = $(subst .sh,.dat.s,$(TESTS))
CONSTANT  = $(subst .sh,.dat.c,$(TESTS))


RESULTS = $(REAL) $(SYNTHETIC) $(CONSTANT)

all: $(RESULTS)
all_s: $(SYNTHETIC) $(CONSTANT)

stat: $(RESULTS)
	@bash stat.sh
	
%.dat:%.sh
	-@bash $< 

%.dat.s:%.sh
	-bash $< s
%.dat.r:%.sh
	-bash $< r
%.dat.c:%.sh
	-bash $< c

clean:
	rm -v t*.dat*

copy_dat_to_benchmark: stat
	@tput setaf 1; echo -n Sure update benchmark results?: ; read  answer ; tput sgr0 ; \
		case $${answer:-} in \
		Y|y) echo yes ; \
		for i in t*.dat*; do cp -uv $$i $${i/t/r} ; done ; \
		;; \
		esac ;

failed: all
	@bash stat.sh -b

