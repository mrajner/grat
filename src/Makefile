tmp:tmp.f90 .FORCE
	make libgrat.a NODEP="D"
	$(FC) $< -o $@ -L. -lgrat -I. -lnetcdff -lgrat
	./$@

fdf: all
	value_check -fdfsdf -Sj -F10
	value_check fdfsdf -Sj -F10
	grat -fdfsdf -Sj -F10@SP
	grat fdfsdf -Sj -F10@SP

gfortran:
	make grat FC=gfortran
g95:
	make grat FC=g95
dfd: grat
	grat -o /dev/null -Sj -F10@SP -D2011:20110102 -L/dev/null@p, fdf@s
dgrat: grat value_check
	grat -S o      \
		-M1,2        \
		-F NCEP@SP , \
		NCEP1 @T ,   \
		NCEP@H,      \
		NCEP@HP ,    \
		NCEP @LS     \
	-BN,I          \
	-G@GN,@GE , @GNc      \
	-H -I-2@DD : 0.21@DB:140@DE  -D2012  \
	-o aa 


# user definitions
LIBDIR    := /usr/local/lib
INCDIR    := /usr/local/include
BINDIR    := /usr/local/bin
LINKS_DIR := ~/.local/bin

# compiler:  currently only ifort is supported
FC      := ifort
#  FC      := gfortran

FC_INFO := $(shell $(FC) --version | sed -n 1p)
HOST    := $(shell hostname)
DATE    := $(shell date "+%Y-%m-%d %H:%M:%S")

VERSION := $(shell git describe --match "v*")
VERSION_MAJOR := $(shell awk '{gsub(/(v*|\..*)/,"");print}' <<< $(VERSION))

LIBGRAT := /usr/local/include/libgrat$(VERSION_MAJOR).a

ifeq ($(FC),ifort)
FFLAGS :=          \
				 -fpic     \
				 -O2       \
				 -xHost    \
				 -warn all

NETCDFf := -lnetcdff
endif

ifeq ($(FC),gfortran)
FFLAGS :=          \
				 -O2       
NETCDFf := -L /usr/local/netcdff_gfortran/lib64 -lnetcdff -I/usr/local/netcdff_gfortran/include
endif

FPP :=                         \
	-fpp                         \
	-D__FFLAGS__='"$(FFLAGS)"'   \
	-D__CDATE__='"$(DATE)"'      \
	-D__VERSION__='"$(VERSION)"' \
	-D__COMPILER__='"$(FC_INFO)"' 

bug1:
	grat -F 10 @SP -Sj -M1 -wn
#  tm: value_check
	#  time \
		#  { value_check -q -F  check/data/air.2012.nc@SP : air  -Sg:10 -wn -Dm:3@D -J1000,950  -otmp:level -G@GN ; }

#  ra: grat  value_check
	#  time \
		#  { grat -F  check/data/air.2012.nc@SP : air  -M2,1 -wn,t -Sj,r,o:g -V -Dm:3@D -J1000,950  -otmp:level -G@GN ; }


#  local:
	#  file=$$(basename $$(ls /home/mrajner/dat/ispd/hourly/___*WARSZAWA*));                               \
	#  grep $$file /home/mrajner/dat/ispd/hourly/sites.sta |awk '{print "aaa" , $$2, $$3}' > local_site  ; \
	#  cat /home/mrajner/dat/ispd/hourly/$${file} | awk '{print $$1, $$3}' > local

SRCS      := \
						mod_constants.f90     \
						mod_utilities.f90     \
						mod_cmdline.f90       \
						mod_printing.f90      \
						mod_spherical.f90     \
						mod_mjd.f90           \
						mod_normalization.f90 \
						mod_atmosphere.f90    \
						mod_aggf.f90          \
						mod_polygon.f90       \
						mod_data.f90          \
						mod_date.f90          \
						mod_site.f90          \
						mod_3d.f90            \
						mod_green.f90         \
						mod_admit.f90         \
						mod_parser.f90        

PROGSSRCS :=                  \
						grat.f90          \
						value_check.f90   \
						polygon_check.f90

OBJS            := $(SRCS:.f90=.o)
MOD             := $(SRCS:.f90=.mod)
PROGS_DIR       := ../bin
PROGS           := $(addprefix $(PROGS_DIR)/,$(PROGSSRCS:.f90=))
PROGS_SHORTCUTS := $(PROGSSRCS:.f90=)

	


all: libgrat.a $(PROGS)
clean:
	rm -f *.o *.mod *.a fort?????? $(PROGS_DIR)/* lib*.{a,so}
	
# local installation of executables
LINKS = $(addprefix $(LINKS_DIR)/,$(PROGSSRCS:.f90=))
$(LINKS_DIR)/%: $(PROGS_DIR)/%
	ln -sf $(realpath $<) $@
link: $(LINKS)
unlink: 
	-rm -f $(LINKS)

install:                                     \
	$(LIBDIR)/libgrat$(VERSION_MAJOR).a        \
	$(LIBDIR)/libgrat.a                        \
	$(addprefix $(BINDIR)/,$(PROGS_SHORTCUTS)) \
	$(addprefix $(INCDIR)/,$(MOD))
	
uninstall: 
	-rm -f                                       \
		$(addprefix $(INCDIR)/,$(MOD))             \
		$(addprefix $(BINDIR)/,$(PROGS_SHORTCUTS)) \
		/usr/local/lib/libgrat*      

$(LIBDIR)/libgrat$(VERSION_MAJOR).a: libgrat$(VERSION_MAJOR).a
	cp -uv $< $@
$(LIBDIR)/libgrat.a: $(LIBDIR)/libgrat$(VERSION_MAJOR).a
	ln -sf $< $@
$(INCDIR)/%.mod: %.mod
	cp -uv $< $@
$(BINDIR)/%: $(PROGS_DIR)/%
	cp -uv $< $@

%.o: %.f90
	$(FC) $(FFLAGS) $< -c

.PHONY: $(PROGS_SHORTCUTS)
$(PROGS_SHORTCUTS):
	@make --no-print-directory $(addprefix $(PROGS_DIR)/,$@)

$(PROGS_DIR)/%:%.f90 libgrat.a
	$(FC) $(FFLAGS) $(FPP)          \
	$< -o $@ -L. -lgrat $(NETCDFF)

libgrat.a: libgrat$(VERSION_MAJOR).a
	ln -sf $(realpath $<) $@

libgrat$(VERSION_MAJOR).a: $(OBJS)
	ar rusv $@ $(OBJS)

.PHONY: install 
test: all install
	make -C test

shared_libraries: $(OBJS)
	$(FC) -shared $(OBJS) -o libgrat$(VERSION_MAJOR).so
install_shared)library:
	cp -uv libgrat.so /usr/local/lib

# module dependencies
ifndef NODEP
include mod_dep.makefile
mod_dep.makefile: mod_dep.awk
	awk -f $< > $@
endif


# help files
HELP_NAMES=grat.hlp polygon_check.hlp value_check.hlp
HELPS=$(addprefix ../dat/, $(HELP_NAMES))
helps: $(HELPS)
$(HELPS): $(PROGS)
%.hlp:
	$(patsubst ../dat%,../bin%,$(@:.hlp=)) -h  > $@

.FORCE:


# documentation
dochtml: doc
	firefox ../doc/html/index.html &
doc:
	make -C ../doc/figures/
	cd ../doc  &&  doxygen Doxyfile
docpdf: doc
	cd ../doc/latex/  &&  pdflatex -shell-escape refman
	evince ../doc/latex/refman.pdf &
install-doc: doc
	cp -vur ../doc/man/man3 /home/mrajner/.local/share/man/ 


