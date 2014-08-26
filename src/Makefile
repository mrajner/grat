# user definitions
LIBDIR := /usr/local/lib
INCDIR := /usr/local/include

# compiler:  currently only ifort is supported
FC      := ifort

HOST    := $(shell hostname)
C_DATE  := $(shell date "+%Y-%m-%d %H:%M:%S")

VERSION := $(shell git describe --abbrev=0 --match "v*")
VERSION_MAJOR := $(shell awk '{gsub(/(v*|\..*)/,"");print}' <<< $(VERSION))
VERSION_MINOR := $(shell awk '{gsub(/(v.*\.)/,"");print}' <<< $(VERSION))

LIBGRAT := /usr/local/include/libgrat$(VERSION_MAJOR).a

COMMON_FFLAGS =           \
								-fpic     \
								-O2       \
								-xHost    \
								-warn all

FFLAGS=$(COMMON_FFLAGS)

#  tmp:tmp.f90 .FORCE
	#  ifort $< -o $@  -L. -lgrat
	#  ./tmp

#  dfd:
#  fds:
	#  echo $(VERSION) $(VERSION_MAJOR) $(VERSION_MINOR)

#  dfsf:all
#  dfs: grat
	#  grat -v
#  ddfff:buggy


#  ifeq ($(FC),gfortran)
#  FFLAGS=
#  endif


#  tm: value_check
	#  time \
		#  { value_check -q -F  check/data/air.2012.nc@SP : air  -Sg:10 -wn -Dm:3@D -J1000,950  -otmp:level -G@GN ; }

#  buggy: grat  value_check
		#  grat -F  check/data/air.2012.nc@SP : air  -M2,1 -wn,t -Sj,r,o:g -V -Dm:3@D -J1000,950  -otmp:level -G@GN 

#  ra: grat  value_check
	#  time \
		#  { grat -F  check/data/air.2012.nc@SP : air  -M2,1 -wn,t -Sj,r,o:g -V -Dm:3@D -J1000,950  -otmp:level -G@GN ; }

#  fd:

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

# change to your path directory
LINKS_DIR     = ~/.local/bin


dsfsfsdf:
	rm -f mod_constants.o mod_parser.o mod_parser.mod
	@make --no-print-directory mod_parser.o 
	
dfdf:install
all: libgrat.a $(PROGS)

# local installation of executables
LINKS = $(addprefix $(LINKS_DIR)/,$(PROGSSRCS:.f90=))
link: $(LINKS)
	
install: \
	all \
	$(LIBDIR)/libgrat$(VERSION_MAJOR).a \
	$(LIBDIR)/libgrat.a \
	$(addprefix $(INCDIR)/,$(MOD))
	
$(LIBDIR)/libgrat$(VERSION_MAJOR).a: libgrat$(VERSION_MAJOR).a
	sudo cp -uv $< $@

$(LIBDIR)/libgrat.a: $(LIBDIR)/libgrat$(VERSION_MAJOR).a
	sudo ln -sf $< $@

$(INCDIR)/%.mod: %.mod
	sudo cp -uv $< $@

# implicit rules for object files
%.o: %.f90
	$(FC) $(FFLAGS) $< -c

.PHONY: $(PROGS_SHORTCUTS)
$(PROGS_SHORTCUTS):
	@make --no-print-directory $(addprefix $(PROGS_DIR)/,$@)

$(PROGS_DIR)/grat:                  \
	FFLAGS +=                         \
	-fpp                              \
	-D__FFLAGS__='"$(COMMON_FFLAGS)"' \
	-D__C_DATE__='"$(C_DATE)"'        \
	-D__VERSION__='"$(VERSION)"'

$(PROGS_DIR)/%:%.f90 libgrat.a
	$(FC) $(FFLAGS) $< -o $@ -L. -lgrat -lnetcdff

libgrat.a: libgrat$(VERSION_MAJOR).a
	ln -sf $(realpath $<) $@

libgrat$(VERSION_MAJOR).a: $(OBJS)
	ar rusv $@ $(OBJS)

PACKAGE = grat-src
DATE    = `date +%Y%m%d`
package:
	tar jcf $(PACKAGE)_$(VERSION)_$(DATE).tar.bz2 \
		../../grat/src/{*.f90,Makefile}         \
		../../grat/dat/*.dat                    \
		../../grat/bin/.README

doc:
	make -C ../doc/figures/
	cd ../doc  &&  doxygen Doxyfile

docpdf: doc
	cd ../doc/latex/  &&  pdflatex -shell-escape refman
	evince ../doc/latex/refman.pdf &

dochtml: doc
	firefox ../doc/html/index.html &


install-doc: doc
	cp -vur ../doc/man/man3 /home/mrajner/.local/share/man/ 

$(LINKS_DIR)/%: $(PROGS_DIR)/%
	ln -sf $(realpath $<) $@

clean:
	rm -f *.o *.mod *.a fort?????? $(PROGS_DIR)/* lib*.{a,so}

uninstall: 
	-sudo rm                   \
	$(addprefix $(INCDIR)/,$(MOD)) \
		/usr/local/lib/libgrat.a \
		/usr/local/lib/libgrat.so
	#  -rm $(LINKS)
	#  -sudo rm                       \
		#  $(LINKS_DIR)/grat        \
		#  $(LINKS_DIR)/value_check

#  HELP_NAMES=grat.hlp polygon_check.hlp value_check.hlp
#  HELPS=$(addprefix ../dat/, $(HELP_NAMES))
#  helps: $(HELPS)
#  $(HELPS): $(PROGS)
#  %.hlp:
	#  $(patsubst ../dat%,../bin%,$(@:.hlp=)) -h  > $@

.FORCE:

ccc:
#  .PHONY: check 
#  test: install
	#  make -C test

shared_libraries: $(OBJS)
	$(FC) -shared $(OBJS) -o libgrat.so
	@sudo cp -uv libgrat.so /usr/local/lib

#  clean_shared_libraries:
	#  sudo rm /usr/local/lib/libgrat.so 
	#
mod_parser.o: mod_constants.o
