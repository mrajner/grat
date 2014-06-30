# compiler:  currently only ifort is supported
FC     := ifort
#  FC   := gfortran

HOST   := $(shell hostname)
C_DATE := $(shell date "+%Y-%m-%d %H:%M:%S")


COMMON_FFLAGS = \
								-fpic  \
								-O2    \
								-xHost \
								-warn all

FFLAGS=$(COMMON_FFLAGS)

mod_printing.o:                     \
	FFLAGS +=                         \
	-fpp                              \
	-D__FFLAGS__='"$(COMMON_FFLAGS)"' \
	-D__C_DATE__='"$(C_DATE)"'

ifeq ($(FC),gfortran)
FFLAGS=
endif

va: value_check
	value_check -F check/data/test_data.nc:gp -V -Sj,r,o -Dm:m -J1000,950  -o:level

fd:

#  local:
	#  file=$$(basename $$(ls /home/mrajner/dat/ispd/hourly/___*WARSZAWA*));                               \
	#  grep $$file /home/mrajner/dat/ispd/hourly/sites.sta |awk '{print "aaa" , $$2, $$3}' > local_site  ; \
	#  cat /home/mrajner/dat/ispd/hourly/$${file} | awk '{print $$1, $$3}' > local

NETCDFLIB = \
						-I/usr/local/include \
						-L/usr/local/lib     \
						-lnetcdff            \
						-lnetcdf

SRCS      = \
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

PROGSSRCS = \
						grat.f90          \
						value_check.f90   \
						polygon_check.f90

EXAMPLESSRC = example_aggf.f90

OBJS = $(SRCS:.f90=.o)

all: $(PROGS)

PROGS_DIR       = ../bin
PROGS           = $(addprefix $(PROGS_DIR)/,$(PROGSSRCS:.f90=))
PROGS_SHORTCUTS = $(PROGSSRCS:.f90=)

# change to your path directory
LINKS_DIR     = /home/mrajner/.local/bin
LINKS_DIR_ALL = /usr/local/bin
LINKS         = $(addprefix $(LINKS_DIR)/,$(PROGSSRCS:.f90=))

EXAMPLES_DIR = ../examples
EXAMPLES     = $(addprefix $(EXAMPLES_DIR)/,$(EXAMPLESSRC:.f90=))

# implicit rules for object files
%.o: %.f90
	$(FC) $(FFLAGS) $< -c

.PHONY: $(PROGS_SHORTCUTS)

$(PROGS_SHORTCUTS):
	@make --no-print-directory $(addprefix $(PROGS_DIR)/,$@)


$(PROGS_DIR)/%:%.f90 $(OBJS)
	$(FC) $(FFLAGS) $(NETCDFLIB) $< -o $@ $(OBJS)

# rules for example files
$(EXAMPLES_DIR)/example%: $(EXAMPLES_DIR)/example%.f90 $(OBJS)
	$(FC) $(FFLAGS) $< -o $@ $(OBJS) $(NETCDFLIB)

all:          \
	$(OBJS)     \
	$(PROGS)    \
	$(LINKS)    \
	$(EXAMPLES)

libgrat.a: $(OBJS)
	ar rs $@ $(OBJS)
	ranlib $@

PACKAGE = grat-src
VER     = pre-alpha
DATE    = `date +%Y%m%d`
package:
	tar jcf $(PACKAGE)_$(VER)_$(DATE).tar.bz2 \
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

docman: doc
	man /home/mrajner/src/grat/doc/man/man3/mod_utilities.3

install:                   \
	$(PROGS)                 \
	/usr/local/lib/libgrat.a \
	$(LINKS_DIR_ALL)/grat    \
	$(LINKS_DIR_ALL)/value_check

install-doc: doc
	cp -vu ../doc/man/man3 ~/.local/share/man/ -r

# Change this path according to your system settings
# Make sure that target path is in your system serch $PATH
$(LINKS_DIR)/%: $(PROGS_DIR)/%
	ln -sf $(realpath $<) $@

$(LINKS_DIR_ALL)/%: $(PROGS_DIR)/%
	sudo cp -uv $(realpath $<) $@


# Change this path according to your system settings
# This could require root privilages
/usr/local/lib/libgrat.a: libgrat.a
	sudo cp -uv libgrat.a /usr/local/lib
	sudo cp -uv *.mod /usr/local/include
	@echo "library grat sucessfully installed"

clean:
	rm -f *.o *.mod *.a fort?????? 

CLEAN: clean
	-sudo rm \
		/usr/local/include/mod_* \
		/usr/local/lib/libgrat.a
	-rm $(LINKS)
	-sudo rm                       \
		$(LINKS_DIR_ALL)/grat        \
		$(LINKS_DIR_ALL)/value_check
	-rm $(EXAMPLES)

HELP_NAMES=grat.hlp polygon_check.hlp value_check.hlp
HELPS=$(addprefix ../dat/, $(HELP_NAMES))
helps: $(HELPS)
$(HELPS): $(PROGS)
%.hlp:
	$(patsubst ../dat%,../bin%,$(@:.hlp=)) -h  > $@

.FORCE:

.PHONY: check 
check: install
	make -C check

shared_libraries: $(OBJS)
	$(FC) -shared $(OBJS) -o libgrat.so
	$(FC) -c grat.f90
	$(FC) grat.o $(NETCDFLIB) libgrat.so 
	sudo cp -uv libgrat.so /usr/local/lib
clean_shared_libraries:
	sudo rm /usr/local/lib/libgrat.so 
