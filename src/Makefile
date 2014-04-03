# compiler:  currently only ifort is supported
FC     := ifort
#  FC   := gfortran
DEP    := n
HOST   := $(shell hostname)
C_DATE := $(shell date "+%Y-%m-%d %H:%M:%S")

FLAGS =           \
			  -xHost    \
			  -fpp      \
			  -warn all

FFLAGS =                           \
				 $(FLAGS)                  \
				 -D__FFLAGS__='"$(FLAGS)"' \
				 -D__C_DATE__='"$(C_DATE)"'

ifeq ($(FC),gfortran)
FFLAGS=
endif

ifeq ($(HOST),grat)
# always optimize for server and resolve dependencies
FLAGS += -O2
DEP    = y
else
# for testing do not optimize
FLAGS += -O0
# for final compilation do optimize
all: FLAGS += -O2
endif

dfdfd: grat
	grat  -I -FNCEP@SP -D2010 -S j -H
	grat  -I -FNCEP@SP -D2010 -S j -H -A1,2,3,

fsdfs:test
	./test -F ERA@SP -Sj --version -v


check3d: grat
	/home/mrajner/dr/rysunki/3d_vs_DE.sh -w 3d_ERA_1000_60000_2____potentialHSP -v -f -o

fsdfds: grat
	for ref in "" ref ; do            \
	time grat                         \
	  -! \
		-F                              \
		NCEP @SP,                       \
		VIENNA@RSP,                     \
		NCEP1@T,                        \
		NCEP@HP,                        \
		ETOPO@H!,                       \
		VIENNA@HRSP,                    \
		NCEP@GP!,                       \
		NCEP@VT!,                       \
		NCEP1@VSH!                      \
		-M1,2,3:potenial:$${ref}        \
		-G@GN, @GNdt,@GNdz,@GNdz2       \
		-S j                            \
		-H                              \
		-D 2011                         \
	 	-o                              \
		-A-0.43 -U                      \
		-I0.03@3D :-10@DD: 20110@HB: 50000 @HS \
		 -o tmp ;                           \
		done

tmp: grat local
	grat                      \
		-F NCEP@SP, VIENNA@RSP  \
		-S j \
		-H                      \
		-D 2009:20090102        \
		-M1,2 -G@GN             \
		-o :free

local:
	file=$$(basename $$(ls /home/mrajner/dat/ispd/hourly/___*WARSZAWA*));                               \
	grep $$file /home/mrajner/dat/ispd/hourly/sites.sta |awk '{print "aaa" , $$2, $$3}' > local_site  ; \
	cat /home/mrajner/dat/ispd/hourly/$${file} | awk '{print $$1, $$3}' > local


# libraries
NETCDFLIB = -I/usr/local/include \
						-L/usr/local/lib     \
						-lnetcdff            \
						-lnetcdf

SRCS  = mod_constants.f90     \
				mod_utilities.f90     \
				mod_printing.f90      \
				mod_cmdline.f90       \
				mod_site.f90          \
				mod_spherical.f90     \
				mod_mjd.f90           \
				mod_atmosphere.f90    \
				mod_aggf.f90          \
				mod_green.f90         \
				mod_polygon.f90       \
				mod_data.f90          \
				mod_date.f90          \
				mod_parser.f90        \
				mod_admit.f90         \
				mod_normalization.f90 \
				mod_3d.f90            \

PROGSSRCS =  grat.f90          \
						 value_check.f90   \
						 polygon_check.f90

EXAMPLESSRC = example_aggf.f90

OBJS = $(SRCS:.f90=.o)

# module dependencies
ifeq ($(DEP),y)
mod_utilities.o:     mod_constants.o
mod_mjd.o:           mod_constants.o
mod_cmdline.o:       mod_utilities.o
mod_normalization.o: mod_utilities.o
mod_printing.o:      mod_cmdline.o
mod_polygon.o:       mod_printing.o
mod_atmosphere.o:    mod_printing.o
mod_aggf.o:          mod_atmosphere.o mod_normalization.o
mod_site.o:          mod_printing.o  mod_utilities.o  mod_constants.o     mod_data.o      mod_cmdline.o  mod_date.o
mod_parser.o:        mod_cmdline.o   mod_data.o       mod_polygon.o       mod_green.o     mod_admit.o    mod_date.o
mod_green.o:         mod_data.o mod_constants.o      mod_polygon.o    mod_date.o          mod_spherical.o mod_site.o     mod_normalization.o mod_aggf.o mod_3d.o mod_data.o
mod_data.o:          mod_mjd.o       mod_atmosphere.o mod_polygon.o       mod_utilities.o mod_printing.o
mod_date.o:          mod_data.o
endif


PROGS_DIR=../bin
PROGS  = $(addprefix $(PROGS_DIR)/,$(PROGSSRCS:.f90=))
PROGS_SHORTCUTS  = $(PROGSSRCS:.f90=)

# change to your path directory
LINKS_DIR=/home/mrajner/.local/bin
LINKS_DIR_ALL=/usr/local/bin
LINKS  = $(addprefix $(LINKS_DIR)/,$(PROGSSRCS:.f90=))

EXAMPLES_DIR = ../examples
EXAMPLES = $(addprefix $(EXAMPLES_DIR)/,$(EXAMPLESSRC:.f90=))

# implicit rules for object files
%.o: %.f90
	$(FC) $(FFLAGS) $< -c

.PHONY: $(PROGS_SHORTCUTS)
$(PROGS_SHORTCUTS):
	@make --no-print-directory $(addprefix $(PROGS_DIR)/, $@)


# prevent owervriting with new version on the server grat
$(PROGS_DIR)/%:%.f90 $(OBJS)
	$(FC) $(FFLAGS) $(NETCDFLIB) $< -o $@ $(OBJS)
# add timestamp to executable (archive for testing purposes)
ifdef UNIQ
	cp $@ $@$(DATE) -ni
endif


# paralelization
#../bin/value_check: value_check.f90 $(OBJS)
#  $(FC) $(FFLAGS) $(NETCDFLIB) $< -o $@ $(OBJS)

# rules for example files
$(EXAMPLES_DIR)/example%: $(EXAMPLES_DIR)/example%.f90 $(OBJS)
	$(FC) $(FFLAGS) $< -o $@ $(OBJS) $(NETCDFLIB)

all:          \
	$(OBJS)     \
	$(PROGS)    \
	$(LINKS)    \
	$(EXAMPLES)

libgrat.a: DEP=y
libgrat.a: $(OBJS)
	ar rs $@ $(OBJS)

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
	cd ../doc  &&  (doxygen Doxyfile > /dev/null 2>/dev/null)

docpdf: doc
	cd ../doc/latex/  &&  pdflatex -shell-escape refman
	evince ../doc/latex/refman.pdf &

dochtml: doc
	firefox ../doc/html/index.html &

docman: doc
	man /home/mrajner/src/grat/doc/man/man3/mod_utilities.3

install:                   \
	clean                    \
	all                      \
	$(PROGS)                 \
	/usr/local/lib/libgrat.a \
	$(LINKS_DIR_ALL)/grat    \
	$(LINKS_DIR_ALL)/value_check

# Change this path according to your system settings
# Make sure that target path is in your system serch $PATH
$(LINKS_DIR)/%: $(PROGS_DIR)/%
	ln -sf $(realpath $<) $@

$(LINKS_DIR_ALL)/%: $(PROGS_DIR)/%
	sudo cp -uv $(realpath $<) $@


# Change this path according to your system settings
# This could require root privilages
/usr/local/lib/libgrat.a: libgrat.a
	@sudo cp -uv libgrat.a /usr/local/lib
	@sudo cp -uv *.mod /usr/local/include
	@echo "library grat sucessfully installed"

rmlib:
	-sudo rm \
		/usr/local/include/mod_* \
		/usr/local/lib/libgrat.a


clean:
	rm -f *.o *.mod *.a fort?????? 

CLEAN: clean
	-sudo rm \
		/usr/local/include/mod_* \
		/usr/local/lib/libgrat.a
	-rm $(LINKS)
	-sudo rm $(LINKS_DIR_ALL)/grat $(LINKS_DIR_ALL)/value_check
	-rm $(EXAMPLES)

HELP_NAMES=grat.hlp polygon_check.hlp value_check.hlp
HELPS=$(addprefix ../dat/, $(HELP_NAMES))
helps: $(HELPS)
$(HELPS): $(PROGS)
%.hlp:
	$(patsubst ../dat%,../bin%,$(@:.hlp=)) -h  > $@

settings_for_gnc: grat
	time grat -F NCEP @SP,NCEP @LS, NCEP@HP, ETOPO@H\! , \
		NCEP1@T , VIENNA@RSP   \
		-Sj \
		-M2 \
		-D2011, 2011010118 \
		-G@GNc, @GNdt -H -o tmpgnc.dat

.PHONY:.FORCE
.FORCE:

version:grat 
	grat --version
	grat -v

# development and testing version of grat
test: libgrat.a
	$(FC) grat.f90 $(NETCDFLIB) libgrat.a -o $@
