# todo at the very and link not to objects but to library
# for example: OBJS  = libgrat.a
#

MYMODEL=NCEP

dfsdfsf: $(OBJS) grat
	grat \
		-F $(MYMODEL) @SP, VIENNA@RSP, NCEP1@T, $(MYMODEL)@HP, ETOPO@H! , VIENNA@HRSP, $(MYMODEL)@GP! ,$(MYMODEL)@VT! \
		-M1,2,3 -G@GN  \
		-S j \
		-H   \
		-D 20100101:2010010623 \
		-I 60@AS: 50@HS: 40000@HE :1@DD :3@DE  \
		-o$(MYMODEL)_U:free:time  -q1 -A-0.43 -U

tmp: grat local
	grat \
		-F NCEP@SP! \
		-S local_site , j, local@L \
		-H  \
		-D 2009:20090102  -M1,2 -G@GN -o tmp

tmp2:
	grat \
		-F NCEP@SP! \
		-S local_site  \
		-H  \
		-D 2009:2011  -M1,2 -G@GN -o tmp2 

local: 
	file=$$(basename $$(ls /home/mrajner/dat/ispd/hourly/___*WARSZAWA*)); \
			 grep $$file /home/mrajner/dat/ispd/hourly/sites.sta |awk '{print "aaa" , $$2, $$3}' > local_site  ; \
			 cat /home/mrajner/dat/ispd/hourly/$${file} | awk '{print $$1, $$3}' > local

holla:grat
	grat -F VIENNA @RSP,NCEP@SP, NCEP1@T,NCEP@HP, ETOPO@H\! -G@GNc, @GN, @GNdt, @GNdz,@GNdz2 -D2010:2011 \
		-Sj \
		-oholla -H



# compiler
FC = ifort

# compiler flags
FFLAGS = -fpp  -debug -warn all -traceback 
# -syntax-only # -O2  -CB    # -g -traceback -heap-arrays

# libraries 
NETCDFLIB = -I/usr/local/include -L/usr/local/lib -lnetcdf -lnetcdff -lnetcdf  

# data for computation
DAT_DIR  = /home/mrajner/dat
NCEP_DIR = $(DAT_DIR)/ncep_reanalysis
SP       = $(NCEP_DIR)/pres.sfc.2011.nc@SP\!:pres
T        = $(NCEP_DIR)/air.sig995.2011.nc.reanalysis1@T:air
LS       = $(NCEP_DIR)/land.nc@LS:land
GPT      = $(NCEP_DIR)/hgt.2011.nc:hgt
H__      = $(NCEP_DIR)/hgt.sfc.nc@HP:hgt@g2h
Hetopo   = /home/mrajner/dat/etopo/ETOPO1_Ice_c_gmt4.grd@H! : z : x: y
	#todo vienna

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
				mod_3d.f90          \

PROGSSRCS =  grat.f90               \
						 value_check.f90        \
						 polygon_check.f90      \
						 real_vs_standard.f90   \
						 barometric_formula.f90

grat:
	echo "offf"

EXAMPLESSRC = example_aggf.f90

OBJS = $(SRCS:.f90=.o)


# module dependencies
ifdef DEP
mod_utilities.o: mod_constants.o
mod_atmosphere.o: mod_printing.o mod_utilities.o
mod_aggf.o: mod_constants.o mod_utilities.o mod_atmosphere.o mod_normalization.o
mod_site.o: mod_printing.o mod_utilities.o mod_constants.o mod_data.o mod_cmdline.o mod_date.o
mod_parser.o: mod_cmdline.o mod_data.o mod_polygon.o mod_green.o mod_admit.o mod_date.o
mod_green.o: mod_data.o mod_polygon.o mod_date.o mod_spherical.o mod_site.o mod_normalization.o mod_aggf.o mod_3d.o mod_data.o
mod_data.o: mod_mjd.o mod_atmosphere.o mod_polygon.o mod_utilities.o mod_printing.o 
mod_date.o: mod_data.o
mod_mjd.o: mod_constants.o
mod_polygon.o:mod_printing.o mod_cmdline.o
mod_printing.o: mod_cmdline.o
mod_cmdline.o: mod_utilities.o
mod_normalization.o: mod_constants.o mod_utilities.o
endif


PROGS_DIR=../bin
#  PROGS  = $(addprefix $(PROGS_DIR)/,$(PROGSSRCS:.f90=))
#  PROGS_SHORTCUTS  = $(PROGSSRCS:.f90=)

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

$(PROGS_DIR)/%:%.f90 $(OBJS)
	$(FC) $(FFLAGS) $(NETCDFLIB) $< -o $@ $(OBJS) 

#  ../bin/value_check: value_check.f90 $(OBJS)
	#  $(FC) $(FFLAGS) $(NETCDFLIB) $< -o $@ $(OBJS) /home/mrajner/src/ncl_ncarg-6.1.2/ni/src/lib/nfpfort/int2p_dp.f

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

PACKAGE = grat-src
VER     = pre-alpha
DATE    = `date +%Y%m%d`
package:
	tar jcf $(PACKAGE)_$(VER)_$(DATE).tar.bz2 \
		../../grat/src/{*.f90,Makefile}         \
		../../grat/dat/*.dat                    \
		../../grat/bin/.README

doc: 
	cd ../doc  &&  doxygen Doxyfile

docpdf: doc
	cd ../doc/latex/  &&  pdflatex -shell-escape refman
	evince ../doc/latex/refman.pdf &

dochtml: doc
	firefox ../doc/html/index.html &

docman: doc
	man /home/mrajner/src/grat/doc/man/man3/mod_utilities.3

install: all $(PROGS) installlib


# Change this path according to your system settings
# Make sure that target path is in your system serch $PATH
$(LINKS_DIR)/%: $(PROGS_DIR)/%
	ln -sf $(realpath $<) $@

$(LINKS_DIR_ALL)/%: $(PROGS_DIR)/%
	sudo cp $(realpath $<) $@

install-httpd: $(LINKS_DIR_ALL)/grat $(LINKS_DIR_ALL)/value_check

# Change this path according to your system settings
# This could require root privilages 
installlib: /usr/local/lib/libgrat.a
/usr/local/lib/libgrat.a: libgrat.a
	@sudo cp -uv libgrat.a /usr/local/lib
	@sudo cp -uv *.mod /usr/local/include
	@echo "library grat sucessfully installed"

rmlib:
	-sudo rm \
		/usr/local/include/mod_* \
		/usr/local/lib/libgrat.a 
rmlink:
	-rm $(LINKS)

clean: rmlib 
	rm -f *.o *.mod *.a fort?????? tmp*

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
