# todo at the very and link not to objects but to library
# for example: OBJS  = libgrat.a

# mak: clean mod_cmdline.o
# as: value_check  example_value_check second
al:
xxx: ddd tmp runtmp

tmp: tmp.f90 ddd
	$(FC) $(FLAGS) -o $@ $< $(OBJS)  $(NETCDFLIB)
runtmp:
	./tmp \
		-S j:4:4:2 \
		-D2011 \
		-f $(SP)  \
		, gfg:xx \
		-G rajner \
		, merriam :2 :4\
		-V 

al: grat example

# tmp: tmp.f90 $(OBJS) mod_cmdline.o
#   $(FC) $(FLAGS) $< -o $@ $(OBJS) $(NETCDFLIB)

# compiler
FC = ifort

# compiler flags
FLAGS =   # -O2  -CB    # -g -traceback -heap-arrays

# libraries 
NETCDFLIB = -I/usr/local/include -L/usr/local/lib -lnetcdf -lnetcdff  
FOULLIB = /home/mrajner/src/foul/foul.o -I/home/mrajner/src/foul

# data for computation
DAT_DIR  = /home/mrajner/dat
NCEP_DIR = $(DAT_DIR)/ncep_reanalysis
SP       = $(NCEP_DIR)/pres.sfc.2011.nc@SP:pres
ST       = $(NCEP_DIR)/air.sig995.2011.nc@ST:air:lon:lat:level:time
LS      = $(DAT_DIR)/landsea/landseaR0-50-0-70I0.2-0.2.grd@LS:z:x:y
GRN_r    = ../dat/rajner_green.dat
GRN_m    = ../dat/merriam_green.dat
POL      = ../polygon/baltyk.poly
GPT      = $(NCEP_DIR)/hgt.2011.nc:hgt
H__      = ../data/ncep_reanalysis/hgt.sfc.nc:hgt
	fnl      = /home/mrajner/meteo/fnl/sfc_hgt/
	# fnl_   = /home/mrajner/meteo/fnl/sfc_hgt/hgt_sfc20090101_00_00.nc:HGT_P0_L1_GLL0:lon_0:lat_0
	fnl_     = /home/mrajner/meteo/fnl/sfc_pres/pres_sfc20090101_06_00.nc:PRES_P0_L1_GLL0:lon_0:lat_0
	#todo vienna
	RS 			 = /home/mrajner/src/grat/data/refpres/vienna_p0.grd@RS
	RS 			 = /home/mrajner/dat/ncep_reanalysis/pres.sfc.2010.nc@RS:pres  
	RS 			 = 101300@RS

SRCS  = mod_constants.f90 \
				mod_utilities.f90 \
				mod_printing.f90  \
				mod_cmdline.f90   \
				mod_site.f90      \
				mod_date.f90      \
				mod_spherical.f90 \
				mod_green.f90     \
				mod_polygon.f90   \
				mod_data.f90      \
				mod_aggf.f90      \
				mod_parser.f90

PROGSSRCS =  grat.f90               \
						 value_check.f90        \
						 polygon_check.f90      \
						 real_vs_standard.f90   \
						 barometric_formula.f90

EXAMPLESSRC = example_aggf.f90

OBJS = $(SRCS:.f90=.o)


# module dependencies
mod_utilities.o: mod_constants.o
mod_aggf.o: mod_constants.o mod_utilities.o
mod_site.o: mod_printing.o mod_utilities.o mod_constants.o
mod_parser.o: mod_cmdline.o mod_data.o mod_polygon.o mod_green.o
mod_green.o: mod_data.o mod_polygon.o


# mod_cmdline.o: mod_green.o mod_polygon.o mod_cmdline.f90
#   
# E:
#   mod_polygon.o: mod_cmdline.o
#   mod_green.o: mod_cmdline.o

PROGS_DIR=../bin
PROGS  = $(addprefix $(PROGS_DIR)/,$(PROGSSRCS:.f90=))
	PROGS_SHORTCUTS  = $(PROGSSRCS:.f90=)

EXAMPLES_DIR = ../examples
EXAMPLES = $(addprefix $(EXAMPLES_DIR)/,$(EXAMPLESSRC:.f90=))

# implicit rules for object files
%.o: %.f90
	$(FC) $(FLAGS) $< -c

# rules for programs files
# todo - shorter
.PHONY: $(PROGS_SHORTCUTS)
value_check: ../bin/value_check
polygon_check: ../bin/polygon_check
grat: ../bin/grat

$(PROGS_DIR)/%:%.f90 $(OBJS)
	$(FC) $(NETCDFLIB) $(FLAGS) $< -o $@ $(OBJS) 

# rules for example files
$(EXAMPLES_DIR)/example%: $(EXAMPLES_DIR)/example%.f90 $(OBJS)
	$(FC) $(FLAGS) $< -o $@ $(OBJS) $(NETCDFLIB)

all:          \
	$(OBJS)     \
	$(PROGS)    \
	$(EXAMPLES) 

example: 
	$(PROGS_DIR)/grat                                 \
		-F $(SP), $(ST)                                 \
		, $(LS) ,5 ,11                                        \
		-G                                              \
		m @GN \
		-P ../polygon/baltyk.poly @ N:+, @E:-               \
		-S j:52:21:10                          \
		-D 2011:2012:1@D \
		 -V \
		 -L @r  \
		 -I 12.9 @rB : 26@rE: 1@DD : 1 @AD : l@I

example_value_check: 
	$(PROGS_DIR)/value_check \
		-F$(SP) \
		, 4@PR\
		-Szr:52:21 \
		-V \
		-I l @I \
		-D2011:1@D ,20110606 

second:

libgrat.a: $(OBJS)
	ar rs $@ $(OBJS)

PACKAGE = grat-src
VER     = pre-alpha
DATE    = `date +%Y%m%d`
package: 
	tar jcf $(PACKAGE)_$(VER)_$(DATE).tar.bz2 ../../grat/src/{*.f90,Makefile} ../../grat/dat/*.dat ../../grat/bin/.README

doc: 
	cd ../doc  &&  doxygen Doxyfile

docpdf: doc
	cd ../doc/latex/  &&  pdflatex -shell-escape refman
	evince ../doc/latex/refman.pdf &

dochtml: doc
	firefox ../doc/html/index.html &

docman: doc
	man /home/mrajner/src/grat/doc/man/man3/mod_utilities.3


# Change this path according to your system settings
# Make sure that target path is in your system serch $PATH
install: installprogs installlib

installprogs: $(PROGS) 
	@cp -u -v ../bin/*  ~/.local/bin/
	@echo "program grat sucessfully installed"

# Change this path according to your system settings
# This could require root privilages 
installlib: /usr/local/lib/libgrat.a
/usr/local/lib/libgrat.a: libgrat.a
	@sudo cp -uv libgrat.a /usr/local/lib
	@sudo cp -uv *.mod /usr/local/include
	@echo "library grat sucessfully installed"

rmlib:
	-sudo rm /usr/local/include/mod_*

clean: rmlib
	rm -f *.o *.mod *.a fort?????? tmp*

todo: joinnc

HELP_NAMES=grat.hlp polygon_check.hlp value_check.hlp
HELPS=$(addprefix ../dat/, $(HELP_NAMES))
helps: $(HELPS) 
$(HELPS): $(PROGS)

%.hlp: 
	$(patsubst ../dat%,../bin%,$(@:.hlp=)) -h  > $@
