# todo at the very and link not to objects but to library
# for example: OBJS  = libgrat.a

al: value_check example

# tmp: tmp.f90 $(OBJS) mod_cmdline.o
#   $(FC) $(FLAGS) $< -o $@ $(OBJS) $(NETCDFLIB)

# un:
#   @./tmp -V -h  -D 2006 06, 2007 12 ,2@M -Ogg@ww , -F




# e: grat install
#   grat -L dddd  -V



# compiler
FC = ifort

# compiler flags
# FLAGS = -r8 -O2  # -CB  # -Wunused -Wall  # -g -traceback

# libraries 
NETCDFLIB = -I/usr/local/include -L/usr/local/lib -lnetcdff -lnetcdf 
FOULLIB = /home/mrajner/src/foul/foul.o -I/home/mrajner/src/foul

# data for computation
DAT_DIR  = /home/mrajner/dat
NCEP_DIR = $(DAT_DIR)/ncep_reanalysis
SP       = $(NCEP_DIR)/pres.sfc.2011.nc@SP:pres
TMP      = $(NCEP_DIR)/air.sig995.2011.nc@@:air:lon:lat:level:time
LND      = $(DAT_DIR)/landsea/landseaR0-50-0-70I0.2-0.2.grd@LS:z:x:y:
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

SRCS  = mod_constants.f90    \
				mod_utilities.f90    \
				mod_cmdline.f90      \
				mod_polygon.f90      \
				mod_data.f90         \
				mod_aggf.f90         \
				mod_green.f90

PROGSSRCS =  grat.f90               \
						 value_check.f90        \
						 polygon_check.f90      \
						 real_vs_standard.f90   \
						 barometric_formula.f90

EXAMPLESSRC = example_aggf.f90

OBJS = $(SRCS:.f90=.o)

PROGS_DIR=../bin
PROGS  = $(addprefix $(PROGS_DIR)/,$(PROGSSRCS:.f90=))
	PROGS_SHORTCUTS  = $(PROGSSRCS:.f90=)

EXAMPLES_DIR = ../examples
EXAMPLES = $(addprefix $(EXAMPLES_DIR)/,$(EXAMPLESSRC:.f90=))

# implicit rules for object files
%.o: %.f90
	$(FC) $< -c

# rules for programs files
# todo - shorter
.PHONY: $(PROGS_SHORTCUTS)
value_check: ../bin/value_check
polygon_check: ../bin/polygon_check
grat: ../bin/grat

$(PROGS_DIR)/%:%.f90 $(OBJS)
	$(FC) $(FLAGS) $< -o $@ $(OBJS) $(NETCDFLIB)

# rules for example files
$(EXAMPLES_DIR)/example%: $(EXAMPLES_DIR)/example%.f90 $(OBJS)
	$(FC) $(FLAGS) $< -o $@ $(OBJS) $(NETCDFLIB)

all:          \
	$(OBJS)     \
	$(PROGS)    \
	$(EXAMPLES) 


example: 
	$(PROGS_DIR)/value_check -F$(SP):a:b:c, $(SP), 55  -L:b -I 2 -V -S0/360/0/1,420 -D2011,1@M,4@D  -Fdd


oo:
	#   @$(PROGS_DIR)/grat V -F $(SP) , $(RS)  -SR,2,2,2 , -Ls
	#   $(PROGS_DIR)/real_vs_standard -F$(SFC),$(GPT) -Sjoze,52,21 -V 
	#   $(PROGS_DIR)/real_vs_standard -F$(SFC),$(GPT) -Sjoze,52,21 -V 

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
install: $(PROGS) 
	@cp -u -v ../bin/*  ~/.local/bin/
	@echo "program grat sucessfully installed"

# Change this path according to your system settings
# This could require root privilages 
installlib: /usr/local/lib/libgrat.a
/usr/local/lib/libgrat.a: libgrat.a
	@sudo cp -uv libgrat.a /usr/local/lib
	@sudo cp -uv *.mod /usr/local/include
	@echo "library grat sucessfully installed"

clean:
	rm -f *.o *.mod tmp* *.a fort??????

todo: joinnc

HELP_NAMES=grat.hlp polygon_check.hlp value_check.hlp
HELPS=$(addprefix ../dat/, $(HELP_NAMES))
helps: $(HELPS) 
$(HELPS): $(PROGS)

%.hlp: 
	$(patsubst ../dat%,../bin%,$(@:.hlp=)) -h  > $@
