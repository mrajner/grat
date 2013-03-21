# todo at the very and link not to objects but to library
# for example: OBJS  = libgrat.a

  # compiler
  FC = ifort
  # FLAGS = -r8 -O2  # -CB  # -Wunused -Wall  # -g -traceback

  # libraries 
  NETCDFLIB = -I/usr/local/include -L/usr/local/lib -lnetcdff -lnetcdf 
  FOULLIB = /home/mrajner/src/foul/foul.o -I/home/mrajner/src/foul

  # data for computation
  DAT_DIR  = /home/mrajner/dat
  NCEP_DIR = $(DAT_DIR)/ncep_reanalysis
  SFC      = $(NCEP_DIR)/pres.sfc.2011.nc@SP:pres
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

  SRCS  = mod_constants.f90    \
          mod_utilities.f90    \
          mod_cmdline.f90      \
          mod_polygon.f90      \
          mod_data.f90         \
          mod_aggf.f90         \
          mod_green.f90

  PROGSSRCS =                  \
          grat.f90             \
          value_check.f90      \
          polygon_check.f90    \
          real_vs_standard.f90

  EXAMPLESSRC =                \
          example_aggf.f90

  OBJS = $(SRCS:.f90=.o)

  PROGS_DIR=../bin
  PROGS  = $(addprefix $(PROGS_DIR)/,$(PROGSSRCS:.f90=))

  EXAMPLES_DIR = ../examples
  EXAMPLES = $(addprefix $(EXAMPLES_DIR)/,$(EXAMPLESSRC:.f90=))

# implicit rules for object files
%.o: %.f90
	$(FC) $< -c

# rules for programs files
$(PROGS_DIR)/%:%.f90 $(OBJS)
	$(FC) $(FLAGS) $< -o $(PROGS_DIR)/$@ $(OBJS) $(NETCDFLIB)

# rules for example files
$(EXAMPLES_DIR)/example%: $(EXAMPLES_DIR)/example%.f90 $(OBJS)
	$(FC) $(FLAGS) $< -o $@ $(OBJS) $(NETCDFLIB)

e: ../bin/value_check example
r: ../bin/real_vs_standard example

all:          \
	$(OBJS)     \
	$(PROGS)    \
	$(EXAMPLES) \
	example


example: 
	# $(PROGS_DIR)/value_check -F $(SFC):::: , $(LND)   -S joze,50.8912,20.6697,100  -P$(POL) -r20110104,20110105 V
	$(PROGS_DIR)/value_check -F  4@SP,$(LND),$(SFC),$(TMP) -I1,1,1,2,2 -P,../polygon/baltyk.poly:+ -R15/15.1/51/56,0.32 -D20110102,20110105 -V
	$(PROGS_DIR)/real_vs_standard -F$(SFC),$(GPT) -Sjoze,52,21 -V 

eeee:
	# @$(PROGS_DIR)/grat -V -F /home/mrajner/dat/ncep_reanalysis/air.2011.nc:air -SR10.2/12/11/11,2,2 -o 
	# $(PROGS_DIR)/grat -F$(SFC),$(TMP),../data/ncep_reanalysis/hgt.sfc.nc:hgt   -Sjoze,52:zeby,21,500 -Vtmp -D20090801,20090806  -BN -Grajner,rajner,rajner,rajner, -P -L: -I2,2,2,2,2 -o tmp P$(POL),$(POL)
	# $(PROGS_DIR)/value_check -S,22,45.4 -F$(SFC),$(SFC),5,$(LND) -I2,,2,1   -o -D2009,2009010112,4 -Ltmpii:c

libgrat.a: $(OBJS)
	ar rs $@ $(OBJS)

PACKAGE = grat-src
VER     = pre-alpha
DATE    = `date +%Y%m%d`

package: 
	tar jcf $(PACKAGE)_$(VER)_$(DATE).tar.bz2 ../../grat/src/{*.f90,Makefile} ../../grat/dat/*.dat ../../grat/bin/.README

doc: 
	cd ../doc  &&  doxygen Doxyfile

docpdf: 
	doc
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
# This require root privilages for my settings
installlib: libgrat.a
	@sudo cp -uv libgrat.a /usr/local/lib
	@sudo cp -uv *.mod /usr/local/include
	@echo "library grat sucessfully installed"

.phony: clean
clean:
	rm -f *.o *.mod tmp* *.a fort??????

todo: joinnc
