# todo at the very and link not to objects but to library
# for example: OBJS  = libgrat.a

# as: value_check  example_value_check

atmp: atmp.f90 mod_utilities.o
	$(FC) $(FLAGS) -o $@ $< $(OBJS)  $(NETCDFLIB) 
	./atmp

		 
i:value_check
	value_check \
		-F \
		 /home/mrajner/dat/ncep_reanalysis/air.2011.nc:air \
		, /home/mrajner/dat/ncep_reanalysis/air.sig995.2011.nc :air \
		-S j -Dm:1@D -V

RSP = /home/mrajner/dat/refpres/refpres0p5.nc@RSP:rp
# RSP = /home/mrajner/dat/ncep_reanalysis/pres.sfc.2010.nc@RSP:pres  
# RSP = 101300@RSP
wl: ../bin/real_vs_standard
	./$<

sx: grat 
	grat \
		-F $(SP) , $(LS)\
		-Sj  \
		-G@GE  -H -Dm:m  -BI   -L s@s -V -I  40 @DS :50@AS \
		-o tmp

#   grat -Sj  \
#     -F $(SP), $(RSP)  \
#     -G \
#     /home/mrajner/dat/green/green_prem_atm_guo.dat@GE : 1 : 4 @f2m \
#      \
#     -H -Dm -tmp -La@g # -I 5 @DS : 80 @AS :0@DB: 10@DE

#   grat \
#     -S j : 52 : 21   \
#     -F  /home/mrajner/dat/ncep_reanalysis/pres.sfc.2010.nc @SP :pres , $(RSP) , $(LS) \
#     , 1000 @EWT \
#     -BI  \
#     -G /home/mrajner/dat/green/green_ce_aplo.dat@GR:1:2@aplo , m@GN, m @GE   -H -V \
#     -P ../polygon/baltyk.poly:- \
#     -Dm  -L@s , p@p:s


oo: value_check
	value_check \
		-F /home/mrajner/dat/ncep_reanalysis/pres.sfc.2009.nc @ SP :pres\
		-S j :52:21 \
     -D 2009:200904  -H -o tmp2
iiiii: grat rrrr
r:
	../bin/grat  -S \
		joze:52.0:21 \
		-F \
		/home/mrajner/dat/wghm/dat/WGHM.nc@EWT \
		-G /home/mrajner/dat/green/green_ce_aplo.dat@GR:1@R:2@aplo \
		-D200201
gnu2:
	../bin/grat  -S \
		joze:52.0:21 \
		-F \
		/home/mrajner/dat/wghm/dat/WGHM.nc@EWT \
		-G /home/mrajner/dat/green/green_ce_aplo.dat@GR:1@R:2@aplo \
		-D2002:2013:1@M  -L ttt@g > tmp3
rrrr:
	../bin/grat  -S \
		joze:52.0:21.2 \
		-F \
		/home/mrajner/dat/wghm/dat/WGHM.nc@EWT \
		-G /home/mrajner/src/gotic2/data/grn1.data@GR:1:2 \
		, /home/mrajner/src/gotic2/data/grn1.data@GH:1:3 \
		-D2011:2011:1@M \
		-I 0 @DB : 90@DE : 0@AB \
		-L a @p, aux @ a \
		-P ../polygon/baltyk.poly -V

	#
gnu:
	../bin/grat  -S \
		joze:52.0:21 \
		-F \
		/home/mrajner/dat/wghm/dat/WGHM.nc@EWT \
		-G /home/mrajner/src/gotic2/data/grn1.data@GR:1:2 \
		-D2004:2011:1@M  -I 1@DD :1@AD :10@rE  -Lttt@g  > tmp

gn:
	../bin/grat  -S \
		joze:52.0:21 \
		-F \
		/home/mrajner/dat/ncep_reanalysis/pres.sfc.2006.nc:pres@SP \
		-G merriam @GE  \
		-D2006:10@D  

#     :55.1:15.3 ,\
#     :48.7:19.6,\
#     :25.6:-1.1,\
#     :-3: -56 \
gggg:


al:
xxx: ddd tmp runtmp


al: grat example

# tmp: tmp.f90 $(OBJS) mod_cmdline.o
#   $(FC) $(FLAGS) $< -o $@ $(OBJS) $(NETCDFLIB)

# compiler
FC = ifort

# compiler flags
FLAGS =   # -syntax-only # -O2  -CB    # -g -traceback -heap-arrays

# libraries 
NETCDFLIB = -I/usr/local/include -L/usr/local/lib -lnetcdf -lnetcdff  

# data for computation
DAT_DIR  = /home/mrajner/dat
NCEP_DIR = $(DAT_DIR)/ncep_reanalysis
SP       = $(NCEP_DIR)/pres.sfc.2011.nc@SP:pres
T       = $(NCEP_DIR)/air.sig995.2011.nc@T:air:lon:lat:level:time
LS      = $(DAT_DIR)/landsea/landseaRdI0.2-0.2.grd@LS:z:x:y
GPT      = $(NCEP_DIR)/hgt.2011.nc:hgt
H__      = ../data/ncep_reanalysis/hgt.sfc.nc:hgt
FNL      = /home/mrajner/meteo/fnl/sfc_hgt/
fnl_     = /home/mrajner/meteo/fnl/sfc_pres/pres_sfc20090101_06_00.nc:PRES_P0_L1_GLL0:lon_0:lat_0
#todo vienna

SRCS  = mod_constants.f90 \
				mod_utilities.f90 \
				mod_printing.f90  \
         mod_cmdline.f90   \
         mod_site.f90      \
         mod_spherical.f90 \
				 mod_mjd.f90 \
         mod_green.f90     \
         mod_polygon.f90   \
         mod_data.f90      \
         mod_date.f90      \
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
mod_site.o: mod_printing.o mod_utilities.o mod_constants.o mod_data.o
mod_parser.o: mod_cmdline.o mod_data.o mod_polygon.o mod_green.o
mod_green.o: mod_data.o mod_polygon.o mod_date.o
mod_data.o: mod_mjd.o
mod_date.o:mod_data.o
mod_mjd.o: mod_constants.o


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
		, $(LS) ,101325@RS ,11                                        \
		-G                                              \
		merriam @GN \
		-P ../polygon/baltyk.poly @ N:+, @E:-               \
		-S j:52:21:10                          \
		-D 2011 \
		-V \
		-L @r  \
		-H \
		-I 12.9 @rB : 26@rE: 1@DD : 1@AD : l@I

example_value_check: value_check
	$(PROGS_DIR)/value_check \
		-F$(SP) \
		, 4@PR\
		-Szr:52:21 \
		-V \
		-I l @I \
		-D2011:1@D ,20110606 \
		-L tmp@d -H


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
	ln -sf /home/mrajner/src/grat/bin/*  ~/.local/bin/
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
