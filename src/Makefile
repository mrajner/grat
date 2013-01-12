.PHONY: clean
  BIN = ../bin
  FC = ifort
  FLAGS = -r8 -O2  # -CB  # -Wunused -Wall  # -g -traceback
  NETCDFLIB = -I/usr/local/include -L/usr/local/lib -lnetcdff -lnetcdf 
  SFC= ../data/ncep_reanalysis/pres.sfc.2011.nc:pres:lon:lat:level:time
  TMP= ../data/ncep_reanalysis/air.sig995.2011.nc:air:lon:lat:level:time
  LND= ../data/landsea/test.grd:z:x:y
  GRN_r= ../dat/rajner_green.dat
  GRN_m= ../dat/merriam_green.dat
	POL= ../polygon/tmp.poly
GPT= ../data/ncep_reanalysis/hgt.2011.nc:hgt
H__= ../data/ncep_reanalysis/hgt.sfc.nc:hgt
  SRCS  = constants.f90 get_cmd_line.f90 mod_polygon.f90 mod_data.f90 aggf.f90 mod_green.f90
  OBJS  = $(SRCS:.f90=.o)
 
 fnl = /home/mrajner/meteo/fnl/sfc_hgt/
#  fnl_ = /home/mrajner/meteo/fnl/sfc_hgt/hgt_sfc20090101_00_00.nc:HGT_P0_L1_GLL0:lon_0:lat_0
 fnl_ = /home/mrajner/meteo/fnl/sfc_pres/pres_sfc20090101_06_00.nc:PRES_P0_L1_GLL0:lon_0:lat_0


# iii: $(BIN)/example_aggf


# $(BIN)/joinnc: joinnc.f90
#   $(FC) $< -o $@ $(OBJS) $(NETCDFLIB) 
#   ./$@ -F/home/mrajner/meteo/fnl/sfc_hgt/hgt_sfc20090101_*nc

$(BIN)/barometric_formula: barometric_formula.f90 libgrat.a
	$(FC) $< -o $@ -lgrat -L.
	./$@ 
	

all:\
	$(OBJS) $(BIN)/grat $(BIN)/example_aggf $(BIN)/polygon_check \
	$(BIN)/real_vs_standard \
	$(BIN)/value_check example

%.o: %.f90
	$(FC) $< -c

$(BIN)/polygon_check: polygon_check.f90 $(OBJS)
	$(FC) $(FLAGS) $< -o $@ $(OBJS) $(NETCDFLIB)
#   ./$@  -S15/20/50/60,100,1 -V -P../polygon/tmp.poly

$(BIN)/grat: grat.f90  $(OBJS)
	$(FC) $(FLAGS) $<  -o $@ $(OBJS) $(NETCDFLIB)

$(BIN)/real_vs_standard: real_vs_standard.f90  $(OBJS)
	$(FC) $(FLAGS) $<  -o $@ $(OBJS) $(NETCDFLIB)

$(BIN)/value_check: value_check.f90  $(OBJS)
	$(FC) $(FLAGS) $<  -o $@ $(OBJS) $(NETCDFLIB)

example:
	grat -h
	grat -v
	$(BIN)/grat -F$(SFC),,,,,$(GPT),$(H__)  -Sjoze,53.8912,20.6697,100 -Vtmp -D20110101,20110102  -BI -G,,,,,  -I1 -o 
#   $(BIN)/real_vs_standard -F$(SFC),$(GPT) -Sjoze,52,21 -V
#   $(BIN)/grat -F$(SFC),$(TMP),../data/ncep_reanalysis/hgt.sfc.nc:hgt,$(LND)   -Sjoze,53.8912,20.6697,100 -Vtmp -D20110101,20111231  -BI -G,,,,,  -L: -I2,2,2,2,2 -o tmp P$(POL),$(POL) -otmp.dat
#   $(BIN)/grat -F$(SFC),$(TMP),../data/ncep_reanalysis/hgt.sfc.nc:hgt   -Sjoze,52:zeby,21,500 -Vtmp -D20090801,20090806  -BN -Grajner,rajner,rajner,rajner, -P -L: -I2,2,2,2,2 -o tmp P$(POL),$(POL)
#   $(BIN)/value_check -S,22,45.4 -F$(SFC),$(SFC),5,$(LND) -I2,,2,1   -o -D2009,2009010112,4 -Ltmpii:c


#-------------------------------------------------------------
green.o: green.f90
#   $(FC) $(FLAGS) -c $< 

../green/green_aux: green_aux.f90 green.o get_cmd_line.o 
#   $(FC) $(FLAGS) green_aux.f90 green.o get_cmd_line.o  -o $@
#   ../green/green_aux 


$(BIN)/example_aggf:example_aggf.f90 aggf.o  constants.o
	$(FC) $(FLAGS) $< aggf.o constants.o -o $@
	$(BIN)/example_aggf


clean:
	rm -f *.o *.mod tmp* *.a fort??????

libgrat.a: $(OBJS)
	make $(OBJS)
	ar rs libgrat.a *.o

PACKAGE = grat-src
VER     = 1.0
DATE     = `date +%Y%m%d`

package: 
	tar jcf $(PACKAGE)_$(VER)_$(DATE).tar.bz2 ../../grat/src/{*.f90,Makefile} ../../grat/dat/*.dat ../../grat/bin/.README

doc:
	cd ../doc  &&  doxygen Doxyfile

docpdf: 
	make doc
	cd ../doc/latex/  &&  make
	evince ../doc/latex/refman.pdf &

dochtml: 
	make doc
	firefox ../doc/html/index.html &

docext: 
	cd ../doc/latex/  &&  pdflatex -shell-escape refman
	
link:
	-cd ~/.local/bin && ln -s ${G}bin/* .
	-cd ~/.local/bin && ln -s ${G}polygon/polygon_map.sh .
