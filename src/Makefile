  BIN = ../bin
  EXMPL = ../examples
  POLY = ../polygon
  FC = ifort
  FLAGS = -r8 -O2 -CB  # -g -traceback
  NETCDFLIB = -L/usr/local/lib -lnetcdff -lnetcdf 
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
 

all:\
	$(OBJS) $(BIN)/grat $(EXMPL)/example_aggf $(POLY)/polygon_check \
	$(BIN)/real_vs_standard \
	$(BIN)/value_check example

%.o: %.f90
	$(FC) $< -c

$(POLY)/polygon_check: polygon_check.f90 $(OBJS)
#   $(FC) $(FLAGS) $< -o $@ $(OBJS)
#   ./$@  -S15/20/50/60,100,1 -V -P../polygon/tmp.poly

$(BIN)/grat: grat.f90  $(OBJS)
#   $(FC) $(FLAGS) $<  -o $@ $(OBJS) $(NETCDFLIB)

$(BIN)/real_vs_standard: real_vs_standard.f90  $(OBJS)
	$(FC) $(FLAGS) $<  -o $@ $(OBJS) $(NETCDFLIB)

$(BIN)/value_check: value_check.f90  $(OBJS)
#   $(FC) $(FLAGS) $<  -o $@ $(OBJS) $(NETCDFLIB)

example:
	$(BIN)/real_vs_standard -F$(SFC),$(GPT) -Sjoze,52,21 -V
#   $(BIN)/grat -F$(SFC),,,,,$(GPT),$(H__)  -Sjoze,53.8912,20.6697,100 -Vtmp -D20110101,20110102  -BI -G,,,,,  -I1 -o tmp P$(POL),$(POL) 
#   $(BIN)/grat -F$(SFC),$(TMP),../data/ncep_reanalysis/hgt.sfc.nc:hgt,$(LND)   -Sjoze,53.8912,20.6697,100 -Vtmp -D20110101,20111231  -BI -G,,,,,  -L: -I2,2,2,2,2 -o tmp P$(POL),$(POL) -otmp.dat
#   $(BIN)/grat -F$(SFC),$(TMP),../data/ncep_reanalysis/hgt.sfc.nc:hgt   -Sjoze,52:zeby,21,500 -Vtmp -D20090801,20090806  -BN -Grajner,rajner,rajner,rajner, -P -L: -I2,2,2,2,2 -o tmp P$(POL),$(POL)
#   $(BIN)/value_check -S,22,45.4 -F$(SFC),$(SFC),5,$(LND) -I2,,2,1   -o -D2009,2009010112,4 -Ltmpii:c

#   cd /home/mrajner/dr/data/site_vs_model/ && make -B

#-------------------------------------------------------------
$(BIN)/symuluj_dane: symuluj_dane.f90 dane.o get_cmd_line.o ispand.o
#   $(FC) $(FLAGS) symuluj_dane.f90 dane.o get_cmd_line.o ispand.o -o $(BIN)/symuluj_dane
#   $(BIN)/symuluj_dane
#   $(BIN)/press2spotl  -F../dane/symulacja/dane.blv -I1. -W0.5 -Glbv -R- 

mapcon_util.o: mapcon_util.f90
#   $(FC) $(FLAGS) -c mapcon_util.f90

mapa_lndsea: mapa_lndsea.f90 mapcon_util.o
#   $(FC) $(FLAGS) mapa_lndsea.f90 mapcon_util.o -o $(BIN)/mapa_lndsea
#   $(BIN)/mapa_lndsea

green.o: green.f90
#   $(FC) $(FLAGS) -c $< 

../green/green_aux: green_aux.f90 green.o get_cmd_line.o 
#   $(FC) $(FLAGS) green_aux.f90 green.o get_cmd_line.o  -o $@
#   ../green/green_aux 

../green/ignewt: ignewt.f90 green.o get_cmd_line.o 
#   $(FC) $(FLAGS) ignewt.f90 green.o get_cmd_line.o  -o ../green/ignewt 
#   ../green/ignewt -B52. -L21. -H33.  

$(BIN)/press2spotl: press2spotl.f90 get_cmd_line.o  ispand.o dane.o
#   $(FC) $(FLAGS) press2spotl.f90 dane.o get_cmd_line.o  ispand.o -o $@
#   $(BIN)/press2spotl  -F../dane/ncep_surf_pres/txt/pres_20100101000000.txt -I2.5 -W1 -Glbv
#   $(BIN)/press2spotl -F../dane/ncep_sig995_temp/txt/temp_20090101000000.txt -I2.5 -W5.5 -Glbv -R+-273.15 


$(EXMPL)/example_aggf:example_aggf.f90 aggf.o  constants.o
#   $(FC) $(FLAGS) $< aggf.o constants.o -o $@
#   $(EXMPL)/example_aggf

clean:
	rm *.o *.mod tmp.*

lib:
	make $(OBJS)
	rm libgrat.a
	ar rvs libgrat.a *.o


PACKAGE = grat-src
VER     = 1.0
DATE     = `date +%Y%m%d`

package: 
	@echo $(PACKAGE)_$(VER)_$(DATE)

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
	
