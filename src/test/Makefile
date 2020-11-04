FC  ?= gfortran
OPT := -O2

# GRATLIB := -I../$(FC) -L../$(FC) -lgrat
GRATLIB := -I/usr/local/include/ -lgrat
MRLIB   := -I/usr/local/include/ -lmr

# ifort settings
ifeq ($(FC),ifort)
	FFLAGS :=      \
					-fpic  \
					$(OPT) \
					-xHost \
					-warn all 
	ENABLE_PREPROCESSING := -fpp
	NETCDFF :=                                         \
		-I/usr/local/include -L/usr/local/lib64 -lnetcdf \
		-I/home/mrajner/src/netcdf-fortran/fortran/      \
		-L/home/mrajner/src/netcdf-fortran/fortran -lnetcdff
endif

# gfortran settings
ifeq ($(FC),gfortran)
	FFLAGS  := $(OPT)
	ENABLE_PREPROCESSING := -cpp
	NETCDFF := -I/usr/include  -lnetcdf -lnetcdff
endif

all: data/test_data.nc data/gmtls.nc data/ncks.nc

data/gmtls.nc:
	gmt grdlandmask -Rg -I5 -Gdata/gmtls.nc

data/ncks.nc: | data/test_data.nc
	ncks -v sp $| $@

ooo: $(NCEP_DATA_FILES)
#  all: ask run data test
#  .PHONY:runtests

runtests: 
	make -C tests

ask:
	@tput setaf 1 ; \
	echo "do you want download ncep data (could take some time) or run test only for synthetic data?" ; \
	echo "[Y|y - download and run test on real data]" ; \
	tput sgr0 ; \
	read answer <<<r;  \
	case $$answer in \
	Y|y) \
	make ncep ; \
	;; \
	esac 

test: 
	make -C tests/
	cd tests/ && ./stat.sh

create_data: create_data.f90
	$(FC) $< -o $@ $(FFLAGS)  $(GRATLIB) $(MRLIB) $(NETCDFF) 

data/test_data.nc: create_data data
	./$< 
	value_check \
		-F data/test_data.nc@SP:sp:lon:lat , @T : t , @GP : gp : lon:lat:level, \
		 @LS:ls \
		-S j -D201201 : m \
		-o :level -J1000,10 -H

data:
	mkdir -p $@

year := 2012

NCEP_DATA_FILES :=           \
	data.hgt.$(year).nc        \
	data.air.$(year).nc        \
	data.hgt.sfc.nc            \
	data.pres.sfc.$(year).nc   \
	data.air.sig995.$(year).nc \
	data.land.nc               \
	data.shum.$(year).nc

ncep: data
	@wget  -nH -nc -nd -P data/                                                      \
		ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis2/pressure/hgt.$(year).nc      \
		ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis2/pressure/air.$(year).nc      \
		ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis2/surface/hgt.sfc.nc           \
		ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis2/surface/pres.sfc.$(year).nc  \
		ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis/surface/air.sig995.$(year).nc \
		ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis/surface/land.nc               \
		ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis/pressure/shum.$(year).nc 

clean:
	rm -vf create_data data/test_data.nc
