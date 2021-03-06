FC  := gfortran
OPT := -O2

GRATLIB := -I/usr/local/include \
						-L/usr/local/lib     \
						-lgrat

# ifort settings
ifeq ($(FC),ifort)
FFLAGS :=          \
				 -fpic     \
				 $(OPT)    \
				 -xHost    \
				 -warn all 

ENABLE_PREPROCESSING := -fpp
endif

# gfortran settings
ifeq ($(FC),gfortran)
	FFLAGS  := \
		$(OPT)   \
		-I/home/mrajner/src/netcdf-gfortran/fortran 
	ENABLE_PREPROCESSING = -cpp
endif

NETCDFF := -lnetcdff

all: ask run data test
	
.PHONY:runtests
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

create_data: create_data.f90 data
	$(FC) $< -o $@ $(FFLAGS) $(NETCDFF) $(GRATLIB)

run: create_data
	./$< 
	value_check \
		-F data/test_data.nc@SP:sp:lon:lat , @T : t , @GP : gp : lon:lat:level, \
		 @LS:ls \
		-S j -D201201 : m \
		-o :level -J1000,10 -H

data:
	mkdir -p $@

year := 2012
ncep: data
	@wget  -nH -m -nd -P data/                                             \
		ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis2/pressure/hgt.$(year).nc      \
		ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis2/pressure/air.$(year).nc      \
		ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis2/surface/hgt.sfc.nc           \
		ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis2/surface/pres.sfc.$(year).nc  \
		ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis/surface/air.sig995.$(year).nc \
		ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis/surface/land.nc               \
		ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis/pressure/shum.$(year).nc 

clean:
	rm create_data
