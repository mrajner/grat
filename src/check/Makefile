FC = ifort
FTPDIR = ftp\://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis2

NETCDFLIB = -I/usr/local/include \
						-L/usr/local/lib     \
						-lnetcdff            \
						-lnetcdf
GRATLIB   = -I/usr/local/include \
						-L/usr/local/lib     \
						-lgrat

all: ask run

ask:
	@tput setaf 1 ; \
	echo "do you want download ncep data (could take some time) or run test only for synthetic data?" ; \
	echo "[Y|y - download and run test on real data]" ; \
	tput sgr0 ; \
	read answer <<<Y;  \
	case $$answer in \
	Y|y) \
	make ncep ; \
	;; \
	esac 


check: 
	make -C tests/

create_data: create_data.f90
	$(FC) $< -o $@ $(NETCDFLIB) $(GRATLIB)

run: create_data
	./$<



year = 2012

data:
	mkdir -p $@

ncep: data
	@wget --quiet  -nH -nc -nd -P data/                                             \
		ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis2/pressure/hgt.$(year).nc      \
		ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis2/pressure/air.$(year).nc      \
		ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis2/surface/hgt.sfc.nc           \
		ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis2/surface/pres.sfc.$(year).nc  \
		ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis/surface/air.sig995.$(year).nc \
		ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis/surface/land.nc               \
		ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis/pressure/shum.$(year).nc 


