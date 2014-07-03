FC = ifort
FTPDIR = ftp\://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis2

NETCDFLIB = -I/usr/local/include \
						-L/usr/local/lib     \
						-lnetcdff            \
						-lnetcdf
GRATLIB   = -I/usr/local/include \
						-L/usr/local/lib     \
						-lgrat

all: run data
	
df:
	# ask run runtests

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


check: 
	make -C tests/

create_data: create_data.f90
	$(FC) $< -o $@ $(NETCDFLIB) $(GRATLIB)

run: create_data
	./$< 
	@make --no-print-directory -C $$GS value_check install
	value_check  -F data/test_data.nc:sp:lon:lat , @T : t -S j , :2: 340 -D201201 : 32@H
	value_check   -F data/test_data.nc:sp:lon:lat, @T! : t -S j , :2: 340 -D201201 : 32@H



year = 2012

data:
	mkdir -p $@

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

