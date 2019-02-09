# system wide $FC
# if not set use gfortran
FC := gfortran

# TODO
# optimalization
OPT=-O0

# user definitions
# you may want to change this settings
LIBDIR := /usr/local/lib
INCDIR := /usr/local/include
BINDIR := /usr/local/bin

# passing compilation info to program
FC_INFO       := $(shell $(FC) --version | sed -n 1p )
HOST          := $(shell hostname ; uname -r)
DATE          := $(shell date "+%Y-%m-%d %H:%M:%S %z")
VERSION       := $(shell git describe --match "v*")
GIT_DATE      := $(shell git show -s --format=%ci HEAD)
VERSION_MAJOR := $(shell awk '{gsub(/(v*|\..*)/,"");print}' <<< $(VERSION))
LIBGRAT       := /usr/local/include/libgrat$(VERSION_MAJOR).a

# ifort settings
ifeq ($(FC),ifort)
	FFLAGS               := $(OPT) -xHost -warn all
	ENABLE_PREPROCESSING := -fpp
	NETCDFF :=                                            \
		-I/usr/local/include -L/usr/local/lib64 -lnetcdf     \
		-I/home/mrajner/src/netcdf-fortran/fortran/          \
		-L/home/mrajner/src/netcdf-fortran/fortran -lnetcdff
endif

#  gfortran settings
ifeq ($(FC),gfortran)
	FFLAGS               := $(OPT)
ifdef PEDANTIC
	FFLAGS += -Werror -Wno-character-truncation -std=f2008 -Wall -pedantic -fcheck=all -fbounds-check -fcheck=bounds -Warray-bounds
endif
	ENABLE_PREPROCESSING := -cpp
	NETCDFF   := -I/usr/lib64/gfortran/modules/ -lnetcdf -lnetcdff
endif

FPP :=                                            \
	$(ENABLE_PREPROCESSING)                          \
	-D__CDATE__='"$(DATE)"'                          \
	-D__GDATE__='"$(GIT_DATE)"'                      \
	-D__GRAT_VERSION__='"$(VERSION)"'                \
	-D__FFLAGS__='"$(filter-out -L% -I%,$(FFLAGS))"' \
	-D__COMPILER__='"$(FC_INFO)"' 

SRCS :=                 \
  mod_constants.f90     \
  mod_utilities.f90     \
  mod_cmdline.f90       \
  mod_printing.f90      \
  mod_spherical.f90     \
  mod_mjd.f90           \
  mod_normalization.f90 \
  mod_atmosphere.f90    \
  mod_aggf.f90          \
  mod_polygon.f90       \
  mod_data.f90          \
  mod_date.f90          \
  mod_site.f90          \
  mod_3d.f90            \
  mod_green.f90         \
  mod_admit.f90         \
  mod_parser.f90        

PROGSSRCS :=                  \
						grat.f90          \
						value_check.f90   \
						polygon_check.f90

OBJS            := $(SRCS:.f90=.o)
MOD             := $(SRCS:.f90=.mod)
PROGS_DIR       := ../bin
PROGS           := $(addprefix $(PROGS_DIR)/,$(PROGSSRCS:.f90=))
PROGS_SHORTCUTS := $(PROGSSRCS:.f90=)
DIRS            := $(PROGS_DIR)

all: $(DIRS) libgrat.a $(PROGS)

# aaa: ../bin/grat
# 	bash /home/mrajner/pub/2017_rlg_loading/t.sh
#
fdsf: grat
	$(PROGS_DIR)/grat -F /home/mrajner/pub/2016_hornsund/load/aas/CMB_2003-2013_3km.nc.grd @EWT:TOTALMBACC  -M2 -G /home/mrajner/src/gotic2/data/grn1.data @GR:1:2,@GHN : 1: 3 -Sh  -wn -I4@DD
	$(PROGS_DIR)/grat -F /home/mrajner/pub/2016_hornsund/load/aas/CMB_2003-2013_3km.nc.grd @EWT:TOTALMBACC  -M2 -G /home/mrajner/src/gotic2/data/grn1.data@GHN : 1: 3 -Sh  -ws  -I111@DD


$(DIRS):
	type $(FC) && mkdir -p $@

exit:
	-value_check                                                 \
		-Sj                                                         \
		-D2013                                                      \
		-F ERA@T : t2m , /home/mrajner/dat/erainterim/sp.2013.nc:sp \
    -V
	#  -value_check \
		#  -Sj \
		#  -D2013 \
		#  -F ERA@T : v2t , /home/mrajner/dat/erainterim/sp.2013.nc:sp -V

xxx:
#  tm: value_check
	#  time \
		#  { value_check -q -F  check/data/air.2012.nc@SP : air  -Sg:10 -wn -Dm:3@D -J1000,950  -otmp:level -G@GN ; }

#  #  ra: grat  value_check
	#  #  time \
		#  #  { grat -F  check/data/air.2012.nc@SP : air  -M2,1 -wn,t -Sj,r,o:g -V -Dm:3@D -J1000,950  -otmp:level -G@GN ; }


#  #  local:
	#  #  file=$$(basename $$(ls /home/mrajner/dat/ispd/hourly/___*WARSZAWA*));                               \
	#  #  grep $$file /home/mrajner/dat/ispd/hourly/sites.sta |awk '{print "aaa" , $$2, $$3}' > local_site  ; \
	#  #  cat /home/mrajner/dat/ispd/hourly/$${file} | awk '{print $$1, $$3}' > local

F= -F ./test/data/test_data.nc @SP :sp 
sdf:
	#  @../bin/grat      \
		#  -S j                     \
		#  -M1,2                    \
		#  -F                       \
		#  VIENNA@RSP,              \
		#  NCEP@SP,                 \
		#  NCEP1 @T ,               \
		#  NCEP@H,                  \
		#  NCEP@HP ,                \
		#  NCEP @LS                 \
		#  -BN,I                    \
		#  -G@GN                    \
		#  -H                       \
		#  -I-2@DD : 0.21@DB:140@DE \
		#  -D2009:20090102          \
		#  -o : free -A  -H

dgrat: grat value_check
	../bin/grat -S o          \
		-M1,2                             \
		-F                                \
		NCEP@SP ,                         \
		NCEP1 @T ,                        \
		NCEP@H,                           \
		NCEP@HP ,                         \
		NCEP @LS                          \
	-BN,I                               \
	-G@GN,@GE , @GNc                    \
	-H -I-2@DD : 0.21@DB:140@DE  -D2009 \
	-o  

clean:
	rm -fv *{.o,.mod,.a,so} $(FC)/* fort??????
	
INSTALL_FILES :=                             \
  $(addprefix $(INCDIR)/,$(MOD))             \
  $(addprefix $(BINDIR)/,$(PROGS_SHORTCUTS)) \
  $(LIBDIR)/libgrat.a             
    
install: $(INSTALL_FILES)

uninstall: 
	$(call colorecho,1,$@)
	rm -f $(INSTALL_FILES)

$(LIBDIR)/libgrat.a: libgrat.a
	cp -v $< $@
$(LIBDIR)/libgrat$(VERSION_MAJOR).a: $(LIBDIR)/libgrat.a
	ln -sf $< $@

$(INCDIR)/%.mod: %.mod
	cp -v $< $@

$(BINDIR)/%: $(PROGS_DIR)/%
	cp -v $< $@

%.o: %.f90
	$(call colorecho,2,$@)
	$(FC) $(FFLAGS) $< -c -o $@ $(FPP) $(NETCDFF) 

.PHONY: $(PROGS_SHORTCUTS)
$(PROGS_SHORTCUTS):
	make --no-print-directory $(addprefix $(PROGS_DIR)/,$(@))

$(PROGS_DIR)/%: %.f90 libgrat.a Makefile
	$(call colorecho,3,$@)
	$(FC)           \
		$(FFLAGS)      \
		$(FPP)         \
		$<             \
		-o $@          \
		-L. -lgrat \
    $(NETCDFF)

libgrat.a: libgrat$(VERSION_MAJOR).a 
	ln -sf $(realpath $<) $@

libgrat$(VERSION_MAJOR).a: $(OBJS)
	$(call colorecho,4,$@)
	ar rusv $@ $(OBJS)

.PHONY: install 
test: all install
	make -C test runtests FC=$(FC) OPT=$(OPT)

#  module dependencies
ifndef NODEP
-include mod_dep.makefile
mod_dep.makefile: mod_dep.awk
	awk -f $< > $@
endif

#  # help files
#  HELP_NAMES=grat.hlp polygon_check.hlp value_check.hlp
#  HELPS=$(addprefix ../dat/, $(HELP_NAMES))
#  helps: $(HELPS)
#  $(HELPS): $(PROGS)
#  %.hlp:
	#  $(patsubst ../dat%,../bin%,$(@:.hlp=)) -h  > $@

.FORCE:

OBSOLETE:
#  # documentation
#  dochtml: doc
	#  firefox ../doc/html/index.html &
#  doc:
	#  make -C ../doc/figures/
	#  cd ../doc  &&  doxygen Doxyfile
#  docpdf: doc
	#  cd ../doc/latex/  &&  pdflatex -shell-escape refman
	#  evince ../doc/latex/refman.pdf &
#  install-doc: doc
	#  cp -vur ../doc/man/man3 /home/mrajner/.local/share/man/ 
#  .EXPORT_ALL_VARIABLES:
#

#  define colorecho
#  @tput setaf $1
#  @echo -n "$2: "
#  @tput sgr0
#  endef

# %.o: %.mod
