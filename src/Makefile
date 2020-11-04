# system wide $FC
# if not set use gfortran
FC := gfortran

# TODO
# optimalization
OPT=-O0 -g

# user definitions
# you may want to change this settings
LIBDIR := /usr/local/lib
INCDIR := /usr/local/include
BINDIR := /usr/local/bin

# passing compilation info to program
HOST          := $(shell hostname ; uname -r)
DATE          := $(shell date "+%Y-%m-%d %H:%M:%S %z")
VERSION       := $(shell git describe --match "v*")
GIT_DATE      := $(shell git show -s --format=%ci HEAD)
VERSION_MAJOR := $(shell awk '{gsub(/(v*|\..*)/,"");print}' <<< $(VERSION))
LIBGRAT       := /usr/local/include/libgrat$(VERSION_MAJOR).a

# # ifort settings
# ifeq ($(FC),ifort)
# 	FFLAGS               := $(OPT) -xHost -warn all
# 	ENABLE_PREPROCESSING := -fpp
# 	NETCDFF :=                                            \
# 		-I/usr/local/include -L/usr/local/lib64 -lnetcdf     \
# 		-I/home/mrajner/src/netcdf-fortran/fortran/          \
# 		-L/home/mrajner/src/netcdf-fortran/fortran -lnetcdff
# endif

#  gfortran settings
ifeq ($(FC),gfortran)
	FFLAGS               := $(OPT)
ifdef PEDANTIC
	FFLAGS += -Werror -Wno-character-truncation -std=f2008 -Wall -pedantic -fcheck=all -fbounds-check -fcheck=bounds -Warray-bounds
endif
	ENABLE_PREPROCESSING := -cpp
	NETCDFFINC           := -I/usr/include
	NETCDFFLIB           := -lnetcdf -lnetcdff
endif

ifdef STATIC
NETCDFFLIB      :=                           \
	-static                                   \
	-lnetcdff                                 \
	-lnetcdf                                  \
	-lhdf5_hl                                 \
	-lhdf5                                    \
	-L/home/mrajner/src/szip/src/.libs/  -lsz \
	-L/home/mrajner/src/curl/lib -lcurl       \
	-L/usr/lib64/hdf/ -lmfhdf                 \
	-L/usr/lib64/hdf/ -ldf                    \
	-L/home/mrajner/src/libjpeg-turbo/ -ljpeg \
	-lm                                       \
	-lz                                       \
	-lpthread                                 \
	-lssl -lcrypto                            \
	-ldl
endif

FPP :=                                            \
	$(ENABLE_PREPROCESSING)                          \
	-D__CDATE__='"$(DATE)"'                          \
	-D__GDATE__='"$(GIT_DATE)"'                      \
	-D__GRAT_VERSION__='"$(VERSION)"'                \
	-D__FFLAGS__='"$(filter-out -L% -I%,$(FFLAGS))"' \
	-D__HOST__='"$(HOST)"' 

SRCS :=                 \
  mod_cmdline.f90       \
  mod_printing.f90      \
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

aaa: ../bin/grat
	bash /home/mrajner/pub/2017_rlg_loading/t.sh
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

$(LIBDIR)/libgrat.a: $(LIBDIR)/libgrat$(VERSION_MAJOR).a
	ln -sf $< $@
$(LIBDIR)/libgrat$(VERSION_MAJOR).a: libgrat.a
	install $< $@

$(INCDIR)/%.mod: %.mod
	install -m 444 $< $@

$(BINDIR)/%: $(PROGS_DIR)/%
	install $< $@

%.o: %.f90
	$(call colorecho,2,$@)
	$(FC) $(FFLAGS) $< -c -o $@ $(FPP) $(NETCDFFINC) \
		-I$(INCDIR) 

.PHONY: $(PROGS_SHORTCUTS)
$(PROGS_SHORTCUTS):
	make --no-print-directory $(addprefix $(PROGS_DIR)/,$(@))

$(PROGS_DIR)/%: %.f90 libgrat.a 
	$(call colorecho,3,$@)
	$(FC)           \
		$(FFLAGS)      \
		$(FPP)         \
		$<             \
		-o $@          \
		-L. -lgrat \
    $(NETCDFFLIB) \
		-I$(INCDIR) \
		-lmr

libgrat.a: libgrat$(VERSION_MAJOR).a 
	ln -sf $(realpath $<) $@

libgrat$(VERSION_MAJOR).a: $(OBJS)
	$(call colorecho,4,$@)
	ar rusv $@ $(OBJS)

.PHONY: install 
test: all install
	make -C test runtests FC=$(FC) OPT="$(OPT)"

include dep.makefile
dep.makefile: 
	makedepf90 *f90 > $@

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
