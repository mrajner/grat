# set fortranc compiler or export system wide $FC
# if not set use gfortran
FC := gfortran

#  a:
	#  make -f tmp.makefile xo
	#
# TODO
# optimalization
OPT=-O0

# user definitions
# you may want to change this
LIBDIR        := /usr/local/lib
INCDIR        := /usr/local/include
BINDIR        := /usr/local/bin
LINKS_DIR     := ~/.local/bin
SUFFIX        := $(OPT)

# passing compilation info to program
FC_INFO       := $(shell $(FC) --version | sed -n 1p )
HOST          := $(shell hostname ; uname -r)
DATE          := $(shell date "+%Y-%m-%d %H:%M:%S")
VERSION       := $(shell git describe --match "v*")
GIT_DATE      := $(shell git show -s --format=%ci HEAD)
VERSION_MAJOR := $(shell awk '{gsub(/(v*|\..*)/,"");print}' <<< $(VERSION))
LIBGRAT       := /usr/local/include/libgrat$(VERSION_MAJOR)$(SUFFIX).a

# ifort settings
ifeq ($(FC),ifort)
	FFLAGS               := $(OPT) -xHost -warn all
	ENABLE_PREPROCESSING := -fpp
	NETCDFF :=                                        \
		-I/usr/local/include -L/usr/local/lib64 -lnetcdf \
		-I/home/mrajner/src/netcdf-fortran/fortran/      \
		-L/home/mrajner/src/netcdf-fortran/fortran -lnetcdff
endif

#  gfortran settings
ifeq ($(FC),gfortran)
	FFLAGS               := $(OPT) -Wall # -pedantic
	ENABLE_PREPROCESSING := -cpp
	NETCDFF   := -I/usr/lib64/gfortran/modules/ -lnetcdf -lnetcdff
endif

FPP :=                                             \
	$(ENABLE_PREPROCESSING)                          \
	-D__CDATE__='"$(DATE)"'                          \
	-D__GDATE__='"$(GIT_DATE)"'                          \
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

OBJS            := $(SRCS:.f90=$(SUFFIX).o)
MOD             := $(SRCS:.f90=.mod)
PROGS_DIR       := ../bin
PROGS           := $(addprefix $(PROGS_DIR)/,$(PROGSSRCS:.f90=))
PROGS_SHORTCUTS := $(PROGSSRCS:.f90=)
DIRS            := $(PROGS_DIR)

pp: value_check
	$< --version

all: $(DIRS) libgrat$(SUFFIX).a $(PROGS)

forf: grat
	grat \
		-Sj -M1 -F1@SP \
		-D2010:2011:2@M 2>&1 a

$(DIRS):
	type $(FC) && mkdir -p $@

<<<<<<< HEAD
B:  ../bin/grat
	make grat FC=$(FC) OPT=-O0
	../bin/grat -D 2010 : 5@D : 22@H 

A: value_check
	../bin/value_check  -F /home/mrajner/dat/gldas/GLDAS_NOAH10_M.A200101_201501.totalH2O.nc : Water_Thickness : Longitude : Latitude  -V -Sj,b,l
  

BUG: grat
	grat -! -H -F test/data/test_data.nc @SP : sp \
		-M2 -G @GN -Sj -D2012:6@H

timeele:
	value_check     \
		-F ERA@H      \
		-V -Sj,b,g:10 \
		-G@GN -D2010:4@D -L@p -H

=======
>>>>>>> master
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
	#  @../bin/grat$(SUFFIX)      \
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
	../bin/grat_$(SUFFIX) -S o          \
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
	
# local installation of executables
LINKS := $(addprefix $(LINKS_DIR)/,$(PROGSSRCS:.f90=))
$(LINKS_DIR)/%: $(PROGS_DIR)/% .FORCE
	ln -sf $(realpath $<) $@
link: $(LINKS) 
	echo $(LINKS)
unlink: 
	-rm -f $(LINKS)

INSTALL_FILES :=                             \
  $(addprefix $(INCDIR)/,$(MOD))             \
  $(addprefix $(BINDIR)/,$(PROGS_SHORTCUTS)) \
  $(LIBDIR)/libgrat.a             
    
install: $(INSTALL_FILES)

uninstall: 
	$(call colorecho,1,$@)
	sudo rm -f $(INSTALL_FILES)

$(LIBDIR)/libgrat.a: libgrat$(SUFFIX).a
	sudo cp -v $< $@
$(LIBDIR)/libgrat$(VERSION_MAJOR).a: $(LIBDIR)/libgrat$(SUFFIX).a
	ln -sf $< $@

$(INCDIR)/%.mod: %.mod
	sudo cp -v $< $@

$(BINDIR)/%: $(PROGS_DIR)/%
	sudo cp -v $< $@

%.o: %.f90
	$(call colorecho,2,$@)
	$(FC) $(FFLAGS) $< -c -o $@ $(FPP) $(NETCDFF) 

.PHONY: $(PROGS_SHORTCUTS)
$(PROGS_SHORTCUTS):
	make --no-print-directory $(addprefix $(PROGS_DIR)/,$(@))

$(PROGS_DIR)/%: %.f90 $(FC)/libgrat.a
	$(call colorecho,3,$@)
	$(FC)                            \
		$(FFLAGS)                       \
		$(FPP)                          \
		$<                              \
		-o $@                           \
		-I$(FC) -L$(FC) -lgrat$(SUFFIX) \
    $(NETCDFF)

$(FC)/libgrat$(SUFFIX).a: $(FC)/libgrat$(SUFFIX)$(VERSION_MAJOR).a 
	ln -sf $(realpath $<) $@

$(FC)/libgrat$(SUFFIX)$(VERSION_MAJOR).a: $(OBJS)
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
