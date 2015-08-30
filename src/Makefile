#  set fortranc compiler or export system wide $FC
#  FC  := gfortran
#  FC  := ifort

# user definitions
LIBDIR        := /usr/local/lib
INCDIR        := /usr/local/include
BINDIR        := /usr/local/bin
LINKS_DIR     := ~/.local/bin
SUFFIX        := _$(FC)$(OPT)

# passing compilation info to program
FC_INFO       := $(shell $(FC) --version | sed -n 1p )
HOST          := $(shell hostname ; uname -r)
DATE          := $(shell date "+%Y-%m-%d %H:%M:%S")
VERSION       := $(shell git describe --match "v*")
VERSION_MAJOR := $(shell awk '{gsub(/(v*|\..*)/,"");print}' <<< $(VERSION))
LIBGRAT       := /usr/local/include/libgrat$(VERSION_MAJOR)$(SUFFIX).a

# ifort settings
ifeq ($(FC),ifort)
	FFLAGS               := $(OPT) -xHost -warn all
	ENABLE_PREPROCESSING := -fpp
	#  NETCDFF := -lnetcdf -I /home/mrajner/src/netcdf-fortran/fortran/ -L /home/mrajner/src/netcdf-fortran/fortran/ -lnetcdff
	  NETCDFF := -lnetcdff
endif

# gfortran settings
ifeq ($(FC),gfortran)
	FFLAGS               := $(OPT)
	ENABLE_PREPROCESSING := -cpp
	NETCDFF := -lnetcdf -I/home/mrajner/src/netcdf-gfortran/fortran/ -L/home/mrajner/src/netcdf-gfortran/fortran/ -lnetcdff
endif

	NETCDFF := -I/usr/lib64/gfortran/modules/ -L/usr/lib64/ -lnetcdf -lnetcdff

FPP :=                                             \
	$(ENABLE_PREPROCESSING)                          \
	-D__CDATE__='"$(DATE)"'                          \
	-D__GRAT_VERSION__='"$(VERSION)"'                \
	-D__FFLAGS__='"$(filter-out -L% -I%,$(FFLAGS))"' \
	-D__COMPILER__='"$(FC_INFO)"' 

A: value_check
	value_check -F /home/mrajner/dat/gldas/GLDAS_NOAH10_M.A200101_201501.totalH2O.nc : Water_Thickness : Longitude : Latitude  -V -Sj,b,l
	

BUG: grat
	grat -! -H -F test/data/test_data.nc @SP : sp \
		-M2 -G @GN -Sj -D2012:6@H

timeele:
	value_check                                                  \
		-F /home/mrajner/dat/erainterim/gp.nc  ,ERA@H ,10@SP \
		-V -Sj,b,g:10                                           \
		-G@GN -D2010:4@D -L@p -H

exit:
	-value_check \
		-Sj \
		-D2013 \
		-F ERA@T : t2m , /home/mrajner/dat/erainterim/sp.2013.nc:sp -V
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


SRCS      :=                      \
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
PROGS           := $(addsuffix $(SUFFIX),$(addprefix $(PROGS_DIR)/,$(PROGSSRCS:.f90=)))
PROGS_SHORTCUTS := $(PROGSSRCS:.f90=)

all: libgrat$(SUFFIX).a $(PROGS)

clean:
	rm -fv *$(SUFFIX){.o,.mod,.a,so} fort??????
	
# local installation of executables
LINKS = $(addprefix $(LINKS_DIR)/,$(PROGSSRCS:.f90=))
$(LINKS_DIR)/%: $(PROGS_DIR)/%$(SUFFIX) .FORCE
	ln -sf $(realpath $<) $@
link: $(LINKS)
unlink: 
	-rm -f $(LINKS)

install:                         \
	$(LIBDIR)/libgrat.a            \
	$(addprefix $(INCDIR)/,$(MOD)) \
	$(addprefix $(BINDIR)/,$(PROGS_SHORTCUTS)) 
	
uninstall: 
	$(call colorecho,1,$@)
	-rm -f                                       \
		$(addprefix $(INCDIR)/,$(MOD))             \
		$(addprefix $(BINDIR)/,$(PROGS_SHORTCUTS)) \
		/usr/local/lib/libgrat.*      

$(LIBDIR)/libgrat.a: $(LIBDIR)/libgrat$(VERSION_MAJOR).a
	ln -sf $< $@
$(LIBDIR)/libgrat$(VERSION_MAJOR).a: $(LIBDIR)/libgrat$(SUFFIX).a
	ln -sf $< $@
$(LIBDIR)/libgrat$(SUFFIX).a: libgrat$(SUFFIX).a
	cp -v $< $@

$(INCDIR)/%.mod: %.mod
	cp -v $< $@
$(BINDIR)/%: $(PROGS_DIR)/%$(SUFFIX)
	cp -v $< $@

%$(SUFFIX).o: %.f90
	$(call colorecho,2,$@)
	$(FC) $(FFLAGS) $< -c -o $@ $(FPP) $(NETCDFF)

.PHONY: $(PROGS_SHORTCUTS)
$(PROGS_SHORTCUTS):
	make --no-print-directory $(addprefix $(PROGS_DIR)/,$(@)$(SUFFIX))

$(PROGS_DIR)/%$(SUFFIX):%.f90 libgrat$(SUFFIX).a 
	mkdir -p $(PROGS_DIR)
	$(call colorecho,3,$@)
	$(FC) $(FFLAGS) $(FPP) \
	$< -o $@ -L. -lgrat$(SUFFIX) $(NETCDFF) 

libgrat$(SUFFIX).a: libgrat$(SUFFIX)$(VERSION_MAJOR).a
	ln -sf libgrat$(SUFFIX)$(VERSION_MAJOR).a libgrat$(SUFFIX).a

libgrat$(SUFFIX)$(VERSION_MAJOR).a: $(OBJS)
	$(call colorecho,4,$@)
	ar rusv $@ $(OBJS)

.PHONY: install 
test: all install
	make -C test FC=$(FC) OPT=$(OPT)

# module dependencies
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

define colorecho
@tput setaf $1
@echo -n "$2: "
@tput sgr0
endef

