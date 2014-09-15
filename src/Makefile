FC := ifort
OPT := -O0

WITH_MONTE_CARLO := yes

	#  $(FC) -c ~/src/fortranlib/src/lib_random.f90 -o lib_random$(SUFFIX).o
#  FC := gfortran

#  WITH_MONTE_CARLO := ~/src/fortranlib/src/lib_random.f90

# user definitions
LIBDIR    := /usr/local/lib
INCDIR    := /usr/local/include
BINDIR    := /usr/local/bin
LINKS_DIR := ~/.local/bin
SUFFIX    := _$(FC)$(OPT)



FC_INFO := $(shell $(FC) --version | sed -n 1p )
HOST    := $(shell hostname)
DATE    := $(shell date "+%Y-%m-%d %H:%M:%S")

VERSION       := $(shell git describe --match "v*")
VERSION_MAJOR := $(shell awk '{gsub(/(v*|\..*)/,"");print}' <<< $(VERSION))

LIBGRAT := /usr/local/include/libgrat$(VERSION_MAJOR)$(SUFFIX).a

# ifort settings
ifeq ($(FC),ifort)
FFLAGS :=                                           \
				 -fpic                                      \
				 $(OPT)                                     \
				 -xHost                                     \
				 -warn all                                  \
				 -I/home/mrajner/src/netcdf-fortran/fortran \
				 -L/home/mrajner/src/netcdf-fortran/fortran 

				 #  -check all

ENABLE_PREPROCESSING := -fpp
NETCDFF := -lnetcdff
endif

# gfortran settings
ifeq ($(FC),gfortran)
FFLAGS  := -O0 \
				 -I/home/mrajner/src/netcdf-gfortran/fortran 
ENABLE_PREPROCESSING = -cpp
NETCDFF := /home/mrajner/src/netcdf-gfortran/fortran/libnetcdff.so.6.0.1
LD_LIBRARY_PATH=/home/mrajner/src/netcdf-gfortran/fortran
endif

ifdef WITH_MONTE_CARLO
FFLAGS += -DWITH_MONTE_CARLO
endif
FPP :=                                             \
	$(ENABLE_PREPROCESSING)                          \
	-D__CDATE__='"$(DATE)"'                          \
	-D__VERSION__='"$(VERSION)"'                     \
	-D__FFLAGS__='"$(filter-out -L% -I%,$(FFLAGS))"' \
	-D__COMPILER__='"$(FC_INFO)"' 


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
rgrat: grat value_check
	@time { ../bin/grat$(SUFFIX) \
		-Sj                        \
		-M1,2                        \
		-F                         \
		VIENNA@RSP,                \
		VIENNA@HRSP,               \
		NCEP@SP,                   \
		NCEP1 @T ,                 \
		NCEP@H,                    \
		NCEP@HP ,                  \
		NCEP @LS                   \
		-BN,I                      \
		-G @GN, @GE                     \
		-U                         \
		-H                         \
		-C                         \
		-D2009:2009010206          \
		-o : free -A 4 -H ; }
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
	../bin/grat_$(SUFFIX) -S o              \
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


SRCS      := \
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

install:                                     \
	$(LIBDIR)/libgrat$(SUFFIX).a               \
	$(addprefix $(BINDIR)/,$(PROGS_SHORTCUTS)) \
	$(addprefix $(INCDIR)/,$(MOD))
	
uninstall: 
	-rm -f                                       \
		$(addprefix $(INCDIR)/,$(MOD))             \
		$(addprefix $(BINDIR)/,$(PROGS_SHORTCUTS)) \
		/usr/local/lib/libgrat$(SUFFIX)*      

$(LIBDIR)/libgrat$(VERSION_MAJOR).a: libgrat$(SUFFIX)$(VERSION_MAJOR).a
	cp -uv $< $@

$(LIBDIR)/libgrat$(SUFFIX).a: $(LIBDIR)/libgrat$(SUFFIX)$(VERSION_MAJOR).a
	ln -sf $< $@
$(INCDIR)/%.mod: %.mod
	cp -uv $< $@
$(BINDIR)/%: $(PROGS_DIR)/%_$(FC)
	cp -uv $< $@

%$(SUFFIX).o: %.f90
	$(FC) $(FFLAGS) $< -c -o $@  -I ~/src/fortranlib/src $(FPP)

.PHONY: $(PROGS_SHORTCUTS)
$(PROGS_SHORTCUTS):
	make --no-print-directory $(addprefix $(PROGS_DIR)/,$(@)$(SUFFIX))

ifdef WITH_MONTE_CARLO
$(PROGS_DIR)/%$(SUFFIX):%.f90 libgrat$(SUFFIX).a lib_random$(SUFFIX).o
	$(FC) $(FFLAGS) $(FPP)          \
	$< -o $@ -L. -lgrat$(SUFFIX) $(NETCDFF) lib_random$(SUFFIX).o 
lib_random$(SUFFIX).o:  ~/src/fortranlib/src/lib_random.f90 
	$(FC) $(FFLAGS) $< -c -o $@$
endif
$(PROGS_DIR)/%$(SUFFIX):%.f90 libgrat$(SUFFIX).a 
	$(FC) $(FFLAGS) $(FPP)          \
	$< -o $@ -L. -lgrat$(SUFFIX) $(NETCDFF) 


libgrat$(SUFFIX).a: libgrat$(SUFFIX)$(VERSION_MAJOR).a
	ln -sf libgrat$(SUFFIX)$(VERSION_MAJOR).a libgrat$(SUFFIX).a

libgrat$(SUFFIX)$(VERSION_MAJOR).a: $(OBJS)
	ar rusv $@ $(OBJS)

.PHONY: install 
test: all install
	make -C test

#  shared_libraries: $(OBJS)
	#  $(FC) -shared $(OBJS) -o libgrat$(VERSION_MAJOR).so
#  install_shared)library:
	#  cp -uv libgrat.so /usr/local/lib

# module dependencies
ifndef NODEP
include mod_dep.makefile
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
.EXPORT_ALL_VARIABLES:


