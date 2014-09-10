FC := ifort
FC := gfortran
#
#
F= -F ./test/data/test_data.nc @SP :sp 


fdf: tt all
	../bin/value_check_$(FC)  -! -Sj $(F)  -D20120101:1@D , 2010 , 1999:1@D   2>/dev/null

tt: .FORCE
	touch grat.f90 value_check.f90

d:
#  dfd: grat
	#  grat -o /dev/null -Sj -F10@SP -D2011:20110102 -L/dev/null@p, fdf@s

#  dgrat: grat value_check
	#  grat -S o                           \
		#  -M1,2                             \
		#  -F NCEP@SP ,                      \
		#  NCEP1 @T ,                        \
		#  NCEP@H,                           \
		#  NCEP@HP ,                         \
		#  NCEP @LS                          \
	#  -BN,I                               \
	#  -G@GN,@GE , @GNc                    \
	#  -H -I-2@DD : 0.21@DB:140@DE  -D2012 \
	#  -o aa 


# user definitions
LIBDIR    := /usr/local/lib
INCDIR    := /usr/local/include
BINDIR    := /usr/local/bin
LINKS_DIR := ~/.local/bin

FC_INFO := $(shell $(FC) --version | sed -n 1p)
HOST    := $(shell hostname)
DATE    := $(shell date "+%Y-%m-%d %H:%M:%S")

VERSION := $(shell git describe --match "v*")
VERSION_MAJOR := $(shell awk '{gsub(/(v*|\..*)/,"");print}' <<< $(VERSION))

LIBGRAT := /usr/local/include/libgrat$(VERSION_MAJOR)_$(FC).a

# ifort settings
ifeq ($(FC),ifort)
FFLAGS :=          \
				 -fpic     \
				 -O0       \
				 -xHost    \
				 -warn all \
				 -I/home/mrajner/src/netcdf-fortran/fortran \
				 -L/home/mrajner/src/netcdf-fortran/fortran

ENABLE_PREPROCESSING = -fpp
NETCDFF = -lnetcdff
MOD_OUT = -module
endif

# gfortran settings
ifeq ($(FC),gfortran)
FFLAGS  := -O0 \
				 -I/home/mrajner/src/netcdf-gfortran/fortran \
				 -L/home/mrajner/src/netcdf-gfortran/fortran
ENABLE_PREPROCESSING = -cpp
NETCDFF = -lnetcdff
MOD_OUT = -J
endif

FPP :=                         \
	$(ENABLE_PREPROCESSING)      \
	-D__CDATE__='"$(DATE)"'      \
	-D__VERSION__='"$(VERSION)"' \
	-D__FFLAGS__='"$(FFLAGS)"'   \
	-D__COMPILER__='"$(FC_INFO)"' 

#  #  tm: value_check
	#  #  time \
		#  #  { value_check -q -F  check/data/air.2012.nc@SP : air  -Sg:10 -wn -Dm:3@D -J1000,950  -otmp:level -G@GN ; }

#  #  ra: grat  value_check
	#  #  time \
		#  #  { grat -F  check/data/air.2012.nc@SP : air  -M2,1 -wn,t -Sj,r,o:g -V -Dm:3@D -J1000,950  -otmp:level -G@GN ; }


#  #  local:
	#  #  file=$$(basename $$(ls /home/mrajner/dat/ispd/hourly/___*WARSZAWA*));                               \
	#  #  grep $$file /home/mrajner/dat/ispd/hourly/sites.sta |awk '{print "aaa" , $$2, $$3}' > local_site  ; \
	#  #  cat /home/mrajner/dat/ispd/hourly/$${file} | awk '{print $$1, $$3}' > local

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

OBJS            := $(addprefix $(FC)/,$(SRCS:.f90=.o))
MOD             := $(SRCS:.f90=.mod)
PROGS_DIR       := ../bin
PROGS           := $(addsuffix _$(FC),$(addprefix $(PROGS_DIR)/,$(PROGSSRCS:.f90=)))
PROGS_SHORTCUTS := $(PROGSSRCS:.f90=)

all: $(FC) $(FC)/libgrat.a $(PROGS)

clean:
	rm -fv {$(FC)/,}*{.o,.mod,.a,so} fort?????? $(PROGS_DIR)/*_$(FC) 
	
# local installation of executables
LINKS = $(addprefix $(LINKS_DIR)/,$(PROGSSRCS:.f90=))
$(LINKS_DIR)/%: $(PROGS_DIR)/%_$(FC)
	ln -sf $(realpath $<) $@
link: $(LINKS)
unlink: 
	-rm -f $(LINKS)

install:                                     \
	$(LIBDIR)/libgrat$(VERSION_MAJOR).a        \
	$(LIBDIR)/libgrat.a                        \
	$(addprefix $(BINDIR)/,$(PROGS_SHORTCUTS)) \
	$(addprefix $(INCDIR)/,$(MOD))
	
uninstall: 
	-rm -f                                       \
		$(addprefix $(INCDIR)/,$(MOD))             \
		$(addprefix $(BINDIR)/,$(PROGS_SHORTCUTS)) \
		/usr/local/lib/libgrat*      

$(LIBDIR)/libgrat$(VERSION_MAJOR).a: libgrat$(VERSION_MAJOR).a
	cp -uv $< $@
$(LIBDIR)/libgrat.a: $(LIBDIR)/libgrat$(VERSION_MAJOR).a
	ln -sf $< $@
$(INCDIR)/%.mod: %.mod
	cp -uv $< $@
$(BINDIR)/%: $(PROGS_DIR)/%
	cp -uv $< $@

$(FC):
	mkdir -pv $(FC)

$(FC)/%.o: %.f90
	$(FC) $(FFLAGS) $< -c -o $@  $(MOD_OUT) $(FC)

.PHONY: $(PROGS_SHORTCUTS)
$(PROGS_SHORTCUTS):
	@make --no-print-directory $(addprefix $(PROGS_DIR)/,$@)

$(PROGS_DIR)/%_$(FC):%.f90 $(FC)/libgrat.a 
ifeq ($(FC),ifort)
	cd /usr/local/lib64/ && sudo ln -sf /home/mrajner/src/netcdf-fortran/fortran/libnetcdff.so.6 .
endif
ifeq ($(FC),gfortran)
	cd /usr/local/lib64/ && sudo ln -sf /home/mrajner/src/netcdf-gfortran/fortran/libnetcdff.so.6 .
endif
	$(FC) $(FFLAGS) $(FPP)          \
	$< -o $@ -I$(FC) -L$(FC) -lgrat $(NETCDFF)

$(FC)/libgrat.a: $(FC)/libgrat$(VERSION_MAJOR).a
	ln -sf $(realpath $<) $@

$(FC)/libgrat$(VERSION_MAJOR).a: $(OBJS)
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
#
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
