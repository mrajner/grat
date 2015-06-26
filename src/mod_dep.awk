#!/usr/bin/awk -f

# This awk utility will resolve dependensies of modules basing on
# the use statement in *.f90 files
# \author Marcin Rajner
# \date 2015.06.23

BEGIN{

  # read in module list into MODULES
  while("ls mod_*.f90" | getline){
    MODULES[++i] = $0
  }
  close("ls mod_*.f90")

  for (i in MODULES) {

    printf gensub("(.*).f90","\\1$(SUFFIX).o:","g",MODULES[i])

    while(getline < MODULES[i]){
      if (/^[^!]*\<use\>\s+\<mod/){
        DEPS[MODULES[i]][gensub(/(^.*)(mod_[[:alnum:]]*).*/,"\\2","g",$0)]++
      }
    }

    if (length(DEPS[MODULES[i]])>0){
      for (dependency in DEPS[MODULES[i]]){
        printf gensub("(.*)"," \\1$(SUFFIX).o","g",dependency)
      }
    }
    printf "\n"
  }
}
