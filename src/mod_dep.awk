#!/usr/bin/awk -f
BEGIN{

  # read in module list into MODULES
  while("ls mod_*.f90" | getline){
    MODULES[++i] = $0
  }
  close("ls mod_*.f90")

  for (i in MODULES) {

    printf gensub("(.*).f90","$(FC)/\\1.o $(FC)/\\1.mod:","g",MODULES[i])

    while(getline < MODULES[i]){
      if (/^[^!]*\<use\>\s+\<mod/){
        DEPS[MODULES[i]][gensub(/(^.*)(mod_[[:alnum:]]*).*/,"\\2","g",$0)]++
      }
    }

    if(length(DEPS[MODULES[i]])>0){
      for (dependency in DEPS[MODULES[i]]){
        printf gensub("(.*)"," $(FC)/\\1.o $(FC)/\\1.mod","g",dependency)
      }
    }
    printf "\n"
  }
}
