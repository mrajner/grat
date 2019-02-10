#!/bin/bash - 
#===============================================================================
#          FILE: definitions.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 30.06.2014 21:45
#===============================================================================

set -o nounset                              # Treat unset variables as an error

[[ -n ${SP-} ]] && 
{
  echo not sourcing definitions.sh...
} || {

[[ ! -z ${2:-} ]] && { echo "verbose mode" ; VERBOSE=true ; }

ifdatafileexist(){
  for co in  file SP T VT GP LS HP H VSH ; do
    [[ ! -z ${!co-} ]] && 
    {
      file=${!co%%@*}
      file=${file// }
      [[ -f $file  ]] || 
      {
        [[ ${VERBOSE-} ]] && echo $file do not exist
        exit 0
      }
    } || : 
  done
}

case ${1:-synthetic_data} in
  real_data|r)
    [[ ${VERBOSE-} ]] && echo selected real
    file="../data/land.nc"
    SP="../data/pres.sfc.2012.nc  @SP  :pres"
    T="../data/air.sig995.2012.nc @T   :air"
    VT="../data/air.2012.nc       @VT  :air "
    GP="../data/hgt.2012.nc       @GP  :hgt"
    LS="../data/land.nc           @LS  :land"
    HP="../data/hgt.sfc.nc        @HP  :hgt"
    H="../data/hgt.sfc.nc         @H   :hgt"
    VSH="../data/shum.2012.nc     @VSH :shum"
    EWT="../data/pres.sfc.2012.nc  @EWT:pres"
    suffix=".r"

    ifdatafileexist
    ;;

  synthetic_data|s)
    [[ ${VERBOSE-} ]] && echo selected synthetic_data
    file="  ../data/test_data.nc  "
    VT="../data/test_data.nc@VT : vt "
    T="../data/test_data.nc @T: t"
    GP="../data/test_data.nc @GP : gp"
    SP="../data/test_data.nc @SP : sp"
    EWT="../data/test_data.nc @EWT : sp"
    LS="../data/test_data.nc @LS : ls"
    LSWT="../data/test_data.nc @LS : ls_withtimedimension"
    # HP="../data/test_data.nc @HP : hgt"
    # H="../data/test_data.nc @H  : hgt"
    # VSH="../data/test_data.nc @VSH: shum"
    suffix=".s"

    ifdatafileexist
    ;;

  constant_values|c)
    SP="1013.25 @ SP:@scale=100"
    T=" 15      @ T :@offset=273.15"
    GP="1e5 @GP "
    VT="0@VT"
    LS="0@LS"
    HP="0@HP"
    H="0@H"
    VSH="0@VSH"
    EWT="124@EWT"
    suffix=".c"
    EWT="1000@EWT"
    ;;

  *)
    echo wrong args in definitions.sh
    exit 1
esac
}
