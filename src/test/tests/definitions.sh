#!/bin/bash - 
#===============================================================================
#          FILE: definitions.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 30.06.2014 21:45
#===============================================================================

set -o nounset                              # Treat unset variables as an error

[[ -n ${SP:-} ]] && 
{
  echo not sourcing definitions.sh...
} || {

case ${1:-synthetic_data} in
  real_data|r)
    file="../data/land.nc"
    SP="../data/pres.sfc.2012.nc  @SP  :pres"
    T="../data/air.sig995.2012.nc @T   :air"
    VT="../data/air.2012.nc       @VT  :air "
    GP="../data/hgt.2012.nc       @GP  :hgt"
    LS="../data/land.nc           @LS  :land"
    HP="../data/hgt.sfc.nc        @HP  :hgt"
    H="../data/hgt.sfc.nc         @H   :hgt"
    VSH="../data/shum.2012.nc     @VSH :shum"
    suffix=".r"
    ;;

  synthetic_data|s)
    file="../data/test_data.nc"
    VT="../data/test_data.nc @VT : vt "
    T="../data/test_data.nc @T: t"
    GP="../data/test_data.nc @GP : gp"
    SP="../data/test_data.nc @SP : sp"
    LS="../data/test_data.nc @LS : land"
    HP="../data/test_data.nc @HP : hgt"
    H="../data/test_data.nc @H  : hgt"
    VSH="../data/test_data.nc @VSH: shum"
    suffix=".s"
    ;;

  constant_values|c)
    SP="1013.25 @ SP:@scale=100"
    T=" 15      @ T :@offset=273.15"
    GP="1e5 @GP "
    VT="0@VT"

    suffix=".c"
    ;;
  *)
    echo wrong args in definitions.sh
    exit 1
esac
}
