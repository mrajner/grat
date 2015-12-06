#!/bin/bash - 
#===============================================================================
#          FILE: interpolation_ilustration.sh
#         USAGE: ./interpolation_ilustration.sh 
#        AUTHOR: mrajner
#       CREATED: 05.12.2012 10:38:30 CET
#===============================================================================

## \file
set -o nounset

grd=grdfile.nc

grdmath -R9/12/9/12 -I1 Y X RAND = $grd

for VC in /home/mrajner/src/grat/bin/value_check_gfortran-O0 /home/mrajner/src/grat/bin/value_check_ifort-O0 ;do

$VC -v
for co in n l
do
  $VC                       \
    -F ${grd}:z:x:y                   \
    -S 9/10/9/10:0.04 -I ${co} @ I | tee >(wc -l) >(head) >/dev/null
    # -o interp${co}1.dat             \
    # -L interp1.dat@l  -V
done

# perl -n -i -e 'print if $. <= 4' interp1.dat
done
