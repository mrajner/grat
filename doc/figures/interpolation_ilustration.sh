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

for co in n l
do
  value_check                       \
    -F $grd:z:x:y                   \
    -S 9/10/9/10:0.04 -I ${co} @ I \
    -o interp${co}1.dat             \
    -L interp1.dat@l 
done

perl -n -i -e 'print if $. <= 4' interp1.dat

