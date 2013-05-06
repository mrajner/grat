#!/bin/bash - 
#===============================================================================
#
#          FILE: podklad_do_convolution.sh
#         USAGE: ./podklad_do_convolution.sh 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#        AUTHOR: mrajner
#       CREATED: 27.04.2013 14:09:47 CEST
#===============================================================================

set -o nounset                              # Treat unset variables as an error


R=0/3/0/3
I=1.0
NAME=podklad ;

for i in 1 2
do
  if [[ $i -eq 1 ]];then
    NAME_out=podklad;E=""; Z=""
  else
    NAME_out=podklad2; E=-E200;Z="-I0.01 -Q"
  fi
  #grdmath \
    #  -F \
    #  -R${R} -I${I} \
    #  0 1 RAND \
    #  = ${NAME}_F.grd

  grdsample ${NAME}_F.grd ${Z} -G${NAME}.grd


  grd2xyz ${NAME}.grd > ${NAME}.dat


  grd2cpt ${NAME}.grd -E8 > ${NAME}.cpt
  makecpt -Cjet -T-0.7/1.3/.1 > ${NAME}.cpt

  grdimage             \
    ${NAME}.grd        \
    -C${NAME}.cpt      \
    $E \
    -JX3c > ${NAME_out}.ps

  #grdview             \
    #  ${NAME}.grd        \
    #  -C${NAME}.cpt      \
    #  -K -O -X11 \
    #  -JM10c \
    #  -T \
    #>> ${NAME}.ps

  #psscale \
    #  -C${NAME}.cpt      \
    #  -D10/2/4/1 \
    #  -O \
    #  >> ${NAME}.ps


  ps2raster\
    -P -Tf -A ${NAME_out}.ps

done
