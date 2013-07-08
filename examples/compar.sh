#!/bin/bash - 
#===============================================================================
#
#          FILE: compar.sh
#         USAGE: ./compar.sh 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#        AUTHOR: mrajner
#       CREATED: 06.07.2013 22:19:01 CEST
#===============================================================================

set -o nounset                              # Treat unset variables as an error

green=(green_newtonian_olsson.dat green_newtonian_spotl.dat)
for co in 0 1
do
  if [ ! -f ${green[$co]}.10hpr ]; then
    echo ${green[$co]} ${green[$co]:16} 
    for col in {2..12}
    do
      kol=$(printf "%02d" $col)
      awk "{print \$1, \$$kol}" ${green[$co]}  |grep -v "NaN"> tmp

      grat -F 1000 @SP, \
        -G tmp @GG : 1:2 \
        -S j  -L tmpp$kol.dat @p:s -o /dev/null
    done
    paste tmpp*.dat   \
      |  awk 'BEGIN{print "psi", "h0", "h1", "h10", "h100", "h1000", "h10000", "h-1", "h-10", "h-100", "h-1000", "h-10000"}
    {print $4, $10, $20 ,$30, $40 ,$50 ,$60 ,$70 ,$80 ,$90 , $100 ,$110 , $120 , $130}' > ${green[$co]}.10hpa
    rm tmpp*
  fi
done

#if [ ! -f merriam.dat.10hpa ]; then
grat -F 1000 @SP, \
  -G  @GN -V \
  -S j  -L merriam.dat.10hpa @p:s -o /dev/null
#fi
