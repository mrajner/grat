#!/bin/bash - 
#===============================================================================
#
#          FILE: bug.sh
#         USAGE: ./bug.sh 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#        AUTHOR: mrajner
#       CREATED: 22.06.2013 15:56:55 CEST
#===============================================================================

set -o nounset                              # Treat unset variables as an error

SP=/home/mrajner/dat/ncep_reanalysis/pres.sfc.2010.nc

function jest (){
 if [ ! -f $1 ];then
   out=$1
   shift
   ${*} -o $out
 fi
}
R="-3/3/-3/3"

#value_check -F ${SP}@SP:pres -S ${R}:1 -V -I n @I -o mapa.xyz 

grat -F ${SP} @SP : pres -S ${R}:1  -G merriam @GE -o -Ltmppoint@p # mapa.xyz
#R="-20/-1/-20/20"
awk '{
}{print $3,$2,$5/1e6}' mapa.xyz \
|xyz2grd -fg   -I1 -Gmapa.grd -R$R
grd2cpt -E10 mapa.grd > mapa.cpt
gmtset D_FORMAT="%5.0f"

grdimage mapa.grd -R$R -JM10c -Cmapa.cpt -K > mapa.ps
pscoast -Dl -R$R -J -K -O -W1p >> mapa.ps
psscale -D5/0/10/1h -Cmapa.cpt -O >> mapa.ps
ps2raster -Tf mapa.ps -A -P

