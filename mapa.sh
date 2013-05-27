#!/bin/bash - 
#===============================================================================
#          FILE: mapa.sh
#         USAGE: ./mapa.sh 
#   DESCRIPTION: 
#       OPTIONS: ---
#        AUTHOR: mrajner
#       CREATED: 20.09.2012 16:08:29 CEST
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
#grd2cpt ./tmp.grd -Crainbow -Z > color.cpt
T=`minmax tmp -C -T1/2`
makecpt $T -Crainbow -Z > color.cpt
#grdimage ./tmp.grd -Ccolor.cpt $R -JH10c -B60f30 -K -E200 > tmp.ps

R="-R02/26/47/57"
R="-R-25/56/07/77"
#R="-R19/25/45/53"
#R="-R-180/180/-80/80"
J="-JM10c"

#R="-Rg"

latitude=52
longitude=21
altitude=500.0
#altitude=15.0
tilt=0
azimuth=0
twist=0
Width=0.0
Height=0.0

#grdcut $R /usr/share/GMT/dbase/etopo2.grd -Gcut.grd
#grdclip cut.grd -Gcut0.grd -Sb4/4

 gmtset FRAME_WIDTH 0.01c
 makecpt -Cglobe -T-3000/3000/300 -Z > color.cpt

J=-JG${longitude}/${latitude}/${altitude}/${azimuth}/${tilt}/${twist}/${Width}/${Height}/1.8i
pscoast -Slightblue -Dl $R $J  -K -Wthinnest -A2000 -N1 > tmp.ps
#grdimage  -R -J -O -K cut0.grd -Ccolor.cpt >> tmp.ps
cat src/tmp | psxy  -m  -W0.001p/gray -R -J -O -K >> tmp.ps
#cat tmp |awk '{print $1,$2,$3}' | psxy -Ccolor.cpt -: -R -J -O -K -Sc0.03c -W0.1p -Gwhite >> tmp.ps
echo 52 21  | psxy  -: -R -J -O -K -Sc0.03c -W2.1p -Gwhite >> tmp.ps

pscoast -Dl $R $J  -O -Wthinnest -A2000 -N1 >> tmp.ps


ps2raster tmp.ps -Tf -A
#evince tmp.pdf  &

