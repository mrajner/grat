#!/bin/bash - 
#===============================================================================
#          FILE: landsea.sh
#         USAGE: ./landsea.sh 
#   DESCRIPTION: 
#       OPTIONS: ---
#        AUTHOR: mrajner
#       CREATED: 18.11.2012 12:13:33 CET
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
#gmtset ELLIPSOID Sphere
#gmtset ELLIPSOID WGS84

# Hel peninsula
#R="18/19/54.5/54.9"
#Hel peninsula details
R="18.6/19/54.5/54.7"

R="d"
R="0/50/0/70"

I="0.2/0.2"
#I="0.05/0.05"
J="M10c+"
J="H10c+"

Rp=$( echo ${R} | sed 's/\//-/g' )
Ip=$( echo ${I} | sed 's/\//-/g' )

grd=landseaR${Rp}I${Ip}.grd
cpt=$(grd2cpt $grd > landsea.cpt)
ps=tmp.ps #${grd/.grd/.ps}

 grdlandmask -Dc -R$R -G${grd}  -I$I 

  grd2xyz $grd |minmax -C


#  grd2cpt  $grd > landsea.cpt
  grdimage $grd  -R$R  -C$cpt -J$J -K -Sn > $ps
  pscoast -Dc -R$R -J$J -O -W2p/green -N12p >> $ps
  ps2raster -Tf $ps -A -P
