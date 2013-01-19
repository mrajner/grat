#!/bin/bash - 
#===============================================================================
#          FILE: baltyk.sh
#         USAGE: ./baltyk.sh 
#   DESCRIPTION: 
#       OPTIONS: ---
#        AUTHOR: mrajner
#       CREATED: 09.11.2012 11:06:26 CET
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error

PS=baltyk.ps
R="-R00/35/50/70"
J="-JM10+"

pscoast  -K  -Di -A1000  -Slightgray -Ggray $R $J -W1 > $PS
#cat baltyk.points | psxy  $R $J -W1p -L -A -K -O >> $PS
psclip baltyk.points  -K -O  $R $J >> $PS
pscoast -Slightblue -O $R $J -Di -K -A1000 -W1  >> $PS
#  grdlandmask -Glandmask.grd $R -I3m
#  grd2cpt landmask.grd -Cjet > landmask.cpt
#grdimage ./landmask.grd $R $J -O -K -Clandmask.cpt >> $PS
psclip -C -O >> $PS

ps2raster -Tf $PS -P -A
