#!/bin/bash - 
set -o nounset 

PS=baltyk.ps
R="-R00/35/50/70"
J="-JM10+"

gmt pscoast  -K  -Di -A1000  -Slightgray -Ggray $R $J -W1 > $PS
cat baltyk.points | gmt psxy  $R $J -W1p -L -A -K -O >> $PS
gmt psclip baltyk.points  -K -O  $R $J >> $PS
gmt pscoast -Slightblue -O $R $J -Di -K -A1000 -W1  >> $PS
#  grdlandmask -Glandmask.grd $R -I3m
#  grd2cpt landmask.grd -Cjet > landmask.cpt
#grdimage ./landmask.grd $R $J -O -K -Clandmask.cpt >> $PS
gmt psclip -C -O >> $PS

gmt psconvert -Tf $PS -P -A
