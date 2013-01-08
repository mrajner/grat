#!/bin/bash - 
#===============================================================================
#          FILE: location_map.sh
#         USAGE: ./location_map.sh 
#   DESCRIPTION: 
#       OPTIONS: ---
#        AUTHOR: mrajner
#       CREATED: 22.11.2012 00:24:53 CET
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
set -x


PS=location_map.ps
R="g" ;  J="H0/10+"
#R="0/50/30/60" ;  J="M10+"

  pscoast -Dc -R$R -B -J$J -Sgray -K  > $PS
  cat hourly/sites.sta  | awk '{print $2, $3}'  | psxy -R$R -J$J -Sc1p -W0.5p/green -O >> $PS
  ps2raster -Tf -A -P $PS
  rm $PS
