#!/bin/bash - 
#===============================================================================
#
#          FILE: mpc.sh
#         USAGE: ./mpc.sh 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#        AUTHOR: mrajner
#       CREATED: 04.12.2013 21:01:36 CET
#===============================================================================

set -o nounset                              # Treat unset variables as an error

function licz(){
grat \
	-F NCEP @SP, VIENNA@RSP, NCEP1@T, NCEP@HP, ETOPO@H! , VIENNA@HRSP, NCEP@GP! ,NCEP@VT! \
	-M1,2,3:$1 -G@GN  \
	-S j \
	-H   \
  $3 \
	-D 20100101:3@Y:2@D \
  -q1 -A-0.43 $2:nc
}

licz masa       "-o tmpm" "-I 60@AS: 50@HS: 40000@HE :1@DD :3@DE"
licz potential  "-o tmpp" "-I 60@AS: 50@HS: 40000@HE :1@DD :3@DE"
licz cylinder   "-o tmpc" "-I 60@AS: 50@HS: 40000@HE :1@DD :3@DE"
join \
<(join \
<(sed -e 1d tmpm -e \$d) \
<(sed -e 1d tmpp -e \$d) ) \
<(sed -e 1d tmpc -e \$d) > tmp


licz masa       "-o point_pot_cyl_m.dat" "-I 60@AS: 30@HS: 60000@HE :2@DD :2@DE" &
licz potential  "-o point_pot_cyl_p.dat" "-I 60@AS: 30@HS: 60000@HE :2@DD :2@DE" &
licz cylinder   "-o point_pot_cyl_c.dat" "-I 60@AS: 30@HS: 60000@HE :2@DD :2@DE" &

wait

join \
<(join \
<(sed -e 1d point_pot_cyl_m.dat -e \$d) \
<(sed -e 1d point_pot_cyl_p.dat -e \$d) ) \
<(sed -e 1d point_pot_cyl_c.dat -e \$d) > point_pot_cyl.dat


