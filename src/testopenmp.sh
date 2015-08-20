#!/bin/bash - 
#===============================================================================
#          FILE: testopenmp.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 19.08.2015 11:19
#===============================================================================

set -e
set -o nounset 
set -o pipefail

# touch grat.f90
# make grat FC=gfortran FFLAGS="-fopenmp"
# grat -v |grep FFLAGS


# for binname in grat_gfortran grat_gfortran_noomp ; do
for binname in grat_gfortran_noomp ; do

# time            \
# ../bin/$binname \
  # -M2           \
  # -G @GN        \
  # -FERA@SP      \
  # -D 2012, 2013:2@D   \
  # -Sj -I3@DD:2@AD
# done 

for j in "" "-j1" ; do
time {
  value_check -D 2012, 2013:2@D  | cut -f2 -d' ' | while read D ; do
echo ../bin/$binname \
  -M2           \
  -G @GN        \
  -FERA@SP      \
  -D $D \
  -Sj -I93@DD:8@AD
done | parallel $j
}
done




done 

# make clean
# make grat FC=gfortran FFLAGS='-O2 -fopenmp'
# grat -v |grep FFLAGS

