#!/bin/bash - 
#===============================================================================
#          FILE: t1.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 18.06.2014 08:19
#===============================================================================

set -o nounset                              # Treat unset variables as an error

outfile(){
echo  ${0%%.*}.dat
}


value_check                         \
  -F ../data/air.2012.nc : air         \
  -S jozefoslaw : 52.0 : 21.0 : 110 \
  -D 20120606 : 30@H                \
  -J100 -w n                        \
  -o $(outfile)


# test2.dat: .FORCE
# 	@value_check                         \
  # 		-!                                \
  # 		-F data/air.2012.nc : air         \
  # 		-S jozefoslaw : 52.0 : 21.0 : 110 \
  # 		-D 20120606 : 30@H                \
  # 		-J100 -w n > test2.dat
# 	@value_check                         \
  # 		-!                                \
  # 		-F data/air.2012.nc : air         \
  # 		-S jozefoslaw : 52.0 : 21.0 : 110 \
  # 		-D 20120606 : 30@H                \
  # 		-J100 -w n -V $@ -o /dev/null
# 	@value_check -F ../data/test_data.nc 
# 	$(call check) 
