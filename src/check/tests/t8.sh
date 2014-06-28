#!/bin/bash - 
#===============================================================================
#
#          FILE: t8.sh
# 
#         USAGE: ./t8.sh 
# 
#   DESCRIPTION: 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: mrajner (), 
#  ORGANIZATION: 
#       CREATED: 28.06.2014 22:16
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error

value_check                                                                     \
  -F 10 , 20:@gh2h , 30:@ scale=10 @ invscale=1000 , 40@name , 10:@gp2h , 30 @H \
  -S a:10:20:100 , : 10 , c ,g :100                                             \
  -H -q0                                                                        \
  -o ${0/.sh/.dat}                                                              \
  -V ${0/.sh/.dat1} \
  2> ${0/.sh/.dat2}

