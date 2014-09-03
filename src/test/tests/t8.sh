#!/bin/bash - 
#===============================================================================
#
#          FILE: t8.sh
#         USAGE: ./t8.sh 
#        AUTHOR: mrajner (), 
#       CREATED: 28.06.2014 22:16
#===============================================================================

set -o nounset                              # Treat unset variables as an error

counter=0

. definitions.sh 

value_check                                                                     \
  -F 10 , 20:@gh2h , 30:@ scale=10 @ invscale=1000 , 40@name , 10:@gp2h , 30 @H \
  -S a:10:20:100 , : 10 , c ,g :100                                             \
  -H                                                                            \
  -o  ${0/.sh/.dat}${counter}${suffix}                                          \
  -V ${0/.sh/.dat1}${counter}${suffix}                                          \
  &> ${0/.sh/.dat2}${counter}${suffix}
let counter=counter+3

touch ${0/.sh/.dat}${suffix}
