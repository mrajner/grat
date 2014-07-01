#!/bin/bash - 
#===============================================================================
#          FILE: t9.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 30.06.2014 21:49
#===============================================================================

set -o nounset                              # Treat unset variables as an error

. definitions.sh

counter=0
for exclamation in "" "-!" ; do
  grat                                                                           \
    ${exclamation} \
    -M 1,2,3                                                                     \
    -G rajner@GN , merriam @GE                                                   \
    -F $SP, $GP , $LS , $HP , $H , $VT , $T , $VSH , 101300 @ RSP , 0 @HRSP \
    -U                                                                           \
    -D m:m:210@D                                                                  \
    -Sj -BI  -H -I500@HS                                                         \
    -o : ${0/.sh/.dat${counter/0}} :n  \
    # -V
  let counter+2
done
