#!/bin/bash - 
#===============================================================================
#          FILE: t9.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 30.06.2014 21:49
#===============================================================================

set -o nounset                              # Treat unset variables as an error

. definitions.sh

grat                         \
  -M 1,2,3                       \
  -G rajner@GN , merriam @GE \
  -F $SP, $GP , $LS , $HP , $H , $VT , $T , 1e-4 @VSH , 101300 @ RSP , 0 @HRSP  \
  -U          \
  -D m:m:10@D  \
  -Sj -BI  -H -I500@HS \
  -o ${0/.sh/.tmp} :nc 

grat                         \
  -! \
  -M 1,2,3                       \
  -G rajner@GN , merriam @GE \
  -F $SP, $GP , $LS , $HP , $H , $VT , $T , 1e-4 @VSH , 101300 @ RSP , 0 @HRSP  \
  -U          \
  -D m:m:10@D  \
  -Sj -BI  -H -I500@HS \
  -o ${0/.sh/.tmp} :nc 


