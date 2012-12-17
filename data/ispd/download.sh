#!/bin/bash - 
#===============================================================================
#          FILE: download.sh
#         USAGE: ./download.sh 
#   DESCRIPTION: 
#       OPTIONS: ---
#        AUTHOR: mrajner
#       CREATED: 22.11.2012 19:05:01 CET
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error

for mm in 09
do

  csh ./download.csh rajner 2009 $mm

done
