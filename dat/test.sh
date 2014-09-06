#!/bin/bash - 
#===============================================================================
#          FILE: test.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 06.09.2014 13:26
#===============================================================================

set -o nounset 
set -o pipefail

for i in *dat ; do
  [[ ! -f ${i}_ ]] && continue
  echo $i
  diff -q $i ${i}_ || 
  {
    # colordiff $i ${i}_ -y
    join ${i} ${i}_ | awk \
      '
    {
      if (/^[[:space:]]*[0-9.]/){
        for (i=2;i<=5;i++){
          if ($i!=0){
            res=($i-$(i+4))/$i*100 
            # if(res!=0){
            printf "%s " , res
          # }
          }
        }
        printf "\n"
      }
    }' | egrep -v "^[[:space:]]*$"  | awk '{if (NF==3){print $0, "0"}else{print }}'| gmtinfo
  }
done
