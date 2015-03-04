#!/bin/bash - 
#===============================================================================
#          FILE: test.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 06.09.2014 13:26
#===============================================================================

set -o nounset 
set -o pipefail

for i in *dat ; do
  [[ $i =~ rough ]] && continue
  [[ ! -f ${i}_ ]] && continue
  tput setaf 2
  echo $i ${i}_
  tput sgr0
  diff -q $i ${i}_ || 
  {
    colordiff $i ${i}_ -y -d |grep '|'
    join ${i} ${i}_ | awk \
      '
    @include "/home/mrajner/.local/src/awk/color.awk"
    function abs(x){return x>0?x:-x }
    BEGIN{
      MAX[4]=0
    }
    {
      if (/^[[:space:]]*[0-9.]/){
        for (i=2;i<=5;i++){

          if ($i!=0){
            res=($i-$(i+4))/$i*100 
          } else 
          {
            res=0
          }

          if( abs(res) > 0.01){
            printf Red"%10.2f"Reset "(%8.2f %8.2f) " , res , $i , $(i+4)
          } else
          {
            printf "%30s" , ""
          }

          if (abs(res)> abs(MAX[i-1])){MAX[i-1]=res}
        }
        printf "\n"
      }
    }
    END{
    for (i=1;i<=4;i++){
      printf LightBlue"%10.3f" Reset, MAX[i]
    }
  }



  ' | grep -v "^[[:space:]]*$"
}
:
done

# colordiff rajner_green_simple.dat rajner_green_full.dat -y
