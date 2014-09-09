#!/bin/bash - 
#===============================================================================
#          FILE: t9.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 30.06.2014 21:49
#===============================================================================

. definitions.sh 

counter=0

for exclamation in "" "-!" ; do
  [[ ${suffix} == ".s" ]] && continue
  {
    time grat                                                               \
    ${exclamation}                                                          \
    -M 1,2,3                                                                \
    -G rajner@GN , merriam @GE                                              \
    -F $SP, $GP , $LS , $HP , $H , $VT , $T , $VSH , 101300 @ RSP , 0 @HRSP \
    -U                                                                      \
    -D m:m:210@D                                                            \
    -Sj -BI  -H -I -10@DD : 5500@HS                                                   \
     -o ${0/.sh/.dat${counter}} :c                                          \
     -V ${0/.sh/.dat$((counter+1))}${suffix} : sparse                                
   } # &> ${0/.sh/.dat$((counter+2))}${suffix} 

  let counter=counter+3
done

touch ${0/.sh/.dat}${suffix}
