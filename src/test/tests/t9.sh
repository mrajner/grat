#!/bin/bash - 
#===============================================================================
#          FILE: t9.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 30.06.2014 21:49
#===============================================================================

. definitions.sh 

counter=0

# for exclamation in "" "-!" ; do
  # [[ ${suffix} == ".s" ]] && continue || 
  # {
    # time grat                                                         \
      # ${exclamation}                                                  \
      # -M 1,2,3                                                        \
      # -G rajner@GN , merriam @GE                                      \
      # -F $SP, $GP, $LS, $HP, $H, $VT, $T, $VSH, 101300 @ RSP, 0 @HRSP \
      # -U                                                              \
      # -D m:m:210@D                                                    \
      # -Sj -BI  -H -I -10@DD : 5500@HS                                 \
      # -o ${0/.sh/.dat${counter}}${suffix} :c                          \
      # -V ${0/.sh/.dat$((counter+1))}${suffix} : sparse                \
      # 2> ${0/.sh/.dat$((counter+2))}${suffix} 
  # }

  # let counter=counter+3
# done

# touch ${0/.sh/.dat}${suffix}

# TODO
    time grat                                                                \
      -M 1,2,3                                                               \
      -G rajner@GN , merriam @GE                                             \
      -F                                                                     \
      /home/mrajner/src/grat/src/test/data/pres.sfc.2012.nc   @ SP  : pres , \
      /home/mrajner/src/grat/src/test/data/hgt.sfc.nc         @ H   : hgt  , \
      /home/mrajner/src/grat/src/test/data/hgt.2012.nc        @ GP  : hgt  , \
      /home/mrajner/src/grat/src/test/data/land.nc            @ LS  : land , \
      /home/mrajner/src/grat/src/test/data/hgt.sfc.nc         @ HP  : hgt  , \
      /home/mrajner/src/grat/src/test/data/air.sig995.2012.nc @ T   : air  , \
      /home/mrajner/src/grat/src/test/data/air.2012.nc        @ VT  : air  , \
      /home/mrajner/src/grat/src/test/data/shum.2012.nc       @ VSH : shum , \
      101300 @ RSP, 0 @HRSP                                                  \
      -U                                                                     \
      -D m:m:210@D                                                           \
      -Sj -BI  -H -I -10@DD : 5500@HS  -V
