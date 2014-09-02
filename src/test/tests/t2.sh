#!/bin/bash - 
#===============================================================================
#          FILE: t1.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 18.06.2014 08:19
#===============================================================================

set -o nounset                              # Treat unset variables as an error


# testing 4 dimensional variables with value_check

. definitions.sh 

counter=0

for exclamation in "" "-!" ; do
  value_check  \
    ${exclamation} \
    -F no_file \
    -S j       \
    -D 2012    \
    -J100 &>${0/.sh/.dat}$counter$suffix
  let counter++

  [[ ${suffix} != ".c" ]] && 
  {
    value_check                            \
      -F $file : wrong_var_name            \
      -S j                                 \
      -D 2012                              \
      -J100 &>${0/.sh/.dat}$counter$suffix
    let counter++

    value_check                 \
      -F                        \
      $SP : lon : lat,          \
      $VT : lon : lat : level , \
      $VT : lon : lat : leel    \
      -S j                      \
      -D 20120103               \
      -o : level                \
      -H                        \
      &>${0/.sh/.dat}$counter$suffix
    let counter++
  }

  value_check \
    -!        \
    -F $GP    \
    -S j      \
    -D 2012   \
    -J1000,100 , 11    \
    -o : level &>${0/.sh/.dat}$counter$suffix
  let counter++
done

touch ${0/.sh/.dat}${suffix} 
