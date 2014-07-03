#!/bin/bash - 
#===============================================================================
#          FILE: t1.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 18.06.2014 08:19
# 
#===============================================================================

set -o nounset   
. definitions.sh

counter=0

# Testing simple value_check, and parser
for exclamation in "" "-!" ; do
  value_check                               \
    ${exclamation}                          \
    -F $SP, $T                              \
    -S                                      \
    jozefoslaw :  52.0 : 21.0 :  110,       \
    tmp        : -33.0 :  1.0 : -110,       \
    -D 20120101 : m : 24 @H                 \
    -o ${0/.sh/.dat}$((counter+0))${suffix} \
    -V ${0/.sh/.dat}$((counter+1))${suffix} \
    &> ${0/.sh/.dat}$((counter+2))${suffix}

  let counter=counter+3

# Testing simple value_check, and parser
  # value_check      \
    # ${exclamation} \
    # -F $SP, $T     \
    # -S g:50       \
    # -D 20120101    \

    # -o ${0/.sh/.dat}$((counter+0))${suffix} \
    # -V ${0/.sh/.dat}$((counter+1))${suffix} \
    # &> ${0/.sh/.dat}$((counter+2))${suffix}
  let counter=counter+3

done

