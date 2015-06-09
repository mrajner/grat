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

  let counter++

  value_check                         \
    ${exclamation}                    \
    -F $SP, $T                        \
    -S                                \
    jozefoslaw :  52.0 : 21.0 :  110, \
    tmp        : -33.0 :  1.0 : -110, \
    -D 20120101 : m : 24 @H           \
    -o ${0/.sh/.dat.out}$counter${suffix} \
    -V ${0/.sh/.dat.log}$counter${suffix} \
    2> ${0/.sh/.dat.err}$counter${suffix}

  let counter++

  value_check      \
    ${exclamation} \
    -F $SP, $T     \
    -S g:50       \
    -D 2011123118 : 2 @M : 12 @H    \
    -o ${0/.sh/.dat.out}$counter${suffix} \
    -V ${0/.sh/.dat.log}$counter${suffix} \
    2> ${0/.sh/.dat.err}$counter${suffix}


  let counter++

 value_check                                            \
    ${exclamation}                                      \
    -F $SP, $T                                          \
    -S                                                  \
    r,                                                  \
    equator    : 0    : 0     : 0,                      \
    south pole : -90  : 0     : 0,                      \
    north_pole : 90   : 0     : 0,                      \
    out        : 0    : 1361  : 0,                      \
    out        : -100 : -1361 : 0                       \
    out        : 100  : 161   : 0                       \
                                                        \
    -D                                                  \
    2012010124@~ ,                                      \
    2012010122@~ ,                                      \
    2012010144@~ ,                                      \
    20120311@~                                          \
    -o ${0/.sh/.dat.out}$counter${suffix}               \
    -V ${0/.sh/.dat.log}$counter${suffix}               \
    2> ${0/.sh/.dat.err}$counter${suffix}

done

touch ${0/.sh/.dat}${suffix} 
