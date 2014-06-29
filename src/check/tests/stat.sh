#!/bin/bash - 
#===============================================================================
#          FILE: stat.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 18.06.2014 08:46
#===============================================================================

set -o nounset                              # Treat unset variables as an error
shopt -s nullglob

counter=0
good=0
bad=0

while getopts "b" flag ; do
  case $flag in 
    b)
      show_failed_diffs=true
      ;;
  esac
done

for test in t*.sh ; do
  
  for is in ${test/.sh/.dat}* ; do

    should_be=${is/t/r}

    diff $is $should_be >/dev/null && 
    { 
      tput setaf 2 ;  echo "$is -- passed" ; tput sgr0 ; let good++; : ;
    } || 
    {
      
      diff <(egrep -v (compiled|started) $is) <(egrep -v (compiled|started) $should_be)

      tput setaf 1 ;  echo "$is -- failed" ; tput sgr0 ; let bad++ ; : ;
      [[ ${show_failed_diffs:-} == "true" ]] && 
      {
        diff $is $should_be
      }
    } 
    let counter++
  done
done
echo tests: $good/$counter
[[ $bad -gt 0 ]] && echo failed: $bad || :

