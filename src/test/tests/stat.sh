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

ok(){
  tput setaf 2 ;  echo "$is -- passed" ; tput sgr0 ; let good++; : ;
}

for test in t*.sh ; do
  
  for is in ${test/.sh/.dat}* ; do

    should_be=${is/t/r}

    # if both files are empty skip comparison
    [[ ! -s $is && ! -s ${should_be}  ]] && 
    {
      continue
    }




    diff $is $should_be >/dev/null && 
    { 
      ok 
    } || 
    {

      do_not_compare_list='compiled|Program|100%|ifort\s|Execution|FFLAGS'

      diff                                       \
        <(egrep -v ${do_not_compare_list:-} $is) \
        <(egrep -v ${do_not_compare_list:-} $should_be) -q >/dev/null  && 
      { 
        ok
      } || 
      {

        tput setaf 1 ;  echo "$is -- failed" ; tput sgr0 ; let bad++ ; : ;

        [[ ${show_failed_diffs:-} == "true" ]] && 
        {
          colordiff                                       \
            <(egrep -v ${do_not_compare_list:-} $is) \
            <(egrep -v ${do_not_compare_list:-} $should_be) 
        }
      }
    } 
    let counter++
  done
done

echo -e "\ntests: $good/$counter"
[[ $bad -gt 0 ]] && echo failed: $bad || :

