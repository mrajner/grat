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

while getopts "bv" flag ; do
  case $flag in 
    b)
      show_failed_diffs=true
      ;;
    v)
      verbose=true
      ;;
  esac
done

ok(){
  ${verbose:-false} && 
  {
    tput setaf 2 ;  echo "$is -- passed" ; tput sgr0 ; let good++; : ;
  } || :
}

for test in t*.sh t*.f90 ; do
  [[ ${test} =~ ".sh" ]]  && results=${test/.sh/.dat}
  [[ ${test} =~ ".f90" ]] && results=${test/.f90/.dat}

  for is in ${results}* ; do

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

      do_not_compare_list='^\(real\|user\|sys\)\|\(Unknown[[:space:]]*\)\{3\}\|Program started\|eta *[[:digit:]]\|Execution time\|^#[[:space:]]\+v[[:digit:]]\|^#[[:space:]]\+compiled on\|| %: *[[:digit:]]'

      diff  -I "$do_not_compare_list"  $is $should_be -q >/dev/null && 
      { 
        ok
      } || 
      {

        tput setaf 1 ;  echo "$is -- failed" ; tput sgr0 ; let bad++ ; : ;

        [[ ${show_failed_diffs:-} != "true" ]] && 
        {
          colordiff  -I "$do_not_compare_list"  $is $should_be 
        }
      }
    } 
    let counter++
  done
done

echo -e "\ntests: $good/$counter"
[[ $bad -gt 0 ]] && echo failed: $bad || :
