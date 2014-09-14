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

while getopts "bvuidwV" flag ; do
  case $flag in 
    d)
      delete_bad_results=true
      ;;
    V)
      vimdiff=true
      ;;
    w)
      ignore_white_spaces="-w"
      ;;
    b)
      show_failed_diffs=true
      ;;
    v)
      verbose=true
      ;;
    u)
      update=true
      ;;
    i)
      interactive="-i"
      ;;
  esac
done

shift $(($OPTIND-1))
[[  $# -ge 1 ]] && test_what=($*) || test_what=(t[0-9_]*.sh t*.f90)

ok(){
  let good++
  ${verbose:-false} && 
  {
    tput setaf 2 ;  echo "$is -- passed" ; tput sgr0 ;  : ;
  } || :
}

for test in ${test_what[*]} ; do
  [[ ! -f $test ]] && { echo $test not exist..;  continue ; }
  [[ ${test} =~ ".sh" ]]  && results=${test/.sh/.dat}
  [[ ${test} =~ ".f90" ]] && results=${test/.f90/.dat}

  for is in ${results}* ; do

    should_be=${is/t/r}

    # if both files are empty skip comparison
    [[ ! -s $is && ! -s ${should_be}  ]] && 
    {
      continue
    }

    diff $is $should_be ${ignore_white_spaces:-} ${ignore_white_spaces:-} >/dev/null && 
    { 
      ok 
    } || 
    {

      do_not_compare_list='com\|FF\|^\(real\|user\|sys\)\|\(Unknown[[:space:]]*\)\{3\}\|Program started\|eta *[[:digit:]]\|[Ee]xecution time\|^#[[:space:]]\+v[[:digit:]]\|^#[[:space:]]\+compiled on\|^#[[:space:]]\+compiler:\|FFLAGS\|| %: *[[:digit:]]'

      diff -I "$do_not_compare_list"  $is $should_be -q ${ignore_white_spaces:-} >/dev/null && 
      { 
        ok
      } || 
      {

        tput setaf 3 ;  echo "$is -- failed" ; tput sgr0 ; let bad++ ; : ;

        [[ ${show_failed_diffs:-} == "true" ]] && 
        {
          echo ---
          echo is: $is , should_be: $should_be
          colordiff  -I "$do_not_compare_list"  $is $should_be  ${ignore_white_spaces:-}

          ${vimdiff:-false} && vimdiff $is $should_be
        }

        ${update:-false} && cp -v ${interactive:-} $is $should_be
        ${delete_bad_results:-false} && rm -v ${interactive:-} $is ${is%%.dat*}.dat.?
      }
    } 
    let counter++
  done
done

echo -e "\npassed: $good/$counter !164"
[[ $bad -gt 0 ]] && echo failed: $bad || :
