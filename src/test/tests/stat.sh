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

help () {
  cat << EOF
-h show help
-d delete bad result
-b show failed diff
-w ignore whitsespace
-V vimdiff
-u update
-U update if result do not exist
-i interactive update
-1 one diff only
EOF
}

while getopts "1hbvuidwVU" flag ; do
  case $flag in 
    1)
      onlyonediff=true
      ;;
    h)
      help
      exit 0
      ;;
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
    U)
      updateifnotexist=true
      ;;
    i)
      interactive="-i"
      ;;
  esac
done

shift $(($OPTIND-1))
[[  $# -ge 1 ]] && test_what=($*) || test_what=($(ls -v t[0-9_]*.sh t*.f90))

ok(){
  let good++
  ${verbose:-false} && 
  {
    tput setaf 2 ;  echo "$is -- passed" ; tput sgr0 ;  : ;
  } || :
}

# echo ${test_what[*]}
for test in ${test_what[*]} ; do
  # echo $test !
  [[ ! -f $test ]] && { echo $test not exist..;  continue ; }
  [[ ${test} =~ ".sh" ]]  && results=${test/.sh/.dat}
  [[ ${test} =~ ".f90" ]] && results=${test/.f90/.dat}

  for is in ${results}* ; do

    should_be=${is/t/r}
    # echo $is

    # if both files are empty skip comparison
    [[ ! -s $is && ! -s ${should_be} && -f ${is} && -f ${should_be}  ]] && 
    {
      continue
    }

    # if result do not exist
    [[ -f ${should_be} ]] || 
    {
      echo ${should_be} do not exists
      [[ ${updateifnotexist-} ]] && 
      {
        echo updating ${should_be}
        cp ${is} ${should_be} -v
				git add -N ${should_be}
      }
      continue
    }

    diff $is $should_be ${ignore_white_spaces:-} ${ignore_white_spaces:-} >/dev/null && 
    { 
      ok 
    } || 
    {

      do_not_compare_list='com\|FF\|^\(real\|user\|sys\)\|\(Unknown[[:space:]]*\)\{3\}\|Program started\|eta *[[:digit:]]\|[Ee]xecution time\|^#[[:space:]]\+v[[:digit:]]\|^#[[:space:]]\+compiled on\|^#.*#$\|^#[[:space:]]\+compiler:\|FFLAGS\|| %: *[[:digit:]]\|Note: The following floating-point exceptions are signalling: IEEE_INVALID_FLAG IEEE_DENORMAL'

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

					${vimdiff:-false} && 
					{
						vimdiff $is $should_be

						${onlyonediff:-false} && exit 0
						${update:-false} || read
					}
				}

				${update:-false} && cp -v ${interactive:-} $is $should_be
				${delete_bad_results:-false} && rm -v ${interactive:-} $is ${is%%.dat*}.dat.?

				${onlyonediff:-false} && exit 0
			}
		} 
		let counter++

	done
done

echo -e "\npassed: $good of total $counter tests"
[[ $bad -gt 0 ]] && 
{
	tput setaf 1
	echo failed: $bad 
	tput sgr0
} ||
	{
		tput setaf 2
		echo "Success"
		tput sgr0
	}
