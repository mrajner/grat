. definitions.sh

{
  # default interpolation is nearest
  value_check -F ${SP} -S onsala -o:free
  value_check -F ${SP} -S onsala -In@I -o:free
  value_check -F ${SP} -S onsala -Il@I -o:free

  echo
  # with -! -I l@I select only small necessary subarray for interpolation
  value_check -! -F ${SP} -S onsala -In@I -o:free
  value_check -! -F ${SP} -S onsala -Il@I -o:free
} | tee ${0/.sh/.dat}${suffix}
