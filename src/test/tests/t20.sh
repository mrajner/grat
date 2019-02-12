
. definitions.sh s

{
  # $G/bin/grat -F $SP   -S j -M1 -A1 -Dm:m
  # $G/bin/grat -F ../data/ncks.nc      @SP : sp -S j -M1 -A1 -Dm:m
  $G/bin/value_check -F $SP , ../data/ncks.nc @SP : sp -S j -M1  -Dm:m
  echo

  # $G/bin/grat        -! -F $SP             -S  j               -M1 -A1 -Dm:m
  # $G/bin/grat        -! -F ../data/ncks.nc @SP :               sp  -S  j  -M1 -A1 -Dm:m
  $G/bin/value_check -! -F $SP             ,   ../data/ncks.nc @SP :   sp -S  j   -M1 -Dm:m
  
} | tee ${0/.sh/.dat}${suffix}
