
. definitions.sh

{
  grat -F $SP   -S j -M1 -A1 -Dm:m
  grat -F ../data/ncks.nc      @SP : sp -S j -M1 -A1 -Dm:m
  value_check -F $SP , ../data/ncks.nc @SP : sp -S j -M1  -Dm:m
  echo

  grat        -! -F $SP             -S  j               -M1 -A1 -Dm:m
  grat        -! -F ../data/ncks.nc @SP :               sp  -S  j  -M1 -A1 -Dm:m
  value_check -! -F $SP             ,   ../data/ncks.nc @SP :   sp -S  j   -M1 -Dm:m
  
} | tee ${0/.sh/.dat}${suffix}
