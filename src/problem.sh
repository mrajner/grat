mask=/tmp/mask.nc
echo a
gmt grdmath -Rg -I2 0 0 NAN = ${mask}=nb
mask2=/tmp/mask2.nc
grdmath $mask 0 DENAN = $mask2=nb

echo  exclamation
$G/bin/value_check -! -F $mask -Sj , os
echo
echo  normal
$G/bin/value_check   -F $mask  -Sj , os 

$G/bin/grat   -F 10 @ SP, $mask @LS  -Sj , os  -M2 -Grajner@GN
$G/bin/grat -!  -F 10 @ SP, $mask @LS  -Sj , os  -M2 -Grajner@GN
