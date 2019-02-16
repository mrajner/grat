set -e
set -o nounset 
set -o pipefail

. definitions.sh

[[ ${EWT+x} == "x" ]] || 
{
  unset SP
  source definitions.sh c
}

grat=grat
{
  $grat \
    -S j \
    -F $EWT \
    -BN \
    -M2 -G /home/mrajner/src/gotic2/data/grn1.data @GR : 1 : 2 

  $grat \
    -S j \
    -F 1000@SP:@mmwater2pascal -BN \
    -M2 -G /home/mrajner/src/grat/dat/merriam_green.dat @GR : 1 : 2  -I2@DD:2@AD

  $grat \
    -S j \
    -F 9800@SP:@mmwater2pascal \
    -w \
    -BN \
    -M2 -G@GE

  $grat \
    -S j \
    -F 98000@SP \
    -w \
    -BN \
    -M2 -G@GE

  echo $EWT $SP
  $grat \
    -S j \
    -F $EWT \
    -M2 -G ../../../dat/merriam_green.dat@GN:1:4 

  } | tee ${0/.sh/.dat}${suffix}
