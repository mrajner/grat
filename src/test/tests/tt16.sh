set -e
set -o nounset 
set -o pipefail

. definitions.sh 

[[ ${EWT+x} == "x" ]] || 
{
  unset SP
  source definitions.sh c
}

grat=$G/bin/grat
{
  $grat \
    -S j \
    -F $EWT \
    -BN \
    -M2 -G /home/mrajner/src/gotic2/data/grn1.data @GR : 1 : 2 
} | tee ${0/.sh/.dat}${suffix}

{
  $grat \
    -S j \
    -F 1000@SP:@mmwater2pascal -BN \
    -M2 -G /home/mrajner/src/grat/dat/merriam_green.dat @GR  -I4@DD:4@AD

} | tee ${0/.sh/.dat}${suffix}
  exit

{
  $grat \
    -S j \
    -F 9800@SP:@mmwater2pascal \
    -w \
    -BN \
    -M2 -G@GE
} | tee ${0/.sh/.dat}${suffix}
{
  $grat \
    -S j \
    -F 98000@SP \
    -w \
    -BN \
    -M2 -G@GE
} | tee ${0/.sh/.dat}${suffix}

{
  echo $EWT $SP
  $grat \
    -S j \
    -F $EWT \
    -M2 -G ../../../dat/merriam_green.dat@GN:1:4 
} | tee ${0/.sh/.dat}${suffix}
