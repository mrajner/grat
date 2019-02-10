. definitions.sh

myLS="../data/gmtls.nc@LS! : z"
{
  time grat        \
    -! \
    -F ${SP}, ${myLS} \
    -G @GN : 1 : 2  \
    -S j -BI \
    -Dm -I3@DE 

  } | tee ${0/.sh/.dat}${suffix}
