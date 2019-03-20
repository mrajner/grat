. definitions.sh

myLS="../data/gmtls.nc@LS! : z"
{
  grat        \
    -! \
    -F ${SP}, ${myLS} \
    -G @GN : 1 : 2  \
    -S j -BI \
    -Dm -I3@DE 

  } 2>&1 | tee ${0/.sh/.dat}${suffix}
