#!/bin/bash - 
set -e
set -o nounset 
set -o pipefail

. definitions.sh

{
  grat \
    -S j \
    -F ${EWT-100@EWT}  \
    -M2 -G ../../../dat/merriam_green.dat @GR : 1 : 6 @ m2f 
} | tee ${0/.sh/.dat}${suffix}
