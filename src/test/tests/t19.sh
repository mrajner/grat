
. definitions.sh 

{
  $G/bin/grat -F $SP -M2 -G rajner@GN : 1 : 3  -Sj
  $G/bin/grat -F $SP -M2 -G rajner@GN : 1 : 4  -Sj
  $G/bin/grat -F $SP -M2 -G rajner@GN : 1 : 22  -Sj
  $G/bin/grat -F $SP -M2 -G ../../../dat/rajner_green.dat@GN : 1 : 3  -Sj
  $G/bin/grat -F $SP -M2 -G ../../../dat/rajner_green.dat@GN : 1 : 4  -Sj
  $G/bin/grat -F $SP -M2 -G ../../../dat/rajner_green.dat@GN : 1 : 22  -Sj
} | tee ${0/.sh/.dat}${suffix}
