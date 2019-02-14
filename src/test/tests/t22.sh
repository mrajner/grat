. definitions.sh 

a="$G/bin/grat  -F$SP -G /home/mrajner/src/gotic2/data/grn1.data @ GG : 1 : 4 -S j -M2D"
b="$G/bin/grat  -F${SP/SP/EWT} @ pascal2mmwater -G /home/mrajner/src/gotic2/data/grn1.data @ GR : 1 : 4 -M2 -S j "

{
  $a
  $b

  echo
  $a -I10@DE
  $b -I10@DE

  echo
  $a -I1@DE
  $b -I1@DE

  echo
  $a -I3@DD
  $b -I3@DD

  echo
  $a -I3@AD
  $b -I3@AD

  echo
  echo "Difference is similar, but values are completely rescalled"
  $G/bin/grat -F$SP -M2 -G@GN -Sj,r         | tee >(awk '{a[NR] = $NF}END{print a[2]-a[1]}')
  $G/bin/grat -F$SP -M2 -G@GN -Sj,r -I100@DE | tee >(awk '{a[NR] = $NF}END{print a[2]-a[1]}')
  $G/bin/grat -F$SP -M2 -G@GN -Sj,r -I10@DE | tee >(awk '{a[NR] = $NF}END{print a[2]-a[1]}')
} | tee ${0/.sh/.dat}${suffix}

