#!/bin/bash

sss(){
  awk  '
  BEGIN{
  ref=0}
  {
    if ($6 ~ /[0-9]+/ && !ref){
      for ( i = 7; i <= NF ; i++ ){
        REF[i]=$i
        ref=1
      }
    }

    #  printf $0
    for (i = 7; i <= NF ; i++ ){
    if (ref) {
       printf "%6.2f ", $i-REF[i]
      }
    }
    printf "\n"
  }
  '
}

site=l

for site in l r o b e ; do
#  for site in l  ; do

  wdiff  \
    <(grat -H -F NCEP@SP, VIENNA@RSP  -D2012:2@D -M2 -G@GN,@GE -BN -S$site -o:free -rt | sss) \
    <(grat -H -F NCEP@SP              -D2012:2@D -M2 -G@GN,@GE -BN -S$site -o:free -rt | sss) | colordiff

  wdiff                                                                                                 \
    <(grat -H -F NCEP@SP, VIENNA@RSP            -D2012:2@D -M2 -G@GN,@GE -BN -S$site -o:free -rt | sss) \
    <(grat -H -F NCEP@SP,  1000@RSP:@scale=100  -D2012:2@D -M2 -G@GN,@GE -BN -S$site -o:free -rt | sss) \
    | colordiff

  wdiff                                                                                               \
    <(grat -H -F NCEP@SP, VIENNA@RSP          -D2012:2@D -M2 -G@GN,@GE -BN -S$site -o:free -rt | sss) \
    <(grat -H -F NCEP@SP,  10@RSP:@scale=100  -D2012:2@D -M2 -G@GN,@GE -BN -S$site -o:free -rt |sss ) \
    | colordiff

  grat -v | grep compiler || :
done | egrep '[]{]'
