#!/bin/bash - 
#===============================================================================
#          FILE: compar.sh
#         USAGE: ./compar.sh 
#   DESCRIPTION: 
#       OPTIONS: ---
#        AUTHOR: mrajner
#       CREATED: 13.12.2012 21:15:45 CET
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error

  WEN="/home/mrajner/pub/2012_wenecja/dane"
  SFC="/home/mrajner/src/grat/data/ncep_reanalysis/pres.sfc.2011.nc:pres"
  TMP="../data/ncep_reanalysis/air.sig995.2011.nc:air:lon:lat:level:time"
  LND="../data/landsea/test.grd:z:x:y"
  HGT="../data/topo/ETOPO2v2g_f4.nc:z:x:y"
#  LND="../data/landsea/test_.grd:z:x:y"
#  POL= ../polygon/tmp.poly


  numer=354
  I=1

  TAB=($(sed -ne 2p ${WEN}/szereg_${numer}.txt))
  L=$(echo ${TAB[4]}|tr "," " ")
  B=$(echo ${TAB[3]}|sed 's/,//')

  echo $B $L
#../bin/grat -V -Stmp,${B},${L}  -F${SFC},${TMP},${HGT},${LND}   -Ghuang,huang,huang,huang,,1:1     -D20110218,2012  -o${numer}_${I}_5  -I1  
../bin/grat -V -L:G -Stmp,${B},${L}  -F${SFC},${TMP},${HGT},${LND}   -G,rajner,,,,1:1    -D2011,2012  -o${numer}_${I}_3  -I1  
#../bin/grat -V -Stmp,${B},${L}  -F${SFC},${TMP},,${LND} -Bi  -G,,,,,1:1   -D20110101,20111231  -o${numer}_${I}_3  -I2
../bin/value_check -V -Stmp,${B},${L}  -F${TMP}     -D20110101,20111231  -o${numer}_${I}_6  -I2 
#../bin/grat  -Stmp,${B},${L}  -F${SFC},${TMP},,${LND} -Bi  -G,,,,,1:1  -L:G 
#../bin/grat  -Stmp,${B},${L}  -F${SFC},${TMP},,${LND} -Bi  -Grajner,rajner,rajner,rajner,,1:1  -L:G 
