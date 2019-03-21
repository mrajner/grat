#!/bin/bash - 
set -o nounset 

R="0/25/40/60"

for i in 1 2 3 ; do
  polygon_check -S $R,0.5,0.3 -P mapa${i}.poly  -o mapa${i}.points 
  ../../polygon/polygon_map.sh \
    -R$R -f mapa${i}.poly      \
    -o mapa${i}                \
    -p mapa${i}.points
done

pdfjoin mapa{1,2,3}.pdf

pdfnup \
  mapa3-joined.pdf\
  --nup 3x1 \
  -q \
  -o polygon_ilustration.pdf\
  --frame true\
  --delta '4mm 4mm'

pdfcrop polygon_ilustration.pdf polygon_ilustration.pdf
