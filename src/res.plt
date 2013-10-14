#!/usr/bin/gnuplot
! for i in tmp tmp2 tmp3; \
do minmax $i -C |awk '{for (i=0;i<=2;i++) printf "%10.4f", $(14+i*2)-$(13+i*2)}END{print ""}'; \
done

i=8
set multiplot lay 3,1
plot 'tmp' us 1:i w l 
plot 'tmp2' us 1:i w l 
plot 'tmp3' us 1:i w l
unset multiplot
pause -1
