#!/usr/bin/gnuplot

i=9
!gmtmath tmp SUM = | column -t
exit
set multiplot lay 3,1
plot 'tmp' us 1:i w l
plot 'tmp2' us 1:i w l
plot 'tmp3' us 1:i w l
unset multiplot
pause -1
