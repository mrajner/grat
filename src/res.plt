#!/usr/bin/gnuplot
! for i in tmp tmp2; \
do minmax $i -C |awk '{for (i=0;i<=3;i++) printf "%10.4f", $(14+i*2)-$(13+i*2)}END{print ""}'; \
done

!join tmp tmp2 > tmp3
!paste tmp0 tmp1 tmp | awk '{print $7-$14,$7-$21}' |minmax -C
exit
set multiplot lay 2,1
plot 'tmp3' us 1:($7-$15-$16-$13) w l ,\
     'tmp3' us 1:($7) w l ,\
     'tmp1' us 1:($7) w l lw 3 ,\
     'tmp0' us 1:($7) w l 
unset multiplot
pause -1
