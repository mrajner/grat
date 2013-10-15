#!/usr/bin/gnuplot
#! for i in tmp tmp2; \
#do minmax $i -C |awk '{for (i=0;i<=3;i++) printf "%10.4f", $(14+i*2)-$(13+i*2)}END{print ""}'; \
#done

#!paste tmpp tmp tmph  tmpt | awk '{print $7+$8-$15,$7+$8-$22,$7+$8-$29}'  |minmax -C \
#| awk '{for (i=2;i<=6;i=i+2){print $i-$(i-1)}}'
#set multiplot lay 2,1
#plot 'tmpp' us 1:($7+$8) w l , \
#'tmp' us 1:7 w l, 'tmph' us 1:7 w l, \
# 'tmpt' us 1:7 w l

plot 'tmpt' us 1:7 w l
#unset multiplot
pause -1
