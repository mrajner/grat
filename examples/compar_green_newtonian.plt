
h=12
set log x
plot  'green_newtonian.dat' us 1:(column(h)/$1) w l lw 4

pause -1


