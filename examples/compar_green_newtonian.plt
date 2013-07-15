
h=10
set log x
plot for [h=2:10] 'green_newtonian.dat' us 1:h w l lw 4, 'green_newtonian_spotl.dat' us 1:h w l, 'green_newtonian_olsson.dat' us 1:h w l,\
'/home/mrajner/src/gotic2/data/grn1.data' us 1:3 w lp

pause -1


