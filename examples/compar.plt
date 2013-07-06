

olsson="./green_newtonian_olsson.dat.10hpa"
spotl="./green_newtonian_spotl.dat.10hpa"
set log x

plot \
spotl us 1:2 w l lw 5  , \
spotl us 1:3 w l lw 3  , \
spotl us 1:4 w l lw 3  , \
spotl us 1:5 w l lw 3  , \
spotl us 1:6 w l lw 3  , \
spotl us 1:7 w l lw 1  , \
spotl us 1:8 w l lw 1  , \
spotl us 1:9 w l lw 1  , \
spotl us 1:10 w l lw 1 , \
spotl us 1:11 w l lw 1 , \
spotl us 1:12 w l lw 1 
pause -1
