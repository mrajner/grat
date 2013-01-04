WEN="/home/mrajner/pub/2012_wenecja/dane/szereg_358.txt"



#set log x

plot "../dat/merriam_green.dat" us 1:3 w lp,\
     "../dat/rajner_green.dat" us 1:3 w lp


#pause -1

plot "./354_1_2" us 6 w l,\
"./354_1_3" us 6  w l ,\
"./354_1_5" us 6 w l ,\
 "./354_1_4" us ($5-273.15) w l axis x1y2,\
 "./354_1_6" us ($5-273.15) w l axis x1y2
pause -1
