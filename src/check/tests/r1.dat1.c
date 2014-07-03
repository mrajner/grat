########################################################################
#                                                          value_check #
#                                                                 beta #
#                                                   ifort 14(20131008) #
#                                   compiled on pw 2014-07-03 09:33:08 #
#                                 FFLAGS = -fpic -O2 -xHost -warn all  #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################
Program started: 2014-07-03 10:09:55 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
value_check -F 1013.25 @ SP:@scale=100, 15 @ T :@offset=273.15 -S jozefo
slaw : 52.0 : 21.0 : 110, tmp : -33.0 : 1.0 : -110, -D 20120101 : m : 24
                                            @H -o t1.dat0.c -V t1.dat1.c
#-----------------------------------------------------------------------
Command parsing:
  -F { -F1013.25@SP:@scale=100,15@T:@offset=273.15 }
    1013.25@SP:@scale=100
      Surface pressure (SP)
      constant value was set:       1.013E+03
      var modifier: scale 100  101.325E+03
      constant value was re-set:  101.325E+03
    15@T:@offset=273.15
      Surface temperature (T)
      constant value was set:      15.000E+00
      var modifier: offset 273.15  288.150E+00
      constant value was re-set:  288.150E+00
  -S { -Sjozefoslaw:52.0:21.0:110,tmp:-33.0:1.0:-110, }
    jozefoslaw:52.0:21.0:110
    tmp:-33.0:1.0:-110
      added site(s): 1
    
  -D { -D20120101:m:24@H }
    20120101:m:24@H
