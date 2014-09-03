########################################################################
#                                                          value_check #
#                                                     v1.0-27-gc2c3b0b #
########################################################################
#                                                           compiler:  #
#                                                      compiled on pw  #
#                                                            FFLAGS =  #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################
Program started: 2014-09-03 14:51:18 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
value_check -! -F 1013.25 @ SP:@scale=100, 15 @ T :@offset=273.15 -S joz
efoslaw : 52.0 : 21.0 : 110, tmp : -33.0 : 1.0 : -110, -D 20120101 : m :
                                        24 @H -o t1.dat9.c -V t1.dat10.c
#-----------------------------------------------------------------------
Command parsing:
  -! { -! }
    all model as huge
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
