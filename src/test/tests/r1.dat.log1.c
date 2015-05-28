########################################################################
#                                                          value_check #
#                                                    v1.1-117-ga157892 #
########################################################################
#         compiler: GNU Fortran (GCC) 4.9.2 20150212 (Red Hat 4.9.2-6) #
#                                                         FFLAGS = -O0 #
#                                compiled on lidka 2015-03-04 15:49:06 #
#                                                                      #
#                                 Copyright 2013-2015 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                              License: GPLv3 or later #
########################################################################
Program started: 2015-03-04 16:10:03 (+01h UTC)
#-----------------------------------------------------------------------
Command invoked:
value_check -F 1013.25 @ SP:@scale=100, 15 @ T :@offset=273.15 -S jozefo
slaw : 52.0 : 21.0 : 110, tmp : -33.0 : 1.0 : -110, -D 20120101 : m : 24
                                    @H -o t1.dat.out1.c -V t1.dat.log1.c
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
