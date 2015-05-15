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
value_check -F 1013.25 @ SP:@scale=100, 15 @ T :@offset=273.15 -S g:50 -
           D 2011123118 : 2 @M : 12 @H -o t1.dat.out2.c -V t1.dat.log2.c
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
  -S { -Sg:50 }
    g:50
  -D { -D2011123118:2@M:12@H }
    2011123118:2@M:12@H
      start date: 2011 12 31 18 00 00
      stop  date: 2012 02 31 18 00 00
      interval: 12.0H
      dates total: 125
  -o { -ot1.dat.out2.c }
    output file was set: t1.dat.out2.c
  -V { -Vt1.dat.log2.c }
    verbose mode
    the log file was set t1.dat.log2.c
#-----------------------------------------------------------------------
    Processing: 32 site(s)
#-----------------------------------------------------------------------
Execution time:   0.1s (proc time:   0.1 |% 37.7)
#-----------------------------------------------------------------------
