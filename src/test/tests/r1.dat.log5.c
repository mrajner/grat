########################################################################
#                                                          value_check #
#                                                      v1.2-9-g2dcf143 #
########################################################################
#                              compiler: ifort (IFORT) 14.0.2 20140120 #
#                                  FFLAGS = -fpic -O0 -xHost -warn all #
#                               compiled on lenovo 2015-05-15 08:41:48 #
#                                                                      #
#                                 Copyright 2013-2015 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                              License: GPLv3 or later #
########################################################################
Program started: 2015-05-15 08:45:42 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
value_check -! -F 1013.25 @ SP:@scale=100, 15 @ T :@offset=273.15 -S g:5
        0 -D 2011123118 : 2 @M : 12 @H -o t1.dat.out5.c -V t1.dat.log5.c
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
  -S { -Sg:50 }
    g:50
  -D { -D2011123118:2@M:12@H }
    2011123118:2@M:12@H
      start date: 2011 12 31 18 00 00
      stop  date: 2012 02 31 18 00 00
      interval: 12.0H
      dates total: 125
  -o { -ot1.dat.out5.c }
    output file was set: t1.dat.out5.c
  -V { -Vt1.dat.log5.c }
    verbose mode
    the log file was set t1.dat.log5.c
#-----------------------------------------------------------------------
    Processing: 32 site(s)
#-----------------------------------------------------------------------
Execution time:   0.1s (proc time:   0.1 |% 57.4)
#-----------------------------------------------------------------------
