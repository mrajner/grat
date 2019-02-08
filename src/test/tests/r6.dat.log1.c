########################################################################
#                                                                 grat #
#                                                 v1.3-330-g43f9b28 () #
########################################################################
#         compiler: GNU Fortran (GCC) 7.3.1 20180712 (Red Hat 7.3.1-6) #
#                                                         FFLAGS = -O0 #
#                       compiled on chalmers 2019-02-08 14:16:31 +0100 #
#                                                                      #
#                                 Copyright 2013-2019 by Marcin Rajner #
#                   Warsaw University of Technology (2003-2017, 2019-) #
#                        Chalmers University of Technology (2017-2019) #
#                                                                      #
#                                              License: GPLv3 or later #
########################################################################
Program started: 2019-02-08 14:18:23 (+01h UTC)
#-----------------------------------------------------------------------
Command invoked:
grat -H -F 1013.25 @ SP:@scale=100 -M1,2 -G rajner @GN -S joze:52:21:100
 -D2012:20@H : 3@H -I 10 @DE : 2@3D -o t6.dat.out1.c : free -V t6.dat.lo
                                                                    g1.c
#-----------------------------------------------------------------------
Command parsing:
  -H { -H }
      header
  -F { -F1013.25@SP:@scale=100 }
    1013.25@SP:@scale=100
      Surface pressure (SP)
      constant value was set:       1.013E+03
      var modifier: scale 100  101.325E+03
      constant value was re-set:  101.325E+03
  -M { -M1,2 }
    method was set: 1D 2D 
  -G { -Grajner@GN }
    rajner@GN
      rajner GN columns: 1 2 lines: 85
  -S { -Sjoze:52:21:100 }
    joze:52:21:100
  -D { -D2012:20@H:3@H }
    2012:20@H:3@H
      start date: 2012 01 01 00 00 00
      stop  date: 2012 01 01 20 00 00
      interval:  3.0H
      dates total: 7
  -I { -I10@DE:2@3D }
    Range: 1
      DB:   0.00|DE:  10.000|I:n|DD: 1|DS:   0.00|HB:     0.0|HE: 60000.0|HS:  25.00|
  -o { -ot6.dat.out1.c:free }
    output file was set: t6.dat.out1.c
  -V { -Vt6.dat.log1.c }
    verbose mode
    the log file was set t6.dat.log1.c
#-----------------------------------------------------------------------
    Processing: 1 site(s)
      Name lat [deg] lon [deg]     H [m]    Hp [m]    H* [m]   Hrsp[m]
      joze   52.0000   21.0000  100.0000        --        --        --
#-----------------------------------------------------------------------
