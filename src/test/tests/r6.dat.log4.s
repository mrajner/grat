########################################################################
#                                                                 grat #
#                                                 v1.3-363-gf045805 () #
########################################################################
#         compiler: GNU Fortran (GCC) 5.3.1 20160406 (Red Hat 5.3.1-6) #
#                                                         FFLAGS = -O0 #
#                               compiled on  2019-02-13 08:52:53 +0100 #
#                                                                      #
#                                 Copyright 2013-2019 by Marcin Rajner #
#                   Warsaw University of Technology (2003-2017, 2019-) #
#                        Chalmers University of Technology (2017-2019) #
#                                                                      #
#                                              License: GPLv3 or later #
########################################################################
Program started: 2019-02-13 12:52:07 (+01h UTC)
#-----------------------------------------------------------------------
Command invoked:
grat -H -F ../data/test_data.nc @SP : sp -M1,2 -G rajner @GN -S joze:52:
21:100 -D2012:20@H : 3@H -I 40 @DE : 2@3D -o t6.dat.out4.s : free -V t6.
                                                              dat.log4.s
#-----------------------------------------------------------------------
Command parsing:
  -H { -H }
      header
  -F { -F../data/test_data.nc@SP:sp }
    ../data/test_data.nc@SP:sp
      Surface pressure (SP) sp lon lat level time 
      Opening file: test_data.nc , huge [T/F]: F
        Getting dim: lon ..     ok
        Getting dim: lat ..     ok
        Getting dim: level ..     ok
        Getting dim: time ..     ok
        Converting time:  hours since 2012-1-1 00:00:0.0
  -M { -M1,2 }
    method was set: 1D 2D 
  -G { -Grajner@GN }
    rajner@GN
      rajner GN columns: 1 2 lines: 85
  -S { -Sjoze:52:21:100 }
    joze:52:21:100
  -D { -D2012:20@H:12@H }
    2012:20@H:12@H
      start date: 2012 01 01 00 00 00
      stop  date: 2012 01 01 20 00 00
      interval: 12.0H
      dates total: 2
  -I { -I40@DE:2@3D }
    Range: 1
      DB:   0.00|DE:  40.000|I:n|DD: 1|DS:   0.00|HB:     0.0|HE: 60000.0|HS:  25.00|
  -o { -ot6.dat.out4.s:free }
    output file was set: t6.dat.out4.s
  -V { -Vt6.dat.log4.s }
    verbose mode
    the log file was set t6.dat.log4.s
#-----------------------------------------------------------------------
    Processing: 1 site(s)
      Name lat [deg] lon [deg]     H [m]    Hp [m]    H* [m]   Hrsp[m]
      joze   52.0000   21.0000  100.0000        --        --        --
#-----------------------------------------------------------------------
Execution time:   0.0s (proc time:   0.2 |%  Inf)
