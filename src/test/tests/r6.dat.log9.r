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
Program started: 2019-02-08 14:18:18 (+01h UTC)
#-----------------------------------------------------------------------
Command invoked:
grat -! -H -F ../data/pres.sfc.2012.nc @SP :pres -M1,2 -G rajner @GN -S 
joze:52:21:100 -D2012:20@H : 3@H -I 40 @DE : 2@3D -o t6.dat.out9.r : fre
                                                      e -V t6.dat.log9.r
#-----------------------------------------------------------------------
Command parsing:
  -! { -! }
    all model as huge
  -H { -H }
      header
  -F { -F../data/pres.sfc.2012.nc@SP:pres }
    ../data/pres.sfc.2012.nc@SP:pres
      Surface pressure (SP) pres lon lat level time 
      Opening file: pres.sfc.2012.nc , huge [T/F]: T
        Getting dim: lon ..     ok
        Getting dim: lat ..     ok
        Getting dim: level ..  level not found, allocating (1)...
          lat, lon, time, pres,
        Getting dim: time ..     ok
        Converting time:  hours since 1800-1-1 00:00:0.0
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
  -I { -I40@DE:2@3D }
    Range: 1
      DB:   0.00|DE:  40.000|I:n|DD: 1|DS:   0.00|HB:     0.0|HE: 60000.0|HS:  25.00|
  -o { -ot6.dat.out9.r:free }
    output file was set: t6.dat.out9.r
  -V { -Vt6.dat.log9.r }
    verbose mode
    the log file was set t6.dat.log9.r
#-----------------------------------------------------------------------
    Processing: 1 site(s)
      Name lat [deg] lon [deg]     H [m]    Hp [m]    H* [m]   Hrsp[m]
      joze   52.0000   21.0000  100.0000        --        --        --
#-----------------------------------------------------------------------
