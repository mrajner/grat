########################################################################
#                                                                 grat #
#                                                     v1.2-12-g020dd5b #
########################################################################
#         compiler: GNU Fortran (GCC) 4.9.2 20150212 (Red Hat 4.9.2-6) #
#                                                         FFLAGS = -O0 #
#                                compiled on lidka 2015-05-15 12:37:23 #
#                                                                      #
#                                 Copyright 2013-2015 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                              License: GPLv3 or later #
########################################################################
Program started: 2015-05-15 13:01:25 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
grat -H -F ../data/pres.sfc.2012.nc @SP :pres -M1,2 -G rajner @GN -S joz
e:52:21:100 -D2012:20@H : 3@H -I 40 @DE : 2@3D -o ./t6.dat.out4.r : free
                                                      -V ./t6.dat.log4.r
#-----------------------------------------------------------------------
Command parsing:
  -H { -H }
      header
  -F { pres.sfc.2012.nc@SP:pres }
    ../data/pres.sfc.2012.nc@SP:pres
      Surface pressure (SP) pres lon lat level time 
      Opening file: pres.sfc.2012.nc , huge [T/F]: F
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
      rajner_green.dat GN columns: 1 2 lines: 85
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
  -o { t6.dat.out4.r:free }
    output file was set: t6.dat.out4.r
  -V { t6.dat.log4.r }
    verbose mode
    the log file was set t6.dat.log4.r
#-----------------------------------------------------------------------
    Processing: 1 site(s)
      Name lat [deg] lon [deg]     H [m]    Hp [m]    H* [m]   Hrsp[m]
      joze   52.0000   21.0000  100.0000        --        --        --
#-----------------------------------------------------------------------
