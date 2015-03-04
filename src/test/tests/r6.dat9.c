########################################################################
#                                                                 grat #
#                                                     v1.1-42-gff6fdb2 #
########################################################################
#                              compiler: ifort (IFORT) 14.0.2 20140120 #
#                               compiled on lenovo 2014-09-16 08:44:26 #
#               FFLAGS = -fpic -O2 -xHost -warn all -DWITH_MONTE_CARLO #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################
Program started: 2014-09-16 08:48:00 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
grat -! -H -F 1013.25 @ SP:@scale=100 -M1,2 -G rajner @GN -S joze:52:21:
                       100 -D2012:20@H : 3@H -o:free -V -I 40 @DE : 2@3D
#-----------------------------------------------------------------------
Command parsing:
  -! { -! }
    all model as huge
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
      rajner_green.dat GN columns: 1 2 lines: 85
  -S { -Sjoze:52:21:100 }
    joze:52:21:100
  -D { -D2012:20@H:3@H }
    2012:20@H:3@H
      start date: 2012 01 01 00 00 00
      stop  date: 2012 01 01 20 00 00
      interval:  3.0H
      dates total: 7
  -o { -o:free }
    output file was set: 
  -V { -V }
    verbose mode
  -I { -I40@DE:2@3D }
    Range: 1
      DB:   0.00|DE:  40.000|I:n|DD: 1|DS:   0.00|HB:     0.0|HE: 60000.0|HS:  25.00|
#-----------------------------------------------------------------------
    Processing: 1 site(s)
      Name lat [deg] lon [deg]     H [m]    Hp [m]    H* [m]   Hrsp[m]
      joze   52.0000   21.0000  100.0000        --        --        --
#-----------------------------------------------------------------------
      mjd           date     name       lat       lon         h          G1D           GN 
55927.000 20120101000000     joze   52.0000   21.0000   100.000     -303.975     -290.259
warning: hours not matching model dates (0,6,12,18) are rejecting and not shown in output 
55927.250 20120101060000     joze   52.0000   21.0000   100.000     -303.975     -290.259
55927.500 20120101120000     joze   52.0000   21.0000   100.000     -303.975     -290.259
55927.750 20120101180000     joze   52.0000   21.0000   100.000     -303.975     -290.259
