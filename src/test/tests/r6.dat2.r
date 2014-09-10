########################################################################
#                                                                 grat #
#                                                     v1.1-11-g274498a #
########################################################################
#                              compiler: ifort (IFORT) 14.0.2 20140120 #
#                               compiled on lenovo 2014-09-10 09:13:38 #
#                                                    FFLAGS = (FFLAGS) #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################
Program started: 2014-09-10 09:13:47 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
grat -H -F ../data/pres.sfc.2012.nc @SP :pres -M1,2 -G rajner @GN -S joz
               e:52:21:100 -D2012:20@H : 3@H -o:free -V -I 20 @DE : 2@3D
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
  -o { -o:free }
    output file was set: 
  -V { -V }
    verbose mode
  -I { -I20@DE:2@3D }
    Range: 1
      DB:   0.00|DE:  20.000|I:n|DD: 1|DS:   0.00|HB:     0.0|HE: 60000.0|HS:  25.00|
#-----------------------------------------------------------------------
    Processing: 1 site(s)
      Name lat [deg] lon [deg]     H [m]    Hp [m]    H* [m]   Hrsp[m]
      joze   52.0000   21.0000  100.0000        --        --        --
#-----------------------------------------------------------------------
         mjd           date     name       lat       lon         h          G1D           GN 
   55927.000 20120101000000     joze   52.0000   21.0000   100.000     -299.220     -358.000
warning: hours not matching model dates (0,6,12,18) are rejecting and not shown in output 
   55927.250 20120101060000     joze   52.0000   21.0000   100.000     -299.190     -358.012
   55927.500 20120101120000     joze   52.0000   21.0000   100.000     -298.920     -357.638
   55927.750 20120101180000     joze   52.0000   21.0000   100.000     -298.110     -356.531
