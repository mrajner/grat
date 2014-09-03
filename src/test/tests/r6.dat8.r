########################################################################
#                                                                 grat #
#                                                     v1.0-23-gf95f55a #
########################################################################
#                                                           compiler:  #
#                                                    compiled on grat  #
#                                                            FFLAGS =  #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################
Program started: 2014-09-03 13:38:46 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
grat -! -H -F ../data/pres.sfc.2012.nc @SP :pres -M1,2 -G rajner @GN -S 
            joze:52:21:100 -D2012:20@H : 3@H -o:free -V -I 30 @DE : 2@3D
#-----------------------------------------------------------------------
Command parsing:
  -! { -! }
    all model as huge
  -H { -H }
      header
  -F { pres.sfc.2012.nc@SP:pres }
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
  -I { -I30@DE:2@3D }
    Range: 1
      DB:   0.00|DE:  30.000|I:n|DD: 1|DS:   0.00|HB:     0.0|HE: 60000.0|HS:  25.00|
#-----------------------------------------------------------------------
    Processing: 1 site(s)
      Name lat [deg] lon [deg]     H [m]    Hp [m]    H* [m]   Hrsp[m]
      joze   52.0000   21.0000  100.0000        --        --        -- 
#-----------------------------------------------------------------------
         mjd           date     name       lat       lon         h          G1D           GN 
   55927.000 20120101000000     joze   52.0000   21.0000   100.000     -299.220     -322.604 
warning: hours not matching model dates (0,6,12,18) are rejecting and not shown in output 
   55927.250 20120101060000     joze   52.0000   21.0000   100.000     -299.190     -322.624 
   55927.500 20120101120000     joze   52.0000   21.0000   100.000     -298.920     -322.244 
   55927.750 20120101180000     joze   52.0000   21.0000   100.000     -298.110     -321.141 