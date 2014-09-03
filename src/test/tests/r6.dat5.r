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
Program started: 2014-09-03 12:39:43 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
grat -! -H -F ../data/pres.sfc.2012.nc @SP :pres -M1,2 -G rajner @GN -S 
             joze:52:21:100 -D2012:20@H : 3@H -o:free -V -I 0 @DE : 2@3D
#-----------------------------------------------------------------------
Command parsing:
  -! { -! }
    all model as huge
  -H { -H }
      header
  -F { pres.sfc.2012.nc@SP:pres }
    ../data/pres.sfc.2012.nc@SP:pres
warning: something wrong with -F. ../data/pres.sfc.2012.nc : file do not exist 
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
  -I { -I0@DE:2@3D }
    Range: 1
      DB:   0.00|DE:   0.000|I:n|DD: 1|DS:   0.00|HB:     0.0|HE: 60000.0|HS:  25.00|
#-----------------------------------------------------------------------
    Processing: 1 site(s)
      Name lat [deg] lon [deg]     H [m]    Hp [m]    H* [m]   Hrsp[m]
      joze   52.0000   21.0000  100.0000        --        --        -- 
#-----------------------------------------------------------------------
         mjd           date     name       lat       lon         h          G1D           GN 
   55927.000 20120101000000     joze   52.0000   21.0000   100.000error: @SP is required with -M1D 
