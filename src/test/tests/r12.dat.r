########################################################################
#                                                                 grat #
#                                                      v1.1-8-g6bc8c09 #
########################################################################
#                              compiler: ifort (IFORT) 14.0.2 20140120 #
#                               compiled on lenovo 2014-09-09 15:36:36 #
#                                                    FFLAGS = (FFLAGS) #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################
Program started: 2014-09-09 15:37:46 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
grat -F ../data/pres.sfc.2012.nc @SP :pres -M2 -G@GN -Sj, r -I-1000@DD :
                                  100@AS -V -L@p , @s -o /dev/null -q -H
#-----------------------------------------------------------------------
Command parsing:
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
  -M { -M2 }
    method was set: 2D  
  -G { -G@GN }
    @GN
      merriam_green.dat GN columns: 1 2 lines: 85
  -S { -Sj,r }
    j
    r
      added site(s): 1
  -I { -I-1000@DD:100@AS }
    Range: 1
      DB:   0.00|DE: 180.000|I:n|DD:**|DS:   0.00|HB:     0.0|HE: 60000.0|HS:  25.00|
  -V { -V }
    verbose mode
  -L { -L@p,@s }
     <- points |sparse:  F
     <- summary |sparse:  F
  -o { null }
    output file was set: null
  -q { -q }
      quiet step 0
  -H { -H }
      header
#-----------------------------------------------------------------------
    Processing: 2 site(s)
      Name lat [deg] lon [deg]     H [m]    Hp [m]    H* [m]   Hrsp[m]
    joze_a   52.0000   21.0000  110.0000        --        --        --
    rysy_a   49.1794   20.0883 2499.0000        --        --        --
#-----------------------------------------------------------------------
    name          lat          lon     distance      azimuth          lat          lon         area      totarea           GN          SP
  joze_a   52.000E+00   21.000E+00    2.500E-06    0.000E+00   52.000E+00   21.000E+00  424.714E-03  424.714E-03    0.000E+00   99.74E+03
  joze_a   52.000E+00   21.000E+00    2.500E-06  100.000E+00   52.000E+00   21.000E+00  424.714E-03  849.428E-03    0.000E+00   99.74E+03
  joze_a   52.000E+00   21.000E+00    2.500E-06  200.000E+00   52.000E+00   21.000E+00  424.714E-03    1.274E+00    0.000E+00   99.74E+03
  joze_a   52.000E+00   21.000E+00   10.000E-06    0.000E+00   52.000E+00   21.000E+00   43.732E+12   43.732E+12 -317.713E+09   99.74E+03
  joze_a   52.000E+00   21.000E+00   10.000E-06  100.000E+00   52.000E+00   21.000E+00   43.732E+12   87.464E+12 -635.426E+09   99.74E+03
  joze_a   52.000E+00   21.000E+00   10.000E-06  200.000E+00   52.000E+00   21.000E+00   43.732E+12  131.196E+12 -953.139E+09   99.74E+03
  joze_a   52.000E+00   21.000E+00  135.000E+00    0.000E+00   -7.000E+00  201.000E+00   97.952E+12  229.149E+12 -953.139E+09  100.94E+03
  joze_a   52.000E+00   21.000E+00  135.000E+00  100.000E+00  -39.257E+00  136.930E+00   97.952E+12  327.101E+12 -953.139E+09  101.51E+03
  joze_a   52.000E+00   21.000E+00  135.000E+00  200.000E+00  -75.081E+00  311.050E+00   97.952E+12  425.054E+12 -953.139E+09   98.87E+03
 station npoints        area     area/R2 t_area_used
  joze_a       9  425.05E+12   10.47E+00    0.00E+00
  rysy_a   49.179E+00   20.088E+00    2.500E-06    0.000E+00   49.179E+00   20.088E+00  424.714E-03  424.714E-03    0.000E+00   98.11E+03
  rysy_a   49.179E+00   20.088E+00    2.500E-06  100.000E+00   49.179E+00   20.088E+00  424.714E-03  849.428E-03    0.000E+00   98.11E+03
  rysy_a   49.179E+00   20.088E+00    2.500E-06  200.000E+00   49.179E+00   20.088E+00  424.714E-03    1.274E+00    0.000E+00   98.11E+03
  rysy_a   49.179E+00   20.088E+00   10.000E-06    0.000E+00   49.179E+00   20.088E+00   43.732E+12   43.732E+12 -312.521E+09   98.11E+03
  rysy_a   49.179E+00   20.088E+00   10.000E-06  100.000E+00   49.179E+00   20.088E+00   43.732E+12   87.464E+12 -625.041E+09   98.11E+03
  rysy_a   49.179E+00   20.088E+00   10.000E-06  200.000E+00   49.179E+00   20.088E+00   43.732E+12  131.196E+12 -937.562E+09   98.11E+03
  rysy_a   49.179E+00   20.088E+00  135.000E+00    0.000E+00   -4.179E+00  200.088E+00   97.952E+12  229.149E+12 -937.562E+09  101.02E+03
  rysy_a   49.179E+00   20.088E+00  135.000E+00  100.000E+00  -37.979E+00  138.027E+00   97.952E+12  327.101E+12 -937.562E+09  100.99E+03
  rysy_a   49.179E+00   20.088E+00  135.000E+00  200.000E+00  -75.805E+00  299.620E+00   97.952E+12  425.054E+12 -937.562E+09   94.51E+03
 station npoints        area     area/R2 t_area_used
  rysy_a       9  425.05E+12   10.47E+00    0.00E+00
Execution time:   0.1s (proc time:   0.0 |% 16.5)
########################################################################
#                                                                 grat #
#                                                      v1.1-8-g6bc8c09 #
########################################################################
#                              compiler: ifort (IFORT) 14.0.2 20140120 #
#                               compiled on lenovo 2014-09-09 15:36:36 #
#                                                    FFLAGS = (FFLAGS) #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################
Program started: 2014-09-09 15:37:47 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
grat -F ../data/pres.sfc.2012.nc @SP :pres -M2 -G@GN -Sj, r -I-1000@DD :
                                 100@AS -V -L@p:s, @s -o /dev/null -q -H
#-----------------------------------------------------------------------
Command parsing:
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
  -M { -M2 }
    method was set: 2D  
  -G { -G@GN }
    @GN
      merriam_green.dat GN columns: 1 2 lines: 85
  -S { -Sj,r }
    j
    r
      added site(s): 1
  -I { -I-1000@DD:100@AS }
    Range: 1
      DB:   0.00|DE: 180.000|I:n|DD:**|DS:   0.00|HB:     0.0|HE: 60000.0|HS:  25.00|
  -V { -V }
    verbose mode
  -L { -L@p:s,@s }
     <- points |sparse:  T
     <- summary |sparse:  F
  -o { null }
    output file was set: null
  -q { -q }
      quiet step 0
  -H { -H }
      header
#-----------------------------------------------------------------------
    Processing: 2 site(s)
      Name lat [deg] lon [deg]     H [m]    Hp [m]    H* [m]   Hrsp[m]
    joze_a   52.0000   21.0000  110.0000        --        --        --
    rysy_a   49.1794   20.0883 2499.0000        --        --        --
#-----------------------------------------------------------------------
    name          lat          lon     distance      azimuth          lat          lon         area      totarea           GN 
  joze_a   52.000E+00   21.000E+00    2.500E-06  200.000E+00   52.000E+00   21.000E+00  424.714E-03    1.274E+00    0.000E+00 
  joze_a   52.000E+00   21.000E+00   10.000E-06  200.000E+00   52.000E+00   21.000E+00   43.732E+12  131.196E+12 -953.139E+09 
  joze_a   52.000E+00   21.000E+00  135.000E+00  200.000E+00  -75.081E+00  311.050E+00   97.952E+12  425.054E+12 -953.139E+09 
 station npoints        area     area/R2 t_area_used
  joze_a       9  425.05E+12   10.47E+00    0.00E+00
  rysy_a   49.179E+00   20.088E+00    2.500E-06  200.000E+00   49.179E+00   20.088E+00  424.714E-03    1.274E+00    0.000E+00 
  rysy_a   49.179E+00   20.088E+00   10.000E-06  200.000E+00   49.179E+00   20.088E+00   43.732E+12  131.196E+12 -937.562E+09 
  rysy_a   49.179E+00   20.088E+00  135.000E+00  200.000E+00  -75.805E+00  299.620E+00   97.952E+12  425.054E+12 -937.562E+09 
 station npoints        area     area/R2 t_area_used
  rysy_a       9  425.05E+12   10.47E+00    0.00E+00
Execution time:   0.1s (proc time:   0.0 |% 18.2)
