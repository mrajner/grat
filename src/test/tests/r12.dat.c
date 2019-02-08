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
Program started: 2019-02-08 14:22:04 (+01h UTC)
#-----------------------------------------------------------------------
Command invoked:
grat -F 1013.25 @ SP:@scale=100 -M2 -G@GN -Sj, r -I-1000@DD : 100@AS -V 
                                            -L@p , @s -o /dev/null -q -H
#-----------------------------------------------------------------------
Command parsing:
  -F { -F1013.25@SP:@scale=100 }
    1013.25@SP:@scale=100
      Surface pressure (SP)
      constant value was set:       1.013E+03
      var modifier: scale 100  101.325E+03
      constant value was re-set:  101.325E+03
  -M { -M2 }
    method was set: 2D 
  -G { -G@GN }
    @GN
      merriam GN columns: 1 2 lines: 85
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
  -o { -o/dev/null }
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
  joze_a   52.000E+00   21.000E+00    2.500E-06    0.000E+00   52.000E+00   21.000E+00  424.714E-03  424.714E-03    0.000E+00  101.32E+03
  joze_a   52.000E+00   21.000E+00    2.500E-06  100.000E+00   52.000E+00   21.000E+00  424.714E-03  849.428E-03    0.000E+00  101.32E+03
  joze_a   52.000E+00   21.000E+00    2.500E-06  200.000E+00   52.000E+00   21.000E+00  424.714E-03    1.274E+00    0.000E+00  101.32E+03
  joze_a   52.000E+00   21.000E+00   10.000E-06    0.000E+00   52.000E+00   21.000E+00   43.732E+12   43.732E+12 -322.762E+09  101.32E+03
  joze_a   52.000E+00   21.000E+00   10.000E-06  100.000E+00   52.000E+00   21.000E+00   43.732E+12   87.464E+12 -645.524E+09  101.32E+03
  joze_a   52.000E+00   21.000E+00   10.000E-06  200.000E+00   52.000E+00   21.000E+00   43.732E+12  131.196E+12 -968.285E+09  101.32E+03
  joze_a   52.000E+00   21.000E+00  135.000E+00    0.000E+00   -7.000E+00  201.000E+00   97.952E+12  229.149E+12 -968.285E+09  101.32E+03
  joze_a   52.000E+00   21.000E+00  135.000E+00  100.000E+00  -39.257E+00  136.930E+00   97.952E+12  327.101E+12 -968.285E+09  101.32E+03
  joze_a   52.000E+00   21.000E+00  135.000E+00  200.000E+00  -75.081E+00  311.050E+00   97.952E+12  425.054E+12 -968.285E+09  101.32E+03
 station npoints        area     area/R2 t_area_used
  joze_a       9  425.05E+12   10.47E+00    0.00E+00
  rysy_a   49.179E+00   20.088E+00    2.500E-06    0.000E+00   49.179E+00   20.088E+00  424.714E-03  424.714E-03    0.000E+00  101.32E+03
  rysy_a   49.179E+00   20.088E+00    2.500E-06  100.000E+00   49.179E+00   20.088E+00  424.714E-03  849.428E-03    0.000E+00  101.32E+03
  rysy_a   49.179E+00   20.088E+00    2.500E-06  200.000E+00   49.179E+00   20.088E+00  424.714E-03    1.274E+00    0.000E+00  101.32E+03
  rysy_a   49.179E+00   20.088E+00   10.000E-06    0.000E+00   49.179E+00   20.088E+00   43.732E+12   43.732E+12 -322.762E+09  101.32E+03
  rysy_a   49.179E+00   20.088E+00   10.000E-06  100.000E+00   49.179E+00   20.088E+00   43.732E+12   87.464E+12 -645.524E+09  101.32E+03
  rysy_a   49.179E+00   20.088E+00   10.000E-06  200.000E+00   49.179E+00   20.088E+00   43.732E+12  131.196E+12 -968.285E+09  101.32E+03
  rysy_a   49.179E+00   20.088E+00  135.000E+00    0.000E+00   -4.179E+00  200.088E+00   97.952E+12  229.149E+12 -968.285E+09  101.32E+03
  rysy_a   49.179E+00   20.088E+00  135.000E+00  100.000E+00  -37.979E+00  138.027E+00   97.952E+12  327.101E+12 -968.285E+09  101.32E+03
  rysy_a   49.179E+00   20.088E+00  135.000E+00  200.000E+00  -75.805E+00  299.620E+00   97.952E+12  425.054E+12 -968.285E+09  101.32E+03
 station npoints        area     area/R2 t_area_used
  rysy_a       9  425.05E+12   10.47E+00    0.00E+00
Execution time:   0.0s (proc time:   0.0 |%  Inf)
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
Program started: 2019-02-08 14:22:04 (+01h UTC)
#-----------------------------------------------------------------------
Command invoked:
grat -F 1013.25 @ SP:@scale=100 -M2 -G@GN -Sj, r -I-1000@DD : 100@AS -V 
                                           -L@p:s, @s -o /dev/null -q -H
#-----------------------------------------------------------------------
Command parsing:
  -F { -F1013.25@SP:@scale=100 }
    1013.25@SP:@scale=100
      Surface pressure (SP)
      constant value was set:       1.013E+03
      var modifier: scale 100  101.325E+03
      constant value was re-set:  101.325E+03
  -M { -M2 }
    method was set: 2D 
  -G { -G@GN }
    @GN
      merriam GN columns: 1 2 lines: 85
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
  -o { -o/dev/null }
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
  joze_a   52.000E+00   21.000E+00   10.000E-06  200.000E+00   52.000E+00   21.000E+00   43.732E+12  131.196E+12 -968.285E+09
  joze_a   52.000E+00   21.000E+00  135.000E+00  200.000E+00  -75.081E+00  311.050E+00   97.952E+12  425.054E+12 -968.285E+09
 station npoints        area     area/R2 t_area_used
  joze_a       9  425.05E+12   10.47E+00    0.00E+00
  rysy_a   49.179E+00   20.088E+00    2.500E-06  200.000E+00   49.179E+00   20.088E+00  424.714E-03    1.274E+00    0.000E+00
  rysy_a   49.179E+00   20.088E+00   10.000E-06  200.000E+00   49.179E+00   20.088E+00   43.732E+12  131.196E+12 -968.285E+09
  rysy_a   49.179E+00   20.088E+00  135.000E+00  200.000E+00  -75.805E+00  299.620E+00   97.952E+12  425.054E+12 -968.285E+09
 station npoints        area     area/R2 t_area_used
  rysy_a       9  425.05E+12   10.47E+00    0.00E+00
Execution time:   0.0s (proc time:   0.0 |%  Inf)
