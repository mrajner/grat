########################################################################
#                                                          value_check #
#                                                     v1.2-20-g9ef2c82 #
########################################################################
#         compiler: GNU Fortran (GCC) 4.9.2 20150212 (Red Hat 4.9.2-6) #
#                                                         FFLAGS = -O0 #
#                                compiled on lidka 2015-05-15 15:46:44 #
#                                                                      #
#                                 Copyright 2013-2015 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                              License: GPLv3 or later #
########################################################################
Program started: 2015-05-15 15:56:34 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
                   value_check -F 1e5 @GP -S 10,j , r -J 1000,100, 66 -V
#-----------------------------------------------------------------------
Command parsing:
  -F { -F1e5@GP }
    1e5@GP
      geopotential or geop. height (GP)
      constant value was set:     100.000E+03
  -S { -S10,j,r }
    10
    j
    r
      added site(s): 1
  -J { -J1000,100,66 }
    level pressure: 1000 100  66
  -V { -V }
    verbose mode
#-----------------------------------------------------------------------
    Processing: 2 site(s)
      Name lat [deg] lon [deg]     H [m]    Hp [m]    H* [m]   Hrsp[m]
    joze_a   52.0000   21.0000  110.0000        --        --        --
    rysy_a   49.1794   20.0883 2499.0000        --        --        --
#-----------------------------------------------------------------------
  joze_a   52.0000   21.0000  100.000E+03
  rysy_a   49.1794   20.0883  100.000E+03
  joze_a   52.0000   21.0000  100.000E+03
  rysy_a   49.1794   20.0883  100.000E+03
  joze_a   52.0000   21.0000  100.000E+03
  rysy_a   49.1794   20.0883  100.000E+03
Execution time:   0.0s (proc time:   0.0 |% 57.1)
#-----------------------------------------------------------------------
     out   90.0000  200.0000  101.325E+03
     out   90.0000  355.0000  101.325E+03
55927.000 20120101000000   joze_a   52.0000   21.0000  101.325E+03
54832.000 20090101000000   joze_a   52.0000   21.0000  101.325E+03
53005.000 20040101000000   joze_a   52.0000   21.0000  101.325E+03
    name       lat       lon           NN           NN
  balt_a   57.0000   21.0000          NaN          NaN
     out   90.0000  200.0000  101.325E+03  100.000E+03
  balt_a   57.0000   21.0000  101.325E+03  100.000E+03
     out   90.0000  200.0000  101.325E+03  100.000E+03
  balt_a   57.0000   21.0000  101.325E+03  100.000E+03
