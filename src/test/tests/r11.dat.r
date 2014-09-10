########################################################################
#                                                          value_check #
#                                                     v1.1-12-g42924be #
########################################################################
#                              compiler: ifort (IFORT) 14.0.2 20140120 #
#                               compiled on lenovo 2014-09-10 11:59:13 #
#                                                    FFLAGS = (FFLAGS) #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################
Program started: 2014-09-10 11:59:37 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
value_check -F ../data/hgt.2012.nc @GP :hgt -S 10,j , r -J 1000,100, 66 
                                                                      -V
#-----------------------------------------------------------------------
Command parsing:
  -F { hgt.2012.nc@GP:hgt }
    ../data/hgt.2012.nc@GP:hgt
      geopotential or geop. height (GP) hgt lon lat level time 
      Opening file: hgt.2012.nc , huge [T/F]: F
        Getting dim: lon ..     ok
        Getting dim: lat ..     ok
        Getting dim: level ..     ok
        Getting dim: time ..     ok
        Converting time:  hours since 1800-1-1 00:00:0.0
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
  joze_a   52.0000   21.0000  119.000E+00
  rysy_a   49.1794   20.0883  144.000E+00
  joze_a   52.0000   21.0000   15.696E+03
  rysy_a   49.1794   20.0883   15.765E+03
  joze_a   52.0000   21.0000          NaN
  rysy_a   49.1794   20.0883          NaN
Execution time:   0.0s (proc time:   0.0 |% 98.2)
#-----------------------------------------------------------------------
     out   90.0000  200.0000   99.970E+03
     out   90.0000  355.0000   99.970E+03
 55927.000 20120101000000   joze_a   52.0000   21.0000   99.740E+03
 54832.000 20090101000000   joze_a   52.0000   21.0000          NaN
 53005.000 20040101000000   joze_a   52.0000   21.0000          NaN
    name       lat       lon           NN           NN
  balt_a   57.0000   21.0000          NaN          NaN
     out   90.0000  200.0000   99.970E+03  -39.000E+00
  balt_a   57.0000   21.0000  101.090E+03   99.000E+00
     out   90.0000  200.0000   99.970E+03    4.846E+03
  balt_a   57.0000   21.0000  101.090E+03    5.327E+03
