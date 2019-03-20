########################################################################
#                                                          value_check #
#                                                 v1.3-397-g7b6d6e0 () #
########################################################################
#         compiler: GNU Fortran (GCC) 7.3.1 20180712 (Red Hat 7.3.1-6) #
#                                                         FFLAGS = -O0 #
#                               compiled on  2019-03-20 18:04:30 +0100 #
#                                                                      #
#                                 Copyright 2013-2019 by Marcin Rajner #
#                   Warsaw University of Technology (2003-2017, 2019-) #
#                        Chalmers University of Technology (2017-2019) #
#                                                                      #
#                                              License: GPLv3 or later #
########################################################################
Program started: 2019-03-20 18:06:42 (+01h UTC)
#-----------------------------------------------------------------------
Command invoked:
value_check -F ../data/hgt.2012.nc @GP :hgt -S 10,j , r -J 1000,100, 66 
                                                                      -V
#-----------------------------------------------------------------------
Command parsing:
  -F { -F../data/hgt.2012.nc@GP:hgt }
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
Execution time: *****h (proc time:   0.0 |%  0.0)
#-----------------------------------------------------------------------
     out   90.0000  200.0000   99.970E+03
     out   90.0000  355.0000   99.970E+03
55927.000 20120101000000   joze_a   52.0000   21.0000   99.740E+03
54832.000 20090101000000   joze_a   52.0000   21.0000          NaN
53005.000 20040101000000   joze_a   52.0000   21.0000          NaN
