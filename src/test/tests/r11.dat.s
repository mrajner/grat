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
value_check -F ../data/test_data.nc @GP : gp -S 10,j , r -J 1000,100, 66
                                                                      -V
#-----------------------------------------------------------------------
Command parsing:
  -F { -F../data/test_data.nc@GP:gp }
    ../data/test_data.nc@GP:gp
      geopotential or geop. height (GP) gp lon lat level time 
      Opening file: test_data.nc , huge [T/F]: F
        Getting dim: lon ..     ok
        Getting dim: lat ..     ok
        Getting dim: level ..     ok
        Getting dim: time ..     ok
        Converting time:  hours since 2012-1-1 00:00:0.0
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
  joze_a   52.0000   21.0000   70.000E+00
  rysy_a   49.1794   20.0883   70.000E+00
  joze_a   52.0000   21.0000          NaN
  rysy_a   49.1794   20.0883          NaN
  joze_a   52.0000   21.0000          NaN
  rysy_a   49.1794   20.0883          NaN
Execution time: *****h (proc time:   0.0 |%  0.0)
#-----------------------------------------------------------------------
     out   90.0000  200.0000  100.623E+03
     out   90.0000   -5.0000          NaN
55927.000 20120101000000   joze_a   52.0000   21.0000   99.257E+03
54832.000 20090101000000   joze_a   52.0000   21.0000          NaN
53005.000 20040101000000   joze_a   52.0000   21.0000          NaN
