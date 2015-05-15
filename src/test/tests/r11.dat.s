########################################################################
#                                                          value_check #
#                                                     v1.2-15-g4392bc1 #
########################################################################
#         compiler: GNU Fortran (GCC) 4.9.2 20150212 (Red Hat 4.9.2-6) #
#                                                         FFLAGS = -O0 #
#                                compiled on lidka 2015-05-15 13:12:17 #
#                                                                      #
#                                 Copyright 2013-2015 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                              License: GPLv3 or later #
########################################################################
Program started: 2015-05-15 13:23:03 (+02h UTC)
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
Execution time:   0.0s (proc time:   0.0 |% 87.7)
#-----------------------------------------------------------------------
     out   90.0000  200.0000  100.623E+03
     out   90.0000   -5.0000          NaN
55927.000 20120101000000   joze_a   52.0000   21.0000   99.257E+03
54832.000 20090101000000   joze_a   52.0000   21.0000          NaN
53005.000 20040101000000   joze_a   52.0000   21.0000          NaN
    name       lat       lon           NN           NN
  balt_a   57.0000   21.0000          NaN          NaN
     out   90.0000  200.0000  100.623E+03  280.000E+00
  balt_a   57.0000   21.0000   99.512E+03   80.000E+00
     out   90.0000  200.0000  100.623E+03          NaN
  balt_a   57.0000   21.0000   99.512E+03          NaN
