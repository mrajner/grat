########################################################################
#                                                          value_check #
#                                                     v1.0-48-g1971ecb #
########################################################################
#                              compiler: ifort (IFORT) 14.0.3 20140422 #
#                           compiled on lidka_virt 2014-09-07 20:23:49 #
#                                  FFLAGS = -fpic -O0 -xHost -warn all #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################
Program started: 2014-09-07 20:24:26 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
                   value_check -F 1e5 @GP -S 10,j , r -J 1000,100, 66 -V
#-----------------------------------------------------------------------
Command parsing:
  -F { -F1e5@GP }
    1e5@GP
      geopotential or geop. height (GP)
      constant value was set:     100.000E+03
      constant value was re-set:  100.000E+03
  -S { -S10,j,r }
    10
warning: something wrong with -S|-R specification. IGNORED 
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
Execution time:   0.0s (proc time:   0.0 |%200.0)
#-----------------------------------------------------------------------
     out   90.0000  200.0000  101.325E+03 
     out   90.0000  355.0000  101.325E+03 
 55927.000 20120101000000   joze_a   52.0000   21.0000  101.325E+03 
 54832.000 20090101000000   joze_a   52.0000   21.0000  101.325E+03 
 53005.000 20040101000000   joze_a   52.0000   21.0000  101.325E+03 
warning: something wrong with -F. non_exisiting_path : file do not exist 
warning: something wrong with -F. another_one : file do not exist 
    name       lat       lon           NN           NN
  balt_a   57.0000   21.0000          NaN          NaN 
     out   90.0000  200.0000  101.325E+03  100.000E+03 
  balt_a   57.0000   21.0000  101.325E+03  100.000E+03 
     out   90.0000  200.0000  101.325E+03  100.000E+03 
  balt_a   57.0000   21.0000  101.325E+03  100.000E+03 
########################################################################
#                                                          value_check #
#                                                     v1.0-48-g1971ecb #
########################################################################
#                              compiler: ifort (IFORT) 14.0.3 20140422 #
#                           compiled on lidka_virt 2014-09-07 20:23:49 #
#                                  FFLAGS = -fpic -O0 -xHost -warn all #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################
Program started: 2014-09-07 20:24:26 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
                value_check -! -F 1e5 @GP -S 10,j , r -J 1000,100, 66 -V
#-----------------------------------------------------------------------
Command parsing:
  -! { -! }
    all model as huge
  -F { -F1e5@GP }
    1e5@GP
      geopotential or geop. height (GP)
      constant value was set:     100.000E+03
      constant value was re-set:  100.000E+03
  -S { -S10,j,r }
    10
warning: something wrong with -S|-R specification. IGNORED 
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
Execution time:   0.0s (proc time:   0.0 |%200.0)
#-----------------------------------------------------------------------
     out   90.0000  200.0000  101.325E+03 
     out   90.0000  355.0000  101.325E+03 
 55927.000 20120101000000   joze_a   52.0000   21.0000  101.325E+03 
 54832.000 20090101000000   joze_a   52.0000   21.0000  101.325E+03 
 53005.000 20040101000000   joze_a   52.0000   21.0000  101.325E+03 
warning: something wrong with -F. non_exisiting_path : file do not exist 
warning: something wrong with -F. another_one : file do not exist 
    name       lat       lon           NN           NN
  balt_a   57.0000   21.0000          NaN          NaN 
     out   90.0000  200.0000  101.325E+03  100.000E+03 
  balt_a   57.0000   21.0000  101.325E+03  100.000E+03 
     out   90.0000  200.0000  101.325E+03  100.000E+03 
  balt_a   57.0000   21.0000  101.325E+03  100.000E+03 
