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
Program started: 2014-09-07 20:24:24 (+02h UTC)
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
warning: reading whole file with levels into memory could slow down computation 
  joze_a   52.0000   21.0000  119.000E+00 
  rysy_a   49.1794   20.0883  144.000E+00 
  joze_a   52.0000   21.0000   15.696E+03 
  rysy_a   49.1794   20.0883   15.765E+03 
warning: level not found (no warning again) hgt.2012.nc                                                                                                                                                                                              
  joze_a   52.0000   21.0000          NaN 
  rysy_a   49.1794   20.0883          NaN 
Execution time:   0.0s (proc time:   0.0 |%106.4)
#-----------------------------------------------------------------------
     out   90.0000  200.0000   99.970E+03 
     out   90.0000  355.0000   99.970E+03 
 55927.000 20120101000000   joze_a   52.0000   21.0000   99.740E+03 
warning: cannot find date: 20090101000000      var: pres in file: ../data/pres.sfc.2012.nc 
 54832.000 20090101000000   joze_a   52.0000   21.0000          NaN 
warning: cannot find date: 20040101000000      var: pres in file: ../data/pres.sfc.2012.nc 
 53005.000 20040101000000   joze_a   52.0000   21.0000          NaN 
warning: something wrong with -F. non_exisiting_path : file do not exist 
warning: something wrong with -F. another_one : file do not exist 
    name       lat       lon           NN           NN
  balt_a   57.0000   21.0000          NaN          NaN 
warning: reading whole file with levels into memory could slow down computation 
     out   90.0000  200.0000   99.970E+03  -39.000E+00 
  balt_a   57.0000   21.0000  101.090E+03   99.000E+00 
     out   90.0000  200.0000   99.970E+03    4.846E+03 
  balt_a   57.0000   21.0000  101.090E+03    5.327E+03 
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
Program started: 2014-09-07 20:24:25 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
value_check -! -F ../data/hgt.2012.nc @GP :hgt -S 10,j , r -J 1000,100, 
                                                                   66 -V
#-----------------------------------------------------------------------
Command parsing:
  -! { -! }
    all model as huge
  -F { hgt.2012.nc@GP:hgt }
    ../data/hgt.2012.nc@GP:hgt
      geopotential or geop. height (GP) hgt lon lat level time 
      Opening file: hgt.2012.nc , huge [T/F]: T
        Getting dim: lon ..     ok
        Getting dim: lat ..     ok
        Getting dim: level ..     ok
        Getting dim: time ..     ok
        Converting time:  hours since 1800-1-1 00:00:0.0
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
  joze_a   52.0000   21.0000  119.000E+00 
  rysy_a   49.1794   20.0883  144.000E+00 
  joze_a   52.0000   21.0000   15.696E+03 
  rysy_a   49.1794   20.0883   15.765E+03 
warning: level not found (no warning again) hgt.2012.nc                                                                                                                                                                                              
  joze_a   52.0000   21.0000          NaN 
  rysy_a   49.1794   20.0883          NaN 
Execution time:   0.0s (proc time:   0.0 |% 96.8)
#-----------------------------------------------------------------------
     out   90.0000  200.0000   99.970E+03 
     out   90.0000  355.0000   99.970E+03 
 55927.000 20120101000000   joze_a   52.0000   21.0000   99.740E+03 
warning: check NetCDF: Index exceeds dimension bound 
warning: skipping get_value 
 54832.000 20090101000000   joze_a   52.0000   21.0000          NaN 
warning: check NetCDF: Index exceeds dimension bound 
warning: skipping get_value 
 53005.000 20040101000000   joze_a   52.0000   21.0000          NaN 
warning: something wrong with -F. non_exisiting_path : file do not exist 
warning: something wrong with -F. another_one : file do not exist 
    name       lat       lon           NN           NN
  balt_a   57.0000   21.0000          NaN          NaN 
     out   90.0000  200.0000   99.970E+03  -39.000E+00 
  balt_a   57.0000   21.0000  101.090E+03   99.000E+00 
     out   90.0000  200.0000   99.970E+03    4.846E+03 
  balt_a   57.0000   21.0000  101.090E+03    5.327E+03 
