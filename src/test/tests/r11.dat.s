########################################################################
#                                                          value_check #
#                                                      v1.1-6-g95f7421 #
########################################################################
#                              compiler: ifort (IFORT) 14.0.2 20140120 #
#                               compiled on lenovo 2014-09-09 13:20:36 #
# FFLAGS = -fpic -O0 -xHost -warn all -I /home/mrajner/src/netcdf-fort #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################
Program started: 2014-09-09 13:21:02 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
value_check -F ../data/test_data.nc @GP : gp -S 10,j , r -J 1000,100, 66
                                                                      -V
#-----------------------------------------------------------------------
Command parsing:
  -F { test_data.nc@GP:gp }
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
  joze_a   52.0000   21.0000   70.000E+00
  rysy_a   49.1794   20.0883   70.000E+00
warning: level not found (no warning again) test_data.nc                                                                                                                                                                                             
  joze_a   52.0000   21.0000          NaN
  rysy_a   49.1794   20.0883          NaN
  joze_a   52.0000   21.0000          NaN
  rysy_a   49.1794   20.0883          NaN
Execution time:   0.0s (proc time:   0.0 |% 28.4)
#-----------------------------------------------------------------------
     out   90.0000  200.0000  100.623E+03
warning: outside lon|lat range maybe actual range not specified in nc file 
     out   90.0000   -5.0000          NaN
 55927.000 20120101000000   joze_a   52.0000   21.0000   99.257E+03
warning: cannot find date: 20090101000000      var: sp in file: ../data/test_data.nc 
 54832.000 20090101000000   joze_a   52.0000   21.0000          NaN
warning: cannot find date: 20040101000000      var: sp in file: ../data/test_data.nc 
 53005.000 20040101000000   joze_a   52.0000   21.0000          NaN
warning: something wrong with -F. non_exisiting_path : file do not exist 
warning: something wrong with -F. another_one : file do not exist 
    name       lat       lon           NN           NN
  balt_a   57.0000   21.0000          NaN          NaN
warning: reading whole file with levels into memory could slow down computation 
     out   90.0000  200.0000  100.623E+03  280.000E+00
  balt_a   57.0000   21.0000   99.512E+03   80.000E+00
warning: level not found (no warning again) test_data.nc                                                                                                                                                                                             
     out   90.0000  200.0000  100.623E+03          NaN
  balt_a   57.0000   21.0000   99.512E+03          NaN
########################################################################
#                                                          value_check #
#                                                      v1.1-6-g95f7421 #
########################################################################
#                              compiler: ifort (IFORT) 14.0.2 20140120 #
#                               compiled on lenovo 2014-09-09 13:20:36 #
# FFLAGS = -fpic -O0 -xHost -warn all -I /home/mrajner/src/netcdf-fort #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################
Program started: 2014-09-09 13:21:02 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
value_check -! -F ../data/test_data.nc @GP : gp -S 10,j , r -J 1000,100,
                                                                   66 -V
#-----------------------------------------------------------------------
Command parsing:
  -! { -! }
    all model as huge
  -F { test_data.nc@GP:gp }
    ../data/test_data.nc@GP:gp
      geopotential or geop. height (GP) gp lon lat level time 
      Opening file: test_data.nc , huge [T/F]: T
        Getting dim: lon ..     ok
        Getting dim: lat ..     ok
        Getting dim: level ..     ok
        Getting dim: time ..     ok
        Converting time:  hours since 2012-1-1 00:00:0.0
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
  joze_a   52.0000   21.0000   70.000E+00
  rysy_a   49.1794   20.0883   70.000E+00
warning: level not found (no warning again) test_data.nc                                                                                                                                                                                             
  joze_a   52.0000   21.0000          NaN
  rysy_a   49.1794   20.0883          NaN
  joze_a   52.0000   21.0000          NaN
  rysy_a   49.1794   20.0883          NaN
Execution time:   0.0s (proc time:   0.0 |% 22.1)
#-----------------------------------------------------------------------
     out   90.0000  200.0000  100.623E+03
warning: outside lon|lat range maybe actual range not specified in nc file 
     out   90.0000   -5.0000          NaN
 55927.000 20120101000000   joze_a   52.0000   21.0000   99.257E+03
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
     out   90.0000  200.0000  100.623E+03  280.000E+00
  balt_a   57.0000   21.0000   99.512E+03   80.000E+00
warning: level not found (no warning again) test_data.nc                                                                                                                                                                                             
     out   90.0000  200.0000  100.623E+03          NaN
  balt_a   57.0000   21.0000   99.512E+03          NaN
