########################################################################
#                                                          value_check #
#                                                     v1.0-48-g1971ecb #
########################################################################
#                              compiler: ifort (IFORT) 14.0.3 20140422 #
#                           compiled on lidka_virt 2014-09-07 19:44:30 #
#                                  FFLAGS = -fpic -O0 -xHost -warn all #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################
Program started: 2014-09-07 19:44:38 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
value_check -Sg:75,j,b -F../data/test_data.nc @SP : sp -P ../../../polyg
                                                    on/baltyk.poly -V -H
#-----------------------------------------------------------------------
Command parsing:
  -S { -Sg:75,j,b }
    g:75
    j
      added site(s): 1
    b
      added site(s): 1
  -F { test_data.nc@SP:sp }
    ../data/test_data.nc@SP:sp
      Surface pressure (SP) sp lon lat level time 
      Opening file: test_data.nc , huge [T/F]: F
        Getting dim: lon ..     ok
        Getting dim: lat ..     ok
        Getting dim: level ..     ok
        Getting dim: time ..     ok
        Converting time:  hours since 2012-1-1 00:00:0.0
  -P { baltyk.poly }
    polygon file: ../../../polygon/baltyk.poly
       name: ../../../polygon/baltyk.poly
       number of polygons: 1
       use [true/false]: T
       number of coords: 17
  -V { -V }
    verbose mode
  -H { -H }
      header
#-----------------------------------------------------------------------
    Processing: 17 site(s)
#-----------------------------------------------------------------------
    name       lat       lon           SP
    auto  -90.0000    0.0000    0.000E+00 
    auto  -15.0000    0.0000    0.000E+00 
    auto   60.0000    0.0000    0.000E+00 
    auto  -90.0000   75.0000    0.000E+00 
    auto  -15.0000   75.0000    0.000E+00 
    auto   60.0000   75.0000    0.000E+00 
    auto  -90.0000  150.0000    0.000E+00 
    auto  -15.0000  150.0000    0.000E+00 
    auto   60.0000  150.0000    0.000E+00 
    auto  -90.0000  225.0000    0.000E+00 
    auto  -15.0000  225.0000    0.000E+00 
    auto   60.0000  225.0000    0.000E+00 
    auto  -90.0000  300.0000    0.000E+00 
    auto  -15.0000  300.0000    0.000E+00 
    auto   60.0000  300.0000    0.000E+00 
  joze_a   52.0000   21.0000    0.000E+00 
  balt_a   57.0000   21.0000   99.512E+03 
Execution time:   0.0s (proc time:   0.0 |% 43.3)
#-----------------------------------------------------------------------
########################################################################
#                                                                 grat #
#                                                     v1.0-48-g1971ecb #
########################################################################
#                              compiler: ifort (IFORT) 14.0.3 20140422 #
#                           compiled on lidka_virt 2014-09-07 19:44:30 #
#                                  FFLAGS = -fpic -O0 -xHost -warn all #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################
Program started: 2014-09-07 19:44:38 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
grat -Sg:75,j,b -F../data/test_data.nc @SP : sp -P ../../../polygon/balt
                                                           yk.poly -V -H
#-----------------------------------------------------------------------
Command parsing:
  -S { -Sg:75,j,b }
    g:75
    j
      added site(s): 1
    b
      added site(s): 1
  -F { test_data.nc@SP:sp }
    ../data/test_data.nc@SP:sp
      Surface pressure (SP) sp lon lat level time 
      Opening file: test_data.nc , huge [T/F]: F
        Getting dim: lon ..     ok
        Getting dim: lat ..     ok
        Getting dim: level ..     ok
        Getting dim: time ..     ok
        Converting time:  hours since 2012-1-1 00:00:0.0
  -P { baltyk.poly }
    polygon file: ../../../polygon/baltyk.poly
       name: ../../../polygon/baltyk.poly
       number of polygons: 1
       use [true/false]: T
       number of coords: 17
  -V { -V }
    verbose mode
  -H { -H }
      header
warning: -M no method was set assuming 1D 
#-----------------------------------------------------------------------
    Processing: 17 site(s)
#-----------------------------------------------------------------------
    name       lat       lon         h          G1D
    auto  -90.0000    0.0000     0.000 -304.029E+00 
    auto  -15.0000    0.0000     0.000 -307.989E+00 
    auto   60.0000    0.0000     0.000 -298.536E+00 
    auto  -90.0000   75.0000     0.000 -304.029E+00 
    auto  -15.0000   75.0000     0.000 -307.989E+00 
    auto   60.0000   75.0000     0.000 -298.536E+00 
    auto  -90.0000  150.0000     0.000 -304.029E+00 
    auto  -15.0000  150.0000     0.000 -307.989E+00 
    auto   60.0000  150.0000     0.000 -298.536E+00 
    auto  -90.0000  225.0000     0.000 -304.029E+00 
    auto  -15.0000  225.0000     0.000 -307.989E+00 
    auto   60.0000  225.0000     0.000 -298.536E+00 
    auto  -90.0000  300.0000     0.000 -304.029E+00 
    auto  -15.0000  300.0000     0.000 -307.989E+00 
    auto   60.0000  300.0000     0.000 -298.536E+00 
  joze_a   52.0000   21.0000   110.000 -297.771E+00 
  balt_a   57.0000   21.0000     0.000 -298.536E+00 
########################################################################
#                                                        polygon_check #
#                                                     v1.0-48-g1971ecb #
########################################################################
#                              compiler: ifort (IFORT) 14.0.3 20140422 #
#                           compiled on lidka_virt 2014-09-07 19:44:30 #
#                                  FFLAGS = -fpic -O0 -xHost -warn all #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################
Program started: 2014-09-07 19:44:38 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
      polygon_check -Sj,b,o,:57:22 -P ../../../polygon/baltyk.poly -V -H
#-----------------------------------------------------------------------
Command parsing:
  -S { -Sj,b,o,:57:22 }
    j
    b
      added site(s): 1
    o
      added site(s): 1
    :57:22
      added site(s): 1
  -P { baltyk.poly }
    polygon file: ../../../polygon/baltyk.poly
       name: ../../../polygon/baltyk.poly
       number of polygons: 1
       use [true/false]: T
       number of coords: 17
  -V { -V }
    verbose mode
  -H { -H }
warning: not accepted switch -H 
#-----------------------------------------------------------------------
    Processing: 4 site(s)
      Name lat [deg] lon [deg]     H [m]    Hp [m]    H* [m]   Hrsp[m]
    joze_a   52.0000   21.0000  110.0000        --        --        -- 
    balt_a   57.0000   21.0000    0.0000        --        --        -- 
    mari_a   11.3170  142.2500-9910.0000        --        --        -- 
             57.0000   22.0000    0.0000        --        --        -- 
#-----------------------------------------------------------------------
  joze_a   21.00000  52.00000   0
  balt_a   21.00000  57.00000   1
  mari_a  142.25000  11.31700   0
           22.00000  57.00000   1
