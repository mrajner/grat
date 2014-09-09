########################################################################
#                                                          value_check #
#                                                      v1.1-8-g6bc8c09 #
########################################################################
#                              compiler: ifort (IFORT) 14.0.2 20140120 #
#                               compiled on lenovo 2014-09-09 15:36:36 #
#                                                    FFLAGS = (FFLAGS) #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################
Program started: 2014-09-09 15:37:47 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
value_check -Sg:75,j,b -F1013.25 @ SP:@scale=100 -P ../../../polygon/bal
                                                          tyk.poly -V -H
#-----------------------------------------------------------------------
Command parsing:
  -S { -Sg:75,j,b }
    g:75
    j
      added site(s): 1
    b
      added site(s): 1
  -F { -F1013.25@SP:@scale=100 }
    1013.25@SP:@scale=100
      Surface pressure (SP)
      constant value was set:       1.013E+03
      var modifier: scale 100  101.325E+03
      constant value was re-set:  101.325E+03
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
  balt_a   57.0000   21.0000  101.325E+03
Execution time:   0.0s (proc time:   0.0 |% 13.6)
#-----------------------------------------------------------------------
########################################################################
#                                                                 grat #
#                                                      v1.1-8-g6bc8c09 #
########################################################################
#                              compiler: ifort (IFORT) 14.0.2 20140120 #
#                               compiled on lenovo 2014-09-09 15:36:36 #
#                                                    FFLAGS = (FFLAGS) #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################
Program started: 2014-09-09 15:37:47 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
grat -Sg:75,j,b -F1013.25 @ SP:@scale=100 -P ../../../polygon/baltyk.pol
                                                                 y -V -H
#-----------------------------------------------------------------------
Command parsing:
  -S { -Sg:75,j,b }
    g:75
    j
      added site(s): 1
    b
      added site(s): 1
  -F { -F1013.25@SP:@scale=100 }
    1013.25@SP:@scale=100
      Surface pressure (SP)
      constant value was set:       1.013E+03
      var modifier: scale 100  101.325E+03
      constant value was re-set:  101.325E+03
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
    auto  -90.0000    0.0000     0.000 -303.975E+00 
    auto  -15.0000    0.0000     0.000 -303.975E+00 
    auto   60.0000    0.0000     0.000 -303.975E+00 
    auto  -90.0000   75.0000     0.000 -303.975E+00 
    auto  -15.0000   75.0000     0.000 -303.975E+00 
    auto   60.0000   75.0000     0.000 -303.975E+00 
    auto  -90.0000  150.0000     0.000 -303.975E+00 
    auto  -15.0000  150.0000     0.000 -303.975E+00 
    auto   60.0000  150.0000     0.000 -303.975E+00 
    auto  -90.0000  225.0000     0.000 -303.975E+00 
    auto  -15.0000  225.0000     0.000 -303.975E+00 
    auto   60.0000  225.0000     0.000 -303.975E+00 
    auto  -90.0000  300.0000     0.000 -303.975E+00 
    auto  -15.0000  300.0000     0.000 -303.975E+00 
    auto   60.0000  300.0000     0.000 -303.975E+00 
  joze_a   52.0000   21.0000   110.000 -303.975E+00 
  balt_a   57.0000   21.0000     0.000 -303.975E+00 
Execution time:   0.0s (proc time:   0.0 |%125.0)
########################################################################
#                                                        polygon_check #
#                                                      v1.1-8-g6bc8c09 #
########################################################################
#                              compiler: ifort (IFORT) 14.0.2 20140120 #
#                               compiled on lenovo 2014-09-09 15:36:36 #
#                                                    FFLAGS = (FFLAGS) #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################
Program started: 2014-09-09 15:37:47 (+02h UTC)
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
