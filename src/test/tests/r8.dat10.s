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
Program started: 2015-05-15 13:12:26 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
value_check -F 10 , 20:@gh2h , 30:@ scale=10 @ invscale=1000 , 40@name ,
 10:@gp2h , 30 @H -S a:10:20:100 , : 10 , c ,g :100 -H -o t8.dat0.s -V t
                                                               8.dat10.s
#-----------------------------------------------------------------------
Command parsing:
  -F { -F10,20:@gh2h,30:@scale=10@invscale=1000,40@name,10:@gp2h,30@H }
    10
      unknown (NN)
      constant value was set:      10.000E+00
    20:@gh2h
      unknown (NN)
      constant value was set:      20.000E+00
      var modifier: gh2h    20.000E+00
      constant value was re-set:   20.000E+00
    30:@scale=10@invscale=1000
      unknown (NN)
      constant value was set:      30.000E+00
      var modifier: scale 10  300.000E+00
      var modifier: invscale 1000  300.000E-03
      constant value was re-set:  300.000E-03
    40@name
      unknown (name)
      constant value was set:      40.000E+00
    10:@gp2h
      unknown (NN)
      constant value was set:      10.000E+00
      var modifier: gp2h     1.020E+00
      constant value was re-set:    1.020E+00
    30@H
      Surface height (H)
      constant value was set:      30.000E+00
  -S { -Sa:10:20:100,:10,c,g:100 }
    a:10:20:100
    :10
    c
    g:100
      added site(s): 8
  -H { -H }
      header
  -o { -ot8.dat0.s }
    output file was set: t8.dat0.s
  -V { -Vt8.dat10.s }
    verbose mode
    the log file was set t8.dat10.s
#-----------------------------------------------------------------------
    Processing: 9 site(s)
      Name lat [deg] lon [deg]     H [m]    Hp [m]    H* [m]   Hrsp[m]
         a   10.0000   20.0000  100.0000        --   30.0000        --
      auto  -90.0000    0.0000    0.0000        --   30.0000        --
      auto   10.0000    0.0000    0.0000        --   30.0000        --
      auto  -90.0000  100.0000    0.0000        --   30.0000        --
      auto   10.0000  100.0000    0.0000        --   30.0000        --
      auto  -90.0000  200.0000    0.0000        --   30.0000        --
      auto   10.0000  200.0000    0.0000        --   30.0000        --
      auto  -90.0000  300.0000    0.0000        --   30.0000        --
      auto   10.0000  300.0000    0.0000        --   30.0000        --
#-----------------------------------------------------------------------
Execution time:   0.0s (proc time:   0.0 |%  Inf)
#-----------------------------------------------------------------------
