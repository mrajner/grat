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
value_check -F 1013.25 @ SP:@scale=100, 15 @ T :@offset=273.15 -S r, equ
ator : 0 : 0 : 0, south pole : -90 : 0 : 0, north_pole : 90 : 0 : 0, out
 : 0 : 1361 : 0, out : -100 : -1361 : 0 out : 100 : 161 : 0 -D 201201012
4@~ , 2012010122@~ , 2012010144@~ , 20120311@~ -o t1.dat6.c -V t1.dat7.c
                                                                        
#-----------------------------------------------------------------------
Command parsing:
  -F { -F1013.25@SP:@scale=100,15@T:@offset=273.15 }
    1013.25@SP:@scale=100
      Surface pressure (SP)
      constant value was set:       1.013E+03
      var modifier: scale 100  101.325E+03
      constant value was re-set:  101.325E+03
    15@T:@offset=273.15
      Surface temperature (T)
      constant value was set:      15.000E+00
      var modifier: offset 273.15  288.150E+00
      constant value was re-set:  288.150E+00
  -S { -Sr,equator:0:0:0,southpole:-90:0:0,north_pole:90:0:0,out:0:1361:0,out:-100:-1361:0out:100:161:0 }
    r
    equator:0:0:0
      added site(s): 1
    southpole:-90:0:0
      added site(s): 1
    north_pole:90:0:0
      added site(s): 1
    out:0:1361:0
      added site(s): 1
    out:-100:-1361:0out:100:161:0
      added site(s): 1
  -D { -D2012010124@~,2012010122@~,2012010144@~,20120311@~ }
    2012010124@~
      start date: 2012 01 01 24 00 00
    2012010122@~
      start date: 2012 01 01 22 00 00
      added date(s): 1
    2012010144@~
      start date: 2012 01 01 44 00 00
      added date(s): 1
    20120311@~
      start date: 2012 03 11 00 00 00
      added date(s): 1
      dates total: 4
  -o { -ot1.dat6.c }
    output file was set: t1.dat6.c
  -V { -Vt1.dat7.c }
    verbose mode
    the log file was set t1.dat7.c
#-----------------------------------------------------------------------
    Processing: 6 site(s)
      Name lat [deg] lon [deg]     H [m]    Hp [m]    H* [m]   Hrsp[m]
    rysy_a   49.1794   20.0883 2499.0000        --        --        --
   equator    0.0000    0.0000    0.0000        --        --        --
 southpole  -90.0000    0.0000    0.0000        --        --        --
north_pole   90.0000    0.0000    0.0000        --        --        --
       out    0.0000  281.0000    0.0000        --        --        --
       out  -90.0000   79.0000    0.0000        --        --        --
#-----------------------------------------------------------------------
Execution time:   0.0s (proc time:   0.0 |% 17.7)
#-----------------------------------------------------------------------
