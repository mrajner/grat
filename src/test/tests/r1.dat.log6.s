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
value_check -! -F ../data/test_data.nc @SP : sp, ../data/test_data.nc @T
: t -S r, equator : 0 : 0 : 0, south pole : -90 : 0 : 0, north_pole : 90
 : 0 : 0, out : 0 : 1361 : 0, out : -100 : -1361 : 0 out : 100 : 161 : 0
 -D 2012010124@~ , 2012010122@~ , 2012010144@~ , 20120311@~ -o t1.dat.ou
                                                   t6.s -V t1.dat.log6.s
#-----------------------------------------------------------------------
Command parsing:
  -! { -! }
    all model as huge
  -F { -F../data/test_data.nc@SP:sp,../data/test_data.nc@T:t }
    ../data/test_data.nc@SP:sp
      Surface pressure (SP) sp lon lat level time 
      Opening file: test_data.nc , huge [T/F]: T
        Getting dim: lon ..     ok
        Getting dim: lat ..     ok
        Getting dim: level ..     ok
        Getting dim: time ..     ok
        Converting time:  hours since 2012-1-1 00:00:0.0
    ../data/test_data.nc@T:t
      Surface temperature (T) t lon lat level time 
      Opening file: test_data.nc , huge [T/F]: T
        Getting dim: lon ..     ok
        Getting dim: lat ..     ok
        Getting dim: level ..     ok
        Getting dim: time ..     ok
        Converting time:  hours since 2012-1-1 00:00:0.0
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
      start date: 2012 01 02 00 00 00
    2012010122@~
      start date: 2012 01 02 00 00 00
      added date(s): 1
    2012010144@~
      start date: 2012 01 02 18 00 00
      added date(s): 1
    20120311@~
      start date: 2012 01 03 06 00 00
      added date(s): 1
      dates total: 4
  -o { -ot1.dat.out6.s }
    output file was set: t1.dat.out6.s
  -V { -Vt1.dat.log6.s }
    verbose mode
    the log file was set t1.dat.log6.s
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
Execution time:   0.0s (proc time:   0.0 |%105.8)
#-----------------------------------------------------------------------
