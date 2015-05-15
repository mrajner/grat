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
: t -S jozefoslaw : 52.0 : 21.0 : 110, tmp : -33.0 : 1.0 : -110, -D 2012
                      0101 : m : 24 @H -o t1.dat.out4.s -V t1.dat.log4.s
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
  -S { -Sjozefoslaw:52.0:21.0:110,tmp:-33.0:1.0:-110, }
    jozefoslaw:52.0:21.0:110
    tmp:-33.0:1.0:-110
      added site(s): 1
    
  -D { -D20120101:m:24@H }
    20120101:m:24@H
      start date: 2012 01 01 00 00 00
      stop  date: 2012 01 03 06 00 00
      interval: 24.0H
      dates total: 3
  -o { -ot1.dat.out4.s }
    output file was set: t1.dat.out4.s
  -V { -Vt1.dat.log4.s }
    verbose mode
    the log file was set t1.dat.log4.s
#-----------------------------------------------------------------------
    Processing: 2 site(s)
      Name lat [deg] lon [deg]     H [m]    Hp [m]    H* [m]   Hrsp[m]
jozefoslaw   52.0000   21.0000  110.0000        --        --        --
       tmp  -33.0000    1.0000 -110.0000        --        --        --
#-----------------------------------------------------------------------
Execution time:   0.0s (proc time:   0.0 |% 99.6)
#-----------------------------------------------------------------------
