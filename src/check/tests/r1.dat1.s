########################################################################
#                                                          value_check #
#                                                                 beta #
#                                                   ifort 14(20131008) #
#                                   compiled on pw 2014-07-03 09:33:08 #
#                                 FFLAGS = -fpic -O2 -xHost -warn all  #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################
Program started: 2014-07-03 10:09:52 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
value_check -F ../data/test_data.nc @SP : sp, ../data/test_data.nc @T: t
 -S jozefoslaw : 52.0 : 21.0 : 110, tmp : -33.0 : 1.0 : -110, -D 2012010
                                 1 : m : 24 @H -o t1.dat0.s -V t1.dat1.s
#-----------------------------------------------------------------------
Command parsing:
  -F { test_data.nc@T:t }
    ../data/test_data.nc@SP:sp
      Surface pressure (SP) sp lon lat level time 
      Opening file: test_data.nc , huge [T/F]: F
        Getting dim: lon ..     ok
        Getting dim: lat ..     ok
        Getting dim: level ..     ok
        Getting dim: time ..     ok
        Converting time:  hours since 2012-1-1 00:00:0.0
    ../data/test_data.nc@T:t
      Surface temperature (T) t lon lat level time 
      Opening file: test_data.nc , huge [T/F]: F
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
  -o { -ot1.dat0.s }
    output file was set: t1.dat0.s
  -V { -Vt1.dat1.s }
    verbose mode
    the log file was set t1.dat1.s
#-----------------------------------------------------------------------
    Processing: 2 site(s)
      Name lat [deg] lon [deg]     H [m]    Hp [m]    H* [m]   Hrsp[m]
jozefoslaw   52.0000   21.0000  110.0000        --        --        -- 
       tmp  -33.0000    1.0000 -110.0000        --        --        -- 
#-----------------------------------------------------------------------
Execution time:   0.0s (proc time:   0.0 |% 87.0)
#-----------------------------------------------------------------------
