########################################################################
#                                                          value_check #
#                                                     v1.2-11-g168849e #
########################################################################
#                              compiler: ifort (IFORT) 14.0.2 20140120 #
#                                  FFLAGS = -fpic -O0 -xHost -warn all #
#                               compiled on lenovo 2015-05-15 09:25:38 #
#                                                                      #
#                                 Copyright 2013-2015 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                              License: GPLv3 or later #
########################################################################
Program started: 2015-05-15 09:48:51 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
value_check -F ../data/pres.sfc.2012.nc @SP :pres, ../data/air.sig995.20
12.nc @T :air -S jozefoslaw : 52.0 : 21.0 : 110, tmp : -33.0 : 1.0 : -11
            0, -D 20120101 : m : 24 @H -o t1.dat.out1.r -V t1.dat.log1.r
#-----------------------------------------------------------------------
Command parsing:
  -F { air.sig995.2012.nc@T:air }
    ../data/pres.sfc.2012.nc@SP:pres
      Surface pressure (SP) pres lon lat level time 
      Opening file: pres.sfc.2012.nc , huge [T/F]: F
        Getting dim: lon ..     ok
        Getting dim: lat ..     ok
        Getting dim: level ..  level not found, allocating (1)...
          lat, lon, time, pres,
        Getting dim: time ..     ok
        Converting time:  hours since 1800-1-1 00:00:0.0
    ../data/air.sig995.2012.nc@T:air
      Surface temperature (T) air lon lat level time 
      Opening file: air.sig995.2012.nc , huge [T/F]: F
        Getting dim: lon ..     ok
        Getting dim: lat ..     ok
        Getting dim: level ..  level not found, allocating (1)...
          lat, lon, time, air,
        Getting dim: time ..     ok
        Converting time:  hours since 1800-01-01 00:00:0.0
  -S { -Sjozefoslaw:52.0:21.0:110,tmp:-33.0:1.0:-110, }
    jozefoslaw:52.0:21.0:110
    tmp:-33.0:1.0:-110
      added site(s): 1
    
  -D { -D20120101:m:24@H }
    20120101:m:24@H
      start date: 2012 01 01 00 00 00
      stop  date: 2012 12 31 18 00 00
      interval: 24.0H
      dates total: 366
  -o { -ot1.dat.out1.r }
    output file was set: t1.dat.out1.r
  -V { -Vt1.dat.log1.r }
    verbose mode
    the log file was set t1.dat.log1.r
#-----------------------------------------------------------------------
    Processing: 2 site(s)
      Name lat [deg] lon [deg]     H [m]    Hp [m]    H* [m]   Hrsp[m]
jozefoslaw   52.0000   21.0000  110.0000        --        --        --
       tmp  -33.0000    1.0000 -110.0000        --        --        --
#-----------------------------------------------------------------------
Execution time:   0.6s (proc time:   0.6 |% 99.7)
#-----------------------------------------------------------------------
