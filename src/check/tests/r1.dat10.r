########################################################################
#                                                          value_check #
#                                                                 beta #
#                                                   ifort 14(20140120) #
#                               compiled on lenovo 2014-07-04 10:09:21 #
#                                 FFLAGS = -fpic -O2 -xHost -warn all  #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################
Program started: 2014-07-04 10:34:52 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
value_check -! -F ../data/pres.sfc.2012.nc @SP : pres, ../data/air.sig99
5.2012.nc @T: air -S jozefoslaw : 52.0 : 21.0 : 110, tmp : -33.0 : 1.0 :
                -110, -D 20120101 : m : 24 @H -o t1.dat9.r -V t1.dat10.r
#-----------------------------------------------------------------------
Command parsing:
  -! { -! }
    all model as huge
  -F { air.sig995.2012.nc@T:air }
    ../data/pres.sfc.2012.nc@SP:pres
      Surface pressure (SP) pres lon lat level time 
      Opening file: pres.sfc.2012.nc , huge [T/F]: T
        Getting dim: lon ..     ok
        Getting dim: lat ..     ok
        Getting dim: level ..  level not found, allocating (1)...
          lat, lon, time, pres,
        Getting dim: time ..     ok
        Converting time:  hours since 1800-1-1 00:00:0.0
    ../data/air.sig995.2012.nc@T:air
      Surface temperature (T) air lon lat level time 
      Opening file: air.sig995.2012.nc , huge [T/F]: T
        Getting dim: lon ..     ok
        Getting dim: lat ..     ok
        Getting dim: level ..  level not found, allocating (1)...
          lat, lon, time, air,
        Getting dim: time ..     ok
        Converting time:  hours since 1-1-1 00:00:0.0
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
  -o { -ot1.dat9.r }
    output file was set: t1.dat9.r
  -V { -Vt1.dat10.r }
    verbose mode
    the log file was set t1.dat10.r
#-----------------------------------------------------------------------
    Processing: 2 site(s)
      Name lat [deg] lon [deg]     H [m]    Hp [m]    H* [m]   Hrsp[m]
jozefoslaw   52.0000   21.0000  110.0000        --        --        -- 
       tmp  -33.0000    1.0000 -110.0000        --        --        -- 
#-----------------------------------------------------------------------
Execution time:   0.0s (proc time:   0.0 |%100.5)
#-----------------------------------------------------------------------
