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
Program started: 2014-07-04 10:34:51 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
value_check -F ../data/pres.sfc.2012.nc @SP : pres, ../data/air.sig995.2
012.nc @T: air -S g:50 -D 2011123118 : 2 @M : 12 @H -o t1.dat3.r -V t1.d
                                                                   at4.r
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
        Converting time:  hours since 1-1-1 00:00:0.0
  -S { -Sg:50 }
    g:50
  -D { -D2011123118:2@M:12@H }
    2011123118:2@M:12@H
      start date: 2011 12 31 18 00 00
      stop  date: 2012 02 31 18 00 00
      interval: 12.0H
      dates total: 125
  -o { -ot1.dat3.r }
    output file was set: t1.dat3.r
  -V { -Vt1.dat4.r }
    verbose mode
    the log file was set t1.dat4.r
#-----------------------------------------------------------------------
    Processing: 32 site(s)
#-----------------------------------------------------------------------
Execution time:   0.2s (proc time:   0.1 |% 56.5)
#-----------------------------------------------------------------------
