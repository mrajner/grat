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
Program started: 2015-05-15 09:48:52 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
value_check -F ../data/pres.sfc.2012.nc @SP :pres, ../data/air.sig995.20
12.nc @T :air -S g:50 -D 2011123118 : 2 @M : 12 @H -o t1.dat.out2.r -V t
                                                            1.dat.log2.r
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
  -S { -Sg:50 }
    g:50
  -D { -D2011123118:2@M:12@H }
    2011123118:2@M:12@H
      start date: 2011 12 31 18 00 00
      stop  date: 2012 02 31 18 00 00
      interval: 12.0H
      dates total: 125
  -o { -ot1.dat.out2.r }
    output file was set: t1.dat.out2.r
  -V { -Vt1.dat.log2.r }
    verbose mode
    the log file was set t1.dat.log2.r
#-----------------------------------------------------------------------
    Processing: 32 site(s)
#-----------------------------------------------------------------------
Execution time:   0.3s (proc time:   0.3 |% 99.7)
#-----------------------------------------------------------------------
