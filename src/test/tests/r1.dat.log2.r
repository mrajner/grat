########################################################################
#                                                          value_check #
#                                                    v1.1-119-g918951e #
########################################################################
#                              compiler: ifort (IFORT) 14.0.2 20140120 #
#                                  FFLAGS = -fpic -O0 -xHost -warn all #
#                               compiled on lenovo 2015-03-06 08:19:15 #
#                                                                      #
#                                 Copyright 2013-2015 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                              License: GPLv3 or later #
########################################################################
Program started: 2015-03-06 08:19:56 (+01h UTC)
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
