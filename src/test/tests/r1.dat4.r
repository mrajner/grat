########################################################################
#                                                          value_check #
#                                                     v1.0-23-gf95f55a #
########################################################################
#                                                           compiler:  #
#                                                    compiled on grat  #
#                                                            FFLAGS =  #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################
Program started: 2014-09-03 12:39:43 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
value_check -F ../data/pres.sfc.2012.nc @SP :pres, ../data/air.sig995.20
12.nc @T :air -S g:50 -D 2011123118 : 2 @M : 12 @H -o t1.dat3.r -V t1.da
                                                                    t4.r
#-----------------------------------------------------------------------
Command parsing:
  -F { air.sig995.2012.nc@T:air }
    ../data/pres.sfc.2012.nc@SP:pres
    ../data/air.sig995.2012.nc@T:air
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
Execution time:   0.1s (proc time:   0.1 |% 99.2)
#-----------------------------------------------------------------------
