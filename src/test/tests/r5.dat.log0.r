########################################################################
#                                                          value_check #
#                                                     v1.2-24-g3b3db98 #
########################################################################
#         compiler: GNU Fortran (GCC) 4.8.3 20140911 (Red Hat 4.8.3-7) #
#                                                         FFLAGS = -O0 #
#                                     compiled on  2015-05-15 19:19:09 #
#                                                                      #
#                                 Copyright 2013-2015 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                              License: GPLv3 or later #
########################################################################
Program started: 2015-05-15 19:20:57 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
value_check -F ../data/air.2012.nc @VT :air -S g:20 -J 1000, 400 , 200 -
                                        o t5.dat.out0.r -V t5.dat.log0.r
#-----------------------------------------------------------------------
Command parsing:
  -F { -F../data/air.2012.nc@VT:air }
    ../data/air.2012.nc@VT:air
      unknown (VT) air lon lat level time 
      Opening file: air.2012.nc , huge [T/F]: F
        Getting dim: lon ..     ok
        Getting dim: lat ..     ok
        Getting dim: level ..     ok
        Getting dim: time ..     ok
        Converting time:  hours since 1800-1-1 00:00:0.0
  -S { -Sg:20 }
    g:20
  -J { -J1000,400,200 }
    level pressure: 1000 400 200
  -o { -ot5.dat.out0.r }
    output file was set: t5.dat.out0.r
  -V { -Vt5.dat.log0.r }
    verbose mode
    the log file was set t5.dat.log0.r
#-----------------------------------------------------------------------
    Processing: 180 site(s)
#-----------------------------------------------------------------------
Execution time:   0.0s (proc time:   0.0 |%110.1)
#-----------------------------------------------------------------------
