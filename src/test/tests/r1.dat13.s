########################################################################
#                                                          value_check #
#                                                     v1.0-36-g2b51da8 #
########################################################################
#                                                           compiler:  #
#                                                  compiled on lenovo  #
#                                                            FFLAGS =  #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################
Program started: 2014-09-04 08:02:06 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
value_check -! -F ../data/test_data.nc @SP : sp, ../data/test_data.nc @T
    : t -S g:50 -D 2011123118 : 2 @M : 12 @H -o t1.dat12.s -V t1.dat13.s
#-----------------------------------------------------------------------
Command parsing:
  -! { -! }
    all model as huge
  -F { test_data.nc@T:t }
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
  -S { -Sg:50 }
    g:50
  -D { -D2011123118:2@M:12@H }
    2011123118:2@M:12@H
      start date: 2011 12 31 18 00 00
      stop  date: 2012 02 31 18 00 00
      interval: 12.0H
      dates total: 125
  -o { -ot1.dat12.s }
    output file was set: t1.dat12.s
  -V { -Vt1.dat13.s }
    verbose mode
    the log file was set t1.dat13.s
#-----------------------------------------------------------------------
    Processing: 32 site(s)
#-----------------------------------------------------------------------
Execution time:   0.2s (proc time:   0.2 |% 99.6)
#-----------------------------------------------------------------------
