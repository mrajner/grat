########################################################################
#                                                          value_check #
#                                                      v1.2-9-g2dcf143 #
########################################################################
#                              compiler: ifort (IFORT) 14.0.2 20140120 #
#                                  FFLAGS = -fpic -O0 -xHost -warn all #
#                               compiled on lenovo 2015-05-15 08:41:48 #
#                                                                      #
#                                 Copyright 2013-2015 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                              License: GPLv3 or later #
########################################################################
Program started: 2015-05-15 09:01:58 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
value_check -! -F ../data/test_data.nc @SP : sp, ../data/test_data.nc @T
: t -S g:50 -D 2011123118 : 2 @M : 12 @H -o t1.dat.out5.s -V t1.dat.log5
                                                                      .s
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
  -o { -ot1.dat.out5.s }
    output file was set: t1.dat.out5.s
  -V { -Vt1.dat.log5.s }
    verbose mode
    the log file was set t1.dat.log5.s
#-----------------------------------------------------------------------
    Processing: 32 site(s)
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
