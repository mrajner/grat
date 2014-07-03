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
Program started: 2014-07-03 10:09:55 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
value_check -! -F 1013.25 @ SP:@scale=100, 15 @ T :@offset=273.15 -S r, 
equator : 0 : 0 : 0, south pole : -90 : 0 : 0, north_pole : 90 : 0 : 0, 
out : 0 : 1361 : 0, out : -100 : -1361 : 0 out : 100 : 161 : 0 -D 201201
0124@~ , 2012010122@~ , 2012010144@~ , 20120311@~ -o t1.dat15.c -V t1.da
                                                                   t16.c
#-----------------------------------------------------------------------
Command parsing:
  -! { -! }
    all model as huge
  -F { -F1013.25@SP:@scale=100,15@T:@offset=273.15 }
    1013.25@SP:@scale=100
      Surface pressure (SP)
      constant value was set:       1.013E+03
      var modifier: scale 100  101.325E+03
      constant value was re-set:  101.325E+03
    15@T:@offset=273.15
      Surface temperature (T)
      constant value was set:      15.000E+00
      var modifier: offset 273.15  288.150E+00
      constant value was re-set:  288.150E+00
  -S { -Sr,equator:0:0:0,southpole:-90:0:0,north_pole:90:0:0,out:0:1361:0,out:-100:-1361:0out:100:161:0 }
    r
    equator:0:0:0
      added site(s): 1
    southpole:-90:0:0
      added site(s): 1
    north_pole:90:0:0
      added site(s): 1
    out:0:1361:0
      added site(s): 1
    out:-100:-1361:0out:100:161:0
      added site(s): 1
  -D { -D2012010124@~,2012010122@~,2012010144@~,20120311@~ }
    2012010124@~
