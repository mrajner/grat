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
5.2012.nc @T: air -S r, equator : 0 : 0 : 0, south pole : -90 : 0 : 0, n
orth_pole : 90 : 0 : 0, out : 0 : 1361 : 0, out : -100 : -1361 : 0 out :
 100 : 161 : 0 -D 2012010124@~ , 2012010122@~ , 2012010144@~ , 20120311@
                                           ~ -o t1.dat15.r -V t1.dat16.r
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
      start date: 2012 01 02 00 00 00
    2012010122@~
      start date: 2012 01 02 00 00 00
      added date(s): 1
    2012010144@~
      start date: 2012 01 02 18 00 00
      added date(s): 1
    20120311@~
      start date: 2012 03 11 00 00 00
      added date(s): 1
      dates total: 4
  -o { -ot1.dat15.r }
    output file was set: t1.dat15.r
  -V { -Vt1.dat16.r }
    verbose mode
    the log file was set t1.dat16.r
#-----------------------------------------------------------------------
    Processing: 6 site(s)
      Name lat [deg] lon [deg]     H [m]    Hp [m]    H* [m]   Hrsp[m]
    rysy_a   49.1794   20.0883 2499.0000        --        --        -- 
   equator    0.0000    0.0000    0.0000        --        --        -- 
 southpole  -90.0000    0.0000    0.0000        --        --        -- 
north_pole   90.0000    0.0000    0.0000        --        --        -- 
       out    0.0000  281.0000    0.0000        --        --        -- 
       out  -90.0000   79.0000    0.0000        --        --        -- 
#-----------------------------------------------------------------------
Execution time:   0.0s (proc time:   0.0 |%100.0)
#-----------------------------------------------------------------------
