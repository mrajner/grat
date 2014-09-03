Program started: 2014-09-03 12:44:18 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
grat -! -M 1,2,3 -G rajner@GN , merriam @GE -F ../data/pres.sfc.2012.nc 
@SP :pres, ../data/hgt.2012.nc @GP :hgt , ../data/land.nc @LS :land , ..
/data/hgt.sfc.nc @HP :hgt , ../data/hgt.sfc.nc @H :hgt , ../data/air.201
2.nc @VT :air , ../data/air.sig995.2012.nc @T :air , ../data/shum.2012.n
     c @VSH :shum , 101300 @ RSP , 0 @HRSP -U -D m:m:210@D -Sj -BI -H -I
#-----------------------------------------------------------------------
Command parsing:
  -! { -! }
    all model as huge
  -M { -M1,2,3 }
    method refinment for near area using 3D point      0.1000000    
  -G { -Grajner@GN,merriam@GE }
  -F { shum.2012.nc@VSH:shum,101300@RSP,0@HRSP }
      Reference surface pressure (RSP)
      constant value was set:     101.300E+03
      constant value was re-set:  101.300E+03
      Height of reference surface pressure (HRSP)
      constant value was set:       0.000E+00
      constant value was re-set:    0.000E+00
  -U { -U }
  -D { -Dm:m:210@D }
