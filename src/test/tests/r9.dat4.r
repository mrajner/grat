Program started: 2015-05-15 15:49:00 (+02h UTC)
#-----------------------------------------------------------------------
Command invoked:
grat -! -M 1,2,3 -G rajner@GN , merriam @GE -F ../data/pres.sfc.2012.nc 
@SP :pres, ../data/hgt.2012.nc @GP :hgt, ../data/land.nc @LS :land, ../d
ata/hgt.sfc.nc @HP :hgt, ../data/hgt.sfc.nc @H :hgt, ../data/air.2012.nc
 @VT :air , ../data/air.sig995.2012.nc @T :air, ../data/shum.2012.nc @VS
     H :shum, 101300 @ RSP, 0 @HRSP -U -D m:m:210@D -Sj -BI -H -I -10@DD
#-----------------------------------------------------------------------
Command parsing:
  -! { -! }
    all model as huge
  -M { -M1,2,3 }
    method refinment for near area using 3D point     0.100    
  -G { -Grajner@GN,merriam@GE }
  -F { -F../data/pres.sfc.2012.nc@SP:pres,../data/hgt.2012.nc@GP:hgt,../data/land.nc@LS:land,../data/hgt.sfc.nc@HP:hgt,../data/hgt.sfc.nc@H:hgt,../data/air.2012.nc@VT:air,../data/air.sig995.2012.nc@T:air,../data/shum.2012.nc@VSH:shum,101300@RSP,0@HRSP }
      Reference surface pressure (RSP)
      constant value was set:     101.300E+03
      Height of reference surface pressure (HRSP)
      constant value was set:       0.000E+00
  -U { -U }
  -D { -Dm:m:210@D }
  -S { -Sj }
  -B { -BI }
  -H { -H }
  -I { -I-10@DD:5500@HS }
    Range: 1
      DB:   0.00|DE: 180.000|I:n|DD:**|DS:   0.00|HB:     0.0|HE: 60000.0|HS:5500.00|
      HSP:F|3D:  10.00|3D:point    |3:@DE   0.10|3::refF|
  -o { -ot9.dat3.r:c }
  -V { -Vt9.dat4.r:sparse }
#-----------------------------------------------------------------------
Execution time:   2.5s (proc time:   2.5 |% 99.9)
