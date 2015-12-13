program unit_tests
  use mod_utilities
 
  print*, "count_separator", count_separator("dfdfsffssfsff,fdfdfdf,fdfsdf,f,dsf,sdfsdfsdfs,,sdffsdf")
  print*, "count_separator", count_separator("-Fsp@SP:1:2:3:4:5:6")
  print*, "count_separator", count_separator("-Fsp@SP:1:2:3:4:5:6",":")
  print*, "count_separator", count_separator("-Fsp@SP:x:y:z:",":")
 print*,  count_separator("-D2010:5@D:22@H",":")
 print*, count_separator2("2010:5@D:22@H",":")
 print*, count_separator2("2010:5@D:22@H",":")
end program
