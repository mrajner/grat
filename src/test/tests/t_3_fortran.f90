program unit_tests
  use mod_utilities, only: count_separator
 
  print*, "count_separator", count_separator("dfdfsffssfsff,fdfdfdf,fdfsdf,f,dsf,sdfsdfsdfs,,sdffsdf")
  print*, "count_separator", count_separator("-Fsp@SP:1:2:3:4:5:6")
  print*, "count_separator", count_separator("-Fsp@SP:1:2:3:4:5:6",":")
  print*, "count_separator", count_separator("-Fsp@SP:x:y:z:",":")

end program
