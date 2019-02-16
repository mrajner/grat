program unit_tests
  use mr_utilities
  use mr_constants, only: density,earth
  implicit none
  print *, 'testing'

  print * ,"1m of water =", mmwater2pascal(1000._dp)/100 , "hPa"
  print * ,"10m of water =", mmwater2pascal(10000._dp)/100 , "hPa"
  print * , mmwater2pascal(101325._dp,inverted=.true.)/1000

  print*, density%water
  print*, earth%gravity%mean

end program

