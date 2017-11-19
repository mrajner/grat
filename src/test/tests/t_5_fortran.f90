program unit_tests
  use mod_utilities
  use mod_constants, only: density,earth
  implicit none
  print *, 'testing'

  print * , mmwater2pascal(1000._dp)
  print * , mmwater2pascal(101325._dp,inverted=.true.)

  print*, density%water
  print*, earth%gravity%mean

end program

