program unit_tests
  use mod_spherical
  use mod_constants
  real(dp) :: a, b, c(2)
  print *, 'testing'
  call spher_trig(real(1,dp),2._dp, 1._dp,1._dp,a,b)
  print*, a, b
  call spher_trig(real(0,dp),0._dp, 1._dp,0._dp,a,b)
  print*, a, b


  print*, tmp()
  c = tmp()
  print*, c(1), c

contains
pure function tmp()
  real(dp), dimension(2) :: tmp
  tmp(1) = 1._dp
  tmp(2) = 2._dp
end function

end program

