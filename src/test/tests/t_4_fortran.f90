program unit_tests
  use mod_spherical
  use mod_constants
  real(dp) :: a, b

  print *, 'testing'

  call spher_trig(real(1,dp),2._dp, 1._dp,1._dp,a,b)
  print*, a, b
  call spher_trig(real(0,dp),-1._dp, 1._dp,0._dp,a,b, .false.)
  print*, a, b
  call spher_trig(real(0,dp),-1._dp, 1._dp,0._dp,a,b, .true.)
  print*, a, b


end program

