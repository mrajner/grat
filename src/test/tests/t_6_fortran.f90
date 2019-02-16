program test
  use mr_utilities

  print*, linspace(1._dp,5._dp,-1)
  print*, linspace(1._dp,5._dp,-0)
  print*, linspace(1._dp,5._dp,2)
  print*, linspace(1._dp,5.2_dp,5)
  print*, logspace(100._dp,5._dp,25)
  print*, logspace(1.e-5_dp,1.0e10_dp,15)
  print*, logspace(100._dp,5._dp,1)
  print*, logspace(-100._dp,5._dp,5)

end program
