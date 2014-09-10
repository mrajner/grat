program test_aggf
  use mod_constants, only: dp
  use mod_aggf

  print * , aggf(0._dp, method="full")
  print * , aggf(1e-9_dp, method="full")
  print * , aggf(1e-6_dp, method="full")

  print * , aggf(10._dp, method="full")
  print * , aggf(10._dp, method="berg")
  print * , aggf(10._dp, method="simple")

  print * , aggf(10._dp, method="full", dz=20._dp)
  print * , aggf(10._dp, method="full", zmin=1000._dp, zmax=2000._dp, dz=20._dp)
  print * , aggf(10._dp, method="full", zmin=1000._dp, zmax=2000._dp, dz=20._dp, first_derivative_h=.true.)
end program
