program test_aggf
  use mod_constants, only: dp
  use mod_aggf, only: aggf

  ! write (*,10) , aggf(0._dp, method="full")
  ! write (*,10) , aggf(1e-9_dp, method="full")
  write (*,10) , aggf(1e-6_dp, method="full")
  ! write (*,10) , aggf(10._dp, method="full")

  ! write (*,10) , aggf(10._dp, method="berg")
  ! write (*,10) , aggf(10._dp, method="simple")

  ! write (*,10) , aggf(10._dp, method="full", dz=20._dp)

  ! write (*,10) , aggf(10._dp, method="full", zmin=1000._dp, zmax=2000._dp, dz=20._dp)
  ! write (*,10) , aggf(10._dp, method="full", zmin=1000._dp, zmax=2000._dp, dz=20._dp, first_derivative_h=.true.)

10 FORMAT  (f19.9)
end program
