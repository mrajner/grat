program test_aggf
  use mr_constants, only: dp
  use mod_aggf, only: aggf
  use mod_atmosphere , only: standard_pressure

  write(*, 10) aggf(psi = 1e-0_dp, method="full")
  write(*, 10) aggf(0._dp, zmax = 10._dp,    method="full")+epsilon(0._dp)
  write(*, 10) aggf(1e-9_dp, zmin = 40000._dp,  method="full")
  write(*, 10) aggf(1e-11_dp, zmin = 3000._dp, zmax = 2000.0_dp, method="full")
  write (*,10) aggf(psi = 1e-6_dp, dz = 2000._dp, method="standard", fels_type="US1976")
  write (*,10) aggf(psi = 1e-6_dp, dz = 100000.0_dp, method="simple")

  write(*,10) aggf(psi = 1e-6_dp, dz = 0.3_dp, method="full")
  write(*,10) aggf(psi = 1e-6_dp, dz = 0.3_dp, method="standard")
  write(*,10) aggf(psi = 1e-6_dp, dz = 0.3_dp, method="berg")
  write(*,10) aggf(psi = 1e-6_dp, dz = 0.3_dp, method="simple")
  write(*,10) aggf(10._dp, method="full")

  write(*,10) aggf(10._dp, method="berg")
  write(*,10) aggf(10._dp, method="simple")

  write (*,10) aggf(10._dp, method="full", dz=2000._dp)

  write (*,10) aggf(10._dp, method="full", zmin=1000._dp, zmax=2000._dp, dz=20._dp)
  write (*,10) aggf(10._dp, method="full", zmin=1000._dp, zmax=2000._dp, dz=20._dp, first_derivative_h=.true.)
  write (*,10) aggf(0._dp)+epsilon(0._dp)

10 FORMAT  (f25.10)
end program
