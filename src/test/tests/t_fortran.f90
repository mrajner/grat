program testing_fortran_procedures
  use mr_utilities
  use mod_cmdline
  use mod_aggf

  integer :: iunit
  character(30) :: filename

  print *, "output of: ./test"

  call uniq_name_unit(prefix="non_existing_file_",filename=filename,digits=5,unit=iunit,ifcreate=.false.)
  print *, filename
  call uniq_name_unit(filename=filename,start=30,unit=iunit, suffix="_suff",ifcreate=.false.)
  print *, filename

  print *, method3dnames


  call test_standard_atmosphere("full")
  call test_standard_atmosphere("standard")
  call test_standard_atmosphere("berg")
  call test_standard_atmosphere("simple")

  write (*,10) aggf(psi=0.1_dp, method="simple", dz=2._dp)

  write(*,10) aggf(psi = 1e-6_dp, method = "full")
  write(*,10) aggf(psi = 1e-6_dp, method = "standard")


10 FORMAT (f14.7)
contains

subroutine test_standard_atmosphere (method)
  use mr_atmosphere, only: standard_pressure
  character(*),optional,intent(in)  :: method
  write(*,10) standard_pressure(         &
    height                   = 6000._dp, &
    use_standard_temperature = .true. ,  &
    temperature              = 480._dp,  &
    method                   = method    &
    )
  write(*,10) standard_pressure(         &
    height                   = 6000._dp, &
    use_standard_temperature = .false. , &
    temperature              = 480._dp,  &
    method                   = method    &
    )
  
10 FORMAT (f14.7)
end subroutine
end program
