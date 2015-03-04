program testing_fortran_procedures
  use mod_utilities
  use mod_cmdline
  use mod_aggf
  integer :: iunit
  character(30) :: filename, my_method

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

  print *, aggf(psi=0.1_dp, method="simple", dz=2._dp)

  print *, aggf(psi=1e-6_dp, method="full")
  print *, aggf(psi=1e-6_dp, method="standard")

contains

subroutine test_standard_atmosphere (method)
  use mod_atmosphere, only: standard_pressure
  character(*),optional,intent(in)  :: method
  print *, standard_pressure( &
    height= 6000._dp,  &
    use_standard_temperature=.true. ,&
    temperature=480._dp, &
    method=method &
    )
  print *, standard_pressure( &
    height= 6000._dp,  &
    use_standard_temperature=.false. ,&
    temperature=480._dp, &
    method=method &
    )
  
end subroutine
end program
