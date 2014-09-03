program testing_fortran_procedures
  use mod_utilities
  use mod_cmdline
  integer :: iunit
  character(30) :: filename

  print *, "output of: ./test"

  call uniq_name_unit(prefix="non_existing_file_",filename=filename,digits=5,unit=iunit,ifcreate=.false.)
  print *, filename
  call uniq_name_unit(filename=filename,start=30,unit=iunit, suffix="_suff",ifcreate=.false.)
  print *, filename

  print *, method3dnames

end program
