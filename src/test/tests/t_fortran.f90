program testing_fortran_procedures
  use mod_utilities
  integer :: iunit
  character(30) :: filename

  print *, "output of ./test"
  call uniq_name_unit(prefix="non_existing_file_",filename=filename,digits=5,unit=iunit,ifcreate=.false.)
  print *, filename
  call uniq_name_unit(filename=filename,digits=3,start=30,unit=iunit, suffix="_suff",ifcreate=.false.)
  print *, filename
end program
