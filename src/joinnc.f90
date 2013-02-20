program joinnc
  use mod_cmdline
  use mod_data
  use netcdf
  ! branch tmp added

  character (80) :: filename, dummy
  integer :: idimid(2)

  filename = ".filelist"

  print * ,"joining files in ", filename
  call intro("")
!  
  call system ("ls "//trim(model(1)%name)//" > "// trim(filename) )

  open(newunit = iunit, file = filename, action = "read")
  do 
    read (iunit , '(a)' , iostat = io_stat) dummy
    if (io_stat == iostat_end ) exit
    print * , trim(dummy)

    
    call check ( nf90_open (dummy , nf90_nowrite , incid) )
    
!    call check ( nf90_inquire (incid , idim , ivar ,  iatt , iunlimited , iform))
    call check( nf90_inq_dimid(incid, "lat_0" , idimid(1) ))
    call check( nf90_inq_dimid(incid, "lon_0" , idimid(2) )) 
    call check( nf90_inq_varid(incid, "pres" , idimid(2) )) 
    print *, sqrt(-1)

    print * , idimid, "kdf:"
  enddo


end program
