program createdata
  use netcdf
  use mod_mjd
  use mod_data, only: nc_error

      ! implicit none
      integer(2) :: i
      integer :: ncid, status, londimid, latdimid, timedimid,leveldimid, &
      latvarid
      integer :: dimids2(2), dimids3(3)

      real, parameter :: resolution = 5

      integer, parameter ::       &
        nlon = 360. / resolution, &
        nlat = 180. / resolution, &
        nlevel = 10


     real :: &
     lats(nlat) = [(-89.75 + (i-1)*resolution, i = 1,nlat) ], &
    lons(nlon) = [ (  0.25 + (i-1)*resolution, i = 1,nlon) ], &
      levels(nlevel) = [ (1000-(i-1)*50, i=1,nlevel) ]

      call nc_error (nf90_create(path = "test_data.nc", cmode = nf90_clobber, ncid = ncid))

      call nc_error (nf90_def_dim(ncid = ncid , name = "lon"  , len = nlon           , dimid = londimid))
      call nc_error (nf90_def_dim(ncid = ncid , name = "lat"  , len = nlat           , dimid = latdimid))
      call nc_error (nf90_def_dim(ncid = ncid , name = "levels"  , len = nlevel     , dimid = leveldimid))
      call nc_error (nf90_def_dim(ncid = ncid , name = "time" , len = nf90_unlimited , dimid = timedimid))

      call nc_error( nf90_def_var(ncid, "latitude", NF90_REAL, latdimid, latvarid) )
      call nc_error( nf90_def_var(ncid, "longitude", NF90_REAL, londimid, lonvarid) )
  ! call check( nf90_def_var(ncid, LON_NAME, NF90_REAL, lon_dimid, lon_varid) )

      call nc_error( nf90_put_att(ncid, latvarid, "units", "degree") )

      dimids2 =  (/ londimid, latdimid /)
!
    call nc_error ( nf90_put_var(ncid, latvarid, lats ))
!  call check( nf90_put_var(ncid, lonvarid, model%lon ))
!  call check( nf90_put_var(ncid, varid, model%data(:,:,1)) )
!
      call nc_error (nf90_close(ncid =ncid))


      call system("ncdump test_data.nc")

! contains
  ! subroutine check(status)
    ! integer, intent ( in) :: status

    ! if(status /= nf90_noerr) then 
    ! print *, trim(nf90_strerror(status))
    ! stop "Stopped"
    ! end if
  ! end subroutine check  
    end program
