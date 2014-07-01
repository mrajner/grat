program createdata
  use netcdf
  use mod_data, only: nc_error

  implicit none

  integer(2) :: i, ilevel, itime, ilat, ilon
  integer :: ncid, status, &
  londimid, latdimid, timedimid,leveldimid, &
  lonvarid, latvarid, timevarid,levelvarid, &
     gpvarid
  integer :: dimids(3)

  real, parameter :: resolution = 50

  integer, parameter ::       &
    nlon   = ceiling(360. / resolution), &
    nlat   = ceiling(180. / resolution), &
    nlevel = 5 , &
    ntime = 10

  real, parameter :: &
    lats   ( nlat   )  = [ ( -89.75 + ( i-1 ) *resolution , i = 1 , nlat   ) ] , &
    lons   ( nlon   )  = [ ( 0.25 +   ( i-1 ) *resolution , i = 1 , nlon   ) ] , &
    levels ( nlevel )  = [ ( 1000-    ( i-1 ) *50         , i = 1 , nlevel ) ] , &
    times(ntime)= [ (i*6 , i=1, ntime) ]

  real :: gp (nlon, nlat, nlevel, ntime) 

  call nc_error (nf90_create(path = "data/test_data.nc", cmode = nf90_clobber, ncid = ncid))

  call nc_error (nf90_def_dim(ncid = ncid , name = "lon"   , len = nlon           , dimid = londimid))
  call nc_error (nf90_def_dim(ncid = ncid , name = "lat"   , len = nlat           , dimid = latdimid))
  call nc_error (nf90_def_dim(ncid = ncid , name = "level", len = nlevel         , dimid = leveldimid))
  call nc_error (nf90_def_dim(ncid = ncid , name = "time"  , len = nf90_unlimited , dimid = timedimid))

  call nc_error( nf90_def_var(ncid , "lat"  , NF90_REAL , latdimid   , latvarid) )
  call nc_error( nf90_def_var(ncid , "lon" , NF90_REAL , londimid   , lonvarid) )
  call nc_error( nf90_def_var(ncid , "level", nf90_real , leveldimid , levelvarid) )
  call nc_error( nf90_def_var(ncid , "time", nf90_real , timedimid , timevarid) )

  dimids =  (/ londimid, latdimid, leveldimid /)

  call nc_error(nf90_def_var(ncid , "gp", NF90_int , dimids, gpvarid) )

  call nc_error(nf90_put_att(ncid, latvarid, "units", "degree") )
  call nc_error(nf90_put_att(ncid, lonvarid, "units", "degree") )

  call nc_error(nf90_put_att(ncid, timevarid, "units", "hours since 2010-1-1 00:00:0.0") )

  call nc_error(nf90_enddef(ncid) )

  call nc_error (nf90_put_var(ncid, latvarid, lats ))
  call nc_error (nf90_put_var(ncid, lonvarid, lons ))
  call nc_error (nf90_put_var(ncid, levelvarid, levels ))
  call nc_error (nf90_put_var(ncid, timevarid, times ))

  do itime = 1 , ntime
  do ilevel = 1 , nlevel
    gp(:,:,ilevel,itime) = levels(ilevel) + 0 *times(itime)
  enddo
  enddo

  call nc_error (nf90_put_var(ncid, gpvarid, gp))

  call nc_error (nf90_close(ncid=ncid))
  call system("ncdump data/test_data.nc ")

end program
